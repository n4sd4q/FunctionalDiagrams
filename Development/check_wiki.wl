(* ::Package:: *)

(* Checks the pages of the wiki (wiki/*.md, published to the GitHub wiki by .github/workflows/wiki.yml): the Mathematica
   code of every page is run, block after block, in a fresh kernel with the package of this repository, and what each
   block gives is compared with what the page shows after it.

     wolfram -script Development/check_wiki.wl                  every page (one kernel per page, up to one per core)
     wolfram -script Development/check_wiki.wl FAQ Home         the pages named
     wolfram -script Development/check_wiki.wl --images         also redraw the pictures that are out of date

   The rules of a page:
   - A fenced block ```mathematica is run, in the order of the page, in one kernel per page (the pages are independent).
     A block preceded by the line <!-- not run --> (or <!-- not run: why -->) is only parsed: installation lines, paths.
   - What a block gives is shown right after it, with only blank lines between: a fenced block (```text, or ```latex for
     LaTeX code), a picture ![...](images/NAME.svg), or both. Without them, the block must give nothing (its statements
     end with ;).
   - A text output is what a terminal session prints: the messages and the printed lines, then the result of every
     statement that does not end with ; and is not Null, in OutputForm with no line width (strings without quotes,
     fractions and powers over two lines; a notebook shows the same in its own form).
     Blank lines and trailing spaces are ignored in the comparison.
   - A picture is drawn by the last statement of the block, DiagramPlot, DrawDiagram or DiagramToTikZ: its TikZ code (the
     same call with DiagramToTikZ) is typeset and converted to SVG on a white card, legible on light and dark pages, and
     the SVG keeps the SHA-1 of the TikZ code; the check compares it with the code drawn now. --images redraws the
     pictures whose code changed (needs pdflatex and pdftocairo).
   The package is loaded from this repository before a page is run, so Needs["FunctionalDiagrams`"] in a page loads
   nothing else. The script needs Mathematica; Development/check_consistency.py checks the links, the version and the
   public names of the pages without it. *)

Begin["CheckWiki`"];

$root = ParentDirectory[DirectoryName[$InputFileName]];
$wikiDirectory = FileNameJoin[{$root, "wiki"}];
$imageScale = 1.5; (* the SVG is shown 1.5 times larger than the PDF, so that the pictures read at the size of the text *)

(* ---------------------------------------------------------------- parsing *)
(* the blocks of a page: <|"Line" -> n, "Code" -> code, "Run" -> True|False, "Text" -> None | text, "Picture" -> None |
   name|>: the output is a text block, a picture, or a text block followed by a picture (the TikZ code and the picture
   it draws) *)
fenceQ[line_String, languages_] := StringMatchQ[line, StartOfString ~~ "```" ~~ languages ~~ WhitespaceCharacter ... ~~ EndOfString];
pictureLineQ[line_String] := StringMatchQ[line, StartOfString ~~ "![" ~~ ___ ~~ "](images/" ~~ __ ~~ ".svg)" ~~ ___];
pageBlocks[file_String] := Module[{lines = StringSplit[ReadString[file], "\n", All], blocks = {}, i = 1, n, start, code, run,
    j, text, picture, k, prev, nextNonBlank},
   n = Length[lines];
   nextNonBlank[from_] := Module[{m = from}, While[m <= n && StringTrim[lines[[m]]] === "", m++]; m];
   While[i <= n,
    If[fenceQ[lines[[i]], "mathematica"],
     start = i;
     prev = SelectFirst[Reverse[lines[[;; i - 1]]], StringTrim[#] =!= "" &, ""];
     run = ! StringMatchQ[StringTrim[prev], "<!--" ~~ WhitespaceCharacter ... ~~ "not run" ~~ ___ ~~ "-->"];
     j = i + 1;
     While[j <= n && ! fenceQ[lines[[j]], ""], j++];
     code = StringRiffle[lines[[i + 1 ;; j - 1]], "\n"];
     (* the output: the next fenced block, picture, or both, with only blank lines before them *)
     {text, picture} = {None, None};
     k = nextNonBlank[j + 1];
     If[k <= n && fenceQ[lines[[k]], "text" | "latex"],
      Module[{m = k + 1},
       While[m <= n && ! fenceQ[lines[[m]], ""], m++];
       text = StringRiffle[lines[[k + 1 ;; m - 1]], "\n"]; j = m; k = nextNonBlank[m + 1]]];
     If[k <= n && pictureLineQ[lines[[k]]],
      picture = First[StringCases[lines[[k]], "](images/" ~~ name : Except[")"] .. ~~ ".svg)" :> name, 1]]; j = k];
     AppendTo[blocks, <|"Line" -> start, "Code" -> code, "Run" -> run, "Text" -> text, "Picture" -> picture|>];
     i = j + 1,
     i++]];
   blocks];

(* the statements of a block, as in a notebook or a script: a line that completes an expression ends a statement *)
withoutComments[s_String] := StringReplace[s, "(*" ~~ Shortest[___] ~~ "*)" -> ""];
statements[code_String] := Module[{buffer = "", out = {}},
   Do[buffer = If[buffer === "", line, buffer <> "\n" <> line];
    If[StringTrim[withoutComments[buffer]] === "", If[StringFreeQ[buffer, "(*"] || SyntaxQ[buffer], buffer = ""],
     If[SyntaxQ[buffer], AppendTo[out, buffer]; buffer = ""]],
    {line, StringSplit[code, "\n", All]}];
   If[StringTrim[buffer] =!= "", AppendTo[out, buffer]];
   out];
silentQ[statement_String] := StringEndsQ[StringTrim[withoutComments[statement]], ";"];

normalized[s_String] := StringRiffle[Select[StringTrim[#, WhitespaceCharacter .. ~~ EndOfString] & /@ StringSplit[s, "\n"],
    StringTrim[#] =!= "" &], "\n"];

(* ---------------------------------------------------------------- pictures *)
svgHashPattern = "wiki picture, TikZ SHA-1 " ~~ hash : HexadecimalCharacter .. :> hash;
svgFile[name_String] := FileNameJoin[{$wikiDirectory, "images", name <> ".svg"}];
storedHash[name_String] := If[FileExistsQ[svgFile[name]], First[StringCases[ReadString[svgFile[name]], svgHashPattern, 1], None], None];

(* the TikZ code of the picture drawn by the last statement of a block, or $Failed *)
pictureTikZ[statement_String] := Module[{held = Block[{$Context = "Global`"}, ToExpression[statement, InputForm, HoldComplete]]},
   held = held /. HoldComplete[CompoundExpression[___, last_]] :> HoldComplete[last];
   If[MatchQ[held, HoldComplete[(FunctionalDiagrams`DiagramPlot | FunctionalDiagrams`DrawDiagram | FunctionalDiagrams`DiagramToTikZ)[__]]],
    Block[{$Context = "Global`"}, ReleaseHold[ReplacePart[held, {1, 0} -> FunctionalDiagrams`DiagramToTikZ]]],
    $Failed]];

(* typeset the TikZ code, convert it to SVG on a white card, with the hash of the code; True if it worked *)
drawPicture[name_String, tikz_String, hash_String] := Module[{dir = CreateDirectory[], tex, run, svg, opening},
   tex = StringJoin["\\documentclass[border=6pt, varwidth=16cm]{standalone}\n\\usepackage{tikz}\n",
     "\\usetikzlibrary{decorations.pathmorphing}\n\\begin{document}\n$\\displaystyle ", tikz, "$\n\\end{document}\n"];
   Export[FileNameJoin[{dir, "picture.tex"}], tex, "Text"];
   run = RunProcess[{"pdflatex", "-interaction=nonstopmode", "-halt-on-error", "picture.tex"}, ProcessDirectory -> dir];
   If[run["ExitCode"] =!= 0, Print["  pdflatex failed for ", name, ":\n", StringTake[run["StandardOutput"], -UpTo[1500]]];
    DeleteDirectory[dir, DeleteContents -> True]; Return[False]];
   run = RunProcess[{"pdftocairo", "-svg", "picture.pdf", "picture.svg"}, ProcessDirectory -> dir];
   If[run["ExitCode"] =!= 0, Print["  pdftocairo failed for ", name]; DeleteDirectory[dir, DeleteContents -> True]; Return[False]];
   svg = ReadString[FileNameJoin[{dir, "picture.svg"}]];
   DeleteDirectory[dir, DeleteContents -> True];
   (* larger, and on a white card with rounded corners and a grey edge, as the pictures of the README *)
   opening = First[StringCases[svg, "<svg " ~~ Shortest[___] ~~ ">", 1]];
   svg = StringReplace[svg, opening -> Module[{w, h, o = opening},
       {w, h} = ToExpression /@ First[StringCases[o, "viewBox=\"0 0 " ~~ a : NumberString ~~ " " ~~ b : NumberString ~~ "\"" :> {a, b}, 1]];
       o = StringReplace[o, {"width=\"" ~~ NumberString ~~ "pt\"" -> "width=\"" <> ToString[NumberForm[$imageScale w, {6, 2}]] <> "pt\"",
          "height=\"" ~~ NumberString ~~ "pt\"" -> "height=\"" <> ToString[NumberForm[$imageScale h, {6, 2}]] <> "pt\""}];
       StringJoin[o, "\n<!-- wiki picture, TikZ SHA-1 ", hash, ": drawn by Development/check_wiki.wl -->\n",
        "<rect x=\"0.5\" y=\"0.5\" width=\"", ToString[NumberForm[w - 1, {6, 3}]], "\" height=\"", ToString[NumberForm[h - 1, {6, 3}]],
        "\" rx=\"5\" fill=\"#ffffff\" stroke=\"#d0d7de\" stroke-width=\"0.75\"/>"]]];
   If[! DirectoryQ[DirectoryName[svgFile[name]]], CreateDirectory[DirectoryName[svgFile[name]]]];
   Export[svgFile[name], svg, "Text"];
   True];

(* ---------------------------------------------------------------- running a page *)
(* evaluate one statement in Global`, with the messages and the printed lines captured *)
evaluateStatement[statement_String] := Module[{file = FileNameJoin[{$TemporaryDirectory, "check_wiki_" <> ToString[$ProcessID] <> ".txt"}],
    stream, result, text},
   stream = OpenWrite[file, FormatType -> OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF8"];
   result = Block[{$Messages = {stream}, $Output = {stream}, $Context = "Global`"},
     (* an abort (a message that stops the evaluation) gives $Aborted, as in a notebook *)
     CheckAbort[TimeConstrained[ToExpression[statement, InputForm], 900, Message[CheckWiki::time]; $Aborted], $Aborted]];
   Close[stream];
   text = ReadString[file]; If[! StringQ[text], text = ""];
   DeleteFile[file];
   {result, text}];
CheckWiki::time = "The statement took more than 900 seconds.";

runPage[file_String, images_] := Module[{blocks = pageBlocks[file], page = FileBaseName[file], failures = 0, passes = 0,
    workDirectory = CreateDirectory[], t0 = AbsoluteTime[]},
   SetDirectory[workDirectory];
   Do[Module[{where = page <> ":" <> ToString[b["Line"]], st = statements[b["Code"]], shown = "", result, text, tikz, hash, t = AbsoluteTime[]},
     If[! b["Run"],
      If[AllTrue[st, SyntaxQ], passes++, failures++; Print["FAIL ", where, ": the code is not valid Wolfram Language"]],
      Do[{result, text} = evaluateStatement[s];
       shown = shown <> text;
       If[! silentQ[s] && result =!= Null,
        shown = shown <> "\n" <> ToString[result, OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF8"]],
       {s, st}];
      (* the text: what the page shows, or nothing (a picture stands for the graphics of DiagramPlot) *)
      Which[
       StringQ[b["Text"]],
       If[normalized[shown] === normalized[b["Text"]], passes++,
        failures++; Print["FAIL ", where, ": the page shows\n", b["Text"], "\nthe block gives\n", normalized[shown]]],
       StringQ[b["Picture"]],
       If[! StringMatchQ[normalized[shown], "" | "-Graphics-"],
        failures++; Print["FAIL ", where, ": the block gives, besides the picture,\n", normalized[shown]]],
       True,
       If[normalized[shown] === "", passes++,
        failures++; Print["FAIL ", where, ": the block gives output that the page does not show:\n", shown]]];
      (* the picture: drawn from the TikZ code of the last statement *)
      If[StringQ[b["Picture"]],
       tikz = pictureTikZ[Last[st]];
       Which[
        ! StringQ[tikz], failures++; Print["FAIL ", where, ": a picture follows a block whose last statement is not DiagramPlot, DrawDiagram or DiagramToTikZ"],
        hash = Hash[tikz, "SHA", "HexString"]; storedHash[b["Picture"]] === hash, passes++,
        TrueQ[images] && drawPicture[b["Picture"], tikz, hash], passes++; Print["drew images/", b["Picture"], ".svg"],
        True, failures++; Print["FAIL ", where, ": images/", b["Picture"], ".svg is not drawn from the code of this block ",
         "(run with --images)"]]]];
     If[AbsoluteTime[] - t > 20, Print["  ", where, ": ", Round[AbsoluteTime[] - t], " s"]]],
    {b, blocks}];
   ResetDirectory[];
   DeleteDirectory[workDirectory, DeleteContents -> True];
   Print[page, ": ", passes, " blocks passed, ", failures, " failed (", Round[AbsoluteTime[] - t0], " s)"];
   failures];

(* ---------------------------------------------------------------- main *)
(* the arguments after the name of this script *)
arguments = Replace[$CommandLine, {{___, "-script" | "-file", _, after___} :> {after}, _ -> {}}];
Which[
 (* a page in this kernel: --page FILE [--images] *)
 MemberQ[arguments, "--page"],
 Get[FileNameJoin[{$root, "FunctionalDiagrams.wl"}]];
 Exit[If[runPage[arguments[[Position[arguments, "--page"][[1, 1]] + 1]], MemberQ[arguments, "--images"]] == 0, 0, 1]],
 (* every page, or those named, each in a fresh kernel; the kernels run at the same time, up to the number of cores,
    and their reports are printed in the order of the pages *)
 True,
 Module[{names = Select[arguments, ! StringStartsQ[#, "--"] &], files, kernel, failed = {}, missing, batches, procs},
  files = If[names === {}, FileNames["*.md", $wikiDirectory],
    FileNameJoin[{$wikiDirectory, StringReplace[#, ".md" ~~ EndOfString -> ""] <> ".md"}] & /@ names];
  missing = Select[files, ! FileExistsQ[#] &];
  Scan[(Print["no page ", #]; AppendTo[failed, FileBaseName[#]]) &, missing];
  files = Select[Complement[files, missing], pageBlocks[#] =!= {} &];
  kernel = If[FileExistsQ[First[$CommandLine]], First[$CommandLine], "wolfram"];
  batches = Partition[files, UpTo[Max[1, $ProcessorCount]]];
  Do[
   procs = StartProcess[Join[{kernel, "-script", $InputFileName, "--page", #}, If[MemberQ[arguments, "--images"], {"--images"}, {}]]] & /@ batch;
   (* reading each report to its end waits for its kernel *)
   MapThread[Function[{f, p}, Module[{out = ReadString[p, EndOfFile]},
       While[ProcessStatus[p] === "Running", Pause[0.1]];
       If[StringQ[out], WriteString["stdout", out]];
       If[ProcessInformation[p, "ExitCode"] =!= 0, AppendTo[failed, FileBaseName[f]]]]],
    {batch, procs}],
   {batch, batches}];
  Print[If[failed === {}, "wiki: every block gives what its page shows",
    "wiki: blocks failed in " <> StringRiffle[failed, ", "]]];
  Exit[If[failed === {}, 0, 1]]]];

End[];
