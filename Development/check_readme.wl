(* ::Package:: *)

(* Checks that the first example of README.md gives the pictures that follow it. The code blocks of its section
   "A first example" are run, one after the other, in this kernel, with the package of this repository; each block is
   followed in the README by a panel, Documentation/readme_N.svg, which Development/readme_gallery.tex draws from the
   figures of the showcase (Showcase/figures/NAME.terms, written by Showcase/make_figures.wl), one sum per DiagramPlot of
   the block (\ordersum), one band per order (\orderrow{order}{\fig{NAME}}). For every DiagramPlot of a block the check
   takes the same call with DiagramToTikZ, at the size of the showcase's pictures (pictureOptions of make_figures.wl),
   splits its terms by order as make_figures.wl does, and compares them with the bands of the panel: the same orders,
   and in each the same weights and the same TikZ code, in the same order. So a change of the code of the README, of the
   package or of the figures that makes the README show something else than its code gives is caught.

     wolfram -script Development/check_readme.wl

   Run it after changing the first example of the README, its panels, or the package (with the figures of the showcase
   computed again). It needs Mathematica; the README's pictures are drawn by Development/make_readme_figure.sh. *)

Begin["CheckReadme`"];

$root = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{$root, "FunctionalDiagrams.wl"}]];
failures = 0;
fail[msg__] := (failures++; Print["FAIL ", msg]);

(* ---------------------------------------------------------------- the README *)
(* the code blocks of the section "A first example", each with the number N of the first picture readme_N.svg after it *)
readme = ReadString[FileNameJoin[{$root, "README.md"}]];
section = First[StringCases[readme, "\n## A first example\n" ~~ s : Shortest[___] ~~ "\n## " :> s, 1]];
blocks = StringCases[section, "```mathematica\n" ~~ code : Shortest[___] ~~ "\n```" ~~ after : Shortest[___] ~~
     (("\n```mathematica\n") | EndOfString) :> {code, StringCases[after, "Documentation/readme_" ~~ n : DigitCharacter .. ~~ ".svg" :> ToExpression[n], 1]},
   Overlaps -> True];

(* the statements of a block, as in a notebook: a line that completes an expression ends a statement *)
withoutComments[s_String] := StringReplace[s, "(*" ~~ Shortest[___] ~~ "*)" -> ""];
statements[code_String] := Module[{buffer = "", out = {}},
   Do[buffer = If[buffer === "", line, buffer <> "\n" <> line];
    If[StringTrim[withoutComments[buffer]] === "", If[StringFreeQ[buffer, "(*"] || SyntaxQ[buffer], buffer = ""],
     If[SyntaxQ[buffer], AppendTo[out, buffer]; buffer = ""]],
    {line, StringSplit[code, "\n", All]}];
   If[StringTrim[buffer] =!= "", AppendTo[out, buffer]];
   out];

(* ---------------------------------------------------------------- the panels *)
(* the panels of the README (readme_gallery.tex without the panel of the social preview), each a list of sums, each sum a
   list of bands {order, figure}: the order as {coupling, power}, "V^2" -> {"V", 2}, "\\lambda" -> {"lambda", 1} *)
gallery = ReadString[FileNameJoin[{$root, "Development", "readme_gallery.tex"}]];
readmePart = First[StringCases[gallery, "\\ifdefined\\socialpreviewpanel" ~~ Shortest[___] ~~ "\n\\else\n" ~~ rest___ :> rest, 1]];
orderOf[label_String] := Module[{base, power},
   {base, power} = First[StringCases[StringTrim[label],
      {b : Except["^"] .. ~~ "^{" ~~ p : DigitCharacter .. ~~ "}" :> {b, p}, b : Except["^"] .. ~~ "^" ~~ p : DigitCharacter :> {b, p},
       b : Except["^"] .. :> {b, "1"}}, 1]];
   {StringDelete[base, "\\"], ToExpression[power]}];
panels = Table[
   Table[{orderOf[#1], #2} & @@@ StringCases[sum, "\\orderrow{" ~~ order : Except["}"] .. ~~ "}{\\fig{" ~~ name : Except["}"] .. ~~ "}}" :> {order, name}],
    {sum, Rest[StringSplit[panel, "\\ordersum"]]}],
   {panel, StringCases[readmePart, "\\begin{panel}" ~~ body : Shortest[___] ~~ "\\end{panel}" :> body]}];

(* ---------------------------------------------------------------- the figures of the showcase *)
figures = FileNameJoin[{$root, "Showcase", "figures"}];
figureBlocks[name_String] := With[{file = FileNameJoin[{figures, name <> ".terms"}]},
   If[FileExistsQ[file], StringTrim /@ StringSplit[ReadString[file], "%%TERM%%"], Missing[name]]];
(* the size of the pictures of the showcase, read from make_figures.wl *)
pictureOptions = ToExpression[First[StringCases[ReadString[FileNameJoin[{$root, "Showcase", "make_figures.wl"}]],
     "\npictureOptions = " ~~ o : Shortest[___] ~~ ";\n" :> o, 1]]];
(* the terms of an expression drawn by DiagramToTikZ, with their orders in a coupling, as make_figures.wl splits them *)
integrationCount[t_] := Total[Cases[{t}, FunctionalDiagrams`IntegralOver[_, vars_List] :> Length[vars], Infinity]];
orderOfTerm[t_, v_] := SelectFirst[Range[0, 12], FunctionalDiagrams`OrderPart[t, v, #] =!= 0 &];

(* ---------------------------------------------------------------- running the blocks *)
evaluate[statement_String] := Block[{$Context = "Global`"}, ToExpression[statement, InputForm]];
(* the arguments of a statement DiagramPlot[e, ext, opts], evaluated, or None *)
plotArguments[statement_String] := Module[{held = Block[{$Context = "Global`"}, ToExpression[statement, InputForm, HoldComplete]]},
   If[MatchQ[held, HoldComplete[FunctionalDiagrams`DiagramPlot[__]]],
    Block[{$Context = "Global`"}, ReleaseHold[held /. HoldComplete[FunctionalDiagrams`DiagramPlot[args__]] :> HoldComplete[{args}]]],
    None]];

compare[where_String, args_List, sum_List] := Module[{e = args[[1]], ext = If[Length[args] > 1, args[[2]], {}],
    opts = Drop[args, Min[2, Length[args]]], terms, parts, coupling, orders, shown, drawn},
   coupling = Symbol["Global`" <> sum[[1, 1, 1]]];
   terms = FunctionalDiagrams`DiagramTerms[e];
   parts = FunctionalDiagrams`DiagramToTikZ[e, ext, Sequence @@ opts, "Output" -> "List", Sequence @@ pictureOptions];
   If[Length[parts] =!= Length[terms], fail[where, ": DiagramToTikZ draws ", Length[parts], " terms of ", Length[terms]]; Return[]];
   orders = orderOfTerm[#, coupling] & /@ terms[[Ordering[integrationCount /@ terms]]];
   shown = sum[[All, 1, 2]];
   If[Complement[Union[orders], shown] =!= {},
    fail[where, ": the orders ", Complement[Union[orders], shown], " of ", coupling, " are not in the panel"]];
   Do[With[{m = band[[1, 2]], name = band[[2]]},
     drawn = StringTrim[#[[1]] <> "\n%%PICTURE%%\n" <> #[[2]]] & /@ Pick[parts, orders, m];
     Which[
      MissingQ[figureBlocks[name]], fail[where, ": Showcase/figures/", name, ".terms is missing"],
      drawn === figureBlocks[name], Print["  ", coupling, "^", m, ": ", Length[drawn], If[Length[drawn] == 1, " diagram", " diagrams"], ", as ", name],
      Length[drawn] =!= Length[figureBlocks[name]],
      fail[where, ": order ", m, " of ", coupling, " has ", Length[drawn], " diagrams, ", name, " ", Length[figureBlocks[name]]],
      Sort[drawn] === Sort[figureBlocks[name]], fail[where, ": order ", m, " of ", coupling, ": the diagrams of ", name, " in another order"],
      True, fail[where, ": order ", m, " of ", coupling, ": the weights or the pictures differ from ", name]]],
    {band, sum}]];

Module[{plots, n, panel, where},
 If[blocks === {}, fail["README.md: no code block in the section A first example"]];
 Do[
  {n, panel} = {If[block[[2]] === {}, None, First[block[[2]]]], None};
  plots = {};
  Do[With[{a = plotArguments[s]}, If[a === None, evaluate[s], AppendTo[plots, a]]], {s, statements[block[[1]]]}];
  where = "README block " <> ToString[k];
  Which[
   plots === {}, Null,
   n === None || n > Length[panels], fail[where, ": no panel of readme_gallery.tex follows it"],
   Length[plots] =!= Length[panels[[n]]],
   fail[where, ": ", Length[plots], " DiagramPlot, but panel ", n, " (readme_", n, ".svg) has ", Length[panels[[n]]], " sums"],
   True,
   Print[where, " (readme_", n, ".svg):"];
   MapIndexed[compare[where <> ", DiagramPlot " <> ToString[First[#2]], #1, panels[[n, First[#2]]]] &, plots]],
  {k, Length[blocks]}, {block, {blocks[[k]]}}]];

Print[If[failures === 0, "README: the first example gives the pictures that follow it", "README: " <> ToString[failures] <> " failures"]];
End[];
Exit[If[CheckReadme`failures === 0, 0, 1]];
