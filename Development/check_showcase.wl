(* ::Package:: *)

(* Checks the code of the showcase (Showcase/FunctionalDiagrams_Showcase.tex): every code block is run as printed, in the
   order of the document, in one fresh kernel with the package of this repository, one statement per complete line as a
   notebook reads it, and what each statement gives is compared with what the page shows after the block (up to the next
   block), as make_figures.wl wrote it into Showcase/figures.

     wolfram -script Development/check_showcase.wl              the showcase of this repository (about two minutes)
     wolfram -script Development/check_showcase.wl --verbose    also what the statements print, and the pictures and
                                                                formulas after a block that no statement gives (text,
                                                                left-hand sides)
     wolfram -script Development/check_showcase.wl --blocks N   the first N blocks only
     wolfram -script Development/check_showcase.wl --showcase DIR   the .tex, figures/ and make_figures.wl of DIR

   The rules:
   - Every statement that does not end with ; and gives something other than Null has an output, and that output must be
     shown after its block: a sum of diagrams, a formula, a value, or an integer written in an output box. A list or an
     association is shown whole or element by element. A statement that gives a message fails.
   - A sum: \figuresum{lhs}{NAME}, an ordersum (\orderrow{order}{NAME_m}, one band per order) or \inputfigure{NAME}
     with figures/NAME.terms. The output is drawn as DiagramPlot draws it, by DiagramToTikZ with the same arguments
     (DiagramPlot[e, {x1, ...}, options] as written; any other expression with its free labels in canonical order),
     with the sizing options of the figure ("Scale", "LineWidth", "LabelFont", read from its TikZ code), and its pictures
     must be those of the files, weights and TikZ code, in the same order. The bands of an ordersum are the orders of the
     output in the coupling of their label (\lambda, V, g, W, D_\sigma, \mathcal V, \Delta_{\rm imp}): a band holds
     exactly the pictures of its order. A single sum NAME_m (no band) may be the order m of the output alone, as
     make_figures.wl writes it.
   - A formula: \formula{NAME}, \formulasplit{NAME} or \formulalines{NAME}, compared as a string with
     figures/formula_NAME.tex, the output written by ExpressionToLaTeX with the options that make_figures.wl gives it in
     writeFormula[NAME, e, options] (directly or through a function such as qedFormula), its option "Display" -> f
     applied (names only: contracted indices called rho and sigma, eps written after pi, the step function as Theta). A
     formula may be the orders 0 to k (or 1 to k) of the output.
   - A value: \outval{Key}, typeset as \val{Key}, marks a value that a statement of the block gives. The outputs of the
     block are matched one to one and in order with the \outval after it: an output is compared with the first \outval
     not yet matched and with no other \outval or \val (besides an integer written in an output box and the table
     below), and an \outval that no output matches fails. Key is a key that
     assemble.py writes into figures/values.tex, and stands for the value of figures/values.txt that assemble.py writes
     under it: "check Dyson" is CheckDyson; KeyN the entry N of a list, numbered as assemble.py numbers it (from 1 for the
     checks, from the first index of its table list_indices for the other lists: 0 for orders that start at 0); KeyTotal
     the total of a list of integers; KeyK the entry K of an association (not a check) and KeyKN the entry N of that
     entry. \outval[N]{Key} shows the whole value of Key, of which the output is the entry N (the value of KeyN), and
     \outval[Total]{Key} a list of which the output is the sum (residuals order by order, the output being the residual
     of all the orders together). \val{Key} is a value quoted in the text, computed by make_figures.wl, and is never
     compared with an output. The table figures/parquet_table.tex stands for the values it is made of.
   - A string (the TikZ code of DiagramToTikZ): \inputfigure{NAME} with figures/NAME.tex written by make_figures.wl.
   - Part of an output shown (some orders of a sum, the first orders of a formula, an element of a list or association
     whose orders are left out in the others) passes, and is listed at the end: the text must say what is shown.
   - A block preceded by the line "% not run" (or "% not run: why") in the .tex is only parsed: installation lines, paths;
     the report lists it.
   - Pictures and formulas after a block that no statement gives are not checked: pictures in the text, left-hand sides
     and exact equations after text, results of make_figures.wl stated in words.
   It prints one line per block, ok or what differs, and a summary; the exit code is 1 when a block fails. The blocks are
   read by Development/showcase_blocks.py (python3, standard library). The figures must be those of the current code:
   run Showcase/make_figures.wl first. *)

Begin["CheckShowcase`"];

$root = ParentDirectory[DirectoryName[$InputFileName]];
arguments = Replace[$CommandLine, {{___, "-script" | "-file", _, after___} :> {after}, _ -> {}}];
$verbose = MemberQ[arguments, "--verbose"];
$showcase = Replace[arguments, {{___, "--showcase", dir_, ___} :> ExpandFileName[dir], _ -> FileNameJoin[{$root, "Showcase"}]}];
$lastBlock = Replace[arguments, {{___, "--blocks", n_, ___} :> ToExpression[n], _ -> Infinity}];
$texFile = FileNameJoin[{$showcase, "FunctionalDiagrams_Showcase.tex"}];
$figures = FileNameJoin[{$showcase, "figures"}];
$makeFigures = FileNameJoin[{$showcase, "make_figures.wl"}];
$statementTime = 900;
CheckShowcase::time = "The statement took more than `1` seconds.";
startTime = AbsoluteTime[];

Get[FileNameJoin[{$root, "FunctionalDiagrams.wl"}]];

(* ---------------------------------------------------------------- the blocks *)
blocksOf[tex_String] := Module[{run = RunProcess[{"python3", FileNameJoin[{$root, "Development", "showcase_blocks.py"}], tex}]},
   If[run["ExitCode"] =!= 0, Print["showcase_blocks.py failed:\n", run["StandardError"]]; Exit[2]];
   ImportString[run["StandardOutput"], "RawJSON"]];

(* the statements of a block, as in a notebook or a script: a line that completes an expression ends a statement (the
   same reading as Development/check_wiki.wl) *)
withoutComments[s_String] := StringReplace[s, "(*" ~~ Shortest[___] ~~ "*)" -> ""];
statements[code_String] := Module[{buffer = "", out = {}},
   Do[buffer = If[buffer === "", line, buffer <> "\n" <> line];
    If[StringTrim[withoutComments[buffer]] === "", If[StringFreeQ[buffer, "(*"] || SyntaxQ[buffer], buffer = ""],
     If[SyntaxQ[buffer], AppendTo[out, buffer]; buffer = ""]],
    {line, StringSplit[code, "\n", All]}];
   If[StringTrim[buffer] =!= "", AppendTo[out, buffer]];
   out];
silentQ[statement_String] := StringEndsQ[StringTrim[withoutComments[statement]], ";"];
shortStatement[s_String] := With[{t = StringReplace[StringTrim[withoutComments[s]], WhitespaceCharacter .. -> " "]},
   If[StringLength[t] > 70, StringTake[t, 67] <> "...", t]];

(* ---------------------------------------------------------------- what make_figures.wl wrote *)
readFile[name_String] := With[{f = FileNameJoin[{$figures, name}]}, If[FileExistsQ[f], ReadString[f], Missing[name]]];

(* the terms of figures/NAME.terms: {weight, tikzpicture} *)
termsOf[name_String] := termsOf[name] = With[{text = readFile[name <> ".terms"]},
   If[! StringQ[text], Missing[name],
    If[StringTrim[text] === "", {}, weightAndPicture /@ StringSplit[StringTrim[text], "\n%%TERM%%\n"]]]];
weightAndPicture[block_String] := With[{k = StringPosition[block, "\n%%PICTURE%%", 1]},
   If[k === {}, {StringTrim[block], ""}, StringTrim /@ {StringTake[block, k[[1, 1]] - 1], StringDrop[block, k[[1, 2]]]}]];

(* the sizing options of a picture, from its TikZ code: [x=0.340cm, y=0.340cm, line width=0.420pt, ...] and the font
   of its labels *)
sizingOf[terms_List] := Module[{all = StringJoin[Riffle[terms[[All, 2]], "\n"]], scale, width, font},
   scale = StringCases[all, "[x=" ~~ s : NumberString ~~ "cm" :> ToExpression[s], 1];
   width = StringCases[all, "[x=" ~~ Shortest[___] ~~ "line width=" ~~ w : NumberString ~~ "pt" :> ToExpression[w], 1];
   font = StringCases[all, "\\node[font=\\" ~~ f : LetterCharacter .. :> "\\" <> f, 1];
   {"Scale" -> First[scale, 0.34], "LineWidth" -> First[width, 0.42], "LabelFont" -> First[font, "\\tiny"]}];

(* the values of figures/values.txt, {name, value} line by line *)
keyOf[name_String] := StringJoin[(ToUpperCase[StringTake[#, 1]] <> StringDrop[#, 1]) & /@
    Select[StringSplit[name, RegularExpression["[^A-Za-z0-9]+"]], # =!= "" &]];
$valueLines = Module[{text = readFile["values.txt"]},
   If[! StringQ[text], {},
    Cases[StringSplit[text, "\n"], line_String /; StringContainsQ[line, " :: "] :>
      With[{parts = StringSplit[line, " :: ", 2]}, {parts[[1]], Quiet[Block[{$Context = "Global`"}, ToExpression[parts[[2]], InputForm]]]}]]]];
(* the first index and the step of the entries of a list of values, from the table list_indices of Showcase/assemble.py
   ({1, 1} for a list that it does not name) *)
$listIndices = Module[{table = StringCases[ReadString[FileNameJoin[{$root, "Showcase", "assemble.py"}]],
     "\nlist_indices = {" ~~ t : Shortest[___] ~~ "}\n" :> t, 1]},
   If[table === {}, Print["check_showcase: the table list_indices of Showcase/assemble.py cannot be read"]; Exit[2]];
   Association[StringCases[First[table], "'" ~~ name : Except["'"] .. ~~ "': (" ~~ a : DigitCharacter .. ~~ ", " ~~ b : DigitCharacter .. ~~ ")" :>
      name -> ToExpression[{a, b}]]]];
(* every key that assemble.py writes into figures/values.tex, with the value it stands for, by the rules of assemble.py:
   the key of a line ("check Dyson" is CheckDyson) for its value; a list of checks (lines check ... and parquet residuals
   ...) also KeyN for its entry N, from 1; an association that is not a check KeyK for its entry K and KeyKN for the
   entry N of that entry, from 1; another list KeyN for its entries, numbered by list_indices, and KeyTotal for the total
   of a list of integers. A later line of the same key wins, as a later \setvalue does *)
$namedValues = Module[{named = <||>, key},
   Do[With[{name = line[[1]], v = line[[2]]}, key = keyOf[name];
     Which[
      name === "version", named[key] = v,
      StringStartsQ[name, "check"] || StringStartsQ[name, "parquet residuals"],
      named[key] = v; If[ListQ[v], Do[named[key <> ToString[i]] = v[[i]], {i, Length[v]}]],
      AssociationQ[v],
      KeyValueMap[Function[{k, x}, named[key <> keyOf[ToString[k]]] = x;
         If[ListQ[x], Do[named[key <> keyOf[ToString[k]] <> ToString[i]] = x[[i]], {i, Length[x]}]]], v];
      named[key] = v,
      ListQ[v],
      named[key] = v;
      With[{first = Lookup[$listIndices, name, {1, 1}]}, Do[named[key <> ToString[first[[1]] + first[[2]] (i - 1)]] = v[[i]], {i, Length[v]}]];
      If[VectorQ[v, IntegerQ], named[key <> "Total"] = Total[v]],
      True, named[key] = v]],
    {line, $valueLines}];
   named];
(* the tables that assemble.py makes of values: figures/NAME.tex stands for the values of these keys and their entries *)
$assembledTables = <|"parquet_table" -> {"ParquetCounts", "ParquetResiduals"}|>;
entriesOf[v_] := Which[AssociationQ[v], Join[Values[v], Flatten[entriesOf /@ Values[v], 1]],
   ListQ[v], Join[v, Flatten[entriesOf /@ v, 1]], True, {}];
tableValues[keys_List] := Flatten[Function[k, If[KeyExistsQ[$namedValues, k], Prepend[entriesOf[$namedValues[k]], $namedValues[k]], {}]] /@ keys, 1];

(* how make_figures.wl writes each formula: {options of ExpressionToLaTeX, the function applied for the display}, from its
   calls writeFormula[NAME, e, options] and from its functions that call writeFormula (qedFormula[name_String, x_] :=
   writeFormula[name, x, options]) and their calls. The option "Display" -> f of writeFormula writes f(e) instead of e,
   for names only: the contracted indices called rho and sigma, eps written after pi, the step function written Theta.
   The code is read without being run; the tables of names (assignments to ...Names) are evaluated in a context of
   their own, and the symbols are then those of Global`, where the code of the showcase runs; a function of
   make_figures.wl named in the options is defined again in CheckShowcaseDisplay`, its symbols those of Global` *)
$mfContexts = {"FunctionalDiagrams`", "System`"};
SetAttributes[inMF, HoldAll];
inMF[e_] := Block[{$Context = "CheckShowcaseMF`", $ContextPath = $mfContexts}, e];
(* a held expression of make_figures.wl as a string for Global`, the functions of make_figures.wl in helpers renamed *)
globalString[h_Hold, helpers_List] := Fold[StringReplace[#1, RegularExpression["(?<![A-Za-z0-9`$])" <> #2 <> "(?![A-Za-z0-9`$])"] ->
      "CheckShowcaseDisplay`" <> #2] &, StringReplace[inMF[ToString[h, InputForm]], "CheckShowcaseMF`" -> ""], helpers];
toGlobal[h_Hold, helpers_List : {}] := Block[{$Context = "Global`"}, ToExpression[globalString[h, helpers], InputForm]];
$formulaWriting := $formulaWriting = With[{wf = inMF[Symbol["CheckShowcaseMF`writeFormula"]]},
  Module[{held, definitions, helpers, wrappers, writing = <||>, loaded = {}, load, optionsOf},
   held = inMF[ReadList[$makeFigures, Hold[Expression]]];
   inMF[Scan[ReleaseHold, Cases[held, HoldPattern[Set[s_Symbol, r_]] /; StringEndsQ[SymbolName[Unevaluated[s]], "Names"] :> Hold[Set[s, r]],
      Infinity]]];
   (* the functions defined in make_figures.wl by f[x_] := ... *)
   definitions = GroupBy[Cases[held, d : HoldPattern[SetDelayed[(f_Symbol)[Verbatim[Pattern][_Symbol, Verbatim[Blank][]]], _]] :>
       (SymbolName[Unevaluated[f]] -> Hold[d]), Infinity], First -> Last];
   helpers = Keys[definitions];
   load[name_String] := If[! MemberQ[loaded, name], AppendTo[loaded, name]; Scan[ReleaseHold[toGlobal[#, {name}]] &, definitions[name]]];
   (* the options, evaluated in make_figures.wl's context and taken to Global`: {options, display} *)
   optionsOf[h_Hold] := Module[{string = globalString[Hold @@ {inMF[ReleaseHold[h]]}, helpers], o},
     Scan[load, Select[helpers, StringContainsQ[string, "CheckShowcaseDisplay`" <> #] &]];
     o = ReleaseHold[Block[{$Context = "Global`"}, ToExpression[string, InputForm, Hold]]][[1]];
     {DeleteCases[o, "Display" -> _], Lookup[Association[Cases[o, _Rule]], "Display", Identity]}];
   Scan[(writing[#[[1]]] = optionsOf[#[[2]]]) &, Cases[held, HoldPattern[wf[name_String, _, o___]] :> {name, Hold[{o}]}, Infinity]];
   wrappers = Cases[held, HoldPattern[SetDelayed[(w_Symbol)[Verbatim[Pattern][n_Symbol, Verbatim[Blank][String]], Verbatim[Pattern][x_Symbol, Verbatim[Blank][]]],
        wf[n2_Symbol, x2_Symbol, o___]]] /; SymbolName[Unevaluated[n]] === SymbolName[Unevaluated[n2]] &&
        SymbolName[Unevaluated[x]] === SymbolName[Unevaluated[x2]] :> {Hold[w], Hold[{o}]}, Infinity];
   Do[With[{w = ReleaseHold[wr[[1]]], o = optionsOf[wr[[2]]]},
     Scan[(writing[#] = o) &, Cases[held, HoldPattern[w[name_String, _]] :> name, Infinity]]], {wr, wrappers}];
   writing]];
formulaWriting[name_String] := Lookup[$formulaWriting, name, {{}, Identity}];

(* ---------------------------------------------------------------- outputs *)
(* DiagramPlot gives its picture as usual (its messages are those of a notebook), and the output keeps the call *)
Unprotect[DiagramPlot];
DownValues[DiagramPlot] = Prepend[DownValues[DiagramPlot],
   HoldPattern[DiagramPlot[args___] /; ! TrueQ[$inPlot]] :> Block[{$inPlot = True}, DiagramPlot[args]; plotted[{args}]]];
Protect[DiagramPlot];

(* the free labels of an expression, in canonical order: the symbols of Global` that are arguments of a function *)
freeLabels[e_] := Sort[Union[Cases[e /. IntegralOver[f_, _] :> f, (h_ /; ! MatchQ[h, _Symbol] || Context[h] =!= "System`")[args___] :>
       Sequence @@ Cases[{args}, s_Symbol /; Context[s] === "Global`", {1}], {0, Infinity}, Heads -> True]]];
diagramsQ[e_] := ! (NumericQ[e] || StringQ[e] || ListQ[e] || AssociationQ[e] || MatchQ[e, _Rule | _SeriesData | _Graphics | True | False]);

(* the pictures of an output, drawn with the given sizing: {weight, tikzpicture} for every term, and the expression *)
drawn[plotted[{e_, rest___}], sizing_] := drawn[plotted[{e, rest}], sizing] = Module[{ext, opts},
   {ext, opts} = If[MatchQ[{rest}, {_List, ___}], {First[{rest}], Rest[{rest}]}, {{}, {rest}}];
   {pictureList[Quiet[DiagramToTikZ[e, ext, Sequence @@ opts, "Output" -> "List", Sequence @@ sizing]]], e}];
drawn[e_, sizing_] := drawn[e, sizing] = {pictureList[Quiet[DiagramToTikZ[e, freeLabels[e], "Output" -> "List", Sequence @@ sizing]]], e};
pictureList[r_] := If[MatchQ[r, {{_String, _String} ...}], Map[StringTrim, r, {2}], $Failed];
(* the order of every picture in the coupling v, in the order in which DiagramToTikZ draws the terms *)
termOrders[e_, v_] := termOrders[e, v] = Table[SelectFirst[Range[0, 12], OrderPart[FunctionalDiagrams`Private`termToExpression[t], v, #] =!= 0 &, -1],
   {t, FunctionalDiagrams`Private`termsForDrawing[e]}];

(* the couplings of the bands of an ordersum, by their labels: V^2 -> {InteractionPotential, 2} *)
$couplings = <|"\\lambda" -> "lambda", "V" -> "InteractionPotential", "g" -> "g", "W" -> "ScreenedW", "D_\\sigma" -> "FullSigmaPropagator",
   "\\mathcal V" -> "NambuVertex", "\\Delta_{\\rm imp}" -> "DisorderCorrelator"|>;
couplingSymbols[] := Symbol["Global`" <> #] & /@ Values[$couplings];
orderOfLabel[Null] = None;
orderOfLabel[label_String] := Module[{base, m},
   {base, m} = Replace[StringCases[StringTrim[label], {StartOfString ~~ b__ ~~ "^{" ~~ n : DigitCharacter .. ~~ "}" ~~ EndOfString :> {b, ToExpression[n]},
       StartOfString ~~ b__ ~~ "^" ~~ n : DigitCharacter .. ~~ EndOfString :> {b, ToExpression[n]}}], {{r_, ___} :> r, _ :> {label, 1}}];
   If[KeyExistsQ[$couplings, StringTrim[base]], {Symbol["Global`" <> $couplings[StringTrim[base]]], m}, None]];

(* does an output match an item? True; partial[note, v, orders] when part of it is shown (orders of the coupling v left
   out); the reason why not (a string); or None when it cannot be compared *)
matchItem[out_, item_] := Switch[item["Kind"],
   "sum", matchSum[out, item],
   "formula", matchFormula[out, item],
   "table", matchTable[out, item],
   "raw", If[StringQ[out], If[StringTrim[out] === StringTrim[item["Text"]], True, "the TikZ code differs from figures/" <> item["Name"] <> ".tex"], None],
   _, None];

matchSum[out_, item_] := Module[{rows = item["Rows"], files, sizing, pictures, e, orders, label, problems = {}, shownOrders, notShown, m, name},
   If[! (MatchQ[out, _plotted] || diagramsQ[out]), Return[None]];
   files = termsOf /@ rows[[All, "name"]];
   If[! FreeQ[files, _Missing], Return["missing " <> StringRiffle[Cases[files, Missing[n_] :> "figures/" <> n <> ".terms"], ", "]]];
   sizing = sizingOf[Flatten[files, 1]];
   {pictures, e} = drawn[out, sizing];
   If[! ListQ[pictures], Return[None]];
   label = orderOfLabel /@ rows[[All, "label"]];
   If[FreeQ[label, None] && Length[Union[label[[All, 1]]]] === 1,
    (* the bands are orders of the output *)
    orders = termOrders[e, label[[1, 1]]];
    If[Length[orders] =!= Length[pictures], Return["cannot split the pictures by order"]];
    shownOrders = label[[All, 2]];
    Do[With[{want = files[[k]], have = Pick[pictures, orders, shownOrders[[k]]]},
      If[want =!= have, AppendTo[problems, rows[[k, "name"]] <> ": " <> termsDifference[have, want]]]], {k, Length[rows]}];
    notShown = Complement[Union[orders], shownOrders];
    If[problems =!= {}, Return[StringRiffle[problems, "; "]]];
    Return[If[notShown === {}, True, partial["orders " <> StringRiffle[ToString /@ notShown, ", "] <> " of " <> ToString[label[[1, 1]]] <>
        " not drawn (" <> StringRiffle[rows[[All, "name"]], ", "] <> ")", label[[1, 1]], notShown]]]];
   (* one sum: the whole output, or its order m alone when the sum is NAME_m *)
   If[Length[rows] =!= 1, Return[None]];
   name = rows[[1, "name"]];
   If[pictures === First[files], Return[True]];
   m = StringCases[name, "_" ~~ d : DigitCharacter .. ~~ EndOfString :> ToExpression[d]];
   Catch[
    If[m =!= {} && rows[[1, "label"]] === Null,
     Do[If[! FreeQ[e, v] && (orders = termOrders[e, v]; Length[orders] === Length[pictures] && Pick[pictures, orders, m[[1]]] === First[files]),
       Throw[partial["only the order " <> ToString[m[[1]]] <> " of " <> ToString[v] <> " drawn (" <> name <> ")", v,
         Complement[Union[orders], m]], matchSum]], {v, couplingSymbols[]}]];
    name <> ": " <> termsDifference[pictures, First[files]], matchSum]];
termsDifference[have_List, want_List] := Which[
   Length[have] =!= Length[want], ToString[Length[have]] <> " pictures drawn, " <> ToString[Length[want]] <> " in the file",
   Sort[have] === Sort[want], "the same pictures in another order",
   True, With[{k = SelectFirst[Range[Length[have]], have[[#]] =!= want[[#]] &]},
    "picture " <> ToString[k] <> " differs (" <> If[have[[k, 1]] =!= want[[k, 1]], "weight " <> have[[k, 1]] <> " drawn, " <> want[[k, 1]] <> " in the file",
      "the drawing"] <> ")"]];

matchFormula[out_, item_] := Module[{file = readFile["formula_" <> item["Name"] <> ".tex"], opts, display, tex, full, ks, selections},
   If[MatchQ[out, _plotted | _String] || ListQ[out] || AssociationQ[out], Return[None]];
   If[! StringQ[file], Return["missing figures/formula_" <> item["Name"] <> ".tex"]];
   file = StringTrim[file];
   {opts, display} = formulaWriting[item["Name"]];
   tex[x_] := With[{t = Quiet[ExpressionToLaTeX[display[x], Sequence @@ opts]]}, If[StringQ[t], StringTrim[t], $Failed]];
   full = tex[out];
   If[full === file, Return[True]];
   If[! StringQ[full], Return[None]];
   (* the first orders of the output *)
   Catch[
    Do[If[! FreeQ[out, v],
      ks = Select[Range[0, 8], Quiet[OrderPart[out, v, #]] =!= 0 &];
      selections = DeleteDuplicates[Join[Table[Take[ks, j], {j, Length[ks] - 1}], Table[Take[Rest[ks], j], {j, Length[ks] - 2}]]];
      Do[If[tex[OrderPart[out, v, s]] === file,
        Throw[partial["formula " <> item["Name"] <> ": the orders " <> StringRiffle[ToString /@ s, ", "] <> " of " <> ToString[v] <> " shown",
          v, Complement[ks, s]], matchFormula]], {s, selections}]], {v, couplingSymbols[]}];
    "formula " <> item["Name"] <> ": " <> stringDifference[full, file], matchFormula]];
stringDifference[a_String, b_String] := Module[{k = 1, n = Min[StringLength[a], StringLength[b]], around},
   While[k <= n && StringTake[a, {k}] === StringTake[b, {k}], k++];
   around[s_] := StringTake[s, {Max[1, k - 25], Min[StringLength[s], k + 35]}];
   "ExpressionToLaTeX gives ..." <> around[a] <> "... where the file has ..." <> around[b] <> "..."];

matchTable[out_, item_] := Module[{vs = tableValues[item["Keys"]]},
   If[MatchQ[out, _plotted] || vs === {}, Return[None]];
   If[AnyTrue[vs, sameValueQ[out, #] &], True, None]];
(* the value that \outval{Key} stands for, the value of the key Key; \outval[N]{Key} the value of the key KeyN, the entry N of
   the value of Key; \outval[Total]{Key} the sum of the entries of the list of Key (the residuals order by order, when the
   output is the residual of all the orders together) *)
outvalValue[item_] := With[{key = item["Key"] <> Replace[item["Entry"], Except[_String] -> ""]}, Which[
    item["Entry"] === "Total" && ListQ[Lookup[$namedValues, item["Key"]]], Total[$namedValues[item["Key"]]],
    KeyExistsQ[$namedValues, key], $namedValues[key],
    True, Missing[key]]];
matchOutval[out_, item_] := With[{v = outvalValue[item], where = describe[item] <> " (line " <> ToString[item["Line"]] <> ")"}, Which[
    MissingQ[v], where <> ": assemble.py writes no value " <> First[v],
    sameValueQ[out, v], True,
    True, where <> " is " <> shortForm[v] <> ", the output " <> shortForm[out]]];
shortForm[e_] := With[{s = ToString[e, InputForm]}, If[StringLength[s] > 60, StringTake[s, 57] <> "...", s]];
(* an output not shown that a \val after the block has: a value that a statement gives is shown by \outval *)
quotedHint[out_, block_] := If[MatchQ[out, _plotted], "",
   With[{quoted = SelectFirst[Select[block["items"], #["kind"] === "value" &],
       KeyExistsQ[$namedValues, #["key"]] && sameValueQ[out, $namedValues[#["key"]]] &]},
    If[MissingQ[quoted], "", " (\\val{" <> quoted["key"] <> "}, line " <> ToString[quoted["line"]] <>
      ", has its value: a value that a statement gives is shown by \\outval)"]]];
sameValueQ[a_, b_] := TrueQ[a === b] || (NumericQ[a] && NumericQ[b] && TrueQ[Abs[N[a] - N[b]] <= 10.^-8 Max[1., Abs[N[b]]]]) ||
   (ListQ[a] && ListQ[b] && Length[a] === Length[b] && And @@ MapThread[sameValueQ, {a, b}]) ||
   (AssociationQ[a] && AssociationQ[b] && Keys[a] === Keys[b] && sameValueQ[Values[a], Values[b]]);

(* an integer written in an output box after the block: {0, 3} in a list typeset by hand *)
writtenQ[n_Integer, boxes_List] := AnyTrue[boxes, StringContainsQ[#, RegularExpression["(?<![0-9A-Za-z.])" <> ToString[n] <> "(?![0-9.])"]] &];

(* an output is shown when an item matches it whole, or when every element of a list or association is shown (an
   element of a sum of diagrams whose orders are all left out in the elements shown counts as shown in part). The items
   not yet matched are tried first; an item may show two outputs that are the same. Gives {shown, descriptions, notes of
   the parts left out, reasons why not, {coupling, orders left out}}. A value is first compared with the first \outval
   not yet matched, and with no other \outval *)
SetAttributes[cover, HoldRest];
cover[out_, items_, boxes_, depth_ : 0] := Catch[Module[{tried = {}, r, partials = {}, k, sub, leftOut, failedAt, accepted, next},
   next = SelectFirst[Range[Length[items]], items[[#, "Kind"]] === "outval" && ! items[[#, "Used"]] &];
   If[IntegerQ[next] && ! MatchQ[out, _plotted],
    r = matchOutval[out, items[[next]]];
    If[r === True, items[[next, "Used"]] = True; Throw[{True, {describe[items[[next]]]}, {}, {}, {}}, cover]];
    AppendTo[tried, r]];
   Do[If[items[[j, "Kind"]] =!= "outval" && items[[j, "Used"]] === pass,
     r = matchItem[out, items[[j]]];
     Which[
      r === True, items[[j, "Used"]] = True; Throw[{True, {describe[items[[j]]]}, {}, {}, {}}, cover],
      MatchQ[r, _partial], AppendTo[partials, {j, r}],
      StringQ[r], AppendTo[tried, r]]],
    {pass, {False, True}}, {j, Length[items]}];
   (* part of the output shown, when no item shows all of it *)
   If[partials =!= {}, {k, r} = First[partials]; items[[k, "Used"]] = True;
    Throw[{True, {describe[items[[k]]]}, {r[[1]]}, {}, {{r[[2]], r[[3]]}}}, cover]];
   If[IntegerQ[out] && writtenQ[out, boxes], Throw[{True, {ToString[out] <> " (written)"}, {}, {}, {}}, cover]];
   If[depth < 2 && (ListQ[out] || AssociationQ[out]) && Length[out] > 0,
    sub = Table[cover[o, items, boxes, depth + 1], {o, If[AssociationQ[out], Values[out], out]}];
    leftOut = Flatten[sub[[All, 5]], 1];
    failedAt = Flatten[Position[sub[[All, 1]], False]];
    accepted = Select[failedAt, Function[i, With[{o = If[AssociationQ[out], Values[out], out][[i]]},
        diagramsQ[o] && AnyTrue[Union[leftOut[[All, 1]]], Function[v, ! FreeQ[o, v] &&
            SubsetQ[Union @@ Cases[leftOut, {v, os_} :> os], Union[termOrders[o, v]]]]]]]];
    If[Complement[failedAt, accepted] === {},
     Throw[{True, Flatten[sub[[All, 2]]], Join[Flatten[sub[[All, 3]]],
        Table["element " <> ToString[i] <> " of the output, whose orders are left out in the others, not shown", {i, accepted}]], {}, leftOut}, cover]];
    Throw[{False, {}, {}, Join[tried, Flatten[sub[[Complement[failedAt, accepted], 4]]]], {}}, cover]];
   {False, {}, {}, tried, {}}], cover];
describe[item_] := Switch[item["Kind"],
   "sum", If[Length[item["Rows"]] === 1, item["Rows"][[1, "name"]], item["Rows"][[1, "name"]] <> ".." <> Last[item["Rows"]]["name"]],
   "formula", "formula " <> item["Name"],
   "outval", "\\outval" <> If[StringQ[item["Entry"]], "[" <> item["Entry"] <> "]", ""] <> "{" <> item["Key"] <> "}",
   "table", item["Name"] <> ".tex",
   "raw", item["Name"] <> ".tex",
   _, "?"];

(* the items after a block: an ordersum whose bands are not orders gives one item per band; \inputfigure is a sum when
   figures/NAME.terms exists, a table of values or the TikZ code of figures/NAME.tex otherwise; \outval a value given by
   a statement; \val (a value quoted in the text) none *)
itemsOf[block_] := Flatten[Function[it, Switch[it["kind"],
      "sum", If[FreeQ[orderOfLabel /@ it["rows"][[All, "label"]], None] || Length[it["rows"]] === 1,
       <|"Kind" -> "sum", "Rows" -> it["rows"], "Line" -> it["line"], "Used" -> False|>,
       Table[<|"Kind" -> "sum", "Rows" -> {r}, "Line" -> it["line"], "Used" -> False|>, {r, it["rows"]}]],
      "formula", <|"Kind" -> "formula", "Name" -> it["name"], "Line" -> it["line"], "Used" -> False|>,
      "outval", <|"Kind" -> "outval", "Key" -> it["key"], "Entry" -> it["entry"], "Line" -> it["line"], "Used" -> False|>,
      "value", Nothing,
      "figure", Which[
       StringQ[readFile[it["name"] <> ".terms"]], <|"Kind" -> "sum", "Rows" -> {<|"label" -> Null, "name" -> it["name"]|>}, "Line" -> it["line"],
        "Used" -> False|>,
       KeyExistsQ[$assembledTables, it["name"]], <|"Kind" -> "table", "Name" -> it["name"], "Keys" -> $assembledTables[it["name"]],
        "Line" -> it["line"], "Used" -> False|>,
       StringQ[readFile[it["name"] <> ".tex"]], <|"Kind" -> "raw", "Name" -> it["name"], "Text" -> readFile[it["name"] <> ".tex"],
        "Line" -> it["line"], "Used" -> False|>,
       True, Nothing]]] /@ block["items"]];

(* ---------------------------------------------------------------- running *)
(* one statement in Global`, its messages and printed lines captured *)
evaluateStatement[statement_String] := Module[{files = Table[FileNameJoin[{$TemporaryDirectory, "check_showcase_" <> ToString[$ProcessID] <> s <> ".txt"}],
     {s, {"m", "o"}}], streams, result, texts},
   streams = OpenWrite[#, FormatType -> OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF8"] & /@ files;
   result = Block[{$Messages = {streams[[1]]}, $Output = {streams[[2]]}, $Context = "Global`"},
     CheckAbort[TimeConstrained[ToExpression[statement, InputForm], $statementTime, Message[CheckShowcase::time, $statementTime]; $Aborted],
      $Aborted]];
   Close /@ streams;
   texts = Replace[ReadString[#], Except[_String] -> ""] & /@ files;
   DeleteFile /@ files;
   {result, StringTrim[texts[[1]]], StringTrim[texts[[2]]]}];

blocks = Take[blocksOf[$texFile], UpTo[$lastBlock]];
Print["check_showcase: ", Length[blocks], " code blocks of ", FileNameTake[$texFile], ", FunctionalDiagrams ", $FunctionalDiagramsVersion,
  ", Mathematica ", First[StringSplit[$Version]]];
SetDirectory[CreateDirectory[]];
failed = {}; notRun = {}; notes = {}; unshown = {}; outputCount = 0;
Do[Module[{b = blocks[[i]], where, items, st, problems = {}, shown = {}, result, messages, printed, t = AbsoluteTime[], c},
   where = "block " <> ToString[i] <> " (line " <> ToString[b["line"]] <> ")";
   items = itemsOf[b];
   st = statements[b["code"]];
   If[! b["run"],
    AppendTo[notRun, where];
    If[! AllTrue[st, SyntaxQ], AppendTo[problems, "the code is not valid Wolfram Language"]],
    Do[
     {result, messages, printed} = evaluateStatement[s];
     If[messages =!= "", AppendTo[problems, shortStatement[s] <> " gives the messages\n      " <> StringReplace[messages, "\n" -> "\n      "]]];
     If[printed =!= "" && $verbose, Print["  ", shortStatement[s], " prints\n    ", printed]];
     If[! silentQ[s] && result =!= Null,
      outputCount++;
      c = cover[result, items, b["boxes"]];
      If[First[c],
       AppendTo[shown, StringRiffle[c[[2]], " + "]]; notes = Join[notes, (where <> ": " <> #) & /@ c[[3]]],
       AppendTo[problems, shortStatement[s] <> ": its output " <> If[MatchQ[result, _plotted] || diagramsQ[result], "", shortForm[result] <> " "] <>
        "is not shown after the block" <> quotedHint[result, b] <>
         If[c[[4]] === {}, "", "; the nearest:\n      " <> StringRiffle[Take[DeleteDuplicates[c[[4]]], UpTo[3]], "\n      "]]]]],
     {s, st}]];
   (* every \outval is the output of a statement *)
   Scan[AppendTo[problems, describe[#] <> " (line " <> ToString[#["Line"]] <> ") is the output of no statement"] &,
    Select[items, #["Kind"] === "outval" && ! #["Used"] &]];
   If[problems === {},
    Print["ok    ", where, ": ", If[shown === {}, "no output", StringRiffle[shown, ", "]],
     If[AbsoluteTime[] - t > 20, " (" <> ToString[Round[AbsoluteTime[] - t]] <> " s)", ""]],
    AppendTo[failed, where];
    Print["FAIL  ", where, ":\n    ", StringRiffle[problems, "\n    "]]];
   unshown = Join[unshown, (where <> ": " <> describe[#]) & /@ Select[items, ! #["Used"] && ! MemberQ[{"table", "outval"}, #["Kind"]] &]]],
  {i, Length[blocks]}];

Print[""];
If[notes =!= {}, Print["shown in part (the text must say what is shown):\n  ", StringRiffle[notes, "\n  "]]];
If[notRun =!= {}, Print["not run (% not run): ", StringRiffle[notRun, ", "]]];
If[$verbose && unshown =!= {}, Print["pictures and formulas given by no statement (text, left-hand sides):\n  ", StringRiffle[unshown, "\n  "]]];
Print["check_showcase: ", Length[blocks], " blocks, ", outputCount, " outputs: ", Length[blocks] - Length[failed] - Length[notRun], " ok, ",
  Length[failed], " failed, ", Length[notRun], " not run (", Round[AbsoluteTime[] - startTime], " s)",
  If[failed === {}, "", "\nfailed: " <> StringRiffle[failed, ", "]]];
Exit[If[failed === {}, 0, 1]];

End[];
