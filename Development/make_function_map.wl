(* ::Package:: *)

(* The map of the public functions of the package: Documentation/function_map.json, drawn by make_function_map.py
   (function_map.svg and the interactive function_map.html).

   For every public name ($publicNames in 08_footer.wl) it records
   - its category: the block of 00_header.wl in which its usage message stands;
   - its kind: "function", or "data" for an inert head or symbol that stands for a mathematical object (IntegralOver,
     LoopMomentum, GammaMatrix, ...): a name without definitions, or whose definitions only normalize it (they give the
     name again, a number, or an algebraic expression of their arguments);
   - the public names it uses: those that occur in its definitions (OwnValues, DownValues, UpValues, SubValues, NValues) and,
     transitively, in the definitions of the private helpers reached from them, stopping at public names. The
     definitions are read without being evaluated. Names bound in a definition (pattern variables, the variables of
     Module, With, Block and Function, iterators of Table, Do, Sum and Product) are not references. Formatting rules
     (FormatValues) and options are not followed.
   - a size: the private functions reached in this way (helpers; private tables and state variables are followed but
     not counted) and the leaf count of all these definitions;
   - its usage message and the source file that defines it (for a data head without definitions, the file of its group).
   A public function that occurs as a caller tag is not a reference: the argument of a private helper that names the
   caller in messages, the default of an optional argument, or an operand of SameQ and UnsameQ (caller === MatsubaraSum).

   Run from any folder:  wolfram -script Development/make_function_map.wl
   Then:                 python3 Development/make_function_map.py *)

$RecursionLimit = 10^5;
root = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root, "FunctionalDiagrams.wl"}]];
publicNames = FunctionalDiagrams`Private`$publicNames;
privateContext = "FunctionalDiagrams`Private`";
heldPublic = ToExpression["FunctionalDiagrams`" <> #, InputForm, HoldComplete] & /@ publicNames;
publicIndex = AssociationThread[heldPublic, publicNames];

(* ---------------------------------------------------------------- categories: the blocks of 00_header.wl *)
category = None; categoryOf = <||>; categories = {};
Do[Which[
   StringMatchQ[line, RegularExpression["\\(\\* -{3,} .* \\*\\)\\s*"]],
   category = StringTrim[StringReplace[line, {RegularExpression["^\\(\\* -+ "] -> "", RegularExpression["\\*\\)\\s*$"] -> ""}]];
   AppendTo[categories, category],
   category =!= None && StringMatchQ[line, RegularExpression["[A-Za-z][A-Za-z0-9]*::usage.*"]],
   categoryOf[First[StringSplit[line, "::"]]] = category],
  {line, ReadList[FileNameJoin[{root, "Development", "src", "00_header.wl"}], String]}];
categories = Select[categories, MemberQ[Values[categoryOf], #] &];

(* ---------------------------------------------------------------- free symbols of a held expression *)
(* symbols are handled as HoldComplete[symbol], so that nothing is evaluated; bound is a list of them. A symbol is an
   atom with head Symbol (an unevaluated Symbol["x"] has head Symbol too). The symbol of a message name f::tag is not a
   reference: helpers issue the messages of the functions they serve *)
ClearAll[freeSymbols, patternNames];
patternNames[held_] := Cases[held, Verbatim[Pattern][s_Symbol, _] :> HoldComplete[s], {0, Infinity}, Heads -> True];
freeSymbols[HoldComplete[s_Symbol], bound_] /; AtomQ[Unevaluated[s]] := If[MemberQ[bound, HoldComplete[s]], {}, {HoldComplete[s]}];
freeSymbols[HoldComplete[_MessageName], _] := {};
freeSymbols[HoldComplete[Verbatim[Pattern][_, p_]], bound_] := freeSymbols[HoldComplete[p], bound];
(* rules and assignments: the pattern variables of the left side are bound in both sides (conditions included) *)
freeSymbols[HoldComplete[(Rule | RuleDelayed | Set | SetDelayed | UpSet | UpSetDelayed)[lhs_, rhs_]], bound_] :=
  With[{b = Join[bound, patternNames[HoldComplete[lhs]]]}, Join[freeSymbols[HoldComplete[lhs], b], freeSymbols[HoldComplete[rhs], b]]];
freeSymbols[HoldComplete[(Module | With | Block)[{vars___}, body___]], bound_] := Module[{held = HoldComplete[vars], names, inits},
   names = Cases[held, (s_Symbol | (Set | SetDelayed)[s_Symbol, _]) /; AtomQ[Unevaluated[s]] :> HoldComplete[s], {1}];
   inits = Cases[held, (Set | SetDelayed)[_Symbol, init_] :> HoldComplete[init], {1}];
   Join[Join @@ (freeSymbols[#, bound] & /@ inits), freeSymbols[HoldComplete[{body}], Join[bound, names]]]];
freeSymbols[HoldComplete[Function[s_Symbol, body_, ___]], bound_] /; AtomQ[Unevaluated[s]] :=
  freeSymbols[HoldComplete[body], Append[bound, HoldComplete[s]]];
freeSymbols[HoldComplete[Function[{vars___}, body_, ___]], bound_] :=
  freeSymbols[HoldComplete[body], Join[bound, Cases[HoldComplete[vars], s_Symbol /; AtomQ[Unevaluated[s]] :> HoldComplete[s], {1}]]];
freeSymbols[HoldComplete[(Table | Do | Sum | Product)[body_, iterators__]], bound_] :=
  With[{b = Join[bound, Cases[HoldComplete[iterators], {s_Symbol, ___} /; AtomQ[Unevaluated[s]] :> HoldComplete[s], {1}]]},
   Join[freeSymbols[HoldComplete[{iterators}], b], freeSymbols[HoldComplete[body], b]]];
(* a public function given as an argument to a private helper, or as the default of an optional argument, is the caller
   named in messages and options (checkOptionNames[QEDIntegrand, ...], caller_Symbol : EvaluateMatsubara), not a call:
   callerTags, the public functions, is set once their kinds are known *)
callerTags = {};
freeSymbols[HoldComplete[Verbatim[Optional][p_, d_]], bound_] :=
  Join[freeSymbols[HoldComplete[p], bound], If[MemberQ[callerTags, HoldComplete[d]], {}, freeSymbols[HoldComplete[d], bound]]];
(* so is a public function compared with SameQ or UnsameQ: caller === MatsubaraSum only chooses the message to issue *)
freeSymbols[HoldComplete[(SameQ | UnsameQ)[args___]], bound_] :=
  Join @@ Table[With[{part = Extract[HoldComplete[{args}], {1, i}, HoldComplete]},
     If[MemberQ[callerTags, part], {}, freeSymbols[part, bound]]], {i, Length[Unevaluated[{args}]]}];
freeSymbols[HoldComplete[e_], bound_] := With[{helperCall = MatchQ[Extract[HoldComplete[e], {1, 0}, HoldComplete],
      HoldComplete[s_Symbol] /; AtomQ[Unevaluated[s]] && Context[s] === privateContext]},
   Join @@ Table[With[{part = Extract[HoldComplete[e], {1, i}, HoldComplete]},
      If[helperCall && i > 0 && MemberQ[callerTags, part], {}, freeSymbols[part, bound]]], {i, 0, Length[Unevaluated[e]]}]];

definitionsOf[HoldComplete[s_Symbol]] := Join[OwnValues[s], DownValues[s], UpValues[s], SubValues[s], NValues[s]];
contextOf[HoldComplete[s_Symbol]] := Context[s];
helperQ[h_] := contextOf[h] === privateContext && definitionsOf[h] =!= {};
(* a helper proper, as counted in the summary: a private function, not a table or a state variable (OwnValues) *)
privateFunctionQ[HoldComplete[s_Symbol]] := DownValues[s] =!= {} || SubValues[s] =!= {};
(* the public names and the private helpers that occur free in the definitions of h *)
referenceCache = <||>;
references[h_HoldComplete] := If[KeyExistsQ[referenceCache, h], referenceCache[h],
   referenceCache[h] = DeleteDuplicates[Select[Join @@ (freeSymbols[HoldComplete[#], {}] & /@ definitionsOf[h]),
       KeyExistsQ[publicIndex, #] || helperQ[#] &]]];

(* the public names reached from p through private helpers only, and the helpers *)
reach[p_HoldComplete] := Module[{seen = <|p -> True|>, queue = {p}, current, uses = {}, helpers = {}},
   While[queue =!= {},
    current = First[queue]; queue = Rest[queue];
    Do[If[! KeyExistsQ[seen, r], seen[r] = True;
       If[KeyExistsQ[publicIndex, r], AppendTo[uses, r], AppendTo[helpers, r]; AppendTo[queue, r]]],
     {r, references[current]}]];
   {uses, helpers}];

(* a chain of helpers from p to the public name q, for the summary *)
helperPath[p_HoldComplete, q_HoldComplete] := Module[{parent = <|p -> None|>, queue = {p}, current, path},
   While[queue =!= {} && ! KeyExistsQ[parent, q],
    current = First[queue]; queue = Rest[queue];
    Do[If[! KeyExistsQ[parent, r], parent[r] = current; If[! KeyExistsQ[publicIndex, r], AppendTo[queue, r]]],
     {r, references[current]}]];
   If[! KeyExistsQ[parent, q], {}, path = {q}; While[parent[First[path]] =!= None, PrependTo[path, parent[First[path]]]];
    Replace[#, HoldComplete[s_] :> SymbolName[Unevaluated[s]]] & /@ path]];

(* ---------------------------------------------------------------- kind *)
(* a definition that only normalizes the name: its right side is the name again, or is built from its arguments and
   numbers with Plus, Times and Power alone *)
normalizingQ[h : HoldComplete[s_Symbol], rule_] := Module[{rhs = Extract[HoldComplete[rule], {1, 2}, HoldComplete], args},
   args = patternNames[Extract[HoldComplete[rule], {1, 1}, HoldComplete]];
   rhs = rhs /. HoldComplete[Verbatim[Condition][r_, _]] :> HoldComplete[r];
   MatchQ[rhs, HoldComplete[s[___]]] || SubsetQ[Join[args, {HoldComplete[Plus], HoldComplete[Times], HoldComplete[Power],
       HoldComplete[Rational], HoldComplete[Complex], HoldComplete[Integer], HoldComplete[Real]}],
     DeleteDuplicates[freeSymbols[rhs, {}]]]];
usageOf[name_String] := ToExpression[name <> "::usage"];
kindOf[h : HoldComplete[s_Symbol]] := If[AllTrue[Join[DownValues[s], SubValues[s]], normalizingQ[h, #] &], "data", "function"];

(* ---------------------------------------------------------------- source file of a name *)
(* the files of the definitions (not 00_header.wl, the usage messages; 08_footer.wl, which defines the update check, is
   not counted for the mentions: it names every public name in $publicNames) *)
sourceFiles = Select[FileNames["*.wl", FileNameJoin[{root, "Development", "src"}]], FileNameTake[#] =!= "00_header.wl" &];
sourceTexts = Association[(FileNameTake[#] -> Import[#, "Text"]) & /@ sourceFiles];
(* the file with its first definition; a data head without definitions (LoopMomentum) gets the file in which most of
   the names of its group are defined, or else the one that mentions it most often *)
definitionFile[name_String] := SelectFirst[Keys[sourceTexts], StringContainsQ[sourceTexts[#],
     RegularExpression["(?m)^(" <> name <> "\\[|" <> name <> " /:|SetAttributes\\[" <> name <> ",)"]] &, None];
fileOf[name_String] := Module[{definition = definitionFile[name], group, mentions},
   group = DeleteCases[definitionFile /@ Select[publicNames, categoryOf[#] === categoryOf[name] &], None];
   mentions = Table[{StringCount[sourceTexts[f], RegularExpression["\\b" <> name <> "\\b"]], f},
     {f, DeleteCases[Keys[sourceTexts], "08_footer.wl"]}];
   Which[definition =!= None, definition, group =!= {}, First[Commonest[group]],
    Max[mentions[[All, 1]]] > 0, Last[First[SortBy[mentions, -First[#] &]]], True, "00_header.wl"]];

(* ---------------------------------------------------------------- the map *)
kinds = AssociationThread[heldPublic, kindOf /@ heldPublic];
callerTags = Select[heldPublic, kinds[#] === "function" &];
nodes = Table[Module[{name = publicIndex[h], uses, helpers},
    {uses, helpers} = reach[h];
    <|"name" -> name, "category" -> categoryOf[name], "kind" -> kinds[h],
     "uses" -> Sort[publicIndex /@ uses],
     "helpers" -> Count[helpers, _?privateFunctionQ],
     "leafCount" -> Total[LeafCount /@ (definitionsOf /@ Prepend[helpers, h])],
     "file" -> fileOf[name],
     "usage" -> usageOf[name]|>], {h, heldPublic}];
usedBy = GroupBy[Flatten[Table[{u, n["name"]}, {n, nodes}, {u, n["uses"]}], 1], First -> Last];
nodes = Table[Append[n, "usedBy" -> Sort[Lookup[usedBy, n["name"], {}]]], {n, nodes}];
edges = Flatten[Table[{n["name"], u}, {n, nodes}, {u, n["uses"]}], 1];

map = <|"package" -> "FunctionalDiagrams", "version" -> FunctionalDiagrams`$FunctionalDiagramsVersion,
   "description" -> "Public names of the package, their categories (the blocks of Development/src/00_header.wl) and the public names they use (edges from user to used). Generated by Development/make_function_map.wl.",
   "categories" -> categories, "nodes" -> nodes, "edges" -> edges|>;
output = FileNameJoin[{root, "Documentation", "function_map.json"}];
Export[output, map, "RawJSON", "Compact" -> 2];

(* ---------------------------------------------------------------- summary *)
Print["public names: ", Length[nodes], " (", Count[nodes, n_ /; n["kind"] === "function"], " functions, ",
  Count[nodes, n_ /; n["kind"] === "data"], " data heads); edges: ", Length[edges]];
If[Length[nodes] =!= Length[publicNames] || MemberQ[nodes[[All, "category"]], _Missing],
  Print["WARNING: names without a category: ", Select[nodes, MissingQ[#["category"]] &][[All, "name"]]]];
Do[With[{ns = Select[nodes, #["category"] === c &]},
   Print[StringPadRight[c, 42], Length[ns], " names, ", Total[Length /@ ns[[All, "uses"]]], " edges out"]], {c, categories}];
Print["data heads: ", Select[nodes, #["kind"] === "data" &][[All, "name"]]];
Print["most used: ", Take[SortBy[{#["name"], Length[#["usedBy"]]} & /@ nodes, -Last[#] &], 12]];
Print["largest: ", Take[SortBy[{#["name"], #["helpers"], #["leafCount"]} & /@ nodes, -Last[#] &], 8]];
Print["a sample of edges, with the helpers through which they go (checked against the code):"];
Do[With[{path = helperPath @@ (ToExpression["FunctionalDiagrams`" <> #, InputForm, HoldComplete] & /@ pair)},
   Print["  ", pair[[1]], " -> ", pair[[2]], ": ", If[path === {}, "no edge", StringRiffle[path, " > "]]]],
  {pair, {{"DrawDiagram", "DiagramPlot"}, {"IrreducibleVertex", "SelfEnergy"}, {"SelfEnergy", "FunctionalDerivative"},
    {"LuttingerWardFunctional", "ParticleIrreducibleDiagrams"}, {"HubbardStratonovich", "DeclareSymmetric"},
    {"FieldDerivative", "GrassmannProduct"}, {"EpsilonExpand", "BubbleIntegral"}, {"CombineParameterIntegrals", "GammaMatrix"},
    {"QEDIntegrand", "LoopFrequency"}, {"MatsubaraSum", "LoopFrequency"}, {"DiagramIntegrand", "EvaluateMatsubara"}}}];
Print["written: ", output];
