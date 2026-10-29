
(* ========================================================================= *)
(*  1. Notation and declarations                                             *)
(* ========================================================================= *)

SetAttributes[DeltaFunction, Orderless];

(* displayed forms, in StandardForm and TraditionalForm: Interpretation keeps them copyable as input. They are not
   given for OutputForm (a terminal session), where Interpretation is not a typesetting construct and its second
   argument would be formatted again, endlessly: there the expressions print as input *)
SetAttributes[defineDisplayForm, HoldAll];
defineDisplayForm[lhs_, rhs_] := (Format[lhs, StandardForm] := rhs; Format[lhs, TraditionalForm] := rhs;);
defineDisplayForm[DeltaFunction[x_, y_], Interpretation[Subscript["\[Delta]", Row[{x, y}]], DeltaFunction[x, y]]];
defineDisplayForm[GrassmannProduct[a__], Interpretation[Row[{a}, "\[CenterDot]"], GrassmannProduct[a]]];
defineDisplayForm[IntegralOver[f_, v_List], Interpretation[Row[{Subscript["\[Integral]", Row[v, ","]], " ", f}], IntegralOver[f, v]]];
defineDisplayForm[DummyVariable[depth_Integer, k_Integer],
  Interpretation[Subscript[{"z", "w", "u", "v", "t"}[[Min[depth, 5]]], k], DummyVariable[depth, k]]];
Format[DummyVariable[depth_Integer, k_Integer], OutputForm] := {"z", "w", "u", "v", "t"}[[Min[depth, 5]]] <> ToString[k];

(* a single integration variable may be given without braces, as in Integrate *)
IntegralOver[f_, v_Symbol] := IntegralOver[f, {v}];

(* ---- the declaration registry ---- *)
$bosonicSources = {}; $grassmannSources = {}; $grassmannOddHeads = {}; $gradedHeads = {};
$symmetricHeads = {}; $antisymmetricHeads = {}; $derivativeRules = {}; $derivativePairs = {};
$discreteIndexValues = {}; $fieldSourceMap = {}; $lineStyles = {}; $displayNames = {}; $localVertexHeads = {};
$blobDeclarations = {}; $interactionLineHeads = {}; $couplingSymbols = {};

(* the symmetries are undone too: the attribute Orderless of symmetric heads and the sign rules of antisymmetric ones *)
ResetDeclarations[] := (
   (* the rules of the tree expansion (ConnectedFunctionFromVertices: FullPropagator, VertexFunction), section 13 *)
   clearTreeRules[];
   Scan[ClearAttributes[#, Orderless] &, $symmetricHeads];
   Scan[(DownValues[#] = Select[DownValues[#], FreeQ[#, hasDuplicatesQ | permutationSign] &]) &, $antisymmetricHeads];
   $bosonicSources = {}; $grassmannSources = {}; $grassmannOddHeads = {}; $gradedHeads = {};
   $symmetricHeads = {}; $antisymmetricHeads = {}; $derivativeRules = {}; $derivativePairs = {};
   $discreteIndexValues = {}; $fieldSourceMap = {}; $lineStyles = {}; $displayNames = {}; $localVertexHeads = {}; $blobDeclarations = {};
   $interactionLineHeads = {}; $couplingSymbols = {}; $contractionCache = {}; $matsubaraFrequencies = {};
   $superfieldTraceComponents = {};
   defineTreeDisplayForms[];);

(* a warning for heads that are built-in symbols (for instance GreenFunction in Mathematica 13 and later): their own
   definitions and messages interfere with the package *)
FunctionalDiagrams::builtin = "`1` is a built-in symbol of the Wolfram Language, whose own definitions interfere with the package: use another name.";
warnIfBuiltin[h_Symbol] := If[Context[h] === "System`", Message[FunctionalDiagrams::builtin, h]];
warnIfBuiltin[_] := Null;

DeclareSource[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $bosonicSources = Union[$bosonicSources, {heads}];);
(* fields declared before their Grassmann source become odd now: the order of the declarations does not matter *)
DeclareGrassmannSource[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $grassmannSources = Union[$grassmannSources, {heads}];
   Scan[If[MemberQ[{heads}, #[[2]]], DeclareGrassmannOdd[#[[1]]]] &, $fieldSourceMap]; $contractionCache = {};);
DeclareGrassmannOdd[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $grassmannOddHeads = Union[$grassmannOddHeads, {heads}];);
DeclareGrassmannGraded[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $gradedHeads = Union[$gradedHeads, {heads}];);

DeclareSymmetric[heads__Symbol] := Scan[(warnIfBuiltin[#]; SetAttributes[#, Orderless];
     $symmetricHeads = Union[$symmetricHeads, {#}]) &, {heads}];

DeclareAntisymmetric[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; Scan[defineAntisymmetric, {heads}]);
defineAntisymmetric[head_Symbol] := (
   head[args___] /; hasDuplicatesQ[{args}] := 0;
   head[args___] /; Sort[{args}] =!= {args} := permutationSign[{args}] (head @@ Sort[{args}]);
   $antisymmetricHeads = Union[$antisymmetricHeads, {head}];);

(* discrete index values: the test uses SameQ, so that a pattern such as x_ is never taken for an index value *)
discreteIndexValueQ[x_] := AnyTrue[$discreteIndexValues, # === x &];
DeclareDiscreteIndexValues[values_List] := (
   $discreteIndexValues = Union[$discreteIndexValues, values];
   DeltaFunction[x_, y_] /; discreteIndexValueQ[x] && discreteIndexValueQ[y] := If[x === y, 1, 0];);

DeclareField[field_Symbol, source_Symbol, sign_ : 1] := (
   warnIfBuiltin[field];
   $fieldSourceMap = Append[DeleteCases[$fieldSourceMap, {field, _, _}], {field, source, sign}];
   If[MemberQ[$grassmannSources, source], DeclareGrassmannOdd[field]];
   $contractionCache = {};);

SetAttributes[DeclareFunctionalDerivative, HoldAll];
DeclareFunctionalDerivative[object_, source_, rhs_] := Module[{held = Hold[rhs], variables, new},
   variables = Union[Flatten[Cases[held, IntegralOver[_, v_List] :> v, Infinity]]];
   new = Table[Unique["FunctionalDiagrams`Private`d"], {Length[variables]}];
   held = held /. Thread[variables -> new];
   $derivativeRules = Append[$derivativeRules, RuleDelayed @@ Join[Hold[{object, source}], held]];
   $derivativePairs = Union[$derivativePairs, {{Head[object], Head[source]}}];];

$allowedLineStyles = {"Plain", "Arrow", "DoubleArrow", "DottedArrow", "Double", "Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted",
   "Zigzag", "Impurity", "DivergingArrows", "ConvergingArrows"};
DeclareLineStyle::style = "Unknown line style `1`; use one of `2`.";
DeclareLineStyle[head_Symbol, style_String] := If[! MemberQ[$allowedLineStyles, style],
   Message[DeclareLineStyle::style, style, $allowedLineStyles],
   warnIfBuiltin[head]; $lineStyles = Append[DeleteCases[$lineStyles, {head, _}], {head, style}];];
DeclareDirectedPropagator[heads__Symbol] := Scan[DeclareLineStyle[#, "Arrow"] &, {heads}];
(* interaction lines: kernels of S_int, drawn wavy (or in another style); in the weight written with a picture each one
   stands for minus its kernel *)
DeclareInteractionLine[heads__Symbol] := DeclareInteractionLine[heads, "Wavy"];
DeclareInteractionLine[heads__Symbol, style_String] := Scan[(DeclareLineStyle[#, style];
     If[MemberQ[$allowedLineStyles, style], $interactionLineHeads = Union[$interactionLineHeads, {#}]]) &, {heads}];
(* couplings: symbols such as lambda in S_int = lambda/4! Integral phi^4, absorbed into the vertices of the pictures *)
DeclareCoupling[couplings__Symbol] := (Scan[warnIfBuiltin, {couplings}]; $couplingSymbols = Union[$couplingSymbols, {couplings}];);

DeclareDisplayName[head_Symbol, name_] := If[Context[head] === "System`", warnIfBuiltin[head],
   $displayNames = Append[DeleteCases[$displayNames, {head, _}], {head, name}];
   defineDisplayForm[head[args___], Interpretation[Subscript[name, Row[{args}]], head[args]]];];

lineStyleOf[head_] := With[{m = Select[$lineStyles, #[[1]] === head &]}, If[m === {}, "Plain", m[[1, 2]]]];
directedStyleQ[style_] := MemberQ[{"Arrow", "DoubleArrow", "DottedArrow"}, style];
(* anomalous propagators: -<T psi(x) psi(y)> with two arrowheads pointing to its ends ("DivergingArrows": an arrow into
   each psi end), -<T psibar(x) psibar(y)> with two pointing to its middle ("ConvergingArrows"); with them the arrows run
   on through every vertex and turn only inside an anomalous line *)
anomalousStyleQ[style_] := MemberQ[{"DivergingArrows", "ConvergingArrows"}, style];
fermionStyleQ[style_] := directedStyleQ[style] || anomalousStyleQ[style];
interactionStyleQ[style_] := MemberQ[{"Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag", "Impurity"}, style];

(* local vertices: kernels such as the three-gluon vertex V3(x, y, z), whose labels carry different indices (Lorentz,
   colour) at one point of space-time; a picture draws them as one point, not as a blob *)
DeclareLocalVertex[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $localVertexHeads = Union[$localVertexHeads, {heads}];);
localVertexQ[f_] := ! AtomQ[f] && MemberQ[$localVertexHeads, Head[f]];

(* blobs: factors h[x1, ..., xn] drawn as a circle or a square with a label inside (a LaTeX string), even for n = 2, e.g.
   a self-energy; {head, label, shape, fill, axial ports} *)
$blobShapes = {"Circle", "Box", "Triangle", "TriangleUp", "Diamond"}; $blobFills = {"Hatched", "CrossHatched", "Dotted", "Grey", "White"};
DeclareBlob::opt = "Unknown value `1` of the option `2` of DeclareBlob; use one of `3`.";
DeclareBlob[head_Symbol, label_String, opts___Rule] := Module[
   {shape = optionValue[{opts}, {"Shape" -> "Circle"}, "Shape"], fill = optionValue[{opts}, {"Fill" -> "Hatched"}, "Fill"],
    axial = optionValue[{opts}, {"AxialPorts" -> Automatic}, "AxialPorts"]},
   Which[
    ! MemberQ[$blobShapes, shape], Message[DeclareBlob::opt, shape, "Shape", $blobShapes],
    ! MemberQ[$blobFills, fill], Message[DeclareBlob::opt, fill, "Fill", $blobFills],
    True, warnIfBuiltin[head];
    $blobDeclarations = Append[DeleteCases[$blobDeclarations, {head, ___}],
      {head, label, shape, fill, Replace[axial, Automatic :> Switch[shape, "TriangleUp", {1, 2}, "Diamond", {1, 3}, _, {}]]}]];];
declaredBlobHeadQ[h_] := MemberQ[Cases[$blobDeclarations, {g_, ___} :> g], h];
declaredBlobQ[f_] := ! AtomQ[f] && declaredBlobHeadQ[Head[f]];

(* ========================================================================= *)
(*  2. Small utilities                                                       *)
(* ========================================================================= *)

hasDuplicatesQ[list_List] := Length[Union[list]] < Length[list];

(* sign of the permutation performed by Sort: Ordering gives the positions in sorted order, ties by position *)
permutationSign[list_List] := Signature[Ordering[list]];

(* sort a list of Grassmann-odd atoms: {sign, sorted list}; sign 0 if an atom repeats (nilpotency) *)
sortGrassmannAtoms[list_List] := If[hasDuplicatesQ[list], {0, list}, {permutationSign[list], Sort[list]}];

factorList[c_] := If[Head[c] === Times, List @@ c, {c}];

(* the components merged with every set that meets them, one set after the other (the classes of the equivalence that
   the sets generate, when the components start as singletons); Complement sorts, the merged component comes last *)
mergeOverlapping[components_List, sets_List] := Fold[Function[{cs, s}, With[{hit = Select[cs, Intersection[#, s] =!= {} &]},
     Append[Complement[cs, hit], Union[Flatten[{hit, s}]]]]], components, sets];
numericFactor[e_] := Which[NumberQ[e], e, Head[e] === Times, Times @@ Select[List @@ e, NumberQ], True, 1];

sourceHeads[] := Join[$bosonicSources, $grassmannSources];
sourceAtomQ[a_] := ! AtomQ[a] && MemberQ[sourceHeads[], Head[a]];
grassmannSourceQ[s_] := ! AtomQ[s] && MemberQ[$grassmannSources, Head[s]];

(* Grassmann-odd atoms: Grassmann sources, declared odd quantities, graded objects with an odd number of arguments *)
grassmannOddAtomQ[a_] := ! AtomQ[a] && (MemberQ[$grassmannSources, Head[a]] || MemberQ[$grassmannOddHeads, Head[a]] ||
      (MemberQ[$gradedHeads, Head[a]] && OddQ[Length[a]]));

(* atoms that vanish at zero sources when they are odd *)
sourceDependentOddAtomQ[a_] := MemberQ[$grassmannSources, Head[a]] || MemberQ[$gradedHeads, Head[a]];

headsDependingOn[source_] := Union[{Head[source]}, Cases[$derivativePairs, {h_, Head[source]} :> h]];
dependsOnSourceQ[f_, source_] := Or @@ (! FreeQ[f, #[___]] & /@ headsDependingOn[source]);
declaredDerivativeAtomQ[f_] := ! AtomQ[f] && MemberQ[$derivativePairs[[All, 1]], Head[f]];

freshSymbols[n_Integer] := Table[Unique["FunctionalDiagrams`Private`x"], {n}];

(* option lookup that accepts strings and symbols as option names *)
optionKey[k_String] := k;
optionKey[k_Symbol] := SymbolName[k];
optionKey[k_] := ToString[k];
(* the first rule whose key is name (a string, a symbol of that name, or anything whose ToString is name), a string key
   tested first *)
optionValue[options_List, defaults_List, name_String] := With[{r = FirstCase[Flatten[options],
      rule : (Rule | RuleDelayed)[k_, _] /; (k === name || (! StringQ[k] && optionKey[k] === name)) :> rule, None, {1}]},
   If[r === None, name /. defaults, r[[2]]]];
