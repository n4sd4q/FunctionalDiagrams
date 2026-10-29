
(* ========================================================================= *)
(*  10. Classes of diagrams                                                  *)
(* ========================================================================= *)

(* connectivity of the diagram of a term: components of the points (external labels and integration variables) *)
connectedComponentsOfTerm[diagramTerm[c_, o_, d_], externals_List] := Module[{nodes = Join[externals, d], sets},
   sets = Select[Table[Select[nodes, ! FreeQ[f, #] &], {f, Join[factorList[c], o]}], # =!= {} &];
   mergeOverlapping[List /@ Select[nodes, ! FreeQ[{c, o}, #] &], sets]];

(* the canonical terms of e that pass test, a property of the diagram (unchanged by renaming the integration variables
   and by combining equal diagrams): the terms are selected before canonicalisation, so that only the selected ones are
   canonicalised. Terms with nested integrals (whose canonicalisation may change them) are tested again afterwards. *)
selectCanonicalTermList[terms_List, test_] := With[{nestedQ = (! FreeQ[#[[1]], IntegralOver] &)},
   Select[canonicalTermList[Select[terms, nestedQ[#] || test[#] &]], ! nestedQ[#] || test[#] &]];
selectCanonicalTerms[e_, test_] := selectCanonicalTermList[rawTermsOf[e], test];

ConnectedDiagrams[e_, externals_List] := termListToExpression[selectCanonicalTerms[e,
    Length[connectedComponentsOfTerm[#, externals]] == 1 &]];
DropVacuumBubbles[e_, externals_List] := termListToExpression[selectCanonicalTerms[e,
    AllTrue[connectedComponentsOfTerm[#, externals], Intersection[#, externals] =!= {} &] &]];
(* the canonical terms with a property of the diagram *)
selectTerms[e_, test_] := termListToExpression[selectCanonicalTerms[e, test]];

expandIntegerPowers[factors_List] := Flatten[If[Head[#] === Power && IntegerQ[#[[2]]] && #[[2]] > 0, Table[#[[1]], {#[[2]]}], {#}] & /@ factors];

(* ---- connectivity of graphs with nodes 1..n and edges {a, b} (a packed integer matrix), compiled (the virtual
   machine of Compile: no C compiler needed); a union-find over the edges, the roots found by their parent pointers ---- *)
$cutCompileOptions = {CompilationTarget -> "WVM", CompilationOptions -> {"InlineCompiledFunctions" -> True, "InlineExternalDefinitions" -> True}};
(* the classes of the points 1..n joined by the pairs: the root (smallest point) of the class of each point *)
cGlueRoots = Compile[{{n, _Integer}, {pairs, _Integer, 2}},
   Module[{parent = Range[n], ra = 0, rb = 0},
    Do[ra = pairs[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
     rb = pairs[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
     If[ra != rb, If[ra < rb, parent[[rb]] = ra, parent[[ra]] = rb]], {e, Length[pairs]}];
    Table[ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; ra, {v, n}]], CompilationTarget -> "WVM"];
(* the graph without the edges numbered s1 and s2 (0: none) is connected *)
cConnectedWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], components = n, ra = 0, rb = 0},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra; components--]], {e, Length[edges]}];
    components <= 1], CompilationTarget -> "WVM"];
(* every component of the graph without the edges s1, s2 holds a node marked in external (0 or 1 for each node) *)
cEveryComponentExternalWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {external, _Integer, 1}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], hasExternal = Table[0, {n}], ra = 0, rb = 0, ok = True},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra]], {e, Length[edges]}];
    Do[ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[external[[v]] == 1, hasExternal[[ra]] = 1], {v, n}];
    Do[If[parent[[v]] == v && hasExternal[[v]] == 0, ok = False], {v, n}];
    ok], CompilationTarget -> "WVM"];
(* without the edges s1, s2 the nodes of left (not empty) are in one component and those of right in another one *)
cSeparatesWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {left, _Integer, 1}, {right, _Integer, 1}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], ra = 0, rb = 0, rl = 0, rr = 0, ok = True},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra]], {e, Length[edges]}];
    rl = left[[1]]; While[parent[[rl]] != rl, rl = parent[[rl]]];
    rr = right[[1]]; While[parent[[rr]] != rr, rr = parent[[rr]]];
    If[rl == rr, ok = False];
    Do[If[ok, ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[ra != rl, ok = False]], {v, left}];
    Do[If[ok, ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[ra != rr, ok = False]], {v, right}];
    ok], CompilationTarget -> "WVM"];
(* the graph stays connected when any k <= 2 of the edges listed in cut are removed *)
cConnectedAfterCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {k, _Integer}},
   Module[{ok = True, m = Length[cut]},
    ok = cConnectedWithout[n, edges, 0, 0];
    If[ok && k >= 1, Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], 0]], {i, m}]];
    If[ok && k >= 2, Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], cut[[j]]]], {i, m}, {j, i + 1, m}]];
    ok], Evaluate[Sequence @@ $cutCompileOptions]];
(* the skeleton test of a connected graph: no single edge of cut disconnects it, and no pair of them leaves a component
   without an external node (hasExternal 1) or disconnects it (hasExternal 0) *)
cSkeletonCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {external, _Integer, 1}, {hasExternal, _Integer}},
   Module[{ok = True, m = Length[cut]},
    Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], 0]], {i, m}];
    Do[If[ok, ok = If[hasExternal == 0, cConnectedWithout[n, edges, cut[[i]], cut[[j]]],
        cEveryComponentExternalWithout[n, edges, external, cut[[i]], cut[[j]]]]], {i, m}, {j, i + 1, m}];
    ok], Evaluate[Sequence @@ $cutCompileOptions]];
(* some k <= 2 of the edges listed in cut separate left from right *)
cSeparatedByCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {left, _Integer, 1}, {right, _Integer, 1}, {k, _Integer}},
   Module[{found = False, m = Length[cut]},
    If[k >= 1, Do[If[! found, found = cSeparatesWithout[n, edges, left, right, cut[[i]], 0]], {i, m}]];
    If[k >= 2, Do[If[! found, found = cSeparatesWithout[n, edges, left, right, cut[[i]], cut[[j]]]], {i, m}, {j, i + 1, m}]];
    found], Evaluate[Sequence @@ $cutCompileOptions]];
(* the edges as the compiled functions take them; a self-loop, which joins nothing, stands for no edge (n >= 1) *)
packedEdges[edges_List] := Developer`ToPackedArray[If[edges === {}, {{1, 1}}, edges]];

(* The graph used for cuts. Points are the integration variables and the external labels; factors that are not lines
   (vertices, interaction lines, delta functions) glue their points into one node. An external label that ends a single
   line coming from an integration variable is an external leg: the leg is not a line of the graph, and the node it
   ends on is marked as external. Any other external label is a point of the graph (amputated vertex functions).
   Result: {number of nodes n, edges {i, j} between the nodes 1..n (in the order of the lines), the external nodes,
   rules external label -> node}. The points are numbered by an association, the arguments of all the factors looked up
   at once, the points glued by a compiled union-find; the nodes are numbered in the order of their first point. *)
FunctionalDiagrams::lineargs = "Lines are functions of two labels, but `1` has `2` arguments, and it is treated as a vertex: put internal indices such as spin into the head (Gup, Gdown) or sum over them explicitly.";
graphForCuts[diagramTerm[c_, o_, d_], externals_List, lineHeads_List] := Module[{factors = Join[factorList[c], o], compound,
    lengths, lineHead, occurrences, legEnds = {}, legFactors = {}, vertexExternals, points, np, index, kept, keptLengths, args,
    lineQ, glue, roots, seen, present, presentRoots, renumber, node, rules},
   If[! FreeQ[factors, Power], factors = expandIntegerPowers[factors]];
   compound = Pick[factors, AtomQ /@ factors, False];
   lengths = Length /@ compound;
   lineHead = (MemberQ[lineHeads, #] &) /@ (Head /@ compound);
   Do[If[lengths[[i]] =!= 2, Message[FunctionalDiagrams::lineargs, userForm[compound[[i]]], lengths[[i]]]], {i, Pick[Range[Length[compound]], lineHead]}];
   (* external legs *)
   Do[occurrences = DeleteCases[Union[Position[factors, e, {1, Infinity}][[All, 1]]], 0];
    If[Length[occurrences] == 1, With[{f = factors[[occurrences[[1]]]]},
      If[! AtomQ[f] && MemberQ[lineHeads, Head[f]] && Length[f] == 2 && Count[List @@ f, e] == 1 &&
        MemberQ[d, First[DeleteCases[List @@ f, e]]],
       AppendTo[legEnds, {e, First[DeleteCases[List @@ f, e]]}]; AppendTo[legFactors, occurrences[[1]]]]]], {e, externals}];
   vertexExternals = Complement[externals, legEnds[[All, 1]]];
   points = Join[vertexExternals, d]; np = Length[points];
   index = AssociationThread[points -> Range[np]];
   (* the factors of the graph: not atoms, not legs *)
   If[legFactors === {}, kept = compound; keptLengths = lengths,
    With[{mask = (! MemberQ[legFactors, #] &) /@ Pick[Range[Length[factors]], AtomQ /@ factors, False]},
     kept = Pick[compound, mask]; keptLengths = Pick[lengths, mask]; lineHead = Pick[lineHead, mask]]];
   (* the point of every argument of every factor (0: not a point); a line: a line head with two points *)
   args = TakeList[Lookup[index, Join @@ (List @@@ kept), 0], keptLengths];
   lineQ = MapThread[#1 && #2 == 2 && FreeQ[#3, 0] &, {lineHead, keptLengths, args}];
   glue = Join @@ (With[{l = DeleteCases[#, 0]}, If[Length[l] < 2, {}, Thread[{First[l], Rest[l]}]]] &) /@ Pick[args, lineQ, False];
   roots = If[glue === {}, Range[np], cGlueRoots[np, glue]];
   (* the nodes: the classes of the points that occur *)
   seen = DeleteCases[Union[Flatten[args]], 0];
   present = If[Length[seen] == np, seen, Union[seen, Select[Complement[Range[np], seen], ! FreeQ[{c, o}, points[[#]]] &]]];
   presentRoots = DeleteDuplicates[roots[[present]]];
   renumber = ConstantArray[0, np]; If[presentRoots =!= {}, renumber[[presentRoots]] = Range[Length[presentRoots]]];
   node = renumber[[roots[[index[#]]]]] &;
   rules = Join[(# -> node[#] &) /@ Select[vertexExternals, ! FreeQ[{c, o}, #] &], (#[[1]] -> node[#[[2]]] &) /@ legEnds];
   {Length[presentRoots], Partition[renumber[[roots[[Flatten[Pick[args, lineQ]]]]]], 2], Union[rules[[All, 2]]], rules}];

(* connected components of a graph with nodes 1..n *)
graphComponents[n_Integer, edges_List] := mergeOverlapping[List /@ Range[n], edges];

(* removable lines: self-loops never disconnect anything *)
cuttableLines[edges_List] := If[edges === {}, {}, Pick[Range[Length[edges]], Unitize[edges[[All, 1]] - edges[[All, 2]]], 1]];

particleIrreducibleTermQ[t_diagramTerm, n_Integer, externals_List, lineHeads_List] := Module[{nodes, edges, ext, rules, cut},
   {nodes, edges, ext, rules} = graphForCuts[t, externals, lineHeads];
   If[nodes == 0, Return[False]];
   cut = cuttableLines[edges];
   If[n <= 2, cConnectedAfterCuts[nodes, packedEdges[edges], cut, n],
    (* more than two lines: every subset, each tested by the compiled connectivity *)
    cConnectedWithout[nodes, packedEdges[edges], 0, 0] && AllTrue[Join @@ Table[Subsets[cut, {k}], {k, 1, Min[n, Length[cut]]}],
      cConnectedWithout[nodes, packedEdges[Delete[edges, List /@ #]], 0, 0] &]]];

(* the external labels must occur in the expression as arguments of its functions: a label that does not is a misprint,
   or the arguments are in the order of version 1 (line heads before labels) *)
FunctionalDiagrams::label = "The label `1` does not occur in the expression: check the labels and the order of the arguments (labels before line heads).";
FunctionalDiagrams::labelsymbol = "The label `1` is not a symbol: labels must be symbols (numbers are confused with the indices of the integration variables).";
checkLabels[e_, labels_List] := (Scan[If[Head[#] =!= Symbol, Message[FunctionalDiagrams::labelsymbol, #]] &, Flatten[labels]];
   If[e =!= 0, With[{leaves = Union[Level[e, {-1}]]},
     Scan[If[! MemberQ[leaves, #], Message[FunctionalDiagrams::label, #]] &, Flatten[labels]]]]);

ParticleIrreducibleQ[t_, n_Integer, externals_List, lineHeads_List] := With[{ts = canonicalTermsOf[t]},
   checkLabels[t, externals]; Length[ts] == 1 && particleIrreducibleTermQ[First[ts], n, externals, lineHeads]];
ParticleIrreducibleDiagrams[e_, n_Integer, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[selectCanonicalTerms[e, particleIrreducibleTermQ[#, n, externals, lineHeads] &]]);

(* Skeleton: one-particle irreducible, and no two lines cut off a part without external points (a self-energy
   insertion). For a two-point function with external legs this is the lemma "closing the legs gives a 2PI vacuum
   graph"; for amputated vertex functions it is the usual definition. *)
skeletonTermQ[t_diagramTerm, externals_List, lineHeads_List] := Module[{n, edges, ext, rules, marks},
   {n, edges, ext, rules} = graphForCuts[t, externals, lineHeads];
   If[n == 0 || ! cConnectedWithout[n, packedEdges[edges], 0, 0], Return[False]];
   marks = ConstantArray[0, n]; If[ext =!= {}, marks[[ext]] = 1];
   (* without external points (vacuum diagrams) a skeleton is two-particle irreducible *)
   cSkeletonCuts[n, packedEdges[edges], cuttableLines[edges], marks, If[ext === {}, 0, 1]]];

SkeletonDiagrams[e_, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[selectCanonicalTerms[e, skeletonTermQ[#, externals, lineHeads] &]]);

(* ---- closed loops of lines (fermion loops) ---- *)
(* closed components of directed lines {from, to, head}: every point has one incoming and one outgoing line *)
closedComponents[lines_List] := Module[{points = Union[Flatten[lines[[All, {1, 2}]]]], components, inDegree, outDegree},
   If[lines === {}, Return[{}]];
   components = graphComponents[Length[points], Map[First[First[Position[points, #, {1}]]] &, lines[[All, {1, 2}]], {2}]];
   inDegree[x_] := Count[lines, {_, x, _}]; outDegree[x_] := Count[lines, {x, _, _}];
   Select[points[[#]] & /@ components, AllTrue[#, inDegree[#] == 1 && outDegree[#] == 1 &] &]];
(* the loops of each head, also where several lines meet at a local vertex (Hubbard), and the loops made of lines of
   different heads *)
closedLoopCountOfTerm[diagramTerm[c_, o_, d_], lineHeads_List] := Module[{lines},
   lines = Cases[expandIntegerPowers[factorList[c]], f_ /; ! AtomQ[f] && MemberQ[lineHeads, Head[f]] && Length[f] == 2 :>
      {f[[2]], f[[1]], Head[f]}];
   Total[Table[Length[closedComponents[Select[lines, #[[3]] === h &]]], {h, lineHeads}]] +
    Count[closedComponents[lines], cmp_ /; Length[Union[Select[lines, MemberQ[cmp, #[[1]]] &][[All, 3]]]] > 1]];
ClosedLoopCount[t_, lineHeads_List] := With[{ts = canonicalTermsOf[t]}, If[Length[ts] == 1, closedLoopCountOfTerm[First[ts], lineHeads], $Failed]];
DropClosedLoops[e_, lineHeads_List] := termListToExpression[selectCanonicalTerms[e, closedLoopCountOfTerm[#, lineHeads] == 0 &]];

(* ---- amputation of the external legs ---- *)
amputateTerm[diagramTerm[c_, o_, d_], externals_List, lineHeads_List] := Module[{factors = expandIntegerPowers[factorList[c]],
    legs = {}, renamed = {}, deltas = 1, ok = True, occurrences, f, z, previous},
   Do[If[ok,
     occurrences = Select[Range[Length[factors]], ! FreeQ[factors[[#]], e] &];
     If[Length[occurrences] != 1 || ! FreeQ[o, e], ok = False,
      f = factors[[occurrences[[1]]]];
      If[AtomQ[f] || ! MemberQ[lineHeads, Head[f]] || Length[f] != 2 || Count[List @@ f, e] != 1, ok = False,
       z = First[DeleteCases[List @@ f, e]];
       If[! MemberQ[d, z] || MemberQ[legs, occurrences[[1]]], ok = False,
        AppendTo[legs, occurrences[[1]]];
        previous = Select[renamed, #[[1]] === z &];
        If[previous === {}, AppendTo[renamed, {z, e}], deltas = deltas DeltaFunction[e, previous[[1, 2]]]]]]]], {e, externals}];
   If[! ok, {},
    With[{rules = (#[[1]] -> #[[2]] &) /@ renamed},
     {diagramTerm[((Times @@ Delete[factors, List /@ legs]) /. rules) deltas, o /. rules, Complement[d, renamed[[All, 1]]]]}]]];

AmputateExternalLegs[e_, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[amputatedTerms[e, externals, lineHeads]]);
(* the canonical terms of the amputated diagrams: the raw terms are amputated (amputation commutes with the renaming of
   the integration variables) and brought into canonical form once *)
amputatedTerms[e_, externals_List, lineHeads_List] := canonicalTermList[Join @@ (amputateTerm[#, externals, lineHeads] &) /@ rawTermsOf[e]];

(* ========================================================================= *)
(*  11. The Luttinger-Ward functional, the self-energy and the vertices      *)
(* ========================================================================= *)

Options[LuttingerWardFunctional] = {"Method" -> Automatic};
LuttingerWardFunctional[z0_, interaction_, n_, rule_Rule, opts___Rule] := LuttingerWardFunctional[z0, interaction, n, {rule}, opts];
(* a full propagator without the symmetry of the bare one (G0 symmetric, G undeclared): G(x, y) and G(y, x) would be
   different terms *)
FunctionalDiagrams::rulesymmetry = "`1` is `2` and `3` is not: declare it (`4`[`3`]), or its terms with the labels exchanged are not recognized as equal.";
checkRuleSymmetry[rules_List] := Scan[Function[r, If[MatchQ[r, _Symbol -> _Symbol],
     Which[orderlessQ[r[[1]]] && ! orderlessQ[r[[2]]], Message[FunctionalDiagrams::rulesymmetry, r[[1]], "symmetric", r[[2]], DeclareSymmetric],
      MemberQ[$antisymmetricHeads, r[[1]]] && ! MemberQ[$antisymmetricHeads, r[[2]]],
      Message[FunctionalDiagrams::rulesymmetry, r[[1]], "antisymmetric", r[[2]], DeclareAntisymmetric]]]], rules];
(* several propagators made full at once (e.g. G and the screened interaction W of a Hubbard-Stratonovich field): the
   diagrams that stay connected when any two lines, of any of these kinds, are cut *)
LuttingerWardFunctional[z0_, interaction_, n_, rules : {__Rule}, opts___Rule] := (Scan[warnIfBuiltin, rules[[All, 2]]]; checkRuleSymmetry[rules];
  -connectedVacuumDiagrams[z0, interaction, n, {opts}, particleIrreducibleTermQ[#, 2, {}, rules[[All, 1]]] &] /. rules);

FunctionalDiagrams::statistics = "The option \"Statistics\" -> `1` is not one of `2`; \"Fermion\" is used.";
statisticsOption[opts_List, allowed_List : {"Fermion", "Boson"}] := With[{s = optionValue[opts, {"Statistics" -> "Fermion"}, "Statistics"]},
   If[MemberQ[allowed, s], s, Message[FunctionalDiagrams::statistics, s, allowed]; "Fermion"]];
(* "Boson" is for a real field, with a symmetric propagator *)
FunctionalDiagrams::realboson = "\"Statistics\" -> \"Boson\" is for real fields, with a symmetric propagator, and `1` is not symmetric: for complex bosons use \"ComplexBoson\".";
checkRealBoson[g_Symbol] := If[! MemberQ[Attributes[g], Orderless], Message[FunctionalDiagrams::realboson, g]];
(* "Superfield" is for a Grassmann field with an antisymmetric propagator: the Nambu field (psi, psibar), whose propagator
   calG(a, b) = -<T NambuField_a NambuField_b> has the normal block G and the anomalous blocks -<T psi psi>, -<T psibar psibar> *)
FunctionalDiagrams::superfield = "\"Statistics\" -> \"Superfield\" is for a Grassmann field with an antisymmetric propagator (DeclareAntisymmetric), and `1` is not antisymmetric.";
checkSuperfield[g_Symbol] := If[! MemberQ[$antisymmetricHeads, g], Message[FunctionalDiagrams::superfield, g]];

(* the default "Statistics" -> "Fermion" on a symmetric or antisymmetric G (the propagator of a real boson or of a
   superfield): a message *)
FunctionalDiagrams::statisticsdefault = "`1` is `2`, and \"Statistics\" is not given: `3` uses the fermionic convention (\[CapitalSigma](x, y) = \[Delta]\[CapitalPhi]/\[Delta]`1`(y, x), \[CapitalGamma]0(x1, x2; x3, x4) = \[Delta]\[CapitalSigma](x1, x2)/\[Delta]`1`(x4, x3)). For `4` give \"Statistics\" -> \"`5`\".";
checkStatisticsGiven[f_, g_, opts_List] := If[! MemberQ[optionKey /@ opts[[All, 1]], "Statistics"],
   Which[orderlessQ[g], Message[FunctionalDiagrams::statisticsdefault, g, "symmetric", f, "a real boson", "Boson"],
    MemberQ[$antisymmetricHeads, g], Message[FunctionalDiagrams::statisticsdefault, g, "antisymmetric", f, "a superfield", "Superfield"]]];
FunctionalDiagrams::vertexlabels = "`1` takes the list of its `2` labels, `3`; `4` was given.";

(* derivative with respect to G[x, y], G being treated as a source; for a symmetric G (bosons) the derivative is
   taken with respect to an unsymmetrised copy and then symmetrised; for an antisymmetric G (a superfield propagator,
   mode "Antisymmetric") it is antisymmetrised, (d - d(x <-> y))/2, the counterpart of dG(u, v)/dG(x, y) =
   (delta(u, x) delta(v, y) - delta(u, y) delta(v, x))/2 *)
$kernelCopies = {};
derivativeWithRespectToPropagator[e_, g_Symbol, {x_, y_}, mode_] := Module[{copy = Unique["FunctionalDiagrams`Private`g"], d},
   Block[{$bosonicSources = Union[$bosonicSources, {copy}], $kernelCopies = Append[$kernelCopies, copy -> g]},
    d = FunctionalDerivative[e /. g -> copy, copy[x, y]];
    Switch[mode,
     True, d = SimplifyDiagrams[(d + (d /. {x -> y, y -> x}))/2],
     "Antisymmetric", d = SimplifyDiagrams[(d - (d /. {x -> y, y -> x}))/2]]];
   d /. copy -> g];
(* the mode of the derivative with respect to a kernel g *)
propagatorDerivativeMode[g_Symbol] := Which[orderlessQ[g], True, MemberQ[$antisymmetricHeads, g], "Antisymmetric", True, False];

(* G = G0 + G0 Sigma G in all cases: fermions (G = -<psi psibar>) Sigma(x, y) = dPhi/dG(y, x); real bosons (G = <phi phi>,
   symmetric) Sigma = -2 dPhi/dG(x, y); complex bosons (G = <phi phibar>) Sigma(x, y) = -dPhi/dG(y, x); superfields
   (calG = -<NambuField NambuField>, antisymmetric) Sigma = -2 dPhi/dcalG(x, y), the derivative antisymmetrised *)
Options[SelfEnergy] = {"Statistics" -> "Fermion"};
SelfEnergy[phi_, g_Symbol, labels_List, ___Rule] /; Length[labels] != 2 :=
  (Message[FunctionalDiagrams::vertexlabels, SelfEnergy, 2, "{x, y}", labels]; $Failed);
SelfEnergy[phi_, g_Symbol, {x_, y_}, opts___Rule] := (checkStatisticsGiven[SelfEnergy, g, {opts}]; selfEnergyOf[phi, g, {x, y}, opts]);
selfEnergyOf[phi_, g_Symbol, {x_, y_}, opts___Rule] := Switch[statisticsOption[{opts}, {"Fermion", "Boson", "ComplexBoson", "Superfield"}],
   "Boson", checkRealBoson[g]; SimplifyDiagrams[-2 derivativeWithRespectToPropagator[phi, g, {x, y}, True]],
   "Superfield", checkSuperfield[g]; SimplifyDiagrams[-2 derivativeWithRespectToPropagator[phi, g, {x, y}, "Antisymmetric"]],
   "ComplexBoson", SimplifyDiagrams[-derivativeWithRespectToPropagator[phi, g, {y, x}, False]],
   _, derivativeWithRespectToPropagator[phi, g, {y, x}, False]];

(* Gamma0(x1, x2; x3, x4) = dSigma(x1, x2)/dG(x4, x3) for fermions and complex bosons; -2 dSigma(x1, x2)/dG(x3, x4) for
   real bosons and superfields (antisymmetrised for these) *)
Options[IrreducibleVertex] = {"Statistics" -> "Fermion", "Channel" -> Automatic};
FunctionalDiagrams::channel = "\"Channel\" -> `1` is not one of `2` for \"Statistics\" -> `3`.";
FunctionalDiagrams::ppkernel = "The kernel of the particle-particle channel is a derivative with respect to the anomalous propagators -<T psi psi> and -<T psibar psibar>, not with respect to G: use the superfield (psi, psibar) (\"Statistics\" -> \"Superfield\", \"Channel\" -> \"t\") and SuperfieldComponents.";
IrreducibleVertex[phi_, g_Symbol, labels_List, ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, IrreducibleVertex, 4, "{x1, x2, x3, x4}", labels]; $Failed);
IrreducibleVertex[phi_, g_Symbol, labels : {x1_, x2_, x3_, x4_}, opts___Rule] := Module[
   {statistics = statisticsOption[{opts}, {"Fermion", "Boson", "ComplexBoson", "Superfield"}],
    channel = optionValue[{opts}, Options[IrreducibleVertex], "Channel"], rest, allowed, first},
   checkStatisticsGiven[IrreducibleVertex, g, {opts}];
   rest = Append[Select[{opts}, ! MemberQ[{"Channel", "Statistics"}, optionKey[#[[1]]]] &], "Statistics" -> statistics];
   allowed = Switch[statistics, "Boson" | "Superfield", {"s", "t", "u", All}, "Fermion", {"ParticleHole", "TransverseParticleHole"}, _, {}];
   Which[
    channel === Automatic || channel === First[Append[allowed, None]], irreducibleVertexFirstChannel[phi, g, labels, statistics, rest],
    statistics === "Fermion" && MemberQ[{"ParticleParticle", All}, channel], Message[FunctionalDiagrams::ppkernel]; $Failed,
    ! MemberQ[allowed, channel], Message[FunctionalDiagrams::channel, channel, Prepend[allowed, Automatic], statistics]; $Failed,
    True,
    first = irreducibleVertexFirstChannel[phi, g, labels, statistics, rest];
    If[statistics === "Fermion",
     (* the transverse particle-hole channel (x1 x4|x3 x2): -Gamma0(x3, x2; x1, x4) *)
     SimplifyDiagrams[-(first /. {x1 -> x3, x3 -> x1})],
     (* Gamma_r(a, b, c, d) = sgn(pi_r) Gamma0(pi_r(a, b, c, d)): pi_t = (a c b d), odd, pi_u = (a d b c), even; the sign is +1
        for real bosons *)
     With[{sign = If[statistics === "Superfield", -1, 1]},
      With[{rules = {"s" -> first, "t" -> SimplifyDiagrams[sign (first /. {x2 -> x3, x3 -> x2})],
          "u" -> SimplifyDiagrams[first /. {x2 -> x4, x3 -> x2, x4 -> x3}]}},
       If[channel === All, Association[rules], channel /. rules]]]]]];
irreducibleVertexFirstChannel[phi_, g_, {x1_, x2_, x3_, x4_}, statistics_, opts_List] := Switch[statistics,
   "Boson", SimplifyDiagrams[-2 derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x3, x4}, True]],
   "Superfield", SimplifyDiagrams[-2 derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x3, x4}, "Antisymmetric"]],
   _, derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x4, x3}, False]];

Options[PerturbativeFullVertex] = {"Method" -> Automatic};
(* the orders of the vertex: n means 1 to n (the vertex has no zeroth order), {k} only k, {kmin, kmax} kmin to kmax *)
vertexOrders[n_Integer] := {1, n};
vertexOrders[{k_Integer}] := {k};
vertexOrders[{kmin_Integer, kmax_Integer}] := {Max[kmin, 1], kmax};
vertexOrders[n_] := n;
PerturbativeFullVertex[z0_, interaction_, n_, fields_List, labels_List, rules : (_Rule | {__Rule}), ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, PerturbativeFullVertex, 4, "{x1, x2, x3, x4}", labels]; $Failed);
(* several bare propagators made full at once (one per spin, or G and the screened interaction): a list of rules *)
PerturbativeFullVertex[z0_, interaction_, n_, fields_List, labels : {x1_, x2_, x3_, x4_}, rule : (_Rule | {__Rule}), opts___Rule] := Module[{correlation, sign, vertexTerms,
    rules = Flatten[{rule}]},
   Scan[warnIfBuiltin, rules[[All, 2]]]; checkRuleSymmetry[rules];
   If[Length[fields] == 2,
    (* fermions: <psi(1) psibar(2) psibar(4) psi(3)>_c = G G F G G *)
    correlation = ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[2]][x2], fields[[2]][x4], fields[[1]][x3]},
      interaction, vertexOrders[n], opts]; sign = 1,
    (* real bosons: F = Gamma^(4) = -(amputated connected four-point function) *)
    correlation = ConnectedCorrelationFunction[z0, fields[[1]] /@ labels, interaction, vertexOrders[n], opts]; sign = -1];
   (* SkeletonDiagrams[sign AmputateExternalLegs[correlation, ...], ...], with one canonical form instead of three *)
   checkLabels[correlation, labels];
   vertexTerms = amputatedTerms[sign correlation, labels, rules[[All, 1]]];
   checkLabels[If[vertexTerms === {}, 0, vertexTerms], labels];
   termListToExpression[Select[vertexTerms, skeletonTermQ[#, labels, rules[[All, 1]]] &]] /. rules];

Options[DysonSeries] = {"MaximumOrder" -> None};
dysonChain[pieces_List, {s1_, s2_}, g0_, {x_, y_}] := Module[{k = Length[pieces], as, bs},
   as = freshSymbols[k]; bs = freshSymbols[k];
   IntegralOver[g0[x, as[[1]]] Times @@ Table[(pieces[[i]] /. {s1 -> as[[i]], s2 -> bs[[i]]}) If[i < k, g0[bs[[i]], as[[i + 1]]], g0[bs[[i]], y]],
       {i, k}], Join[as, bs]]];
DysonSeries::zeroth = "The self-energy has a part of order 0 in `1`, which the chains of \"MaximumOrder\" leave out.";
DysonSeries[sigma_, {s1_, s2_}, g0_, {x_, y_}, m_Integer, opts___Rule] := Module[{maximum = optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"],
    total = g0[x, y], parts},
   If[maximum === None,
    Do[total = total + dysonChain[Table[sigma, {k}], {s1, s2}, g0, {x, y}], {k, m}],
    (* only the chains of total order <= n in the coupling x *)
    parts = orderParts[sigma, maximum[[1]], maximum[[2]]];
    If[First[parts] =!= 0, Message[DysonSeries::zeroth, maximum[[1]]]];
    parts = Rest[parts];
    Do[Do[If[Total[c] <= maximum[[2]] && ! MemberQ[parts[[c]], 0], total = total + dysonChain[parts[[c]], {s1, s2}, g0, {x, y}]],
       {c, Tuples[Range[maximum[[2]]], k]}], {k, Min[m, maximum[[2]]]}]];
   SimplifyDiagrams[total]];

(* ========================================================================= *)
(*  12. Two-particle channels, Bethe-Salpeter ladders and the parquet         *)
(* ========================================================================= *)

channelPairings["Fermion", {x1_, x2_, x3_, x4_}] := {"ParticleHole" -> {{x1, x2}, {x3, x4}},
   "TransverseParticleHole" -> {{x1, x4}, {x3, x2}}, "ParticleParticle" -> {{x1, x3}, {x2, x4}}};
channelPairings["Boson", {x1_, x2_, x3_, x4_}] := {"s" -> {{x1, x2}, {x3, x4}}, "t" -> {{x1, x3}, {x2, x4}}, "u" -> {{x1, x4}, {x2, x3}}};
(* a superfield: the channels of real bosons *)
channelPairings["Superfield", labels_List] := channelPairings["Boson", labels];

twoParticleReducibleTermQ[t_diagramTerm, pairs : {_List, _List}, lineHeads_List] := cutReducibleTermQ[t, pairs, 2, lineHeads];

TwoParticleReducibleQ[t_, pairs : {_List, _List}, lineHeads_List] := With[{ts = canonicalTermsOf[t]},
   checkLabels[t, pairs]; Length[ts] == 1 && twoParticleReducibleTermQ[First[ts], pairs, lineHeads]];

(* cutting n or fewer lines separates the external labels of the first group from those of the second *)
cutReducibleTermQ[t_diagramTerm, {left_List, right_List}, n_Integer, lineHeads_List] :=
  cutReducibleGraphQ[graphForCuts[t, Join[left, right], lineHeads], {left, right}, n];
(* the graph does not depend on the order of the external labels: it is computed once for all the channels *)
cutReducibleGraphQ[{m_Integer, edges_, ext_, rules_}, {left_List, right_List}, n_Integer] := Module[{cut, l, r},
   l = Union[left /. rules]; r = Union[right /. rules];
   If[Intersection[l, r] =!= {} || ! AllTrue[Join[l, r], IntegerQ], Return[False]];
   If[edges === {} || l === {} || r === {}, Return[False]];
   cut = cuttableLines[edges];
   If[n <= 2, cSeparatedByCuts[m, packedEdges[edges], cut, l, r, n],
    (* more than two lines: every subset, each tested by the compiled separation *)
    AnyTrue[Join @@ Table[Subsets[cut, {k}], {k, 1, Min[n, Length[cut]]}],
     cSeparatesWithout[m, packedEdges[Delete[edges, List /@ #]], l, r, 0, 0] &]]];
ReducibleDiagrams[e_, groups : {_List, _List}, n_Integer, lineHeads_List] := (checkLabels[e, groups];
   selectTerms[e, cutReducibleTermQ[#, groups, n, lineHeads] &]);
IrreducibleDiagrams[e_, groups : {_List, _List}, n_Integer, lineHeads_List] := (checkLabels[e, groups];
   selectTerms[e, ! cutReducibleTermQ[#, groups, n, lineHeads] &]);

Options[ParquetDecomposition] = {"Statistics" -> "Fermion"};
$parquetStatistics = {"Fermion", "Boson", "Superfield"};
(* the canonical terms of f by class: {"FullyIrreducible" -> terms, channel -> terms, ..., "SeveralChannels" -> terms} *)
parquetClassTerms[terms_List, channels_List, lineHeads_List, labels_List] := Module[{classes},
   classes = Table[With[{graph = graphForCuts[t, labels, lineHeads]},
      Select[channels, cutReducibleGraphQ[graph, #[[2]], 2] &][[All, 1]]], {t, terms}];
   Join[{"FullyIrreducible" -> Pick[terms, (# === {} &) /@ classes]},
    Table[ch[[1]] -> Pick[terms, (# === {ch[[1]]} &) /@ classes], {ch, channels}],
    {"SeveralChannels" -> Pick[terms, (Length[#] > 1 &) /@ classes]}]];

(* misuse: four labels, the heads of the lines in a list (one head alone is accepted), else a message *)
ParquetDecomposition::args = "ParquetDecomposition takes the vertex F, the list of its four external labels {x1, x2, x3, x4} and the list of the heads of its lines {G, ...}, then options; it was given `1` arguments with the heads `2`.";
ParquetDecomposition[f_, labels_List, lineHeads_List, ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, ParquetDecomposition, 4, "{x1, x2, x3, x4}", labels]; $Failed);
ParquetDecomposition[f_, labels_List, g_Symbol, opts___Rule] := ParquetDecomposition[f, labels, {g}, opts];
ParquetDecomposition[args___] /; ! MatchQ[{args}, {_, _List, _List | _Symbol, ___Rule}] :=
  (Message[ParquetDecomposition::args, Length[{args}], Head /@ {args}]; $Failed);
ParquetDecomposition[f_, labels_List, lineHeads_List, opts___Rule] := Module[{channels = channelPairings[statisticsOption[{opts}, $parquetStatistics], labels],
    terms = canonicalTermsOf[f]},
   checkLabels[f, labels];
   Association[(#[[1]] -> termListToExpression[#[[2]]] &) /@ parquetClassTerms[terms, channels, lineHeads, labels]]];

(* L(a, b, c, d): the vertex expression with its labels x1..x4 replaced simultaneously *)
relabelVertex[v_, labels_List, new_List] := v /. Thread[labels -> new];

Options[BetheSalpeterLadder] = {"Statistics" -> "Fermion", "MaximumOrder" -> None};
BetheSalpeterLadder[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol, opts___Rule] /;
   optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"] =!= None :=
  Module[{maximum = optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"], rest = Select[{opts}, optionKey[#[[1]]] =!= "MaximumOrder" &]},
   (* only the products of total order <= n *)
   ladderUpToOrder[orderParts[left, maximum[[1]], maximum[[2]]], orderParts[right, maximum[[1]], maximum[[2]]], labels, channel, g,
    statisticsOption[rest, $parquetStatistics]]];
BetheSalpeterLadder[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol, opts___Rule] :=
  SimplifyDiagrams[ladderIntegral[left, right, labels, channel, g, statisticsOption[{opts}, $parquetStatistics]]];

(* the parts of order 0, 1, ..., n of the canonical terms (as OrderPart, whose order of a term is termOrder; a term of
   symbolic order, x^m, is in none of them) *)
orderPartsOfTerms[terms_List, x_, headQ_, n_Integer] := With[{orders = termOrder[#, x, headQ] & /@ terms},
   Table[Pick[terms, (TrueQ[# == k] &) /@ orders], {k, 0, n}]];
(* OrderPart[e, x, k] for k = 0, ..., n, with one canonical form of e *)
orderParts[e_, x_, n_Integer] := termListToExpression /@ orderPartsOfTerms[canonicalTermsOf[e], x, ! FreeQ[e, x[___]], n];
(* the ladder of the parts of order k1 of the left vertex and k2 of the right one, k1 + k2 <= n (lists of the parts of
   order 0..n), brought into canonical form once *)
ladderUpToOrder[leftParts_List, rightParts_List, labels_List, channel_String, g_Symbol, statistics_ : "Fermion"] := With[{n = Length[leftParts] - 1},
   SimplifyDiagrams[Total[Flatten[Table[If[leftParts[[k1 + 1]] === 0 || rightParts[[k2 + 1]] === 0, 0,
        ladderIntegral[leftParts[[k1 + 1]], rightParts[[k2 + 1]], labels, channel, g, statistics]], {k1, 0, n}, {k2, 0, n - k1}]]]]];

(* superfields: the ladders of real bosons times -1, +1, -1 in the channels s, t, u (the Grassmann signs of the legs
   relabelled, checked by CheckParquetEquations) *)
ladderIntegral[left_, right_, labels_List, channel : ("s" | "t" | "u"), g_Symbol, "Superfield"] :=
  If[channel === "t", 1, -1] ladderIntegral[left, right, labels, channel, g];
ladderIntegral[left_, right_, labels_List, channel_String, g_Symbol, _String] := ladderIntegral[left, right, labels, channel, g];
ladderIntegral[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol] := Module[{p5, p6, p7, p8, l, r},
   {p5, p6, p7, p8} = freshSymbols[4];
   l[a_, b_, c_, d_] := relabelVertex[left, labels, {a, b, c, d}];
   r[a_, b_, c_, d_] := relabelVertex[right, labels, {a, b, c, d}];
   Switch[channel,
     (* fermions leave the vertices at positions 1 and 3 and enter at 2 and 4; G[a, b] runs from b to a. The signs of
        the transverse and particle-particle channels are the Grassmann signs of the relabelled legs (checked by
        CheckParquetEquations against the diagrams order by order). *)
     "ParticleHole", IntegralOver[l[x1, x2, p5, p6] g[p6, p7] g[p8, p5] r[p7, p8, x3, x4], {p5, p6, p7, p8}],
     "TransverseParticleHole", -IntegralOver[l[x1, p6, p5, x4] g[p6, p7] g[p8, p5] r[p7, x2, x3, p8], {p5, p6, p7, p8}],
     "ParticleParticle", -1/2 IntegralOver[l[x1, p5, x3, p6] g[p5, p7] g[p6, p8] r[p7, x2, p8, x4], {p5, p6, p7, p8}],
     (* real bosons *)
     "s", -1/2 IntegralOver[l[x1, x2, p5, p6] g[p5, p7] g[p6, p8] r[p7, p8, x3, x4], {p5, p6, p7, p8}],
     "t", -1/2 IntegralOver[l[x1, p5, x3, p6] g[p5, p7] g[p6, p8] r[p7, x2, p8, x4], {p5, p6, p7, p8}],
     "u", -1/2 IntegralOver[l[x1, p5, p6, x4] g[p5, p7] g[p6, p8] r[p7, x2, x3, p8], {p5, p6, p7, p8}]]];

Options[CheckParquetEquations] = {"Statistics" -> "Fermion"};
(* the canonical terms of f are computed once and split by class and by order: the canonical form of an expression made
   of canonical terms gives the same terms, and is not computed again *)
CheckParquetEquations[f_, labels_List, g_Symbol, x_, n_Integer, opts___Rule] := Module[{pairings, channels, terms, classTerms, part,
    rightParts, residuals, counts},
   pairings = channelPairings[statisticsOption[{opts}, $parquetStatistics], labels];
   channels = pairings[[All, 1]];
   terms = canonicalTermsOf[f];
   checkLabels[f, labels];
   classTerms = parquetClassTerms[terms, pairings, {g}, labels];
   part[key_] := key /. classTerms;
   rightParts = termListToExpression /@ orderPartsOfTerms[terms, x, ! FreeQ[f, x[___]], n];
   residuals = With[{rules = Table[ch -> Module[{left = Complement[terms, part[ch]], ladder, difference},
          ladder = ladderUpToOrder[termListToExpression /@ orderPartsOfTerms[left, x, ! FreeQ[left, x[___]], n], rightParts, labels, ch, g,
            statisticsOption[{opts}, $parquetStatistics]];
          difference = canonicalTermsOf[termListToExpression[part[ch]] - ladder];
          termListToExpression /@ Rest[orderPartsOfTerms[difference, x, ! FreeQ[difference, x[___]], n]]], {ch, channels}]},
     Association[rules]];
   counts = With[{rules = Table[key -> Length /@ Rest[orderPartsOfTerms[part[key], x, ! FreeQ[part[key], x[___]], n]],
        {key, Join[{"FullyIrreducible"}, channels, {"SeveralChannels"}]}]}, Association[rules]];
   With[{rules = {"Residuals" -> residuals, "DiagramCounts" -> counts,
       "SeveralChannels" -> termListToExpression[part["SeveralChannels"]], "FullyIrreducible" -> termListToExpression[part["FullyIrreducible"]]}},
    Association[rules]]];

(* ========================================================================= *)
(*  12a. Vertices of one-body operators and exact equations                  *)
(* ========================================================================= *)

(* The vertex of the density at z. Fermions: a potential U coupled to rho = psibar psi gives G^-1 = G0^-1 - U - Sigma, and
   Lambda(x1, x2; z) = -dG^-1(x1, x2)/dU(z), so that <psi(x1) psibar(x2) rho(z)>_c = dG(x1, x2)/dU(z) = G G Lambda. Real
   bosons: rho = phi^2/2, G^-1 = G0^-1 + U - Sigma and Lambda = dG^-1/dU, so that <phi(x1) phi(x2) rho(z)>_c = G G Lambda.
   In both cases Lambda = delta(x1, z) delta(x2, z) + corrections. The legs x1, x2 are removed and the skeleton diagrams
   kept; the bare term is added by hand (AmputateExternalLegs removes only legs that end at internal points). *)
Options[PerturbativeDensityVertex] = {"Method" -> Automatic};
PerturbativeDensityVertex[z0_, interaction_, n_, fields_List, {x1_, x2_, z_}, rule_Rule, opts___Rule] := Module[{correlation},
   warnIfBuiltin[rule[[2]]];
   correlation = If[Length[fields] == 2,
     ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[2]][x2], fields[[2]][z], fields[[1]][z]}, interaction, vertexOrders[n], opts],
     1/2 ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[1]][x2], fields[[1]][z], fields[[1]][z]}, interaction,
       vertexOrders[n], opts]];
   SimplifyDiagrams[DeltaFunction[x1, z] DeltaFunction[x2, z] +
     (SkeletonDiagrams[AmputateExternalLegs[correlation, {x1, x2}, {rule[[1]]}], {x1, x2, z}, {rule[[1]]}] /. rule)]];

(* a function of labels from an expression: LabelFunction[e, {x, y}][a, b] is e with x -> a, y -> b. The integration
   variables of e (DummyVariable[...] in canonical form) are renamed to fresh symbols first, so that an argument with the
   same name (a variable of an expression in canonical form) is not captured. The fresh symbols are made in the user's
   context, x$nn as those of Module, so that a printed result does not show the private context of the package *)
labelFunctionSymbols[n_Integer] := Table[Unique["x$"], {n}];
LabelFunction[e_, labels_List] := With[{vars = labelFunctionSymbols[Length[labels]], dummies = Union[Cases[e, _DummyVariable, Infinity]]},
   Function @@ {vars, e /. Thread[dummies -> labelFunctionSymbols[Length[dummies]]] /. Thread[labels -> vars]}];

(* The exact equations of many-body theory between full objects, given as heads (e.g. declared blobs, for the pictures)
   or as functions of their labels (LabelFunction of perturbative series, for the checks). Fermions leave at x1, x3 and
   enter at x2, x4; G(a, b) runs from b to a. The vertex equations take an optional "BareVertex" -> gamma, a function of
   three labels (the density, gamma(a, b; z) = delta(a, z) delta(b, z), by default; a current, a spin density). *)
FunctionalDiagrams::exactobject = "ExactEquation[`1`, ...] needs the object `2` (a rule \"`2`\" -> h).";
FunctionalDiagrams::exactname = "Unknown equation `1`; the equations are `2`.";
$exactEquations = {"Dyson", "BetheSalpeter", "Parquet", "SchwingerDyson", "Vertex", "VertexFromF", "SelfEnergyFromVertex", "Hedin",
   "Response", "Susceptibility"};
Options[ExactEquation] = {"Statistics" -> "Fermion", "Channel" -> Automatic, "MaximumOrder" -> None};
ExactEquation[name_String, objects_List, labels_List, opts___Rule] := Module[{statistics = statisticsOption[{opts}], ob, missing,
    a, b, c, d, u, v, w, g, gbar, gam, given, maximum = optionValue[{opts}, Options[ExactEquation], "MaximumOrder"], side},
   If[! MemberQ[$exactEquations, name], Message[FunctionalDiagrams::exactname, name, $exactEquations]; Return[$Failed]];
   missing = Select[Switch[name,
      "Dyson", {"G", "G0", "Sigma"}, "BetheSalpeter", {"F", "Gamma0", "G"},
      "Parquet", If[statistics === "Boson", {"F", "Lambda", "Gammas", "Gammat", "Gammau", "G"}, {"F", "Lambda", "GammaPh", "GammaPhbar", "GammaPp", "G"}],
      "SchwingerDyson", If[statistics === "Boson", {"Sigma", "G", "F", "Coupling"}, {"Sigma", "G", "F", "V"}],
      "Vertex", {"Lambda", "Gamma0", "G"}, "VertexFromF", {"Lambda", "F", "G"}, "SelfEnergyFromVertex", {"Sigma", "G", "V", "Lambda"},
      "Hedin", {"Sigma", "G", "W", "P", "Lambda", "GammaXC", "V"},
      "Response", {"L", "Gamma0", "G"}, "Susceptibility", {"Chi", "L"}], ! MemberQ[optionKey /@ objects[[All, 1]], #] &];
   If[missing =!= {}, Message[FunctionalDiagrams::exactobject, name, First[missing]]; Return[$Failed]];
   ob[k_String] := First[Cases[objects, (key_ -> h_) /; optionKey[key] === k :> h]];
   given[k_String] := MemberQ[optionKey /@ objects[[All, 1]], k];
   {a, b, c, d, u, v, w} = freshSymbols[7];
   g = If[given["G"], ob["G"], None];
   gam = If[given["BareVertex"], ob["BareVertex"], Function[{p, q, r}, DeltaFunction[p, r] DeltaFunction[q, r]]];
   (* both sides in canonical form (an equation whose sides are the same diagrams becomes True); with "MaximumOrder" ->
      {x, n} only their parts of order <= n in x *)
   side[e_] := If[maximum === None, SimplifyDiagrams[e], OrderPart[e, maximum[[1]], Range[0, maximum[[2]]]]];
   Replace[#, HoldPattern[Equal[l_, r_]] :> side[l] == side[r], {0, 1}] &@ Switch[name,
    (* G = G0 + G0 Sigma G *)
    "Dyson", g @@ labels == ob["G0"] @@ labels + IntegralOver[ob["G0"][labels[[1]], u] ob["Sigma"][u, v] g[v, labels[[2]]], {u, v}],
    (* F = Gamma0 + Gamma0 o F in one channel (BetheSalpeterLadder); the particle-hole channel, or s for real bosons *)
    "BetheSalpeter", With[{channel = Replace[optionValue[{opts}, Options[ExactEquation], "Channel"],
        Automatic -> If[statistics === "Boson", "s", "ParticleHole"]]},
     ob["F"] @@ labels == ob["Gamma0"] @@ labels + BetheSalpeterLadder[ob["Gamma0"] @@ labels, ob["F"] @@ labels, labels, channel, g,
       "Statistics" -> statistics, "MaximumOrder" -> maximum]],
    (* F = Lambda + Sum_r Gamma_r o F *)
    "Parquet", ob["F"] @@ labels == ob["Lambda"] @@ labels + Total[Table[
       BetheSalpeterLadder[ob[ch[[1]]] @@ labels, ob["F"] @@ labels, labels, ch[[2]], g, "Statistics" -> statistics, "MaximumOrder" -> maximum],
       {ch, If[statistics === "Boson", {{"Gammas", "s"}, {"Gammat", "t"}, {"Gammau", "u"}},
         {{"GammaPh", "ParticleHole"}, {"GammaPhbar", "TransverseParticleHole"}, {"GammaPp", "ParticleParticle"}}]}]],
    (* fermions with a density-density interaction V: Sigma = Hartree + Fock - V G G G F; real bosons with the interaction
       lambda/4! phi^4: Sigma = -lambda/2 G(x, x) + lambda/6 G G G F *)
    "SchwingerDyson", If[statistics === "Boson",
     ob["Sigma"] @@ labels == -ob["Coupling"]/2 DeltaFunction @@ labels g[labels[[1]], labels[[1]]] +
       ob["Coupling"]/6 IntegralOver[g[labels[[1]], a] g[labels[[1]], b] g[labels[[1]], c] ob["F"][a, b, c, labels[[2]]], {a, b, c}],
     ob["Sigma"] @@ labels == DeltaFunction @@ labels IntegralOver[ob["V"][labels[[1]], u] g[u, u], {u}] - ob["V"] @@ labels g @@ labels -
       IntegralOver[ob["V"][labels[[1]], u] g[labels[[1]], a] g[d, u] g[u, c] ob["F"][a, labels[[2]], c, d], {u, a, c, d}]],
    (* the vertex of the density at z: Lambda = gamma + Gamma0 G G Lambda (fermions), gamma - 1/2 Gamma0 G G Lambda (real
       bosons), gamma(x1, x2; z) = delta(x1, z) delta(x2, z), or the bare vertex given *)
    "Vertex", With[{x1 = labels[[1]], x2 = labels[[2]], z = labels[[3]]}, ob["Lambda"][x1, x2, z] ==
      gam[x1, x2, z] + If[statistics === "Boson",
       -1/2 IntegralOver[ob["Gamma0"][x1, x2, a, b] g[a, c] g[b, d] ob["Lambda"][c, d, z], {a, b, c, d}],
       IntegralOver[ob["Gamma0"][x1, x2, a, b] g[b, c] ob["Lambda"][c, d, z] g[d, a], {a, b, c, d}]]],
    (* Lambda = gamma + F G gamma G *)
    "VertexFromF", With[{x1 = labels[[1]], x2 = labels[[2]], z = labels[[3]]}, ob["Lambda"][x1, x2, z] ==
      gam[x1, x2, z] + Which[
       ! given["BareVertex"] && statistics === "Boson", -1/2 IntegralOver[ob["F"][x1, x2, a, b] g[a, z] g[b, z], {a, b}],
       ! given["BareVertex"], IntegralOver[ob["F"][x1, x2, a, b] g[b, z] g[z, a], {a, b}],
       statistics === "Boson", -1/2 IntegralOver[ob["F"][x1, x2, a, b] g[a, c] g[b, d] gam[c, d, z], {a, b, c, d}],
       True, IntegralOver[ob["F"][x1, x2, a, b] g[b, c] gam[c, d, z] g[d, a], {a, b, c, d}]]],
    (* the equation of motion: Sigma = Hartree - V G Lambda, with the vertex of the density *)
    "SelfEnergyFromVertex", ob["Sigma"] @@ labels == DeltaFunction @@ labels IntegralOver[ob["V"][labels[[1]], u] g[u, u], {u}] -
      IntegralOver[ob["V"][labels[[1]], u] g[labels[[1]], a] ob["Lambda"][a, labels[[2]], u], {u, a}],
    (* Hedin's equations, with the proper vertex Lambda (irreducible with respect to an interaction line), the polarization P,
       the screened interaction W and the kernel GammaXC = dSigma_xc/dG = Gamma0 without its Hartree part *)
    "Hedin", With[{x = labels[[1]], y = labels[[2]], z = labels[[3]]}, {
      ob["Sigma"][x, y] == DeltaFunction[x, y] IntegralOver[ob["V"][x, u] g[u, u], {u}] - IntegralOver[ob["W"][x, u] g[x, a] ob["Lambda"][a, y, u], {u, a}],
      ob["P"][x, y] == IntegralOver[g[x, a] g[b, x] ob["Lambda"][a, b, y], {a, b}],
      ob["W"][x, y] == ob["V"][x, y] + IntegralOver[ob["V"][x, u] ob["P"][u, v] ob["W"][v, y], {u, v}],
      ob["Lambda"][x, y, z] == DeltaFunction[x, z] DeltaFunction[y, z] +
        IntegralOver[ob["GammaXC"][x, y, a, b] g[b, c] ob["Lambda"][c, d, z] g[d, a], {a, b, c, d}]}],
    (* L = L0 + L0 Gamma0 L, the Bethe-Salpeter equation of the response of G to a one-body source U, with L0 the pair of
       propagators. Fermions, with the legs of a four-point vertex (leaving at x1, x3, entering at x2, x4):
       L(x1, x2; x3, x4) = dG(x1, x2)/dU(x4, x3) = -<psi(x1) psibar(x2) psi(x3) psibar(x4)>_c + G(x1, x4) G(x3, x2), for
       the source Integral psibar U psi. Real bosons: L = -2 dG/dU for 1/2 Integral phi U phi, <phi1 phi2 phi3 phi4>_c +
       G13 G24 + G14 G23, and the pair of lines in the channel carries -1/2 *)
    "Response", With[{x1 = labels[[1]], x2 = labels[[2]], x3 = labels[[3]], x4 = labels[[4]], l = ob["L"], gamma0 = ob["Gamma0"]},
     l[x1, x2, x3, x4] == If[statistics === "Boson",
       g[x1, x3] g[x2, x4] + g[x1, x4] g[x2, x3] - 1/2 IntegralOver[g[x1, a] g[x2, b] gamma0[a, b, c, d] l[c, d, x3, x4], {a, b, c, d}],
       g[x1, x4] g[x3, x2] + IntegralOver[g[x1, a] g[b, x2] gamma0[a, b, c, d] l[d, c, x3, x4], {a, b, c, d}]]],
    (* Chi = gamma L gamma: the response of the one-body operator O(z) = Integral psibar(a) gamma(a, b; z) psi(b) (1/2 phi
       gamma phi for real bosons) to its source u(z), Chi(z1, z2) = d<O(z1)>/du(z2) = d^2 Omega/du(z1) du(z2) =
       -<O(z1) O(z2)>_c (the sign of the Lindhard function) *)
    "Susceptibility", ob["Chi"] @@ labels == If[statistics === "Boson",
      -1/4 IntegralOver[gam[a, b, labels[[1]]] ob["L"][a, b, c, d] gam[c, d, labels[[2]]], {a, b, c, d}],
      IntegralOver[gam[a, b, labels[[1]]] ob["L"][b, a, d, c] gam[c, d, labels[[2]]], {a, b, c, d}]]]];

(* the residuals of an exact equation (or of a list of them), order by order in x from 0 to n *)
CheckEquation[equations_List, x_, n_Integer] := CheckEquation[#, x, n] & /@ equations;
CheckEquation[True, x_, n_Integer] := Table[0, {n + 1}];
(* only the terms of order <= n are brought into canonical form (the order of a term does not change under renaming and
   delta integration; terms with nested integrals are tested again after it), and split by order in one pass *)
CheckEquation[HoldPattern[Equal[lhs_, rhs_]], x_, n_Integer] := With[{headQ = ! FreeQ[{lhs, rhs}, x[___]]},
   With[{terms = selectCanonicalTermList[rawTermsOf[lhs - rhs], termOrder[#, x, headQ] <= n &]},
    termListToExpression /@ orderPartsOfTerms[terms, x, ! FreeQ[termListToExpression[terms], x[___]], n]]];

(* ---- auxiliary fields: Hubbard-Stratonovich, and the converse ---- *)
(* An interaction 1/2 Integral K(x, y) O(x) O(y), with O a local operator of even Grassmann parity (a density psibar psi, or
   phi^2), is the exchange of a real boson phi with <phi phi>_0 = K coupled by -i Integral phi O, because
   <exp(i Integral phi O)>_phi = exp(-1/2 Integral O K O). The kernel K is read off the terms of the interaction: a term
   with two labels gives K = kernel(x, y) + kernel(y, x), a local term (one label) K = 2 kernel(x) delta(x, y); the sign of
   the reordering of the fields is found by comparing the terms with 1/2 O K O. *)
FunctionalDiagrams::hsform = "The term `1` of the interaction is not of the form 1/2 Integral K(x, y) O(x) O(y) for the operator O given; it is kept as it is.";
FunctionalDiagrams::hsnone = "No term of the interaction is of the form 1/2 Integral K(x, y) O(x) O(y) for the operator O given.";
FunctionalDiagrams::hshead = "The kernel `1` is not a number times a propagator h[x, y]: give the head of the propagator of the auxiliary field.";
fieldHeadsQ[f_] := MemberQ[$fieldSourceMap[[All, 1]], If[Head[f] === Power, Head[f[[1]]], Head[f]]];
hsKernelOfTerm[diagramTerm[c_, o_, d_], operator_] := Module[{factors = expandIntegerPowers[factorList[c]], kern, x, y, k, candidate, term},
   term = termToExpression[diagramTerm[c, o, d]];
   kern = Times @@ Select[factors, ! fieldHeadsQ[#] &];
   {x, y} = freshSymbols[2];
   k = Switch[Length[d],
     2, (kern /. Thread[d -> {x, y}]) + (kern /. Thread[d -> {y, x}]),
     1, 2 (kern /. d[[1]] -> x) DeltaFunction[x, y],
     _, Return[None]];
   candidate[sign_] := SimplifyDiagrams[term - sign/2 IntegralOver[k operator[x] ** operator[y], {x, y}]];
   Which[candidate[1] === 0, {x, y, k}, candidate[-1] === 0, {x, y, -k}, True, None]];
HubbardStratonovich[{z0_, interaction_}, operator_, {field_Symbol, source_Symbol}, propagator_] := Module[
   {terms = canonicalTermsOf[interaction], kernels, matched, kept, x, y, kernel, d, coupling, kernelFunction, a, b, z},
   kernels = hsKernelOfTerm[#, operator] & /@ terms;
   matched = Pick[terms, (# =!= None &) /@ kernels]; kept = Pick[terms, (# === None &) /@ kernels];
   Scan[Message[FunctionalDiagrams::hsform, userForm[termToExpression[#]]] &, kept];
   If[matched === {}, Message[FunctionalDiagrams::hsnone]; Return[$Failed]];
   {x, y} = freshSymbols[2];
   kernel = SimplifyDiagrams[Total[Table[k[[3]] /. {k[[1]] -> x, k[[2]] -> y}, {k, DeleteCases[kernels, None]}]]];
   kernelFunction = LabelFunction[kernel, {x, y}];
   (* the propagator of the auxiliary field: the head of the kernel (Automatic), with the number in front in the coupling;
      or a new head standing for the whole kernel *)
   {d, coupling} = If[propagator === Automatic,
     With[{heads = Cases[{kernel}, h_Symbol[x, y] | h_Symbol[y, x] :> h, Infinity]},
      If[Length[DeleteDuplicates[heads]] == 1 && FreeQ[Simplify[kernel/heads[[1]][x, y]], x | y | IntegralOver],
       {heads[[1]], Sqrt[Simplify[kernel/heads[[1]][x, y]]]},
       Message[FunctionalDiagrams::hshead, userForm[kernel]]; Return[$Failed]]],
     {propagator, 1}];
   DeclareSource[source]; DeclareField[field, source]; DeclareSymmetric[d];
   If[lineStyleOf[d] === "Plain", DeclareInteractionLine[d]];
   {a, b, z} = freshSymbols[3];
   With[{rules = {"FreeGeneratingFunctional" -> z0 Exp[1/2 IntegralOver[source[a] d[a, b] source[b], {a, b}]],
       "Interaction" -> SimplifyDiagrams[termListToExpression[kept] - I coupling IntegralOver[field[z] operator[z], {z}]],
       "Propagator" -> d, "Kernel" -> kernelFunction}}, Association[rules]]];

(* The converse: a real Gaussian field with <phi phi>_0 = D, coupled linearly, Integral phi(z) A(z), integrated out:
   <exp(-Integral phi A)>_phi = exp(1/2 Integral A D A), so that the interaction becomes -1/2 Integral A(x) D(x, y) A(y) *)
FunctionalDiagrams::outsource = "`1` has no Gaussian factor exp(1/2 Integral `2`[x] D(x, y) `2`[y]).";
FunctionalDiagrams::outlinear = "The field `1` does not enter the interaction linearly: it cannot be integrated out exactly.";
IntegrateOutField[{z0_, interaction_}, field_Symbol, source_Symbol] := Module[{exponents, gaussian, rest, a, b, w, x, y, propagatorFunction,
    terms = canonicalTermsOf[interaction], linear, others, coupledTo, coupled},
   (* the exponent with the source, and the propagator D(a, b) = its second derivative *)
   exponents = Cases[{z0}, Exp[e_] /; ! FreeQ[e, source] :> e, Infinity];
   If[Length[exponents] != 1, Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   {a, b, w, x, y} = freshSymbols[5];
   gaussian = SetSourcesToZero[FunctionalDerivative[exponents[[1]], source[a], source[b]]];
   If[! FreeQ[gaussian, source] || gaussian === 0, Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   propagatorFunction = LabelFunction[gaussian, {a, b}];
   rest = SimplifyDiagrams[exponents[[1]] - 1/2 IntegralOver[source[x] propagatorFunction[x, y] source[y], {x, y}]];
   If[! FreeQ[rest, source], Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   (* the terms with the field, each linear in it: phi(u) times A, with u renamed w *)
   coupledTo[t : diagramTerm[c_, o_, d_]] := With[{f = Cases[expandIntegerPowers[factorList[c]], field[u_] :> u]},
     If[Length[f] != 1 || ! FreeQ[c /. field[f[[1]]] -> 1, field], $Failed,
      termToExpression[diagramTerm[(c /. field[f[[1]]] -> 1) /. f[[1]] -> w, o /. f[[1]] -> w, DeleteCases[d, f[[1]]]]]]];
   linear = Select[terms, ! FreeQ[#, field] &]; others = Select[terms, FreeQ[#, field] &];
   coupled = coupledTo /@ linear;
   If[MemberQ[coupled, $Failed] || linear === {}, Message[FunctionalDiagrams::outlinear, field]; Return[$Failed]];
   With[{af = LabelFunction[Total[coupled], {w}]},
    With[{rules = {"FreeGeneratingFunctional" -> (z0 /. Exp[exponents[[1]]] -> Exp[rest]),
        "Interaction" -> SimplifyDiagrams[termListToExpression[others] -
          1/2 IntegralOver[propagatorFunction[x, y] af[x] ** af[y], {x, y}]]}}, Association[rules]]]];

(* ========================================================================= *)
(*  13. Connected functions from 1PI vertices (the tree expansion)           *)
(* ========================================================================= *)

defineTreeDisplayForms[] := (
   defineDisplayForm[FullPropagator[a_, b_], Interpretation[Subscript["G", Row[{a, b}]], FullPropagator[a, b]]];
   defineDisplayForm[VertexFunction[a__], Interpretation[Subsuperscript["\[CapitalGamma]", Row[{a}], Row[{"(", Length[{a}], ")"}]],
     VertexFunction[a]]];);
defineTreeDisplayForms[];
defineDisplayForm[MeanField[a_], Interpretation[Subscript[OverBar["\[Phi]"], a], MeanField[a]]];

(* the rules and declarations of the three modes removed, so that the state after a switch does not depend on the
   previous mode *)
clearTreeRules[] := (
   Clear[FullPropagator, VertexFunction]; ClearAttributes[{FullPropagator, VertexFunction}, Orderless];
   $lineStyles = DeleteCases[$lineStyles, {FullPropagator, _}];
   $derivativeRules = Select[$derivativeRules, FreeQ[#, ExternalSource] &];
   $derivativePairs = Select[$derivativePairs, FreeQ[#, ExternalSource] &];
   $grassmannSources = DeleteCases[$grassmannSources, ExternalSource];
   $bosonicSources = DeleteCases[$bosonicSources, ExternalSource];
   $gradedHeads = Complement[$gradedHeads, {FullPropagator, VertexFunction, MeanField}];
   $antisymmetricHeads = Complement[$antisymmetricHeads, {FullPropagator, VertexFunction}];
   $symmetricHeads = Complement[$symmetricHeads, {FullPropagator, VertexFunction}];);

setupTreeRules["Boson"] := (
   clearTreeRules[];
   DeclareSource[ExternalSource]; DeclareSymmetric[FullPropagator, VertexFunction];
   (* dphibar_a/dJ_b = G_ab,  dG_ab/dJ_c = -G_ax G_by G_cz Gamma_xyz,  dGamma_a../dJ_b = Gamma_a..x G_xb *)
   DeclareFunctionalDerivative[MeanField[a_], ExternalSource[b_], FullPropagator[a, b]];
   DeclareFunctionalDerivative[FullPropagator[a_, b_], ExternalSource[c_],
    -IntegralOver[FullPropagator[a, x] FullPropagator[b, y] FullPropagator[c, z] VertexFunction[x, y, z], {x, y, z}]];
   DeclareFunctionalDerivative[VertexFunction[as__], ExternalSource[b_], IntegralOver[VertexFunction[as, x] FullPropagator[x, b], {x}]];
   defineTreeDisplayForms[];);

setupTreeRules["Fermion"] := (
   clearTreeRules[];
   DeclareGrassmannSource[ExternalSource]; DeclareGrassmannGraded[FullPropagator, VertexFunction, MeanField];
   DeclareAntisymmetric[FullPropagator, VertexFunction];
   (* superfield rules: d_c phibar_a = G_ca,  d_c G_ab = G_ae G_cd Gamma_def G_fb,  d_c Gamma_a.. = G_cd Gamma_da.. *)
   DeclareFunctionalDerivative[MeanField[a_], ExternalSource[c_], FullPropagator[c, a]];
   DeclareFunctionalDerivative[FullPropagator[a_, b_], ExternalSource[c_],
    IntegralOver[FullPropagator[a, e] FullPropagator[c, dd] VertexFunction[dd, e, f] FullPropagator[f, b], {dd, e, f}]];
   DeclareFunctionalDerivative[VertexFunction[as__], ExternalSource[c_], IntegralOver[FullPropagator[c, dd] VertexFunction[dd, as], {dd}]];
   defineTreeDisplayForms[];);

(* ---- fermion lines: the superfield trees projected on the components psi, psibar ----
   A superfield label a = (x, psi) or (x, psibar); at zero source only the components that conserve the number of
   fermions survive. A line G_ab is <psi(x) psibar(y)>_c = -G(x, y) for a = (x, psi), b = (y, psibar), with the
   many-body propagator G = -<psi psibar> (a line from y to x), and G(y, x) for the reverse; a vertex Gamma_a1...an becomes VertexFunction[outs..., ins...], the legs where the
   fermion lines leave the vertex (its psibar components) and then those where they enter it (its psi components), with
   the sign of that permutation. VertexFunction is then antisymmetric within the outs and within the ins. *)
halfAntisymmetricForm[h_, args_List] := With[{m = Length[args]/2}, With[{o = Take[args, m], i = Drop[args, m]},
    If[hasDuplicatesQ[o] || hasDuplicatesQ[i], 0, permutationSign[o] permutationSign[i] (h @@ Join[Sort[o], Sort[i]])]]];
halfUnsortedQ[args_List] := EvenQ[Length[args]] && With[{m = Length[args]/2},
    Take[args, m] =!= Sort[Take[args, m]] || Drop[args, m] =!= Sort[Drop[args, m]] || hasDuplicatesQ[Take[args, m]] ||
     hasDuplicatesQ[Drop[args, m]]];
(* components: 1 for psi, 2 for psibar *)
fermionLineFactor[f_, comp_] := f /. {
    FullPropagator[p_, q_] :> Which[{comp[p], comp[q]} === {1, 2}, -lineG[p, q], {comp[p], comp[q]} === {2, 1}, lineG[q, p], True, 0],
    VertexFunction[ps__] :> With[{cs = comp /@ {ps}}, If[Count[cs, 1] =!= Count[cs, 2], 0,
       With[{order = Join[Select[Range[Length[cs]], cs[[#]] === 2 &], Select[Range[Length[cs]], cs[[#]] === 1 &]]},
        permutationSign[order] (lineVertex @@ ({ps}[[order]]))]]]};
(* the superfield trees e, with the components of the external labels (rules label -> 1 | 2), summed over the
   components of the integration variables *)
lineVertex[args___] /; halfUnsortedQ[{args}] := halfAntisymmetricForm[lineVertex, {args}];
fermionLinesFromSuperfield[e_, external_List] := SimplifyDiagrams[Total[
     (# /. IntegralOver[f_, d_] :> IntegralOver[Total[Table[fermionLineFactor[f, Function[x, x /. Join[external, Thread[d -> tuple]]]],
           {tuple, liveComponentTuples[f, d, external]}]], d]) & /@ DiagramTerms[e]]];
(* the components of the variables d, the tuples of Tuples[{1, 2}, Length[d]] in their order, but those for which a
   line or a vertex of f vanishes: a partial assignment is dropped as soon as a factor whose labels are all assigned
   vanishes. All the tuples when f has other factors with lines or vertices (a power, an inverse) *)
liveComponentTuples[f_, d_List, external_List] := Module[{factors = factorList[f], last, search},
   If[! AllTrue[factors, MatchQ[#, _FullPropagator | _VertexFunction] || FreeQ[#, FullPropagator | VertexFunction] &],
    Tuples[{1, 2}, Length[d]],
    factors = Select[factors, MatchQ[#, _FullPropagator | _VertexFunction] &];
    (* the factor is checked once its last variable (in the order of d) has its component; 0: no variable *)
    last = Table[Max[0, Flatten[Position[d, #, {1}] & /@ List @@ g]], {g, factors}];
    search[k_, tuple_List] := If[AllTrue[Pick[factors, last, k],
        fermionLineFactor[#, Function[x, x /. Join[external, Thread[Take[d, k] -> tuple]]]] =!= 0 &],
      If[k == Length[d], Sow[tuple], search[k + 1, Append[tuple, 1]]; search[k + 1, Append[tuple, 2]]]];
    With[{r = Reap[search[0, {}]][[2]]}, If[r === {}, {}, First[r]]]]];

(* the results with fermion lines: FullPropagator directed, VertexFunction antisymmetric within its outs and its ins *)
setupTreeRules["FermionLines"] := (
   clearTreeRules[];
   VertexFunction[args___] /; halfUnsortedQ[{args}] := halfAntisymmetricForm[VertexFunction, {args}];
   $lineStyles = Append[$lineStyles, {FullPropagator, "Arrow"}];
   defineTreeDisplayForms[];);

ConnectedFunctionFromVertices::fermionlines = "The directions `1` must be \"Out\" or \"In\", as many of each, one for every label.";
fermionLineTrees[labels_List, directions_List] := Module[{trees},
   setupTreeRules["Fermion"];
   trees = fermionLinesFromSuperfield[termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[
        toTermList[FullPropagator[labels[[-2]], labels[[-1]]]], ExternalSource /@ Drop[labels, -2]]]],
     Thread[labels -> (directions /. {"Out" -> 1, "In" -> 2})]];
   setupTreeRules["FermionLines"];
   SimplifyDiagrams[trees /. {lineG -> FullPropagator, lineVertex -> VertexFunction}]];

ConnectedFunctionFromVertices[n_Integer, type_String : "Boson"] :=
  ConnectedFunctionFromVertices[Table[Symbol["x" <> ToString[i]], {i, n}], type];
ConnectedFunctionFromVertices::labels = "At least two labels are needed; got `1`.";
ConnectedFunctionFromVertices[labels_List, type_String : "Boson"] /; Length[labels] < 2 :=
  (Message[ConnectedFunctionFromVertices::labels, labels]; $Failed);
ConnectedFunctionFromVertices[labels_List, "FermionLines", directions_ : Automatic] /; Length[labels] >= 2 :=
  With[{dirs = If[directions === Automatic, Table[If[OddQ[k], "Out", "In"], {k, Length[labels]}], directions]},
   If[! ListQ[dirs] || Length[dirs] =!= Length[labels] || ! SubsetQ[{"Out", "In"}, dirs] || Count[dirs, "Out"] =!= Count[dirs, "In"],
    Message[ConnectedFunctionFromVertices::fermionlines, directions]; $Failed, fermionLineTrees[labels, dirs]]];
ConnectedFunctionFromVertices[labels_List, type_String : "Boson"] /; Length[labels] >= 2 && type =!= "FermionLines" := Module[{n = Length[labels]},
   setupTreeRules[type];
   (* W^(n) = d_1 ... d_(n-2) G_(n-1, n); terms that cannot survive at zero source are dropped early *)
   termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[toTermList[FullPropagator[labels[[n - 1]], labels[[n]]]],
      ExternalSource /@ Take[labels, n - 2]]]]];

(* ========================================================================= *)
(*  13a. Superfields: the components                                        *)
(*                                                                           *)
(*  A superfield NambuField_a, a = (x, s), is an ordinary Grassmann field of *)
(*  the package with plain labels; its components (psi, psibar for the Nambu *)
(*  field) live only here. SuperfieldComponents assigns a component to every *)
(*  label of every term (the vertices first, zero entries dropped at once)   *)
(*  and replaces the superfield objects by their components: a propagator    *)
(*  calG = [[G_psipsi, G], [-G^T, G_psibarpsibar]], any two-label kernel as  *)
(*  a matrix, a vertex by the derivatives of the component interaction, the  *)
(*  field and its mean by their components; traces and kernels of operators  *)
(*  of heads without anomalous blocks by the normal block.                   *)
(* ========================================================================= *)

SuperfieldComponents::field = "The rules `1` give no superfield: a rule field -> {component, ...} for a declared field (the Nambu field NambuField -> {psi, psibar}) is required.";
SuperfieldComponents::head = "`1` has no rule (a superfield head, or a factor that carries a label of the superfield): give its components, a propagator h -> {G, Gpsipsi, Gpsibarpsibar}, a matrix of heads h -> {{h11, h12}, {h21, h22}}, a mean m -> {m1, m2}, or for a vertex the component interaction h -> S.";
SuperfieldComponents::labels = "The free label `1` is not among the external labels `2`: give label -> component[new label], the component one of `3`.";
SuperfieldComponents::trace = "`1` has no component form: the trace of an operator that mixes the components through anomalous blocks (Tr ln of a Nambu matrix with anomalous blocks) is not a sum of traces of the blocks, and an operator without a rule has no known blocks. It is kept as it is, beside the components of the other terms, and its derivatives with respect to the components are not taken (they vanish for the means); with the anomalous blocks set to 0 (h -> {G, 0, 0}) it has components.";
SuperfieldComponents::rule = "The rule `1` is not understood: the right side must be a list of the components of a field or a mean, a list {G, Gpsipsi, Gpsibarpsibar} (two components), a matrix of entries (heads, 0 or functions of two labels) or the component interaction of a vertex.";

SuperfieldComponents[e_, externals_List, rules_List] := Catch[superfieldComponents[e, externals, rules], superfieldFailure];
SetAttributes[superfieldFailure, HoldFirst];
superfieldFailure[message_, args___] := (Message[message, args]; Throw[$Failed, superfieldFailure]);

superfieldComponents[e_, externals_List, rules_List] := Module[{fieldRules, field, components, n, meanRules, matrixRules = {}, vertexRules = {},
    oneLabel, proxies, e1, kernelRules = {}, types, normal, terms, externalComponents, renaming, cache, result},
   fieldRules = Select[rules, MatchQ[#, (_Symbol -> {__Symbol})] && MemberQ[$fieldSourceMap[[All, 1]], #[[1]]] &];
   If[fieldRules === {}, superfieldFailure[SuperfieldComponents::field, rules]];
   {field, components} = List @@ First[fieldRules];
   n = Length[components];
   meanRules = Select[rules, MatchQ[#, (_Symbol -> {__Symbol})] && Length[#[[2]]] == n && # =!= First[fieldRules] &];
   Scan[Function[r, Which[
       MemberQ[meanRules, r] || r === First[fieldRules], Null,
       MatchQ[r, _Symbol -> {_, _, _}] && n == 2, AppendTo[matrixRules, r[[1]] -> propagatorMatrix[r[[2]]]],
       MatchQ[r, _Symbol -> _List] && Dimensions[r[[2]], 2] === {n, n}, AppendTo[matrixRules, r[[1]] -> r[[2]]],
       MatchQ[r, _Symbol -> _] && ! ListQ[r[[2]]], AppendTo[vertexRules, r],
       True, superfieldFailure[SuperfieldComponents::rule, r]]], rules];
   (* the means of a Grassmann-odd superfield are Grassmann-odd: their components too *)
   Scan[Function[r, If[grassmannOddAtomQ[r[[1]][Unique["FunctionalDiagrams`Private`x"]]],
       Scan[If[! MemberQ[$grassmannSources, #] && ! MemberQ[$grassmannOddHeads, #], DeclareGrassmannSource[#]] &, r[[2]]]]], meanRules];
   oneLabel = Join[{First[fieldRules]}, meanRules];
   (* the external labels: label -> component index, label -> new label *)
   externalComponents = Function[r, If[! MatchQ[r, _ -> _[_]] || ! MemberQ[components, Head[r[[2]]]],
        superfieldFailure[SuperfieldComponents::labels, r, externals, components], r[[1]] -> First[Flatten[Position[components, Head[r[[2]]]]]]]] /@ externals;
   renaming = (#[[1]] -> First[#[[2]]] &) /@ externals;
   (* the operators: heads without anomalous blocks are "row" ([[0, p], [-p^T, 0]], a propagator) or "column" ([[0, -q^T],
      [q, 0]], an inverse propagator or a one-body source) operators, represented by their normal block *)
   types = Select[(#[[1]] -> normalBlockType[#[[2]]] &) /@ matrixRules, #[[2]] =!= None &];
   normal = (#[[1]] -> #[[2, 2]] &) /@ types;
   (* the traces first (a trace of products that has no component form as a whole is written as rings, whose kernels the
      second replacement projects; a trace kept as it is, keptTrace, is restored at the end) *)
   e1 = e /. {
      TraceLog[a_] /; superfieldOperatorQ[a, matrixRules] :> superfieldTraceLog[a, types, normal],
      OperatorTrace[a_] /; superfieldOperatorQ[a, matrixRules] :> superfieldTrace[a, types, normal]};
   e1 = e1 /. {
      OperatorKernel[a_][x_, y_] /; superfieldOperatorQ[a, matrixRules] :> With[{k = Unique["FunctionalDiagrams`Private`kernel"]},
        AppendTo[kernelRules, k -> superfieldKernelMatrix[a, types, normal]]; k[x, y]]};
   (* the traces kept as they are: their superfield heads and the components of these, for the message supertrace of a
      later derivative with respect to a component *)
   Scan[Function[k, Scan[Function[h, $superfieldTraceComponents = Union[$superfieldTraceComponents,
          {h -> superfieldComponentHeads[h /. matrixRules]}]], Select[matrixRules[[All, 1]], ! FreeQ[k[[2]], #] &]]],
     Cases[e1, _keptTrace, {0, Infinity}]];
   matrixRules = Join[matrixRules, kernelRules];
   proxies = Table[Unique["FunctionalDiagrams`Private`component"], {n}];
   Block[{$grassmannSources = Union[$grassmannSources, proxies]},
    (* the components of a vertex h(a1, ..., ak) = d_ak ... d_a1 S of the component interaction S, computed once per
       tuple of components with fresh labels *)
    cache[h_, comps_List] := cache[h, comps] = Module[{ys = freshSymbols[Length[comps]]},
       {ys, SimplifyDiagrams[FunctionalDerivative[(h /. vertexRules) /. Thread[components -> proxies],
          Sequence @@ Reverse[MapThread[proxies[[#1]][#2] &, {comps, ys}]]]]}];
    terms = canonicalTermsOf[e1];
    result = Total[projectSuperfieldTerm[#, n, externalComponents, matrixRules, vertexRules, oneLabel, components, cache] & /@ terms]];
   SimplifyDiagrams[result /. renaming /. keptTrace[h_, a_] :> h[a]]];

(* the heads of the entries of a matrix of components (symbols, 0, or functions of two labels such as -G[#2, #1] &) *)
$superfieldTraceComponents = {};
superfieldComponentHeads[m_] := Union[Cases[Flatten[{m}], s_Symbol /; s =!= 0], Cases[m, s_Symbol[_, _] :> s, {0, Infinity}]];

(* {G, Gpsipsi, Gpsibarpsibar} as the matrix [[Gpsipsi, G], [-G^T, Gpsibarpsibar]] *)
propagatorMatrix[{g_, gpp_, gbb_}] := {{gpp, g}, {If[g === 0, 0, Function[{x, y}, -g[y, x]]], gbb}};
normalBlockType[m_List] := Which[Dimensions[m] =!= {2, 2} || m[[1, 1]] =!= 0 || m[[2, 2]] =!= 0, None,
   Head[m[[1, 2]]] === Symbol && m[[1, 2]] =!= 0, {"row", m[[1, 2]]},
   Head[m[[2, 1]]] === Symbol && m[[2, 1]] =!= 0, {"col", m[[2, 1]]},
   True, None];
superfieldOperatorQ[a_, matrixRules_List] := ! FreeQ[a, Alternatives @@ (matrixRules[[All, 1]])] && matrixRules =!= {};
swapOperatorType["row"] = "col"; swapOperatorType["col"] = "row"; swapOperatorType[_] = None;
superfieldAtomType[h_Symbol, types_List] := Replace[h, Append[(#[[1]] -> #[[2, 1]] &) /@ types, _ -> None]];
superfieldAtomType[Power[b_, -1], types_List] := swapOperatorType[If[Head[b] === Symbol, superfieldAtomType[b, types], superfieldOperatorType[b, types]]];
superfieldAtomType[_, _] := None;
(* the type of an operator all of whose monomials are single atoms of one type *)
superfieldOperatorType[a_, types_List] := With[{m = Catch[collectMonomials[opMonomials[a]], operatorFailure]},
   If[! ListQ[m] || m === {}, None,
    With[{t = (If[Length[#[[2]]] == 1, superfieldAtomType[#[[2, 1]], types], None] &) /@ m}, If[Length[Union[t]] == 1, First[t], None]]]];
(* det(c P) = det(c p)^2 for P = [[0, p], [-p^T, 0]], and so for the column operators: Tr ln -> 2 Tr ln of the normal block *)
superfieldTraceLog[a_, types_List, normal_List] := If[superfieldOperatorType[a, types] === None,
   Message[SuperfieldComponents::trace, TraceLog[a]]; keptTrace[TraceLog, a], 2 TraceLog[a /. normal]];
(* Tr(Q . P) = Tr(P . Q) = 2 Tr(q p), Tr 1 doubles: every monomial an identity or a product of a column and a row operator;
   otherwise the identity monomials double and the others are written as rings (ExpandTrace), projected term by term *)
superfieldTrace[a_, types_List, normal_List] := With[{m = Catch[collectMonomials[opMonomials[a]], operatorFailure]},
   Which[
    ! ListQ[m], Message[SuperfieldComponents::trace, OperatorTrace[a]]; keptTrace[OperatorTrace, a],
    AllTrue[m, Function[mono, mono[[2]] === {} || (Length[mono[[2]]] == 2 && Sort[superfieldAtomType[#, types] & /@ mono[[2]]] === {"col", "row"})]],
    2 OperatorTrace[a /. normal],
    True, With[{identity = Total[Select[m, #[[2]] === {} &][[All, 1]]], rings = ExpandTrace[OperatorTrace[opFromMonomials[Select[m, #[[2]] =!= {} &]]]]},
     If[! FreeQ[rings, OperatorTrace | $Failed], Message[SuperfieldComponents::trace, OperatorTrace[a]]; keptTrace[OperatorTrace, a],
      2 identity OperatorTrace[1] + rings]]]];
(* the kernel of a row operator [[0, p], [-p^T, 0]] or of a column operator [[0, -q^T], [q, 0]] *)
superfieldKernelMatrix[a_, types_List, normal_List] := With[{t = superfieldOperatorType[a, types], b = a /. normal},
   With[{k = If[Head[b] === Symbol, b, OperatorKernel[b]]},
    Switch[t,
     "row", {{0, k}, {Function[{x, y}, -k[y, x]], 0}},
     "col", {{0, Function[{x, y}, -k[y, x]]}, {k, 0}},
     _, superfieldFailure[SuperfieldComponents::trace, OperatorKernel[a]]]]];

(* the value of a factor for an assignment of components (a list of rules label -> component) *)
superfieldEntry[0, _, _] := 0;
superfieldEntry[entry_, x_, y_] := entry[x, y];
superfieldFactorValue[f_, asg_List, n_, matrixRules_, vertexRules_, oneLabel_, components_, cache_] := With[{h = Head[f], comp = Replace[#, asg] &},
   Which[
    h === DeltaFunction, If[comp[f[[1]]] === comp[f[[2]]], f, 0],
    MemberQ[matrixRules[[All, 1]], h], superfieldEntry[(h /. matrixRules)[[comp[f[[1]]], comp[f[[2]]]]], f[[1]], f[[2]]],
    MemberQ[vertexRules[[All, 1]], h], With[{c = cache[h, comp /@ (List @@ f)]}, c[[2]] /. Thread[c[[1]] -> (List @@ f)]],
    MemberQ[oneLabel[[All, 1]], h], (h /. oneLabel)[[comp[f[[1]]]]] @@ f,
    True, f]];
superfieldFactorQ[f_, matrixRules_, vertexRules_, oneLabel_] := ! AtomQ[f] && (Head[f] === DeltaFunction ||
    (MemberQ[matrixRules[[All, 1]], Head[f]] && Length[f] == 2) || MemberQ[vertexRules[[All, 1]], Head[f]] ||
    (MemberQ[oneLabel[[All, 1]], Head[f]] && Length[f] == 1));

projectSuperfieldTerm[diagramTerm[c_, o_List, d_List], n_, externalComponents_, matrixRules_, vertexRules_, oneLabel_, components_, cache_] :=
  Module[{fresh = freshSymbols[Length[d]], factors, odd, projectable, others, labels, free, start, asgs, value, auts, groups},
   factors = expandIntegerPowers[factorList[c /. Thread[d -> fresh]]];
   odd = o /. Thread[d -> fresh];
   projectable = Select[Join[factors, odd], superfieldFactorQ[#, matrixRules, vertexRules, oneLabel] &];
   others = Select[Join[factors, odd], ! superfieldFactorQ[#, matrixRules, vertexRules, oneLabel] &];
   labels = Union[Flatten[(List @@ #) & /@ projectable]];
   Scan[If[! AtomQ[#] && (MemberQ[$antisymmetricHeads, Head[#]] || ! FreeQ[#, Alternatives @@ Append[labels, Unique[]]]),
      superfieldFailure[SuperfieldComponents::head, #]] &, others];
   free = Complement[labels, fresh];
   Scan[If[! MemberQ[externalComponents[[All, 1]], #], superfieldFailure[SuperfieldComponents::labels, #, externalComponents[[All, 1]], components]] &, free];
   start = Select[externalComponents, MemberQ[free, #[[1]]] &];
   value = superfieldFactorValue[#1, #2, n, matrixRules, vertexRules, oneLabel, components, cache] &;
   (* the assignments, factor by factor (the factors with most labels first), the zero ones dropped as they appear *)
   asgs = Fold[Function[{current, f}, Join @@ Map[Function[asg, With[{new = Select[DeleteDuplicates[List @@ f], ! MemberQ[First /@ asg, #] &]},
          Select[(Join[asg, Thread[new -> #]] &) /@ Tuples[Range[n], Length[new]], value[f, #] =!= 0 &]]], current]],
     {start}, SortBy[projectable, -Length[DeleteDuplicates[List @@ #]] &]];
   (* the assignments related by an automorphism of the term give the same diagram: one of each orbit, times its size *)
   auts = termAutomorphisms[Times @@ factors, odd, fresh];
   groups = If[Length[auts] < 2, List /@ asgs, GatherBy[asgs, Function[asg, With[{v = fresh /. asg}, First[Sort[(v[[#]] &) /@ auts]]]]]];
   Total[Function[g, Length[g] termToExpression[diagramTerm[(Times @@ (value[#, First[g]] & /@ factors)), value[#, First[g]] & /@ odd, fresh]]] /@ groups]];

(* The automorphisms of a term c o with the integration variables d (atomic symbols): the permutations p of d (d[[i]]
   -> d[[p[[i]]]]) under which the term is unchanged, its sign included. Backtracking within the classes of the colour
   refinement (an invariant colouring), a factor checked as soon as its variables are mapped (its image must be a factor
   of the term, up to the order of the labels of a symmetric head), every complete candidate checked with termKey.
   Only the identity when the term has nested integrals or the search would be long (no reduction then). *)
$automorphismSearchLimit = 20000;
termAutomorphisms[c_, o_List, d_List] := Module[{factors, inerts, data, incidence, colours, present, heads, k1, order, nodes = 0, auts = {}, search, n = Length[d]},
   If[n < 2 || ! FreeQ[c, IntegralOver], Return[{Range[n]}]];
   factors = Select[Join[factorList[c], o], ! FreeQ[#, Alternatives @@ d] &];
   inerts = inertForm /@ factors;
   data = refinementData[inerts, d];
   incidence = data[[4]];
   colours = colourRefinement[data, Table[1, {n}]];
   If[Length[Union[colours]] == n, Return[{Range[n]}]];
   present = Union[normalizedInertForm /@ inerts];
   heads = ({#[[1]], Length[#[[2]]]} &) /@ inerts;
   k1 = termKey[c, o, {}];
   (* the variables in the order of the search: each next one sharing most factors with those before it (so that factors
      are checked early), then of the smallest class *)
   order = {First[SortBy[Range[n], {Count[colours, colours[[#]]], #} &]]};
   Do[With[{rest = Complement[Range[n], order]},
     AppendTo[order, First[SortBy[rest, {-Length[Intersection[incidence[[#]], Join @@ incidence[[order]]]], Count[colours, colours[[#]]], #} &]]]],
    {n - 1}];
   search[img_List, pos_Integer] := If[pos > n,
     If[termKey[c, o, Thread[d -> d[[img]]]] === k1, AppendTo[auts, img]],
     With[{i = order[[pos]]},
      Do[If[colours[[j]] == colours[[i]] && ! MemberQ[img, j],
        If[++nodes > $automorphismSearchLimit, Throw[{Range[n]}, automorphismSearch]];
        With[{img2 = ReplacePart[img, i -> j]},
         If[AllTrue[incidence[[i]], Function[k, With[{vars = data[[3, k]]}, With[{images = Select[img2[[vars]], # > 0 &]},
              If[Length[images] == Length[vars],
               MemberQ[present, normalizedInertForm[inerts[[k]] /. Thread[d[[vars]] -> d[[img2[[vars]]]]]]],
               (* a factor mapped in part: the images of its variables mapped so far lie in one factor with its head *)
               AnyTrue[incidence[[First[images]]], Function[k2, heads[[k2]] === heads[[k]] && Complement[images, data[[3, k2]]] === {}]]]]]]],
          search[img2, pos + 1]]]], {j, n}]]];
   Catch[search[Table[0, {n}], 1]; If[auts === {}, {Range[n]}, auts], automorphismSearch]];
