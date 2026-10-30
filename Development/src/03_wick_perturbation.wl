
(* ========================================================================= *)
(*  8. Gaussian expectation values: Wick's theorem                           *)
(*  Two equivalent methods:                                                  *)
(*   - "Differentiation": the source derivatives are applied to Z0 one at a  *)
(*     time (terms that cannot survive at zero sources are dropped early);   *)
(*   - "WickPairings": the sum over complete pairings of the fields, each    *)
(*     pair replaced by its contraction <f1 f2>_0 (itself obtained from Z0   *)
(*     by differentiation), with the sign of the permutation for Grassmann   *)
(*     fields. Interchangeable interaction vertices and identical fields are *)
(*     enumerated once, with their multiplicity.                             *)
(* ========================================================================= *)

(* number of source atoms in the polynomial prefactor of a term *)
sourceDegree[diagramTerm[c_, o_, d_]] := Count[o, a_ /; sourceDependentOddAtomQ[a]] +
   Total[Map[Which[sourceAtomQ[#], 1, Head[#] === Power && sourceAtomQ[#[[1]]] && IntegerQ[#[[2]]], #[[2]], True, 0] &,
     factorList[c]]];

(* apply the derivatives (rightmost first) and set the sources to zero, dropping terms that cannot survive *)
differentiateAndSetSourcesToZero[terms0_List, sources_List] := Module[{terms = terms0, n = Length[sources]},
   Do[terms = canonicalTermList[differentiateTermList[terms, sources[[k]]]];
    terms = Select[terms, sourceDegree[#] <= k - 1 &], {k, n, 1, -1}];
   Join @@ (setSourcesToZeroInTerm /@ terms)];

expectationByDifferentiation[z0_, operators_List] := Module[{ops = fieldOperator /@ operators},
   (Times @@ ops[[All, 1]]) termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[toTermList[z0], ops[[All, 2]]]]]];

(* Z0 = Exp[quadratic form in the sources] *)
gaussianFunctionalQ[z0_] := Head[z0] === Power && z0[[1]] === E && With[{q = toTermList[z0[[2]]]},
    q =!= {} && AllTrue[q, sourceDegree[#] == 2 &]];

(* contraction <f1 f2>_0, computed once per pair of field heads and arities *)
$contractionCache = {};
contractionValue[z0_, f1_, f2_] := Module[{key = {z0, Head[f1], Length[f1], Head[f2], Length[f2]}, entry, g1, g2, value},
   entry = Select[$contractionCache, #[[1]] === key &, 1];
   If[entry === {},
    g1 = freshSymbols[Length[f1]]; g2 = freshSymbols[Length[f2]];
    value = expectationByDifferentiation[z0, {Head[f1] @@ g1, Head[f2] @@ g2}];
    entry = {key, {g1, g2, value}}; AppendTo[$contractionCache, entry],
    entry = First[entry]];
   entry[[2, 3]] /. Thread[Join[entry[[2, 1]], entry[[2, 2]]] -> Join[List @@ f1, List @@ f2]]];

fieldParityOdd[f_] := grassmannOddAtomQ[f];

(* the automorphisms of a vertex as permutations of its slots (image of each slot): permutations of the integration
   variables that leave the coefficient unchanged and permute the odd fields evenly, combined with the permutations of
   identical commuting fields; $Failed if there are more than 5040 of them *)
vertexSlotGroup[{coefficient_, fields_List, dummies_List}] := Module[{n = Length[fields], oddPositions, classes, within = {Range[Length[fields]]},
    group = {}, image, used, base, ok},
   oddPositions = Select[Range[n], fieldParityOdd[fields[[#]]] &];
   classes = Select[GatherBy[Complement[Range[n], oddPositions], fields[[#]] &], Length[#] > 1 &];
   If[Times @@ (Factorial[Length[#]] & /@ classes) > 5040, Return[$Failed]];
   (* the permutations within the classes of identical commuting fields *)
   Do[within = Join @@ Table[With[{t = ReplacePart[w, Thread[c -> q]]}, t], {w, within}, {q, Permutations[c]}], {c, classes}];
   Do[With[{rule = Thread[dummies -> p]},
      If[(coefficient /. rule) === coefficient,
       image = fields /. rule;
       If[Sort[image] === Sort[fields],
        used = {}; ok = True;
        base = Table[With[{t = Select[Flatten[Position[fields, image[[i]], {1}]], ! MemberQ[used, #] &, 1]},
           If[t === {}, ok = False; i, AppendTo[used, t[[1]]]; t[[1]]]], {i, n}];
        If[ok && permutationSign[base[[oddPositions]]] === 1,
         group = Join[group, Table[w[[base]], {w, within}]]]]]],
    {p, If[1 < Length[dummies] <= 5, Permutations[dummies], {dummies}]}];
   If[Length[group] > 5040, $Failed, Union[group]]];

(* the orbits of the slots of a vertex under its automorphisms (group: vertexSlotGroup): {orbit id, orbit size} for every
   slot. When the group is $Failed (too large), the classes of identical commuting fields: the orbits of a subgroup, so that
   the multiplicities stay exact (the enumeration is less pruned) *)
vertexSlotOrbits[fields_List, group_] := Table[With[{o = Which[group =!= $Failed, Union[group[[All, i]]],
       fieldParityOdd[fields[[i]]], {i}, True, Flatten[Position[fields, fields[[i]], {1}]]]}, {Min[o], Length[o]}],
   {i, Length[fields]}];

(* the orbits of the free slots under the automorphisms that fix the slots fixed (local slot numbers) *)
stabiliserOrbits[group_List, fixed_List] := With[{h = Select[group, Function[pi, AllTrue[fixed, pi[[#]] == # &]]]},
   DeleteDuplicates[Table[Union[h[[All, i]]], {i, Complement[Range[Length[First[group]]], fixed]}]]];

findRoot[parent_List, g_Integer] := Module[{r = g}, While[parent[[r]] =!= r, r = parent[[r]]]; r];

(* the labels of a field that are points (not discrete index values such as a spin projection) *)
pointLabelsOf[field_] := Select[List @@ field, ! discreteIndexValueQ[#] &];

(* The sum over complete pairings. groups: one per external field, then one per interaction vertex.
   mode: "All", "NoVacuumBubbles" (components without external fields are discarded) or "Connected". *)

wickPairingSum[z0_, externalFields_List, vertices_List, mode_String] := Module[{slots = {}, slotGroup = {}, groupType = {},
    groupSlots, orbitId = {}, orbitSize = {}, nGroups, external, odd, oddCount, contraction, results, enumerate, initialParent,
    slotGroups = {}, contractionTable, slotClasses, slotClass, classCompatible, compatible, stuckQ, orbitMemo,
    orbitMultiplicities, matchableQ, groupsOfType},
   (* slots *)
   Do[AppendTo[slots, externalFields[[i]]]; AppendTo[slotGroup, i]; AppendTo[groupType, -i];
    AppendTo[orbitId, 0]; AppendTo[orbitSize, 1]; AppendTo[slotGroups, $Failed], {i, Length[externalFields]}];
   Do[With[{v = vertices[[j]], g = Length[externalFields] + j},
     AppendTo[groupType, v[[1]]];
     AppendTo[slotGroups, With[{hit = Select[Take[vertices, j - 1], #[[1]] === v[[1]] &, 1]},
       If[hit =!= {}, slotGroups[[Length[externalFields] + First[First[Position[vertices[[All, 1]], v[[1]], {1}]]]]],
        vertexSlotGroup[v[[2 ;; 4]]]]]];
     With[{orbits = vertexSlotOrbits[v[[3]], Last[slotGroups]]},
      Do[AppendTo[slots, v[[3, i]]]; AppendTo[slotGroup, g]; AppendTo[orbitId, orbits[[i, 1]]]; AppendTo[orbitSize, orbits[[i, 2]]],
       {i, Length[v[[3]]]}]]], {j, Length[vertices]}];
   nGroups = Length[groupType];
   groupSlots = Table[Select[Range[Length[slots]], slotGroup[[#]] == g &], {g, nGroups}];
   external = Table[g <= Length[externalFields], {g, nGroups}];
   odd = fieldParityOdd /@ slots; oddCount = Boole[odd];
   (* the groups of each vertex type *)
   groupsOfType = Association[(# -> Select[Range[nGroups], Function[g, groupType[[g]] === #]] &) /@ DeleteDuplicates[groupType]];
   (* the orbits of the free slots of an opened vertex of type t under its automorphisms that fix its contracted slots
      (local slot numbers): only the first slot of each orbit is contracted, with the size of the orbit as multiplicity;
      orbitMultiplicities: that multiplicity for every local slot (0 for the others) *)
   orbitMemo[t_, fixedLocal_] := orbitMemo[t, fixedLocal] =
     stabiliserOrbits[slotGroups[[First[Select[Range[nGroups], groupType[[#]] === t &, 1]]]], fixedLocal];
   orbitMultiplicities[t_, fixedLocal_, size_] := orbitMultiplicities[t, fixedLocal, size] = Module[{m = ConstantArray[0, size]},
      Scan[(m[[First[#]]] = Length[#]) &, Reverse[orbitMemo[t, fixedLocal]]]; m];
   contractionTable = Table[Null, {Length[slots]}, {Length[slots]}];
   contraction[i_, j_] := With[{known = contractionTable[[i, j]]}, If[known =!= Null, known,
      contractionTable[[i, j]] = If[odd[[i]] =!= odd[[j]], 0, contractionValue[z0, slots[[i]], slots[[j]]]]]];
   (* classes of slots (head, number of labels, parity): two slots can be contracted only if their classes can *)
   slotClasses = Union[Table[{Head[s], Length[s], fieldParityOdd[s]}, {s, slots}]];
   slotClass = Table[First[First[Position[slotClasses, {Head[s], Length[s], fieldParityOdd[s]}, {1}]]], {s, slots}];
   classCompatible = Table[c1[[3]] === c2[[3]] &&
      contractionValue[z0, c1[[1]] @@ freshSymbols[c1[[2]]], c2[[1]] @@ freshSymbols[c2[[2]]]] =!= 0, {c1, slotClasses}, {c2, slotClasses}];
   compatible = Boole[classCompatible];
   (* a component with free slots none of which can be contracted with a free slot outside it can only close: it is
      discarded if it is not the whole diagram ("Connected") or if it has no external field ("NoVacuumBubbles") *)
   (* (only the component just joined is checked: comp = the component of every group, root = its label, members = its
      groups) *)
   stuckQ[free_List, comp_List, root_, members_List] := With[{labels = comp[[slotGroup[[free]]]]},
     With[{inside = slotClass[[Pick[free, labels, root]]], outside = slotClass[[Pick[free, Unitize[labels - root], 1]]]},
      inside =!= {} && (outside === {} || Max[compatible[[Union[inside], Union[outside]]]] == 0) &&
       If[mode === "Connected", Length[members] < nGroups, ! AnyTrue[members, external[[#]] &]]]];
   (* a complete pairing exists only if the numbers of slots of each class can be matched (e.g. an even number of
      gluons, as many quarks as antiquarks): checked once, on the numbers of slots of each class *)
   matchableQ[counts_List] := matchableQ[counts] = With[{nonzero = Select[Range[Length[counts]], counts[[#]] > 0 &]},
     If[nonzero === {}, True,
      With[{c = First[nonzero]}, AnyTrue[nonzero, Function[d, classCompatible[[c, d]] && (d =!= c || counts[[c]] >= 2) &&
          matchableQ[ReplacePart[counts, If[d === c, {c -> counts[[c]] - 2}, {c -> counts[[c]] - 1, d -> counts[[d]] - 1}]]]]]]]];
   (* comp: the component of every group, an integer label (on a merge the label of the first slot's component is kept);
      freeCount: the number of free slots of every group *)
   enumerate[free_List, weight_, factors_List, opened_List, comp_List, freeCount_List] := Module[{a, rest, ga, firstUnopened = {},
      usedOrbits = {}, usedIdentical = {}, unopenedCount, gb, multiplicity, value, sign, oddBefore, newFree, newComp, newFreeCount,
      root, cb, members, newOpened},
     If[free === {}, Sow[{weight, factors}],
      a = First[free]; rest = Rest[free]; ga = slotGroup[[a]];
      newOpened = ReplacePart[opened, ga -> True];
      (* the number of odd slots of rest before each one *)
      oddBefore = If[odd[[a]] && rest =!= {}, Prepend[Most[Accumulate[oddCount[[rest]]]], 0], {}];
      Do[With[{b = rest[[j]]},
       gb = slotGroup[[b]];
       value = contraction[a, b];
       If[value =!= 0,
        multiplicity = 0;
        If[! newOpened[[gb]],
         (* an unopened vertex: only the first unopened vertex of its type, and one slot per orbit *)
         With[{t = groupType[[gb]]},
          With[{f = Select[firstUnopened, #[[1]] === t &]},
           If[f === {}, AppendTo[firstUnopened, {t, gb}]];
           If[f === {} || f[[1, 2]] === gb,
            If[! MemberQ[usedOrbits, {gb, orbitId[[b]]}],
             AppendTo[usedOrbits, {gb, orbitId[[b]]}];
             unopenedCount = Count[newOpened[[groupsOfType[t]]], False];
             multiplicity = unopenedCount orbitSize[[b]]]]]],
         (* an opened vertex: slots related by the automorphisms that fix its contracted slots are interchangeable *)
         If[slotGroups[[gb]] =!= $Failed,
          With[{offset = First[groupSlots[[gb]]] - 1},
           multiplicity = orbitMultiplicities[groupType[[gb]], Complement[groupSlots[[gb]], rest] - offset,
              Length[groupSlots[[gb]]]][[b - offset]]],
         (* an external field: identical commuting fields are interchangeable *)
         If[odd[[b]], multiplicity = 1,
          If[! MemberQ[usedIdentical, {gb, slots[[b]]}],
           AppendTo[usedIdentical, {gb, slots[[b]]}];
           multiplicity = Count[rest, s_ /; slotGroup[[s]] == gb && slots[[s]] === slots[[b]]]]]]];
        If[multiplicity > 0,
         sign = If[odd[[a]], (-1)^oddBefore[[j]], 1];
         newFree = Delete[rest, j];
         root = comp[[ga]]; cb = comp[[gb]];
         newComp = If[root === cb, comp, comp + (root - cb) (1 - Unitize[comp - cb])];
         members = Pick[Range[nGroups], newComp, root];
         newFreeCount = freeCount; newFreeCount[[ga]]--; newFreeCount[[gb]]--;
         If[Which[
           Total[newFreeCount[[members]]] > 0, True,                  (* the component is still open *)
           mode === "NoVacuumBubbles", AnyTrue[members, external[[#]] &],
           mode === "Connected", newFree === {},
           True, True] && (mode === "All" || newFree === {} || ! stuckQ[newFree, newComp, root, members]),
          enumerate[newFree, weight multiplicity sign, Append[factors, value], ReplacePart[newOpened, gb -> True], newComp,
           newFreeCount]]]]],
       {j, Length[rest]}]]];
   (* external fields at the same point (e.g. the two fields of a density psibar(a) psi(a)) are connected from the start *)
   initialParent = Range[nGroups];
   Do[If[Intersection[pointLabelsOf[externalFields[[i]]], pointLabelsOf[externalFields[[j]]]] =!= {},
      With[{ri = findRoot[initialParent, i], rj = findRoot[initialParent, j]}, If[ri =!= rj, initialParent[[rj]] = ri]]],
    {i, Length[externalFields]}, {j, i + 1, Length[externalFields]}];
   results = If[! matchableQ[Table[Count[slotClass, k], {k, Length[slotClasses]}]], {},
     Reap[enumerate[Range[Length[slots]], 1, {}, Table[False, {nGroups}], Table[findRoot[initialParent, g], {g, nGroups}],
        Length /@ groupSlots]][[2]]];
   (* the local functions removed, with the variables they hold: a memo would keep them in the kernel *)
   Remove[enumerate, contraction, contractionTable, orbitMemo, orbitMultiplicities, matchableQ, stuckQ];
   If[results === {}, {}, First[results]]];

(* a vertex of the interaction: {coefficient (without the fields), fields in order, integration variables} *)
splitInteractionTerm[diagramTerm[c_, o_, d_]] := Module[{factors = factorList[c], bosons = {}, rest = 1},
   Do[Which[
     ! AtomQ[f] && MemberQ[$fieldSourceMap[[All, 1]], Head[f]], AppendTo[bosons, f],
     Head[f] === Power && ! AtomQ[f[[1]]] && MemberQ[$fieldSourceMap[[All, 1]], Head[f[[1]]]] && IntegerQ[f[[2]]],
     bosons = Join[bosons, Table[f[[1]], {f[[2]]}]],
     True, rest = rest f], {f, factors}];
   {rest, Join[bosons, o], d}];

(* the vertices of all the multisets of k interaction terms, with the weight (-1)^k / (m1! m2! ...) *)
vertexMultisets[interactionTerms_List, k_Integer] := Module[{types = Range[Length[interactionTerms]], multisets},
   multisets = Select[Tuples[types, k], OrderedQ];
   Table[With[{counts = Table[Count[m, t], {t, types}]},
     {(-1)^k/(Times @@ (Factorial /@ counts)),
      Table[With[{v = splitInteractionTerm[interactionTerms[[m[[i]]]]]},
        With[{new = freshSymbols[Length[v[[3]]]]}, {m[[i]], v[[1]] /. Thread[v[[3]] -> new], v[[2]] /. Thread[v[[3]] -> new], new}]],
       {i, Length[m]}]}], {m, multisets}]];

(* a pairing whose factors are all plain is one term directly (the vertex variables are fresh symbols already); the
   pairings share a few distinct factors, each tested once *)
pairingTermsToTermList[pairs_List, vertices_List] := Module[{coefficient = Times @@ vertices[[All, 2]],
    dummies = Join @@ vertices[[All, 4]], plain = <||>, plainQ},
   plainQ[f_] := Lookup[plain, Key[f], plain[f] = plainFactorQ[f]];
   Join @@ Table[With[{c = p[[1]] coefficient Times @@ p[[2]]},
      If[AllTrue[factorList[c], plainQ], If[c === 0, {}, {diagramTerm[c, {}, dummies]}],
       toTermList[If[dummies === {}, c, IntegralOver[c, dummies]]]]], {p, pairs}]];

GaussianExpectationValue::method = "Method `1` is not available; using \"Differentiation\".";
Options[GaussianExpectationValue] = {"Method" -> Automatic};
GaussianExpectationValue[z0_, operatorList_List, opts___Rule] := Module[{o = callOptions[GaussianExpectationValue, {opts}],
    operators = normaliseOperators[operatorList]},
   If[operators === $Failed, Return[$Failed]];
   checkGeneratingFunctional[z0];
   If[perturbationMethod[z0, o] === "WickPairings",
    termListToExpression[canonicalTermList[pairingTermsToTermList[wickPairingSum[z0, operators, {}, "All"], {}]]],
    expectationByDifferentiation[z0, operators]]];

(* ========================================================================= *)
(*  9. Perturbation theory                                                   *)
(* ========================================================================= *)

PerturbativeCorrelationFunction::order = "The order `1` must be a non-negative integer n, {k} or {kmin, kmax}.";
orderList[n_Integer?NonNegative] := Range[0, n];
orderList[{k_Integer?NonNegative}] := {k};
orderList[{kmin_Integer?NonNegative, kmax_Integer}] := Range[kmin, kmax];
orderList[n_] := (Message[PerturbativeCorrelationFunction::order, n]; {});

(* the sum over orders of <operators exp(-interaction)>_0 with the given pruning of the pairings *)
(* coupling: None, or {x, m} to keep only the terms of order x^m (as OrderPart[result, x, m]): the multisets of vertices
   of another order are skipped when every interaction term is a monomial in x (and Z0 does not contain x), and the
   terms of the pairings are selected before canonicalisation *)
(* test: None, or a property of the diagrams (the particle irreducibility of LuttingerWardFunctional) by which the terms
   are selected before the canonical form, with their delta functions integrated (as rawTermsOf does) *)
perturbativePairingSum[z0_, operators_List, interaction_, orders_List, mode_String, coupling_ : None, test_ : None] := Module[
   {interactionTerms = toTermList[interaction], terms = {}, x, m, headQ, exponents = None},
   If[coupling =!= None,
    {x, m} = coupling; headQ = ! FreeQ[{z0, interaction}, x[___]];
    If[! headQ && FreeQ[z0, x] && AllTrue[interactionTerms, FreeQ[#[[1]]/x^Exponent[#[[1]], x], x] && FreeQ[#[[2]], x] &],
     exponents = Exponent[#[[1]], x] & /@ interactionTerms]];
   Do[Do[If[exponents === None || Total[exponents[[ms[[2, All, 1]]]]] == m,
       terms = Join[terms, (diagramTerm[ms[[1]] #[[1]], #[[2]], #[[3]]] &) /@
          pairingTermsToTermList[wickPairingSum[z0, operators, ms[[2]], mode], ms[[2]]]]],
     {ms, vertexMultisets[interactionTerms, k]}], {k, orders}];
   termListToExpression[Which[
     test =!= None, selectCanonicalTermList[integrateDeltaFunctions /@ terms, If[coupling === None, test, termOrder[#, x, headQ] == m && test[#] &]],
     coupling === None, canonicalTermList[terms],
     True, selectCanonicalTermList[terms, termOrder[#, x, headQ] == m &]]]];

FunctionalDiagrams::coupling = "The value `1` of \"CouplingOrder\" is not None or {coupling, k} with an integer k; it is ignored.";
couplingOption[opts_List] := With[{c = optionValue[opts, {"CouplingOrder" -> None}, "CouplingOrder"]},
   Which[c === None, None, MatchQ[c, {_, _Integer}], c, True, Message[FunctionalDiagrams::coupling, c]; None]];
(* the order x^m of the result of the method "Differentiation" *)
couplingOrderPart[e_, None] := e;
couplingOrderPart[e_, {x_, m_}] := OrderPart[e, x, m];

(* the same sum by literal differentiation of Z0 (any Z0) *)
perturbativeDifferentiationSum[z0_, operators_List, interaction_, orders_List] := Module[{total = 0,
    z00 = expectationByDifferentiation[z0, {}]},
   Do[Module[{accumulated = 0},
      Do[Module[{vs = splitInteractionTerm /@ tuple, coefficient, allOperators, dummies},
        coefficient = Times @@ vs[[All, 1]];
        allOperators = Join[operators, Join @@ vs[[All, 2]]];
        dummies = Join @@ vs[[All, 3]];
        accumulated = accumulated + IntegralOver[coefficient expectationByDifferentiation[z0, allOperators]/z00, dummies]],
       {tuple, Tuples[Table[toTermList[interaction], {i, k}]]}];
      total = total + (-1)^k/k! accumulated], {k, orders}];
   SimplifyDiagrams[total]];

FunctionalDiagrams::method = "The method `1` is not Automatic, \"WickPairings\" or \"Differentiation\"; Automatic is used.";
perturbationMethod[z0_, opts_List] := With[{m = Replace[optionValue[opts, {"Method" -> Automatic}, "Method"],
     mm_ /; ! MemberQ[{Automatic, "WickPairings", "Differentiation"}, mm] :> (Message[FunctionalDiagrams::method, mm]; Automatic)]},
   Which[m === Automatic, If[gaussianFunctionalQ[z0], "WickPairings", "Differentiation"],
    m === "WickPairings" && ! gaussianFunctionalQ[z0], Message[GaussianExpectationValue::method, m]; "Differentiation",
    True, m]];

labelArguments[operators_List] := Union[Select[Flatten[List @@@ operators], ! discreteIndexValueQ[#] &]];

(* the correlation functions: mode is the pruning of the pairings, filter the same selection of the terms for the
   method "Differentiation" (a pure function, so that it reads as a call, not as the name of a caller) *)
correlationFunction[z0_, operatorList_List, interaction_, n_, opts_List, mode_String, filter_] :=
  With[{operators = normaliseOperators[operatorList]}, If[operators === $Failed, $Failed, checkGeneratingFunctional[z0]; checkFieldArity[z0, operators];
    checkLabels[0, labelArguments[operators]];
    If[perturbationMethod[z0, opts] === "WickPairings",
     perturbativePairingSum[z0, operators, interaction, orderList[n], mode, couplingOption[opts]],
     couplingOrderPart[filter[perturbativeDifferentiationSum[z0, operators, interaction, orderList[n]], labelArguments[operators]],
      couplingOption[opts]]]]];

Options[PerturbativeCorrelationFunction] = {"Method" -> Automatic, "CouplingOrder" -> None};
PerturbativeCorrelationFunction[z0_, operatorList_List, interaction_, n_, opts___Rule] :=
  correlationFunction[z0, operatorList, interaction, n, callOptions[PerturbativeCorrelationFunction, {opts}], "NoVacuumBubbles",
   DropVacuumBubbles[#1, #2] &];

Options[ConnectedCorrelationFunction] = {"Method" -> Automatic, "CouplingOrder" -> None};
ConnectedCorrelationFunction[z0_, operatorList_List, interaction_, n_, opts___Rule] :=
  connectedCorrelationFunction[z0, operatorList, interaction, n, callOptions[ConnectedCorrelationFunction, {opts}]];
connectedCorrelationFunction[z0_, operatorList_List, interaction_, n_, opts_List] :=
  correlationFunction[z0, operatorList, interaction, n, opts, "Connected", ConnectedDiagrams[#1, #2] &];

(* linked-cluster theorem: the connected terms of Z/Z0 are log(Z/Z0) *)
Options[ConnectedVacuumDiagrams] = {"Method" -> Automatic, "CouplingOrder" -> None};
ConnectedVacuumDiagrams[z0_, interaction_, n_, opts___Rule] :=
  connectedVacuumDiagrams[z0, interaction, n, callOptions[ConnectedVacuumDiagrams, {opts}], None];
(* the connected vacuum diagrams with a property (test, or None for all) *)
connectedVacuumDiagrams[z0_, interaction_, n_, opts_List, test_] := With[{orders = DeleteCases[orderList[n], 0]},
   checkGeneratingFunctional[z0];
   If[perturbationMethod[z0, opts] === "WickPairings",
    perturbativePairingSum[z0, {}, interaction, orders, "Connected", couplingOption[opts], test],
    With[{all = couplingOrderPart[ConnectedDiagrams[perturbativeDifferentiationSum[z0, {}, interaction, orders], {}], couplingOption[opts]]},
     If[test === None, all, selectTerms[all, test]]]]];

(* order of a term: power of a coupling symbol, or number of factors with a given head *)
termOrder[diagramTerm[c_, o_, d_], x_, headQ_] := If[headQ,
   Total[Map[Which[Head[#] === x, 1, Head[#] === Power && Head[#[[1]]] === x && IntegerQ[#[[2]]], #[[2]], True, 0] &, factorList[c]]],
   Exponent[c, x]];
OrderPart[e_, x_, k_Integer] := With[{headQ = ! FreeQ[e, x[___]]},
   termListToExpression[selectCanonicalTerms[e, termOrder[#, x, headQ] == k &]]];
OrderPart[e_, x_, ks_List] := With[{headQ = ! FreeQ[e, x[___]]},
   termListToExpression[selectCanonicalTerms[e, MemberQ[ks, termOrder[#, x, headQ]] &]]];
