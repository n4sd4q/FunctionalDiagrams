
(* ========================================================================= *)
(*  3. Terms:  diagramTerm[c, odds, dummies] = IntegralOver[c * GrassmannProduct @@ odds, dummies] *)
(* ========================================================================= *)

unitTermList = {diagramTerm[1, {}, {}]};

multiplyTwoTerms[diagramTerm[c1_, o1_, d1_], diagramTerm[c2_, o2_, d2_]] := diagramTerm[c1 c2, Join[o1, o2], Join[d1, d2]];
multiplyTermLists[l1_List, l2_List] := Flatten[Table[multiplyTwoTerms[a, b], {a, l1}, {b, l2}], 1];
multiplyTermListsInOrder[lists_List] := Fold[multiplyTermLists, unitTermList, lists];

toTermList[0] := {};
(* a Grassmann product with a zero factor vanishes *)
toTermList[(NonCommutativeMultiply | GrassmannProduct)[___, 0, ___]] := {};
toTermList[e_Plus] := Join @@ (toTermList /@ (List @@ e));
(* Grassmann-odd factors multiplied with Times have lost their order, and with it the sign of the result: the
   evaluation is aborted, rather than giving a result with a sign that may be wrong *)
(* a product of plain factors (plainFactorQ, below) is one term *)
toTermList[e_Times] := If[plainFactorsQ[List @@ e], {diagramTerm[e, {}, {}]},
   Module[{parts = toTermList /@ (List @@ e)},
    If[Count[parts, l_ /; AnyTrue[l, OddQ[Length[#[[2]]]] &]] > 1, Message[FunctionalDerivative::times, userForm[e]]; Abort[]];
    multiplyTermListsInOrder[parts]]];
toTermList[NonCommutativeMultiply[a___]] := multiplyTermListsInOrder[toTermList /@ {a}];
toTermList[GrassmannProduct[a___]] := multiplyTermListsInOrder[toTermList /@ {a}];
(* the integration variables are renamed apart; a nested integral over a variable of the same name (an expression in
   canonical form substituted into another) keeps its own *)
scopedReplace[f_, {}] := f;
scopedReplace[f_, rules_List] := f /. Join[{IntegralOver[g_, w_List] :> IntegralOver[scopedReplace[g, Select[rules, ! MemberQ[w, #[[1]]] &]], w]},
    rules];
toTermList[IntegralOver[f_, v_List]] := Module[{new = freshSymbols[Length[v]]},
   (diagramTerm[#[[1]], #[[2]], Join[new, #[[3]]]] &) /@
    toTermList[If[FreeQ[f, IntegralOver], f /. Thread[v -> new], scopedReplace[f, Thread[v -> new]]]]];
(* a Grassmann-odd atom is nilpotent (Times folds eta[x] eta[x] into a power) *)
toTermList[Power[b_?grassmannOddAtomQ, n_Integer]] /; n > 1 := {};
toTermList[Power[b_, n_Integer?Positive]] /; (Head[b] === Plus || Head[b] === IntegralOver || Head[b] === NonCommutativeMultiply ||
     Head[b] === GrassmannProduct) :=
  multiplyTermListsInOrder[Table[toTermList[b], {n}]];
toTermList[a_?grassmannOddAtomQ] := {diagramTerm[1, {a}, {}]};
toTermList[e_] := {diagramTerm[e, {}, {}]};
(* a plain factor f is one with toTermList[f] === {diagramTerm[f, {}, {}]} (the last rule above): a product of plain
   factors is one term, without a toTermList of every factor *)
$compositeHeads = {Plus, Times, NonCommutativeMultiply, GrassmannProduct, IntegralOver};
plainFactorQ[f_] := AtomQ[f] || ! (MemberQ[$compositeHeads, Head[f]] || grassmannOddAtomQ[f] ||
     (Head[f] === Power && (grassmannOddAtomQ[f[[1]]] || MemberQ[$compositeHeads, Head[f[[1]]]])));
(* the same for a list of factors at once (graded heads count as not plain, whatever the number of their arguments: the
   general route is then taken, with the same result) *)
plainFactorsQ[factors_List] := With[{special = Join[$compositeHeads, $grassmannSources, $grassmannOddHeads, $gradedHeads]},
   Intersection[Head /@ factors, special] === {} &&
    Intersection[Cases[factors, Power[b_?(! AtomQ[#] &), _] :> Head[b]], special] === {}];

FunctionalDerivative::times = "Grassmann-odd factors are multiplied with Times in `1`: Times sorts its factors, so that their order and the sign are lost. Write the product with ** ; the evaluation is aborted.";
(* an expression for messages: the copies of the user's kernels that derivatives are taken with respect to by their
   user's names (G -> g$123 in derivativeWithRespectToPropagator, $kernelCopies), the integration variables renamed by
   the package without their context *)
userForm[e_] := (e /. $kernelCopies) /. s_Symbol /; Context[s] === "FunctionalDiagrams`Private`" :> SymbolName[s];

grassmannProductOrOne[{}] := 1;
grassmannProductOrOne[o_List] := GrassmannProduct @@ o;

termToExpression[diagramTerm[c_, o_, {}]] := c grassmannProductOrOne[o];
termToExpression[diagramTerm[c_, o_, d_]] := Module[{factors = factorList[c], outside, inside},
   outside = Select[factors, FreeQ[#, Alternatives @@ d] &];
   inside = Select[factors, ! FreeQ[#, Alternatives @@ d] &];
   (Times @@ outside) IntegralOver[(Times @@ inside) grassmannProductOrOne[o], d]];
termListToExpression[l_List] := Plus @@ (termToExpression /@ l);

(* ========================================================================= *)
(*  4. Integration of delta functions                                        *)
(* ========================================================================= *)

findIntegrableDelta[c_, d_] := Module[{factors = factorList[c]}, Catch[
    Do[With[{f = factors[[i]]}, With[{g = If[Head[f] === Power && Head[f[[1]]] === DeltaFunction, f[[1]], f]},
       If[Head[g] === DeltaFunction && Length[g] == 2 && g[[1]] =!= g[[2]],
        If[MemberQ[d, g[[1]]], Throw[{i, g[[1]], g[[2]]}]];
        If[MemberQ[d, g[[2]]], Throw[{i, g[[2]], g[[1]]}]]]]], {i, Length[factors]}];
    None]];

(* a term without delta functions is left as it is. The deltas are integrated one at a time, on the product of the
   deltas alone: their relative order is the same as in the whole product (Times sorts its factors by one total order,
   and a delta combines with nothing but a delta), so the substitutions x -> y are those of integrating them in the whole
   term, composed into one list of rules applied once to the other factors and the odd atoms. A term that vanishes is
   integrated as before, one substitution at a time in the whole term, which stops where it vanishes *)
deltaFactorQ[f_] := Head[f] === DeltaFunction || (Head[f] === Power && Head[f[[1]]] === DeltaFunction);
integrateDeltaFunctions[t : diagramTerm[c0_, o0_, d0_]] := If[FreeQ[c0, DeltaFunction], t,
   Module[{factors = factorList[c0], deltaMask, cd, d = d0, found, fs, f, i, x, y, rules = {}, c},
    deltaMask = deltaFactorQ /@ factors;
    cd = Times @@ Pick[factors, deltaMask];
    While[(found = findIntegrableDelta[cd, d]) =!= None,
     {i, x, y} = found; fs = factorList[cd]; f = fs[[i]];
     cd = (Times @@ Delete[fs, i]) If[Head[f] === Power, f[[1]]^(f[[2]] - 1), 1];
     cd = cd /. x -> y; rules = Append[rules /. x -> y, x -> y]; d = DeleteCases[d, x]];
    If[rules === {}, normalizeExternalDeltas[t],
     c = If[cd === 0, 0, cd ((Times @@ Pick[factors, deltaMask, False]) /. rules)];
     If[c === 0, integrateDeltaFunctionsInTerm[t], normalizeExternalDeltas[diagramTerm[c, o0 /. rules, d]]]]]];
integrateDeltaFunctionsInTerm[diagramTerm[c0_, o0_, d0_]] := Module[{c = c0, o = o0, d = d0, found, factors, f, i, x, y},
   While[(found = findIntegrableDelta[c, d]) =!= None,
    {i, x, y} = found; factors = factorList[c]; f = factors[[i]];
    c = (Times @@ Delete[factors, i]) If[Head[f] === Power, f[[1]]^(f[[2]] - 1), 1];
    c = c /. x -> y; o = o /. x -> y; d = DeleteCases[d, x]];
   normalizeExternalDeltas[diagramTerm[c, o, d]]];

(* Delta functions between labels that are not integrated over: the labels of a class joined by deltas are replaced,
   everywhere else in the term, by the first of them in canonical order; delta(a, b) f(a, b) = delta(a, b) f(a, a). *)
normalizeExternalDeltas[t : diagramTerm[c_, o_, d_]] := If[FreeQ[c, DeltaFunction], t, Module[{factors = expandIntegerPowers[factorList[c]], positions, classes, rules},
   positions = Select[Range[Length[factors]], Head[factors[[#]]] === DeltaFunction && Length[factors[[#]]] == 2 &&
       factors[[#, 1]] =!= factors[[#, 2]] && FreeQ[factors[[#]], Alternatives @@ d] &];
   If[positions === {}, t,
    classes = Sort /@ mergeOverlapping[{}, Table[List @@ factors[[p]], {p, positions}]];
    rules = Flatten[Table[(# -> First[cl] &) /@ Rest[cl], {cl, classes}]];
    diagramTerm[((Times @@ Delete[factors, List /@ positions]) /. rules) Times @@ Flatten[Table[DeltaFunction[First[cl], m], {cl, classes}, {m, Rest[cl]}]],
     o /. rules, d]]]];

(* ========================================================================= *)
(*  5. The functional derivative                                             *)
(* ========================================================================= *)

differentiateTermList[terms_List, source_] := Join @@ (differentiateTerm[#, source] & /@ terms);

differentiateTerm[diagramTerm[c_, o_, d_], source_] := Module[{result, sign, derivatives},
   (* derivative of the commuting part, placed to the left of the odd factors *)
   result = multiplyTermLists[differentiateCommutingPart[c, source], {diagramTerm[1, o, d]}];
   (* derivative of the odd atoms, with the Grassmann sign of a left derivative *)
   Do[derivatives = differentiateAtom[o[[k]], source];
    sign = If[grassmannSourceQ[source], (-1)^(k - 1), 1];
    result = Join[result, (diagramTerm[sign c #[[1]], Join[Take[o, k - 1], #[[2]], Drop[o, k]], Join[d, #[[3]]]] &) /@ derivatives],
    {k, Length[o]}];
   integrateDeltaFunctions /@ result];

differentiateCommutingPart[c_, source_] := Module[{factors = factorList[c], out = {}, plain},
   plain = plainFactorsQ[factors];
   Do[If[dependsOnSourceQ[factors[[i]], source],
     out = Join[out, multiplyTermLists[differentiateFactor[factors[[i]], source],
        If[plain, {diagramTerm[Times @@ Delete[factors, i], {}, {}]}, toTermList[Times @@ Delete[factors, i]]]]]],
    {i, Length[factors]}];
   out];

differentiateAtom[a_, source_] := Which[
   ! dependsOnSourceQ[a, source], {},
   sourceAtomQ[a] && Head[a] === Head[source] && Length[a] == Length[source],
   {diagramTerm[Times @@ MapThread[DeltaFunction, {List @@ a, List @@ source}], {}, {}]},
   sourceAtomQ[a], {},
   True, With[{r = Replace[{a, source}, $derivativeRules]}, If[r === {a, source}, {}, toTermList[r]]]];

differentiateFactor[f_, source_] := Which[
   ! dependsOnSourceQ[f, source], {},
   sourceAtomQ[f] || declaredDerivativeAtomQ[f], differentiateAtom[f, source],
   Head[f] === Power, differentiatePower[f[[1]], f[[2]], source],
   Head[f] === Log, With[{split = sharedExponential[f[[1]]]}, If[split === None,
     multiplyTermLists[toTermList[1/f[[1]]], differentiateTermList[toTermList[f[[1]]], source]],
     (* Log[E^X p] = X + Log[p]: the Gaussian factor of Z = Z0 + Z1 + ... is not carried through the chain rule *)
     Join[differentiateTermList[toTermList[split[[1]]], source],
      multiplyTermLists[toTermList[1/split[[2]]], differentiateTermList[toTermList[split[[2]]], source]]]]],
   MemberQ[{Plus, Times, IntegralOver, GrassmannProduct, NonCommutativeMultiply}, Head[f]], differentiateTermList[toTermList[f], source],
   True, (* any other function: chain rule *)
   Join @@ Table[If[dependsOnSourceQ[f[[i]], source],
      multiplyTermLists[toTermList[(Derivative @@ UnitVector[Length[f], i])[Head[f]] @@ (List @@ f)],
       differentiateTermList[toTermList[f[[i]]], source]], {}], {i, Length[f]}]];

(* a sum whose every term has one exponential factor E^X, the same X up to the names of its integration variables (as
   Z0 + Z1 + Z2/2 of a Gaussian Z0): {X, the sum with E^X divided out}; None otherwise *)
sharedExponential[s_Plus] := Module[{terms = With[{x = Expand[s]}, If[Head[x] === Plus, List @@ x, {x}]], xs, canon},
   xs = (Cases[If[Head[#] === Times, List @@ #, {#}], Power[E, x_] :> x] &) /@ terms;
   If[! AllTrue[xs, Length[#] == 1 &], Return[None]];
   xs = xs[[All, 1]];
   canon = SimplifyDiagrams /@ xs;
   If[Length[Union[canon]] != 1, None, {First[canon], Total[MapThread[#1/Power[E, #2] &, {terms, xs}]]}]];
sharedExponential[_] := None;

differentiatePower[b_, n_, source_] := Which[
   ! dependsOnSourceQ[n, source], multiplyTermLists[toTermList[n b^(n - 1)], differentiateTermList[toTermList[b], source]],
   ! dependsOnSourceQ[b, source], multiplyTermLists[{diagramTerm[b^n Log[b], {}, {}]}, differentiateTermList[toTermList[n], source]],
   True, differentiateFactor[Exp[n Log[b]], source]];

(* ========================================================================= *)
(*  6. Canonical form                                                        *)
(*  A colour refinement of the integration variables (each is characterised  *)
(*  by the factors it appears in, iteratively), followed by individualisation *)
(*  of the remaining ties, as in graph-isomorphism algorithms. Transpositions *)
(*  that leave a term invariant are pruned; a term that is odd under one of   *)
(*  them vanishes.                                                           *)
(* ========================================================================= *)

inertForm[e_?AtomQ] := e;
inertForm[e_] := inert[inertForm[Head[e]], inertForm /@ (List @@ e)];
(* during a canonical form (canonicalTermList) the answers are remembered in $symmetricMemo: the declarations and the
   attributes cannot change meanwhile *)
$symmetricMemo = None;
symmetricHeadQ[h_Symbol] := If[AssociationQ[$symmetricMemo], Lookup[$symmetricMemo, h, $symmetricMemo[h] = symmetricHeadQAt[h]],
   symmetricHeadQAt[h]];
symmetricHeadQ[_] := False;
symmetricHeadQAt[h_Symbol] := MemberQ[Join[{Times, Plus, DeltaFunction}, $symmetricHeads, $antisymmetricHeads], h] ||
   MemberQ[Attributes[h], Orderless];
normalizedInertForm[inert[h_, args_List]] := With[{a = normalizedInertForm /@ args}, inert[h, If[symmetricHeadQ[h], Sort[a], a]]];
normalizedInertForm[e_] := e;

(* the rank of each descriptor among the distinct ones, looked up in an association; descriptors with inexact numbers
   (machine reals that Union takes as equal may be different keys) by rules: they contain no patterns (heads and atoms
   of the factors are wrapped in inert), so that they can be the left-hand sides of rules *)
rankOfEach[descriptors_List] := With[{u = Union[descriptors]}, If[Precision[descriptors] === Infinity,
    Lookup[AssociationThread[u -> Range[Length[u]]], descriptors], Replace[descriptors, Thread[u -> Range[Length[u]]], {1}]]];

(* The data of the refinement, computed once per term: for each inert factor the variables it contains and their
   positions in d; for each variable the factors that contain it. A flat factor (an atomic head with atomic arguments,
   which most factors are) is also stored as its head, whether the head is symmetric, and a template: the indices of
   its arguments in the list of values Join[colours of d, constants]. *)
refinementData[inerts_, d_] := Module[{n = Length[d], marked, positions, flat, constants, rules},
   marked = inerts /. Thread[d -> Table[refinementVariable[j], {j, n}]];
   positions = Table[Union[Cases[f, refinementVariable[j_] :> j, {0, Infinity}]], {f, marked}];
   flat = Table[MatchQ[f, inert[_?AtomQ, {___?AtomQ}]], {f, inerts}];
   constants = Union[Select[Flatten[Table[If[flat[[k]], marked[[k, 2]], {}], {k, Length[inerts]}]], AtomQ]];
   rules = Join[{refinementVariable[j_] :> j}, Thread[constants -> n + Range[Length[constants]]]];
   {inerts, d, positions, Table[First /@ Position[positions, i, {2}], {i, n}], flat,
    Table[If[flat[[k]], {inerts[[k, 1]], symmetricHeadQ[inerts[[k, 1]]], Replace[marked[[k, 2]], rules, {1}]}, None], {k, Length[inerts]}],
    constants}];

(* The descriptor of the variable i: its colour and the factors that contain it, with i marked SELF and the other variables
   replaced by their colours (normalizedInertForm[f /. {d[[i]] -> SELF, d[[j]] -> COLOUR[colours[[j]]]}], computed directly
   for the flat factors from values = Join[COLOUR /@ colours, constants], made once per round). The descriptors are ordered
   by the colour first, so that a variable alone in its colour keeps its place whatever its factors: they are not computed
   for it. Once every variable has a colour of its own, a further round gives the same colours: the refinement stops
   without it *)
colourRefinement[{inerts_, d_, positions_, incidence_, flat_, flatData_, constants_}, colours0_] := Module[{colours = colours0, new,
    again = True, values, mine, shared},
   While[again,
    values = Join[COLOUR /@ colours, constants];
    shared = Cases[Tally[colours], {c_, m_ /; m > 1} :> c];
    new = rankOfEach[Table[If[! MemberQ[shared, colours[[i]]], {colours[[i]], {}},
        mine = values; mine[[i]] = SELF;
        {colours[[i]], Sort[Table[If[flat[[k]], With[{a = mine[[flatData[[k, 3]]]]}, inert[flatData[[k, 1]], If[flatData[[k, 2]], Sort[a], a]]],
            normalizedInertForm[inerts[[k]] /. Thread[d[[positions[[k]]]] -> mine[[positions[[k]]]]]]], {k, incidence[[i]]}]]}],
       {i, Length[colours]}]];
    again = Length[Union[new]] > Length[Union[colours]] && Length[Union[new]] < Length[new];
    colours = new];
   colours];

(* evaluated key of a term under a renaming rule: {key, coefficient} *)
termKey[c_, o_, rule_] := Module[{cRenamed = recanonicalizeNestedIntegrals[c /. rule, rule[[All, 2]]], oRenamed = o /. rule, numbers, sign, sorted},
   numbers = numericFactor /@ oRenamed;
   If[cRenamed === 0 || MemberQ[numbers, 0], {None, 0},
    {sign, sorted} = sortGrassmannAtoms[oRenamed/numbers];
    If[sign === 0, {None, 0}, {{cRenamed/numericFactor[cRenamed], sorted}, numericFactor[cRenamed] (Times @@ numbers) sign}]]];

(* the transposition of x and y is an automorphism (+1), an odd automorphism (-1), or neither (0); k1 is the key of the
   identity, termKey[c, o, {}] *)
transpositionSymmetrySign[c_, o_, x_, y_, k1_] := Module[{k2 = termKey[c, o, {x -> y, y -> x}]},
   Which[k1[[1]] =!= k2[[1]], 0, k1[[2]] === k2[[2]], 1, k1[[2]] === -k2[[2]], -1, True, 0]];

(* The search tree of the individualisation: the leaves are the orders of the variables (names: their canonical names, so
   that a leaf has a key). At a node whose target cell {m1, ..., mk} is not fully symmetric the subtree of m1 is searched;
   for each other member m only the leftmost leaf of its subtree (leftmostOnly) is computed. Refinement and
   individualisation keep the order of the cells, so the permutation between the two leftmost leaves fixes the prefix and
   maps m1 to m: if their keys and coefficients are equal it is an automorphism and the subtree of m (its image) gives the
   same keys, so it is skipped; opposite coefficients, an odd automorphism: the term vanishes. In integrated trees a leaf
   can only be exchanged together with its vertex (a product of transpositions), which the transpositions do not see.
   keys holds the keys of the term, each computed once: keys["Identity"], keys["Transposition", {x, y}] (the sign of the
   transposition of x and y), keys["Leaf", order] *)
individualizationSearch[keys_, data_, d_, colours0_, prefix_List, leftmostOnly_ : False] := Module[{colours = colourRefinement[data, colours0],
    target, members, signs, k, branch, first, firstKey, out, key, leaf, done},
   If[Length[Union[colours]] == Length[d],
    {Transpose[Sort[Transpose[{colours, d}]]][[2]]},
    target = First[Select[Union[colours], Count[colours, #] > 1 &]];
    members = Select[Range[Length[d]], colours[[#]] == target &];
    signs = Table[keys["Transposition", Sort[{d[[members[[1]]]], d[[m]]}]], {m, Rest[members]}];
    k = Length[members];
    (* the colours after individualising the member m *)
    branch = Function[m, Table[If[j == m, 2 colours[[j]] - 1, 2 colours[[j]]], {j, Length[d]}]];
    Which[
     MemberQ[signs, -1], Throw[$vanishingTerm, canonical],
     (* all transpositions with the first member are automorphisms: they generate the symmetric group of the class, so
        that the class is fully symmetric and its members are individualised at once, in one branch *)
     FreeQ[signs, 0],
     individualizationSearch[keys, data, d, Table[With[{p = Position[members, j, {1}]},
        If[p === {}, (k + 1) colours[[j]], (k + 1) colours[[j]] - (k + 1) + p[[1, 1]]]], {j, Length[d]}], Join[prefix, members], leftmostOnly],
     leftmostOnly, individualizationSearch[keys, data, d, branch[members[[1]]], Append[prefix, members[[1]]], True],
     True,
     first = individualizationSearch[keys, data, d, branch[members[[1]]], Append[prefix, members[[1]]]];
     firstKey = keys["Leaf", First[first]];
     out = first; done = {members[[1]]};
     Do[
      (* a member in the orbit of one done, under the automorphisms found so far that fix the prefix: its subtree is the
         image of one searched, with the same keys *)
      If[! MemberQ[automorphismOrbit[done, Select[keys["Automorphisms"], Function[pi, AllTrue[prefix, pi[[#]] == # &]]]], m],
       leaf = First[individualizationSearch[keys, data, d, branch[m], Append[prefix, m], True]];
       key = keys["Leaf", leaf];
       Which[
        firstKey[[2]] =!= 0 && key[[1]] === firstKey[[1]] && key[[2]] === firstKey[[2]],
        (* the permutation taking this leaf to the first one is an automorphism of the term *)
        keys["Automorphisms"] = Append[keys["Automorphisms"], leafPermutation[d, leaf, First[first]]],
        firstKey[[2]] =!= 0 && key[[1]] === firstKey[[1]] && key[[2]] === -firstKey[[2]], Throw[$vanishingTerm, canonical],
        True, out = Join[out, individualizationSearch[keys, data, d, branch[m], Append[prefix, m]]]];
       AppendTo[done, m]],
      {m, Rest[members]}];
     out]]];

(* the permutation of the positions in d that takes the leaf l1 to the leaf l2; the orbit of a set of positions under a
   list of permutations *)
leafPermutation[d_, l1_, l2_] := Module[{pi = Range[Length[d]], index},
   index = Function[v, First[First[Position[d, v, {1}, Heads -> False]]]];
   Do[pi[[index[l1[[j]]]]] = index[l2[[j]]], {j, Length[d]}];
   pi];
automorphismOrbit[start_List, perms_List] := Module[{orbit = start, new = start, images},
   While[new =!= {},
    images = Complement[Union[Flatten[Table[pi[[new]], {pi, perms}]]], orbit];
    orbit = Union[orbit, images]; new = images];
   orbit];

canonicalizeNestedIntegrals[c_, depth_] := c /. i_IntegralOver :> nestedCanonicalExpression[i, depth + 1];
(* the canonical form of a nested integral depends on the names of the outer variables it contains: it is computed
   again once they have their canonical names, so that the key of a term does not depend on the names it came with.
   Only the nested integrals that contain one of the renamed variables vars can change: the others are already in
   canonical form (canonicalizeNestedIntegrals) *)
recanonicalizeNestedIntegrals[c_, vars_List] := If[FreeQ[c, IntegralOver] || vars === {}, c,
   c /. i : IntegralOver[_, {DummyVariable[depth_, _], ___}] /; ! FreeQ[i, Alternatives @@ vars] :> nestedCanonicalExpression[i, depth]];
canonicalExpression[e_, depth_] := termListToExpression[canonicalTermList[toTermList[e], depth]];
(* the canonical form of a nested integral, computed once for each integral and depth during a canonical form at depth 1
   (the same integrals, such as the exponent of Z0 or the integrals of a sum z in Log[1 + z], occur in many terms); the
   memo nestedMemo lives as long as that canonical form, so that the declarations cannot change meanwhile *)
nestedCanonicalExpression[i_, depth_] := If[! TrueQ[$nestedMemoActive], canonicalExpression[i, depth],
   With[{m = nestedMemo[i, depth]}, If[Head[m] === nestedMemo, nestedMemo[i, depth] = canonicalExpression[i, depth], m]]];

(* returns {coefficient, diagramTerm[rest, odds, dummies]}, or {0, None} *)
canonicalTerm[diagramTerm[c0_, o0_, d_], depth_] := Module[{c, o, result},
   {c, o} = List @@ normalizeExternalDeltas[diagramTerm[c0, o0, d]][[{1, 2}]];
   c = canonicalizeNestedIntegrals[c, depth];
   result = If[d === {}, canonicalTermWithoutDummies[c, o], Catch[canonicalTermWithDummies[c, o, d, depth], canonical]];
   If[result === $vanishingTerm, {0, None}, result]];

canonicalTermWithoutDummies[c_, o_] := With[{k = termKey[c, o, {}]},
   If[k[[2]] === 0, {0, None}, {k[[2]], diagramTerm[k[[1, 1]], k[[1, 2]], {}]}]];

canonicalTermWithDummies[c0_, o0_, d0_, depth_] := Module[{canonicalNames = Table[DummyVariable[depth, k], {k, Length[d0]}],
    d = freshSymbols[Length[d0]], c, o, inerts, orders, candidates, best, coefficients, keys},
   (* work with atomic symbols, whatever the incoming names (e.g. canonical DummyVariable[..]) *)
   c = c0 /. Thread[d0 -> d]; o = o0 /. Thread[d0 -> d];
   (* the variables of nested integrals are masked: their numbering depends on the outer names *)
   inerts = inertForm /@ (Select[Join[factorList[c], o], ! FreeQ[#, Alternatives @@ d] &] /. DummyVariable[k_ /; k > depth, _] -> nestedVariable);
   (* the keys of the term, each computed once *)
   keys["Identity"] := keys["Identity"] = termKey[c, o, {}];
   keys["Transposition", {x_, y_}] := keys["Transposition", {x, y}] = transpositionSymmetrySign[c, o, x, y, keys["Identity"]];
   keys["Leaf", p_List] := keys["Leaf", p] = termKey[c, o, Thread[p -> canonicalNames]];
   keys["Automorphisms"] = {};
   orders = individualizationSearch[keys, refinementData[inerts, d], d, Table[1, {Length[d]}], {}];
   candidates = Select[keys["Leaf", #] & /@ orders, #[[2]] =!= 0 &];
   If[candidates === {}, {0, None},
    best = First[Sort[candidates[[All, 1]]]];
    coefficients = Union[Select[candidates, #[[1]] === best &][[All, 2]]];
    If[Length[coefficients] > 1, {0, None}, {First[coefficients], diagramTerm[best[[1]], best[[2]], canonicalNames]}]]];

(* Copies: many raw terms are copies of one another (the Wick sums of real fields, SuperfieldComponents, the derivatives
   of exp(1/2 J G J), whose copies have fresh names), and the canonical form is computed once for the first term of each
   key, scaled for the others. The key of a term: {its product without the exact numeric factor and its odd atoms, the
   variables renamed by their position in d inside Hold (nothing evaluated), that factor}; two terms with one key are
   n2/n1 times each other renamed, and the canonical form is invariant under the renaming and scales with an exact
   factor. A term that is a number, or whose numeric factor is inexact, has a key of its own (its position i). *)
copyKey[diagramTerm[c_, o_, d_], i_] := If[Head[c] === Times && NumberQ[First[c]],
   If[ExactNumberQ[First[c]], {{Length[d], Delete[Hold[c, o], {1, 1}] /. Thread[d -> Array[copySlot, Length[d]]]}, First[c]},
    {{"single", i}, 1}],
   If[NumberQ[c], {{"single", i}, 1}, {{Length[d], Hold[c, o] /. Thread[d -> Array[copySlot, Length[d]]]}, 1}]];
copyKey[_, i_] := {{"single", i}, 1};
(* the canonical forms of the terms, in their order: computed for the first term of each key (in the order of the
   terms), the copies' from it *)
canonicalTermsOfCopies[terms_List, keys_List, depth_] := Module[{factors = keys[[All, 2]], firstOf, representatives, canonicalOf},
   firstOf = Lookup[AssociationThread[Reverse[keys[[All, 1]]] -> Reverse[Range[Length[terms]]]], keys[[All, 1]]];
   representatives = DeleteDuplicates[firstOf];
   canonicalOf = AssociationThread[representatives -> (canonicalTerm[#, depth] & /@ terms[[representatives]])];
   MapThread[Function[{k, f, nk, nf}, If[k === f, canonicalOf[f],
       With[{r = canonicalOf[f]}, If[r[[1]] === 0, r, {r[[1]] nk/nf, r[[2]]}]]]], {Range[Length[terms]], firstOf, factors, factors[[firstOf]]}]];

canonicalTermList[terms_List, depth_ : 1] := If[! AssociationQ[$symmetricMemo],
   Block[{$symmetricMemo = <||>}, canonicalTermList[terms, depth]], canonicalTermListMemo[terms, depth]];
canonicalTermListMemo[terms_List, depth_] := If[depth === 1 && ! TrueQ[$nestedMemoActive] && ! FreeQ[terms, IntegralOver],
   Block[{$nestedMemoActive = True, nestedMemo}, canonicalTermListAt[terms, depth]], canonicalTermListAt[terms, depth]];
canonicalTermListAt[terms_List, depth_] := Module[{keys = MapIndexed[copyKey[#1, First[#2]] &, terms], canonical, groups},
   canonical = Select[If[DuplicateFreeQ[keys[[All, 1]]], canonicalTerm[#, depth] & /@ terms, canonicalTermsOfCopies[terms, keys, depth]],
     #[[1]] =!= 0 &];
   groups = GatherBy[canonical, Last];
   Select[Table[With[{w = Together[Total[g[[All, 1]]]]}, diagramTerm[w First[g][[2, 1]], First[g][[2, 2]], First[g][[2, 3]]]],
     {g, groups}], #[[1]] =!= 0 &]];

(* ========================================================================= *)
(*  7. The user-level calculus                                               *)
(* ========================================================================= *)

(* "MaximumOrder" -> {x, n}: only the orders 0 to n in x of the result (OrderPart[result, x, Range[0, n]]), the terms of
   higher order dropped after each derivative. This needs derivatives that never lower the order of a term: x a coupling,
   or the head of a line that is neither a source nor a head with a declared derivative (DeclareFunctionalDerivative);
   otherwise the whole derivative is taken, then cut *)
FunctionalDerivative[e_, s__, "MaximumOrder" -> {x_, n_Integer}] := With[{chain = derivativeChain[Hold[FunctionalDerivative[e, s]]]},
   If[chain === $Failed || ! orderKeepingQ[x], OrderPart[FunctionalDerivative[e, s], x, Range[0, n]],
    With[{order = {x, ! FreeQ[e, x[___]], n}},
     chain[[3]] termListToExpression[differentiateUpToOrder[toTermList[seriesLogs[ReleaseHold[chain[[1]]], order]], chain[[2]], order, False]]]]];
orderKeepingQ[x_] := ! MemberQ[sourceHeads[], x] && ! MemberQ[$derivativePairs[[All, 1]], x];
(* with an order in a coupling x: Log[s] of a sum polynomial in x is replaced by its power series in x up to x^n (after
   the shared exponential is split off, Log[E^X p] = X + Log[p]): Log[p0 + d] = Log[p0] + Sum_k (-1)^(k+1) (d/p0)^k/k,
   d = p - p0 of order >= 1, so that the terms of higher order are dropped by the order, as for a polynomial *)
seriesLogs[e_, {x_, headQ_, n_}] := If[headQ || FreeQ[e, Log], e, e /. Log[s_] /; ! FreeQ[s, x] :> logSeries[s, x, n]];
logSeries[s_, x_, n_] := With[{split = sharedExponential[s]}, If[split === None, logPowerSeries[s, x, n], split[[1]] + logPowerSeries[split[[2]], x, n]]];
(* d = p - p0 by order, d_j x^j; only the products of total order n or less of the powers of d are written: d^k as the
   products d_j1 ... d_jk over the compositions j1 + ... + jk <= n (2^n - 1 of them for all k together) *)
logPowerSeries[p_, x_, n_] := With[{q = Expand[p]}, With[{p0 = q /. x -> 0}, If[p0 === 0 || ! PolynomialQ[q, x], Log[p],
    With[{ds = Table[Coefficient[q, x, j] x^j/p0, {j, n}]},
     Log[p0] + Sum[(-1)^(k + 1)/k Total[(Times @@ ds[[#]] &) /@ compositionsUpTo[k, n]], {k, n}]]]]];
(* the k-tuples of positive integers with total m or less, in lexicographic order *)
compositionsUpTo[0, _] := {{}};
compositionsUpTo[k_Integer, m_Integer] := If[m < k, {}, Join @@ Table[(Prepend[#, j] &) /@ compositionsUpTo[k - 1, m - j], {j, m - k + 1}]];
FunctionalDerivative[e_, l_List] := FunctionalDerivative[e, Sequence @@ l];
FunctionalDerivative::nosource = "`1` is not a declared source (DeclareSource, DeclareGrassmannSource): the derivative with respect to it is 0.";
(* a kernel of an antisymmetric head written with its labels out of canonical order, h[y, x] (as the propagator
   G_psibarpsibar of a superfield), has already evaluated to -h[x, y]: the derivative with respect to it is minus the
   derivative with respect to h[x, y] *)
FunctionalDerivative[e_, Times[-1, k : (g_Symbol[__])]] /; Context[g] =!= "System`" := -FunctionalDerivative[e, k];
FunctionalDerivative[e_, source_] := If[! sourceAtomQ[source], Message[FunctionalDerivative::nosource, source]; 0,
   termListToExpression[canonicalTermList[differentiateTermList[toTermList[e], source]]]];
FunctionalDerivative[e_, sources__, last_] := FunctionalDerivative[FunctionalDerivative[e, last], sources];

(* the raw terms of an expression, with their delta functions integrated, not yet in canonical form; the canonical terms *)
rawTermsOf[e_] := integrateDeltaFunctions /@ toTermList[e];
canonicalTermsOf[e_] := canonicalTermList[rawTermsOf[e]];
SimplifyDiagrams[e_] := termListToExpression[canonicalTermsOf[e]];
DiagramTerms[e_] := termToExpression /@ canonicalTermsOf[e];

setSourcesToZeroInTerm[diagramTerm[c_, o_, d_]] := If[AnyTrue[o, sourceDependentOddAtomQ], {},
   With[{cc = c //. {a_ /; sourceAtomQ[a] :> 0} //. {(GrassmannProduct | NonCommutativeMultiply)[___, 0, ___] -> 0, IntegralOver[0, _] -> 0}},
    If[cc === 0, {}, {diagramTerm[cc, o, d]}]]];
(* SetSourcesToZero holds its argument: the derivatives of a FunctionalDerivative written in it (directly, or nested) are
   taken one at a time with the terms that cannot survive at zero sources dropped after each of them (the number of their
   factors that vanish at zero sources, sourceDegree, is larger than the number of derivatives left: a derivative removes
   at most one of them), as GaussianExpectationValue does; the result is the same *)
SetAttributes[SetSourcesToZero, HoldFirst];
SetSourcesToZero[d : FunctionalDerivative[_, __]] := With[{chain = derivativeChain[Hold[d]]},
   If[chain === $Failed, setSourcesToZero[d],
    chain[[3]] termListToExpression[canonicalTermList[If[chain[[4]] === None,
       differentiateAndSetSourcesToZero[toTermList[ReleaseHold[chain[[1]]]], chain[[2]]],
       differentiateUpToOrder[toTermList[seriesLogs[ReleaseHold[chain[[1]]], chain[[4]]]], chain[[2]], chain[[4]], True]]]]]];
(* the derivatives (the last source first) with a canonical form after each. A term is dropped when its order in x can
   be read (x, or x[..] when headQ, only in factors x^k) and is above n: the derivatives do not lower it. A term whose
   order cannot be read (x in an exponential, a logarithm, a non-integer power or a nested integral) is kept; atZero, the
   terms that cannot survive at zero sources are dropped too, and the sources are then set to zero. At the end the orders
   0 to n are kept, read as OrderPart reads them *)
differentiateUpToOrder[terms0_List, sources_List, {x_, headQ_, n_}, atZero_] := Module[{terms = terms0, k},
   terms = Select[terms, ! orderAboveQ[#, x, headQ, n] &];
   Do[terms = Select[canonicalTermList[differentiateTermList[terms, sources[[k]]]], ! orderAboveQ[#, x, headQ, n] && (! atZero || sourceDegree[#] <= k - 1) &],
    {k, Length[sources], 1, -1}];
   If[atZero, terms = Join @@ (setSourcesToZeroInTerm /@ terms)];
   Select[terms, MemberQ[Range[0, n], termOrder[#, x, headQ]] &]];
orderAboveQ[t : diagramTerm[c_, _, _], x_, headQ_, n_] := AllTrue[factorList[c], If[headQ,
     FreeQ[#, x[___]] || Head[#] === x || (Head[#] === Power && Head[#[[1]]] === x && IntegerQ[#[[2]]] && #[[2]] > 0),
     FreeQ[#, x] || # === x || (Head[#] === Power && #[[1]] === x && IntegerQ[#[[2]]] && #[[2]] > 0)] &] &&
   termOrder[t, x, headQ] > n;
SetSourcesToZero[e_] := setSourcesToZero[e];
setSourcesToZero[e_] := termListToExpression[canonicalTermList[Join @@ (setSourcesToZeroInTerm /@ toTermList[e])]];
(* {held expression, sources (the last one acts first), sign, order} of nested FunctionalDerivative[...], with the order
   {x, headQ, n} of "MaximumOrder" -> {x, n} or None; $Failed when a source is not a declared source atom (the derivative
   then gives its message) or the order cannot be used while differentiating *)
SetAttributes[derivativeChain, HoldAll];
derivativeChain[Hold[FunctionalDerivative[e_, s__]]] := Module[{inner = derivativeChain[Hold[e]], sources = Flatten[{s}], signs, order},
   order = Cases[sources, ("MaximumOrder" -> {x_, n_Integer}) :> {x, n}];
   sources = DeleteCases[sources, "MaximumOrder" -> _];
   signs = Replace[sources, {Times[-1, k : (g_Symbol[__])] /; Context[g] =!= "System`" :> -1, _ -> 1}, {1}];
   sources = Replace[sources, Times[-1, k : (g_Symbol[__])] /; Context[g] =!= "System`" :> k, {1}];
   Which[
    (* an order given only to the outermost derivative: the inner ones may then be cut at it too, not the converse *)
    inner === $Failed || ! AllTrue[sources, sourceAtomQ] || Length[order] > 1 || inner[[4]] =!= None, $Failed,
    order === {}, {inner[[1]], Join[sources, inner[[2]]], inner[[3]] Times @@ signs, None},
    ! orderKeepingQ[order[[1, 1]]], $Failed,
    True, With[{x = order[[1, 1]]}, {inner[[1]], Join[sources, inner[[2]]], inner[[3]] Times @@ signs,
      {x, ! FreeQ[ReleaseHold[inner[[1]]], x[___]], order[[1, 2]]}}]]];
derivativeChain[Hold[e_]] := {Hold[e], {}, 1, None};

fieldOperator[f_] := Module[{m = Select[$fieldSourceMap, #[[1]] === Head[f] &]},
   If[m === {}, Message[FieldDerivative::nofield, f]; {1, f}, {m[[1, 3]], m[[1, 2]] @@ (List @@ f)}]];
FieldDerivative::nofield = "No source declared for `1` (use DeclareField).";

(* a composite operator may be written as a product of fields: psibar[a] ** psi[a] stands for the two entries psibar[a],
   psi[a] of the list, and a power or a Times of commuting fields for its factors. Grassmann fields multiplied with Times
   have lost their order: $Failed, with a message *)
FieldDerivative::times = "The Grassmann fields in `1` are multiplied with Times, which loses their order: write them as separate entries of the list of fields (or with **).";
expandOperator[(GrassmannProduct | NonCommutativeMultiply)[a___]] := Join @@ (expandOperator /@ {a});
expandOperator[Power[a_, n_Integer?Positive]] := Join @@ Table[expandOperator[a], {n}];
expandOperator[e_Times] := Join @@ (expandOperator /@ (List @@ e));
expandOperator[e_] := {e};
normaliseOperators[operators_List] := With[{bad = Select[operators, Head[#] === Times && Count[List @@ #, _?grassmannOddAtomQ] > 1 &]},
   If[bad =!= {}, Message[FieldDerivative::times, First[bad]]; $Failed, Join @@ (expandOperator /@ operators)]];

FieldDerivative[e_, operators_List] := With[{ops = normaliseOperators[operators]}, If[ops === $Failed, $Failed,
    Fold[With[{p = fieldOperator[#2]}, p[[1]] FunctionalDerivative[#1, p[[2]]]] &, e, Reverse[ops]]]];

FieldExpectationValue[z_, operators_List] := With[{ops = normaliseOperators[operators]}, If[ops === $Failed, $Failed,
    checkGeneratingFunctional[z]; checkFieldArity[z, ops]; SimplifyDiagrams[SetSourcesToZero[FieldDerivative[z, ops]]/SetSourcesToZero[z]]]];

(* a generating functional without any declared source: the sources were not declared, or a propagator named with a
   built-in symbol (D, ...) evaluated to 0, so that every expectation value would vanish *)
FunctionalDiagrams::nosources = "The generating functional `1` contains no declared source: declare its sources with DeclareSource or DeclareGrassmannSource (and do not name a propagator with a built-in symbol such as D).";
checkGeneratingFunctional[z_] := If[FreeQ[z, _?sourceAtomQ], Message[FunctionalDiagrams::nosources, Short[z]]];
(* a field with another number of arguments than its source in Z (psi[x] while Z holds eta[x, s], with a spin index):
   every expectation value of it vanishes *)
FunctionalDiagrams::fieldarity = "The field `1` has `2` argument(s), but its source `3` occurs in the generating functional only with `4`: the expectation values of `1` vanish. Give the field the arguments of its source (a spin index: psi[x, up]).";
checkFieldArity[z_, operators_List] := Scan[Function[f, Module[{m = Select[$fieldSourceMap, #[[1]] === Head[f] &], arities},
     If[m =!= {},
      arities = Union[Cases[{z}, m[[1, 2]][a___] :> Length[{a}], {0, Infinity}]];
      If[arities =!= {} && ! MemberQ[arities, Length[f]],
       Message[FunctionalDiagrams::fieldarity, f, Length[f], m[[1, 2]], If[Length[arities] == 1, First[arities], arities]]]]]],
   DeleteDuplicatesBy[operators, {Head[#], Length[#]} &]];
