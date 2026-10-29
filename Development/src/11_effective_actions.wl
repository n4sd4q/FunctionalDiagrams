
(* ========================================================================= *)
(*  20. Effective actions as objects                                         *)
(*                                                                           *)
(*  Operator traces: Tr ln A (TraceLog), Tr A (OperatorTrace) and kernels of *)
(*  operator expressions (OperatorKernel); their functional derivatives and  *)
(*  their expansion into diagrams (ExpandTrace); functional derivatives with *)
(*  respect to two-index objects. The 2PI effective action Gamma[G] and its  *)
(*  field dependence Gamma[phibar, G] (EffectiveAction2PI), the 1PI          *)
(*  effective action Gamma[phibar] (EffectiveAction1PI, phibar = <phi>),     *)
(*  Potthoff's universal functional                                          *)
(*  F[Sigma] (LegendreTransform, LegendrePropagator) and self-energy         *)
(*  functional (SelfEnergyFunctional), and the value of diagrams in zero     *)
(*  dimensions (ZeroDimensionalValue). The sums of logarithms, the traces in *)
(*  momentum and frequency and the limits of a response are in section 17,   *)
(*  the exact equations of the response in section 12a.                      *)
(* ========================================================================= *)


(* ---- operator expressions ---- *)
(* An operator is a kernel (a symbol h standing for the function h[x, y] of two labels, or a LabelFunction), a number
   (times the identity), a sum, a number times an operator, a product A . B (Dot) or an inverse A^-1. The kernel of
   A . B is Integral A(x, u) B(u, y) and Tr(A . B) = Integral A(x, y) B(y, x): with this trace, Tr(Sigma . G) is the
   Integral Sigma(x, y) G(y, x) of SelfEnergy (Sigma(x, y) = dPhi/dG(y, x) for fermions). *)

FunctionalDiagrams::optimes = "`1` multiplies two operators with Times: write a product of operators with Dot (A . B).";
FunctionalDiagrams::opscalar = "In `1` the operator cannot be told from its coefficients: declare its kernel (DeclareDirectedPropagator, DeclareSymmetric, ...). A single symbol in an operator is a kernel: a chemical potential is LabelFunction[mu DeltaFunction[a, b], {a, b}].";
FunctionalDiagrams::opvariation = "The variation of `1` with respect to `2` is not known (sums, coefficients times operators, products, integer powers and inverses are): it is taken as 0.";
FunctionalDiagrams::opfunction = "The kernel `1` depends on `2` through a function of labels (LabelFunction): its derivative inside a trace is not taken.";
FunctionalDiagrams::opzero = "The inverse of the operator `1`, which is zero, is not defined.";
FunctionalDiagrams::opdotpower = "`1` is a power whose exponent is a product: Mathematica reads A^-1 . B as A^(-1 . B). It is taken as `2`; write A^(-1) . B.";
FunctionalDiagrams::opsource = "The operator `1` depends on `2` in a way the derivative cannot follow, and the derivative of `3` is taken as 0: a source enters an operator through a kernel of two labels, a LabelFunction (LabelFunction[lambda/2 phibar[u]^2 DeltaFunction[u, v], {u, v}]) or a head whose derivative is declared (DeclareFunctionalDerivative).";

(* the input of an operator: Inverse[A] is A^-1, and A^(-(1 . B)), which is how Mathematica reads A^-1 . B, is A^-1 . B
   (with a message; likewise A^(n . B)) *)
dotPowerFixed[p_, q_] := (Message[FunctionalDiagrams::opdotpower, p, q]; q);
operatorInput[a_] := a /. {Inverse[b_] :> Power[b, -1],
    p : Power[b_, Times[-1, Dot[n_?NumericQ, c__]]] :> dotPowerFixed[p, Dot[Power[b, -n], c]],
    p : Power[b_, Dot[n_?NumericQ, c__]] :> dotPowerFixed[p, Dot[Power[b, n], c]]};
operatorInputQ[a_] := ! FreeQ[a, _Inverse | Power[_, Dot[_?NumericQ, __] | Times[-1, Dot[_?NumericQ, __]]]];
TraceLog[a_] /; operatorInputQ[a] := TraceLog[operatorInput[a]];
OperatorTrace[a_] /; operatorInputQ[a] := OperatorTrace[operatorInput[a]];
OperatorKernel[a_] /; operatorInputQ[a] := OperatorKernel[operatorInput[a]];

orderlessQ[h_Symbol] := MemberQ[Attributes[h], Orderless];
orderlessQ[_] := False;

dotOf[l_List] := With[{m = DeleteCases[l, 1]}, Which[m === {}, 1, Length[m] == 1, First[m], True, Dot @@ m]];
atomInverse[Power[a_, -1]] := a;
atomInverse[a_] := Power[a, -1];
(* the heads known to be kernels: declared lines, symmetric and antisymmetric heads, sources (the variable of a
   derivative with respect to a kernel is one, through the copy of derivativeWithRespectToPropagator) and heads with a
   declared derivative *)
kernelHeadQ[h_Symbol] := MemberQ[Cases[$lineStyles, {k_, _} :> k], h] || orderlessQ[h] || MemberQ[$antisymmetricHeads, h] ||
   MemberQ[sourceHeads[], h] || MemberQ[Cases[$derivativePairs, {k_, _} :> k], h];
kernelHeadQ[_] := False;
(* a factor of a product that is certainly an operator: it contains a known kernel, a product of operators, a kernel of an
   operator or a function of labels *)
operatorFactorQ[f_] := ! FreeQ[f, _Dot | _Function | _OperatorKernel] || ! FreeQ[f, h_Symbol /; kernelHeadQ[h]];
(* a coefficient and the one operator factor of a product: numbers, and symbols or expressions free of known kernels
   beside a factor that is certainly an operator, are coefficients (lambda G0, -mu S with S declared). A single
   non-numeric factor is the operator (-U, 2 S: an undeclared kernel). Otherwise a message, and $Failed thrown to the
   entry point (ExpandTrace, the derivatives of traces and kernels) *)
splitScalar[a_Times] := Module[{num = Select[List @@ a, NumericQ], rest = Select[List @@ a, ! NumericQ[#] &], ops},
   ops = Select[rest, operatorFactorQ];
   Which[
    Length[rest] == 1, {Times @@ num, First[rest]},
    Length[ops] == 1, {Times @@ Join[num, Select[rest, ! operatorFactorQ[#] &]], First[ops]},
    Length[ops] == 0, Message[FunctionalDiagrams::opscalar, userForm[a]]; Throw[$Failed, operatorFailure],
    True, Message[FunctionalDiagrams::optimes, userForm[a]]; Throw[$Failed, operatorFailure]]];

(* the monomials {c, {a1, ..., ak}} of an operator, c a1 . a2 ... ak, with atoms: kernel symbols, functions of labels,
   and inverses Power[b, -1] of a kernel or of a sum (the sum in normal form); adjacent inverse pairs cancel *)
opMonomials[a_?NumericQ] := {{a, {}}};
opMonomials[a_Symbol] := {{1, {a}}};
opMonomials[f_Function] := {{1, {f}}};
opMonomials[a_Plus] := Join @@ (opMonomials /@ List @@ a);
opMonomials[a_Times] := With[{s = splitScalar[a]}, ({s[[1]] #[[1]], #[[2]]} &) /@ opMonomials[s[[2]]]];
opMonomials[a_Dot] := Fold[productOfMonomials, {{1, {}}}, opMonomials /@ cancelAdjacentInverses[List @@ a]];
opMonomials[Power[b_, -1]] := inverseMonomials[b];
opMonomials[Power[b_, n_Integer?Positive]] := opMonomials[Dot @@ Table[b, {n}]];
opMonomials[Power[b_, n_Integer?Negative]] := opMonomials[Dot @@ Table[Power[b, -1], {-n}]];
opMonomials[a_] := {{1, {a}}};

inverseMonomials[b_?NumericQ] := {{1/b, {}}};
inverseMonomials[b_Symbol] := {{1, {Power[b, -1]}}};
inverseMonomials[f_Function] := {{1, {Power[f, -1]}}};
inverseMonomials[Power[b_, -1]] := opMonomials[b];
inverseMonomials[b_Times] := With[{s = splitScalar[b]}, ({#[[1]]/s[[1]], #[[2]]} &) /@ inverseMonomials[s[[2]]]];
inverseMonomials[b_Dot] := opMonomials[Dot @@ Reverse[Power[#, -1] & /@ (List @@ b)]];
inverseMonomials[b_] := With[{m = collectMonomials[opMonomials[b]]}, Which[
    m === {}, Message[FunctionalDiagrams::opzero, b]; {},
    Length[m] == 1, {{1/m[[1, 1]], Reverse[atomInverse /@ m[[1, 2]]]}},
    (* the inverse of a sum, normalised with the coefficient of its first monomial 1: (G0^-1 - S)^-1 and -(S - G0^-1)^-1
       are the same atom *)
    True, With[{c = m[[1, 1]]}, {{1/c, {Power[opFromMonomials[({#[[1]]/c, #[[2]]} &) /@ m], -1]}}}]]];

productOfMonomials[l1_List, l2_List] := Flatten[Table[{a[[1]] b[[1]], cancelAtoms[Join[a[[2]], b[[2]]]]}, {a, l1}, {b, l2}], 1];
(* adjacent atoms x, x^-1 cancel *)
cancelAtoms[l_List] := Module[{m = l, k},
   While[(k = Catch[Do[If[m[[i + 1]] === atomInverse[m[[i]]], Throw[i]], {i, Length[m] - 1}]; None]) =!= None, m = Drop[m, {k, k + 1}]];
   m];
(* adjacent factors of a product that are inverse to each other, e.g. (G0^-1 - S) . (G0^-1 - S)^-1, cancel before the
   product is expanded *)
operatorNormal[a_] := opFromMonomials[opMonomials[a]];
inverseOperatorQ[p_, q_] := With[{iq = opMonomials[q], ip = inverseMonomials[p]},
   collectMonomials[iq] === collectMonomials[ip]];
cancelAdjacentInverses[l_List] := Module[{m = l, k},
   While[(k = Catch[Do[If[! NumericQ[m[[i]]] && inverseOperatorQ[m[[i]], m[[i + 1]]], Throw[i]], {i, Length[m] - 1}]; None]) =!= None,
    m = Drop[m, {k, k + 1}]];
   If[m === {}, {1}, m]];

(* equal monomials combined (gathered once), zeros dropped, in the canonical order of the atom lists *)
collectMonomials[m_List] := With[{g = GatherBy[m, Last]},
   Select[({Together[Total[#[[All, 1]]]], #[[1, 2]]} &) /@ g[[Ordering[g[[All, 1, 2]]]]], #[[1]] =!= 0 &]];
opFromMonomials[m_List] := Total[(#[[1]] dotOf[#[[2]]] &) /@ collectMonomials[m]];

(* ---- kernels and traces of monomials ---- *)
(* a kernel symbol h gives h[x, y]; a function of labels is applied; an inverse is the kernel OperatorKernel[h^-1][x, y],
   symmetric (its labels in canonical order) when h is *)
OperatorKernel[p : Power[h_Symbol, -1]][x_, y_] /; orderlessQ[h] && ! OrderedQ[{x, y}] := OperatorKernel[p][y, x];
(* the inverse of an antisymmetric kernel (a superfield propagator) is antisymmetric *)
OperatorKernel[Power[h_Symbol, -1]][x_, x_] /; MemberQ[$antisymmetricHeads, h] := 0;
OperatorKernel[p : Power[h_Symbol, -1]][x_, y_] /; MemberQ[$antisymmetricHeads, h] && ! OrderedQ[{x, y}] := -OperatorKernel[p][y, x];
atomKernel[a : Power[_, -1], x_, y_] := OperatorKernel[a][x, y];
atomKernel[a_, x_, y_] := a[x, y];
monomialKernelNow[{c_, {}}, x_, y_] := c DeltaFunction[x, y];
monomialKernelNow[{c_, atoms_List}, x_, y_] := Module[{k = Length[atoms], u = freshSymbols[Length[atoms] - 1], points},
   points = Join[{x}, u, {y}];
   c If[k == 1, atomKernel[First[atoms], x, y], IntegralOver[Times @@ Table[atomKernel[atoms[[i]], points[[i]], points[[i + 1]]], {i, k}], u]]];
(* the monomials of an operator (memoised during one ExpandTrace: the same operator stands for every line of Phi[G]) *)
monomialsOf[a_] := collectMonomials[opMonomials[a]];
kernelOf[a_, x_, y_] := Total[monomialKernel[#, x, y] & /@ monomialsOf[a]];
(* the trace is cyclic: x . ... . x^-1 cancels too *)
cyclicCancel[l_List] := Module[{m = cancelAtoms[l]},
   While[Length[m] >= 2 && Last[m] === atomInverse[First[m]], m = Take[m, {2, -2}]]; m];
monomialTraceNow[{c_, atoms0_List}] := Module[{atoms = cyclicCancel[atoms0], k, u},
   k = Length[atoms];
   If[k == 0, c OperatorTrace[1],
    u = freshSymbols[k];
    c IntegralOver[Times @@ Table[atomKernel[atoms[[i]], u[[i]], u[[If[i == k, 1, i + 1]]]], {i, k}], u]]];
traceOf[a_] := Total[monomialTrace /@ monomialsOf[a]];
(* With "MaximumOrder" -> {x, n} in ExpandTrace ($operatorOrder = {x, headQ}), a monomial of a kernel or a trace is the
   inert orderBounded[m, k, labels] with m a lower bound of the orders of its terms and k the number of the monomial in
   lazyMonomial (blocked in ExpandTrace), written out only when the budget of truncatedRawTerms leaves room for m: the
   long products of series (Phi[G] at G = (G0^-1 - S)^-1) are not written out for the orders that are cut. A function of
   labels in an operator (a LabelFunction, e.g. a self-energy series) is split into its parts of each lowest order
   (gradedFunctions), so that their products are bounded too. *)
$operatorOrder = None;
lazyMonomialNumber[data_List] := ($lazyCount++; lazyMonomial[$lazyCount] = data; $lazyCount);
(* the labels of a kernel stay in the inert expression, orderBounded[m, k, x, y], so that the renaming of integration
   variables (scopedReplace) reaches them *)
monomialKernel[m_, x_, y_] := If[$operatorOrder === None, monomialKernelNow[m, x, y],
   orderBounded[monomialLowerOrder[m], lazyMonomialNumber[{monomialKernelNow, m}], x, y]];
monomialTrace[m_] := If[$operatorOrder === None, monomialTraceNow[m], orderBounded[monomialLowerOrder[m], lazyMonomialNumber[{monomialTraceNow, m}]]];
writtenOut[orderBounded[_, k_, labels___]] := With[{data = lazyMonomial[k]}, data[[1]][data[[2]], labels]];
(* whether the terms of e, with its monomials written out, contain factors x[..] (the headQ of truncatedRawTerms): x is a
   kernel of a monomial, or a function of labels or an inverse in a monomial contains x[..] *)
lazyHeadQ[e_, x_] := ! FreeQ[e, x[___]] || With[{atoms = Union @@ Table[lazyMonomial[k][[2, 2]], {k, $lazyCount}]},
    MemberQ[atoms, x] || ! FreeQ[atoms, x[___]] || ! FreeQ[Table[lazyMonomial[k][[2, 1]], {k, $lazyCount}], x[___]]];
monomialLowerOrder[{c_, atoms_List}] := coefficientLowerOrder[c] + Total[atomLowerOrder /@ atoms];
coefficientLowerOrder[c_] := With[{x = $operatorOrder[[1]]},
   If[$operatorOrder[[2]] || FreeQ[c, x], 0, With[{k = Exponent[c, x]}, If[FreeQ[c/x^k, x], k, 0]]]];
atomLowerOrder[h_Symbol] := If[$operatorOrder[[2]] && h === $operatorOrder[[1]], 1, 0];
atomLowerOrder[f_Function] := functionLowerOrder[f];
atomLowerOrder[_] := 0;
(* the lowest order of the terms of a function of labels (computed once per ExpandTrace: functionLowerOrder is blocked) *)
functionLowerOrderOf[f_Function] := With[{t = toTermList[f @@ freshSymbols[Length[First[f]]]]},
   If[t === {}, 0, Min[termOrder[#, $operatorOrder[[1]], $operatorOrder[[2]]] & /@ t]]];
gradedFunctions[f_Function] := If[Length[f] != 2 || Head[f[[2]]] =!= Plus, f,
   With[{groups = GatherBy[List @@ f[[2]], functionLowerOrder[Function @@ {f[[1]], #}] &]},
    If[Length[groups] < 2, f, Total[(Function @@ {f[[1]], Plus @@ #} &) /@ groups]]]];

(* ---- contraction of inverse kernels: Integral A^-1(a, u) A(u, b) du = delta(a, b) ---- *)
(* the other label of a two-label factor f that contains u at position p (any position for a symmetric kernel) *)
otherLabel[f_, u_, p_, symmetric_] := Which[
   f[[p]] === u, f[[3 - p]],
   symmetric && f[[3 - p]] === u, f[[p]],
   True, None];
(* the other label of an antisymmetric kernel f that contains u, and the sign that brings u to position q *)
antisymmetricOtherLabel[f_, u_, q_] := Which[f[[q]] === u, {f[[3 - q]], 1}, f[[3 - q]] === u, {f[[q]], -1}, True, {None, 0}];
findInversePair[factors_List, d_List] := Catch[
   Do[With[{f = factors[[i]]}, If[MatchQ[f, OperatorKernel[Power[_, -1]][_, _]],
      With[{h = f[[0, 1, 1]], symmetric = orderlessQ[f[[0, 1, 1]]], antisymmetric = MemberQ[$antisymmetricHeads, f[[0, 1, 1]]]},
       Do[With[{u = f[[p]]}, If[MemberQ[d, u] && Count[factors, _?(! FreeQ[#, u] &)] == 2,
           Do[If[j != i && ! AtomQ[factors[[j]]] && Head[factors[[j]]] === h && Length[factors[[j]]] == 2,
             (* h^-1(a, u) h(u, b) or h(a, u) h^-1(u, b); for an antisymmetric h also with u in the other slot of h,
                h(b, u) = -h(u, b) *)
             With[{os = If[antisymmetric, antisymmetricOtherLabel[factors[[j]], u, If[p == 2, 1, 2]],
                 {otherLabel[factors[[j]], u, If[p == 2, 1, 2], symmetric], 1}], own = f[[3 - p]]},
              With[{other = os[[1]]},
               If[other =!= None && u =!= other && u =!= own,
                Throw[{i, j, u, os[[2]] If[p == 2, DeltaFunction[own, other], DeltaFunction[other, own]]}]]]]],
            {j, Length[factors]}]]],
        {p, {1, 2}}]]]],
    {i, Length[factors]}];
   None];
contractInverseTerm[diagramTerm[c_, o_, d_]] := Module[{factors = expandIntegerPowers[factorList[c]], dd = d, pair},
   While[(pair = findInversePair[factors, dd]) =!= None,
    factors = Append[Delete[factors, {{pair[[1]]}, {pair[[2]]}}], pair[[4]]]; dd = DeleteCases[dd, pair[[3]]]];
   integrateDeltaFunctions[diagramTerm[Times @@ factors, o, dd]]];
(* Integral delta(u, u) du, with u in no other factor, is the trace of the identity, OperatorTrace[1] *)
identityTraceTerm[t : diagramTerm[c_, o_, d_]] := Module[{factors = expandIntegerPowers[factorList[c]], k},
   k = Catch[Do[If[MatchQ[factors[[i]], DeltaFunction[u_, u_] /; MemberQ[d, u]] &&
        Count[factors, _?(! FreeQ[#, factors[[i, 1]]] &)] == 1 && FreeQ[o, factors[[i, 1]]], Throw[i]], {i, Length[factors]}]; None];
   If[k === None, t, identityTraceTerm[diagramTerm[(Times @@ Delete[factors, k]) OperatorTrace[1], o, DeleteCases[d, factors[[k, 1]]]]]]];

(* ---- expansion around G0 ---- *)
(* the operator a = c (G0^-1 - X), as {c, G0 . X}; None if a has no single term c G0^-1 *)
splitAroundBare[a_, g0_] := Module[{m = collectMonomials[opMonomials[a]], free, c},
   free = Select[m, #[[2]] === {Power[g0, -1]} &];
   If[Length[free] != 1 || Length[m] == 1, None,
    c = free[[1, 1]];
    {c, operatorNormal[Dot[g0, opFromMonomials[({-#[[1]]/c, #[[2]]} &) /@ DeleteCases[m, free[[1]]]]]]}]];
(* Tr ln(c (G0^-1 - X)) = Tr ln(c G0^-1) - Sum_k Tr((G0 . X)^k)/k *)
traceLogExpansion[a_, None, _] := TraceLog[a];
traceLogExpansion[a_, g0_, n_Integer] := With[{s = splitAroundBare[a, g0]},
   If[s === None, TraceLog[a],
    TraceLog[s[[1]] Power[g0, -1]] - Sum[traceOf[If[k == 1, s[[2]], Dot @@ Table[s[[2]], {k}]]]/k, {k, n}]]];
(* (c (G0^-1 - X))^-1 = (1/c) Sum_k (G0 . X)^k . G0, the Dyson (Neumann) series, to n insertions of X *)
neumannInverse[b_, g0_, n_Integer] := With[{s = splitAroundBare[b, g0]},
   If[s === None, Power[b, -1], 1/s[[1]] Sum[Dot @@ Append[Table[s[[2]], {k}], g0], {k, 0, n}]]];
expandInverses[a_, None, _] := a;
expandInverses[a_, g0_, n_] := a /. Power[b_, -1] /; ! AtomQ[b] && Head[b] =!= Function :> neumannInverse[b, g0, n];

(* the raw terms of e, with the products truncated at order nmax in x as they are formed (orders add up and are not
   negative), so that long products of series are not expanded beyond the order wanted. Factors without Grassmann-odd
   parts commute: the shortest lists (single factors, e.g. the interaction lines) are multiplied first, so that the
   order they carry cuts the products of the long ones *)
(* Without Grassmann-odd factors (vacuum diagrams, two-point functions at zero source) the terms are generated with a
   budget: in a product, the single factors first, then every composite factor (a series) expanded only up to the order
   that the product built so far leaves; so that, e.g., the lines of a third-order diagram of Phi[G] with G a Dyson series
   give only their bare term. *)
truncatedRawTerms[e_, {x_, nmax_Integer}] /; FreeQ[e, _NonCommutativeMultiply | _GrassmannProduct] && FreeQ[e, _?grassmannOddAtomQ] :=
  With[{headQ = ! FreeQ[e, x[___]]}, integrateDeltaFunctions /@ budgetTerms[e, nmax, x, headQ]];
budgetOrder[t_, x_, headQ_] := termOrder[t, x, headQ];
budgetTerms[e_, b_, x_, headQ_] := If[b < 0, {}, budgetTermsOf[e, b, x, headQ]];
budgetTermsOf[0, _, _, _] := {};
budgetTermsOf[e_Plus, b_, x_, headQ_] := Join @@ (budgetTerms[#, b, x, headQ] & /@ List @@ e);
budgetTermsOf[e_Times, b_, x_, headQ_] := budgetProduct[List @@ e, b, x, headQ];
budgetTermsOf[Power[base_, n_Integer?Positive], b_, x_, headQ_] /; MemberQ[{Plus, Times, IntegralOver, orderBounded}, Head[base]] :=
  budgetProduct[Table[base, {n}], b, x, headQ];
budgetTermsOf[o : orderBounded[m_, __], b_, x_, headQ_] := If[m > b, {}, budgetTerms[writtenOut[o], b, x, headQ]];
budgetTermsOf[IntegralOver[f_, v_List], b_, x_, headQ_] := Module[{new = freshSymbols[Length[v]]},
   (diagramTerm[#[[1]], #[[2]], Join[new, #[[3]]]] &) /@ budgetTerms[scopedReplace[f, Thread[v -> new]], b, x, headQ]];
budgetTermsOf[e_, b_, x_, headQ_] := Select[toTermList[e /. o_orderBounded :> writtenOut[o]], budgetOrder[#, x, headQ] <= b &];
budgetProduct[factors_List, b_, x_, headQ_] := Module[{plain = Select[factors, plainFactorQ[#] && Head[#] =!= orderBounded &],
    composite = Select[factors, ! plainFactorQ[#] || Head[#] === orderBounded &], acc},
   acc = Select[{diagramTerm[Times @@ plain, {}, {}]}, budgetOrder[#, x, headQ] <= b &];
   Do[If[acc =!= {},
     acc = Select[multiplyTermLists[acc, budgetTerms[f, b - Min[budgetOrder[#, x, headQ] & /@ acc], x, headQ]], budgetOrder[#, x, headQ] <= b &]],
    {f, SortBy[composite, LeafCount]}];
   acc];
truncatedRawTerms[e_, {x_, nmax_Integer}] := With[{headQ = ! FreeQ[e, x[___]]},
   With[{keep = (termOrder[#, x, headQ] <= nmax &)},
    Block[{multiplyTermLists, multiplyTermListsInOrder},
     multiplyTermLists[l1_List, l2_List] := Select[Flatten[Table[multiplyTwoTerms[a, b], {a, l1}, {b, l2}], 1], keep];
     multiplyTermListsInOrder[lists_List] := Fold[multiplyTermLists, unitTermList,
       If[AllTrue[lists, Function[l, AllTrue[l, #[[2]] === {} &]]], SortBy[lists, Length], lists]];
     integrateDeltaFunctions /@ Select[toTermList[e], keep]]]];

(* Potthoff's F[S] written out: Phi[G] + kappa Tr(S . G), with G standing for G[S]; G[S] (LegendrePropagator) as G *)
legendreWrittenOut[e_] := e /. {LegendreTransform[phi_, g_ -> s_, opts___Rule] :>
     phi + statisticsFactors[{opts}][[2]] OperatorTrace[Dot[s, g]],
    LegendrePropagator[g_ -> _, ___][x_, y_] :> g[x, y]};

Options[ExpandTrace] = {"MaximumOrder" -> None};
ExpandTrace[e_, opts___Rule] := Catch[expandTraces[e, None, 0, {opts}], operatorFailure];
ExpandTrace[e_, g0_Symbol, n_Integer, opts___Rule] := Catch[expandTraces[e, g0, n, {opts}], operatorFailure];
(* the monomials are written out lazily (orderBounded) only for terms without Grassmann-odd factors (those of the budget of
   truncatedRawTerms) *)
expandTraces[e_, g0_, n_, opts_List] := Module[{maximum = optionValue[opts, Options[ExpandTrace], "MaximumOrder"], r, terms, lazy},
   lazy = MatchQ[maximum, {_, _Integer}] && FreeQ[e, _NonCommutativeMultiply | _GrassmannProduct] &&
     FreeQ[e, Alternatives @@ Join[$grassmannSources, $grassmannOddHeads, $gradedHeads]];
   Block[{$operatorOrder = If[lazy, {maximum[[1]], ! FreeQ[e, maximum[[1]][___]]}, None], functionLowerOrder, graded, lazyMonomial, $lazyCount = 0,
     monomialsOf, expanded},
    functionLowerOrder[f_] := functionLowerOrder[f] = functionLowerOrderOf[f];
    monomialsOf[a_] := monomialsOf[a] = collectMonomials[opMonomials[a]];
    expanded[a_] := expanded[a] = expandInverses[graded[a], g0, n];
    (* the same operator (a propagator replaced by an operator) is expanded once *)
    graded[a_] := graded[a] = If[$operatorOrder === None, a, a /. f_Function :> gradedFunctions[f]];
    r = legendreWrittenOut[e] /. {
      TraceLog[a_] :> traceLogExpansion[expandInverses[operatorNormal[graded[a]], g0, n], g0, n],
      OperatorTrace[a_] :> traceOf[expanded[a]],
      OperatorKernel[a_][x_, y_] :> kernelOf[expanded[a], x, y],
      (* a propagator replaced by an operator, e.g. G -> (G0^-1 - S)^-1 in Phi[G] *)
      (a : (_Power | _Dot | _Plus | _Times))[x_, y_] :> kernelOf[expanded[a], x, y]};
    terms = Which[maximum === None, rawTermsOf[r],
      (* the bounds were computed with the headQ of e: the budget runs if it is that of the terms, else everything is
         written out *)
      lazy && lazyHeadQ[r, maximum[[1]]] === $operatorOrder[[2]],
      integrateDeltaFunctions /@ budgetTerms[r, maximum[[2]], maximum[[1]], $operatorOrder[[2]]],
      True, truncatedRawTerms[r /. o_orderBounded :> writtenOut[o], maximum]]];
   terms = identityTraceTerm /@ (contractInverseTerm /@ terms);
   termListToExpression[canonicalTermList[terms]] /. OperatorTrace[1] DeltaFunction[x_, x_] :> OperatorTrace[1]];

(* ---- functional derivatives of traces and kernels with respect to a kernel s (a source s[x1, x2], or a propagator
   through the copy that SelfEnergy differentiates) ---- *)
(* the variation of an operator: a list of {c, L, R} with delta A = Sum c L . delta s . R *)
opVariation[a_, s_] /; FreeQ[a, s] := {};
opVariation[a_, s_] /; a === s := {{1, 1, 1}};
opVariation[a_Plus, s_] := Join @@ (opVariation[#, s] & /@ List @@ a);
opVariation[a_Times, s_] := With[{sc = splitScalar[a]}, ({sc[[1]] #[[1]], #[[2]], #[[3]]} &) /@ opVariation[sc[[2]], s]];
opVariation[a_Dot, s_] := With[{args = List @@ a}, Join @@ Table[
    ({#[[1]], dotOf[Append[Take[args, i - 1], #[[2]]]], dotOf[Prepend[Drop[args, i], #[[3]]]]} &) /@ opVariation[args[[i]], s],
    {i, Length[args]}]];
opVariation[Power[b_, -1], s_] := ({-#[[1]], dotOf[{Power[b, -1], #[[2]]}], dotOf[{#[[3]], Power[b, -1]}]} &) /@ opVariation[b, s];
opVariation[Power[b_, n_Integer?Positive], s_] := opVariation[Dot @@ Table[b, {n}], s];
opVariation[Power[b_, n_Integer], s_] /; n < -1 := opVariation[Dot @@ Table[Power[b, -1], {-n}], s];
opVariation[f_Function, s_] := (Message[FunctionalDiagrams::opfunction, userForm[f], userForm[s]]; {});
(* reached only when a depends on s (non-integer powers, unknown functions of operators) *)
opVariation[a_, s_] := (Message[FunctionalDiagrams::opvariation, userForm[a], userForm[s]]; {});

(* delta Tr(A) = Sum c Tr(R . L . delta s), delta Tr ln A = Sum c Tr(R . A^-1 . L . delta s); the derivative with
   respect to s(x1, x2) is the kernel at (x2, x1) *)
traceDerivative[OperatorTrace[a_], s_Symbol[x1_, x2_]] := kernelOf[Total[(#[[1]] dotOf[{#[[3]], #[[2]]}] &) /@ opVariation[a, s]], x2, x1];
traceDerivative[TraceLog[a_], s_Symbol[x1_, x2_]] := kernelOf[Total[(#[[1]] dotOf[{#[[3]], Power[a, -1], #[[2]]}] &) /@ opVariation[a, s]], x2, x1];
(* delta A(x, y) = Sum c (L . delta s . R)(x, y): the derivative with respect to s(x1, x2) is Sum c L(x, x1) R(x2, y) *)
kernelDerivative[OperatorKernel[a_][x_, y_], s_Symbol[x1_, x2_]] := Total[(#[[1]] kernelOf[#[[2]], x, x1] kernelOf[#[[3]], x2, y] &) /@ opVariation[a, s]];
(* F[S] = Phi[G] + kappa Tr(S G) at G = G[S], where F is stationary in G: dF/dS(x1, x2) = kappa G[S](x2, x1), with G[S]
   the inert LegendrePropagator[G -> S, {V, ...}] (V, ... the other kernels of Phi, on which G[S] depends too); with
   respect to any other kernel or source v of Phi (an interaction), dF/dv = dPhi/dv at G = G[S] (Hellmann-Feynman) *)
legendreKernels[phi_, g_] := Select[Union[Cases[phi, h_Symbol[a_, b_] /; ! ListQ[a] && ! ListQ[b] && ! NumericQ[a] :> h, {0, Infinity}]],
   # =!= g && Context[#] =!= "System`" && ! MemberQ[{IntegralOver, GrassmannProduct, DummyVariable, OperatorTrace, TraceLog}, #] &];
legendrePropagatorOf[phi_, g_ -> s_] := LegendrePropagator[g -> s, legendreKernels[phi, g]];
legendreDerivative[LegendreTransform[phi_, g_ -> s_, opts___Rule], s_[x1_, x2_]] := statisticsFactors[{opts}][[2]] legendrePropagatorOf[phi, g -> s][x2, x1];
legendreDerivative[LegendreTransform[phi_, g_ -> s_, ___Rule], source_] /; Head[source] =!= s && Head[source] =!= g :=
  FunctionalDerivative[phi, source] /. g[x_, y_] :> legendrePropagatorOf[phi, g -> s][x, y];
traceDerivative[_, _] := 0;
kernelDerivative[_, _] := 0;
legendreDerivative[_, _] := 0;

(* the traces and kernels depend on a source explicitly or through a kernel with a declared derivative: dependsOnSourceQ
   of TraceLog, OperatorTrace and OperatorKernel with the variations below. The transform depends on S and on what Phi
   depends on besides G (G is the variable of the transform) *)
dependsOnSourceQ[f_LegendreTransform, source_] := Head[source] =!= f[[2, 1]] && (! FreeQ[f[[1]], Head[source]] || ! FreeQ[f[[2, 2]], Head[source]]);
differentiateFactor[f_TraceLog, source_] := toTermList[Catch[traceDerivative[f, source], operatorFailure]];
differentiateFactor[f_OperatorTrace, source_] := toTermList[Catch[traceDerivative[f, source], operatorFailure]];
differentiateFactor[f : OperatorKernel[_][_, _], source_] := toTermList[Catch[kernelDerivative[f, source], operatorFailure]];
differentiateFactor[f_LegendreTransform, source_] := toTermList[legendreDerivative[f, source]];

(* ---- G[S], the propagator at the stationary point of F[S] ----
   Its derivatives (dG[S]/dS = (dSigma/dG)^-1, the inverse of the irreducible vertex, and dG[S]/dV) are not available:
   a message, and the term is $Failed *)
LegendrePropagator::derivative = "The derivatives of G[S] (LegendrePropagator), the propagator at the stationary point of Potthoff's F[S], are not available: dG[S]/dS is the inverse of the irreducible vertex, (\[Delta]\[CapitalSigma]/\[Delta]G)^-1. The terms that need one are $Failed.";
LegendrePropagator[g_ -> s_][x_, y_] := LegendrePropagator[g -> s, {}][x, y];
LegendrePropagator[g_ -> s_, k_List][x_, y_] /; orderlessQ[g] && ! OrderedQ[{x, y}] := LegendrePropagator[g -> s, k][y, x];
dependsOnSourceQ[f : LegendrePropagator[g_ -> s_, k_List][_, _], source_] := MemberQ[Prepend[k, s], Head[source]] && Head[source] =!= g;
differentiateFactor[f : LegendrePropagator[_ -> _, _List][_, _], source_] := If[dependsOnSourceQ[f, source],
   Message[LegendrePropagator::derivative]; {diagramTerm[$Failed, {}, {}]}, {}];

(* the derivative with respect to a kernel that is not a source, G[x, y] (a propagator, a self-energy): every G is an
   independent variable, as in SelfEnergy (the derivative symmetrised for a symmetric G, antisymmetrised for an
   antisymmetric one); zero for an expression free of
   a known kernel G (an undeclared two-label head gives the message nosource) *)
FunctionalDerivative[e_, g_Symbol[x_, y_]] /; ! sourceAtomQ[g[x, y]] && Context[g] =!= "System`" && ! FreeQ[e, g] :=
  (checkSuperfieldTraces[e, g]; derivativeWithRespectToPropagator[e, g, {x, y}, propagatorDerivativeMode[g]]);
FunctionalDerivative[e_, g_Symbol[x_, y_]] /; ! sourceAtomQ[g[x, y]] && FreeQ[e, g] && kernelHeadQ[g] := (checkSuperfieldTraces[e, g]; 0);
(* a trace that SuperfieldComponents kept as it is (Tr ln of a Nambu matrix with anomalous blocks) depends on the
   components of its superfield heads, but not through anything the derivative can see: a message, not a silent 0 *)
FunctionalDerivative::supertrace = "`1`, kept by SuperfieldComponents, depends on `2` through the superfield `3`, but it has no component form: its derivative with respect to `2` is taken as 0. Take the derivative of the superfield functional with respect to `3` first, then the components (SuperfieldComponents with `3` -> {G, 0, 0}, at zero anomalous blocks): FunctionalDerivative[Gamma, `3`[a, b]] with a -> psi[x], b -> psi[y] is the derivative with respect to the anomalous propagator Gpsipsi(x, y).";
checkSuperfieldTraces[e_, g_Symbol] := Scan[Function[t, Scan[Function[r,
      If[! FreeQ[t, r[[1]]] && MemberQ[r[[2]], g], Message[FunctionalDerivative::supertrace, t, g, r[[1]]]]], $superfieldTraceComponents]],
   If[$superfieldTraceComponents === {}, {}, Union[Cases[e, (TraceLog | OperatorTrace)[_], {0, Infinity}]]]];

(* ---- display ---- *)
operatorDisplay[a_] := a /. {Power[h_, -1] :> Superscript[h, "-1"], Dot[x__] :> Row[{x}]};
defineDisplayForm[TraceLog[a_], Interpretation[Row[{"Tr ln(", operatorDisplay[a], ")"}], TraceLog[a]]];
defineDisplayForm[OperatorTrace[a_], Interpretation[Row[{"Tr(", operatorDisplay[a], ")"}], OperatorTrace[a]]];
defineDisplayForm[OperatorKernel[a_][x_, y_], Interpretation[Subscript[If[MatchQ[a, Power[_Symbol, -1]], operatorDisplay[a],
     Row[{"(", operatorDisplay[a], ")"}]], Row[{x, y}]], OperatorKernel[a][x, y]]];
defineDisplayForm[LegendreTransform[phi_, g_ -> s_, opts___Rule], Interpretation[Row[{"\[ScriptCapitalF][", s, "]"}],
    LegendreTransform[phi, g -> s, opts]]];
defineDisplayForm[LegendrePropagator[g_ -> s_, k_List][x_, y_], Interpretation[Subscript[Row[{g, "[", s, "]"}], Row[{x, y}]],
    LegendrePropagator[g -> s, k][x, y]]];

(* ---- LaTeX (ExpressionToLaTeX) ---- *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {TraceLog, OperatorTrace, LegendreTransform, operatorProduct, inverseOperator}];
latexOperator[a_] := latexOfExpression[a /. {Dot -> operatorProduct} /. Power[h_, -1] :> inverseOperator[h]];
latexFactorOf[inverseOperator[h_]] := If[Head[h] === Plus, "\\left(" <> latexOperator[h] <> "\\right)", latexFactor[h]] <> "^{-1}";
latexFactorOf[operatorProduct[a__]] := joinStrings[(If[Head[#] === Plus, "\\left(" <> latexOperator[#] <> "\\right)", latexFactor[#]] &) /@ {a}, "\\,"];
latexFactorOf[TraceLog[a_]] := "\\mathrm{Tr}\\ln\\left(" <> latexOperator[a] <> "\\right)";
latexFactorOf[OperatorTrace[a_]] := "\\mathrm{Tr}\\left(" <> latexOperator[a] <> "\\right)";
latexFactorOf[OperatorKernel[a_][x_, y_]] := If[MatchQ[a, Power[_, -1]], latexOperator[a], "\\left(" <> latexOperator[a] <> "\\right)"] <>
   "(" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";
latexFactorOf[LegendreTransform[_, _ -> s_, ___]] := "\\mathcal{F}[" <> latexFactor[s] <> "]";
latexFactorOf[LegendrePropagator[g_ -> s_, ___][x_, y_]] := latexFactor[g] <> "[" <> latexFactor[s] <> "](" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";

(* ========================================================================= *)
(*  The 2PI effective action Gamma[G]                                        *)
(* ========================================================================= *)

(* {zeta, kappa}: zeta = -1 for fermions, +1 for bosons; kappa = zeta, 1/2 for real bosons:
     Gamma[G] = Phi[G] + kappa Tr ln(zeta G^-1) + kappa Tr(G0^-1 G - 1)
   fermions      Phi - Tr ln(-G^-1) - Tr(G0^-1 G - 1)            (G = -<psi psibar>, Sigma(x, y) = dPhi/dG(y, x))
   real bosons   Phi + 1/2 Tr ln G^-1 + 1/2 Tr(D0^-1 G - 1)      (G = <phi phi>, Sigma = -2 dPhi/dG)
   complex bosons Phi + Tr ln G^-1 + Tr(G0^-1 G - 1)             (G = <phi phibar>, Sigma(x, y) = -dPhi/dG(y, x))
   stationary where G^-1 = G0^-1 - Sigma[G] (Dyson), with the value -log Z there. *)
statisticsFactors[opts_List, allowed_List : {"Fermion", "Boson", "ComplexBoson"}] := Switch[statisticsOption[opts, allowed],
   "Fermion", {-1, -1}, "Boson", {1, 1/2}, "Superfield", {-1, -1/2}, _, {1, 1}];
(* superfields (a Grassmann field with an antisymmetric propagator calG = -<T NambuField NambuField>, the Nambu field NambuField = (psi, psibar)):
     Gamma[calG] = Phi - 1/2 Tr ln(-calG^-1) - 1/2 Tr((calG0^-1 - K) calG - 1)
   (the Gaussian Grassmann integral of one field is a Pfaffian: the factor 1/2), Sigma = -2 dPhi/dcalG, stationary where
   calG^-1 = calG0^-1 - K - Sigma (Dyson in Nambu form, the Gor'kov equations in components). The two-point source K
   contains the pairing sources. Accepted by the 2PI and 1PI effective actions (bare lines) only. *)
$superfieldActionStatistics = {"Fermion", "Boson", "ComplexBoson", "Superfield"};

EffectiveAction2PI::meanrule = "\"MeanFields\" -> `1` is not a list of rules field -> head of its mean field: one real boson phi -> phiMean, one superfield NambuField -> X, or both fields of fermions and complex bosons (psi -> Psi, psibar -> Psibar).";
EffectiveAction2PI::barred = "Which of the means `1` belongs to the barred field cannot be told: give Z0 = Exp[Integral Jbar(x) G0(x, y) J(y)] (the source of the unbarred field in the first slot) or declare the barred field with the sign -1 (DeclareField[phibar, J, -1]).";
Options[EffectiveAction2PI] = {"Statistics" -> "Fermion", "MeanFields" -> {}, "Method" -> Automatic, "Source" -> None, "MeanFieldDegree" -> All};
(* "Source" -> U: a one-body source in the bare inverse propagator, so that the action always gains Integral (barred
   field) U (field): G0^-1 -> G0^-1 - U for fermions (free action -psibar G0^-1 psi; it gains Integral psibar U psi),
   G0^-1 + U for complex bosons (free action phibar G0^-1 phi; Integral phibar U phi), D0^-1 + U for real bosons (1/2
   Integral phi U phi, as for the density vertex of PerturbativeDensityVertex). Gamma depends on U only through the trace
   term, so that at the stationary point dOmega/dU(x, y) = |kappa| G(y, x): <psibar(x) psi(y)> for fermions, <phibar(x)
   phi(y)> for complex bosons, 1/2 <phi(x) phi(y)> for real bosons *)
bareInverse[g0_, opts_List] := With[{u = optionValue[opts, Options[EffectiveAction2PI], "Source"]},
   Which[u === None, Power[g0, -1], statisticsFactors[opts, $superfieldActionStatistics][[1]] === 1, Power[g0, -1] + u, True, Power[g0, -1] - u]];
(* the default "Statistics" -> "Fermion" on a symmetric or antisymmetric G (a real boson or a superfield): a message, as
   for SelfEnergy and IrreducibleVertex *)
EffectiveAction2PI::statisticsdefault = "`1` is `2`, and \"Statistics\" is not given: EffectiveAction2PI uses the fermionic trace terms, -Tr ln(-`1`^-1) - Tr(G0^-1 `1` - 1). For `3` give \"Statistics\" -> \"`4`\".";
checkActionStatisticsGiven[g_, opts_List] := If[! MemberQ[optionKey /@ opts[[All, 1]], "Statistics"],
   Which[orderlessQ[g], Message[EffectiveAction2PI::statisticsdefault, g, "symmetric", "a real boson", "Boson"],
    MemberQ[$antisymmetricHeads, g], Message[EffectiveAction2PI::statisticsdefault, g, "antisymmetric", "a superfield", "Superfield"]]];
EffectiveAction2PI[phi_, g0_Symbol -> g_Symbol, opts___Rule] := Module[{zk = statisticsFactors[{opts}, $superfieldActionStatistics]},
   checkActionStatisticsGiven[g, {opts}];
   If[zk[[2]] === 1/2, checkRealBoson[g]];
   If[zk === {-1, -1/2}, checkSuperfield[g]];
   phi + zk[[2]] TraceLog[zk[[1]] Power[g, -1]] + zk[[2]] OperatorTrace[Dot[bareInverse[g0, {opts}], g] - 1]];
EffectiveAction2PI[z0_, interaction_, n_Integer, g0_Symbol -> g_Symbol, opts___Rule] := Module[
   {means = optionValue[{opts}, Options[EffectiveAction2PI], "MeanFields"], method = optionValue[{opts}, Options[EffectiveAction2PI], "Method"]},
   If[! MatchQ[means, {(_Symbol -> _Symbol) ...}], Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
   Which[
    means === {}, EffectiveAction2PI[If[n == 0, 0, LuttingerWardFunctional[z0, interaction, n, g0 -> g, "Method" -> method]], g0 -> g, opts],
    interaction === 0, With[{free = classicalFreeAction[z0, means, g0, {opts}]}, If[free === $Failed, $Failed, free + EffectiveAction2PI[0, g0 -> g, opts]]],
    True, meanFieldEffectiveAction[z0, interaction, n, g0 -> g, means, {opts}]]];

(* the free action at the mean fields: 1/2 phibar . D0^-1 . phibar (real bosons, phibar = <phi>), -Psibar . G0^-1 . Psi
   (fermions, Psi = <psi>, Psibar = <psibar>, G0 = -<psi psibar>_0), <phi>* . G0^-1 . <phi> (complex bosons, G0 = <phi phibar>_0). The means are declared as sources
   (Grassmann for fermions), so that FunctionalDerivative differentiates with respect to them. *)
(* the sources in the first slot of G0 in Z0 = Exp[Integral Jbar(x) G0(x, y) J(y)]: those of the unbarred fields *)
firstSlotSources[z0_, g0_] := Union[Flatten[Cases[z0, t_Times /; ! FreeQ[t, g0[_, _]] :> With[{first = Cases[t, g0[a_, _] :> a, {1}]},
      If[Length[first] == 1, Cases[t, h_Symbol[first[[1]]] /; MemberQ[sourceHeads[], h] :> h, Infinity], {}]], {0, Infinity}]]];
(* the mean of the unbarred field, from Z0, else from the sign -1 of the barred field (DeclareField[psibar, eta, -1]);
   None if neither tells *)
plainMean[z0_, g0_, means_List] := Module[{first = firstSlotSources[z0, g0], fromZ0, fromSign},
   fromZ0 = Select[means, Function[r, MemberQ[first, Replace[Cases[$fieldSourceMap, {r[[1]], src_, _} :> src], {{} -> None, {src_, ___} :> src}]]]];
   fromSign = Select[means, ! MemberQ[$fieldSourceMap, {#[[1]], _, -1}] &];
   Which[Length[fromZ0] == 1, fromZ0[[1]], Length[fromSign] == 1, fromSign[[1]], True, None]];
classicalFreeAction[z0_, means_List, g0_, opts_List] := Module[{zk = statisticsFactors[opts, $superfieldActionStatistics], x, y, barred, plain},
   {x, y} = freshSymbols[2];
   (* a superfield: one Grassmann-odd mean X = <NambuField>, -1/2 X . calG0^-1 . X *)
   If[zk === {-1, -1/2},
    If[Length[means] != 1, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    If[! MemberQ[sourceHeads[], means[[1, 2]]], DeclareGrassmannSource[means[[1, 2]]]];
    Return[SimplifyDiagrams[-1/2 IntegralOver[OperatorKernel[Power[g0, -1]][x, y] means[[1, 2]][x] ** means[[1, 2]][y], {x, y}]]]];
   If[zk[[2]] === 1/2,
    (* one real boson with one propagator *)
    If[Length[means] != 1, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    If[! MemberQ[sourceHeads[], means[[1, 2]]], DeclareSource[means[[1, 2]]]];
    SimplifyDiagrams[1/2 IntegralOver[OperatorKernel[Power[g0, -1]][x, y] means[[1, 2]][x] means[[1, 2]][y], {x, y}]],
    If[Length[means] != 2, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    plain = plainMean[z0, g0, means];
    If[plain === None, Message[EffectiveAction2PI::barred, means]; Return[$Failed]];
    barred = DeleteCases[means, plain];
    If[zk[[1]] === -1,
     Scan[If[! MemberQ[sourceHeads[], #], DeclareGrassmannSource[#]] &, means[[All, 2]]];
     SimplifyDiagrams[-IntegralOver[OperatorKernel[Power[g0, -1]][x, y] barred[[1, 2]][x] ** plain[[2]][y], {x, y}]],
     Scan[If[! MemberQ[sourceHeads[], #], DeclareSource[#]] &, means[[All, 2]]];
     SimplifyDiagrams[IntegralOver[OperatorKernel[Power[g0, -1]][x, y] barred[[1, 2]][x] plain[[2]][y], {x, y}]]]]];

(* ========================================================================= *)
(*  Potthoff: the universal functional F[Sigma] and the self-energy          *)
(*  functional Omega_G0[Sigma]                                              *)
(* ========================================================================= *)

(* LegendreTransform[Phi, G -> S] is inert: F[S] = Phi[G[S]] + kappa Tr(S G[S]), with G[S] the propagator at which the
   self-energy of Phi (SelfEnergy) equals S. Its derivative with respect to S(x, y) is kappa G[S](y, x), written with
   the inert LegendrePropagator; ExpandTrace writes it out as Phi[G] + kappa Tr(S . G). *)
SelfEnergyFunctional[phi_, g0_Symbol -> s_, g_Symbol, opts___Rule] := With[{zk = statisticsFactors[{opts}]},
   LegendreTransform[phi, g -> s, opts] + zk[[2]] TraceLog[zk[[1]] (Power[g0, -1] - s)]];

(* ========================================================================= *)
(*  Zero dimensions                                                          *)
(* ========================================================================= *)

(* integrals dropped, delta functions 1, the kernels replaced by their values, a factor n (the number of internal states,
   2s + 1) for every closed loop of kernels and every trace; Tr ln A = Log[a^n] for an integer n (the sign of a then does
   not matter, e.g. Tr ln(-G^-1) = Log[g^-2] for spin 1/2), n Log[a] otherwise. The kernels of operators
   (OperatorKernel[A][x, y]) are given temporary heads, so that the loops through them are counted too (as in the trace
   Integral A(x, y) B(y, x) = Tr(A . B)). "Loops" -> Automatic: the heads of every two-label kernel that is neither
   symmetric nor an interaction line (propagators, self-energies, one-body sources), the kernels of operators made of
   such heads included *)
Options[ZeroDimensionalValue] = {"LoopFactor" -> 1, "Loops" -> Automatic, "Indices" -> {}};
zeroDimensionalLoopHeadQ[h_Symbol] := Context[h] =!= "System`" && ! orderlessQ[h] && ! MemberQ[$antisymmetricHeads, h] &&
   ! interactionStyleQ[lineStyleOf[h]] && ! MemberQ[{IntegralOver, GrassmannProduct, DummyVariable, OperatorTrace, TraceLog, LegendreTransform}, h];
zeroDimensionalLoopHeadQ[_] := False;
ZeroDimensionalValue[e_, rules_List, opts___Rule] /; AnyTrue[rules, MatchQ[#, _Symbol -> _List] &] :=
  Catch[zeroDimensionalArrayValue[e, rules, {opts}], arrayFailure];
ZeroDimensionalValue[e_, rules_List, opts___Rule] := Module[{n = optionValue[{opts}, Options[ZeroDimensionalValue], "LoopFactor"],
    loops = optionValue[{opts}, Options[ZeroDimensionalValue], "Loops"], headRules, valueRules, operatorValue, e1, operators, kernelHeads},
   headRules = Cases[rules, (h_Symbol -> v_) :> (h -> v)];
   valueRules = Join[Cases[rules, (h_Symbol -> v_) :> (h[___] -> v)], Cases[rules, r : ((_[___] -> _) | (_[___] :> _))]];
   operatorValue[a_] := ((a /. Dot -> Times) /. headRules);
   e1 = legendreWrittenOut[e] /. {TraceLog[a_] :> If[IntegerQ[n], Log[operatorValue[a]^n], n Log[operatorValue[a]]],
      OperatorTrace[a_] :> n operatorValue[a]};
   operators = Union[Cases[e1, OperatorKernel[a_][_, _] :> a, {0, Infinity}]];
   kernelHeads = Table[Unique["FunctionalDiagrams`Private`kernel"], {Length[operators]}];
   e1 = e1 /. OperatorKernel[a_][x_, y_] :> Replace[a, Thread[operators -> kernelHeads]][x, y];
   valueRules = Join[valueRules, MapThread[(#1[___] -> operatorValue[#2]) &, {kernelHeads, operators}]];
   If[loops === Automatic, loops = Join[
      Select[Union[Cases[e1, h_Symbol[a_, b_] /; ! ListQ[a] && ! ListQ[b] && ! NumericQ[a] :> h, {0, Infinity}]],
       ! MemberQ[kernelHeads, #] && zeroDimensionalLoopHeadQ[#] &],
      Pick[kernelHeads, (! FreeQ[#, h_Symbol /; zeroDimensionalLoopHeadQ[h]] &) /@ operators]],
    loops = Join[loops, Pick[kernelHeads, (! FreeQ[#, h_Symbol /; MemberQ[loops, h]] &) /@ operators]]];
   Expand[Total[(If[loops === {}, 1, n^ClosedLoopCount[#, loops]] ((# //. IntegralOver[f_, _] :> f) /. DeltaFunction[__] -> 1 /. valueRules)) & /@
      DiagramTerms[e1]]]];

(* ---- array values: the labels run over internal states ----
   A kernel with a list value (an array of rank = its number of labels, e.g. the 4 x 4 Nambu matrix of one site with spin
   1/2) makes every label an index: DeltaFunction is the identity, integrals are sums (the arrays of each term contracted
   pairwise, as sparse tensors), operators are matrices (numbers times the identity), Tr ln A = Log[Det[A]], Tr A = Tr[A],
   OperatorKernel[A][x, y] the entry; "Indices" -> {label -> i, ...} gives the internal state of the free labels. *)
ZeroDimensionalValue::loopfactor = "With array values the internal states are summed explicitly: \"LoopFactor\" is ignored.";
ZeroDimensionalValue::indices = "The free labels `1` need an internal state: \"Indices\" -> {label -> i, ...}.";
ZeroDimensionalValue::operator = "The operator `1` has no matrix value: give an array for each of its kernels.";
ZeroDimensionalValue::rank = "The array of `1` has not the rank `2` of its labels.";
SetAttributes[arrayFailure, HoldFirst];
arrayFailure[message_, args___] := (Message[message, args]; Throw[$Failed, arrayFailure]);
zeroDimensionalArrayValue[e_, rules_List, opts_List] := Module[{indices = optionValue[opts, Options[ZeroDimensionalValue], "Indices"],
    arrays, scalars, dim, e1, operators, kernelHeads},
   If[optionValue[opts, Options[ZeroDimensionalValue], "LoopFactor"] =!= 1, Message[ZeroDimensionalValue::loopfactor]];
   arrays = Cases[rules, (h_Symbol -> v_List) :> (h -> v)];
   scalars = Join[Cases[rules, (h_Symbol -> v_) /; ! ListQ[v] :> (h[___] -> v)], Cases[rules, r : ((_[___] -> _) | (_[___] :> _))]];
   dim = Length[arrays[[1, 2]]];
   e1 = legendreWrittenOut[e] /. {TraceLog[a_] :> Log[Factor[Det[arrayMatrix[a, arrays, dim]]]], OperatorTrace[a_] :> Tr[arrayMatrix[a, arrays, dim]]};
   operators = Union[Cases[e1, OperatorKernel[a_][_, _] :> a, {0, Infinity}]];
   kernelHeads = Table[Unique["FunctionalDiagrams`Private`kernel"], {Length[operators]}];
   e1 = e1 /. OperatorKernel[a_][x_, y_] :> Replace[a, Thread[operators -> kernelHeads]][x, y];
   arrays = Join[arrays, MapThread[(#1 -> arrayMatrix[#2, arrays, dim]) &, {kernelHeads, operators}]];
   Expand[Total[arrayTermValue[#, arrays, scalars, indices, dim] & /@ canonicalTermsOf[e1]]]];

(* the matrix of an operator *)
arrayOperatorQ[a_, arrays_List] := ! FreeQ[a, Alternatives @@ (arrays[[All, 1]])] || ! FreeQ[a, _Dot];
arrayMatrix[a_?NumericQ, _, dim_] := a IdentityMatrix[dim];
arrayMatrix[h_Symbol, arrays_List, dim_] := Which[MemberQ[arrays[[All, 1]], h], h /. arrays,
   kernelHeadQ[h], arrayFailure[ZeroDimensionalValue::operator, h], True, h IdentityMatrix[dim]];
arrayMatrix[a_Plus, arrays_List, dim_] := Total[arrayMatrix[#, arrays, dim] & /@ (List @@ a)];
arrayMatrix[a_Times, arrays_List, dim_] := With[{ops = Select[List @@ a, arrayOperatorQ[#, arrays] &]},
   Which[ops === {}, a IdentityMatrix[dim],
    Length[ops] == 1, (Times @@ Select[List @@ a, ! arrayOperatorQ[#, arrays] &]) arrayMatrix[First[ops], arrays, dim],
    True, arrayFailure[ZeroDimensionalValue::operator, a]]];
arrayMatrix[a_Dot, arrays_List, dim_] := Dot @@ (arrayMatrix[#, arrays, dim] & /@ (List @@ a));
arrayMatrix[Power[b_, -1], arrays_List, dim_] := Inverse[arrayMatrix[b, arrays, dim]];
arrayMatrix[Power[b_, n_Integer?Positive], arrays_List, dim_] := Dot @@ Table[arrayMatrix[b, arrays, dim], {n}];
arrayMatrix[Power[b_, n_Integer?Negative], arrays_List, dim_] := Inverse[Dot @@ Table[arrayMatrix[b, arrays, dim], {-n}]];
arrayMatrix[a_, _, _] := arrayFailure[ZeroDimensionalValue::operator, a];

(* the value of a term: its arrays contracted, the free labels fixed by one-entry tensors *)
arrayTermValue[diagramTerm[c_, o_List, d_List], arrays_List, scalars_List, indices_List, dim_] := Module[{factors = expandIntegerPowers[factorList[c]],
    tensorQ, tensors, rest, free, missing},
   tensorQ = (! AtomQ[#] && (Head[#] === DeltaFunction || MemberQ[arrays[[All, 1]], Head[#]])) &;
   tensors = (If[Head[#] === DeltaFunction, sparseTensor[IdentityMatrix[dim], List @@ #, dim], arrayTensor[#, arrays, dim]] &) /@ Select[factors, tensorQ];
   rest = (Times @@ Select[factors, ! tensorQ[#] &]) /. scalars;
   free = Complement[Union[Flatten[First /@ tensors]], d];
   missing = Select[free, ! MemberQ[First /@ indices, #] &];
   If[missing =!= {}, arrayFailure[ZeroDimensionalValue::indices, missing]];
   tensors = Join[tensors, ({{#}, {{{# /. indices}, 1}}} &) /@ free];
   rest grassmannProductOrOne[o] If[tensors === {}, 1, contractTensors[tensors]]];
arrayTensor[f_, arrays_List, dim_] := With[{v = Head[f] /. arrays},
   If[ArrayDepth[v] =!= Length[f], arrayFailure[ZeroDimensionalValue::rank, Head[f], Length[f]]]; sparseTensor[v, List @@ f, dim]];
(* a sparse tensor {labels, {{indices, value}, ...}} (nonzero entries only) of distinct labels; a repeated label takes the
   diagonal *)
sparseTensor[array_, labels_List, dim_] := With[{distinct = DeleteDuplicates[labels]},
   With[{where = (First[Flatten[Position[distinct, #, {1}, Heads -> False]]] &) /@ labels},
    {distinct, Select[({#, Extract[array, #[[where]]]} &) /@ Tuples[Range[dim], Length[distinct]], #[[2]] =!= 0 &]}]];
(* two tensors contracted over their shared labels; the labels in summed are summed over *)
contractTensorPair[{la_, ea_}, {lb_, eb_}, summed_List] := Module[{shared = Intersection[la, lb], pa, pb, out, keyRules, res},
   pa = (First[Flatten[Position[la, #, {1}, Heads -> False]]] &) /@ shared;
   pb = (First[Flatten[Position[lb, #, {1}, Heads -> False]]] &) /@ shared;
   out = Select[DeleteDuplicates[Join[la, lb]], ! MemberQ[summed, #] &];
   keyRules = Append[(#[[1, 1, pb]] -> #) & /@ GatherBy[eb, #[[1, pb]] &], _ -> {}];
   res = Join @@ (Function[a1, ({Replace[out, Join[Thread[la -> a1[[1]]], Thread[lb -> #[[1]]]], {1}], a1[[2]] #[[2]]} &) /@
        Replace[a1[[1, pa]], keyRules]] /@ ea);
   {out, Select[({#[[1, 1]], Expand[Total[#[[All, 2]]]]} &) /@ GatherBy[res, First], #[[2]] =!= 0 &]}];
(* all tensors contracted, the pair that leaves the fewest open labels first; every label is summed *)
contractTensors[tensors_List] := Module[{ts = tensors, count, pairs, best, summed},
   While[Length[ts] > 1,
    count[l_] := Count[Flatten[ts[[All, 1]]], l];
    pairs = Flatten[Table[{i, j}, {i, Length[ts]}, {j, i + 1, Length[ts]}], 1];
    best = First[SortBy[pairs, With[{l = Union[ts[[#[[1]], 1]], ts[[#[[2]], 1]]]},
        Length[Select[l, Function[x, count[x] > Count[Join[ts[[#[[1]], 1]], ts[[#[[2]], 1]]], x]]]]] &]];
    summed = Select[Union[ts[[best[[1]], 1]], ts[[best[[2]], 1]]], count[#] == Count[Join[ts[[best[[1]], 1]], ts[[best[[2]], 1]]], #] &];
    ts = Append[Delete[ts, {{best[[1]]}, {best[[2]]}}], contractTensorPair[ts[[best[[1]]]], ts[[best[[2]]]], summed]]];
   Expand[Total[(#[[2]] &) /@ ts[[1, 2]]]]];

(* ========================================================================= *)
(*  The field dependence: Gamma[phibar, G] of Cornwall, Jackiw and Tomboulis *)
(*  (phibar = <phi>), Grassmann-odd means, the 1PI effective action          *)
(*  Gamma[phibar]                                                            *)
(* ========================================================================= *)

(* ---- the interaction at the shifted field ----
   S_int[M + phi] by degree in the fluctuation phi: the list {S_int[M], linear, quadratic, cubic, ...}. means: {field ->
   head of its mean}; for fermions {psi -> Psi, psibar -> Psibar}. *)
EffectiveAction2PI::shift = "The interaction at the shifted field could not be split by degree in the fluctuations.";
shiftedInteractionParts[interaction_, means_List] := Module[{t, shifted, degree},
   shifted = SimplifyDiagrams[interaction /. (With[{f = #[[1]], m = #[[2]]}, f[a___] :> m[a] + t f[a]] & /@ means)];
   If[! FreeQ[Cases[shifted, _IntegralOver, Infinity], t], Message[EffectiveAction2PI::shift]; Return[$Failed]];
   degree = Exponent[shifted, t];
   Table[Coefficient[shifted, t, k], {k, 0, Max[degree, 2]}]];

(* ---- vacuum diagrams of vertices that carry Grassmann-odd means ----
   The Wick contraction of the package pairs fields only. A vertex with odd means M (constants for the contraction) is
   written s M F, its means in front of its fields F (s the sign of that reordering), with commuting proxies of the means
   in its coefficient, so that the symmetries of the vertex respect them; the fields are paired as usual, and the product
   v1 ... vk = s1 ... sk (-1)^(sum_{i<j} |F_i| |M_j|) M1 ... Mk F1 ... Fk gives the ordered odd factors of each term. For
   commuting means (bosons) nothing changes: they stay in the coefficient. *)
oddMeanSplit[diagramTerm[c_, o_List, d_], oddHeads_List] := Module[{isMean = (MemberQ[oddHeads, Head[#]] &) /@ o, means, fields, sign},
   means = Pick[o, isMean]; fields = Pick[o, isMean, False];
   sign = (-1)^Total[Table[If[isMean[[i]], Count[Take[isMean, i - 1], False], 0], {i, Length[o]}]];
   {sign c (Times @@ ((meanProxy[Head[#]] @@ #) & /@ means)), fields, d, means}];
meanVertex[term_diagramTerm, oddHeads_List] := With[{s = oddMeanSplit[term, oddHeads]},
   Append[splitInteractionTerm[diagramTerm[s[[1]], s[[2]], s[[3]]]], s[[4]]]];
(* keep: a selection of the products of vertices by their coefficients (with the proxies of the means), e.g. a given
   number of means; all by default *)
meanConnectedVacuum[z0_, interaction_, orders_List, oddHeads_List, keep_ : (True &)] :=
  termListToExpression[canonicalTermList[meanConnectedVacuumTerms[z0, interaction, orders, oddHeads, keep]]];
(* the n-particle irreducible ones (n = 1, 2) with lines of the head line, selected before the canonical form *)
meanIrreducibleVacuum[z0_, interaction_, orders_List, oddHeads_List, n_Integer, line_, keep_ : (True &)] :=
  termListToExpression[selectCanonicalTermList[integrateDeltaFunctions /@ meanConnectedVacuumTerms[z0, interaction, orders, oddHeads, keep],
    particleIrreducibleTermQ[#, n, {}, {line}] &]];
(* the raw terms *)
meanConnectedVacuumTerms[z0_, interaction_, orders_List, oddHeads_List, keep_ : (True &)] := Module[{types = toTermList[interaction], split, terms = {}},
   If[interaction === 0, Return[{}]];
   split = meanVertex[#, oddHeads] & /@ types;
   Do[Do[Module[{counts, weight, vs, fieldParity, meanParity, sign, meansOrdered},
       counts = Table[Count[m, ty], {ty, Length[types]}];
       weight = (-1)^k/(Times @@ (Factorial /@ counts));
       vs = Table[With[{v = split[[m[[i]]]], new = freshSymbols[Length[split[[m[[i]], 3]]]]},
          {m[[i]], v[[1]] /. Thread[v[[3]] -> new], v[[2]] /. Thread[v[[3]] -> new], new, v[[4]] /. Thread[v[[3]] -> new]}], {i, Length[m]}];
       fieldParity = (Count[#, _?grassmannOddAtomQ] &) /@ vs[[All, 3]];
       meanParity = Length /@ vs[[All, 5]];
       sign = (-1)^Sum[meanParity[[j]] Total[Take[fieldParity, j - 1]], {j, Length[vs]}];
       meansOrdered = Join @@ vs[[All, 5]];
       If[TrueQ[keep[vs[[All, 2]]]], terms = Join[terms, (diagramTerm[weight sign (#[[1]] /. meanProxy[_][___] -> 1), Join[meansOrdered, #[[2]]], #[[3]]] &) /@
          pairingTermsToTermList[wickPairingSum[z0, {}, vs[[All, 1 ;; 4]], "Connected"], vs[[All, 1 ;; 4]]]]]],
      {m, Select[Tuples[Range[Length[types]], k], OrderedQ]}], {k, orders}];
   terms];

(* ---- Gamma[phibar, G] ----
   Gamma[M, G] (M = phibar = <phi>) = S[M] + kappa Tr ln(zeta G^-1) + kappa Tr(D^-1[M] G - 1) + Gamma2[M, G]
   (Cornwall, Jackiw and Tomboulis): S[M] = S0[M] + S_int[M] the classical action, D^-1[M] = S''[M] = G0^-1 + S_int''[M]
   its second derivative, the bare inverse propagator in the background (for fermions d^2 S/dPsibar(x) dPsi(y), the Psi
   derivative first: G0^-1 - Sigma_HF, the Hartree and Fock potentials of the means, the first-order self-energy with
   -Psi(x) Psibar(y) in place of G(x, y); phi^4: S_int'' = lambda M^2/2). Its part kappa Tr(S_int''[M] G) (mass below), the
   vertex of degree two in the fluctuation closed by one line G, gives the self-energy its mean-field part (stationarity:
   G^-1 = D^-1[M] - Sigma[M, G], Sigma from Gamma2); it is computed as minus the connected vacuum diagram of that vertex
   with one line G (its Gaussian average with the propagator G). Gamma2[M, G] = minus the 2PI vacuum diagrams with
   lines G and the vertices of S_int[M + phi] of degree three or more in the fluctuation (phi^4: lambda M phi^3/6 and
   lambda phi^4/24). The linear part of S_int[M + phi] does not appear: it is cancelled by the source (the one-point
   counterterm j of the self-consistent representation). At M = 0 it is Gamma[G] built on the Luttinger-Ward functional.
   Fermions: the means Psi, Psibar are Grassmann-odd (declared as Grassmann sources), and the vertices with an odd number
   of fluctuating fields carry one of them; Gamma is then Nambu's functional at zero anomalous propagator. *)
(* "MeanFieldDegree" -> {k, ...}: only the terms of these degrees in the means (the Taylor coefficients that give the vertex
   functions at zero mean field; {4} for the fully irreducible vertex), computed directly *)
meanDegreeOfTerm[diagramTerm[c_, o_List, _], heads_List] := Module[{tt},
   Exponent[c /. h_Symbol[a___] /; MemberQ[heads, h] :> tt h[a], tt] + Count[o, h_Symbol[___] /; MemberQ[heads, h]]];
selectMeanDegree[e_, _, All] := e;
selectMeanDegree[e_, heads_List, degrees_List] := termListToExpression[Select[canonicalTermsOf[e], MemberQ[degrees, meanDegreeOfTerm[#, heads]] &]];
meanFieldEffectiveAction[z0_, interaction_, n_Integer, g0_ -> g_, means_List, opts_List] := Module[
   {zk = statisticsFactors[opts, $superfieldActionStatistics], free, oddHeads, parts, mass, vertices, gamma2, degrees, keep, tt},
   free = classicalFreeAction[z0, means, g0, opts];
   If[free === $Failed, Return[$Failed]];
   oddHeads = If[zk[[1]] === -1, means[[All, 2]], {}];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   mass = -meanConnectedVacuum[z0 /. g0 -> g, parts[[3]], {1}, oddHeads];
   vertices = Total[Drop[parts, 3]];
   degrees = optionValue[opts, Options[EffectiveAction2PI], "MeanFieldDegree"];
   If[IntegerQ[degrees], degrees = {degrees}];
   keep = If[degrees === All, True &, MemberQ[degrees, Exponent[(Times @@ #) /. {h_Symbol[a___] /; MemberQ[means[[All, 2]], h] :> tt h[a],
         p_meanProxy[a___] :> tt p[a]}, tt]] &];
   gamma2 = If[n == 0 || vertices === 0, 0,
     -meanIrreducibleVacuum[z0, vertices, Range[n], oddHeads, 2, g0, keep] /. g0 -> g];
   selectMeanDegree[free + parts[[1]] + EffectiveAction2PI[0, g0 -> g, Sequence @@ opts] + mass + gamma2, means[[All, 2]], degrees]];

(* ---- the 1PI effective action Gamma[M] (EffectiveAction1PI) ----
   Gamma[M] = S[M] + 1/2 Tr ln S''[M] + Gamma1[M], Gamma1 = -(connected 1PI vacuum diagrams, with two or more loops, with lines
   D[M] and the vertices of S_int[M + phi] of degree three or more), D[M] = S''[M]^-1 = (D0^-1 + S_int''[M])^-1 the propagator in
   the background, written with the head given (D0 -> Dm). Dm depends on M: dDm(x, y)/dM(a) = -Integral Dm(x, u) S_int'''[M](u, v, a) Dm(v, y), declared with
   DeclareFunctionalDerivative, so that FunctionalDerivative differentiates through the lines and the trace-log. At M = 0,
   Dm = D0 if the interaction has no quadratic part. Real bosons only.
   With bare lines (D0 -> D0, or G0 -> G0 for fermions) the vertices of degree two stay vertices: Gamma[M] = S[M] +
   kappa Tr ln(zeta G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of S_int[M + phi] of degree two
   or more): the rings of the degree-two vertices are the one loop, the diagrams with two or more loops Gamma1 with its lines
   expanded in G0. For fermions ({psi -> Psi, psibar -> Psibar}, Grassmann-odd means) this is the only form: in a Grassmann
   background the vertices of degree two include the anomalous ones V Psi Psi psibar psibar, S'' is a Nambu matrix and the
   propagator in the background S''^-1 has anomalous parts; Gamma = S - 1/2 Tr ln(-S'') + Gamma1, as for a superfield. *)
EffectiveAction1PI::onepi = "The 1PI effective action is available for a real boson with one mean field (phi -> phiMean, lines in the background D0 -> Dm or bare D0 -> D0), for fermions with their two Grassmann-odd means ({psi -> Psi, psibar -> Psibar}, bare lines G0 -> G0) and for a superfield with its Grassmann-odd mean (NambuField -> X, \"Statistics\" -> \"Superfield\", bare lines).";
Options[EffectiveAction1PI] = {"Statistics" -> "Boson"};
EffectiveAction1PI[z0_, interaction_, n_Integer, field_Symbol -> mean_Symbol, d0_Symbol -> dm_Symbol, opts___Rule] :=
  onePIEffectiveAction[z0, interaction, n, d0 -> dm, {field -> mean}, Join[{opts}, Options[EffectiveAction1PI]]];
EffectiveAction1PI[z0_, interaction_, n_Integer, {field_Symbol -> mean_Symbol}, d0_Symbol -> dm_Symbol, opts___Rule] :=
  EffectiveAction1PI[z0, interaction, n, field -> mean, d0 -> dm, opts];
(* fermions: two fields with Grassmann sources *)
EffectiveAction1PI[z0_, interaction_, n_Integer, means : {_Symbol -> _Symbol, _Symbol -> _Symbol}, g0_Symbol -> dm_Symbol, opts___Rule] :=
  If[AllTrue[means[[All, 1]], MemberQ[$grassmannSources, fieldSourceOf[#]] &] && dm === g0,
   onePIBareLines[z0, interaction, n, g0, means, Join[{"Statistics" -> "Fermion"}, DeleteCases[{opts}, "Statistics" -> _]]],
   Message[EffectiveAction1PI::onepi]; $Failed];
fieldSourceOf[field_] := Replace[Cases[$fieldSourceMap, {field, s_, _} :> s], {{} -> None, {s_, ___} :> s}];
onePIEffectiveAction[z0_, interaction_, n_Integer, g0_ -> dm_, means_List, opts_List] := Module[
   {free, parts, mean, field, source, cubic, x, y, a, u, v, rhs, vacuum},
   (* a superfield: bare lines only *)
   If[statisticsOption[opts, $superfieldActionStatistics] === "Superfield",
    Return[If[Length[means] == 1 && dm === g0 && MemberQ[$grassmannSources, fieldSourceOf[means[[1, 1]]]],
      onePIBareLines[z0, interaction, n, g0, means, opts], Message[EffectiveAction1PI::onepi]; $Failed]]];
   If[statisticsFactors[opts][[2]] =!= 1/2 || Length[means] != 1, Message[EffectiveAction1PI::onepi]; Return[$Failed]];
   {field, mean} = {means[[1, 1]], means[[1, 2]]};
   source = fieldSourceOf[field];
   If[source === None || MemberQ[$grassmannSources, source], Message[EffectiveAction1PI::onepi]; Return[$Failed]];
   If[dm === g0, Return[onePIBareLines[z0, interaction, n, g0, means, opts]]];
   free = classicalFreeAction[z0, means, g0, opts];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   If[! orderlessQ[dm], DeclareSymmetric[dm]];
   (* S_int'''[M](u, v, a): the third derivative of the cubic part, fields replaced by their sources *)
   {x, y, a, u, v} = freshSymbols[5];
   cubic = If[Length[parts] >= 4, parts[[4]], 0] /. field[b___] :> source[b];
   rhs = SimplifyDiagrams[-IntegralOver[dm[x, u] FunctionalDerivative[cubic, source[u], source[v], source[a]] dm[v, y], {u, v}]];
   $derivativeRules = DeleteCases[$derivativeRules, r_ /; ! FreeQ[r[[1]], dm] && ! FreeQ[r[[1]], mean]];
   With[{d = dm, m = mean, xx = x, yy = y, aa = a, r = rhs},
    DeclareFunctionalDerivative[d[xx_, yy_], m[aa_], r]];
   vacuum = If[n == 0 || Total[Drop[parts, 3]] === 0, 0,
     meanIrreducibleVacuum[z0 /. g0 -> dm, Total[Drop[parts, 3]], Range[n], {}, 1, dm]];
   free + parts[[1]] + 1/2 TraceLog[Power[dm, -1]] - vacuum];
(* bare lines: S[M] + kappa Tr ln(zeta G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of degree two
   or more); the means are declared as sources (Grassmann for fermions) by classicalFreeAction *)
onePIBareLines[z0_, interaction_, n_Integer, g0_, means_List, opts_List] := Module[{zk = statisticsFactors[opts, $superfieldActionStatistics], free, parts, vertices, vacuum},
   free = classicalFreeAction[z0, means, g0, opts];
   If[free === $Failed, Return[$Failed]];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   vertices = Total[Drop[parts, 2]];
   vacuum = If[n == 0 || vertices === 0, 0, meanIrreducibleVacuum[z0, vertices, Range[n], If[zk[[1]] === -1, means[[All, 2]], {}], 1, g0]];
   free + parts[[1]] + zk[[2]] TraceLog[zk[[1]] Power[g0, -1]] - vacuum];

(* ---- derivatives through kernels that depend on a source implicitly (the background propagator) ----
   d Tr ln A = Tr(A^-1 dA), d Tr A = Tr dA, d A(x, y) = (dA)(x, y), with dA from opVariation for every head h of A whose
   derivative is declared: dA = c L dh R, and dh(u, w)/dsource from FunctionalDerivative *)
(* the implicit kernels of A: heads with a declared derivative, and functions of two labels (LabelFunction) whose body
   contains the source, d f(u, w)/dsource being the derivative of the body *)
implicitHeadsIn[a_, source_] := Join[Select[Union[Cases[{a}, h_Symbol, {0, Infinity}, Heads -> True]],
    MemberQ[$derivativePairs, {#, Head[source]}] &],
   Union[Cases[{a}, f_Function /; ! FreeQ[f, Head[source]], {0, Infinity}]]];
dependsOnSourceQ[f_TraceLog, source_] := ! FreeQ[f, Head[source]] || implicitHeadsIn[f[[1]], source] =!= {};
dependsOnSourceQ[f_OperatorTrace, source_] := ! FreeQ[f, Head[source]] || implicitHeadsIn[f[[1]], source] =!= {};
dependsOnSourceQ[f : OperatorKernel[_][__], source_] := ! FreeQ[f[[0]], Head[source]] || implicitHeadsIn[f[[0, 1]], source] =!= {};
implicitVariation[a_, source_, inner_] := Total[Function[h, Module[{u, w}, {u, w} = freshSymbols[2];
      IntegralOver[Total[(#[[1]] inner[#[[2]], #[[3]], u, w] &) /@ opVariation[a, h]] FunctionalDerivative[h[u, w], source], {u, w}]]] /@
    implicitHeadsIn[a, source]];
traceDerivative[TraceLog[a_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[dotOf[{#2, Power[a, -1], #1}], #4, #3] &];
traceDerivative[OperatorTrace[a_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[dotOf[{#2, #1}], #4, #3] &];
kernelDerivative[OperatorKernel[a_][x_, y_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[#1, x, #3] kernelOf[#2, #4, y] &];
(* a source in an operator, not in a kernel (D0^-1 + lambda/2 phibar^2): a message, and 0 *)
traceDerivative[f : (_TraceLog | _OperatorTrace), source : _[_]] /; ! FreeQ[f[[1]], Head[source]] :=
  (Message[FunctionalDiagrams::opsource, userForm[f[[1]]], Head[source], userForm[f]]; 0);
kernelDerivative[f : OperatorKernel[a_][_, _], source : _[_]] /; ! FreeQ[a, Head[source]] :=
  (Message[FunctionalDiagrams::opsource, userForm[a], Head[source], userForm[f]]; 0);
