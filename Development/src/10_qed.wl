(* ========================================================================= *)
(*  19. QED at one loop in dimensional regularization                       *)
(*  The diagrams of the package with spinor and Lorentz indices (condensed  *)
(*  notation: a label stands for a point and its indices; the vertex is a   *)
(*  local kernel V[x, y, z] = c gamma^(mu_z)_(a_x a_y)) in momentum space:  *)
(*  the momenta are routed as in section 17, the spinor indices followed     *)
(*  along the fermion lines (an open line is a product of Dirac matrices, a *)
(*  closed loop a trace), and the one-loop integrals done by Feynman         *)
(*  parameters, shift and tensor reduction in d = 4 - 2 eps.                 *)
(*  Conventions: Minkowski metric (+,-,-,-), {gamma^mu, gamma^nu} = 2 g^munu, *)
(*  Tr 1 = 4, no gamma5 (GammaMatrix[5] is refused); abstract Lorentz        *)
(*  indices (explicit components are refused), a repeated index contracted  *)
(*  (one upper, one lower); momenta are linear combinations of momentum     *)
(*  names (symbols, p[1], LoopMomentum[i]) with coefficients that are       *)
(*  numbers or polynomials in the Feynman parameters;                      *)
(*  FeynmanDenominator[q, m] = 1/(q^2 - m^2 + i0), so that the Feynman-      *)
(*  parameter integrals have Delta - i0 (their N takes it); every loop is    *)
(*  mu^(2 eps) (e^gammaE/(4 pi))^eps Int d^dk/(2 pi)^d, the measure of       *)
(*  section 18, so that minimal subtraction is the MS-bar scheme.            *)
(*  The algebra is done with symbols in place of compound momenta and of the *)
(*  Feynman parameters (Expand and Coefficient work on plain symbols).       *)
(* ========================================================================= *)

DiracSimplify::momentum = "`1` is not a linear combination of momenta with coefficients that are numbers or polynomials in FeynmanParameter[i] (a momentum is a symbol or an object such as p[1] or LoopMomentum[i]): the slashed momenta, four-vectors and scalar products that contain it are left as they are.";
GammaMatrix::gamma5 = "GammaMatrix[5] is not \[Gamma]5: \[Gamma]5 is not implemented (its traces and contractions would be those of an ordinary index called 5); the expression is not evaluated.";
DiracSimplify::component = "`1` has an explicit component as a Lorentz index: the indices are abstract (d-dimensional, contracted when repeated), components are not supported; the expression is not evaluated.";
DiracSimplify::product = "`1` is a commutative product (Times or Power) of Dirac matrices: a product of matrices is DiracProduct[a, b, ...]; the expression is not evaluated.";
DiracSimplify::index = "The Lorentz index `1` appears more than twice in the term `2`; the expression is not evaluated.";
(* an integrand that is 0 (e.g. a trace of an odd number of gamma matrices taken inside the integral) *)
MinkowskiLoopIntegral[0, _List] := 0;
SetAttributes[LorentzMetric, Orderless];
SetAttributes[ScalarProduct, Orderless];

(* ---- momenta: linear combinations of momentum names with coefficients that are numbers or polynomials in the Feynman
   parameters ---- *)
(* the symbols that are coefficients (the Feynman parameters, as symbols, during IntegrateOneLoop) *)
$coefficientSymbols = {};
nonMomentumHeads = GammaMatrix | SlashedMomentum | DiracProduct | DiracTrace | SpinorChain | LorentzMetric | FourVector |
   ScalarProduct | FeynmanDenominator | MinkowskiLoopIntegral | ParameterIntegral | SimplexIntegral | FeynmanParameterIntegral |
   FeynmanParameter | List | Rule | RuleDelayed;
momentumNameQ[s_Symbol] := ! NumericQ[s] && s =!= SpacetimeDimension && ! MemberQ[$coefficientSymbols, s];
momentumNameQ[s_] := ! AtomQ[s] && ! NumericQ[s] && Head[Head[s]] === Symbol && ! MatchQ[Head[s], nonMomentumHeads] &&
   ! MemberQ[Attributes[Evaluate[Head[s]]], NumericFunction];
coefficientQ[c_] := NumericQ[c /. FeynmanParameter[_] -> 1/3 /. Thread[$coefficientSymbols -> 1/3]];
(* a term of a momentum: {coefficient, name}, or $Failed *)
momentumTerm[t_] := Which[
   momentumNameQ[t], {1, t},
   Head[t] === Times, With[{names = Select[List @@ t, momentumNameQ]},
    If[Length[names] == 1 && coefficientQ[t/names[[1]]], {t/names[[1]], names[[1]]}, $Failed]],
   True, $Failed];
(* the terms {coefficient, name} of a linear combination, or $Failed *)
momentumTerms[q_] := With[{x = Expand[q]}, If[x === 0, {},
    With[{ts = Table[momentumTerm[t], {t, productTermsOf[x]}]}, If[MemberQ[ts, $Failed], $Failed, ts]]]];
linearMomentumQ[q_] := ! momentumNameQ[q] && momentumTerms[q] =!= $Failed;
splittableQ[q_] := momentumTerms[q] =!= $Failed;
linearMomentumRules = {
   SlashedMomentum[q_] /; linearMomentumQ[q] :> Total[Table[t[[1]] SlashedMomentum[t[[2]]], {t, momentumTerms[q]}]],
   FourVector[q_, mu_] /; linearMomentumQ[q] :> Total[Table[t[[1]] FourVector[t[[2]], mu], {t, momentumTerms[q]}]],
   ScalarProduct[a_, b_] /; (linearMomentumQ[a] || linearMomentumQ[b]) && splittableQ[a] && splittableQ[b] :>
    Total[Flatten[Table[s[[1]] t[[1]] ScalarProduct[s[[2]], t[[2]]], {s, momentumTerms[a]}, {t, momentumTerms[b]}]]]};
scalarProductOf[a_, b_] := Expand[ScalarProduct[a, b] //. linearMomentumRules];
(* the momenta of an expression that are not linear combinations (they are left as they are, with a message) *)
momentumProblems[e_] := Union[Select[Join[Cases[{e}, SlashedMomentum[q_] :> q, Infinity], Cases[{e}, FourVector[q_, _] :> q, Infinity],
     Flatten[Cases[{e}, ScalarProduct[a_, b_] :> {a, b}, Infinity]]], ! splittableQ[#] &]];
(* the compound momentum names (p[1], LoopMomentum[i]): replaced by symbols during the algebra *)
compoundMomentumNames[e_] := Union[Cases[Join[Cases[{e}, (SlashedMomentum | FeynmanDenominator)[q_, ___] :> q, Infinity],
     Cases[{e}, FourVector[q_, _] :> q, Infinity], Flatten[Cases[{e}, ScalarProduct[a_, b_] :> {a, b}, Infinity]],
     Cases[{e}, SpinorChain[{q2_, _}, _, {q1_, _}] :> Sequence[q2, q1], Infinity]], s_ /; ! AtomQ[s] && momentumNameQ[s], {0, Infinity}]];
symbolsFor[xs_List, name_String] := Table[Unique["FunctionalDiagrams`Private`" <> name], {Length[xs]}];

(* ---- products of Dirac matrices: linearity ---- *)
diracFreeQ[x_] := FreeQ[x, GammaMatrix | SlashedMomentum | DiracProduct | DiracTrace | SpinorChain];
(* a matrix outside DiracProduct is a product of one factor *)
wrapDiracElements[e_] := e /. {g_GammaMatrix :> DiracProduct[g], s_SlashedMomentum :> DiracProduct[s]};
(* a loop integral inside a spinor chain or a trace is taken out of it (the spinors do not depend on the loop momentum,
   and the trace is linear), so that the indices are counted term by term in its integrand; a loop momentum with the name
   of a momentum of the spinors is renamed first (it is a bound variable) *)
freshLoopMomenta[ks_List] := Table[Unique["FunctionalDiagrams`Private`k"], {Length[ks]}];
productRules = {
   SpinorChain[u_, MinkowskiLoopIntegral[f_, ks_List], v_] :> With[{new = If[FreeQ[{u, v}, Alternatives @@ ks], ks, freshLoopMomenta[ks]]},
     MinkowskiLoopIntegral[SpinorChain[u, f /. Thread[ks -> new], v], new]],
   DiracTrace[MinkowskiLoopIntegral[f_, ks_]] :> MinkowskiLoopIntegral[DiracTrace[f], ks],
   DiracProduct[a___, DiracProduct[b___], c___] :> DiracProduct[a, b, c],
   DiracProduct[a___, b_Plus, c___] :> (DiracProduct[a, #, c] & /@ b),
   DiracProduct[a___, b_?diracFreeQ, c___] :> b DiracProduct[a, c],
   DiracProduct[a___, Times[b_?diracFreeQ, r__], c___] :> b DiracProduct[a, Times[r], c],
   DiracTrace[b_Plus] :> (DiracTrace /@ b),
   DiracTrace[b_?diracFreeQ] :> 4 b,
   DiracTrace[Times[b_?diracFreeQ, r__]] :> b DiracTrace[Times[r]],
   SpinorChain[u_, b_Plus, v_] :> (SpinorChain[u, #, v] & /@ b),
   SpinorChain[u_, b_?diracFreeQ, v_] :> b SpinorChain[u, DiracProduct[], v],
   SpinorChain[u_, Times[b_?diracFreeQ, r__], v_] :> b SpinorChain[u, Times[r], v]};
(* the integrand of an unevaluated loop integral is expanded too, so that its indices are contracted *)
diracLinear[e_] := Expand[Expand[wrapDiracElements[e /. MinkowskiLoopIntegral[f_, ks_] :> MinkowskiLoopIntegral[Expand[f], ks]] //.
      linearMomentumRules] //. Join[productRules, linearMomentumRules]];

(* ---- input that is not evaluated: gamma5, components, commutative products of matrices, an index three times ---- *)
matrixValuedQ[f_] := ! FreeQ[f /. {_DiracTrace -> 1, _SpinorChain -> 1}, GammaMatrix | SlashedMomentum | DiracProduct];
componentIndexPattern = GammaMatrix[_?NumericQ] | FourVector[_, _?NumericQ] | LorentzMetric[_?NumericQ, ___] | LorentzMetric[_, _?NumericQ, ___];
(* True (after the message) when e is not evaluated *)
diracInputProblemQ[e_] := Module[{g5 = Cases[{e}, GammaMatrix[5], Infinity], comps, prods},
   comps = Cases[{e}, componentIndexPattern, Infinity];
   (* the test function only on the products that contain a matrix at all *)
   prods = Join[Cases[{e}, t_Times /; ! FreeQ[t, GammaMatrix | SlashedMomentum | DiracProduct] && Count[List @@ t, _?matrixValuedQ] > 1, Infinity],
     Cases[{e}, Power[_?matrixValuedQ, _], Infinity]];
   Which[
    g5 =!= {}, Message[GammaMatrix::gamma5]; True,
    comps =!= {}, Message[DiracSimplify::component, First[comps]]; True,
    prods =!= {}, Message[DiracSimplify::product, First[prods]]; True,
    True, False]];
termIndices[t_] := Flatten[Table[Join[Cases[{f}, GammaMatrix[a_] :> a, Infinity], Flatten[Cases[{f}, LorentzMetric[a_, b_] :> {a, b}, Infinity]],
     Cases[{f}, FourVector[_, a_] :> a, Infinity]], {f, expandIntegerPowers[factorList[t]]}]];
(* the free indices of e (those once in a term), after checking every level: in a term, the indices of the Dirac and
   tensor factors count, and a factor of another kind (a MinkowskiLoopIntegral, a Feynman-parameter integral, a power)
   counts as the free indices of its arguments, each checked in the same way; the elements of a list are independent *)
algebraFactorQ[f_] := MatchQ[f, _GammaMatrix | _SlashedMomentum | _DiracProduct | _DiracTrace | _SpinorChain | _LorentzMetric |
    _FourVector | _ScalarProduct];
indexUse[e_List] := (Scan[indexUse, e]; {});
(* an expression without indices (a Feynman-parameter integral, a Gamma function): nothing to count, nothing to expand *)
indexUse[e_] /; FreeQ[e, GammaMatrix | LorentzMetric | FourVector] := {};
indexUse[e_] := Union @@ Table[With[{fs = expandIntegerPowers[factorList[t]]},
     With[{all = Join[termIndices[Times @@ Select[fs, algebraFactorQ]],
         Flatten[Table[indexUse /@ (List @@ f), {f, Select[fs, ! AtomQ[#] && ! algebraFactorQ[#] &]}]]]},
      If[MemberQ[Tally[all][[All, 2]], n_ /; n > 2],
       Message[DiracSimplify::index, Select[Tally[all], #[[2]] > 2 &][[1, 1]], t]; Throw[True, overused]];
      Cases[Tally[all], {a_, 1} :> a]]], {t, productTermsOf[Expand[e]]}];
(* True (after the message) when an index appears more than twice in a term, at some level *)
overusedIndexQ[e_] := Catch[indexUse[e]; False, overused];

(* ---- contractions ---- *)
(* gamma^a X gamma_a for X = x1 ... xn, as {coefficient, product} pairs: C(X a) = 2 a X - C(X) a, C() = d *)
gammaContraction[{}] := {{SpacetimeDimension, {}}};
gammaContraction[xs_List] := Join[{{2, Prepend[Most[xs], Last[xs]]}},
   Table[{-c[[1]], Append[c[[2]], Last[xs]]}, {c, gammaContraction[Most[xs]]}]];
elementDot[GammaMatrix[a_], GammaMatrix[b_]] := LorentzMetric[a, b];
elementDot[GammaMatrix[a_], SlashedMomentum[p_]] := FourVector[p, a];
elementDot[SlashedMomentum[p_], GammaMatrix[a_]] := FourVector[p, a];
elementDot[SlashedMomentum[p_], SlashedMomentum[q_]] := ScalarProduct[p, q];
contractionRules = {
   LorentzMetric[a_, a_] :> SpacetimeDimension,
   Power[LorentzMetric[a_, b_], 2] :> SpacetimeDimension,
   LorentzMetric[a_, b_] LorentzMetric[b_, c_] :> LorentzMetric[a, c],
   LorentzMetric[a_, b_] FourVector[p_, b_] :> FourVector[p, a],
   FourVector[p_, a_] FourVector[q_, a_] :> ScalarProduct[p, q],
   Power[FourVector[p_, a_], 2] :> ScalarProduct[p, p],
   LorentzMetric[a_, b_] (h : DiracProduct | DiracTrace | SpinorChain)[x___] /; ! FreeQ[{x}, GammaMatrix[b]] :> (h[x] /. GammaMatrix[b] -> GammaMatrix[a]),
   FourVector[p_, a_] (h : DiracProduct | DiracTrace | SpinorChain)[x___] /; ! FreeQ[{x}, GammaMatrix[a]] :> (h[x] /. GammaMatrix[a] -> SlashedMomentum[p]),
   DiracProduct[x___, GammaMatrix[a_], y___, GammaMatrix[a_], z___] :>
    Total[Table[c[[1]] DiracProduct[x, Sequence @@ c[[2]], z], {c, gammaContraction[{y}]}]],
   DiracProduct[x___, SlashedMomentum[p_], SlashedMomentum[p_], z___] :> ScalarProduct[p, p] DiracProduct[x, z]};

(* ---- traces: Tr(a1 ... an) = Sum_k (-1)^k (a1.ak) Tr(a2 ... (no ak) ... an), Tr 1 = 4; memoized in traceTable, which
   DiracSimplify makes local (Block) ---- *)
diracTraceOf[xs_List] := With[{v = traceTable[xs]}, If[Head[v] === traceTable, traceTable[xs] = diracTraceValue[xs], v]];
diracTraceValue[xs_List] := Which[xs === {}, 4, OddQ[Length[xs]], 0, True,
   Sum[(-1)^k elementDot[xs[[1]], xs[[k]]] diracTraceOf[Delete[Rest[xs], k - 1]], {k, 2, Length[xs]}]];
traceRules = {DiracTrace[DiracProduct[xs___]] :> diracTraceOf[{xs}]};

(* ---- on-shell spinor chains ubar(p2) X u(p1): pslash1 moved to the right (pslash1 u(p1) = m1 u(p1)), pslash2 to the
   left (ubar(p2) pslash2 = m2 ubar(p2)), by {a, b} = 2 a.b ---- *)
chainReduce[xs_List, u_List, v_List] := Module[{n = Length[xs], s1 = SlashedMomentum[v[[1]]], s2 = SlashedMomentum[u[[1]]], i},
   Which[
    n > 0 && Last[xs] === s1, v[[2]] chainReduce[Most[xs], u, v],
    n > 0 && First[xs] === s2, u[[2]] chainReduce[Rest[xs], u, v],
    MemberQ[xs, s1], i = Position[xs, s1, {1}][[1, 1]];
     If[xs[[i + 1]] === s1, ScalarProduct[v[[1]], v[[1]]] chainReduce[Delete[xs, {{i}, {i + 1}}], u, v],
      2 elementDot[s1, xs[[i + 1]]] chainReduce[Delete[xs, {{i}, {i + 1}}], u, v] -
       chainReduce[ReplacePart[xs, {i -> xs[[i + 1]], i + 1 -> s1}], u, v]],
    MemberQ[xs, s2], i = Position[xs, s2, {1}][[1, 1]];
     If[xs[[i - 1]] === s2, ScalarProduct[u[[1]], u[[1]]] chainReduce[Delete[xs, {{i - 1}, {i}}], u, v],
      2 elementDot[xs[[i - 1]], s2] chainReduce[Delete[xs, {{i - 1}, {i}}], u, v] -
       chainReduce[ReplacePart[xs, {i - 1 -> s2, i -> xs[[i - 1]]}], u, v]],
    True, SpinorChain[u, DiracProduct @@ xs, v]]];
chainRules = {SpinorChain[u : {p2_, m2_}, DiracProduct[xs___], v : {p1_, m1_}] /;
     MemberQ[{xs}, SlashedMomentum[p1] | SlashedMomentum[p2]] :> chainReduce[{xs}, u, v]};
onShellRules[e_] := Union[Flatten[Cases[{e}, SpinorChain[{p2_, m2_}, _, {p1_, m1_}] :>
      {ScalarProduct[p1, p1] -> m1^2, ScalarProduct[p2, p2] -> m2^2}, Infinity]]];

diracStep[e_] := Module[{f = diracLinear[e]},
   f = Expand[f //. Join[contractionRules, productRules, linearMomentumRules]];
   f = Expand[f //. traceRules];
   f = Expand[f //. Join[contractionRules, productRules]];
   f = Expand[f //. chainRules];
   f /. onShellRules[f]];
DiracSimplify[e_] := Catch[Module[{names, syms, f, blobs, masks, ruleSets = {}, g},
    If[diracInputProblemQ[e], Throw[$Failed, diracFailure]];
    Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[e]];
    names = compoundMomentumNames[e]; syms = symbolsFor[names, "momentum"];
    f = diracLinear[e /. Thread[names -> syms]];
    If[overusedIndexQ[f], Throw[$Failed, diracFailure]];
    (* the Feynman-parameter integrals carry no matrix and no index: symbols during the algebra, which changes them only
       by the on-shell rules of the spinor chains, applied to them afterwards, those of each step in turn *)
    blobs = Select[Union[Cases[{f}, _ParameterIntegral | _SimplexIntegral | _FeynmanParameterIntegral, Infinity]],
      diracFreeQ[#] && FreeQ[#, LorentzMetric | FourVector] &];
    masks = symbolsFor[blobs, "integral"];
    g = Block[{traceTable}, FixedPoint[With[{s = diracStep[#]}, AppendTo[ruleSets, onShellRules[s]]; s] &, f /. Thread[blobs -> masks], 30]];
    g /. Thread[masks -> Fold[ReplaceAll, blobs, ruleSets]] /. Thread[syms -> names]], diracFailure];

(* ---- a canonical order of the factors of each product (by anticommutation), to compare products ---- *)
orderedProduct[xs_List] := Module[{n = Length[xs], i},
   i = Select[Range[n - 1], xs[[#]] === xs[[# + 1]] || ! OrderedQ[{xs[[#]], xs[[# + 1]]}] &];
   If[i === {}, DiracProduct @@ xs, i = First[i];
    If[xs[[i]] === xs[[i + 1]], elementDot[xs[[i]], xs[[i]]] orderedProduct[Delete[xs, {{i}, {i + 1}}]],
     2 elementDot[xs[[i]], xs[[i + 1]]] orderedProduct[Delete[xs, {{i}, {i + 1}}]] -
      orderedProduct[ReplacePart[xs, {i -> xs[[i + 1]], i + 1 -> xs[[i]]}]]]]];
DiracOrder[e_] := With[{s = DiracSimplify[e]}, If[s === $Failed, $Failed, DiracSimplify[s /. DiracProduct[xs___] :> orderedProduct[{xs}]]]];

(* ---- the momentum-space integrand of the diagrams of the package ---- *)
QEDIntegrand::rule = "`1` is not a rule of QEDIntegrand: use S -> {\"Fermion\", m}, Ph -> {\"Photon\", \[Xi]} or Ph -> {\"Photon\", {\[Xi], \[Lambda]}} (\[Lambda] a photon mass), V -> {\"Vertex\", c} (V[x, y, z] = c \[Gamma]^\[Mu], \[Mu] the Lorentz index of z, the spinor indices of x and y).";
QEDIntegrand::external = DiagramLoopIntegral::external;
QEDIntegrand::factor = DiagramLoopIntegral::factor;
QEDIntegrand::grassmann = DiagramLoopIntegral::grassmann;
QEDIntegrand::conservation = DiagramLoopIntegral::conservation;
QEDIntegrand::loops = "The diagram has more than one loop (`2`): only one-loop diagrams are evaluated.";
QEDIntegrand::spinor = "The spinor indices of `1` do not form fermion lines (each label once as a row, once as a column), or there is more than one open fermion line.";
(* the rules are checked as those of DiagramLoopIntegral (checkRuleKinds); the photon parameters are xi or {xi, lambda} *)
photonParameters[par_] := If[ListQ[par], par, {par, 0}];
qedRulesQ[rules_List] := checkRuleKinds[rules, {"Fermion", "Photon", "Vertex"}, QEDIntegrand] && With[
    {bad = Select[rules, #[[2, 1]] === "Photon" && ListQ[#[[2, 2]]] && Length[#[[2, 2]]] =!= 2 &]},
    Scan[Message[QEDIntegrand::rule, #] &, bad]; bad === {}];
Options[QEDIntegrand] = {"LoopMomentum" -> LoopMomentum[1], "Indices" -> {}};
QEDIntegrand[e_, externals_, rules_List, opts___Rule] := Module[{o = callOptions[QEDIntegrand, {opts}], k, indexRules, terms},
   k = optionValue[o, {}, "LoopMomentum"];
   indexRules = optionValue[o, {}, "Indices"];
   Which[
    ! qedRulesQ[rules], $Failed,
    ! externalMomentaQ[externals], Message[QEDIntegrand::external, externals]; $Failed,
    True, terms = qedTerm[#, externals, rules, k, indexRules] & /@ canonicalTermsOf[e];
     If[MemberQ[terms, $Failed], $Failed, Total[terms]]]];

(* the fermion lines of a list of spinor pieces {row label, column label, matrix}: one open line (a product) and closed
   loops (traces) *)
spinorLines[pieces_List, t_] := Module[{rows = Table[p[[1]], {p, pieces}], cols = Table[p[[2]], {p, pieces}], left = pieces,
    starts, chain, result = 1, open = 0, follow},
   If[Length[Union[rows]] < Length[rows] || Length[Union[cols]] < Length[cols], Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
   follow[c_] := Module[{ch = c, nx},
     While[(nx = Select[left, #[[1]] === Last[ch][[2]] &]) =!= {}, AppendTo[ch, First[nx]]; left = DeleteCases[left, First[nx]]]; ch];
   starts = Select[pieces, ! MemberQ[cols, #[[1]]] &];
   Do[left = DeleteCases[left, s]; chain = follow[{s}]; open++;
    result = result DiracProduct @@ Table[c[[3]], {c, chain}], {s, starts}];
   While[left =!= {},
    chain = {First[left]}; left = Rest[left]; chain = follow[chain];
    If[Last[chain][[2]] =!= First[chain][[1]], Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
    result = result DiracTrace[DiracProduct @@ Table[c[[3]], {c, chain}]]];
   If[open > 1, Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
   result];

qedTerm[t : diagramTerm[c_, o_, d_], externals_List, rules_List, k_, indexRules_] := Catch[Module[
    {kindOf, vertexHeads, lineHeads, c2, data, index, pieces = {}, others = 1, vertices},
    kindOf[h_] := First[Select[rules, #[[1]] === h &]][[2]];
    vertexHeads = Table[r[[1]], {r, Select[rules, #[[2, 1]] === "Vertex" &]}];
    lineHeads = Table[r[[1]], {r, Select[rules, MemberQ[{"Fermion", "Photon"}, #[[2, 1]]] &]}];
    index[x_] := Replace[x, indexRules];
    (* the local vertices identify their labels, for the routing *)
    c2 = c /. v_[a_, b_, z_] /; MemberQ[vertexHeads, v] :> DeltaFunction[a, b] DeltaFunction[a, z];
    data = routeTerm[diagramTerm[c2, o, d], momentumExternals[externals],
      Table[h -> {"Instantaneous", Function[q, 1]}, {h, lineHeads}], {k}, QEDIntegrand];
    If[data === $Failed, Throw[$Failed, qedFailure]];
    (* the lines of routeTerm, h[x, y] with its momentum flowing from y to x *)
    Do[With[{x = line[[6, 2]], y = line[[6, 1]], p = line[[5, 1]], kind = kindOf[line[[3]]]},
      If[kind[[1]] === "Fermion",
       AppendTo[pieces, {x, y, I (SlashedMomentum[p] + kind[[2]])}]; others = others FeynmanDenominator[p, kind[[2]]],
       With[{xi = photonParameters[kind[[2]]][[1]], lambda = photonParameters[kind[[2]]][[2]], mu = index[x], nu = index[y]},
        others = others (-I) FeynmanDenominator[p, lambda] (LorentzMetric[mu, nu] -
            (1 - xi) FourVector[p, mu] FourVector[p, nu] FeynmanDenominator[p, lambda])]]], {line, routeData["Lines", data]}];
    vertices = Select[expandIntegerPowers[factorList[c]], ! AtomQ[#] && MemberQ[vertexHeads, Head[#]] && Length[#] == 3 &];
    Do[AppendTo[pieces, {v[[1]], v[[2]], kindOf[Head[v]][[2]] GammaMatrix[index[v[[3]]]]}], {v, vertices}];
    With[{f = spinorLines[pieces, termToExpression[t]] others, coefficient = routeData["Coefficient", data]},
     If[routeData["LoopMomenta", data] === {}, coefficient f, coefficient MinkowskiLoopIntegral[f, {k}]]]], qedFailure];

(* ---- one-loop integrals: Feynman parameters, shift, tensor reduction, d-dimensional master formula ---- *)
IntegrateOneLoop::loops = "`1` has more than one loop momentum: only one-loop integrals are evaluated.";
IntegrateOneLoop::momentum = "The denominator momentum `1` is not +-`2` plus external momenta.";
IntegrateOneLoop::numerator = "The factor `1` of a numerator depends on the loop momentum `2` in a way that is not reduced (only slashed momenta, four-vectors and scalar products are, also in Dirac products, traces and spinor chains): the integral is left unevaluated.";
IntegrateOneLoop::power = "The power `1` of a denominator that depends on the loop momentum is not an integer: the integral is left unevaluated.";
IntegrateOneLoop[e_, eps_Symbol] := e /. MinkowskiLoopIntegral[f_, ks_List] :>
    If[Length[ks] == 1, With[{r = oneLoopIntegral[f, ks[[1]], eps]}, If[r === $Failed, MinkowskiLoopIntegral[f, ks], r]],
     Message[IntegrateOneLoop::loops, MinkowskiLoopIntegral[f, ks]]; MinkowskiLoopIntegral[f, ks]];

(* every factor of a term that depends on the loop momentum l: a denominator (an integer power of it), or reduced *)
reducedFactorQ[f_, l_] := MatchQ[f, ScalarProduct[l, _] | FourVector[l, _]] ||
   (MatchQ[f, (DiracProduct | DiracTrace | SpinorChain)[___]] && FreeQ[f /. SlashedMomentum[l] -> 1, l]);
checkNumerator[t_, l_, k_, back_List] := Scan[Which[
     FreeQ[#, l] || Head[#] === FeynmanDenominator || reducedFactorQ[#, l], Null,
     Head[#] === Power && Head[#[[1]]] === FeynmanDenominator, Message[IntegrateOneLoop::power, # /. back]; Throw[$Failed, loopFailure],
     True, Message[IntegrateOneLoop::numerator, # /. back, k]; Throw[$Failed, loopFailure]] &, expandIntegerPowers[factorList[t]]];

(* the loop momentum and the compound external momenta are symbols during the algebra; a negative integer power of a
   denominator is a numerator *)
oneLoopIntegral[f_, k_, eps_] := Catch[Module[{names, syms, back, l, g, terms, groups},
    If[diracInputProblemQ[f], Throw[$Failed, loopFailure]];
    Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[f]];
    names = Union[compoundMomentumNames[f], {k}]; syms = symbolsFor[names, "momentum"]; back = Thread[syms -> names];
    l = Replace[k, Thread[names -> syms]];
    g = f /. Thread[names -> syms] /. Power[FeynmanDenominator[q_, m_], n_Integer?Negative] /; ! FreeQ[q, l] :> (ScalarProduct[q, q] - m^2)^(-n);
    terms = productTermsOf[diracLinear[g]];
    Scan[checkNumerator[#, l, k, back] &, terms];
    groups = GatherBy[Table[splitDenominators[t, l, back], {t, terms}], First];
    Total[Table[feynmanParametrized[gr[[1, 1]], Total[Table[h[[2]], {h, gr}]], l, eps], {gr, groups}]] /. back], loopFailure];

(* a term: {denominators {q, m, power} with q = k + r (sorted), numerator}; back: the symbols back to the momenta, for the
   message *)
splitDenominators[t_, k_, back_List] := Module[{dens = {}, num = 1, q, c},
   Do[With[{base = baseAndExponent[f][[1]], n = baseAndExponent[f][[2]]},
     If[Head[base] === FeynmanDenominator && ! FreeQ[base[[1]], k] && IntegerQ[n] && n > 0,
      q = Expand[base[[1]]]; c = Coefficient[q, k];
      If[c =!= 1 && c =!= -1, Message[IntegrateOneLoop::momentum, base[[1]] /. back, k /. back]; Throw[$Failed, loopFailure]];
      AppendTo[dens, {Expand[c q], base[[2]], n}],
      num = num f]], {f, factorList[t]}];
   {Sort[Table[{g[[1, 1]], g[[1, 2]], Total[Table[h[[3]], {h, g}]]}, {g, GatherBy[dens, Take[#, 2] &]}]], num}];

(* the pairings of the indices of l^a1 ... l^a2n *)
pairingSum[{}] := 1;
pairingSum[{a_, rest___}] := Total[Table[LorentzMetric[a, {rest}[[j]]] pairingSum[Delete[{rest}, j]], {j, Length[{rest}]}]];
$tensorIndexCount = 0;
newTensorIndex[] := tensorIndex[++$tensorIndexCount];
(* a term of the shifted numerator: {J, structure} with l^a1 ... l^a2n -> (l^2)^n Sym(g...)/(d (d + 2) ... (d + 2n - 2)) *)
tensorReduce[t_, l_] := Module[{idx = {}, rest = 1, l2 = 0, n},
   Do[Which[
     f === ScalarProduct[l, l], l2++,
     Head[f] === ScalarProduct && ! FreeQ[f, l], With[{a = newTensorIndex[], v = If[f[[1]] === l, f[[2]], f[[1]]]},
      AppendTo[idx, a]; rest = rest FourVector[v, a]],
     MatchQ[f, FourVector[l, _]], AppendTo[idx, f[[2]]],
     MatchQ[f, (DiracProduct | DiracTrace | SpinorChain)[___]] && FreeQ[f /. SlashedMomentum[l] -> 1, l],
      rest = rest (f /. SlashedMomentum[l] :> With[{a = newTensorIndex[]}, AppendTo[idx, a]; GammaMatrix[a]]),
     ! FreeQ[f, l], Message[IntegrateOneLoop::numerator, f, l]; Throw[$Failed, loopFailure],
     True, rest = rest f], {f, expandIntegerPowers[factorList[t]]}];
   n = Length[idx]/2;
   If[OddQ[Length[idx]], {}, {{l2 + n, rest pairingSum[idx]/Product[SpacetimeDimension + 2 j, {j, 0, n - 1}]}}]];

(* no denominator: a polynomial in k (the numerator has been checked), scaleless, 0 in dimensional regularization *)
feynmanParametrized[{}, num_, k_, eps_] := 0;
(* the Feynman parameters are symbols during the algebra, FeynmanParameter[i] in the result *)
feynmanParametrized[dens_List, num_, k_, eps_] := Catch[Module[{n = Length[dens], d = 4 - 2 eps, xs, rs, ms, as, bigA, r, delta, l,
    shifted, reduced, scalar, result},
   xs = If[n == 1, {1}, symbolsFor[Range[n], "parameter"]];
   Block[{$coefficientSymbols = Join[$coefficientSymbols, If[n == 1, {}, xs]]},
    rs = Table[Expand[dd[[1]] - k], {dd, dens}]; ms = Table[dd[[2]], {dd, dens}]; as = Table[dd[[3]], {dd, dens}];
    bigA = Total[as];
    r = Expand[Total[xs rs]];
    delta = Expand[scalarProductOf[r, r] - Sum[xs[[i]] (scalarProductOf[rs[[i]], rs[[i]]] - ms[[i]]^2), {i, n}]];
    (* a scaleless integral (a massless tadpole): 0 *)
    If[delta === 0, Throw[0, scalelessIntegral]];
    l = Unique["FunctionalDiagrams`Private`l"];
    shifted = diracLinear[num /. k -> (l - r)];
    reduced = Flatten[Table[tensorReduce[t, l], {t, productTermsOf[shifted]}], 1];
    (* the scalar factor of a term {J, structure}: the master formula for (l^2)^J *)
    scalar[j_] := I (-1)^(bigA + j) loopMeasure[eps] Gamma[Expand[j + d/2]] Gamma[Expand[bigA - j - d/2]]/
       (Gamma[Expand[d/2]] Product[Gamma[a], {a, as}]) Product[xs[[i]]^(as[[i]] - 1), {i, n}] Power[delta, Expand[d/2 + j - bigA]];
    result = groupedDiracSimplify[reduced, scalar, onShellRules[shifted]] /. SpacetimeDimension -> d;
    collectStructures[result, xs]] /. If[n == 1, {}, Thread[xs -> Table[FeynmanParameter[i], {i, n}]]]], scalelessIntegral];

(* Sum_J scalar[J] structure through DiracSimplify, for the terms {J, structure}: DiracSimplify is linear over the factors
   without matrices and indices, so the algebra runs on the structures of each J alone (the on-shell rules of the spinor
   chains act on the scalar factors). The messages are those of DiracSimplify of the whole sum: when a group fails (an
   index more than twice in a term), the whole sum goes through DiracSimplify, as it is *)
groupedDiracSimplify[reduced_List, scalar_, onShell_List] := Module[{groups = GatherBy[reduced, First], parts, whole},
   whole := Total[Table[rd[[2]] scalar[rd[[1]]], {rd, reduced}]];
   parts = Quiet[Table[DiracSimplify[Total[Table[rd[[2]], {rd, gr}]]], {gr, groups}],
     {GammaMatrix::gamma5, DiracSimplify::component, DiracSimplify::product, DiracSimplify::momentum, DiracSimplify::index}];
   If[MemberQ[parts, $Failed], DiracSimplify[whole],
    If[momentumProblems[{reduced, Table[scalar[gr[[1, 1]]], {gr, groups}]}] =!= {},
     Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[whole]]];
    Expand[Total[Table[(scalar[groups[[i, 1, 1]]] /. onShell) parts[[i]], {i, Length[groups]}]]]]];
(* Sum over the tensor and Dirac structures of structure x (Feynman-parameter integral of its scalar coefficient) *)
structureFactorQ[f_] := ! diracFreeQ[f] || ! FreeQ[f, LorentzMetric | FourVector];
collectStructures[e_, xs_List] := Module[{pairs},
   pairs = Table[With[{s = Times @@ Select[factorList[t], structureFactorQ]}, {s, t/s}], {t, productTermsOf[Expand[e]]}];
   Total[Table[g[[1, 1]] parameterIntegralOf[Total[Table[h[[2]], {h, g}]], xs], {g, GatherBy[pairs, First]}]]];
(* the bases of the powers (the Delta of the master formula) are written as expanded polynomials *)
expandedBases[f_] := f /. Power[b_, e_] /; ! IntegerQ[e] && Head[b] === Plus :> Power[Expand[b], e];
parameterIntegralOf[f_, xs_List] := Which[
   Length[xs] == 1, f,
   Length[xs] == 2, ParameterIntegral[expandedBases[Expand[f /. xs[[2]] -> 1 - xs[[1]]]], xs[[1]]],
   True, SimplexIntegral[expandedBases[f], xs]];

(* ---- the Feynman-parameter integrals ---- *)
(* d/dv of an expression linear in the integrals: the coefficients and the integrands are differentiated (D itself would
   apply the chain rule to the inert heads) *)
DifferentiateUnderIntegral[e_, v_] := Module[{ints = Union[Cases[{e}, _ParameterIntegral | _SimplexIntegral, Infinity]], syms, f},
   syms = Table[Unique["FunctionalDiagrams`Private`integral"], {Length[ints]}];
   f = e /. Thread[ints -> syms];
   (D[f, v] + Total[Table[D[f, syms[[i]]] (ints[[i]] /. (h : ParameterIntegral | SimplexIntegral)[g_, x_] :> h[D[g, v], x]), {i, Length[ints]}]]) /.
    Thread[syms -> ints]];
SetAttributes[{SimplexIntegral, ParameterIntegral}, NHoldAll];
ParameterIntegral[0, _] := 0;
SimplexIntegral[0, _] := 0;

(* N: the Feynman prescription Delta - i0 for the logarithms and the non-integer powers of the bases that depend on the
   variables (Log|b| - i pi, |b|^a e^(-i pi a) where b < 0, above a threshold), the interval split at the real roots of
   those bases (one NIntegrate for each piece); a power <= -1 of a base with a root inside the region (finite with
   Delta - i0, a distribution, but a pole that NIntegrate does not integrate): a message and Indeterminate. The simplex is
   mapped to the unit cube (no limits that depend on the other variables). *)
ParameterIntegral::threshold = "The integrand `1` has a pole inside the region of integration that N does not integrate (a power <= -1 of a base that vanishes there, above a threshold; with \[CapitalDelta] - i0 the integral is finite): it is not evaluated.";
SimplexIntegral::threshold = ParameterIntegral::threshold;
logMinusI0[b_?NumericQ] := If[Im[b] == 0 && Re[b] < 0, Log[-b] - I Pi, Log[b]];
powerMinusI0[b_?NumericQ, a_?NumericQ] := If[Im[b] == 0 && Re[b] < 0, (-b)^a Exp[-I Pi a], b^a];
i0Form[f_, vars_List] := f //. {Log[b_] /; ! FreeQ[b, Alternatives @@ vars] :> logMinusI0[b],
    Power[b_, a_] /; ! FreeQ[b, Alternatives @@ vars] && ! IntegerQ[a] :> powerMinusI0[b, a]};
(* the bases {b, exponent} of the logarithms (exponent 0) and powers of f that depend on the variables *)
branchBases[f_, vars_List] := Union[Join[Cases[{f}, Log[b_] /; ! FreeQ[b, Alternatives @@ vars] :> {b, 0}, Infinity],
    Cases[{f}, Power[b_, a_] /; ! FreeQ[b, Alternatives @@ vars] && ! (IntegerQ[a] && a > 0) && Head[b] =!= Symbol :> {b, a}, Infinity]]];
numericPolynomialQ[p_, vars_List] := PolynomialQ[p, vars] && NumericQ[p /. Thread[vars -> 1/3]];
(* the roots to the working precision of N (Solve, then N: the roots of NSolve were machine numbers, and the pieces of
   the interval then did not end at the roots beyond 16 digits) *)
realRootsInside[b_, x_, opts_List] := With[{p = Numerator[Together[b]]},
   If[numericPolynomialQ[p, {x}] && ! FreeQ[p, x],
    Union[Select[Quiet[N[x /. Solve[p == 0, x], WorkingPrecision /. opts /. WorkingPrecision -> MachinePrecision]],
       Abs[Im[#]] < 10^-10 && 0 < Re[#] < 1 &] /. c_Complex :> Re[c]], {}]];
singularRootQ[{b_, a_}, roots_List] := NumericQ[a] && a != 0 && Re[a] <= -1 && roots =!= {};
parameterNIntegrate[f_, x_, opts_List] := Module[{t = Unique["FunctionalDiagrams`Private`t"], g, bases, roots},
   g = f /. x -> t; bases = branchBases[g, {t}];
   roots = Table[realRootsInside[b[[1]], t, opts], {b, bases}];
   If[Or @@ Table[singularRootQ[bases[[i]], roots[[i]]], {i, Length[bases]}], Message[ParameterIntegral::threshold, f]; Indeterminate,
    (* one NIntegrate for each piece between the roots: with the roots as intermediate points of one NIntegrate, the
       value above machine precision is wrong beyond about 17 digits (NIntegrate::ncvb) *)
    With[{integrand = i0Form[g, {t}], points = Join[{0}, Sort[Union[Flatten[roots]]], {1}]},
     Sum[NIntegrate[integrand, Evaluate[{t, points[[i]], points[[i + 1]]}], Evaluate[Sequence @@ opts]], {i, Length[points] - 1}]]]];
(* x1 = y1, x2 = (1 - y1) y2, ..., xn = (1 - y1) ... (1 - y(n-1)); dx = Prod_i (1 - yi)^(n - 1 - i) dy *)
simplexNIntegrate[f_, xs_List, opts_List] := Module[{n = Length[xs], ys, map, jacobian, g, bases, sample},
   ys = symbolsFor[Range[n - 1], "y"];
   map = Table[xs[[i]] -> If[i < n, ys[[i]], 1] Product[1 - ys[[j]], {j, 1, i - 1}], {i, n}];
   jacobian = Product[(1 - ys[[i]])^(n - 1 - i), {i, 1, n - 2}];
   g = (f /. map) jacobian; bases = Select[branchBases[f, xs], NumericQ[#[[2]]] && #[[2]] != 0 && Re[#[[2]]] <= -1 &];
   (* a base of a power <= -1 that changes sign on the simplex (sampled on a grid) *)
   sample = Table[Union[Sign[Re[N[Flatten[Table[b[[1]] /. map /. Thread[ys -> v], {v, Tuples[Range[1, 9]/10, n - 1]}]]]]]], {b, bases}];
   If[MemberQ[sample, s_ /; MemberQ[s, -1] && MemberQ[s, 1]], Message[SimplexIntegral::threshold, f]; Indeterminate,
    (* above machine precision the product Gauss-Kronrod rule: the default multidimensional rule takes minutes there (the
       method name is Mathematica's; N to machine precision passes no method) *)
    With[{integrand = i0Form[g, ys], iterators = Table[{y, 0, 1}, {y, ys}], method = If[opts === {}, {}, {Method -> "GaussKronrodRule"}]},
     NIntegrate[integrand, Evaluate[Sequence @@ iterators], Evaluate[Sequence @@ opts], Evaluate[Sequence @@ method]]]]];
ParameterIntegral /: N[ParameterIntegral[f_, x_], precision___] := parameterNIntegrate[f, x, precisionOptions[precision]];
SimplexIntegral /: N[SimplexIntegral[f_, xs_List], precision___] := simplexNIntegrate[f, xs, precisionOptions[precision]];

(* the integration variables are symbols while an integral is reduced or expanded *)
withVariableSymbols[fn_, f_, xs_List] := With[{ys = symbolsFor[xs, "x"]}, fn[f /. Thread[xs -> ys], ys] /. Thread[ys -> xs]];
(* the bases of the non-integer powers of f that depend on the variables *)
parameterBases[f_, xs_List] := Union[Cases[{f}, Power[b_, e_] /; ! FreeQ[b, Alternatives @@ xs] && ! IntegerQ[e] :> Expand[b], Infinity]];
(* Int_0^U dy f, f a polynomial in y *)
integrateOutVariable[f_, y_, u_] := Expand[powerSumIntegral[CoefficientList[Expand[f], y], u]];
(* Int over the simplex of a polynomial (Dirichlet: Int x1^a1 ... xn^an = a1! ... an!/(a1 + ... + an + n - 1)!) *)
simplexPolynomialIntegral[poly_, xs_List] := Module[{g = Expand[poly /. Last[xs] -> 1 - Total[Most[xs]]]},
   Do[g = integrateOutVariable[g, xs[[j]], 1 - Total[Take[xs, j - 1]]], {j, Length[xs] - 1, 1, -1}];
   g];
(* a simplex integral whose non-polynomial factors depend on one variable only (after the delta function is used), and
   polynomially on the others, is done in the other variables; the rest stays *)
reducedSimplexIntegral[f_, xs_List] := Catch[Module[{n = Length[xs]},
    If[n == 2, Throw[closedParameterIntegral[expandedBases[Expand[f /. xs[[2]] -> 1 - xs[[1]]]], xs[[1]]], reduced]];
    Do[Module[{others = Delete[xs, i], free, g},
      free = Most[others];
      g = expandedBases[Expand[f /. Last[others] -> 1 - xs[[i]] - Total[free]]];
      If[FreeQ[parameterBases[g, xs], Alternatives @@ free] && PolynomialQ[g, free],
       Do[g = integrateOutVariable[g, free[[j]], 1 - xs[[i]] - Total[Take[free, j - 1]]], {j, Length[free], 1, -1}];
       Throw[closedParameterIntegral[expandedBases[g], xs[[i]]], reduced]]], {i, n}];
    SimplexIntegral[f, xs]], reduced];

(* b = c x^al (1 - x)^be, or $Failed *)
monomialForm[b_, x_] := Module[{r = Together[b], al = 0, be = 0},
   While[al < 12 && r =!= 0 && Together[r /. x -> 0] === 0, r = Cancel[r/x]; al++];
   While[be < 12 && r =!= 0 && Together[r /. x -> 1] === 0, r = Cancel[r/(1 - x)]; be++];
   If[FreeQ[r, x] && r =!= 0, {r, al, be}, $Failed]];
(* c^a for the coefficient c of a base (Delta - i0): for a negative number, (-c)^a e^(-i pi a) *)
coefficientPower[c_, a_] := If[NumericQ[c] && Im[c] == 0 && Re[c] < 0, (-c)^a Exp[-I Pi a], c^a];
(* Int_0^1 dx of a product of x^a and powers of monomials c x^al (1 - x)^be, with exponents that do not depend on x: Beta
   functions, exact in d (the analytic continuation in the exponents); an exponent at a pole of Gamma (a + 1 or b + 1 a
   number 0, -1, -2, ...: a divergence that nothing regulates) is not done *)
gammaPoleQ[z_] := With[{v = Together[z]}, NumericQ[v] && TrueQ[Im[v] == 0] && TrueQ[Re[v] <= 0] && TrueQ[Re[v] == Round[Re[v]]]];
betaIntegral[t_, x_] := Catch[Module[{coefficient = 1, a = 0, b = 0},
    Do[Which[
      FreeQ[f, x], coefficient = coefficient f,
      f === x, a = a + 1,
      (* an exponent that depends on x (E^x, x^x, 2^x): not a Beta function *)
      Head[f] === Power && ! FreeQ[f[[2]], x], Throw[$Failed, beta],
      Head[f] === Power && f[[1]] === x, a = a + f[[2]],
      Head[f] === Power && f[[1]] === 1 - x, b = b + f[[2]],
      Head[f] === Power, With[{mf = monomialForm[f[[1]], x]}, If[mf === $Failed, Throw[$Failed, beta]];
        coefficient = coefficient coefficientPower[mf[[1]], f[[2]]]; a = a + mf[[2]] f[[2]]; b = b + mf[[3]] f[[2]]],
      True, Throw[$Failed, beta]], {f, factorList[t]}];
    If[gammaPoleQ[a + 1] || gammaPoleQ[b + 1], Throw[$Failed, beta]];
    coefficient Gamma[a + 1] Gamma[b + 1]/Gamma[a + b + 2]], beta];
closedParameterIntegral[f_, x_] := Module[{open = 0, total = 0},
   Do[With[{c = betaIntegral[t, x]}, If[c === $Failed, open = open + t, total = total + c]], {t, productTermsOf[Expand[f]]}];
   total + If[open === 0, 0, ParameterIntegral[open, x]]];
closedParameterIntegral[f_, {x_}] := closedParameterIntegral[f, x];
(* the integrals with the same variables and the same factor in front (structures and the symbols that are not in the
   integrand, such as the coupling) combined into one; the numbers go into the integrand *)
integralFactorQ[f_] := MatchQ[f, _ParameterIntegral | _SimplexIntegral];
CombineParameterIntegrals[e_] := Module[{plain = 0, pairs, keyed},
   pairs = Table[With[{ints = Select[factorList[t], integralFactorQ]},
      If[Length[ints] == 1, With[{rest = t/ints[[1]]}, With[{s = Times @@ Select[factorList[rest], structureFactorQ[#] ||
             (! NumberQ[#] && FreeQ[#, FeynmanParameter] && FreeQ[ints[[1]], #]) &]},
         {{s, Head[ints[[1]]], ints[[1, 2]]}, rest/s ints[[1, 1]]}]], {None, t}]], {t, productTermsOf[Expand[e]]}];
   keyed = GatherBy[Select[pairs, #[[1]] =!= None &], First];
   Total[Table[If[p[[1]] === None, p[[2]], 0], {p, pairs}]] +
    Total[Table[With[{key = g[[1, 1]], f = Expand[Total[Table[h[[2]], {h, g}]]]}, If[f === 0, 0, key[[1]] key[[2]][f, key[[3]]]]], {g, keyed}]]];
ReduceParameterIntegrals[e_] := CombineParameterIntegrals[e /. i : (SimplexIntegral[_, _List] | ParameterIntegral[_, _]) :> reducedIntegral[i]];
(* the reduction of an integral is a function of the integral alone (its variables are FeynmanParameter[i]): remembered,
   since every expansion of a result (pole, finite part, on shell, ...) reduces the same integrals again; the table is
   cleared beyond 5000 entries *)
reducedIntegral[i_] := With[{v = reductionTable[i]}, If[Head[v] =!= reductionTable, v,
    If[Length[DownValues[reductionTable]] > 5000, Clear[reductionTable]];
    reductionTable[i] = Replace[i, {SimplexIntegral[f_, xs_List] :> withVariableSymbols[reducedSimplexIntegral, f, xs],
       ParameterIntegral[f_, x_] :> withVariableSymbols[closedParameterIntegral, f, {x}]}]]];

(* ---- integrability at eps = 0 near the faces of the region, and the subtraction of a singular face ----
   The region is the simplex of n >= 2 variables vs (a ParameterIntegral in x is the simplex of {x, 1 - x}). Near the face
   where the variables of S vanish, x_i = lam t_i (i in S), x_j = (1 - lam) u_j (j not in S), Sum t = Sum u = 1, and
   dx = lam^(|S| - 1) (1 - lam)^(|C| - 1) dlam dt du. A polynomial base b is lam^k W(lam, t, u), W(0, t, u) not 0; a term
   c Prod b^e is lam^(p + q eps) G(lam, t, u) with p = Sum k e(0), q = Sum k e'(0): integrable at eps = 0 iff |S| + p > 0.
   A singular group of terms (same p, q, q not 0) is subtracted: with G_j the Taylor coefficients of G in lam (j <= K =
   Floor[-(|S| + p)]), Int lam^(p + q eps) Sum_j lam^j G_j = Sum_j B(|S| + p + j + q eps, |C|) Int dt du G_j (exact in d),
   and the remainder (the integrand minus lam^(p + q eps) Sum_j lam^j G_j(t(x), u(x)), lam = Sum_S x, t = x_S/lam,
   u = x_C/Sum_C x) has integrable coefficients. Supported: one singular face (both endpoints in one dimension), |S| = 1 or
   |C| = 1, G_j regular on the face (its bases do not vanish at the faces of the t or u simplex). ---- *)
ExpandParameterIntegrals::ir = "The integrand of `1` is not integrable at \[Epsilon] = 0 near the face where `2` vanish (an infrared divergence), and its divergence is not of the form that is subtracted (one singular face, every factor of the singular terms that depends on the parameters a power of a polynomial, the exponents of those that vanish there linear in \[Epsilon] and regulated by \[Epsilon]): it is not expanded.";
ExpandParameterIntegrals::failed = "The integrand `1` was not expanded in `2`.";
(* the factors of a term that depend on vs: {coefficient, {{base, exponent}, ...}, all factors powers of polynomials} *)
dependentFactors[t_, vs_List] := Module[{c = 1, bs = {}, ok = True, pat = Alternatives @@ vs},
   Do[Which[
      FreeQ[f, pat], c = c f,
      Head[f] === Power && PolynomialQ[f[[1]], vs] && FreeQ[f[[2]], pat], AppendTo[bs, {f[[1]], f[[2]]}],
      PolynomialQ[f, vs], AppendTo[bs, {f, 1}],
      True, ok = False; c = c f], {f, factorList[t]}];
   {c, bs, ok}];
(* the coordinates near the face S of the simplex of vs: {rules vs -> lam t, (1 - lam) u, independent t, independent u} *)
faceCoordinates[vs_List, s_List, lam_] := Module[{c = Complement[Range[Length[vs]], s], ts, us, tl, ul},
   ts = symbolsFor[Range[Length[s] - 1], "t"]; us = symbolsFor[Range[Length[c] - 1], "u"];
   tl = Append[ts, 1 - Total[ts]]; ul = Append[us, 1 - Total[us]];
   {Join[Table[vs[[s[[i]]]] -> lam tl[[i]], {i, Length[s]}], Table[vs[[c[[j]]]] -> (1 - lam) ul[[j]], {j, Length[c]}]], ts, us, c}];
(* the order k of a base at lam = 0 and W = b/lam^k; {Infinity, 0} if b vanishes identically there *)
baseOrder[b_, rules_List, lam_] := Module[{cs = CoefficientList[Expand[b /. rules], lam], k = 1},
   While[k <= Length[cs] && Expand[cs[[k]]] === 0, k++];
   If[k > Length[cs], {Infinity, 0}, {k - 1, Sum[cs[[j]] lam^(j - k), {j, k, Length[cs]}]}]];
exponentParts[e_, eps_] := With[{e0 = e /. eps -> 0, e1 = Coefficient[Expand[e], eps]},
   {e0, If[FreeQ[Expand[e - e0 - e1 eps], eps], e1, $Failed]}];
(* the analysis of the face S: {S, lam, coordinates, data of each term {p, q, known, ok, {{W, exponent}, ...}}}; known: the
   order is determined (numeric exponents, linear in eps, of the bases that vanish there); ok: every factor is a power of a
   polynomial *)
faceAnalysis[facs_List, vs_List, eps_, s_List] := Module[{lam = Unique["FunctionalDiagrams`Private`lambda"], coords, bases, table},
   coords = faceCoordinates[vs, s, lam];
   bases = Union[Flatten[Table[Table[b[[1]], {b, fc[[2]]}], {fc, facs}]]];
   table = Table[b -> baseOrder[b, coords[[1]], lam], {b, bases}];
   {s, lam, coords, Table[Module[{p = 0, q = 0, known = True, ws = {}},
      Do[With[{bo = Replace[b[[1]], table], ep = exponentParts[b[[2]], eps]},
        If[bo[[1]] === Infinity, known = False,
         If[bo[[1]] > 0, If[NumericQ[ep[[1]]] && ep[[2]] =!= $Failed && NumericQ[ep[[2]]], p = p + bo[[1]] ep[[1]]; q = q + bo[[1]] ep[[2]], known = False]]];
        AppendTo[ws, {bo[[2]], b[[2]]}]], {b, fc[[2]]}];
      {p, q, known, fc[[3]], ws}], {fc, facs}]}];
singularTermQ[o_List, size_] := ! o[[3]] || Re[o[[1]]] + size <= 0;
(* a polynomial b in the independent coordinates ind of a simplex (the last coordinate is 1 - Total[ind]) does not vanish
   identically at any face of it *)
nonVanishingOnFacesQ[b_, ind_List, lam_] := ind === {} || With[{full = Append[ind, Unique["FunctionalDiagrams`Private`last"]]},
   AllTrue[Subsets[Range[Length[full]], {1, Length[full] - 1}], Function[f,
     baseOrder[b, Select[faceCoordinates[full, f, lam][[1]], MemberQ[ind, #[[1]]] &], lam][[1]] === 0]]];
(* G is analytic at lam = 0 on the whole face: the bases of its non-polynomial factors do not vanish at the faces of the
   t and u simplices *)
faceRegularQ[ws_List, coords_List, lam_] := With[{bases = Union[Table[Expand[w[[1]] /. lam -> 0], {w, Select[ws, ! (IntegerQ[#[[2]]] && #[[2]] > 0) &]}]]},
   AllTrue[bases, nonVanishingOnFacesQ[#, coords[[2]], lam] && nonVanishingOnFacesQ[#, coords[[3]], lam] &]];
subtractFace[g_, facs_List, vs_List, eps_, analysis_List, make_] := Module[{s, lam, coords, orders, c, size, csize, sing, groups,
    closed = 0, subtraction = 0, lamx, back, ts, us},
   {s, lam, coords, orders} = analysis; c = coords[[4]]; size = Length[s]; csize = Length[c]; ts = coords[[2]]; us = coords[[3]];
   sing = Select[Range[Length[facs]], singularTermQ[orders[[#]], size] &];
   If[! AllTrue[sing, orders[[#, 3]] && orders[[#, 4]] &] || (size > 1 && csize > 1),
    irMessage[make[g], vs[[s]]]];
   groups = GatherBy[sing, orders[[#, {1, 2}]] &];
   (* lam, t and u as functions of the variables *)
   lamx = Total[vs[[s]]];
   back = Join[Table[ts[[i]] -> vs[[s[[i]]]]/lamx, {i, Length[ts]}], Table[us[[j]] -> vs[[c[[j]]]]/Total[vs[[c]]], {j, Length[us]}]];
   Do[Module[{p = orders[[gr[[1]], 1]], q = orders[[gr[[1]], 2]], bigK, bigG, gs},
      If[TrueQ[q == 0] || ! faceRegularQ[Flatten[Table[orders[[i, 5]], {i, gr}], 1], coords, lam],
       irMessage[make[g], vs[[s]]]];
      bigK = Floor[-(size + p)];
      bigG = Total[Table[facs[[i, 1]] Product[Power[w[[1]], w[[2]]], {w, orders[[i, 5]]}], {i, gr}]];
      gs = Table[(D[bigG, {lam, j}] /. lam -> 0)/j!, {j, 0, bigK}];
      closed = closed + Sum[Gamma[size + p + j + q eps] Gamma[csize]/Gamma[size + p + j + q eps + csize] faceIntegral[gs[[j + 1]], ts, us, vs, s, c], {j, 0, bigK}];
      subtraction = subtraction + lamx^(p + q eps) Sum[lamx^j (gs[[j + 1]] /. back), {j, 0, bigK}]], {gr, groups}];
   {closed, subtraction}];
(* Int over the t (or u) simplex of a coefficient G_j: a number (one point), a ParameterIntegral, or a SimplexIntegral, in
   the variables of the face *)
faceIntegral[gj_, ts_List, us_List, vs_List, s_List, c_List] := Which[
   ts === {} && us === {}, gj,
   us === {}, faceSimplexIntegral[gj, ts, vs[[s]]],
   True, faceSimplexIntegral[gj, us, vs[[c]]]];
faceSimplexIntegral[gj_, vars_List, names_List] := If[Length[names] == 2,
   closedParameterIntegral[expandedBases[gj /. vars[[1]] -> names[[1]]], names[[1]]],
   SimplexIntegral[expandedBases[gj /. Thread[vars -> Most[names]]], names]];
(* the integral h[g, xs] ready for the expansion under the integral sign *)
$variablesBack = {};
irMessage[i_, vars_] := (Message[ExpandParameterIntegrals::ir, i /. $variablesBack, vars /. $variablesBack]; Throw[$Failed, seriesFailure]);
preparedIntegral[g_, vs_List, eps_, make_] := Module[{terms, facs, analyses, singular, closed = 0, subtraction = 0},
   terms = productTermsOf[Expand[g]];
   facs = Table[dependentFactors[t, vs], {t, terms}];
   analyses = Table[faceAnalysis[facs, vs, eps, s], {s, Subsets[Range[Length[vs]], {1, Length[vs] - 1}]}];
   singular = Select[analyses, Function[a, AnyTrue[a[[4]], singularTermQ[#, Length[a[[1]]]] &]]];
   If[singular === {}, make[g],
    If[Length[vs] > 2 && Length[singular] > 1, irMessage[make[g], vs[[singular[[1, 1]]]]]];
    Do[With[{cs = subtractFace[g, facs, vs, eps, a, make]}, closed = closed + cs[[1]]; subtraction = subtraction + cs[[2]]], {a, singular}];
    closed + make[g - subtraction]]];
(* a ParameterIntegral in x is the simplex of {x, xc}, xc = 1 - x *)
prepareIntegral[ParameterIntegral[g_, x_], eps_] := With[{y = Unique["FunctionalDiagrams`Private`x"], yc = Unique["FunctionalDiagrams`Private`xc"]},
   Block[{$variablesBack = {y -> x, yc -> 1 - x}},
    preparedIntegral[g /. x -> y, {y, yc}, eps, ParameterIntegral[# /. yc -> 1 - y, y] &] /. $variablesBack]];
prepareIntegral[SimplexIntegral[g_, xs_List], eps_] := With[{ys = symbolsFor[xs, "x"]},
   Block[{$variablesBack = Thread[ys -> xs]}, preparedIntegral[g /. Thread[xs -> ys], ys, eps, SimplexIntegral[#, ys] &] /. $variablesBack]];

(* ---- the expansion in eps under the integral sign ---- *)
(* the polynomial terms of a coefficient integrated exactly, the others inert; the rational coefficients of each product of
   logarithms and non-integer powers are put over a common denominator, so that the singular terms that cancel after a
   subtraction (1/x - 1/(x (1 - x)) + ..., ln x/x - ln x/(x (1 - x))) are not left to N *)
transcendentalFactorQ[f_, xs_List] := ! FreeQ[f, Alternatives @@ xs] && (Head[f] === Log ||
     (Head[f] === Power && (Head[f[[1]]] === Log || ! IntegerQ[f[[2]]])));
combinedRationalCoefficients[e_, xs_List] := Module[{pairs},
   pairs = Table[With[{key = Times @@ Select[factorList[t], transcendentalFactorQ[#, xs] &]}, {key, t/key}], {t, productTermsOf[e]}];
   Total[Table[g[[1, 1]] Together[Total[Table[h[[2]], {h, g}]]], {g, GatherBy[pairs, First]}]]];
inertParameterIntegral[c_, {x_}] := Module[{poly = 0, rest = 0},
   Do[If[PolynomialQ[t, x], poly = poly + t, rest = rest + t], {t, productTermsOf[Expand[c]]}];
   rest = combinedRationalCoefficients[rest, {x}];
   polynomialIntegral[poly, x] + If[rest === 0, 0, ParameterIntegral[rest, x]]];
inertParameterIntegral[c_, xs_List] := Module[{poly = 0, rest = 0},
   Do[If[PolynomialQ[t, xs], poly = poly + t, rest = rest + t], {t, productTermsOf[Expand[c]]}];
   rest = combinedRationalCoefficients[rest, xs];
   simplexPolynomialIntegral[poly, xs] + If[rest === 0, 0, SimplexIntegral[rest, xs]]];
(* the series of an integral to eps^order: the integrand expanded (09's ser, from its true lowest order), the coefficients
   integrated; the variables are plain symbols during the expansion (its zero tests evaluate the coefficients numerically) *)
integralSeries[(h : ParameterIntegral | SimplexIntegral)[g_, x_], eps_, order_] := Module[{xs = If[ListQ[x], x, {x}], ys, s},
   ys = symbolsFor[xs, "x"];
   s = Catch[sumSeries[g /. Thread[xs -> ys], eps, order], expansionFailure];
   If[! ListQ[s], Message[ExpandParameterIntegrals::failed, g, eps]; Throw[$Failed, seriesFailure]];
   Sum[eps^(s[[1]] + j) (inertParameterIntegral[Expand[s[[2, j + 1]]], ys] /. Thread[ys -> xs]), {j, 0, Length[s[[2]]] - 1}]];
(* a lower bound of the valuation of a factor; an integral: that of its integrand *)
factorValuation[f_, eps_] := With[{v = Catch[If[integralFactorQ[f], valuation[f[[1]], eps], valuation[f, eps]], expansionFailure]},
   If[IntegerQ[v], v, Message[ExpandParameterIntegrals::failed, f, eps]; Throw[$Failed, seriesFailure]]];
(* the order to which each integral is expanded: n minus the valuation of what multiplies it, in every term *)
integralOrders[r_, ints_List, eps_, n_] := Module[{orders = Table[n, {Length[ints]}]},
   Do[With[{fs = expandIntegerPowers[factorList[t]]}, With[{vs = Table[factorValuation[f, eps], {f, fs}]},
      Do[If[integralFactorQ[fs[[i]]], With[{j = Position[ints, fs[[i]], {1}][[1, 1]]}, orders[[j]] = Max[orders[[j]], n - (Total[vs] - vs[[i]])]]],
       {i, Length[fs]}]]], {t, productTermsOf[Expand[r]]}];
   orders];
ExpandParameterIntegrals[e_List, eps_Symbol, n_Integer] := Table[ExpandParameterIntegrals[x, eps, n], {x, e}];
ExpandParameterIntegrals[e_, eps_Symbol, n_Integer] := withSeriesMemo[Catch[Module[{r, ints, orders},
    r = ReduceParameterIntegrals[e /. SpacetimeDimension -> 4 - 2 eps];
    r = r /. i : (_ParameterIntegral | _SimplexIntegral) :> prepareIntegral[i, eps];
    ints = Union[Cases[{r}, _ParameterIntegral | _SimplexIntegral, Infinity]];
    orders = integralOrders[r, ints, eps, n];
    EpsilonExpand[r /. Table[ints[[i]] -> integralSeries[ints[[i]], eps, orders[[i]]], {i, Length[ints]}], eps, n]], seriesFailure]];

(* ---- form factors of a vertex between on-shell spinors of equal mass ---- *)
VertexFormFactors::rest = "The vertex has a part `1` that is not of the form gamma^mu, p1^mu, p2^mu between the spinors: no form factors are given.";
VertexFormFactors[g_, mu_, {p1_, p2_, m_}] := Module[{f = DiracSimplify[g], cg, c1, a = 0, b1 = 0, b2 = 0, rest = 0, scalarQ},
   cg = SpinorChain[{p2, m}, DiracProduct[GammaMatrix[mu]], {p1, m}]; c1 = SpinorChain[{p2, m}, DiracProduct[], {p1, m}];
   scalarQ[x_] := diracFreeQ[x] && FreeQ[x, mu];
   If[f === $Failed, $Failed,
    (* each term: a scalar coefficient times one of the structures *)
    Do[With[{fs = factorList[t]}, Which[
        MemberQ[fs, cg] && scalarQ[t/cg], a = a + t/cg,
        MemberQ[fs, c1] && MemberQ[fs, FourVector[p1, mu]] && scalarQ[t/(c1 FourVector[p1, mu])], b1 = b1 + t/(c1 FourVector[p1, mu]),
        MemberQ[fs, c1] && MemberQ[fs, FourVector[p2, mu]] && scalarQ[t/(c1 FourVector[p2, mu])], b2 = b2 + t/(c1 FourVector[p2, mu]),
        True, rest = rest + t]], {t, productTermsOf[Expand[f]]}];
    If[rest =!= 0, Message[VertexFormFactors::rest, rest]; $Failed, {a + m (b1 + b2), -m (b1 + b2), (b2 - b1)/2}]]];

(* ---- display ---- *)
defineDisplayForm[GammaMatrix[mu_], Interpretation[Superscript["\[Gamma]", mu], GammaMatrix[mu]]];
defineDisplayForm[SlashedMomentum[p_], Interpretation[Overscript[p, "/"], SlashedMomentum[p]]];
defineDisplayForm[DiracProduct[], Interpretation["\[DoubleStruckOne]", DiracProduct[]]];
defineDisplayForm[DiracProduct[a__], Interpretation[Row[{a}, " "], DiracProduct[a]]];
defineDisplayForm[DiracTrace[a_], Interpretation[Row[{"tr[", a, "]"}], DiracTrace[a]]];
defineDisplayForm[SpinorChain[{p2_, m2_}, x_, {p1_, m1_}], Interpretation[Row[{OverBar["u"], "(", p2, ") ", x, " u(", p1, ")"}], SpinorChain[{p2, m2}, x, {p1, m1}]]];
defineDisplayForm[LorentzMetric[a_, b_], Interpretation[Superscript["g", Row[{a, b}]], LorentzMetric[a, b]]];
defineDisplayForm[FourVector[p_, a_], Interpretation[Superscript[p, a], FourVector[p, a]]];
defineDisplayForm[ScalarProduct[p_, q_], Interpretation[Row[{p, "\[CenterDot]", q}], ScalarProduct[p, q]]];
defineDisplayForm[SpacetimeDimension, Interpretation["d", SpacetimeDimension]];
defineDisplayForm[FeynmanDenominator[q_, m_], Interpretation[1/HoldForm[#1^2 - #2^2] &[q, m], FeynmanDenominator[q, m]]];
defineDisplayForm[MinkowskiLoopIntegral[f_, ks_List], Interpretation[Row[{Subscript["\[Integral]", Row[ks, ","]], " ", f}], MinkowskiLoopIntegral[f, ks]]];
defineDisplayForm[ParameterIntegral[f_, x_], Interpretation[Row[{Subsuperscript["\[Integral]", 0, 1], "d", x, " ", f}], ParameterIntegral[f, x]]];
defineDisplayForm[SimplexIntegral[f_, xs_List], Interpretation[Row[{Subscript["\[Integral]", Row[{"\[Delta](1-", Row[xs, "-"], ")"}]], " ", f}], SimplexIntegral[f, xs]]];
defineDisplayForm[FeynmanParameter[i_], Interpretation[Subscript["x", i], FeynmanParameter[i]]];
