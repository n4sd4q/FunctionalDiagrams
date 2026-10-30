
(* ========================================================================= *)
(*  18. Feynman integrals in dimensional regularization                      *)
(*  The diagrams of the package, for a translation-invariant theory at zero  *)
(*  temperature in Euclidean space, as momentum integrals in d = 4 - 2 eps:  *)
(*  the momenta are routed through the lines by the routing of section 17   *)
(*  (routeTerm), the loops are done one after the other as one-loop          *)
(*  integrals in closed form (tadpoles, massless G-functions, massive        *)
(*  bubbles by Feynman parameters), and the results are expanded in eps      *)
(*  by a Laurent expansion of our own (no Series, Limit or Residue).         *)
(*  Conventions: a line is 1/(q^2 + m^2); every loop is                      *)
(*    mu^(2 eps) (e^gammaE/(4 pi))^eps Int d^dk/(2 pi)^d,                     *)
(*  so that Int_k 1/(k^2 + Delta)^n = e^(eps gammaE) mu^(2 eps)               *)
(*  Gamma(n - d/2)/(16 pi^2 Gamma(n)) Delta^(d/2 - n): no gammaE and no       *)
(*  ln 4 pi appear, and minimal subtraction is the MS-bar scheme.            *)
(* ========================================================================= *)

(* ---- the measure and the one-loop integrals in closed form ---- *)

loopMeasure[eps_] := Exp[eps EulerGamma] (RenormalizationScale^2)^eps/(16 Pi^2);
nonPositiveIntegerQ[a_] := IntegerQ[a] && a <= 0;
(* x^n, with 0^0 = 1 *)
powerOrOne[x_, 0] := 1;
powerOrOne[x_, n_] := x^n;

GFunction[a_, b_, eps_] := Gamma[2 - eps - a] Gamma[2 - eps - b] Gamma[a + b - 2 + eps]/(Gamma[a] Gamma[b] Gamma[4 - 2 eps - a - b]);

TadpoleIntegral[a_, 0, eps_] := 0;
(* a polynomial in k: 0 in dimensional regularization *)
TadpoleIntegral[a_Integer, m_, eps_] /; a <= 0 := 0;
TadpoleIntegral[a_, m_, eps_] /; m =!= 0 := loopMeasure[eps] Gamma[a - 2 + eps]/Gamma[a] (m^2)^(2 - eps - a);

(* closed forms; otherwise BubbleIntegral stays, as its Feynman-parameter integral (expanded by EpsilonExpand) *)
BubbleIntegral[{a_, b_}, p2_, {0, 0}, eps_] := If[p2 === 0 || nonPositiveIntegerQ[a] || nonPositiveIntegerQ[b], 0,
   loopMeasure[eps] GFunction[a, b, eps] p2^(2 - eps - a - b)];
(* a power that is a non-positive integer: a numerator, reduced to tadpoles *)
BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_] /; (m1 =!= 0 || m2 =!= 0) && (nonPositiveIntegerQ[a] || nonPositiveIntegerQ[b]) :=
  If[nonPositiveIntegerQ[b], numeratorBubble[-b, m2, a, m1, p2, eps], numeratorBubble[-a, m1, b, m2, p2, eps]];
BubbleIntegral[{a_, b_}, 0, {m_, m_}, eps_] /; m =!= 0 := TadpoleIntegral[a + b, m, eps];
(* a massless line at zero momentum: Int_k (k^2)^-a (k^2 + m^2)^-b = Gamma(d/2 - a) Gamma(a + b - d/2)/(Gamma(d/2) Gamma(b)) (m^2)^(d/2-a-b) *)
BubbleIntegral[{a_, b_}, 0, {0, m_}, eps_] /; m =!= 0 :=
  loopMeasure[eps] Gamma[2 - eps - a] Gamma[a + b - 2 + eps]/(Gamma[2 - eps] Gamma[b]) (m^2)^(2 - eps - a - b);
BubbleIntegral[{a_, b_}, 0, {m_, 0}, eps_] /; m =!= 0 := BubbleIntegral[{b, a}, 0, {0, m}, eps];
(* unequal masses at zero momentum, a = b = 1: partial fractions *)
BubbleIntegral[{1, 1}, 0, {m1_, m2_}, eps_] /; m1 =!= 0 && m2 =!= 0 && m1 =!= m2 :=
  (TadpoleIntegral[1, m1, eps] - TadpoleIntegral[1, m2, eps])/(m2^2 - m1^2);

(* Int_q ((q - P)^2 + mn^2)^n/(q^2 + md^2)^b for an integer n >= 0, P^2 = p2: ((q - P)^2)^s expanded, the odd powers of q.P
   dropped and (q.P)^(2j) replaced by its angular average (q^2 P^2)^j (2j - 1)!!/(d (d + 2) ... (d + 2j - 2)); then
   q^(2l) = ((q^2 + md^2) - md^2)^l gives tadpoles. With md = 0 every term is scaleless: 0 *)
numeratorBubble[n_, mn_, b_, md_, p2_, eps_] := If[md === 0, 0, Module[{d = 4 - 2 eps},
   Sum[Binomial[n, s] powerOrOne[mn^2, n - s] Binomial[s, 2 j] 4^j Product[(2 r + 1)/(d + 2 r), {r, 0, j - 1}] Binomial[s - 2 j, t]
      powerOrOne[p2, s - j - t] Sum[Binomial[j + t, i] powerOrOne[-md^2, j + t - i] TadpoleIntegral[b - i, md, eps], {i, 0, j + t}],
    {s, 0, n}, {j, 0, Floor[s/2]}, {t, 0, s - 2 j}]]];

MomentumSquared[0] = 0;
MomentumSquared[p_Symbol] := p^2;
MomentumSquared[c_?NumberQ p_Symbol] := c^2 p^2;
MomentumSquared[q_] /; ! NumberQ[q] && firstTermSign[q] < 0 := MomentumSquared[Expand[-q]];

(* ---- the momentum-space integrand of a diagram: the routing of section 17, each line a propagator ---- *)

DiagramLoopIntegral::rule = "`1` is not a line rule: use head -> {\"Scalar\", m} (the line is 1/(q^2 + m^2)) or head -> {\"Kernel\", f} (the line is f[q], q its momentum).";
DiagramLoopIntegral::external = "`1` is not a list of external momenta label -> momentum.";
DiagramLoopIntegral::loopnames = EvaluateMatsubara::loopnames;
DiagramLoopIntegral::loops = EvaluateMatsubara::loops;
DiagramLoopIntegral::factor = EvaluateMatsubara::factor;
DiagramLoopIntegral::grassmann = EvaluateMatsubara::grassmann;
DiagramLoopIntegral::conservation = "The external momenta do not add up to zero on a connected part of a diagram (`1`).";
InfraredRearrangedIntegral::rule = DiagramLoopIntegral::rule;
InfraredRearrangedIntegral::factor = DiagramLoopIntegral::factor;
InfraredRearrangedIntegral::grassmann = DiagramLoopIntegral::grassmann;
InfraredRearrangedIntegral::conservation = DiagramLoopIntegral::conservation;

$loopLineKinds = {"Scalar", "Kernel"};
routingRule[(Rule | RuleDelayed)[h_, {"Scalar", m_}]] := h -> {"Instantaneous", Function[q, ScalarPropagator[q, m]]};
routingRule[(Rule | RuleDelayed)[h_, {"Kernel", f_}]] := h -> {"Instantaneous", f};
externalMomentaQ[externals_] := ListQ[externals] && AllTrue[externals, MatchQ[#, (Rule | RuleDelayed)[_, _]] &];

loopIntegralOfRoute[data_List] := With[{c = routeData["Coefficient", data], f = routeData["Integrand", data], ks = routeData["LoopMomenta", data]},
   If[ks === {}, c f, c LoopIntegral[f, ks]]];
(* external points label -> momentum as those of routeTerm, label -> {momentum, 0} *)
momentumExternals[externals_List] := Table[x[[1]] -> {x[[2]], 0}, {x, externals}];
(* the messages of the routing are those of caller *)
termLoopIntegral[t_, externals_List, rules_List, names_, caller_] :=
  With[{data = routeTerm[t, momentumExternals[externals], routingRule /@ rules, names, caller]},
   If[data === $Failed, $Failed, loopIntegralOfRoute[data]]];

Options[DiagramLoopIntegral] = {"LoopMomenta" -> Automatic};
DiagramLoopIntegral[e_, externals_, rules_List, opts___Rule] := Module[{names, terms},
   names = optionValue[callOptions[DiagramLoopIntegral, {opts}], {}, "LoopMomenta"];
   Which[
    ! externalMomentaQ[externals], Message[DiagramLoopIntegral::external, externals]; $Failed,
    ! checkRuleKinds[rules, $loopLineKinds, DiagramLoopIntegral], $Failed,
    names =!= Automatic && ! ListQ[names], Message[DiagramLoopIntegral::loopnames, names]; $Failed,
    True, terms = termLoopIntegral[#, externals, rules, names, DiagramLoopIntegral] & /@ canonicalTermsOf[e];
     If[MemberQ[terms, $Failed], $Failed, Total[terms]]]];

(* ---- the loops one after the other ----
   A loop momentum k that appears in at most two different propagators (k + r1, m1)^a, (k + r2, m2)^b (with coefficient
   +-1) is integrated: a tadpole, or a bubble of momentum P = r2 - r1 (if there is none, the loop momenta are first changed,
   k -> c (k - r), so that the momentum c k + r of a line becomes a loop momentum). Two massless lines give G(a, b) and a new
   massless line of momentum P and power a + b - 2 + eps; a massive bubble, or a numerator (a power that is a non-positive
   integer) next to a massive line, only when P is free of the other loop momenta.
   Each term of the expanded integrand (a piece) gives {value, flags}. Flags: "Scaleless" (a scaleless integral that is
   only power divergent in the ultraviolet: 0, also for the ultraviolet counterterms of a massless theory),
   "ScalelessLog" (a scaleless integral with a logarithmic divergence: 0 because its ultraviolet and infrared poles
   cancel), "IR" (an infrared divergence: a massless line of power 2 or more at eps = 0 in a loop, a scaleless integral
   that diverges only in the infrared, or a massless line of zero momentum), "Irreducible" and {"Numerator", factor}
   (value $Failed). A piece with a power-divergent scaleless loop is 0 whatever else it contains (the loop is a factor of
   it), so such a loop is looked for first. *)
IntegrateLoops::irreducible = "The loop integral `1` is not reduced to nested one-loop integrals (at each step a loop momentum in at most two different propagators, a massive bubble or a numerator next to a massive line only with an external momentum): it is left unevaluated.";
IntegrateLoops::numerator = "The factor `1` of a loop integral is neither a propagator ScalarPropagator[q, m]^a nor the square of a momentum: the integral is left unevaluated.";
IntegrateLoops::ir = "The loop integral `1` has an infrared divergence (a massless line of power 2 or more at \[Epsilon] = 0, or of zero momentum): its poles in \[Epsilon] are not only ultraviolet ones. For ultraviolet counterterms give the lines a mass, or use InfraredRearrangedIntegral.";
IntegrateLoops::scaleless = "The loop integral `1` contains a logarithmically divergent scaleless integral, which is 0 in dimensional regularization because its ultraviolet and infrared poles cancel: its ultraviolet pole is lost. For ultraviolet counterterms give the lines a mass, let a momentum flow through it, or use InfraredRearrangedIntegral.";
IntegrateLoops::zero = "A massless line of zero momentum, ScalarPropagator[0, 0], is infinite (an infrared divergence): it is left as it is.";


(* the pieces {value, flags} of LoopIntegral[f, ks]; the loop momenta are replaced by symbols during the algebra (Coefficient
   and Expand work on plain symbols) *)
loopPieces[f_, ks_List, eps_] := Module[{symbols = Table[Unique["FunctionalDiagrams`Private`loopMomentum"], {Length[ks]}], fs},
   fs = f /. Table[ks[[i]] -> symbols[[i]], {i, Length[ks]}];
   Table[integratedTerm[t, symbols, eps], {t, productTermsOf[Expand[squaresAsPropagators[fs, symbols]]]}] /.
    Table[symbols[[i]] -> ks[[i]], {i, Length[ks]}]];

(* numerators that are squares of momenta (from kernel lines such as a counterterm c q^2): q^2 = 1/ScalarPropagator[q, m] - m^2
   with the mass of a line of the same momentum, 1/ScalarPropagator[q, 0] otherwise *)
squaresAsPropagators[f_, ks_List] := Module[{lines = Cases[{f}, ScalarPropagator[q_, m_] /; m =!= 0 :> {Expand[q], m}, Infinity]},
   f /. {MomentumSquared[q_] /; ! FreeQ[q, Alternatives @@ ks] :> inversePropagator[q, lines],
     Power[k_, n_Integer] /; MemberQ[ks, k] && n > 0 && EvenQ[n] :> inversePropagator[k, lines]^(n/2)}];
inversePropagator[q_, lines_List] := With[{match = Select[lines, Expand[#[[1]] - q] === 0 || Expand[#[[1]] + q] === 0 &]},
   If[match === {}, 1/ScalarPropagator[q, 0], 1/ScalarPropagator[match[[1, 1]], match[[1, 2]]] - match[[1, 2]]^2]];

integratedTerm[t_, ks_List, eps_] := Module[{props = {}, constant = 1, bad = {}, flags = {}},
   Do[Which[
     FreeQ[f, Alternatives @@ ks], constant = constant f;
      If[! FreeQ[f, ScalarPropagator[0, 0]], AppendTo[flags, "IR"]],
     MatchQ[f, ScalarPropagator[_, _]], AppendTo[props, {f[[1]], f[[2]], 1}],
     MatchQ[f, Power[ScalarPropagator[_, _], _]], AppendTo[props, {f[[1, 1]], f[[1, 2]], f[[2]]}],
     True, AppendTo[bad, f]], {f, factorList[t]}];
   If[bad =!= {}, {$Failed, {{"Numerator", First[bad]}}},
    With[{r = oneLoopSteps[props, ks, eps]}, {If[r[[1]] === $Failed, $Failed, constant r[[1]]], Union[flags, r[[2]]]}]]];

propagatorProduct[props_List] := Times @@ Table[ScalarPropagator[p[[1]], p[[2]]]^p[[3]], {p, props}];
(* an infrared divergence: a massless line of power 2 or more at eps = 0 *)
irPowerQ[a_, eps_] := With[{a0 = a /. eps -> 0}, NumericQ[a0] && a0 >= 2];
(* a scaleless Int_k (k^2)^(-s): power divergent in the ultraviolet (s < 2 at eps = 0), logarithmic (s = 2), or infrared (s > 2) *)
scalelessPowerClass[s_, eps_] := With[{s0 = s /. eps -> 0}, Which[! NumericQ[s0], "ScalelessLog", s0 < 2, "Scaleless", s0 == 2, "ScalelessLog", True, "IR"]];
scalelessClass[powers_List, eps_] := With[{cs = Table[scalelessPowerClass[s, eps], {s, powers}]},
   If[AllTrue[cs, # === "Scaleless" &], {"Scaleless"}, Union[DeleteCases[cs, "Scaleless"]]]];
(* None, or the flags of a scaleless step: no line (Int_k 1), one massless line, or a numerator next to a massless line
   (shifted to the momentum of that line: (k^2)^(l - b), l = 0 ... n) *)
stepScalelessClass[groups_List, eps_] := Switch[Length[groups],
   0, {"Scaleless"},
   1, If[groups[[1, 2]] === 0, scalelessClass[{groups[[1, 3]]}, eps], None],
   2, With[{g1 = groups[[1]], g2 = groups[[2]]}, Which[
      nonPositiveIntegerQ[g1[[3]]] && nonPositiveIntegerQ[g2[[3]]], {"Scaleless"},
      nonPositiveIntegerQ[g1[[3]]], If[g2[[2]] === 0, scalelessClass[Table[g2[[3]] - l, {l, 0, -g1[[3]]}], eps], None],
      nonPositiveIntegerQ[g2[[3]]], If[g1[[2]] === 0, scalelessClass[Table[g1[[3]] - l, {l, 0, -g2[[3]]}], eps], None],
      True, None]]];
(* whether a two-line step depends on the momentum P of the bubble other than through a new massless line: a massive
   bubble, or tadpoles times powers of P^2 (a numerator next to a massive line) *)
momentumDependentStepQ[{g1_, g2_}] := Which[
   nonPositiveIntegerQ[g1[[3]]] && nonPositiveIntegerQ[g2[[3]]], False,
   nonPositiveIntegerQ[g1[[3]]], g2[[2]] =!= 0,
   nonPositiveIntegerQ[g2[[3]]], g1[[2]] =!= 0,
   True, g1[[2]] =!= 0 || g2[[2]] =!= 0];

(* the step for the loop momentum k: {k, groups {momentum k + r, mass, power}, the other propagators}, or $Failed *)
loopStepFor[props_List, ks_List, k_] := Module[{dep = Select[props, ! FreeQ[#[[1]], k] &], cs, normalized, groups},
   cs = Table[Coefficient[Expand[p[[1]]], k], {p, dep}];
   If[! AllTrue[cs, # === 1 || # === -1 &], $Failed,
    normalized = Table[{Expand[cs[[i]] dep[[i, 1]]], dep[[i, 2]], dep[[i, 3]]}, {i, Length[dep]}];
    groups = Table[{g[[1, 1]], g[[1, 2]], Total[g[[All, 3]]]}, {g, GatherBy[normalized, {#[[1]], #[[2]]} &]}];
    Which[
     Length[groups] > 2, $Failed,
     Length[groups] == 2 && momentumDependentStepQ[groups] &&
       ! FreeQ[Expand[groups[[2, 1]] - groups[[1, 1]]], Alternatives @@ DeleteCases[ks, k]], $Failed,
     True, {k, groups, Select[props, FreeQ[#[[1]], k] &]}]]];

changedLoopMomenta[props_List, ks_List] := Catch[Do[With[{c = Coefficient[Expand[pr[[1]]], k]},
     If[c === 1 || c === -1, With[{new = Table[{Expand[q[[1]] /. k -> c (k - Expand[pr[[1]] - c k])], q[[2]], q[[3]]}, {q, props}]},
       With[{steps = Select[Table[loopStepFor[new, ks, l], {l, ks}], # =!= $Failed &]},
        If[steps =!= {}, Throw[{new, steps}, changeFound]]]]]], {pr, props}, {k, ks}]; $Failed, changeFound];

(* {value, flags} of the product of propagators props over the loop momenta ks *)
oneLoopSteps[props0_List, ks0_List, eps_] := Catch[Module[{props = props0, ks = ks0, value = 1, flags = {}, steps, classes,
     k, groups, rest},
    While[ks =!= {},
     steps = Select[Table[loopStepFor[props, ks, q], {q, ks}], # =!= $Failed &];
     (* otherwise a change of loop momenta k -> c (k - r) that makes the momentum c k + r of a line a loop momentum *)
     If[steps === {}, With[{changed = changedLoopMomenta[props, ks]}, If[changed =!= $Failed, {props, steps} = changed]]];
     If[steps === {}, Throw[{$Failed, {"Irreducible"}}, loopFailure]];
     (* a scaleless loop makes the piece 0; a power-divergent one is a legitimate 0, so it is looked for first *)
     classes = DeleteCases[Table[stepScalelessClass[s[[2]], eps], {s, steps}], None];
     If[MemberQ[classes, {"Scaleless"}], Throw[{0, {"Scaleless"}}, loopFailure]];
     If[classes =!= {}, Throw[{0, Union[flags, First[classes]]}, loopFailure]];
     {k, groups, rest} = First[SortBy[steps, Length[#[[2]]] &]];
     If[Length[groups] == 1,
      (* a massive tadpole (a massless one is scaleless) *)
      value = value TadpoleIntegral[groups[[1, 3]], groups[[1, 2]], eps],
      Module[{g1 = groups[[1]], g2 = groups[[2]], p = Expand[groups[[2, 1]] - groups[[1, 1]]], numerator, line},
       Which[
        (* a numerator next to a massive line (next to a massless one it is scaleless) *)
        nonPositiveIntegerQ[g1[[3]]] || nonPositiveIntegerQ[g2[[3]]],
         {numerator, line} = If[nonPositiveIntegerQ[g1[[3]]], {g1, g2}, {g2, g1}];
         value = value numeratorBubble[-numerator[[3]], numerator[[2]], line[[3]], line[[2]], If[p === 0, 0, MomentumSquared[p]], eps],
        g1[[2]] === 0 && g2[[2]] === 0,
         If[irPowerQ[g1[[3]], eps] || irPowerQ[g2[[3]], eps], AppendTo[flags, "IR"]];
         value = value loopMeasure[eps] GFunction[g1[[3]], g2[[3]], eps];
         rest = Append[rest, {p, 0, g1[[3]] + g2[[3]] - 2 + eps}],
        True,
         If[(g1[[2]] === 0 && irPowerQ[g1[[3]], eps]) || (g2[[2]] === 0 && irPowerQ[g2[[3]], eps]), AppendTo[flags, "IR"]];
         value = value BubbleIntegral[{g1[[3]], g2[[3]]}, If[p === 0, 0, MomentumSquared[p]], {g1[[2]], g2[[2]]}, eps]]]];
     props = rest; ks = DeleteCases[ks, k]];
    {value propagatorProduct[props], Union[flags]}], loopFailure];

(* the loop integrals of e evaluated: {value, problems}, problems a list of {object, problem} with problem "IR",
   "ScalelessLog", "ZeroMomentum" (a massless tree line of zero momentum), "Irreducible" or {"Numerator", f} (the
   integral is then left unevaluated); the lines outside the integrals become 1/(q^2 + m^2)^a *)
evaluatedLoops[e_, eps_] := Module[{problems = {}, value},
   value = e /. LoopIntegral[f_, ks_List] :> With[{pieces = loopPieces[f, ks, eps]},
       If[MemberQ[pieces[[All, 1]], $Failed],
        problems = Join[problems, Table[{LoopIntegral[f, ks], x}, {x, Union @@ Select[pieces, #[[1]] === $Failed &][[All, 2]]}]];
        LoopIntegral[f, ks],
        problems = Join[problems, Table[{LoopIntegral[f, ks], x}, {x, DeleteCases[Union @@ pieces[[All, 2]], "Scaleless"]}]];
        Total[pieces[[All, 1]]]]];
   value = value /. {li_LoopIntegral :> li,
      ScalarPropagator[0, 0]^a_. :> (AppendTo[problems, {ScalarPropagator[0, 0], "ZeroMomentum"}]; ScalarPropagator[0, 0]^a),
      ScalarPropagator[q_, m_]^a_. :> (MomentumSquared[q] + m^2)^(-a)};
   {value, problems}];

IntegrateLoops[e_, eps_Symbol] := With[{r = evaluatedLoops[e, eps]},
   Scan[Function[pr, Switch[pr[[2]],
      "IR", Message[IntegrateLoops::ir, pr[[1]]],
      "ScalelessLog", Message[IntegrateLoops::scaleless, pr[[1]]],
      "ZeroMomentum", Message[IntegrateLoops::zero],
      "Irreducible", Message[IntegrateLoops::irreducible, pr[[1]]],
      {"Numerator", _}, Message[IntegrateLoops::numerator, pr[[2, 2]]]]], Union[r[[2]]]];
   r[[1]]];

(* ---- the infrared rearrangement: one momentum p through two external labels ---- *)

InfraredRearrangedIntegral::norouting = "No routing of one momentum through two of the labels `1` gives the term `2` without infrared divergences and logarithmically divergent scaleless integrals, as nested one-loop integrals (found: `3`).";
problemText[p_] := Switch[p, "IR", "an infrared divergence", "ScalelessLog", "a logarithmically divergent scaleless integral",
   "ZeroMomentum", "a massless line of zero momentum", "Irreducible", "an integral that is not nested one-loop",
   {"Numerator", _}, "the numerator " <> ToString[p[[2]], InputForm], _, ToString[p, InputForm]];

Options[InfraredRearrangedIntegral] = {"Routing" -> False};
InfraredRearrangedIntegral[e_, labels_List, p_, rules_List, eps_Symbol, opts___Rule] := Module[{routing, results},
   routing = optionValue[callOptions[InfraredRearrangedIntegral, {opts}], {}, "Routing"];
   If[! checkRuleKinds[rules, $loopLineKinds, InfraredRearrangedIntegral], $Failed,
    results = rearrangedTerm[#, labels, p, rules, eps] & /@ canonicalTermsOf[e];
    Which[MemberQ[results, $Failed], $Failed, TrueQ[routing], results, True, Total[Table[r[[3]], {r, results}]]]]];

(* the first routing in which no piece of the term has a problem; pieces with a power-divergent scaleless loop are 0 *)
rearrangedTerm[t_, labels_List, p_, rules_List, eps_] := Catch[Module[{found = {}, externals, li, r},
    Do[externals = Table[labels[[i]] -> Which[i == pair[[1]], p, i == pair[[2]], -p, True, 0], {i, Length[labels]}];
     li = termLoopIntegral[t, externals, rules, Automatic, InfraredRearrangedIntegral];
     If[li === $Failed, Throw[$Failed, rearranged]];
     r = evaluatedLoops[li, eps];
     If[r[[2]] === {}, Throw[{termToExpression[t], labels[[pair]], r[[1]]}, rearranged]];
     found = Union[found, r[[2, All, 2]]], {pair, Subsets[Range[Length[labels]], {2}]}];
    Message[InfraredRearrangedIntegral::norouting, labels, termToExpression[t], joinStrings[problemText /@ found, ", "]]; $Failed],
   rearranged];

(* ---- -d/dm^2 through the lines: G0[x, y] -> G0[x, z] G0[z, y] ---- *)

MassInsertion::label = "The label `1` of the insertion is not a new symbol: it must be a symbol that does not appear in the diagrams.";
MassInsertion[e_, g_Symbol, z_] := If[Head[z] =!= Symbol || ! FreeQ[e, z], Message[MassInsertion::label, z]; $Failed,
   SimplifyDiagrams[Total[insertMassTerm[#, g, z] & /@ canonicalTermsOf[e]]]];
insertMassTerm[diagramTerm[c_, o_, d_], g_, z_] := Module[{t},
   With[{c1 = Expand[D[c /. g[x_, y_] :> g[x, y] + t g[x, z] g[z, y], t] /. t -> 0]},
    Total[termToExpression[diagramTerm[#, o, d]] & /@ productTermsOf[c1]]]];

(* ---- truncated Laurent series: {v, {c0, c1, ...}} = Sum_i c_i eps^(v + i), known up to eps^(v + Length - 1) ---- *)

EpsilonExpand::function = "The factor `1` is not expanded in `2`: only Gamma functions of arguments linear in it, powers, exponentials, rational functions and massive bubbles are.";
EpsilonExpand::pole = "The expansion of `1` in `2` did not converge: an essential singularity or a series that vanishes identically.";
EpsilonExpand::loop = "`1` is an unevaluated loop integral: its dependence on `2` (through d = 4 - 2 `2`) is not explicit. Evaluate it first with IntegrateLoops (LoopIntegral) or IntegrateOneLoop (MinkowskiLoopIntegral).";
EpsilonExpand::variable = "The bubble `1` is in d = 4 - 2 `3`, not in d = 4 - 2 `2`: it is not expanded.";
EpsilonExpand::bubble = "The bubble `1` is not expanded in `2`: its powers must be positive and independent of `2`, and those of its massless lines less than 2 (otherwise the bubble is infrared divergent).";

serKnown[s_List] := s[[1]] + Length[s[[2]]] - 1;
serConstant[c_, order_] := {0, Table[If[i == 0, c, 0], {i, 0, order}]};
serMonomial[k_, order_] := {k, Table[If[i == 0, 1, 0], {i, 0, order - k}]};
serTimes[s1_List, s2_List, order_] := Module[{v = s1[[1]] + s2[[1]], top, c1 = s1[[2]], c2 = s2[[2]]},
   top = Min[order, serKnown[s1] + s2[[1]], serKnown[s2] + s1[[1]]];
   {v, Table[Sum[c1[[i + 1]] c2[[j - i + 1]], {i, Max[0, j - Length[c2] + 1], Min[j, Length[c1] - 1]}], {j, 0, top - v}]}];
serPlus[ss_List, order_] := Module[{v = Min[Table[s[[1]], {s, ss}]], top = Min[order, Min[Table[serKnown[s], {s, ss}]]]},
   {v, Table[Sum[With[{i = j - s[[1]]}, If[0 <= i < Length[s[[2]]], s[[2, i + 1]], 0]], {s, ss}], {j, v, top}]}];
serScale[s_List, c_] := {s[[1]], c s[[2]]};
(* exp of a series without constant term (valuation >= 1) *)
serExp[s_List, order_] := Module[{result = serConstant[1, order], power = serConstant[1, order]},
   Do[power = serScale[serTimes[power, s, order], 1/j]; result = serPlus[{result, power}, order], {j, 1, Max[order, 0]}];
   result];
(* a coefficient is 0: exact tests first; a value at generic positive values of its symbols that is clearly not 0 proves
   it is not (so Simplify, slow, is called only for coefficients that look like 0) *)
zeroQ[c_] := c === 0 || Expand[c] === 0 || Together[c] === 0 ||
   (! numericallyNonZeroQ[c] && LeafCount[c] < 400 && Simplify[c] === 0);
numericallyNonZeroQ[c_] := FreeQ[c, FeynmanParameterIntegral] && Module[{symbols = Union[Cases[{c}, s_Symbol /; ! NumericQ[s], Infinity]], v},
   v = Quiet[N[c /. Table[symbols[[i]] -> Prime[i + 4]/7, {i, Length[symbols]}], 30]];
   NumberQ[v] && Abs[v] > 10^-12];
serStrip[s_List] := Module[{v = s[[1]], cs = s[[2]]},
   While[cs =!= {} && zeroQ[First[cs]], cs = Rest[cs]; v = v + 1];
   {v, cs}];
(* 1/s, s with a non-zero first coefficient *)
serInverse[s_List, order_] := Module[{c = s[[2]], n, b = {}},
   n = Min[order + s[[1]] + 1, Length[c]];
   Do[AppendTo[b, If[k == 0, 1/c[[1]], -Sum[c[[i + 1]] b[[k - i + 1]], {i, 1, k}]/c[[1]]]], {k, 0, n - 1}];
   {-s[[1]], b}];

(* the valuation of a factor: a lower bound, exact for the factors whose series is computed from it *)
valuation[f_, eps_] /; FreeQ[f, eps] := 0;
valuation[f_, eps_] /; f === eps := 1;
valuation[f_Times, eps_] := Total[valuation[#, eps] & /@ (List @@ f)];
valuation[f_Plus, eps_] := Min[valuation[#, eps] & /@ (List @@ f)];
valuation[Power[b_, e_], eps_] := Which[FreeQ[b, eps], 0, IntegerQ[e] && e > 0, e valuation[b, eps],
   IntegerQ[e] && e < 0, e strippedSeries[b, eps, 0][[1]], True, 0];
valuation[Gamma[x_], eps_] := With[{x0 = x /. eps -> 0}, If[IntegerQ[x0] && x0 <= 0, -1, 0]];
valuation[_, eps_] := 0;

(* within one expansion (EpsilonExpand, ExpandParameterIntegrals) the series of each factor and of each base are
   remembered: the same 1/eps, Gamma functions and powers recur in every term (a failure throws and is not stored) *)
$seriesMemo = False;
ser[f_, eps_, order_] := If[$seriesMemo, With[{v = serTable[f, eps, order]},
    If[Head[v] === serTable, serTable[f, eps, order] = serValue[f, eps, order], v]], serValue[f, eps, order]];
strippedSeries[b_, eps_, extra_] := If[$seriesMemo, With[{v = strippedTable[b, eps, extra]},
    If[Head[v] === strippedTable, strippedTable[b, eps, extra] = strippedSeriesValue[b, eps, extra], v]], strippedSeriesValue[b, eps, extra]];
SetAttributes[withSeriesMemo, HoldAll];
withSeriesMemo[e_] := If[$seriesMemo, e, Block[{$seriesMemo = True, serTable, strippedTable}, e]];

(* the series of b with its true valuation, known to at least `extra` orders beyond it *)
strippedSeriesValue[b_, eps_, extra_] := Module[{v0 = valuation[b, eps], s, k = 0},
   s = serStrip[ser[b, eps, v0 + extra + 2]];
   While[s[[2]] === {} && k < 6, k++; s = serStrip[ser[b, eps, v0 + extra + 2 + 3 k]]];
   If[s[[2]] === {}, Message[EpsilonExpand::pole, b, eps]; Throw[$Failed, expansionFailure]];
   If[serKnown[s] < s[[1]] + extra, s = serStrip[ser[b, eps, s[[1]] + extra]]];
   s];

serValue[f_, eps_, order_] /; FreeQ[f, eps] := serConstant[f, order];
serValue[f_, eps_, order_] /; f === eps := serMonomial[1, order];
serValue[f_Plus, eps_, order_] := serPlus[ser[#, eps, order] & /@ (List @@ f), order];
serValue[f_Times, eps_, order_] := Module[{fs = List @@ f, vs, total, acc},
   vs = valuation[#, eps] & /@ fs; total = Total[vs];
   acc = ser[fs[[1]], eps, order - (total - vs[[1]])];
   Do[acc = serTimes[acc, ser[fs[[i]], eps, order - (total - vs[[i]])], order - Total[Drop[vs, i]]], {i, 2, Length[fs]}];
   acc];
(* an integer power b^e: the j-th partial product b^(j+1) is needed to the order that the e - 1 - j factors still to be
   multiplied, each of valuation v, leave at `order` (beyond it when they have a pole, v < 0); the same for (1/b)^k *)
serValue[Power[b_, e_], eps_, order_] := Which[
   FreeQ[b, eps], With[{e0 = e /. eps -> 0},
    serTimes[serConstant[b^e0, order], serExp[serScale[ser[Expand[e - e0], eps, order], Log[b]], order], order]],
   IntegerQ[e] && e > 0, Module[{v = valuation[b, eps], s, acc},
    s = ser[b, eps, order - (e - 1) v]; acc = s;
    Do[acc = serTimes[acc, s, order - (e - 1 - j) Min[v, 0]], {j, 1, e - 1}]; acc],
   IntegerQ[e] && e < 0, Module[{s0 = strippedSeries[b, eps, 0], vb, s, inv, k = -e, acc},
    vb = s0[[1]];
    (* 1/b, of valuation -vb, to the order needed by its k-th power, from the series of b to that many terms *)
    s = strippedSeries[b, eps, order + (k - 1) vb + vb + 1];
    inv = serInverse[s, order + (k - 1) vb];
    acc = inv; Do[acc = serTimes[acc, inv, order + (k - 1 - j) Max[vb, 0]], {j, 1, k - 1}]; acc],
   True, Message[EpsilonExpand::function, Power[b, e], eps]; Throw[$Failed, expansionFailure]];
(* Gamma(x0 + c eps) = Gamma(x0) exp(Sum_k PolyGamma(k - 1, x0) (c eps)^k/k!), through the recurrence at the poles *)
serValue[Gamma[x_], eps_, order_] := Module[{xe = Expand[x], x0, c},
   x0 = xe /. eps -> 0; c = Coefficient[xe, eps];
   Which[
    Expand[xe - x0 - c eps] =!= 0, Message[EpsilonExpand::function, Gamma[x], eps]; Throw[$Failed, expansionFailure],
    IntegerQ[x0] && x0 <= 0, ser[Gamma[xe + 1 - x0] Times @@ Table[Power[xe + j, -1], {j, 0, -x0}], eps, order],
    True, serTimes[serConstant[Gamma[x0], order],
     serExp[{1, Table[polyGammaValue[k - 1, x0] c^k/k!, {k, 1, order}]}, order], order]]];
(* PolyGamma(n, x) at a positive integer x through the zeta function: (-1)^(n+1) n! (zeta(n + 1) - Sum_(j < x) j^-(n+1)) *)
polyGammaValue[n_, x_] := If[IntegerQ[x] && x >= 1 && n >= 1, (-1)^(n + 1) n! (Zeta[n + 1] - Sum[1/j^(n + 1), {j, 1, x - 1}]), PolyGamma[n, x]];
serValue[bubbleParameterIntegral[a_, b_, p2_, m1_, m2_, eps_], eps_, order_] :=
  {0, Table[(-1)^k/k! feynmanParameterCoefficient[a, b, p2, m1, m2, k], {k, 0, order}]};
serValue[f_, eps_, order_] := (Message[EpsilonExpand::function, f, eps]; Throw[$Failed, expansionFailure]);

(* the powers X^(c eps) of a product written as (X/mu^2)^(c eps) when the powers of mu = RenormalizationScale balance them *)
scaleLogs[t_, eps_] := Module[{fs = factorList[t], isPower, powers, others, mu2 = RenormalizationScale^2, sum},
   isPower[f_] := MatchQ[f, Power[b_, e_] /; FreeQ[b, eps] && ! FreeQ[e, eps] && ! NumericQ[b]];
   powers = Select[fs, isPower]; others = Select[fs, ! isPower[#] &];
   sum = Expand[Total[Table[With[{e = f[[2]]}, e - (e /. eps -> 0)], {f, powers}]]];
   If[sum =!= 0 || ! MemberQ[Table[f[[1]], {f, powers}], mu2], t,
    (Times @@ others) Times @@ Table[With[{b = f[[1]], e = f[[2]]}, With[{e0 = e /. eps -> 0},
        If[b === mu2, mu2^e0, b^e0 (b/mu2)^Expand[e - e0]]]], {f, powers}]]];

(* a massive bubble left by BubbleIntegral: e^(eps gammaE) Gamma(a + b - 2 + eps)/(16 pi^2 Gamma(a) Gamma(b)) times
   Int_0^1 dx x^(a-1) (1-x)^(b-1) Delta^(2-a-b) (Delta/mu^2)^(-eps), the last factor expanded in eps under the integral:
   valid for powers independent of eps, positive, and less than 2 on a massless line (no infrared divergence) *)
expandableBubbleQ[{a_, b_}, {m1_, m2_}, eps_] := FreeQ[{a, b}, eps] && ! (NumericQ[a] && a <= 0) && ! (NumericQ[b] && b <= 0) &&
   ! (m1 === 0 && irPowerQ[a, eps]) && ! (m2 === 0 && irPowerQ[b, eps]);
bubbleForExpansion[e_, eps_] := e /. BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps] :>
    If[expandableBubbleQ[{a, b}, {m1, m2}, eps],
     Exp[eps EulerGamma] Gamma[a + b - 2 + eps]/(16 Pi^2 Gamma[a] Gamma[b]) bubbleParameterIntegral[a, b, p2, m1, m2, eps],
     Message[EpsilonExpand::bubble, BubbleIntegral[{a, b}, p2, {m1, m2}, eps], eps]; Throw[$Failed, expansionFailure]];

(* the series of a sum, term by term *)
sumSeries[g_, eps_, n_] := serPlus[Table[ser[scaleLogs[t, eps], eps, n], {t, productTermsOf[Expand[g]]}], n];
EpsilonExpand[e_, eps_Symbol, n_Integer] := withSeriesMemo[Catch[Module[{loops, others, result},
    loops = Cases[{e}, _LoopIntegral | _MinkowskiLoopIntegral, Infinity];
    If[loops =!= {}, Message[EpsilonExpand::loop, First[loops], eps]; Throw[$Failed, expansionFailure]];
    others = Cases[{e}, BubbleIntegral[_, _, _, x_] /; x =!= eps, Infinity];
    If[others =!= {}, Message[EpsilonExpand::variable, First[others], eps, Last[First[others]]]; Throw[$Failed, expansionFailure]];
    result = sumSeries[bubbleForExpansion[e, eps], eps, n];
    Sum[Expand[result[[2, j + 1]]] eps^(result[[1]] + j), {j, 0, Length[result[[2]]] - 1}]], expansionFailure]];

PolePart[e_, eps_Symbol] := EpsilonExpand[e, eps, -1];

(* ---- the Feynman-parameter coefficients of a massive bubble: Int_0^1 dx w(x) Delta^n0 ln^k(Delta/mu^2),
   w = x^(a-1) (1-x)^(b-1), n0 = 2 - a - b, Delta = x(1-x) p2 + x m1^2 + (1-x) m2^2. In closed form for k = 0, n0 >= 0
   (a polynomial), for k = 1, n0 = 0 (a = b = 1) and for k = 0, n0 = -1 (a + b = 3); FeynmanParameterIntegral otherwise.
   For p2 > 0, Delta = p2 (xp - x)(x - xm) with xp >= 1 and xm <= 0, the roots in the square root of the Kallen function
   (xp = 1 for a massless first line, xm = 0 for a massless second line) ---- *)

SetAttributes[FeynmanParameterIntegral, NHoldAll];
(* the options of NIntegrate for N to a precision (also those of the integrals of QED) *)
precisionOptions[{p_, _}] := precisionOptions[p];
precisionOptions[p_?NumberQ] /; p > MachinePrecision := {WorkingPrecision -> p + 10, PrecisionGoal -> p};
precisionOptions[___] := {};
FeynmanParameterIntegral /: N[FeynmanParameterIntegral[f_, x_Symbol], precision___] :=
  NIntegrate[f, {x, 0, 1}, Evaluate[Sequence @@ precisionOptions[precision]]];

(* Int_0^u dx Sum_j cs[[j]] x^(j - 1) *)
powerSumIntegral[cs_List, u_] := Sum[cs[[j]] u^j/j, {j, Length[cs]}];
polynomialIntegral[poly_, x_] := powerSumIntegral[CoefficientList[Expand[poly], x], 1];
bubbleRoots[p2_, m1_, m2_] := Which[
   m1 === 0 && m2 === 0, {1, 0},
   m2 === 0, {1 + m1^2/p2, 0},
   m1 === 0, {1, -m2^2/p2},
   m1 === m2, With[{beta = Sqrt[1 + 4 m1^2/p2]}, {(1 + beta)/2, (1 - beta)/2}],
   True, With[{s = Sqrt[(p2 + m1^2 - m2^2)^2 + 4 p2 m2^2]}, {(p2 + m1^2 - m2^2 + s)/(2 p2), (p2 + m1^2 - m2^2 - s)/(2 p2)}]];
(* Int_0^1 x^j ln(r - x) dx for r >= 1, and Int_0^1 x^j ln(x - r) dx for r <= 0 *)
rightRootLogMoment[r_, j_] := If[r === 1, -Sum[1/i, {i, 1, j + 1}]/(j + 1),
   ((1 - r^(j + 1)) Log[r - 1] + r^(j + 1) Log[r])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)];
leftRootLogMoment[r_, j_] := If[r === 0, -1/(j + 1)^2,
   ((1 - r^(j + 1)) Log[1 - r] + r^(j + 1) Log[-r])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)];
(* Int_0^1 x^j/(x - r) dx, r outside [0, 1]; logRatio = ln|(1 - r)/r| *)
inverseMoment[r_, j_, logRatio_] := Sum[r^(j - 1 - i)/(i + 1), {i, 0, j - 1}] + r^j logRatio;
(* Int_0^1 dx poly(x)/(x - r) and poly(x)/(r - x), from the coefficients of the polynomial *)
inverseMoments[cs_List, r_, logRatio_] := Sum[cs[[j + 1]] inverseMoment[r, j, logRatio], {j, 0, Length[cs] - 1}];

feynmanParameterCoefficient[a_, b_, p2_, m1_, m2_, k_] := Module[{x = FeynmanParameter, n0 = 2 - a - b, w, delta, cs, mu2 = RenormalizationScale^2},
   w = x^(a - 1) (1 - x)^(b - 1); delta = x (1 - x) p2 + x m1^2 + (1 - x) m2^2;
   cs = If[IntegerQ[a] && IntegerQ[b] && a >= 1 && b >= 1, CoefficientList[Expand[w], x], {}];
   Which[
    cs === {}, FeynmanParameterIntegral[w delta^n0 Log[delta/mu2]^k, x],
    k == 0 && n0 >= 0, polynomialIntegral[w delta^n0, x],
    k == 1 && n0 == 0, Which[
     p2 === 0 && m1 === m2, Log[m1^2/mu2] polynomialIntegral[w, x],
     p2 === 0 && m1 =!= 0 && m2 =!= 0, With[{r = m2^2/(m2^2 - m1^2)},
      Sum[cs[[j + 1]] (((1 - r^(j + 1)) Log[m1^2/mu2] + r^(j + 1) Log[m2^2/mu2])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)),
       {j, 0, Length[cs] - 1}]],
     p2 === 0, FeynmanParameterIntegral[w Log[delta/mu2], x],
     m1 === m2 && m1 =!= 0 && cs === {1}, With[{beta = Sqrt[1 + 4 m1^2/p2]}, Log[m1^2/mu2] - 2 + 2 beta ArcTanh[1/beta]],
     True, With[{roots = bubbleRoots[p2, m1, m2]},
      Log[p2/mu2] polynomialIntegral[w, x] + Sum[cs[[j + 1]] (rightRootLogMoment[roots[[1]], j] + leftRootLogMoment[roots[[2]], j]), {j, 0, Length[cs] - 1}]]],
    k == 0 && n0 == -1, Which[
     p2 === 0 && m1 === m2, polynomialIntegral[w, x]/m1^2,
     p2 === 0 && m1 =!= 0 && m2 =!= 0, With[{r = m2^2/(m2^2 - m1^2)},
      Sum[cs[[j + 1]] inverseMoment[r, j, Log[m1^2/m2^2]], {j, 0, Length[cs] - 1}]/(m1^2 - m2^2)],
     p2 === 0, FeynmanParameterIntegral[w/delta, x],
     (* a massless second line: Delta = p2 x (xp - x), w = x x^(a-2) (1-x)^(b-1) (finite for a >= 2) *)
     m2 === 0 && m1 =!= 0 && a >= 2, With[{xp = bubbleRoots[p2, m1, m2][[1]]},
      -inverseMoments[CoefficientList[Expand[x^(a - 2) (1 - x)^(b - 1)], x], xp, Log[(xp - 1)/xp]]/p2],
     (* a massless first line: Delta = p2 (1 - x)(x - xm), w = (1-x) x^(a-1) (1-x)^(b-2) (finite for b >= 2) *)
     m1 === 0 && m2 =!= 0 && b >= 2, With[{xm = bubbleRoots[p2, m1, m2][[2]]},
      inverseMoments[CoefficientList[Expand[x^(a - 1) (1 - x)^(b - 2)], x], xm, Log[(1 - xm)/(-xm)]]/p2],
     m1 === 0 || m2 === 0, FeynmanParameterIntegral[w/delta, x],
     True, With[{roots = bubbleRoots[p2, m1, m2]},
      (* 1/Delta = (1/(p2 (xp - xm))) (1/(xp - x) + 1/(x - xm)) *)
      (inverseMoments[cs, roots[[2]], Log[(1 - roots[[2]])/(-roots[[2]])]] -
         inverseMoments[cs, roots[[1]], Log[(roots[[1]] - 1)/roots[[1]]]])/(p2 (roots[[1]] - roots[[2]]))]],
    True, FeynmanParameterIntegral[w delta^n0 Log[delta/mu2]^k, x]]];

(* ---- renormalization-group functions from renormalization constants (MS-bar, d = 4 - 2 eps) ---- *)

BetaFunctionFromZ::pole = "The limit `1` -> 0 of `2` does not exist: the poles of the renormalization constant are not consistent.";
AnomalousDimensionFromZ::pole = BetaFunctionFromZ::pole;
(* the coefficients c0 ... cn of the series in g of a rational function of g *)
gSeriesCoefficients[e_, g_, n_] := Module[{t = Together[e], num, den},
   num = Take[PadRight[CoefficientList[Expand[Numerator[t]], g], n + 1], n + 1];
   den = Take[PadRight[CoefficientList[Expand[Denominator[t]], g], n + 1], n + 1];
   Take[PadRight[Together /@ serTimes[{0, num}, serInverse[{0, den}, n], n][[2]], n + 1], n + 1]];
atZeroEpsilon[c_, eps_, what_, caller_] := With[{t = Together[c]},
   If[(Denominator[t] /. eps -> 0) === 0, Message[MessageName[caller, "pole"], eps, what]; $Failed, Together[t /. eps -> 0]]];
(* g_B = mu^(s eps) Z g: 0 = s eps + (1/g + d ln Z/dg) beta_eps, beta_eps = -s eps g/(1 + g d ln Z/dg) = -s eps g + beta(g) *)
Options[BetaFunctionFromZ] = {"ScalePower" -> 2};
BetaFunctionFromZ[z_, g_Symbol, eps_Symbol, n_Integer, opts___Rule] := Module[{s, cs, values},
   s = optionValue[callOptions[BetaFunctionFromZ, {opts}], {}, "ScalePower"];
   cs = gSeriesCoefficients[-s eps g/(1 + g D[z, g]/z) + s eps g, g, n];
   values = Table[atZeroEpsilon[cs[[j + 1]], eps, "the beta function", BetaFunctionFromZ], {j, 0, n}];
   If[MemberQ[values, $Failed], $Failed, Sum[values[[j + 1]] g^j, {j, 0, n}]]];
AnomalousDimensionFromZ[z_, g_Symbol, eps_Symbol, betaEps_, n_Integer] := Module[{cs, values},
   cs = gSeriesCoefficients[betaEps D[z, g]/z, g, n];
   values = Table[atZeroEpsilon[cs[[j + 1]], eps, "the anomalous dimension", AnomalousDimensionFromZ], {j, 0, n}];
   If[MemberQ[values, $Failed], $Failed, Sum[values[[j + 1]] g^j, {j, 0, n}]]];


(* ---- display and LaTeX of the new heads ---- *)
(* (GFunction and TadpoleIntegral are always evaluated) *)
defineDisplayForm[ScalarPropagator[q_, m_], Interpretation[1/If[m === 0, HoldForm[#] &[MomentumSquared[q]], HoldForm[#1 + #2] &[MomentumSquared[q], m^2]],
   ScalarPropagator[q, m]]];
defineDisplayForm[MomentumSquared[q_], Interpretation[HoldForm[#^2] &[q], MomentumSquared[q]]];
defineDisplayForm[RenormalizationScale, Interpretation["\[Mu]", RenormalizationScale]];
defineDisplayForm[FeynmanParameter, Interpretation["x", FeynmanParameter]];
defineDisplayForm[LoopIntegral[f_, ks_List], Interpretation[Row[{Subscript["\[Integral]", Row[ks, ","]], " ", f}], LoopIntegral[f, ks]]];
defineDisplayForm[BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_],
  Interpretation[Row[{Subscript["B", Row[{a, b}, ","]], "(", p2, "; ", m1, ", ", m2, ")"}], BubbleIntegral[{a, b}, p2, {m1, m2}, eps]]];
defineDisplayForm[FeynmanParameterIntegral[f_, x_], Interpretation[Row[{Subsuperscript["\[Integral]", 0, 1], "d", x, " ", f}],
   FeynmanParameterIntegral[f, x]]];

(* the generic rules of latexFactorOf must not apply to the new heads; mu, x and gammaE by their names *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {ScalarPropagator, MomentumSquared, LoopIntegral, GFunction, TadpoleIntegral,
    BubbleIntegral, FeynmanParameterIntegral, ArcTanh, Zeta}];
$latexDefaultNames = Join[$latexDefaultNames, {RenormalizationScale -> "\\mu", FeynmanParameter -> "x", EulerGamma -> "\\gamma_E"}];
latexMomentumSquared[q_] := If[Head[q] === Plus, "(" <> latexOfExpression[q] <> ")^{2}", latexParenthesized[q] <> "^{2}"];
(* the denominator (q^2 + m^2)^a of a line, and whether it needs parentheses next to other factors *)
latexPropagatorDenominator[q_, m_, a_] := With[{s = If[m === 0, latexMomentumSquared[q], latexMomentumSquared[q] <> " + " <> latexParenthesized[m] <> "^{2}"]},
   If[a === 1, s, "\\left(" <> s <> "\\right)^{" <> latexOfExpression[a] <> "}"]];
propagatorFactorQ[f_] := MatchQ[f, ScalarPropagator[_, _]] || MatchQ[f, Power[ScalarPropagator[_, _], a_?NumericQ] /; a > 0];
latexPropagatorFactor[ScalarPropagator[q_, m_]] := {m =!= 0, latexPropagatorDenominator[q, m, 1]};
latexPropagatorFactor[Power[ScalarPropagator[q_, m_], a_]] := {False, latexPropagatorDenominator[q, m, a]};
(* a product with lines: one fraction, the lines in its denominator *)
latexWithPropagators[f_] := Module[{fs = If[Head[f] === Times, List @@ f, {f}], lines, others, dens},
   lines = Select[fs, propagatorFactorQ]; others = Times @@ Select[fs, ! propagatorFactorQ[#] &];
   If[lines === {}, latexOfExpression[f],
    dens = latexPropagatorFactor /@ lines;
    If[NumberQ[others] && others < 0, "-", ""] <> "\\frac{" <> If[others === 1 || others === -1, "1", latexOfExpression[If[NumberQ[others], Abs[others], others]]] <> "}{" <>
     joinStrings[Table[If[Length[dens] > 1 && d[[1]], "(" <> d[[2]] <> ")", d[[2]]], {d, dens}], "\\,"] <> "}"]];
latexFactorOf[ScalarPropagator[q_, m_]] := "\\frac{1}{" <> latexPropagatorDenominator[q, m, 1] <> "}";
latexFactorOf[Power[ScalarPropagator[q_, m_], a_]] := "\\frac{1}{" <> latexPropagatorDenominator[q, m, a] <> "}";
latexFactorOf[MomentumSquared[q_]] := latexMomentumSquared[q];
latexFactorOf[LoopIntegral[f_, ks_List]] := "\\int_{" <> joinStrings[latexArgument /@ ks, "\\,"] <> "}" <>
   If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexWithPropagators[f]];
latexFactorOf[GFunction[a_, b_, eps_]] := "G(" <> latexOfExpression[a] <> "," <> latexOfExpression[b] <> ")";
latexFactorOf[TadpoleIntegral[a_, m_, eps_]] := "A_{" <> latexOfExpression[a] <> "}(" <> latexArgument[m] <> ")";
latexFactorOf[BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_]] := "B_{" <> latexOfExpression[a] <> "," <> latexOfExpression[b] <> "}(" <>
   latexOfExpression[p2] <> ";" <> latexArgument[m1] <> "," <> latexArgument[m2] <> ")";
latexFactorOf[ArcTanh[x_]] := "\\operatorname{artanh}\\left(" <> latexOfExpression[x] <> "\\right)";
latexFactorOf[Zeta[x_]] := "\\zeta(" <> latexOfExpression[x] <> ")";
