
(* ========================================================================= *)
(*  17. Matsubara sums and the evaluation of diagrams                        *)
(*  A diagram of the package lives in label space: a label stands for a      *)
(*  position, an imaginary time and a spin. In a translation-invariant       *)
(*  system it is evaluated in momentum and Matsubara frequency: momenta and  *)
(*  frequencies are routed through the lines (each integral over an         *)
(*  internal label is their conservation), the frequency of each loop is    *)
(*  summed by residues, and the result can be continued to real frequency.  *)
(*  The imaginary part of a retarded two-point function is also given by    *)
(*  the cutting rules at finite temperature, computed independently, in     *)
(*  imaginary time.                                                          *)
(*  Conventions: G0(k, i w) = 1/(i w - xi_k), G(tau) = T Sum_n               *)
(*  e^(-i w_n tau) G(i w_n); a line G[x, y] goes from y to x and carries its *)
(*  momentum and frequency from y to x; a line between labels at equal times *)
(*  (G(z, z), or two labels joined by an instantaneous interaction) is       *)
(*  G(tau = 0-): factor e^(i w 0+), T Sum e^(i w 0+)/(i w - xi) = n_F(xi),   *)
(*  the ordering psibar psi; "ConvergenceFactor" -> -1 takes G(0+),          *)
(*  e^(-i w 0+), n_F - 1 (the ordering psi psibar), and 0 the mean of the    *)
(*  two, the symmetric sum, n_F - 1/2 (the ordering 1/2 [psibar, psi]).      *)
(*  Residues are taken without Residue, Limit or Apart: the poles are read   *)
(*  from the linear denominators of each term.                               *)
(* ========================================================================= *)

(* ---- statistics of Matsubara frequencies ---- *)

statisticalFunction["Fermion"] = FermiFunction;
statisticalFunction["Boson"] = BoseFunction;
otherStatisticalFunction[FermiFunction] = BoseFunction;
otherStatisticalFunction[BoseFunction] = FermiFunction;

(* the frequencies declared by the user: {symbol, statistics}. The loop frequencies LoopFrequency[i, statistics] carry
   their statistics with them; during the sums they are replaced by symbols declared here (withFrequencySymbols) *)
$matsubaraFrequencies = {};
MatsubaraSum::statistics = "The statistics `1` is neither \"Fermion\" nor \"Boson\".";
DeclareMatsubaraFrequency::symbol = "`1` is not a symbol: a Matsubara frequency is declared as a symbol w, written I w in expressions.";
DeclareMatsubaraFrequency[w_Symbol, s_] := DeclareMatsubaraFrequency[{w}, s];
DeclareMatsubaraFrequency[ws_List, s_] := Which[
   ! MemberQ[{"Fermion", "Boson"}, s], Message[MatsubaraSum::statistics, s],
   ! AllTrue[ws, Head[#] === Symbol &], Scan[If[Head[#] =!= Symbol, Message[DeclareMatsubaraFrequency::symbol, #]] &, ws],
   True, Scan[warnIfBuiltin, ws];
   $matsubaraFrequencies = Join[Select[$matsubaraFrequencies, ! MemberQ[ws, #[[1]]] &], ({#, s} &) /@ ws];];
DeclareMatsubaraFrequency[w_, s_] /; Head[w] =!= Symbol && Head[w] =!= List := Message[DeclareMatsubaraFrequency::symbol, w];
frequencyStatistics[LoopFrequency[_, s_String]] := s;
frequencyStatistics[w_] := With[{m = Select[$matsubaraFrequencies, #[[1]] === w &]}, If[m === {}, None, m[[1, 2]]]];
frequencyAtoms[e_] := Select[Table[f[[1]], {f, $matsubaraFrequencies}], ! FreeQ[e, #] &];

(* the loop frequencies of e replaced by symbols, declared with their statistics, while g[e with symbols, rules back] is
   computed; the result is written with the loop frequencies again. So Coefficient, Exponent, Factor and Together work
   on plain symbols *)
withFrequencySymbols[e_, g_] := Module[{fs = Union[Cases[{e}, LoopFrequency[_, "Fermion" | "Boson"], Infinity]], symbols, forward, back},
   symbols = Table[Unique["FunctionalDiagrams`Private`loopFrequency"], {Length[fs]}];
   forward = Table[fs[[i]] -> symbols[[i]], {i, Length[fs]}];
   back = Table[symbols[[i]] -> fs[[i]], {i, Length[fs]}];
   Block[{$matsubaraFrequencies = Join[$matsubaraFrequencies, Table[{symbols[[i]], fs[[i, 2]]}, {i, Length[fs]}]]},
    g[e /. forward, back] /. back]];

(* ---- periodicity: n_F(e + i nu) = n_F(e), n_F(e + i w) = -n_B(e), n_B(e + i nu) = n_B(e), n_B(e + i w) = -n_F(e),
   for bosonic nu and fermionic w, and the same for the derivatives ---- *)

(* the shift of an energy by Matsubara frequencies (written i w): {energy without them, 1 if the shift is fermionic,
   0 if it is bosonic} *)
MatsubaraSum::shift = "`1` is shifted by a combination of `2` that is not a Matsubara frequency (integer coefficients of i w, with statistics declared by DeclareMatsubaraFrequency).";
matsubaraShift[a_] := Module[{fs = frequencyAtoms[a], ex, cs, stats},
   If[fs === {}, {a, 0},
    ex = Expand[a]; cs = Table[Coefficient[ex, f]/I, {f, fs}]; stats = frequencyStatistics /@ fs;
    If[! AllTrue[cs, IntegerQ] || MemberQ[stats, None], Message[MatsubaraSum::shift, a, fs]; {a, 0},
     {Expand[ex /. Table[f -> 0, {f, fs}]], Mod[Total[Table[If[stats[[i]] === "Fermion", cs[[i]], 0], {i, Length[fs]}]], 2]}]]];

periodicStatistics[h_, k_, a_] := Module[{s = matsubaraShift[a], g},
   g = If[s[[2]] == 1, otherStatisticalFunction[h], h];
   If[s[[2]] == 1, -1, 1] signCanonicalStatistics[g, k, s[[1]]]];

(* the argument with a positive leading term: n_F(-x) = 1 - n_F(x), n_B(-x) = -1 - n_B(x), and for the derivatives
   n^(k)(-x) = (-1)^(k + 1) n^(k)(x). The leading term of a sum is its first term, in canonical order, that is neither a
   number nor a multiple of a single symbol, if there is one: a constant such as the chemical potential does not decide
   the sign, n_F(eps(p) - mu) and n_F(p^2/(2 m) - mu) stay as they are. n_F(0) = 1/2 at every temperature. *)
FermiFunction[0] = 1/2;
FermiFunction[0.] = 0.5;
constantTermQ[t_] := NumberQ[t] || MatchQ[t, _Symbol | Times[_?NumberQ, _Symbol]];
leadingTerm[x_Plus] := With[{main = Select[List @@ x, ! constantTermQ[#] &]}, If[main === {}, First[x], First[main]]];
leadingTerm[x_] := x;
leadingSign[e_] := With[{x = Expand[e]}, If[x === 0, 0, signOfTerm[leadingTerm[x]]]];
signOfTerm[t_] := If[TrueQ[Re[numericFactor[t]] < 0], -1, 1];
signCanonicalStatistics[h_, k_, a_] := Which[
   leadingSign[a] >= 0, If[k == 0, h[a], Derivative[k][h][a]],
   k == 0, If[h === FermiFunction, 1 - h[Expand[-a]], -1 - h[Expand[-a]]],
   True, (-1)^(k + 1) Derivative[k][h][Expand[-a]]];

simplifyStatisticalFactors[e_] := e /. {Derivative[k_][h : FermiFunction | BoseFunction][a_] :> periodicStatistics[h, k, a],
    (h : FermiFunction | BoseFunction)[a_] :> periodicStatistics[h, 0, a]};

(* ---- terms, poles and Laurent coefficients of a rational function of z ---- *)

(* the terms of a sum (a product: itself); {base, exponent} of a factor *)
productTermsOf[x_Plus] := List @@ x;
productTermsOf[x_] := {x};
baseAndExponent[p_] := If[Head[p] === Power, {p[[1]], p[[2]]}, {p, 1}];
(* the terms of e, as a list of products: products and positive powers of sums that contain z are multiplied out,
   factors free of z are kept as they are *)
distributeLists[lists_List] := Fold[Flatten[Table[a b, {a, #1}, {b, #2}]] &, {1}, lists];
termsInVariable[e_Plus, z_] := Join @@ (termsInVariable[#, z] & /@ (List @@ e));
termsInVariable[e_Times, z_] := With[{free = Times @@ Select[List @@ e, FreeQ[#, z] &]},
   (free #) & /@ distributeLists[termsInVariable[#, z] & /@ Select[List @@ e, ! FreeQ[#, z] &]]];
termsInVariable[Power[b_Plus, n_Integer?Positive], z_] /; ! FreeQ[b, z] := distributeLists[Table[termsInVariable[b, z], {n}]];
termsInVariable[e_, z_] := {e};

(* a summand that is not split into poles (a denominator of degree three or more that does not factor, a function of z
   that is not rational) makes the sum, or the imaginary part, fail: the flag is set with the message *)
$poleFailure = False;
(* denominators of degree two (boson propagators) are split into linear factors, by Factor or by the roots *)
MatsubaraSum::degree = "The denominator `1` has degree `2` in `3`: only linear and quadratic denominators are split into poles.";
quadraticFactors[b_, z_] := Module[{x = Expand[b], c, s},
   If[Exponent[x, z] != 2, Message[MatsubaraSum::degree, b, Exponent[x, z], z]; $poleFailure = True; b,
    c = Table[Coefficient[x, z, j], {j, 0, 2}]; s = Sqrt[c[[2]]^2 - 4 c[[3]] c[[1]]];
    c[[3]] (z - (-c[[2]] + s)/(2 c[[3]])) (z - (-c[[2]] - s)/(2 c[[3]]))]];
(* a linear factor is written c (z - root), so that its poles give the denominators of the textbooks *)
linearFactor[b_, z_] := With[{x = Expand[b]}, With[{c = Coefficient[x, z]}, c (z - Expand[-(x /. z -> 0)/c])]];
linearFactorsOf[b_, z_] := Module[{f = Factor[b]},
   Times @@ Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
      Which[FreeQ[base, z], base, Exponent[Expand[base], z] <= 1, linearFactor[base, z], True, quadraticFactors[base, z]]^n],
     {p, factorList[f]}]];
linearizeDenominators[t_, z_] := t /. Power[b_, m_Integer?Negative] /; ! FreeQ[b, z] && Exponent[Expand[b], z] > 1 :>
    linearFactorsOf[b, z]^m;

(* the poles of a product t: {root, positions in factorList[t] of the factors 1/(c z + d)^m with root -d/c} *)
MatsubaraSum::pole = "The factor `1` contains `2` in a way that is not a power of a linear function: it is ignored in the search of poles.";
polesOf[t_, z_] := Module[{factors = factorList[t], found = {}},
   Do[With[{f = factors[[i]]},
     If[! FreeQ[f, z],
      If[Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] < 0 && Exponent[Expand[f[[1]]], z] == 1 &&
        FreeQ[Coefficient[Expand[f[[1]]], z], z],
       With[{b = Expand[f[[1]]]}, AppendTo[found, {Expand[-(b /. z -> 0)/Coefficient[b, z]], i}]],
       If[! (Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] > 0) && Head[f] =!= Plus && f =!= z,
        Message[MatsubaraSum::pole, f, z]; $poleFailure = True]]]], {i, Length[factors]}];
   ({#[[1, 1]], #[[All, 2]]} &) /@ GatherBy[found, First]];

(* the coefficient of 1/(z - root)^k in the Laurent expansion of t h(z) at a pole: the factors with that root are
   replaced by c psi, psi^M cancels, and the (M - k)-th derivative of what is left is taken at the root *)
laurentCoefficient[t_, z_, {root_, positions_List}, h_, k_Integer] := Module[{factors = factorList[t], psi, m, phi},
   m = poleOrder[factors, positions];
   phi = psi^m Times @@ Table[If[MemberQ[positions, i], (Coefficient[Expand[factors[[i, 1]]], z] psi)^factors[[i, 2]], factors[[i]]],
       {i, Length[factors]}];
   If[k > m, 0, (If[m == k, phi h, D[phi h, {z, m - k}]] /. z -> root)/(m - k)!]];
(* the order of a pole: the total power of its factors *)
poleOrder[factors_List, positions_List] := Total[Table[-factors[[i, 2]], {i, positions}]];
(* the principal parts of t at its poles in z: {root, k, coefficient of 1/(z - root)^k} for k = 1 ... the order *)
laurentTerms[t_, z_] := Join @@ Table[Table[{pole[[1]], k, laurentCoefficient[t, z, pole, 1, k]}, {k, poleOrder[factorList[t], pole[[2]]]}],
    {pole, polesOf[t, z]}];

(* zero, using the relations between statistical factors: n_F and n_B written with e^(beta e); approximate numbers close
   to zero are zero *)
thermalZeroQ[e_] := With[{x = Together[thermalExponentialForm[e]]}, x === 0 || Chop[x] === 0];
exponentialOf[a_] := With[{x = Expand[a]},
   Times @@ Table[thermalVariable[t/numericFactor[t]]^numericFactor[t], {t, If[Head[x] === Plus, List @@ x, If[x === 0, {}, {x}]]}]];
thermalFunctionInExponentials[h_, k_, a_] := Module[{x},
   Nest[inverseTemperature x D[#, x] &, If[h === FermiFunction, 1/(x + 1), 1/(x - 1)], k] /. x -> exponentialOf[a]];
thermalExponentialForm[e_] := e /. {Derivative[k_][h : FermiFunction | BoseFunction][a_] :> thermalFunctionInExponentials[h, k, a],
    (h : FermiFunction | BoseFunction)[a_] :> thermalFunctionInExponentials[h, 0, a]};

(* ---- the results written as in the textbooks ----
   Terms with the same denominator (up to its sign) are put together, n_F(a)/(z - a + b) + n_F(b)/(-z + a - b) =
   (n_F(a) - n_F(b))/(z - a + b). In their numerators, the statistical factors of a sum of energies that also occur
   alone, such as n_B(e1 + e2) = n_F(e1) n_F(e2)/(1 - n_F(e1) - n_F(e2)) after two fermionic sums, are written with
   those; and a polynomial in the factors n_i of the single energies is written as a sum over processes, each line
   emitted (1 - n_F, 1 + n_B) or absorbed (n_F, n_B), when this has fewer terms:
     N = Sum_S N(S) Prod_(i in S) n_i Prod_(i not in S) (1 - n_i)     (fermions; for bosons -n_i and 1 + n_i)
   with N(S) the value of N at n_i = 1 (bosons: -1) for i in S and n_i = 0 otherwise, as for
   n_2 (1 - n_1) (1 - n_3) + n_1 n_3 (1 - n_2) in the second-order self-energy. *)
energyTerms[a_] := With[{x = Expand[a]}, Which[x === 0, {}, Head[x] === Plus, List @@ x, True, {x}]];
singleEnergyQ[a_] := With[{t = energyTerms[a]}, Length[t] == 1 && numericFactor[t[[1]]] === 1];
singleStatisticalFactors[e_] := Union[Cases[{e}, (h : FermiFunction | BoseFunction)[a_?singleEnergyQ] :> h[a], Infinity]];
(* a factor of a sum of single energies, written with their factors (x = e^(e/T) = 1/n_F - 1 = 1 + 1/n_B) *)
compositeStatisticsRules[factors_List] := With[{energies = Table[f[[1]], {f, factors}],
    exponentials = Table[thermalVariable[f[[1]]] -> If[Head[f] === FermiFunction, 1/f - 1, 1 + 1/f], {f, factors}]},
   {(h : FermiFunction | BoseFunction)[a_] /; ! singleEnergyQ[a] &&
       Complement[Table[t/numericFactor[t], {t, energyTerms[a]}], energies] === {} :>
     (thermalFunctionInExponentials[h, 0, a] /. exponentials)}];
(* otherwise the expression is factored, when it is small *)
factoredForm[e_] := If[LeafCount[e] < 400, Factor[e], e];
textbookStatistics[e_] := Module[{factors = singleStatisticalFactors[e], f, vars, poly, signs, values, monomials, groups},
   Which[
    factors === {} || Length[factors] > 6 || Length[Union[Table[g[[1]], {g, factors}]]] < Length[factors], factoredForm[e],
    f = Together[e /. compositeStatisticsRules[factors]]; ! FreeQ[Denominator[f], FermiFunction | BoseFunction], factoredForm[e],
    True,
    vars = Table[Unique["FunctionalDiagrams`Private`occupation"], {Length[factors]}];
    poly = Expand[Numerator[f] /. Thread[factors -> vars]];
    signs = Table[If[Head[g] === FermiFunction, 1, -1], {g, factors}];
    values = Select[Table[{s, Together[(poly /. Thread[vars -> s signs])/Denominator[f]]}, {s, Tuples[{0, 1}, Length[factors]]}],
      #[[2]] =!= 0 &];
    monomials = Union[Table[Times @@ Select[factorList[t], MemberQ[vars, #] &], {t, productTermsOf[poly]}]];
    If[! AllTrue[vars, Exponent[poly, #] <= 1 &] || 2 Length[values] > Length[monomials], factoredForm[f],
     (* the processes with the same coefficient (up to its sign) together *)
     groups = GatherBy[values, Sort[{Expand[#[[2]]], Expand[-#[[2]]]}] &];
     Total[Table[With[{c = factoredForm[gr[[1, 2]]]}, c Total[Table[
          Together[v[[2]]/gr[[1, 2]]] Times @@ Table[With[{n = factors[[i]]}, If[v[[1, i]] == 1, signs[[i]] n, 1 - signs[[i]] n]],
            {i, Length[factors]}], {v, gr}]]], {gr, groups}]]]]];

(* the sign that makes a denominator i w + ... (its frequency term, if it has one) or else its leading term positive *)
denominatorSign[b_] := With[{x = Expand[b]}, With[{frequency = Select[energyTerms[x], Re[numericFactor[#]] == 0 &]},
    With[{c = numericFactor[If[frequency =!= {}, First[frequency], leadingTerm[x]]]}, If[Re[c] != 0, Sign[Re[c]], If[Im[c] < 0, -1, 1]]]]];
(* a term as {numerator, denominator}, the denominator a product of sums with a positive leading term *)
numeratorDenominator[t_] := Module[{num = 1, den = 1},
   Do[If[Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] < 0 && Head[f[[1]]] === Plus,
     With[{s = denominatorSign[f[[1]]]}, num = num s^f[[2]]; den = den Expand[s f[[1]]]^(-f[[2]])], num = num f], {f, factorList[t]}];
   {num, den}];
collectDenominators[e_, numeratorForm_] := With[{pairs = numeratorDenominator /@ productTermsOf[e]},
   Total[(numeratorForm[Total[#[[All, 1]]]]/#[[1, 2]] &) /@ GatherBy[pairs, Last]]];

(* ---- the Matsubara sum ----
   T Sum_n g(i w_n) e^(i eta w_n 0+) = Sum_poles Res[g(z) n_F(z)] - (1 - eta)/2 c        (fermions)
   T Sum_n g(i nu_n) e^(i eta nu_n 0+) = -Sum_poles Res[g(z) n_B(z)] - (1 - eta)/2 c     (bosons)
   with c = lim z g(z) = Sum_poles Res[g]: n_F(z) has poles at i w_n with residue -T, n_B(z) at i nu_n with residue T,
   and the contour is closed around the poles of g. eta = 1 (e^(i w 0+)), -1, or 0 (symmetric sum). *)
MatsubaraSum::conv = "The summand decays only like 1/`1` at large frequency, and the sum depends on the convergence factor: the symmetric sum is returned (\"ConvergenceFactor\" -> 1 for e^(i w 0+), -1 for e^(-i w 0+)).";
MatsubaraSum::axis = "The summand has a pole at `1` = `2`, on the axis of the Matsubara frequencies: the sum is not defined (an anomalous term).";
MatsubaraSum::free = "The summand does not depend on `1`: the sum diverges.";
MatsubaraSum::diverge = "The summand does not decay at large `1`: the sum diverges.";
MatsubaraSum::factor = "\"ConvergenceFactor\" -> `1` is not one of 1 (e^(i w 0+)), -1 (e^(-i w 0+)), 0 (symmetric sum) and Automatic.";
MatsubaraSum::variable = "The variable of the sum, `1`, is not a symbol.";
EvaluateMatsubara::conv = "The sum over the loop frequency `1` depends on the convergence factor (its summand decays only like 1/z at large frequency), and no line at equal times gives one: the symmetric sum is taken. A boson line that tends to a constant at large frequency should be split, its constant given as an \"Instantaneous\" line.";
EvaluateMatsubara::diverge = "The summand of the loop frequency `1` does not decay at large frequency: the sum diverges (a boson line that does not vanish at large frequency?).";
EvaluateMatsubara::convergence = "\"ConvergenceFactor\" -> `1` is not one of 1 (e^(i w 0+) on the lines at equal times, G(0-) = <psibar psi>, the default), -1 (e^(-i w 0+), G(0+) = -<psi psibar>) and 0 (no factor, the symmetric sum, the mean of the two).";

Options[MatsubaraSum] = {"ConvergenceFactor" -> Automatic};
MatsubaraSum[f_, z_Symbol, statistics_, opts___Rule] := With[{eta = optionValue[callOptions[MatsubaraSum, {opts}], {}, "ConvergenceFactor"]},
   Which[
    ! MemberQ[{"Fermion", "Boson"}, statistics], Message[MatsubaraSum::statistics, statistics]; $Failed,
    ! MemberQ[{Automatic, 1, -1, 0}, eta], Message[MatsubaraSum::factor, eta]; $Failed,
    FreeQ[f, z] && f =!= 0, Message[MatsubaraSum::free, z]; $Failed,
    True, withFrequencySymbols[f, matsubaraSum[#1, z, statistics, eta, MatsubaraSum, z, #2] &]]];
MatsubaraSum[f_, z_, statistics_, opts___Rule] /; Head[z] =!= Symbol := (Message[MatsubaraSum::variable, z]; $Failed);

(* the degree in z of a product, and its leading coefficient *)
degreeInVariable[t_, z_] := Total[Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
     If[FreeQ[base, z], 0, n Exponent[Expand[base], z]]], {p, factorList[t]}]];
leadingCoefficientIn[t_, z_] := Times @@ Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
     If[FreeQ[base, z], p, With[{x = Expand[base]}, Coefficient[x, z, Exponent[x, z]]^n]]], {p, factorList[t]}];
(* the degree of a sum, after its leading terms cancel *)
degreeOfSum[e_, z_] := With[{f = Together[e]}, With[{num = Expand[Numerator[f]]},
    Module[{k = Exponent[num, z]}, While[k >= 0 && thermalZeroQ[Coefficient[num, z, k]], k--]; k - Exponent[Denominator[f], z]]]];
(* how a sum of products decays at large z: like z^d, d = -2 standing for 1/z^2 or faster (the sum converges and does not
   depend on the convergence factor). It is read from the degrees of the terms; those of degree -1 decay like 1/z unless
   their leading coefficients cancel, and those that do not decay are put together, as they may cancel at leading order.
   (The value of the residues at infinity is not used: with numbers, its rounding error would look like a 1/z tail.) *)
decayDegree[terms_List, z_] := Module[{degrees = degreeInVariable[#, z] & /@ terms, top, d},
   top = Max[Append[Select[degrees, NumberQ], -2]];
   Which[
    top <= -2, -2,
    top == -1, If[thermalZeroQ[Total[Table[If[TrueQ[degrees[[i]] == -1], leadingCoefficientIn[terms[[i]], z], 0], {i, Length[terms]}]]], -2, -1],
    True, d = degreeOfSum[Total[Table[If[TrueQ[degrees[[i]] >= -1], terms[[i]], 0], {i, Length[terms]}]], z];
    Which[TrueQ[d >= 0], d, TrueQ[d == -1], -1, True, -2]]];

(* ---- sums of logarithms (the traces Tr ln of section 20) ----
   T Sum_n e^(i eta w_n 0+) ln(a - i w_n) = -T ln(1 - n_F(a)) + (1 - eta)/2 a = T ln(1 + e^(-a/T)) + (1 - eta)/2 a  (fermions)
   T Sum_n e^(i eta v_n 0+) ln(a - i v_n) = -T ln(1 + n_B(a)) + (1 - eta)/2 a = T ln(1 - e^(-a/T)) + (1 - eta)/2 a  (bosons)
   Their derivative in a is -MatsubaraSum[1/(z - a)] with the same convergence factor, and the constant is fixed by
   a -> +Infinity (an empty level, Z = 1). A constant factor in the logarithm gives nothing: T Sum_n e^(+-i w_n 0+) = 0,
   and the symmetric sum of 1 vanishes too (in zeta regularization the number of Matsubara frequencies is zero): the
   trace of the identity is zero. A rational argument is split into the linear factors of its numerator (zeros) and
   denominator (poles). A factor written ln(-a + i w_n) instead, i.e. with w_n -> -w_n (which reverses eta), gives the
   same for fermions and differs by i Pi T for bosons: the form whose energy has a positive leading sign is used, so that
   1/2 Tr ln of a real boson, ln(W^2 - z^2), gives the real W/2 + T ln(1 - e^(-W/T)) with every convergence factor. *)
MatsubaraSum::log = "The term `1` contains a logarithm of `2` that is not of the form c Log[r], with c free of `2` and r a rational function of `2`.";
statisticalLog[a_, "Fermion"] := -Temperature Log[1 - FermiFunction[a]];
statisticalLog[a_, "Boson"] := -Temperature Log[1 + BoseFunction[a]];
logSumOfEnergy[a_, statistics_, eta_] := If[leadingSign[a] < 0,
   statisticalLog[-a, statistics] - (1 + eta)/2 a,
   statisticalLog[a, statistics] + (1 - eta)/2 a];
(* the roots {root, multiplicity} of a polynomial in z, of degree two at most in each factor *)
polynomialRoots[p_, z_] := Join @@ Table[With[{base = Expand[baseAndExponent[f][[1]]], m = baseAndExponent[f][[2]]},
     Which[
      FreeQ[base, z], {},
      Exponent[base, z] == 1, {{Expand[-(base /. z -> 0)/Coefficient[base, z]], m}},
      Exponent[base, z] == 2, With[{c = Table[Coefficient[base, z, j], {j, 0, 2}]}, With[{s = Sqrt[c[[2]]^2 - 4 c[[3]] c[[1]]]},
         {{Expand[(-c[[2]] + s)/(2 c[[3]])], m}, {Expand[(-c[[2]] - s)/(2 c[[3]])], m}}]],
      True, Message[MatsubaraSum::degree, base, Exponent[base, z], z]; $poleFailure = True; {}]],
    {f, factorList[Factor[p]]}];
(* T Sum c Log[r(z)]: the zeros of r count +1, its poles -1. The convergence factor matters when the sum of the zeros
   differs from that of the poles (ln r decays like 1/z only); Automatic then gives the message and the symmetric sum *)
logSum[c_, r_, z_, statistics_, eta0_, caller_, name_] := Module[{f = Together[r], zeros, poles, tail, eta = eta0},
   zeros = polynomialRoots[Numerator[f], z]; poles = polynomialRoots[Denominator[f], z];
   tail = Total[(#[[1]] #[[2]] &) /@ zeros] - Total[(#[[1]] #[[2]] &) /@ poles];
   If[eta === Automatic, If[thermalZeroQ[tail], eta = 1,
     If[caller === MatsubaraSum, Message[MatsubaraSum::conv, name], Message[EvaluateMatsubara::conv, name]]; eta = 0]];
   c (Total[(#[[2]] logSumOfEnergy[#[[1]], statistics, eta] &) /@ zeros] - Total[(#[[2]] logSumOfEnergy[#[[1]], statistics, eta] &) /@ poles])];
logTermQ[t_, z_] := ! FreeQ[t, Log[a_] /; ! FreeQ[a, z]];
splitLogTerm[t_, z_] := With[{dependent = Select[factorList[t], ! FreeQ[#, z] &]},
   If[Length[dependent] == 1 && Head[dependent[[1]]] === Log && FreeQ[dependent[[1, 1]], Log],
    {Times @@ Select[factorList[t], FreeQ[#, z] &], dependent[[1, 1]]}, None]];
matsubaraSum[f_, z_, statistics_, eta0_, caller_, name_, shown_List] /; logTermQ[f, z] := Block[{$poleFailure = False},
   Module[{terms = termsInVariable[f, z], logs, rest, split, logPart, restPart},
    logs = Select[terms, logTermQ[#, z] &]; rest = Select[terms, ! logTermQ[#, z] &];
    split = splitLogTerm[#, z] & /@ logs;
    If[MemberQ[split, None],
     Message[MatsubaraSum::log, logs[[First[First[Position[split, None, {1}]]]]] /. shown, z]; $Failed,
     logPart = Total[logSum[#[[1]], #[[2]], z, statistics, eta0, caller, name] & /@ split];
     restPart = If[rest === {}, 0, matsubaraSum[Total[rest], z, statistics, eta0, caller, name, shown]];
     If[$poleFailure || restPart === $Failed, $Failed, Expand[logPart + restPart]]]]];

(* the sum, for MatsubaraSum and EvaluateMatsubara (caller); name is the frequency of the messages and shown the rules
   that write their expressions with the loop frequencies. A summand that does not decay gives $Failed; one that decays
   like 1/z gives the term of the convergence factor, with c = lim z g(z) the sum of the residues of g *)
matsubaraSum[f_, z_, statistics_, eta0_, caller_, name_, shown_List] := Block[{$poleFailure = False}, Module[
    {eta = eta0, terms, decay, tail = 0, total = 0, poles, result},
    terms = DeleteCases[linearizeDenominators[#, z] & /@ termsInVariable[f, z], 0];
    decay = decayDegree[terms, z];
    If[decay >= 0,
     If[caller === MatsubaraSum, Message[MatsubaraSum::diverge, name], Message[EvaluateMatsubara::diverge, name]]; $Failed,
     Do[poles = polesOf[t, z];
      (* a pole on the axis of the frequencies summed: i w_n (fermions), i nu_n (bosons); a pole at a frequency of the
         other statistics is an ordinary pole, n_F(i nu_m) = n_F(0) = 1/2, n_B(i w_n) = -n_F(0) = -1/2 *)
      Scan[With[{shift = matsubaraShift[#[[1]]]}, If[Expand[shift[[1]]] === 0 && shift[[2]] == If[statistics === "Fermion", 1, 0],
          Message[MatsubaraSum::axis, If[caller === MatsubaraSum, z, I name], #[[1]] /. shown]]] &, poles];
      total += Total[laurentCoefficient[t, z, #, statisticalFunction[statistics][z], 1] & /@ poles];
      If[decay == -1, tail += Total[laurentCoefficient[t, z, #, 1, 1] & /@ poles]], {t, terms}];
     If[decay == -1 && eta === Automatic,
      If[caller === MatsubaraSum, Message[MatsubaraSum::conv, name], Message[EvaluateMatsubara::conv, name]]; eta = 0];
     result = collectDenominators[Expand[simplifyStatisticalFactors[If[statistics === "Fermion", total, -total] -
          If[decay == -1, (1 - eta)/2 tail, 0]]], Identity];
     If[$poleFailure, $Failed, result]]]];

(* ---- routing of momenta and frequencies through a diagram ---- *)

EvaluateMatsubara::rule = "`1` is not a line rule: use head -> {\"Fermion\", dispersion}, head -> {\"Instantaneous\", potential} or head -> {\"Boson\", Function[{q, z}, propagator]} (z = i nu).";
EvaluateMatsubara::factor = "The factor `1` is neither a line of the rules, a delta function nor a constant.";
EvaluateMatsubara::grassmann = "A term contains Grassmann-odd factors `1`: only diagrams with the sources set to zero are evaluated.";
EvaluateMatsubara::conservation = "The external momenta and frequencies do not add up to zero on a connected part of a diagram (`1`).";
EvaluateMatsubara::frequency = "The external frequency `1` has no statistics: declare it with DeclareMatsubaraFrequency.";
(* a rule head -> {kind, value} with one of the kinds; the others give the message caller::rule *)
lineRuleQ[r_, kinds_List] := MatchQ[r, (Rule | RuleDelayed)[_Symbol, {_String, _}]] && MemberQ[kinds, r[[2, 1]]];
checkRuleKinds[rules_List, kinds_List, caller_Symbol] := With[{bad = Select[rules, ! lineRuleQ[#, kinds] &]},
   Scan[Message[MessageName[caller, "rule"], #] &, bad]; bad === {}];
checkLineRules[rules_List] := checkRuleKinds[rules, {"Fermion", "Instantaneous", "Boson"}, EvaluateMatsubara];
checkExternalFrequencies[externals_List] := With[{bad = Select[Union[Cases[Table[e[[2, 2]], {e, externals}], _Symbol, {0, Infinity}]],
      Context[#] =!= "System`" && frequencyStatistics[#] === None &]},
   Scan[Message[EvaluateMatsubara::frequency, #] &, bad]; bad === {}];
(* each external point is label -> {momentum, frequency}, the frequency I times an integer combination of declared
   Matsubara frequencies (or 0) *)
EvaluateMatsubara::external = "`1` is not an external point label -> {momentum, I w}, with w a Matsubara frequency declared by DeclareMatsubaraFrequency (or an integer combination of them, or 0).";
EvaluateMatsubara::loopnames = "\"LoopMomenta\" -> `1` is neither Automatic nor a list of names.";
EvaluateMatsubara::loops = "\"LoopMomenta\" -> `1` gives fewer names than the `2` loops of a diagram.";
matsubaraCombinationQ[f_] := With[{x = Expand[f], ws = Union[Cases[{f}, s_Symbol /; Context[s] =!= "System`", Infinity]]},
   Expand[x /. Table[s -> 0, {s, ws}]] === 0 && AllTrue[ws, IntegerQ[Coefficient[x, #]/I] &]];
checkExternals[externals_List] := With[{bad = Select[externals, ! MatchQ[#, (Rule | RuleDelayed)[_, {_, _}]] &]},
   Scan[Message[EvaluateMatsubara::external, #] &, bad];
   bad === {} && checkExternalFrequencies[externals] &&
    With[{wrong = Select[externals, ! matsubaraCombinationQ[#[[2, 2]]] &]}, Scan[Message[EvaluateMatsubara::external, #] &, wrong]; wrong === {}]];
checkLoopNames[names_] := If[names === Automatic || ListQ[names], True, Message[EvaluateMatsubara::loopnames, names]; False];

(* the sign of the first term, in canonical order, of a momentum or a frequency *)
firstTermSign[e_] := With[{x = Expand[e]}, Which[x === 0, 0, Head[x] === Plus, signOfTerm[First[x]], True, signOfTerm[x]]];
(* the value of a line with momentum p and frequency nu (i nu is its Matsubara frequency) flowing along it; an
   instantaneous interaction and a boson propagator are even, and are written with the momentum of positive sign *)
flowSign[p_, nu_] := If[Expand[p] =!= 0, firstTermSign[p], If[firstTermSign[nu] < 0, -1, 1]];
lineValue[{"Fermion", disp_}, {p_, nu_}] := 1/(I nu - disp[p]);
lineValue[{"Instantaneous", v_}, {p_, nu_}] := v[Expand[flowSign[p, 0] p]];
lineValue[{"Boson", f_}, {p_, nu_}] := With[{s = flowSign[p, nu]}, f[Expand[s p], Expand[I s nu]]];

(* A term of the package as a graph: points (labels) joined by delta functions are one node; lines go from their second
   to their first argument. Spanning tree (instantaneous and boson lines first), a loop momentum and frequency on each
   chord, and the flows of the tree lines from conservation at the nodes, peeling the leaves of the tree.
   Result: rules with the data of the routed term. The messages are those of caller (EvaluateMatsubara by default):
   caller::grassmann, ::factor, ::loops and ::conservation. *)
routeTerm[t : diagramTerm[c_, o_, d_], externals_List, rules_List, loopNames_, caller_Symbol: EvaluateMatsubara] := Catch[Module[
    {heads = Table[r[[1]], {r, rules}], kindOf, labels, factors, lines = {}, deltas = {}, constants = {}, points, index,
     components, nodeOf, nNodes, extFlow, comp, chords = {}, tree = {}, flows, remaining, u, e, net, roots, nLoops,
     loopMomenta, loopFrequencies, times, timeOf, lineData, equalTime},
    If[o =!= {}, Message[MessageName[caller, "grassmann"], o]; Throw[$Failed, routeFailure]];
    kindOf[h_] := First[Select[rules, #[[1]] === h &]][[2]];
    labels = Join[Table[x[[1]], {x, externals}], d];
    factors = expandIntegerPowers[factorList[c]];
    Do[Which[
      ! AtomQ[f] && MemberQ[heads, Head[f]] && Length[f] == 2, AppendTo[lines, {f[[2]], f[[1]], Head[f]}],
      Head[f] === DeltaFunction && Length[f] == 2, AppendTo[deltas, List @@ f],
      FreeQ[f, Alternatives @@ labels], AppendTo[constants, f],
      True, Message[MessageName[caller, "factor"], f]; Throw[$Failed, routeFailure]], {f, factors}];
    (* nodes *)
    points = Union[Table[x[[1]], {x, externals}], Flatten[Table[{l[[1]], l[[2]]}, {l, lines}]], Flatten[deltas]];
    index[x_] := First[First[Position[points, x, {1}]]];
    components = graphComponents[Length[points], Map[index, deltas, {2}]];
    nNodes = Length[components];
    nodeOf[x_] := First[First[Position[components, index[x]]]];
    lineData = Table[{nodeOf[l[[1]]], nodeOf[l[[2]]], l[[3]], kindOf[l[[3]]]}, {l, lines}];
    extFlow = Table[{0, 0}, {nNodes}];
    Do[With[{n = nodeOf[x[[1]]]}, extFlow[[n]] = extFlow[[n]] + {x[[2, 1]], x[[2, 2]]/I}], {x, externals}];
    (* spanning tree *)
    comp = Range[nNodes];
    Do[With[{a = comp[[lineData[[j, 1]]]], b = comp[[lineData[[j, 2]]]]},
      If[a =!= b, AppendTo[tree, j]; comp = comp /. b -> a, AppendTo[chords, j]]],
     {j, Join[Select[Range[Length[lines]], lineData[[#, 4, 1]] =!= "Fermion" &], Select[Range[Length[lines]], lineData[[#, 4, 1]] === "Fermion" &]]}];
    nLoops = Length[chords];
    If[ListQ[loopNames] && Length[loopNames] < nLoops, Message[MessageName[caller, "loops"], loopNames, nLoops]; Throw[$Failed, routeFailure]];
    loopMomenta = If[ListQ[loopNames], Take[loopNames, nLoops], Table[LoopMomentum[i], {i, nLoops}]];
    loopFrequencies = Table[LoopFrequency[i, If[lineData[[chords[[i]], 4, 1]] === "Fermion", "Fermion", "Boson"]], {i, nLoops}];
    flows = Table[None, {Length[lines]}];
    Do[flows[[chords[[i]]]] = {loopMomenta[[i]], loopFrequencies[[i]]}, {i, nLoops}];
    (* conservation on each connected part *)
    roots = Table[First[Select[Range[nNodes], comp[[#]] === r &]], {r, Union[comp]}];
    Do[If[Expand[Total[Table[extFlow[[n]], {n, Select[Range[nNodes], comp[[#]] === comp[[r]] &]}]]] =!= {0, 0},
      Message[MessageName[caller, "conservation"], termToExpression[t]]; Throw[$Failed, routeFailure]], {r, roots}];
    remaining = tree;
    While[remaining =!= {},
     u = First[Select[Complement[Range[nNodes], roots],
        Count[remaining, j_ /; lineData[[j, 1]] == # || lineData[[j, 2]] == #] == 1 &]];
     e = First[Select[remaining, lineData[[#, 1]] == u || lineData[[#, 2]] == u &]];
     net = extFlow[[u]] + Total[Table[If[flows[[j]] === None, 0,
          If[lineData[[j, 2]] == u, flows[[j]], 0] - If[lineData[[j, 1]] == u, flows[[j]], 0]], {j, Length[lines]}]];
     flows[[e]] = Expand[If[lineData[[e, 2]] == u, -net, net]];
     remaining = DeleteCases[remaining, e]];
    (* time vertices: nodes joined by instantaneous interactions are at equal times *)
    times = graphComponents[nNodes, Table[{l[[1]], l[[2]]}, {l, Select[lineData, #[[4, 1]] === "Instantaneous" &]}]];
    timeOf = Table[First[First[Position[times, n]]], {n, nNodes}];
    equalTime = Select[Range[Length[lines]], lineData[[#, 4, 1]] === "Fermion" && timeOf[[lineData[[#, 1]]]] == timeOf[[lineData[[#, 2]]]] &];
    {"Coefficient" -> Times @@ constants, "ClosedLoops" -> closedLoopCountOfTerm[t, Select[heads, kindOf[#][[1]] === "Fermion" &]],
     "Integrand" -> Times @@ Table[lineValue[lineData[[j, 4]], flows[[j]]], {j, Length[lines]}],
     "LoopMomenta" -> loopMomenta, "LoopFrequencies" -> loopFrequencies,
     "EqualTimeFrequency" -> Expand[Total[Table[flows[[j, 2]], {j, equalTime}]]],
     (* each line: {its start node, its end node, head, kind, {momentum, frequency}, {start label, end label}} *)
     "Lines" -> Table[Join[lineData[[j]], {flows[[j]], Take[lines[[j]], 2]}], {j, Length[lines]}], "TimeOf" -> timeOf,
     "ExternalNodes" -> Table[x[[1]] -> nodeOf[x[[1]]], {x, externals}]}], routeFailure];

routeData[key_String, data_List] := key /. data;
(* the factors that do not depend on the loop momenta are taken out of the momentum sum: those of a product, and those
   common to all the terms of a sum (with the sign, when all the terms are negative) *)
momentumSum[0, _] := 0;
momentumSum[f_, {}] := f;
momentumSum[f_Plus, ps_List] := Module[{terms = List @@ f, common},
   common = Select[factorList[First[terms]], ! NumberQ[#] && FreeQ[#, Alternatives @@ ps] &];
   common = Times @@ Select[common, Function[c, AllTrue[Rest[terms], MemberQ[factorList[#], c] &]]];
   If[AllTrue[terms, TrueQ[Re[numericFactor[#]] < 0] &], common = -common];
   common MomentumSum[Total[Table[t/common, {t, terms}]], ps]];
momentumSum[f_, ps_List] := With[{outside = Times @@ Select[factorList[f], FreeQ[#, Alternatives @@ ps] &]},
   outside MomentumSum[f/outside, ps]];

(* the frequencies of the loops are summed one after the other; the convergence factor of each sum comes from the
   lines at equal times that carry its frequency: e^(i eta0 w 0+) for the frequency w along each of them (eta0 the
   option "ConvergenceFactor": 1, -1 or 0). During the sums the loop frequencies are symbols (withFrequencySymbols);
   $Failed if a sum fails *)
sumLoopFrequencies[integrand_, frequencies_List, exponent_, eta0_] := withFrequencySymbols[{integrand, frequencies, exponent},
   Function[{data, shown}, Module[{g = data[[1]], z},
     Catch[Do[With[{f = data[[2, i]], sign = Sign[Coefficient[data[[3]], data[[2, i]]]]},
        g = matsubaraSum[g /. f -> -I z, z, frequencyStatistics[f], If[sign == 0, Automatic, eta0 sign], EvaluateMatsubara, frequencies[[i]], shown];
        If[g === $Failed, Throw[$Failed, sumFailure]]], {i, Length[frequencies]}];
      g, sumFailure]]]];

(* the options (resolved by callOptions): {spin, loop names, convergence factor}; DiagramIntegrand sums no frequency and has
   no convergence factor. The routed terms: $Failed if the rules, the external points or the options are not valid *)
Options[DiagramIntegrand] = {"SpinDegeneracy" -> 1, "LoopMomenta" -> Automatic};
Options[EvaluateMatsubara] = {"SpinDegeneracy" -> 1, "LoopMomenta" -> Automatic, "ConvergenceFactor" -> 1, "Limit" -> None};
Options[CuttingRuleImaginaryPart] = {"SpinDegeneracy" -> 1, "LoopMomenta" -> Automatic, "ConvergenceFactor" -> 1};
Options[CheckCuttingRules] = Options[CuttingRuleImaginaryPart];
matsubaraOptions[opts_List] := {optionValue[opts, {"SpinDegeneracy" -> 1}, "SpinDegeneracy"],
   optionValue[opts, {"LoopMomenta" -> Automatic}, "LoopMomenta"], optionValue[opts, {"ConvergenceFactor" -> 1}, "ConvergenceFactor"]};
checkConvergenceFactor[eta_] := If[MemberQ[{1, -1, 0}, eta], True, Message[EvaluateMatsubara::convergence, eta]; False];
(* the lines of the package in momentum and frequency are directed, 1/(i w - xi_k), or bosonic: an antisymmetric two-label
   kernel (an anomalous propagator -<T psi psi>, a superfield propagator) is refused rather than misread *)
EvaluateMatsubara::anomalous = "The antisymmetric kernels `1` (anomalous or superfield propagators) cannot be evaluated in momentum and frequency yet: write the expression with the normal propagators (SuperfieldComponents at zero anomalous propagators).";
anomalousLinesFree[e_] := With[{heads = Select[Union[Cases[{e}, h_Symbol[_, _] :> h, Infinity]], MemberQ[$antisymmetricHeads, #] &]},
   If[heads === {}, True, Message[EvaluateMatsubara::anomalous, heads]; False]];
routedTerms[e_, externals_List, rules_List, loopNames_, eta_: 1] := If[! anomalousLinesFree[e] || ! checkLineRules[rules] || ! checkExternals[externals] ||
    ! checkLoopNames[loopNames] || ! checkConvergenceFactor[eta], $Failed, routeTerm[#, externals, rules, loopNames] & /@ canonicalTermsOf[e]];

(* the weight of a routed term: its coefficient and the spin factor of its closed fermion loops *)
weightOf[data_List, spin_] := routeData["Coefficient", data] spin^routeData["ClosedLoops", data];
(* the integrand after the frequency sums *)
summedIntegrand[data_List, eta_] := sumLoopFrequencies[routeData["Integrand", data], routeData["LoopFrequencies", data],
   routeData["EqualTimeFrequency", data], eta];

DiagramIntegrand[e_, externals_List, rules_List, opts___Rule] := Module[{spin, names, routed},
   {spin, names} = Take[matsubaraOptions[callOptions[DiagramIntegrand, {opts}]], 2];
   routed = routedTerms[e, externals, rules, names];
   If[routed === $Failed, $Failed,
    If[# === $Failed, $Failed, {"Weight" -> weightOf[#, spin],
        "Integrand" -> routeData["Integrand", #], "LoopMomenta" -> routeData["LoopMomenta", #],
        "LoopFrequencies" -> routeData["LoopFrequencies", #], "EqualTimeFrequency" -> routeData["EqualTimeFrequency", #]}] & /@ routed]];

(* partial fractions in the external frequency w (a function of one external frequency): a term that vanishes at large
   z = I w is replaced by the principal parts at its poles, (n_B + 1 - n_F)/(I w - xi - Omega) + ... (a term whose
   denominators are not split into poles is kept as it is) *)
principalParts[t_, z_] := Total[Table[c[[3]]/linearFactor[z - c[[1]], z]^c[[2]], {c, laurentTerms[t, z]}]];
properInQ[t_, z_] := Exponent[Numerator[t], z] < Exponent[Denominator[t], z];
externalPartialFractions[e_, None] := e;
externalPartialFractions[e_, w_] := Module[{z},
   Total[Table[If[FreeQ[t, z] || ! properInQ[t, z], t,
       Block[{$poleFailure = False}, With[{parts = principalParts[linearizeDenominators[t, z], z]}, If[$poleFailure, t, parts]]]],
      {t, termsInVariable[e /. w -> -I z, z]}]] /. z -> I w];
externalFrequency[externals_List] := With[{ws = Union[Cases[Table[x[[2, 2]], {x, externals}], s_Symbol /; Context[s] =!= "System`", {0, Infinity}]]},
   If[Length[ws] == 1, ws[[1]], None]];

(* The parts of a term between the lines that carry the external frequency w and no loop frequency: the external legs of
   a function that is not amputated (G = G0 Sigma G0 + ...), the lines that join its irreducible parts. Partial fractions
   across such a line would evaluate the parts on the poles of one another (the self-energy of G0 Sigma G0 on shell), with
   denominators free of w that vanish there: they are taken in each part, and these lines are kept as factors.
   Result: {the product of these lines, {{integrand, loop frequencies} of each part}}; the lines that carry neither w nor
   a loop frequency go with the first part. An amputated two-point function has no such line: one part, the whole term *)
externalParts[data_List, w_] := Module[{lines = routeData["Lines", data], fs = routeData["LoopFrequencies", data], loopsOf, valueOf,
    joining, inner, constant, components, parts},
   (* the loops whose frequency flows through a line: momenta and frequencies are routed along the same loops *)
   loopsOf[l_] := Select[Range[Length[fs]], ! FreeQ[l[[5, 2]], fs[[#]]] &];
   valueOf[j_] := lineValue[lines[[j, 4]], lines[[j, 5]]];
   joining = If[w === None, {}, Select[Range[Length[lines]], loopsOf[lines[[#]]] === {} && ! FreeQ[Expand[lines[[#, 5, 2]]], w] &]];
   If[joining === {}, {1, {{routeData["Integrand", data], fs}}},
    inner = Select[Range[Length[lines]], loopsOf[lines[[#]]] =!= {} &];
    constant = Complement[Range[Length[lines]], joining, inner];
    components = Sort[Sort /@ graphComponents[Length[fs], Flatten[Table[With[{ls = loopsOf[lines[[j]]]},
         Table[{ls[[1]], m}, {m, Rest[ls]}]], {j, inner}], 1]]];
    parts = Table[{Times @@ (valueOf /@ Select[inner, Intersection[loopsOf[lines[[#]]], c] =!= {} &]), fs[[c]]}, {c, components}];
    If[parts === {}, parts = {{1, {}}}];
    parts[[1, 1]] = parts[[1, 1]] Times @@ (valueOf /@ constant);
    {Times @@ (valueOf /@ joining), parts}]];

(* a routed term after the sums, in partial fractions part by part; $Failed if a sum fails *)
evaluatedTerm[data_List, spin_, w_, eta_] := Module[{parts = externalParts[data, w], sums},
   sums = Table[sumLoopFrequencies[part[[1]], part[[2]], routeData["EqualTimeFrequency", data], eta], {part, parts[[2]]}];
   If[MemberQ[sums, $Failed], $Failed,
    weightOf[data, spin] parts[[1]] momentumSum[Times @@ Table[collectDenominators[externalPartialFractions[s, w], textbookStatistics],
       {s, sums}], routeData["LoopMomenta", data]]]];

(* ---- traces of operators (Tr ln A, Tr A) in a translation-invariant system ----
   An operator made of kernels with line rules is diagonal in momentum and frequency: G0 -> 1/(z - xi[k]) ("Fermion"),
   v[k] ("Instantaneous", e.g. a static self-energy), f[k, z] ("Boson"), with z = i w. Per unit of beta times volume,
   Tr ln A = (2s + 1) Integral_k T Sum_n e^(i eta w_n 0+) ln A(k, i w_n), and Tr A the same with A: the trace is taken at
   equal times, with the convergence factor of the option "ConvergenceFactor" (1, the default: the ordering psibar psi,
   normal order). The frequencies are fermionic when a kernel of A has a "Fermion" rule, bosonic otherwise. In Tr A the
   constant part of A(k, z) is dropped (Tr 1 = 0, as in the sums of logarithms); a part that grows with z diverges. *)
EvaluateMatsubara::trace = "The term `1` is not a number times one trace (TraceLog, OperatorTrace) free of labels.";
EvaluateMatsubara::kernel = "The kernels `1` of the operator `2` have no line rule.";
EvaluateMatsubara::tracegrows = "The trace of `1` diverges: its summand grows at large frequency.";
EvaluateMatsubara::limit = "\"Limit\" -> `1` is not one of None, \"Static\" (all external momenta and frequencies 0) and \"Dynamic\" (external momenta 0, then the external frequency -> 0).";
kernelMomentumValue[{"Fermion", disp_}, k_, z_] := 1/(z - disp[k]);
kernelMomentumValue[{"Instantaneous", v_}, k_, z_] := v[k];
kernelMomentumValue[{"Boson", f_}, k_, z_] := f[k, z];
operatorSymbols[a_] := Union[Cases[a /. Dot -> List, s_Symbol /; Context[s] =!= "System`", {0, Infinity}]];
operatorMomentumValue[a_, rules_List, k_, z_] := (a /. Dot -> Times) /. Table[r[[1]] -> kernelMomentumValue[r[[2]], k, z], {r, rules}];
traceStatistics[a_, rules_List] := If[MemberQ[Table[If[MemberQ[operatorSymbols[a], r[[1]]], r[[2, 1]], None], {r, rules}], "Fermion"],
   "Fermion", "Boson"];
traceValue[tr : (TraceLog | OperatorTrace)[a_], rules_List, spin_, k_, eta_] := Module[{z, missing, value, stat, num, den, quotient, summand, sum},
   missing = Complement[operatorSymbols[a], rules[[All, 1]]];
   Which[
    NumericQ[a], 0,
    missing =!= {}, Message[EvaluateMatsubara::kernel, missing, a]; $Failed,
    True,
    value = Together[operatorMomentumValue[a, rules, k, z]];
    stat = traceStatistics[a, rules];
    If[Head[tr] === TraceLog, summand = Log[value],
     num = Expand[Numerator[value]]; den = Denominator[value];
     quotient = PolynomialQuotient[num, Expand[den], z];
     If[! FreeQ[quotient, z], Message[EvaluateMatsubara::tracegrows, a]; Return[$Failed]];
     summand = Together[PolynomialRemainder[num, Expand[den], z]/den]];
    sum = If[summand === 0 || FreeQ[summand, z], 0, matsubaraSum[summand, z, stat, eta, EvaluateMatsubara, z, {}]];
    If[sum === $Failed, $Failed, spin momentumSum[sum, {k}]]]];
(* the limits of a function of one external momentum q and one external frequency: "Static" sets both to zero (the
   limit q -> 0 at zero frequency, the thermodynamic response: at q = 0 and frequency 0 double poles give n_F'),
   "Dynamic" sets the momenta to zero and takes the frequency to zero afterwards (zero for a conserved quantity) *)
EvaluateMatsubara::limitterms = "The terms of the function at zero momentum that are singular as `1` -> 0 do not cancel as they are written (they may cancel after a change of the loop momenta, as at second order, or the approximation does not conserve the quantity): the function at zero momentum is returned.";
(* the limit w -> 0 of a rational function of w, by l'Hopital's rule; None if it is singular *)
frequencyLimit[e_, w_] := Module[{x = Together[e], num, den, k = 0},
   num = Numerator[x]; den = Denominator[x];
   While[k < 6 && thermalZeroQ[den /. w -> 0],
    If[! thermalZeroQ[num /. w -> 0], Return[None]];
    num = D[num, w]; den = D[den, w]; k++];
   Together[(num /. w -> 0)/(den /. w -> 0)]];
mergedMomentumSums[e_] := Module[{terms = productTermsOf[Expand[e]], sums, other, keys},
   sums = Select[terms, Count[factorList[#], _MomentumSum] == 1 &];
   other = Total[Select[terms, Count[factorList[#], _MomentumSum] != 1 &]];
   sums = Table[With[{m = First[Cases[factorList[t], _MomentumSum]]}, {m[[2]], (t/m) m[[1]]}], {t, sums}];
   keys = Union[sums[[All, 1]]];
   {other, Table[{key, Total[Select[sums, #[[1]] === key &][[All, 2]]]}, {key, keys}]}];
(* the terms are put together by their loop momenta, and the limit is taken in each group *)
dynamicLimit[e_, w_] := Module[{m = mergedMomentumSums[e], outside, groups},
   outside = frequencyLimit[m[[1]], w];
   groups = Table[{s[[1]], frequencyLimit[s[[2]], w]}, {s, m[[2]]}];
   If[outside === None || MemberQ[groups[[All, 2]], None], Message[EvaluateMatsubara::limitterms, w]; e,
    outside + Total[Table[momentumSum[s[[2]], s[[1]]], {s, groups}]]]];

EvaluateMatsubara[e_, externals_List, rules_List, opts___Rule] := evaluateMatsubara[e, externals, rules, callOptions[EvaluateMatsubara, {opts}]];
traceOrLimitQ[e_, opts_List] := ! FreeQ[e, TraceLog | OperatorTrace] || optionValue[opts, {"Limit" -> None}, "Limit"] =!= None;
evaluateMatsubara[e_, externals_List, rules_List, opts_List] /; traceOrLimitQ[e, opts] := Module[
   {limit = optionValue[opts, {"Limit" -> None}, "Limit"], rest = Select[opts, optionKey[#[[1]]] =!= "Limit" &], spin, names, eta,
    terms, traceTerms, diagrams, k, values, diagramValue, w, ext},
   {spin, names, eta} = matsubaraOptions[rest];
   If[! MemberQ[{None, "Static", "Dynamic"}, limit], Message[EvaluateMatsubara::limit, limit]; Return[$Failed]];
   ext = Switch[limit, None, externals, "Static", (#[[1]] -> {0, 0} &) /@ externals, "Dynamic", (#[[1]] -> {0, #[[2, 2]]} &) /@ externals];
   terms = productTermsOf[Expand[e]];
   traceTerms = Select[terms, ! FreeQ[#, TraceLog | OperatorTrace] &];
   diagrams = Total[Select[terms, FreeQ[#, TraceLog | OperatorTrace] &]];
   k = If[ListQ[names] && names =!= {}, First[names], LoopMomentum[1]];
   values = Table[With[{traces = Select[factorList[t], MatchQ[#, _TraceLog | _OperatorTrace] &]},
      If[Length[traces] != 1 || ! FreeQ[t/traces[[1]], TraceLog | OperatorTrace | IntegralOver | DeltaFunction] || externals =!= {},
       Message[EvaluateMatsubara::trace, t]; $Failed,
       With[{v = traceValue[traces[[1]], rules, spin, k, eta]}, If[v === $Failed, $Failed, (t/traces[[1]]) v]]]], {t, traceTerms}];
   diagramValue = If[diagrams === 0, 0, evaluateMatsubara[diagrams, ext, rules, rest]];
   Which[
    MemberQ[Append[values, diagramValue], $Failed], $Failed,
    limit === "Dynamic" && (w = externalFrequency[externals]) =!= None, Total[values] + dynamicLimit[diagramValue, w],
    True, Total[values] + diagramValue]];

(* $Failed if a term fails *)
evaluateMatsubara[e_, externals_List, rules_List, opts_List] := Module[{spin, names, eta, routed, w, results},
   {spin, names, eta} = matsubaraOptions[opts];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed, $Failed,
    w = externalFrequency[externals];
    results = Table[If[data === $Failed, $Failed, evaluatedTerm[data, spin, w, eta]], {data, routed}];
    If[MemberQ[results, $Failed], $Failed, Total[results]]]];

(* ---- analytic continuation and imaginary part ---- *)

AnalyticContinuation::periodicity = "Statistical factors still depend on `1`: declare the statistics of the frequencies (DeclareMatsubaraFrequency) before the sums.";
AnalyticContinuation[e_, w_Symbol -> omega_] := (
   If[! FreeQ[Cases[{e}, (FermiFunction | BoseFunction)[_] | Derivative[_][FermiFunction | BoseFunction][_], Infinity], w],
    Message[AnalyticContinuation::periodicity, w]];
   e /. w -> -I omega);

(* 1/(x + i 0+)^k = P 1/x^k - i Pi (-1)^(k - 1)/(k - 1)! delta^(k - 1)(x); the coefficients are taken to be real *)
energyDelta[0, x_] := EnergyDelta[x];
energyDelta[n_Integer, x_] := Derivative[n][EnergyDelta][x];
(* inside a momentum sum; the factors outside that depend on omega (external lines) are taken inside first *)
imaginaryPartOfTerm[t_, omega_] := Module[{ms = Select[factorList[t], Head[#] === MomentumSum &]},
   If[ms =!= {}, With[{outside = t/ms[[1]]}, If[FreeQ[outside, omega],
      outside momentumSum[retardedImaginaryPartOf[ms[[1, 1]], omega], ms[[1, 2]]],
      momentumSum[retardedImaginaryPartOf[outside ms[[1, 1]], omega], ms[[1, 2]]]]],
    Total[Table[c[[3]] (-Pi) (-1)^(c[[2]] - 1)/(c[[2]] - 1)! energyDelta[c[[2]] - 1, Expand[omega - c[[1]]]], {c, laurentTerms[t, omega]}]]]];
retardedImaginaryPartOf[e_, omega_] := collectDeltas[Total[imaginaryPartOfTerm[#, omega] & /@
     (linearizeDenominators[#, omega] & /@ termsInVariable[e, omega])]];
(* $Failed if a factor is not split into poles *)
RetardedImaginaryPart[e_, omega_] := Block[{$poleFailure = False}, With[{result = retardedImaginaryPartOf[e, omega]},
    If[$poleFailure, $Failed, result]]];

(* a sum of delta functions, each with its coefficient written as in the textbooks *)
collectDeltas[e_] := If[! FreeQ[e, MomentumSum], e, Total[(textbookStatistics[#[[2]]] #[[1]] &) /@ deltaCoefficients[e]]];

(* the coefficients of the delta functions: {delta, coefficient} *)
deltaCoefficients[e_] := Module[{terms = termsInVariable[e, EnergyDelta], pairs},
   pairs = Table[With[{ds = Select[factorList[t], ! FreeQ[#, EnergyDelta] &]}, {Times @@ ds, t/(Times @@ ds)}], {t, terms}];
   ({#[[1, 1]], Total[#[[All, 2]]]} &) /@ GatherBy[Select[pairs, #[[1]] =!= 1 &], First]];

(* ---- the cutting rules at finite temperature ----
   A two-point function F whose propagating lines form chains from the time vertex where the external frequency enters
   (in) to the one where it leaves (out); a chain passes through vertices with static insertions only (Hartree, Fock:
   lines and interactions at equal times inside the vertex), and all its lines carry the same frequency. Then
   F(tau) = C Prod_chains L_c(tau) for 0 < tau < beta. A chain written as a function of the frequency x flowing from in
   to out, L(x) = Sum_j Sum_k r_jk/(x - E_j)^k, is L(tau) = Sum_j Sum_k r_jk/(k - 1)! d^(k-1)/dE^(k-1)
   [-(1 -+ n(E)) e^(-E tau)] at E = E_j (- for fermions, + for bosons): for a simple pole, the line on shell with the
   factor of emission (1 - n_F(E), 1 + n_B(E)), which for E < 0 is the absorption n(|E|); a static self-energy on the
   line gives a double pole and the derivative. Hence
     F^>(omega) = C (-1)^(N + 1) Sum Prod_c r_c/(k_c - 1)! d^(k_c - 1)/dE_c^(k_c - 1) [Prod_c (1 -+ n_c(E_c)) delta(omega - Sum E_c)],
   with F(tau) = -Integral dE F^>(E) e^(-E tau), and Im F^R(omega) = -Pi F^>(omega) (1 +- e^(-beta omega))
   = -Pi F^>(omega)/(1 -+ n(omega)), the second factor being the inverse process. Lines inside a time vertex are at equal
   times: n_F(xi) for a fermion line (G(0-)), n_F(xi) - 1 (G(0+)) or n_F(xi) - 1/2 with "ConvergenceFactor" -> -1 or 0,
   that is n_F(xi) - (1 - eta)/2; an (even) boson line is the same at 0- and 0+. *)
CuttingRuleImaginaryPart::class = "The cutting rules are implemented for two-point functions whose propagating lines form chains from one external time vertex to the other, through vertices with static insertions only (bubbles, sunsets, Hartree and Fock insertions): a term with external labels `1` is not of this kind.";
staticLineValue[{"Fermion", disp_}, {p_, _}, eta_] := FermiFunction[disp[p]] - (1 - eta)/2;
staticLineValue[{"Instantaneous", v_}, flow_, _] := lineValue[{"Instantaneous", v}, flow];
staticLineValue[{"Boson", f_}, {p_, _}, _] := Module[{x}, With[{g = linearizeDenominators[lineValue[{"Boson", f}, {p, -I x}], x]},
    -Total[(laurentCoefficient[g, x, #, 1, 1] (1 + BoseFunction[#[[1]]]) &) /@ polesOf[g, x]]]];

(* h(omega) delta^(m)(omega - E) = Sum_j Binomial[m, j] (-1)^j h^(j)(E) delta^(m - j)(omega - E): coefficients free of omega *)
deltaNormalForm[e_, omega_] := Total[Table[With[{ds = Select[factorList[t], ! FreeQ[#, EnergyDelta] &]},
     If[Length[ds] != 1 || FreeQ[t/ds[[1]], omega], t,
      With[{c = t/ds[[1]], md = ds[[1]] /. {EnergyDelta[x_] :> {0, x}, Derivative[m_][EnergyDelta][x_] :> {m, x}}},
       Sum[Binomial[md[[1]], j] (-1)^j (If[j == 0, c, D[c, {omega, j}]] /. omega -> Expand[omega - md[[2]]]) energyDelta[md[[1]] - j, md[[2]]],
        {j, 0, md[[1]]}]]]], {t, termsInVariable[e, EnergyDelta]}]];

(* the chains of propagating lines from tIn to tOut: lists of {line, orientation}; $Failed if the lines do not form
   chains through vertices with two propagating lines *)
cutChains[cut_List, timeOf_List, tIn_, tOut_] := Catch[Module[{ends, chains = {}, used = {}, chain, v, next, l},
    ends[j_] := {timeOf[[cut[[j, 1]]]], timeOf[[cut[[j, 2]]]]};
    Do[If[! MemberQ[used, j] && MemberQ[ends[j], tIn],
      chain = {}; v = tIn; next = j;
      While[True,
       l = next; AppendTo[used, l];
       AppendTo[chain, {l, If[ends[l][[1]] == v, 1, -1]}];
       v = If[ends[l][[1]] == v, ends[l][[2]], ends[l][[1]]];
       If[v == tOut, Break[]];
       If[v == tIn, Throw[$Failed, chainFailure]];
       next = Select[Range[Length[cut]], ! MemberQ[used, #] && MemberQ[ends[#], v] &];
       If[Length[next] != 1 || Count[Join @@ Table[ends[i], {i, Length[cut]}], v] != 2, Throw[$Failed, chainFailure]];
       next = First[next]];
      AppendTo[chains, chain]], {j, Length[cut]}];
    If[Length[used] != Length[cut], $Failed, chains]], chainFailure];

cutIntegrand[data_List, spin_, w_, omega_, eta_] := Catch[Module[{lines = routeData["Lines", data], timeOf = routeData["TimeOf", data],
     externals = routeData["Externals", data], nodes = routeData["ExternalNodes", data], inflow, tIn, tOut, static, cut,
     chains, constant, spectra, energies, statistics, thermal, n},
    (* the external frequency entering each time vertex *)
    inflow[tv_] := Total[Table[If[timeOf[[x[[1]] /. nodes]] == tv, Coefficient[Expand[x[[2, 2]]/I], w], 0], {x, externals}]];
    tIn = Select[Union[timeOf], inflow[#] === 1 &]; tOut = Select[Union[timeOf], inflow[#] === -1 &];
    If[tIn === {} && tOut === {}, Throw[0, cutFailure]];
    If[Length[tIn] != 1 || Length[tOut] != 1, Message[CuttingRuleImaginaryPart::class, Table[x[[1]], {x, externals}]]; Throw[$Failed, cutFailure]];
    {tIn, tOut} = {tIn[[1]], tOut[[1]]};
    static = Select[lines, timeOf[[#[[1]]]] == timeOf[[#[[2]]]] &];
    cut = Select[lines, timeOf[[#[[1]]]] != timeOf[[#[[2]]]] &];
    chains = cutChains[cut, timeOf, tIn, tOut];
    If[chains === $Failed, Message[CuttingRuleImaginaryPart::class, Table[x[[1]], {x, externals}]]; Throw[$Failed, cutFailure]];
    constant = weightOf[data, spin] Times @@ (staticLineValue[#[[4]], #[[5]], eta] &) /@ static;
    (* each chain as a function of the frequency x flowing from in to out: {pole, order k, Laurent coefficient r_k/(k - 1)!} *)
    spectra = Table[Module[{x}, With[{f = linearizeDenominators[Times @@ Table[With[{l = cut[[c[[1]]]]},
            lineValue[l[[4]], {l[[5, 1]], -I c[[2]] x}]], {c, chain}], x]},
        Table[{c[[1]], c[[2]], c[[3]]/(c[[2]] - 1)!}, {c, laurentTerms[f, x]}]]], {chain, chains}];
    n = Length[chains]; energies = Table[Unique["FunctionalDiagrams`Private`energy"], {n}];
    thermal = Times @@ Table[If[cut[[chains[[c, 1, 1]], 4, 1]] === "Fermion", 1 - FermiFunction[energies[[c]]], 1 + BoseFunction[energies[[c]]]], {c, n}];
    statistics = frequencyStatistics[w];
    collectDeltas[deltaNormalForm[simplifyStatisticalFactors[Total[Table[
        -Pi constant (-1)^(n + 1) (Times @@ choice[[All, 3]]) (Fold[If[#2[[2]] == 0, #1, D[#1, {#2[[1]], #2[[2]]}]] &,
             thermal EnergyDelta[omega - Total[energies]], Table[{energies[[c]], choice[[c, 2]] - 1}, {c, n}]] /.
            Thread[energies -> choice[[All, 1]]])/If[statistics === "Fermion", 1 - FermiFunction[omega], 1 + BoseFunction[omega]],
        {choice, Tuples[spectra]}]] /. (h : EnergyDelta | Derivative[_][EnergyDelta])[a_] :> h[Expand[a]]], omega]]], cutFailure];

(* the frequency w enters the diagram at one external point and leaves it at another *)
CuttingRuleImaginaryPart::frequency = "The frequency `1` must enter the diagram at one external point and leave it at another: I `1` and -I `1` in the frequencies of exactly two external points.";
cutFrequencyQ[externals_List, w_] := If[Sort[DeleteCases[Table[Coefficient[Expand[x[[2, 2]]/I], w], {x, externals}], 0]] === {-1, 1}, True,
   Message[CuttingRuleImaginaryPart::frequency, w]; False];

(* $Failed if a term is not of the implemented kind *)
CuttingRuleImaginaryPart[e_, externals_List, rules_List, w_Symbol -> omega_, opts___Rule] := Module[{spin, names, eta, routed, results},
   {spin, names, eta} = matsubaraOptions[callOptions[CuttingRuleImaginaryPart, {opts}]];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed || ! cutFrequencyQ[externals, w], $Failed,
    results = Table[If[data === $Failed, $Failed, With[{cut = cutIntegrand[Append[data, "Externals" -> externals], spin, w, omega, eta]},
        If[cut === $Failed, $Failed, momentumSum[cut, routeData["LoopMomenta", data]]]]], {data, routed}];
    If[MemberQ[results, $Failed], $Failed, Total[results]]]];

(* the two sides, term by term and delta function by delta function: the list of the differences that do not vanish *)
CheckCuttingRules[e_, externals_List, rules_List, w_Symbol -> omega_, opts___Rule] := Module[{spin, names, eta, routed, differences = {}},
   {spin, names, eta} = matsubaraOptions[callOptions[CheckCuttingRules, {opts}]];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed || ! cutFrequencyQ[externals, w], $Failed,
    Do[If[data === $Failed, AppendTo[differences, $Failed],
      Module[{summed = summedIntegrand[data, eta], residues, cuts},
       residues = If[summed === $Failed, $Failed, RetardedImaginaryPart[AnalyticContinuation[weightOf[data, spin] summed, w -> omega], omega]];
       cuts = cutIntegrand[Append[data, "Externals" -> externals], spin, w, omega, eta];
       If[residues === $Failed || cuts === $Failed, AppendTo[differences, $Failed],
        Scan[If[! thermalZeroQ[#[[2]]], AppendTo[differences, #]] &, deltaCoefficients[residues - cuts]]]]],
     {data, routed}];
    If[differences === {}, 0, differences]]];

(* T -> 0 of the sums of logarithms (tried first): T ln(1 - n_F(a)) -> a theta(-a), T ln(1 + n_B(a)) -> 0 (a > 0) *)
ZeroTemperatureLimit[e_] /; ! FreeQ[e, Temperature] && ! TrueQ[$temperatureLogsDone] := Block[{$temperatureLogsDone = True},
   ZeroTemperatureLimit[(e /. {Log[1 - FermiFunction[a_]] :> a UnitStep[-a]/Temperature, Log[1 + BoseFunction[_]] :> 0}) /.
     MomentumSum[f_, ps_] :> momentumSum[Expand[f], ps]]];

(* ---- zero temperature: n_F(e) -> theta(-e), n_B(e) -> -theta(-e); n_F(0) = 1/2 is already evaluated ---- *)
ZeroTemperatureLimit[e_] := e /. {Derivative[k_][FermiFunction][a_] :> -energyDelta[k - 1, a],
    Derivative[k_][BoseFunction][a_] :> energyDelta[k - 1, a], FermiFunction[a_] :> UnitStep[-a], BoseFunction[a_] :> -UnitStep[-a]};

(* ---- display and LaTeX of the new heads ---- *)
defineDisplayForm[FermiFunction[e_], Interpretation[Row[{Subscript["n", "F"], "(", e, ")"}], FermiFunction[e]]];
defineDisplayForm[BoseFunction[e_], Interpretation[Row[{Subscript["n", "B"], "(", e, ")"}], BoseFunction[e]]];
defineDisplayForm[EnergyDelta[e_], Interpretation[Row[{"\[Delta]", "(", e, ")"}], EnergyDelta[e]]];
defineDisplayForm[MomentumSum[f_, ps_List], Interpretation[Row[{Subscript["\[Sum]", Row[ps, ","]], " ", f}], MomentumSum[f, ps]]];
defineDisplayForm[LoopMomentum[i_], Interpretation[Subscript["p", i], LoopMomentum[i]]];
defineDisplayForm[LoopFrequency[i_, s_String], Interpretation[Subscript[If[s === "Fermion", "\[Omega]", "\[Nu]"], i], LoopFrequency[i, s]]];

(* the generic rules of latexFactorOf (a function of labels h(x, y), its derivatives) must not apply to the new heads *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {FermiFunction, BoseFunction, EnergyDelta, MomentumSum, LoopMomentum, LoopFrequency,
    UnitStep}];
latexStatistical[h_, k_, e_] := If[h === FermiFunction, "n_F", "n_B"] <>
   If[k == 0, "", "^{" <> If[k <= 3, StringJoin[Table["\\prime", {k}]], "(" <> ToString[k] <> ")"] <> "}"] <> "(" <> latexOfExpression[e] <> ")";
latexFactorOf[(h : FermiFunction | BoseFunction)[e_]] := latexStatistical[h, 0, e];
latexFactorOf[Derivative[k_][h : FermiFunction | BoseFunction][e_]] := latexStatistical[h, k, e];
latexFactorOf[EnergyDelta[e_]] := "\\delta(" <> latexOfExpression[e] <> ")";
latexFactorOf[Derivative[k_][EnergyDelta][e_]] := "\\delta^{(" <> ToString[k] <> ")}(" <> latexOfExpression[e] <> ")";
latexFactorOf[MomentumSum[f_, ps_List]] := "\\sum_{" <> joinStrings[latexArgument /@ ps, ","] <> "}" <>
   If[Head[f] === Plus || TrueQ[numericFactor[f] < 0], "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[LoopMomentum[i_]] := "p_{" <> ToString[i] <> "}";
(* the step function of the zero-temperature limit *)
latexFactorOf[UnitStep[e_]] := "\\theta(" <> latexOfExpression[e] <> ")";
(* the temperature of the sums of logarithms *)
latexFactorOf[Temperature] := "T";
latexFactorOf[LoopFrequency[i_, s_String]] := If[s === "Fermion", "\\omega_{", "\\nu_{"] <> ToString[i] <> "}";
