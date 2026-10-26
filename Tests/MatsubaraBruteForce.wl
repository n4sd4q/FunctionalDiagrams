(* ::Package:: *)

(* The brute-force check of the evaluation in Matsubara frequency, shared by Tests/RunTests.wl and
   Showcase/make_figures.wl: the integrand of each diagram before the sums (DiagramIntegrand), summed over many Matsubara
   frequencies at T = 0.37 (symmetric truncation, Richardson extrapolation, the convergence factor e^(i w 0+) added as
   eta c/2 with c = lim z f(z)), against the evaluated diagram (EvaluateMatsubara), at given momenta. Load it with Get
   after the package; it defines temperature, numeric, frequencyValues, withoutMomentumSum, bruteForceDiagram and
   bruteForceErrors in the current context. *)

temperature = 0.37;
numeric[e_] := N[e /. {FermiFunction -> Function[s, 1/(Exp[s/temperature] + 1)], BoseFunction -> Function[s, 1/(Exp[s/temperature] - 1)]}];
frequencyValues["Fermion", n_] := N[Table[(2 j + 1) Pi temperature, {j, -n, n - 1}]];
frequencyValues["Boson", n_] := N[Table[2 j Pi temperature, {j, -n, n}]];
withoutMomentumSum[e_] := e /. MomentumSum[f_, _] :> f;

(* T Sum over the loop frequencies of the integrand, at the values given; the level m of a sum has 2m (fermions) or
   2m + 1 (bosons) frequencies. One frequency: levels n and 2n, Richardson (errors a/n). Two: the frequency of the lines
   at equal times (eta != 0) is summed inside, first (it appears in one line only, so the sum factorizes), for each
   outer frequency at levels 20 k and 10 k (one grid), Richardson, with k the outer level m (inner = Automatic) or
   k = inner at every outer level; the outer sum at levels n, 2n, 4n with two levels of Richardson (errors a/n + b/n^2) *)
bruteForceDiagram[data_List, values_List, n_, inner_: Automatic] := Module[{outerCache = Null, f = ("Integrand" /. data) /. values,
    fs = "LoopFrequencies" /. data, exponent = "EqualTimeFrequency" /. data, etas, s},
   (* the coefficient of each loop frequency in the exponent, by substitution *)
   etas = Table[Sign[Expand[(exponent /. u -> 1) - (exponent /. u -> 0)]], {u, fs}];
   If[Length[fs] == 2 && etas[[2]] =!= 0 && etas[[1]] === 0, fs = Reverse[fs]; etas = Reverse[etas]];
   s[m_] := Which[
     Length[fs] == 0, f,
     Length[fs] == 1, temperature Total[f /. fs[[1]] -> frequencyValues[fs[[1, 2]], m]] + etas[[1]]/2 ((I fs[[1]] f) /. fs[[1]] -> 10.^8),
     True, Module[{innerSums, w2 = frequencyValues[fs[[2, 2]], m], k = If[inner === Automatic, m, inner], pair, tail},
      (* the inner sums at levels 2 m1 and m1 from one evaluation of f: the frequencies of level m1 are the central rows of
         the grid of level 2 m1 *)
      innerSums[x2_, m1_] := With[{w1 = frequencyValues[fs[[1, 2]], 2 m1]}, With[{grid = f /. {fs[[1]] -> Transpose[ConstantArray[w1, Length[x2]]],
            fs[[2]] -> ConstantArray[x2, Length[w1]]}, etaTerm = etas[[1]]/2 ((I fs[[1]] f) /. {fs[[1]] -> 10.^9, fs[[2]] -> x2})},
         {temperature Total[grid] + etaTerm, temperature Total[grid[[m1 + 1 ;; Length[w1] - m1]]] + etaTerm}]];
      (* a fixed inner level: the inner sums at the largest outer level 4 n, computed once, give those of the levels n and
         2 n (the central columns) *)
      pair = If[inner === Automatic, innerSums[w2, 10 k],
        With[{all = If[outerCache === Null, outerCache = innerSums[frequencyValues[fs[[2, 2]], 4 n], 10 k], outerCache]},
         With[{c = (Length[all[[1]]] - Length[w2])/2}, all[[All, c + 1 ;; c + Length[w2]]]]]];
      tail = innerSums[{10.^6}, 10 k];
      temperature Total[2 pair[[1]] - pair[[2]]] + etas[[2]]/2 (I 10.^6 (2 tail[[1]] - tail[[2]])[[1]])]];
   N[("Weight" /. data) /. values] If[Length[fs] == 2, (8 s[4 n] - 6 s[2 n] + s[n])/3, 2 s[2 n] - s[n]]];

(* each term of a sum of diagrams: the relative difference between the evaluated term and the brute-force sum of its
   integrand (relative to the evaluated value, or to 10^-3 if it is smaller) *)
bruteForceErrors[e_, ext_, rules_, values_, n_, inner_: Automatic] := Table[With[{value = numeric[withoutMomentumSum[
       EvaluateMatsubara[t, ext, rules]] /. values]},
    Abs[value - bruteForceDiagram[First[DiagramIntegrand[t, ext, rules]], values, n, inner]]/Max[Abs[value], 10^-3]], {t, DiagramTerms[e]}];
