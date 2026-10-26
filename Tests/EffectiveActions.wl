(* ::Package:: *)

(* ========================================================================= *)
(*  Tests of the effective actions (11_effective_actions.wl): operator      *)
(*  traces and their derivatives, derivatives with respect to two-index      *)
(*  objects, the 2PI effective action Gamma[G] and Gamma[phibar, G], the 1PI *)
(*  effective action, Potthoff's functionals, zero dimensions, Tr ln in the  *)
(*  Matsubara evaluation, responses and their static and dynamic limits,     *)
(*  the divergences of the irreducible vertex. The electron gas and phi^4    *)
(*  theory to third order at every step.                                     *)
(*  Read by RunTests.wl (before Superfields.wl), or run alone:              *)
(*    wolfram -script EffectiveActions.wl                                    *)
(* ========================================================================= *)

If[! ValueQ[passed],
  (* alone *)
  testDirectory = If[StringQ[$InputFileName] && $InputFileName =!= "", DirectoryName[$InputFileName], Directory[]];
  Get[FileNameJoin[{testDirectory, "..", "FunctionalDiagrams.wl"}]];
  Get[FileNameJoin[{testDirectory, "Harness.wl"}]];
  effectiveActionsAlone = True,
  (* read by RunTests.wl: a fresh start, as in a new session: the symbols of the earlier sections cleared and the
     declarations reset *)
  Quiet[Scan[(Unprotect[#]; ClearAll[#]) &, Complement[Names["Global`*"], harnessNames]]];
  ResetDeclarations[]];
(* zero, after Simplify (the values in zero dimensions are rational functions and logarithms) *)
checkSimplify[name_String, expr_] := check[name, Simplify[expr], 0];

nmax = 3;

(* ------------------------------------------------------------------------- *)
section["operator algebra"];
DeclareDirectedPropagator[A0, B0];
check["kernel of a product", ExpandTrace[OperatorKernel[A0 . B0][x, y]] === SimplifyDiagrams[IntegralOver[A0[x, u] B0[u, y], {u}]], True];
check["trace of a product: a ring", ExpandTrace[OperatorTrace[A0 . B0]] === SimplifyDiagrams[IntegralOver[A0[u, w] B0[w, u], {u, w}]], True];
check["inverse pairs cancel in a trace", ExpandTrace[OperatorTrace[Power[A0, -1] . B0 . A0]] === ExpandTrace[OperatorTrace[B0]], True];
check["kernels of inverses contracted", ExpandTrace[IntegralOver[OperatorKernel[Power[A0, -1]][x, u] A0[u, y], {u}]], DeltaFunction[x, y]];
checkSimplify["Dyson series of (A0^-1 - B0)^-1", ExpandTrace[OperatorKernel[Power[Power[A0, -1] - B0, -1]][x, y], A0, 2] -
   SimplifyDiagrams[A0[x, y] + IntegralOver[A0[x, u] B0[u, v] A0[v, y], {u, v}] + IntegralOver[A0[x, u] B0[u, v] A0[v, w] B0[w, z] A0[z, y], {u, v, w, z}]]];
checkSimplify["Tr ln(-(A0^-1 - B0)): rings", ExpandTrace[TraceLog[-(Power[A0, -1] - B0)], A0, 2] -
   (TraceLog[-Power[A0, -1]] - SimplifyDiagrams[IntegralOver[A0[u, w] B0[w, u], {u, w}] + 1/2 IntegralOver[A0[u, v] B0[v, w] A0[w, z] B0[z, u], {u, v, w, z}]])];
check["derivative of Tr ln A^-1", FunctionalDerivative[TraceLog[Power[A0, -1]], A0[x, y]], -OperatorKernel[Power[A0, -1]][y, x]];
check["derivative of Tr(B0 . A0)", FunctionalDerivative[OperatorTrace[B0 . A0], A0[x, y]], B0[y, x]];

(* ------------------------------------------------------------------------- *)
(* the electron gas: G0 = -<psi psibar>_0, S_int = 1/2 Integral V psibar psibar psi psi *)
DeclareGrassmannSource[eta, etabar]; DeclareDirectedPropagator[G0, G];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
DeclareSymmetric[V]; DeclareInteractionLine[V];
Zf = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
Vf = 1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
(* phi^4 theory: D0 = <phi phi>_0, S_int = lambda/4! Integral phi^4 *)
DeclareSource[J]; DeclareField[phi, J]; DeclareSymmetric[D0, Gb];
Zb = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
Vb = lambda/24 IntegralOver[phi[z]^4, {z}];

section["perturbative objects (third order)"];
vacuumF = ConnectedVacuumDiagrams[Zf, Vf, nmax];
greenF = -PerturbativeCorrelationFunction[Zf, {psi[a], psibar[b]}, Vf, nmax];
sigmaF = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenF, 1, {a, b}, {G0}], {a, b}, {G0}];
phiF = LuttingerWardFunctional[Zf, Vf, nmax, G0 -> G];
vacuumB = ConnectedVacuumDiagrams[Zb, Vb, nmax];
greenB = PerturbativeCorrelationFunction[Zb, {phi[a], phi[b]}, Vb, nmax];
sigmaB = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenB, 1, {a, b}, {D0}], {a, b}, {D0}];
phiB = LuttingerWardFunctional[Zb, Vb, nmax, D0 -> Gb];
Print["diagrams: vacuum ", Length[DiagramTerms[vacuumF]], " and ", Length[DiagramTerms[vacuumB]], ", self-energy ",
  Length[DiagramTerms[sigmaF]], " and ", Length[DiagramTerms[sigmaB]], ", Phi ", Length[DiagramTerms[phiF]], " and ", Length[DiagramTerms[phiB]]];

(* ------------------------------------------------------------------------- *)
section["FunctionalDerivative with respect to two-index objects (propagators, interactions)"];
(* every G[a, b] is an independent variable (symmetrised for a symmetric kernel), as in SelfEnergy *)
check["dPhi/dG(b, a) = SelfEnergy (fermions), third order", SimplifyDiagrams[FunctionalDerivative[phiF, G[b, a]] - SelfEnergy[phiF, G, {a, b}]], 0];
check["d^2 Phi/dG(x4, x3) dG(x2, x1) = IrreducibleVertex (fermions), third order",
  SimplifyDiagrams[FunctionalDerivative[phiF, G[x4, x3], G[x2, x1]] - IrreducibleVertex[phiF, G, {x1, x2, x3, x4}]], 0];
check["-2 dSigma(x1, x2)/dG(x3, x4) = IrreducibleVertex (phi^4), third order",
  SimplifyDiagrams[-2 FunctionalDerivative[SelfEnergy[phiB, Gb, {x1, x2}, "Statistics" -> "Boson"], Gb[x3, x4]] -
    IrreducibleVertex[phiB, Gb, {x1, x2, x3, x4}, "Statistics" -> "Boson"]], 0];
dPhiV = FunctionalDerivative[phiF, V[a, b]];
check["dPhi/dV(a, b) is symmetric (V symmetric)", SimplifyDiagrams[dPhiV - (dPhiV /. {a -> b, b -> a})], 0];
check["Euler: Integral V dPhi/dV = Sum_m m Phi_m (m interaction lines)",
  SimplifyDiagrams[IntegralOver[V[a, b] dPhiV, {a, b}] - Sum[m OrderPart[phiF, V, m], {m, nmax}]], 0];
check["Euler: Integral G dPhi/dG = Sum_m 2 m Phi_m (2 m lines G)",
  SimplifyDiagrams[IntegralOver[G[b, a] FunctionalDerivative[phiF, G[b, a]], {a, b}] - Sum[2 m OrderPart[phiF, V, m], {m, nmax}]], 0];

(* ------------------------------------------------------------------------- *)
section["the 2PI effective action Gamma[G]: electron gas"];
gammaF = EffectiveAction2PI[phiF, G0 -> G];
check["Gamma = Phi - Tr ln(-G^-1) - Tr(G0^-1 G - 1)", gammaF === phiF - TraceLog[-Power[G, -1]] - OperatorTrace[Power[G0, -1] . G - 1], True];
check["EffectiveAction2PI[Z0, interaction, n, G0 -> G] is the same", EffectiveAction2PI[Zf, Vf, nmax, G0 -> G] === gammaF, True];
dGammaF = FunctionalDerivative[gammaF, G[b, a]];
checkSimplify["dGamma/dG(b, a) = Sigma(a, b) + G^-1(a, b) - G0^-1(a, b)",
  dGammaF - SimplifyDiagrams[SelfEnergy[phiF, G, {a, b}] + OperatorKernel[Power[G, -1]][a, b] - OperatorKernel[Power[G0, -1]][a, b]]];
(* G0 (dGamma/dG^T) G = G0 Sigma G + G0 - G: dGamma/dG = 0 is the Dyson equation *)
dysonResidualF = ExpandTrace[IntegralOver[G0[x, u] (dGammaF /. {a -> u, b -> v}) G[v, y], {u, v}]];
checkSimplify["G0 (dGamma/dG) G = -(G - G0 - G0 Sigma[G] G)", dysonResidualF + ExactEquation["Dyson",
     {"G" -> G, "G0" -> G0, "Sigma" -> LabelFunction[SelfEnergy[phiF, G, {a, b}], {a, b}]}, {x, y}] /. Equal -> Subtract];
(* at the perturbative G = (G0^-1 - Sigma_pert)^-1 *)
sF = LabelFunction[sigmaF, {a, b}];
gPertF = Power[Power[G0, -1] - sF, -1];
stationaryF = ExpandTrace[dysonResidualF /. G -> gPertF, G0, nmax, "MaximumOrder" -> {V, nmax}];
check["stationary at the perturbative G, order by order (Sigma[G] = Sigma)", Table[OrderPart[stationaryF, V, m], {m, 0, nmax}], Table[0, {nmax + 1}]];
valueF = ExpandTrace[gammaF /. G -> gPertF, G0, nmax, "MaximumOrder" -> {V, nmax}];
check["Gamma at the perturbative G = -log Z0 - (connected vacuum diagrams), orders 1 to 3",
  Table[SimplifyDiagrams[OrderPart[valueF + vacuumF, V, m]], {m, 0, nmax}], {-TraceLog[-Power[G0, -1]], 0, 0, 0}];
(* the pieces: Phi[G] - Tr(Sigma G) + rings Sum_k Tr((G0 Sigma)^k)/k *)
ringsF = ExpandTrace[-TraceLog[-(Power[G0, -1] - sF)], G0, nmax, "MaximumOrder" -> {V, nmax}];
ringExplicit[k_] := Module[{w = Table[Unique["w"], {2 k}]}, IntegralOver[Product[G0[w[[2 i - 1]], w[[2 i]]] (sigmaF /. {a -> w[[2 i]],
       b -> w[[If[i == k, 1, 2 i + 1]]]}), {i, k}], w]];
checkSimplify["-Tr ln(-(G0^-1 - Sigma)) = -Tr ln(-G0^-1) + Sum_k Tr((G0 Sigma)^k)/k (rings)",
  ringsF + TraceLog[-Power[G0, -1]] - ExpandTrace[Sum[ringExplicit[k]/k, {k, 1, nmax}], "MaximumOrder" -> {V, nmax}]];

(* ------------------------------------------------------------------------- *)
section["the 2PI effective action Gamma[G]: phi^4 theory"];
gammaB = EffectiveAction2PI[phiB, D0 -> Gb, "Statistics" -> "Boson"];
check["Gamma = Gamma2 + 1/2 Tr ln G^-1 + 1/2 Tr(D0^-1 G - 1)", gammaB === phiB + 1/2 TraceLog[Power[Gb, -1]] + 1/2 OperatorTrace[Power[D0, -1] . Gb - 1], True];
dGammaB = FunctionalDerivative[gammaB, Gb[a, b]];
checkSimplify["dGamma/dG(a, b) = 1/2 (D0^-1 - G^-1 - Sigma)(a, b)",
  dGammaB - SimplifyDiagrams[1/2 (OperatorKernel[Power[D0, -1]][a, b] - OperatorKernel[Power[Gb, -1]][a, b] - SelfEnergy[phiB, Gb, {a, b}, "Statistics" -> "Boson"])]];
dysonResidualB = ExpandTrace[IntegralOver[D0[x, u] (-2 dGammaB /. {a -> u, b -> v}) Gb[v, y], {u, v}]];
checkSimplify["-2 D0 (dGamma/dG) G = -(G - D0 - D0 Sigma[G] G)", dysonResidualB + ExactEquation["Dyson",
     {"G" -> Gb, "G0" -> D0, "Sigma" -> LabelFunction[SelfEnergy[phiB, Gb, {a, b}, "Statistics" -> "Boson"], {a, b}]}, {x, y}] /. Equal -> Subtract];
sB = LabelFunction[sigmaB, {a, b}];
gPertB = Power[Power[D0, -1] - sB, -1];
stationaryB = ExpandTrace[dysonResidualB /. Gb -> gPertB, D0, nmax, "MaximumOrder" -> {lambda, nmax}];
check["stationary at the perturbative G, order by order", Table[OrderPart[stationaryB, lambda, m], {m, 0, nmax}], Table[0, {nmax + 1}]];
valueB = ExpandTrace[gammaB /. Gb -> gPertB, D0, nmax, "MaximumOrder" -> {lambda, nmax}];
check["Gamma at the perturbative G = -log Z0 - (connected vacuum diagrams), orders 1 to 3",
  Table[SimplifyDiagrams[OrderPart[valueB + vacuumB, lambda, m]], {m, 0, nmax}], {1/2 TraceLog[Power[D0, -1]], 0, 0, 0}];

(* ------------------------------------------------------------------------- *)
section["mean fields as variables (free theory)"];
gammaFreeF = EffectiveAction2PI[Zf, 0, 0, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}];
check["fermions: Gamma[Psibar, Psi, G] = -Psibar G0^-1 Psi + Gamma[G]", SimplifyDiagrams[gammaFreeF -
   (-IntegralOver[OperatorKernel[Power[G0, -1]][x, y] Psibar[x] ** Psi[y], {x, y}] + EffectiveAction2PI[0, G0 -> G])], 0];
check["dGamma/dPsibar = -G0^-1 Psi (= eta, the source): Psi = -G0 eta", SimplifyDiagrams[ExpandTrace[IntegralOver[G0[x, z] FunctionalDerivative[gammaFreeF, Psibar[z]], {z}]] + Psi[x]], 0];
check["dGamma/dPsi = Psibar G0^-1", SimplifyDiagrams[ExpandTrace[IntegralOver[FunctionalDerivative[gammaFreeF, Psi[z]] G0[z, x], {z}]] - Psibar[x]], 0];
check["on shell at zero sources the means vanish (the derivatives are odd)", SetSourcesToZero[FunctionalDerivative[gammaFreeF, Psibar[z]]], 0];
gammaFreeB = EffectiveAction2PI[Zb, 0, 0, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}];
check["real bosons: dGamma/dphibar = D0^-1 phibar (= J)", ExpandTrace[IntegralOver[D0[x, z] FunctionalDerivative[gammaFreeB, phiMean[z]], {z}]], phiMean[x]];
(* with an interaction: the field dependence below *)

(* ------------------------------------------------------------------------- *)
section["Phi: self-consistent (counterterm) representation"];
(* -Phi = log<exp(-S_int + psibar Sigma psi)>_G + Tr(Sigma G): the vacuum diagrams with lines G and the counterterm
   k = -Sigma[G] (fermions; Gamma2 = 1/2 Tr(k G) - log<exp(-S_int + 1/2 phi k phi)>_G for real bosons): every graph that is
   not 2PI cancels, the trace term removes the ring with one insertion *)
DeclareDirectedPropagator[Kc]; DeclareSymmetric[Kb];
lhsF = ConnectedVacuumDiagrams[Exp[-IntegralOver[G[x, y] etabar[x] ** eta[y], {x, y}]],
   Vf + IntegralOver[Kc[x, y] psibar[x] ** psi[y], {x, y}], nmax] /. Kc -> LabelFunction[-SelfEnergy[phiF, G, {a, b}], {a, b}];
traceKGF = IntegralOver[(-SelfEnergy[phiF, G, {u, w}]) G[w, u], {u, w}];
check["fermions, orders 1 to 3", Table[SimplifyDiagrams[OrderPart[lhsF + phiF + traceKGF, V, m]], {m, 1, nmax}], {0, 0, 0}];
check["without the trace term the first order fails", SimplifyDiagrams[OrderPart[lhsF + phiF, V, 1]] =!= 0, True];
sigmaOfGB = SelfEnergy[phiB, Gb, {a, b}, "Statistics" -> "Boson"];
lhsB = ConnectedVacuumDiagrams[Exp[1/2 IntegralOver[J[x] Gb[x, y] J[y], {x, y}]], Vb - 1/2 IntegralOver[Kb[x, y] phi[x] phi[y], {x, y}], nmax] /.
   Kb -> LabelFunction[-sigmaOfGB, {a, b}];
traceKGB = 1/2 IntegralOver[(-sigmaOfGB /. {a -> u, b -> w}) Gb[w, u], {u, w}];
check["phi^4, orders 1 to 3", Table[SimplifyDiagrams[OrderPart[lhsB - traceKGB + phiB, lambda, m]], {m, 1, nmax}], {0, 0, 0}];

section["Galitskii-Migdal"];
(* <S_int> = -Sum_m m w_m (w_m the connected vacuum diagrams of order m) = 1/2 Tr(Sigma G) for fermions (S_int quadratic
   in psibar), -1/4 Tr(Sigma G) for phi^4 (S_int quartic in phi); on Phi: <S_int> = Sum_m m Phi_m[G] *)
trSGF = SimplifyDiagrams[IntegralOver[AmputateExternalLegs[greenF - G0[a, b], {a}, {G0}] /. b -> a, {a}]];
check["fermions: 1/2 Tr(Sigma G) = -Sum_m m w_m, orders 1 to 3", Table[SimplifyDiagrams[OrderPart[trSGF/2, V, m] + m OrderPart[vacuumF, V, m]], {m, 1, nmax}], {0, 0, 0}];
trSGB = SimplifyDiagrams[IntegralOver[AmputateExternalLegs[greenB - D0[a, b], {a}, {D0}] /. b -> a, {a}]];
check["phi^4: -1/4 Tr(Sigma G) = -Sum_m m w_m, orders 1 to 3", Table[SimplifyDiagrams[-OrderPart[trSGB/4, lambda, m] + m OrderPart[vacuumB, lambda, m]], {m, 1, nmax}], {0, 0, 0}];
check["on Phi, fermions: Sum_m m Phi_m[G] = 1/2 Tr(Sigma[G] G)", SimplifyDiagrams[Sum[m OrderPart[phiF, V, m], {m, 1, nmax}] -
    1/2 IntegralOver[SelfEnergy[phiF, G, {u, w}] G[w, u], {u, w}]], 0];
check["on Gamma2, phi^4: Sum_m m Gamma2_m[G] = -1/4 Tr(Sigma[G] G)", SimplifyDiagrams[Sum[m OrderPart[phiB, lambda, m], {m, 1, nmax}] +
    1/4 IntegralOver[(sigmaOfGB /. {a -> u, b -> w}) Gb[w, u], {u, w}]], 0];

(* ------------------------------------------------------------------------- *)
section["Potthoff: F[Sigma] and the self-energy functional"];
DeclareDirectedPropagator[Sg]; DeclareSymmetric[Sb];
omegaF = SelfEnergyFunctional[phiF, G0 -> Sg, G];
check["Omega_G0[S] = F[S] - Tr ln(-(G0^-1 - S))", omegaF === LegendreTransform[phiF, G -> Sg] - TraceLog[-(Power[G0, -1] - Sg)], True];
check["universality: F (Phi) contains G and V only, never G0", FreeQ[phiF, G0] && FreeQ[phiB, D0] && FreeQ[LegendreTransform[phiF, G -> Sg], G0], True];
check["dF/dS(a, b) = -G[S](b, a)", FunctionalDerivative[LegendreTransform[phiF, G -> Sg], Sg[a, b]], -LegendrePropagator[G -> Sg, {V}][b, a]];
check["dOmega/dS(a, b) = (G0^-1 - S)^-1(b, a) - G[S](b, a): stationary at Dyson", SimplifyDiagrams[FunctionalDerivative[omegaF, Sg[a, b]] -
   (ExpandTrace[OperatorKernel[Power[Power[G0, -1] - Sg, -1]][b, a]] - LegendrePropagator[G -> Sg, {V}][b, a])], 0];
check["G0 enters Omega only through the trace-log", FunctionalDerivative[omegaF, G0[a, b]] === FunctionalDerivative[-TraceLog[-(Power[G0, -1] - Sg)], G0[a, b]], True];
potthoffValueF = ExpandTrace[omegaF /. {G -> gPertF, Sg -> sF}, G0, nmax, "MaximumOrder" -> {V, nmax}];
check["at the physical Sigma: Omega = -log Z0 - (connected vacuum diagrams), electron gas",
  Table[SimplifyDiagrams[OrderPart[potthoffValueF + vacuumF, V, m]], {m, 0, nmax}], {-TraceLog[-Power[G0, -1]], 0, 0, 0}];
omegaB = SelfEnergyFunctional[phiB, D0 -> Sb, Gb, "Statistics" -> "Boson"];
check["dF/dS = 1/2 G[S] (real bosons)", FunctionalDerivative[LegendreTransform[phiB, Gb -> Sb, "Statistics" -> "Boson"], Sb[a, b]], LegendrePropagator[Gb -> Sb, {}][a, b]/2];
potthoffValueB = ExpandTrace[omegaB /. {Gb -> gPertB, Sb -> sB}, D0, nmax, "MaximumOrder" -> {lambda, nmax}];
check["at the physical Sigma: Omega = -log Z0 - (connected vacuum diagrams), phi^4",
  Table[SimplifyDiagrams[OrderPart[potthoffValueB + vacuumB, lambda, m]], {m, 0, nmax}], {1/2 TraceLog[Power[D0, -1]], 0, 0, 0}];

(* ------------------------------------------------------------------------- *)
section["zero dimensions: one site, spin 1/2 (Z = a^2 - u, G0 = 1/a)"];
(* S = -a Sum_s psibar_s psi_s + u n_up n_down: Z = a^2 - u, G = a/(a^2 - u), Sigma = u/a *)
zeroD[e_] := ZeroDimensionalValue[e, {G0 -> 1/a, V -> u, G -> g}, "LoopFactor" -> 2];
order4 = 4;
vacuum4 = ConnectedVacuumDiagrams[Zf, Vf, order4];
check["log(Z/Z0) = log(1 - u/a^2) to u^4", Expand[zeroD[vacuum4]], Expand[-Sum[(u/a^2)^k/k, {k, order4}]]];
check["G = a/(a^2 - u) to u^3", Expand[zeroD[greenF]], Expand[Sum[u^k/a^(2 k + 1), {k, 0, nmax}]]];
check["Sigma = u/a (the first order is exact)", Expand[zeroD[sigmaF]], u/a];
phi4 = LuttingerWardFunctional[Zf, Vf, order4, G0 -> G];
sigma4 = zeroD[SelfEnergy[phi4, G, {x, y}]];
check["skeleton Sigma[g] = Sum_k (-1)^k Catalan(k) u^(k+1) g^(2k+1): the branch (Sqrt[1 + 4 u g^2] - 1)/(2 g)", sigma4,
  Expand[Sum[(-1)^k Binomial[2 k, k]/(k + 1) u^(k + 1) g^(2 k + 1), {k, 0, order4 - 1}]]];
(* Phi(g) = s - 1 - log((1 + s)/2), s = Sqrt[1 + 4 u g^2]; its series from the binomial series of s and of log(1 + y) *)
sSeries = Sum[Binomial[1/2, k] (4 u g^2)^k, {k, 0, order4}];
truncU[e_] := Sum[Coefficient[Expand[e], u, k] u^k, {k, 0, order4}];
phiClosed = truncU[sSeries - 1 - Sum[(-1)^(j + 1) ((sSeries - 1)/2)^j/j, {j, 1, order4}]];
check["Phi(g) = s - 1 - log((1 + s)/2) to u^4", zeroD[phi4], Expand[phiClosed]];
check["dPhi/dg = 2 Sigma (two spin states)", Expand[D[zeroD[phi4], g] - 2 sigma4], 0];
gamma0d = zeroD[EffectiveAction2PI[phi4, G0 -> G]];
check["Gamma(g) = Phi(g) - log(g^-2) - 2 (a g - 1)", Expand[gamma0d - (zeroD[phi4] - Log[1/g^2] - 2 (a g - 1))], 0];
(* at g = a/(a^2 - u): -log(a^2 - u) up to u^5 *)
check["Gamma at the exact g = -log Z, to u^4", Normal[Series[(gamma0d /. g -> a/(a^2 - u)) + Log[a^2 - u], {u, 0, order4}]] // Simplify, 0];
check["stationary at the exact g, to u^4", Normal[Series[D[gamma0d, g] /. g -> a/(a^2 - u), {u, 0, order4}]] // Simplify, 0];

section["zero dimensions: Gamma2 of phi^3 + phi^4 against the exact Legendre transform"];
(* Z(J, K) = Integral dphi/Sqrt[2 Pi] exp(-phi^2/(2 D0) - c3 phi^3/6 - lam phi^4/24 + J phi + K phi^2/2); at phibar = 0 and
   <phi^2>_c = g: Gamma2 = -log Z + K g/2 - 1/2 log(1/g) - 1/2 (g/D0 - 1). With 1/Gt = 1/D0 - K and m = J Gt the mean of
   the Gaussian, Gamma2 = -1/2 log(Gt/g) - m^2/(2 Gt) - log<exp(-V)>_(m, Gt) - g/(2 Gt) + 1/2 (D0 drops out). The
   conditions <phi> = 0, <phi^2>_c = g fix m and Gt order by order in eps (c3 -> eps c3, lam -> eps^2 lam); everything is
   a polynomial truncated at eps^nEps (no Series). *)
exactGamma2[nEps_Integer] := Module[{eps, trunc, mKnown = 0, tKnown = 0, averages, eqs, s, mm, tt, a0, gam, logSeries, geometric, ph, weights},
   trunc[e_, n_] := Sum[Coefficient[e, eps, k] eps^k, {k, 0, n}];
   weights = Expand[Sum[(-(eps c3 ph^3/6 + eps^2 lam ph^4/24))^k/k!, {k, 0, nEps}]];
   (* <ph^q exp(-V)>, q = 0, 1, 2, to eps^n, for the Gaussian of mean m and variance g (1 + t) *)
   averages[m_, t_, n_] := Module[{mpow, tpow, moment},
     mpow = NestList[trunc[Expand[# m], n] &, 1, n];
     tpow = NestList[trunc[Expand[# (1 + t)], n] &, 1, 2 n + 2];
     moment[p_] := Sum[If[p - j > n, 0, Binomial[p, j] mpow[[p - j + 1]] g^(j/2) tpow[[j/2 + 1]] (j - 1)!!], {j, 0, p, 2}];
     Table[With[{cl = CoefficientList[trunc[Expand[ph^q weights], n], ph]},
       trunc[Expand[Sum[If[cl[[p + 1]] === 0, 0, cl[[p + 1]] moment[p]], {p, 0, Length[cl] - 1}]], n]], {q, 0, 2}]];
   (* <phi> = 0 and <phi^2> = g, order by order: linear in the new coefficients mm, tt *)
   Do[With[{av = averages[mKnown + mm eps^k, tKnown + tt eps^k, k]},
     s = First[Solve[{Coefficient[av[[2]], eps, k] == 0, Coefficient[av[[3]] - g av[[1]], eps, k] == 0}, {mm, tt}]];
     mKnown = mKnown + (mm /. s) eps^k; tKnown = tKnown + (tt /. s) eps^k], {k, 1, nEps}];
   a0 = averages[mKnown, tKnown, nEps][[1]];
   logSeries[x_] := trunc[Expand[Sum[(-1)^(j + 1) x^j/j, {j, 1, nEps}]], nEps];
   geometric = trunc[Expand[Sum[(-tKnown)^j, {j, 0, nEps}]], nEps];
   gam = -1/2 logSeries[tKnown] - trunc[Expand[mKnown^2 geometric], nEps]/(2 g) - logSeries[a0 - 1] - 1/2 geometric + 1/2;
   Expand[gam /. eps -> 1]];
t0 = AbsoluteTime[];
exact0d = exactGamma2[8];
gamma2Cubic = LuttingerWardFunctional[Zb, c3/6 IntegralOver[phi[z]^3, {z}] + lam/24 IntegralOver[phi[z]^4, {z}], 4, D0 -> Gb];
package0d = ZeroDimensionalValue[gamma2Cubic, {Gb -> g}];
monomials4[e_] := Select[MonomialList[e, {c3, lam}], Exponent[#, c3] + Exponent[#, lam] <= 4 &];
check["Gamma2[0, g] of phi^3 + phi^4: all terms with up to four vertices", Sort[monomials4[package0d]] === Sort[monomials4[exact0d]], True];
Print["  (", Round[AbsoluteTime[] - t0, 0.1], " s; ", Length[monomials4[exact0d]], " terms, e.g. ", InputForm[Coefficient[exact0d, c3^2 lam^2]], " c3^2 lam^2)"];

section["zero dimensions: Potthoff's functionals"];
(* F[Sigma] = Phi(g[Sigma]) - 2 Sigma g[Sigma], g[Sigma] = Sigma/(u - Sigma^2) (single valued): F = log(1 - Sigma^2/u),
   universal (no a); with Sigma = u sigma, g = sigma/(1 - u sigma^2), a series in u *)
fFromPhi = ZeroDimensionalValue[LegendreTransform[phi4, G -> Sg], {G -> sig Sum[(u sig^2)^k, {k, 0, order4}], Sg -> u sig, V -> u}, "LoopFactor" -> 2];
check["F[Sigma] = log(1 - Sigma^2/u) (Sigma = u sig), to u^4", truncU[fFromPhi] - truncU[-Sum[(u sig^2)^k/k, {k, 1, order4}]], 0];
fExact[s_] := Log[1 - s^2/u];
omega0d[s_] := fExact[s] - Log[(a - s)^2];
check["Omega_a(S) = F(S) - Tr ln(-(a - S)) is stationary only at S = u/a", Solve[D[omega0d[s], s] == 0, s], {{s -> u/a}}];
check["its value there is -log Z", Simplify[Exp[-omega0d[u/a]] - (a^2 - u)], 0];
check["F from any reference system: -log Z' + Tr ln(-G'^-1) at a' = u/S", Simplify[Exp[(-Log[ap^2 - u] + Log[(ap^2 - u)^2/ap^2]) /. ap -> u/s] - Exp[fExact[s]]], 0];
(* the substitution G -> (a - S)^-1 in Gamma[G] is stationary at the same point but is not universal: at fixed S it
   depends on a also outside the trace-log *)
gammaSub[s_] := (gamma0d /. g -> 1/(a - s));
check["Gamma[(a - S)^-1] + Log[(a - S)^2] depends on a (not universal)", Simplify[D[gammaSub[s] + Log[(a - s)^2], a]] =!= 0, True];
check["Omega_a(S) + Log[(a - S)^2] = F(S) does not", D[omega0d[s] + Log[(a - s)^2], a], 0];
(* two branches of Sigma[g]: g Sigma^2 + Sigma - u g = 0 *)
sigmaPlus[gg_] := (Sqrt[1 + 4 u gg^2] - 1)/(2 gg); sigmaMinus[gg_] := (-Sqrt[1 + 4 u gg^2] - 1)/(2 gg);
gOfA[aa_] := aa/(aa^2 - u);
check["u = 1, a = 3: the physical Sigma = u/a is on the + branch (the skeleton series)", {sigmaPlus[gOfA[3]], sigmaMinus[gOfA[3]]} /. u -> 1, {1/3, -3}];
check["u = 1, a = 1/2: the physical Sigma = u/a = 2 is on the - branch; the + branch gives -a", {sigmaPlus[gOfA[1/2]], sigmaMinus[gOfA[1/2]]} /. u -> 1, {-1/2, 2}];
check["4 u g^2 = 4 r/(r - 1)^2 at the exact g (r = a^2/u, u > 0)", Simplify[4 u gOfA[Sqrt[r u]]^2 - 4 r/(r - 1)^2, u > 0 && r > 0], 0];
check["the skeleton series converges (4 u g^2 < 1) for r < 3 - 2 Sqrt[2] or r > 3 + 2 Sqrt[2]: r = 1/10, 2, 10",
  {Sort[r /. Solve[4 r == (r - 1)^2, r]], (4 r/(r - 1)^2 < 1) /. {{r -> 1/10}, {r -> 2}, {r -> 10}}}, {{3 - 2 Sqrt[2], 3 + 2 Sqrt[2]}, {True, False, True}}];
(* the charge vertex dSigma/dg along the physical curve, and the Jacobian dg/da: L^-1 = 1/g^2 - Gamma0 = -(dg/da)^-1 *)
gamma0Phys = Simplify[D[u/aa, aa]/D[gOfA[aa], aa]];
check["Gamma0 = dSigma/dg = u (a^2 - u)^2/(a^2 (a^2 + u)) on the physical curve", Simplify[gamma0Phys - u (aa^2 - u)^2/(aa^2 (aa^2 + u))], 0];
check["1/g^2 - Gamma0 = -1/(dg/da): Gamma0 diverges where dG/dG0 = 0 (a^2 = -u, u < 0)", Simplify[1/gOfA[aa]^2 - gamma0Phys + 1/D[gOfA[aa], aa]], 0];
check["the branch point 1 + 4 u g^2 = 0 is at a^2 = -u", Simplify[1 + 4 u gOfA[aa]^2 /. aa -> Sqrt[-u]], 0];


(* ------------------------------------------------------------------------- *)
(* Tr ln in the Matsubara evaluation, responses as variations, static and dynamic limits *)
SetAttributes[messageGivenQ, HoldAll];
checkSmall[name_String, value_, tolerance_] := check[name, If[NumericQ[value] && Abs[value] < tolerance, True, value], True];
(* the statistical factors as functions, to differentiate and to evaluate *)
toExponentials = {FermiFunction -> Function[e, 1/(Exp[e/Temperature] + 1)], BoseFunction -> Function[e, 1/(Exp[e/Temperature] - 1)]};
integrandOf[e_] := e /. MomentumSum[f_, _] :> f;
DeclareMatsubaraFrequency[w, "Fermion"]; DeclareMatsubaraFrequency[nu, "Boson"];
twoPoint = {y -> {k, I w}, x -> {-k, -I w}};
bubble = {z2 -> {q, I nu}, z1 -> {-q, -I nu}};
lab = {x1, x2, x3, x4};

(* ------------------------------------------------------------------------- *)
section["sums of logarithms: T Sum ln(a - i w_n) with the three orderings"];
logSums = Table[MatsubaraSum[Log[xi - z], z, "Fermion", "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
check["fermions: -T ln(1 - n_F), + xi with e^(-i w 0+), + xi/2 symmetric (the zero-point term)", logSums,
  {-Temperature Log[1 - FermiFunction[xi]], xi - Temperature Log[1 - FermiFunction[xi]], xi/2 - Temperature Log[1 - FermiFunction[xi]]}];
check["-T ln(1 - n_F(xi)) = T ln(1 + e^(-xi/T)), the grand potential of one level with a minus sign",
  Simplify[(logSums[[1]] /. toExponentials) - Temperature Log[1 + Exp[-xi/Temperature]], Temperature > 0 && Element[xi, Reals]], 0];
check["d/dxi of each is -MatsubaraSum[1/(z - xi)] with the same convergence factor",
  Simplify[Table[D[logSums[[i]] /. toExponentials, xi] + (MatsubaraSum[1/(z - xi), z, "Fermion", "ConvergenceFactor" -> {1, -1, 0}[[i]]] /. toExponentials),
     {i, 3}]], {0, 0, 0}];
check["bosons: -T ln(1 + n_B) + (1 - eta)/2 xi", Table[MatsubaraSum[Log[xi - z], z, "Boson", "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}],
  {-Temperature Log[1 + BoseFunction[xi]], xi - Temperature Log[1 + BoseFunction[xi]], xi/2 - Temperature Log[1 + BoseFunction[xi]]}];
check["a real boson, ln(W^2 - z^2): W - 2 T ln(1 + n_B(W)) with every ordering (1/2 of it is T ln(2 sinh(W/2T)))",
  Union[Table[MatsubaraSum[Log[W^2 - z^2], z, "Boson", "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}]], {W - 2 Temperature Log[1 + BoseFunction[W]]}];
checkSmall["  ... = 2 T ln(2 sinh(W/2T)) (W = 1.3, T = 0.37)", Abs[(W - 2 Temperature Log[1 + BoseFunction[W]] /. toExponentials) -
     2 Temperature Log[2 Sinh[W/(2 Temperature)]] /. {W -> 1.3, Temperature -> 0.37}], 10^-14];
checkSmall["fermions: ln(-a - z) = T ln(1 + e^(a/T)), from the factor written the other way (xi = 0.8, T = 0.37)",
  Abs[(MatsubaraSum[Log[-xi - z], z, "Fermion", "ConvergenceFactor" -> 1] /. toExponentials) - Temperature Log[1 + Exp[xi/Temperature]] /.
     {xi -> 0.8, Temperature -> 0.37}], 10^-14];
check["a ratio of linear factors: zeros minus poles", MatsubaraSum[Log[(z - a)/(z - b)], z, "Fermion", "ConvergenceFactor" -> 1],
  Temperature Log[1 - FermiFunction[b]] - Temperature Log[1 - FermiFunction[a]]];
check["a logarithm plus a rational summand", MatsubaraSum[Log[xi - z] + 1/(z - a), z, "Fermion", "ConvergenceFactor" -> 1],
  FermiFunction[a] - Temperature Log[1 - FermiFunction[xi]]];
check["Automatic: a message when the ordering matters, none when it does not", {messageGivenQ[MatsubaraSum[Log[xi - z], z, "Fermion"], MatsubaraSum::conv],
   messageGivenQ[MatsubaraSum[Log[W^2 - z^2], z, "Boson"], MatsubaraSum::conv]}, {True, False}];
check["a logarithm times a function of z: a message and $Failed", {messageGivenQ[MatsubaraSum[Log[xi - z]/(z - a), z, "Fermion"], MatsubaraSum::log],
   Quiet[MatsubaraSum[Log[xi - z]/(z - a), z, "Fermion"], MatsubaraSum::log]}, {True, $Failed}];
(* brute force: convergent combinations (symmetric partial sums) *)
bruteLog[f_, stat_, t_, n_] := t Total[f /. z -> (I Pi t #) & /@ If[stat === "Fermion", Range[-2 n + 1, 2 n - 1, 2], Range[-2 n, 2 n, 2]]];
numericStatistics[t_] := {Temperature -> t, FermiFunction -> Function[e, 1/(Exp[e/t] + 1)], BoseFunction -> Function[e, 1/(Exp[e/t] - 1)]};
checkSmall["brute force: T Sum ln((0.8 - z)/(1.3 - z)), fermions, symmetric sum",
  Abs[(MatsubaraSum[Log[(0.8 - z)/(1.3 - z)], z, "Fermion", "ConvergenceFactor" -> 0] /. numericStatistics[0.37]) -
    bruteLog[Log[(0.8 - z)/(1.3 - z)], "Fermion", 0.37, 100000]], 10^-5];
checkSmall["brute force: T Sum ln((0.8^2 - z^2)/(1.3^2 - z^2)), bosons",
  Abs[(MatsubaraSum[Log[(0.64 - z^2)/(1.69 - z^2)], z, "Boson"] /. numericStatistics[0.37]) -
    bruteLog[Log[(0.64 - z^2)/(1.69 - z^2)], "Boson", 0.37, 100000]], 10^-5];
check["T -> 0: T ln(1 - n_F(xi)) -> xi theta(-xi)", ZeroTemperatureLimit[logSums[[1]]], -xi UnitStep[-xi]];

(* ------------------------------------------------------------------------- *)
section["the grand potential: free gas, Hartree-Fock, free bosons"];
rulesFree = {G0 -> {"Fermion", xi}, G -> {"Fermion", xi}, V -> {"Instantaneous", v}};
gammaFree = EffectiveAction2PI[0, G0 -> G];
omegaFree = Table[EvaluateMatsubara[gammaFree, {}, rulesFree, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
check["Omega_0/V = -Tr ln(-G0^-1) = -2 T Integral ln(1 + e^(-xi/T)), - (1 - eta) Integral xi for the other orderings",
  Simplify[(integrandOf[omegaFree] - {2 Temperature Log[1 - FermiFunction[xi[p]]], 2 Temperature Log[1 - FermiFunction[xi[p]]] - 2 xi[p],
       2 Temperature Log[1 - FermiFunction[xi[p]]] - xi[p]}) /. toExponentials], {0, 0, 0}];
check["  ... and 2 T ln(1 - n_F) = -2 T ln(1 + e^(-xi/T))", Simplify[(2 Temperature Log[1 - FermiFunction[xi[p]]] /. toExponentials) +
     2 Temperature Log[1 + Exp[-xi[p]/Temperature]], Temperature > 0 && Element[xi[p], Reals]], 0];
densityFree = Table[EvaluateMatsubara[OperatorTrace[G0], {}, rulesFree, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
check["N/V = Tr G0: 2 n_F, 2 (n_F - 1), 2 (n_F - 1/2)", Expand[integrandOf[densityFree]], {2 FermiFunction[xi[p]], 2 FermiFunction[xi[p]] - 2, 2 FermiFunction[xi[p]] - 1}];
check["-dOmega_0/dmu = N with each ordering (xi = eps - mu)",
  Simplify[-D[integrandOf[omegaFree] /. xi -> Function[s, eps[s] - mu] /. toExponentials, mu] - (integrandOf[densityFree] /. xi -> Function[s, eps[s] - mu] /. toExponentials)],
  {0, 0, 0}];
check["T -> 0: Omega_0/V = 2 Integral xi theta(-xi)", ZeroTemperatureLimit[omegaFree[[1]]], 2 MomentumSum[UnitStep[-xi[p]] xi[p], {p}]];
check["Tr 1 = 0, Tr ln of a number = 0", EvaluateMatsubara[OperatorTrace[1] + TraceLog[-1], {}, rulesFree], 0];
check["a trace with a kernel without rule: a message and $Failed", {messageGivenQ[EvaluateMatsubara[TraceLog[-Power[Gq, -1]], {}, rulesFree], EvaluateMatsubara::kernel],
   Quiet[EvaluateMatsubara[TraceLog[-Power[Gq, -1]], {}, rulesFree], EvaluateMatsubara::kernel]}, {True, $Failed}];
checkMessage[EvaluateMatsubara[TraceLog[-Power[G, -1]] IntegralOver[G[u, u], {u}], {}, rulesFree], "a trace times a diagram: a message and $Failed",
  EvaluateMatsubara::trace, $Failed];

(* Hartree-Fock: Gamma with Phi of first order, at G = G_HF = 1/(i w - en[k]), en = xi + Sigma_HF *)
phi1 = LuttingerWardFunctional[Zf, Vf, 1, G0 -> G];
gammaHF = EffectiveAction2PI[phi1, G0 -> G];
rulesHF = {G0 -> {"Fermion", xi}, G -> {"Fermion", en}, V -> {"Instantaneous", v}};
omegaHF = Table[EvaluateMatsubara[gammaHF, {}, rulesHF, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
densityHF = Table[EvaluateMatsubara[OperatorTrace[G], {}, rulesHF, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
sigmaHF = Table[EvaluateMatsubara[SelfEnergy[phi1, G, {x, y}], twoPoint, rulesHF, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1}, "ConvergenceFactor" -> eta],
   {eta, {1, -1, 0}}];
Print["  Omega_HF/V = ", InputForm[omegaHF[[1]]]];
(* a ring of M sites (momenta 2 Pi j/M): xi = -2 cos k - mu, v(q) = U0 + 2 V1 cos q; the self-consistent Hartree-Fock
   energies en[k] = xi[k] + Sigma_HF[k] at each mu, by iteration; Omega_HF(mu) and N(mu) from the expressions above *)
ringM = 8; ringT = 0.25; ringU0 = 0.6; ringV1 = 0.25;
ringMomenta = N[2 Pi Range[0, ringM - 1]/ringM];
ringRules[mu_, ens_] := {xi -> Function[s, -2 Cos[s] - mu], v -> Function[s, ringU0 + 2 ringV1 Cos[s]],
   en -> Function[s, ens[[1 + Mod[Round[s ringM/(2 Pi)], ringM]]]], Temperature -> ringT,
   FermiFunction -> Function[e, 1/(Exp[e/ringT] + 1)], BoseFunction -> Function[e, 1/(Exp[e/ringT] - 1)]};
ringValue[e_, mu_, ens_] := Chop[N[e /. MomentumSum[f_, ps_] :> Total[Table[f /. Thread[ps -> t], {t, Tuples[ringMomenta, Length[ps]]}]]/ringM^Length[ps] /.
      ringRules[mu, ens]]];
selfConsistentHF[mu_, i_] := selfConsistentHF[mu, i] = Module[{ens = -2 Cos[ringMomenta] - mu, new, n = 0},
   While[n < 400, new = Table[-2 Cos[kk] - mu + ringValue[sigmaHF[[i]] /. k -> kk, mu, ens], {kk, ringMomenta}];
    If[Max[Abs[new - ens]] < 10^-14, ens = new; Break[]]; ens = 1/2 (ens + new); n++];
   ens];
omegaRing[mu_, i_] := ringValue[omegaHF[[i]], mu, selfConsistentHF[mu, i]];
densityRing[mu_, i_] := ringValue[densityHF[[i]], mu, selfConsistentHF[mu, i]];
derivative5[f_, x0_, h_] := (-f[x0 + 2 h] + 8 f[x0 + h] - 8 f[x0 - h] + f[x0 - 2 h])/(12 h);
secondDerivative5[f_, x0_, h_] := (-f[x0 + 2 h] + 16 f[x0 + h] - 30 f[x0] + 16 f[x0 - h] - f[x0 - 2 h])/(12 h^2);
ringMu = 0.3;
Print["  ring: Sigma_HF converged, N = ", densityRing[ringMu, 1], ", Omega = ", omegaRing[ringMu, 1]];
Do[checkSmall["ring, Hartree-Fock: -dOmega/dmu = N, " <> {"e^(i w 0+)", "e^(-i w 0+)", "symmetric"}[[i]] <> " (by stationarity, Sigma depends on mu)",
   derivative5[omegaRing[#, i] &, ringMu, 10^-3] + densityRing[ringMu, i], 10^-8], {i, 3}];
check["  not trivially: Sigma_HF depends on mu", Abs[ringValue[sigmaHF[[1]] /. k -> 0., ringMu + 0.01, selfConsistentHF[ringMu + 0.01, 1]] -
     ringValue[sigmaHF[[1]] /. k -> 0., ringMu, selfConsistentHF[ringMu, 1]]] > 10^-4, True];
traceLogHF = EvaluateMatsubara[-TraceLog[-Power[G, -1]], {}, rulesHF, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1}];
checkSmall["ring: Omega_HF = -Tr ln(-G^-1) - 1/2 Tr(Sigma G) at the stationary point (Luttinger and Ward's form)",
  omegaRing[ringMu, 1] - ringValue[traceLogHF - MomentumSum[(en[p1] - xi[p1]) FermiFunction[en[p1]], {p1}], ringMu, selfConsistentHF[ringMu, 1]], 10^-12];

(* free bosons: 1/2 Tr ln D0^-1 with <phi phi>_0 = 1/(W[k]^2 - z^2) *)
rulesBoson = {D0 -> {"Boson", Function[{s, zz}, 1/(om[s]^2 - zz^2)]}, Gb -> {"Boson", Function[{s, zz}, 1/(om[s]^2 - zz^2)]}};
omegaBoson = Table[EvaluateMatsubara[EffectiveAction2PI[0, D0 -> Gb, "Statistics" -> "Boson"], {}, rulesBoson, "LoopMomenta" -> {p}, "ConvergenceFactor" -> eta],
   {eta, {1, -1, 0}}];
check["free bosons: Omega/V = Integral (W/2 + T ln(1 - e^(-W/T))), the same with every ordering (zero-point energy included)",
  {Length[Union[omegaBoson]], Simplify[(integrandOf[omegaBoson[[1]]] /. toExponentials) - (om[p]/2 + Temperature Log[1 - Exp[-om[p]/Temperature]]),
     Temperature > 0 && om[p] > 0]}, {1, 0}];

(* phi^4 in the Hartree approximation on the ring: om[k]^2 = m2 + 2 (1 - cos k), M[k]^2 = om^2 - Sigma_H *)
gamma2H = LuttingerWardFunctional[Zb, Vb, 1, D0 -> Gb];
gammaH = EffectiveAction2PI[gamma2H, D0 -> Gb, "Statistics" -> "Boson"];
rulesH = {D0 -> {"Boson", Function[{s, zz}, 1/(om[s]^2 - zz^2)]}, Gb -> {"Boson", Function[{s, zz}, 1/(bm[s]^2 - zz^2)]}};
omegaH = EvaluateMatsubara[gammaH, {}, rulesH, "LoopMomenta" -> {p1, p2}];
sigmaH = EvaluateMatsubara[SelfEnergy[gamma2H, Gb, {x, y}, "Statistics" -> "Boson"], {y -> {k, I nu}, x -> {-k, -I nu}}, rulesH, "LoopMomenta" -> {p1}];
fluctuationH = EvaluateMatsubara[OperatorTrace[Gb], {}, rulesH, "LoopMomenta" -> {p1}];
Print["  Omega_H/V = ", InputForm[omegaH], "\n  Sigma_H = ", InputForm[sigmaH]];
ringLambda = 0.8;
bosonRules[m2_, masses_] := {om -> Function[s, Sqrt[m2 + 2 - 2 Cos[s]]], bm -> Function[s, masses[[1 + Mod[Round[s ringM/(2 Pi)], ringM]]]],
   lambda -> ringLambda, Temperature -> ringT, BoseFunction -> Function[e, 1/(Exp[e/ringT] - 1)]};
bosonValue[e_, m2_, masses_] := Chop[N[e /. MomentumSum[f_, ps_] :> Total[Table[f /. Thread[ps -> t], {t, Tuples[ringMomenta, Length[ps]]}]]/ringM^Length[ps] /.
      bosonRules[m2, masses]]];
selfConsistentH[m2_] := selfConsistentH[m2] = Module[{masses = Sqrt[m2 + 2 - 2 Cos[ringMomenta]], new, n = 0},
   While[n < 400, new = Sqrt[m2 + 2 - 2 Cos[ringMomenta] - bosonValue[sigmaH, m2, masses]];
    If[Max[Abs[new - masses]] < 10^-14, masses = new; Break[]]; masses = 1/2 (masses + new); n++];
   masses];
omegaHRing[m2_] := bosonValue[omegaH, m2, selfConsistentH[m2]];
checkSmall["ring, phi^4 Hartree: dOmega/dm^2 = 1/2 <phi^2> = 1/2 Tr G (the mass is the source of phi^2/2)",
  derivative5[omegaHRing, 0.5, 10^-3] - 1/2 bosonValue[fluctuationH, 0.5, selfConsistentH[0.5]], 10^-8];

(* ------------------------------------------------------------------------- *)
section["expectation values and responses as variations"];
DeclareDirectedPropagator[U]; DeclareSymmetric[Ub];
check["fermions: dGamma/dU(x, y) = G(y, x) = <psibar(x) psi(y)>, only through the trace term (any Phi)",
  FunctionalDerivative[EffectiveAction2PI[phi1, G0 -> G, "Source" -> U], U[x, y]], G[y, x]];
check["real bosons: dGamma/dU(x, y) = 1/2 G(x, y) = 1/2 <phi(x) phi(y)>",
  FunctionalDerivative[EffectiveAction2PI[gamma2H, D0 -> Gb, "Statistics" -> "Boson", "Source" -> Ub], Ub[x, y]], Gb[x, y]/2];
(* zero dimensions, one site with spin 1/2: Z(U) = (a - U)^2 - u, the skeleton Phi(g) in closed form (physical branch for a^2 > u) *)
phiClosed0d[gg_] := With[{s = Sqrt[1 + 4 u gg^2]}, s - 1 - Log[(1 + s)/2]];
gammaU0d = ZeroDimensionalValue[EffectiveAction2PI[0, G0 -> G, "Source" -> U], {G0 -> 1/a, G -> g, U -> uu}, "LoopFactor" -> 2] + phiClosed0d[g];
omegaExact0d = -Log[(a - uu)^2 - u];
gStar = (a - uu)/((a - uu)^2 - u);
values0d = {a -> 3, u -> 1, uu -> 1/5};
check["0d: Gamma(g; U) is stationary at the exact g(U)", Simplify[D[gammaU0d, g] /. g -> gStar /. values0d], 0];
check["0d: its value there is Omega(U) = -log Z(U)", Simplify[(gammaU0d /. g -> gStar) - omegaExact0d /. values0d], 0];
check["0d: dOmega/dU = dGamma/dU at fixed g = 2 g (explicit dependence only)", Simplify[(D[omegaExact0d, uu] - D[gammaU0d, uu] /. g -> gStar) /. values0d], 0];
sigma0d[gg_] := D[phiClosed0d[gg], gg]/2;
check["0d: d^2 Omega/dU^2 = chi = 2 L, L = L0 + L0 Gamma0 L with L0 = g^2 and Gamma0 = dSigma/dg",
  Simplify[(D[omegaExact0d, {uu, 2}] - 2/(1/g^2 - D[sigma0d[g], g]) /. g -> gStar) /. values0d], 0];

(* the Bethe-Salpeter equation of the response against the perturbative four-point function, to second order: the
   skeleton diagrams of L (G0 -> G, no self-energy insertions) are the solution of L = L0 + L0 Gamma0 L with Gamma0 the
   kernel of Phi; the same for chi and for the vertex *)
nResponse = 2;
phiF2 = If[ValueQ[phiF], OrderPart[phiF, V, Range[1, nResponse]], LuttingerWardFunctional[Zf, Vf, nResponse, G0 -> G]];
t0 = AbsoluteTime[];
skeletonF[e_, labels_] := SimplifyDiagrams[SkeletonDiagrams[e, labels, {G0}] /. G0 -> G];
fourF = ConnectedCorrelationFunction[Zf, {psi[x1], psibar[x2], psi[x3], psibar[x4]}, Vf, nResponse];
responseF = LabelFunction[-skeletonF[fourF, lab] + G[x1, x4] G[x3, x2], lab];
gamma0F = LabelFunction[IrreducibleVertex[phiF2, G, lab], lab];
check["electron gas: L = L0 + L0 Gamma0 L, Gamma0 = dSigma/dG of Phi, L = -<psi psibar psi psibar>_c + G G in skeleton form (orders 0-2)",
  CheckEquation[ExactEquation["Response", {"L" -> responseF, "Gamma0" -> gamma0F, "G" -> G}, lab, "MaximumOrder" -> {V, nResponse}], V, nResponse],
  Table[0, {nResponse + 1}]];
densityF = -ConnectedCorrelationFunction[Zf, {psibar[z1], psi[z1], psibar[z2], psi[z2]}, Vf, nResponse];
check["electron gas: chi = gamma L gamma = -<rho rho>_c (orders 0-2)",
  CheckEquation[ExactEquation["Susceptibility", {"Chi" -> LabelFunction[skeletonF[densityF, {z1, z2}], {z1, z2}], "L" -> responseF}, {z1, z2}], V, nResponse],
  Table[0, {nResponse + 1}]];
lambdaF = PerturbativeDensityVertex[Zf, Vf, nResponse, {psi, psibar}, {x1, x2, z}, G0 -> G];
check["electron gas: Lambda = gamma + Gamma0 G G Lambda with the density given as \"BareVertex\" (orders 0-2)",
  CheckEquation[ExactEquation["Vertex", {"Lambda" -> LabelFunction[lambdaF, {x1, x2, z}], "Gamma0" -> gamma0F, "G" -> G,
     "BareVertex" -> Function[{p, r, s}, DeltaFunction[p, s] DeltaFunction[r, s]]}, {x1, x2, z}, "MaximumOrder" -> {V, nResponse}], V, nResponse],
  Table[0, {nResponse + 1}]];
(* a general one-body operator O(z) = Integral psibar(a) gam(a, b; z) psi(b) (current, spin, ...): chi = -<O O>_c, the fully
   connected part and the pair term -<psibar1 psi4><psi2 psibar3> = G(x4, x1) G(x2, x3) *)
fourBar = ConnectedCorrelationFunction[Zf, {psibar[x1], psi[x2], psibar[x3], psi[x4]}, Vf, nResponse];
chiGeneral = SimplifyDiagrams[-IntegralOver[gam[x1, x2, z1] skeletonF[fourBar, lab] gam[x3, x4, z2], lab] +
    IntegralOver[gam[x1, x2, z1] gam[x3, x4, z2] G[x4, x1] G[x2, x3], lab]];
check["electron gas: a general bare vertex gamma, chi = gamma L gamma = -<O O>_c (orders 0-2)",
  CheckEquation[ExactEquation["Susceptibility", {"Chi" -> LabelFunction[chiGeneral, {z1, z2}], "L" -> responseF, "BareVertex" -> gam}, {z1, z2}], V, nResponse],
  Table[0, {nResponse + 1}]];
Print["  (electron gas, response to second order: ", Round[AbsoluteTime[] - t0, 0.1], " s)"];

t0 = AbsoluteTime[];
phiB2 = If[ValueQ[phiB], OrderPart[phiB, lambda, Range[1, nResponse]], LuttingerWardFunctional[Zb, Vb, nResponse, D0 -> Gb]];
skeletonB[e_, labels_] := SimplifyDiagrams[SkeletonDiagrams[e, labels, {D0}] /. D0 -> Gb];
fourB = ConnectedCorrelationFunction[Zb, {phi[x1], phi[x2], phi[x3], phi[x4]}, Vb, nResponse];
responseB = LabelFunction[skeletonB[fourB, lab] + Gb[x1, x3] Gb[x2, x4] + Gb[x1, x4] Gb[x2, x3], lab];
gamma0B = LabelFunction[IrreducibleVertex[phiB2, Gb, lab, "Statistics" -> "Boson"], lab];
check["phi^4: L = L0 - 1/2 L0 Gamma0 L, L = <phi phi phi phi>_c + G G + G G = -2 dG/dU in skeleton form (orders 0-2)",
  CheckEquation[ExactEquation["Response", {"L" -> responseB, "Gamma0" -> gamma0B, "G" -> Gb}, lab, "Statistics" -> "Boson",
    "MaximumOrder" -> {lambda, nResponse}], lambda, nResponse], Table[0, {nResponse + 1}]];
densityB = -1/4 ConnectedCorrelationFunction[Zb, {phi[z1], phi[z1], phi[z2], phi[z2]}, Vb, nResponse];
check["phi^4: chi = -1/4 gamma L gamma = -<rho rho>_c for rho = phi^2/2 (orders 0-2)",
  CheckEquation[ExactEquation["Susceptibility", {"Chi" -> LabelFunction[skeletonB[densityB, {z1, z2}], {z1, z2}], "L" -> responseB}, {z1, z2},
    "Statistics" -> "Boson"], lambda, nResponse], Table[0, {nResponse + 1}]];
Print["  (phi^4, response to second order: ", Round[AbsoluteTime[] - t0, 0.1], " s)"];

(* ------------------------------------------------------------------------- *)
section["static and dynamic limits; the compressibility sum rule"];
lindhardF = SimplifyDiagrams[-ConnectedCorrelationFunction[Zf, {psibar[z1], psi[z1], psibar[z2], psi[z2]}, Vf, 0]];
check["Lindhard, static limit (q -> 0 at zero frequency): 2 Integral n_F'(xi) = -dN/dmu",
  EvaluateMatsubara[lindhardF, bubble, rulesFree, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}, "Limit" -> "Static"], 2 MomentumSum[FermiFunction'[xi[p]], {p}]];
check["  ... = d^2 Omega_0/dmu^2", Simplify[(D[integrandOf[omegaFree[[1]]] /. xi -> Function[s, eps[s] - mu], {mu, 2}] -
      2 FermiFunction'[eps[p] - mu]) /. toExponentials], 0];
check["Lindhard, dynamic limit (q = 0, then frequency -> 0): 0, the density is conserved",
  EvaluateMatsubara[lindhardF, bubble, rulesFree, "SpinDegeneracy" -> 2, "Limit" -> "Dynamic"], 0];
lindhardFull = EvaluateMatsubara[lindhardF, bubble, rulesFree, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}];
check["Lindhard at small q, zero frequency, tends to the static limit (one dimension, T = 0.2, xi = p^2/2 - 1/2)",
 With[{f = integrandOf[lindhardFull] /. {nu -> 0, xi -> Function[s, s^2/2 - 1/2]} /. numericStatistics[0.2]},
  Abs[NIntegrate[f /. q -> 10^-3, {p, -5, 5}] - NIntegrate[2 FermiFunction'[p^2/2 - 1/2] /. numericStatistics[0.2], {p, -5, 5}]] < 10^-4], True];
checkMessage[EvaluateMatsubara[lindhardF, bubble, rulesFree, "Limit" -> "Other"], "\"Limit\" -> \"Other\": a message and $Failed", EvaluateMatsubara::limit,
  $Failed];

(* time-dependent Hartree-Fock: L from the Bethe-Salpeter equation with Gamma0 = dSigma_HF/dG (bare V, direct and exchange)
   and Hartree-Fock lines, order by order in V *)
gamma0HF = LabelFunction[IrreducibleVertex[phi1, G, lab], lab];
nextL[l_] := ExactEquation["Response", {"L" -> LabelFunction[l, lab], "Gamma0" -> gamma0HF, "G" -> G}, lab][[2]];
lTDHF = NestList[nextL, G[x1, x4] G[x3, x2], 3];
chiTDHF = Table[ExactEquation["Susceptibility", {"Chi" -> chi, "L" -> LabelFunction[l, lab]}, {z1, z2}][[2]], {l, lTDHF}];
chiTDHFOrders = Prepend[Table[SimplifyDiagrams[chiTDHF[[i + 1]] - chiTDHF[[i]]], {i, 3}], chiTDHF[[1]]];
check["TDHF: 1, 2, 4, 8 diagrams at orders 0-3", Length[DiagramTerms[#]] & /@ chiTDHFOrders, {1, 2, 4, 8}];
staticTDHF = Table[EvaluateMatsubara[c, bubble, rulesHF, "SpinDegeneracy" -> 2, "Limit" -> "Static"], {c, chiTDHFOrders}];
dynamicTDHF = Table[EvaluateMatsubara[c, bubble, rulesHF, "SpinDegeneracy" -> 2, "Limit" -> "Dynamic"], {c, chiTDHFOrders}];
check["TDHF, dynamic limit: 0 at every order (Phi-derivable: the density is conserved)", dynamicTDHF, {0, 0, 0, 0}];
Print["  TDHF static, first order: ", InputForm[staticTDHF[[2]]]];
(* the static limit summed to all orders on the ring: Lambda_k = 1 + Sum_p w_kp n'_p Lambda_p, w = 2 v(0) - v(k - p), chi = 2 Sum n'_k Lambda_k *)
ensMu = selfConsistentHF[ringMu, 1];
nPrime = Table[ringValue[FermiFunction'[en[kk]], ringMu, ensMu], {kk, ringMomenta}];
wMatrix = Table[ringValue[2 v[0] - v[kk - pp], ringMu, ensMu], {kk, ringMomenta}, {pp, ringMomenta}];
kernelMatrix = wMatrix . DiagonalMatrix[nPrime]/ringM;
neumann = NestList[kernelMatrix . # &, ConstantArray[1., ringM], 3];
checkSmall["ring: the package's static TDHF orders 0-3 are the terms of the Bethe-Salpeter series",
  Max[Abs[Table[ringValue[staticTDHF[[i]], ringMu, ensMu], {i, 4}] - Table[2 nPrime . neumann[[i]]/ringM, {i, 4}]]], 10^-12];
chiStaticAll = 2 nPrime . LinearSolve[IdentityMatrix[ringM] - kernelMatrix, ConstantArray[1., ringM]]/ringM;
omega2 = secondDerivative5[omegaRing[#, 1] &, ringMu, 10^-3];
Print["  ring: d^2 Omega_HF/dmu^2 = ", omega2, ", TDHF static chi = ", chiStaticAll, ", bubble of HF lines alone = ", 2 Total[nPrime]/ringM];
checkSmall["compressibility sum rule: d^2 Omega_HF/dmu^2 = static limit of the TDHF response", omega2 - chiStaticAll, 10^-6];
checkSmall["  and = -dN/dmu", omega2 + derivative5[densityRing[#, 1] &, ringMu, 10^-3], 10^-6];
check["  the bubble of Hartree-Fock lines alone (no vertex corrections) violates it", Abs[omega2 - 2 Total[nPrime]/ringM] > 10^-2, True];
(* the bare bubble with the first-order self-energy inserted, without the vertex corrections: a static self-energy keeps
   chi(q = 0, i nu) = 0, but the static limit misses the vertex corrections *)
sigma1Bare = SelfEnergy[phi1, G, {u1, u2}] /. G -> G0;
gFirst = G0[z1, z2] + IntegralOver[G0[z1, u1] (sigma1Bare) G0[u2, z2], {u1, u2}];
insertionsOnly = OrderPart[SimplifyDiagrams[gFirst (gFirst /. {z1 -> z2, z2 -> z1})], V, 1];
exactFirst = OrderPart[SimplifyDiagrams[-ConnectedCorrelationFunction[Zf, {psibar[z1], psi[z1], psibar[z2], psi[z2]}, Vf, 1]], V, 1];
check["first order, exact (self-energy insertions and vertex corrections): dynamic limit 0",
  EvaluateMatsubara[exactFirst, bubble, rulesFree, "SpinDegeneracy" -> 2, "Limit" -> "Dynamic"], 0];
check["first order, insertions only: chi(q = 0, i nu) = 0 too (a static self-energy), the static limit differs from the exact one",
  {EvaluateMatsubara[insertionsOnly, {z2 -> {0, I nu}, z1 -> {0, -I nu}}, rulesFree, "SpinDegeneracy" -> 2],
   SimplifyDiagrams[EvaluateMatsubara[insertionsOnly - exactFirst, bubble, rulesFree, "SpinDegeneracy" -> 2, "Limit" -> "Static"]] === 0}, {0, False}];

(* phi^4, rho = phi^2/2 (not conserved): Hartree and its response (bubble chains of the local vertex) *)
gamma0H = LabelFunction[IrreducibleVertex[gamma2H, Gb, lab, "Statistics" -> "Boson"], lab];
nextLB[l_] := ExactEquation["Response", {"L" -> LabelFunction[l, lab], "Gamma0" -> gamma0H, "G" -> Gb}, lab, "Statistics" -> "Boson"][[2]];
lH = NestList[nextLB, Gb[x1, x3] Gb[x2, x4] + Gb[x1, x4] Gb[x2, x3], 2];
chiH = Table[ExactEquation["Susceptibility", {"Chi" -> chi, "L" -> LabelFunction[l, lab]}, {z1, z2}, "Statistics" -> "Boson"][[2]], {l, lH}];
chiHOrders = Prepend[Table[SimplifyDiagrams[chiH[[i + 1]] - chiH[[i]]], {i, 2}], chiH[[1]]];
staticH = Table[EvaluateMatsubara[c, bubble, rulesH, "Limit" -> "Static"], {c, chiHOrders}];
dynamicH = Table[EvaluateMatsubara[c, bubble, rulesH, "Limit" -> "Dynamic"], {c, chiHOrders}];
massesRing = selfConsistentH[0.5];
staticHValues = Table[bosonValue[s, 0.5, massesRing], {s, staticH}];
dynamicHValues = Table[bosonValue[s, 0.5, massesRing], {s, dynamicH}];
Print["  phi^4 Hartree response at orders 0-2, static ", staticHValues, ", dynamic ", dynamicHValues];
checkSmall["phi^4: the static Hartree series is geometric (a local vertex: bubble chains at q = 0)",
  staticHValues[[3]]/staticHValues[[2]] - staticHValues[[2]]/staticHValues[[1]], 10^-10];
checkSmall["phi^4: d^2 Omega_H/d(m^2)^2 = static limit of the Hartree response (the sum rule for the mass)",
  secondDerivative5[omegaHRing, 0.5, 10^-3] - staticHValues[[1]]/(1 - staticHValues[[2]]/staticHValues[[1]]), 10^-6];
check["phi^4: phi^2 is not conserved, the dynamic limit differs from the static one", Abs[dynamicHValues[[1]] - staticHValues[[1]]] > 10^-3, True];

(* ------------------------------------------------------------------------- *)
(* the field dependence: Gamma[phibar, G], Grassmann means, Lambda, the 1PI action Gamma[phibar], zero dimensions, vertex divergences *)
zeroByOrder[e_, x_, orders_List] := Table[SimplifyDiagrams[OrderPart[e, x, m]], {m, orders}];
ZbG = Exp[1/2 IntegralOver[J[x] Gb[x, y] J[y], {x, y}]];
labels4 = {x1, x2, x3, x4};

(* ------------------------------------------------------------------------- *)
section["Gamma[phibar, G] of Cornwall, Jackiw and Tomboulis: phi^4"];
(* Gamma[phibar, G] = S[phibar] + 1/2 Tr ln G^-1 + 1/2 Tr(D^-1[phibar] G - 1) + Gamma2[phibar, G], D^-1[phibar] = D0^-1 + lambda phibar^2/2;
   Gamma2 = minus the 2PI vacuum diagrams with lines G and the vertices lambda phibar phi^3/6, lambda phi^4/24 *)
gammaPhi = EffectiveAction2PI[Zb, Vb, 3, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}];
phiB3 = LuttingerWardFunctional[Zb, Vb, 3, D0 -> Gb];
classicalB = 1/2 IntegralOver[OperatorKernel[Power[D0, -1]][x, y] phiMean[x] phiMean[y], {x, y}] + lambda/24 IntegralOver[phiMean[z]^4, {z}];
oneLoopB = 1/2 TraceLog[Power[Gb, -1]] + 1/2 OperatorTrace[Power[D0, -1] . Gb - 1] + lambda/4 IntegralOver[phiMean[z]^2 Gb[z, z], {z}];
gamma2Phi = SimplifyDiagrams[gammaPhi - classicalB - oneLoopB];
check["Gamma - S[phibar] - (one loop with D^-1[phibar] = D0^-1 + lambda phibar^2/2) leaves Gamma2: diagrams in G only",
  FreeQ[gamma2Phi, D0 | TraceLog | OperatorTrace] && ! FreeQ[gamma2Phi, phiMean], True];
check["phibar = 0: Gamma[0, G] is Gamma[G] on the Luttinger-Ward functional",
  SimplifyDiagrams[SetSourcesToZero[gammaPhi] - EffectiveAction2PI[phiB3, D0 -> Gb, "Statistics" -> "Boson"]], 0];
check["the phibar-dependence of Gamma2 at lambda^2: the sunset with two vertices lambda phibar",
  SimplifyDiagrams[OrderPart[gamma2Phi - SetSourcesToZero[gamma2Phi], lambda, 2] +
    lambda^2/12 IntegralOver[phiMean[u] Gb[u, w]^3 phiMean[w], {u, w}]], 0];
degreesPhi = ({FunctionalDiagrams`Private`meanDegreeOfTerm[#, {phiMean}], Exponent[#[[1]], lambda]} &) /@ FunctionalDiagrams`Private`canonicalTermsOf[gamma2Phi];
check["Gamma2 contains even powers of phibar only, at most one per vertex (degree in phibar <= order in lambda)",
  AllTrue[degreesPhi, EvenQ[#[[1]]] && #[[1]] <= #[[2]] &], True];
(* the two equations of motion *)
dGammaDPhi = FunctionalDerivative[gammaPhi, phiMean[a]];
check["field equation, lambda^0 and lambda^1: D0^-1 phibar + lambda phibar^3/6 + lambda phibar G(a, a)/2 (+ dGamma2/dphibar from lambda^2)",
  {SimplifyDiagrams[OrderPart[dGammaDPhi, lambda, 0] - IntegralOver[OperatorKernel[Power[D0, -1]][a, y] phiMean[y], {y}]],
   SimplifyDiagrams[OrderPart[dGammaDPhi, lambda, 1] - lambda/6 phiMean[a]^3 - lambda/2 phiMean[a] Gb[a, a]]}, {0, 0}];
sigmaPhi = SelfEnergy[gamma2Phi, Gb, {a, b}, "Statistics" -> "Boson"];
check["Dyson equation: dGamma/dG(a, b) = 1/2 (D0^-1 + lambda phibar^2/2 - G^-1 - Sigma[phibar, G]), Sigma = -2 dGamma2/dG",
  SimplifyDiagrams[FunctionalDerivative[gammaPhi, Gb[a, b]] - 1/2 (OperatorKernel[Power[D0, -1]][a, b] + lambda/2 phiMean[a]^2 DeltaFunction[a, b] -
       OperatorKernel[Power[Gb, -1]][a, b] - sigmaPhi)], 0];
(* the self-energy in the background, perturbatively: lines D[phibar] (head Dp), vertices lambda Phb phi^3/6 and lambda phi^4/24
   with a mean Phb that is not a source *)
DeclareSymmetric[Dp];
vertsPhb = lambda/6 IntegralOver[Phb[z] phi[z]^3, {z}] + Vb;
greenPhb = ConnectedCorrelationFunction[Exp[1/2 IntegralOver[J[x] Dp[x, y] J[y], {x, y}]], {phi[a], phi[b]}, vertsPhb, 3];
sigmaPert = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenPhb, 1, {a, b}, {Dp}], {a, b}, {Dp}];
skeletonPhb = SkeletonDiagrams[sigmaPert, {a, b}, {Dp}] /. Dp -> Gb;
check["Sigma[phibar, G] = the skeletons of the 1PI self-energy in the background (SkeletonDiagrams), orders 1-3",
  zeroByOrder[skeletonPhb - (sigmaPhi /. phiMean -> Phb), lambda, {1, 2, 3}], {0, 0, 0}];
twoPIPhb = ParticleIrreducibleDiagrams[sigmaPert, 2, {a, b}, {Dp}] /. Dp -> Gb;
lostPhb = SimplifyDiagrams[skeletonPhb - twoPIPhb];
check["pitfall: selected as 2PI open graphs, Sigma loses the bubble with two vertices lambda phibar (lambda^2)",
  SimplifyDiagrams[OrderPart[lostPhb, lambda, 2] - lambda^2/2 Phb[a] Phb[b] Gb[a, b]^2], 0];
Print["  (skeletons that are not 2PI as open graphs at lambda^3: ", Length[DiagramTerms[OrderPart[lostPhb, lambda, 3]]], ")"];
(* at phibar = 0 and fixed G: second derivative = G^-1 off shell too (2PI vertex functions are skeletons) *)
check["d^2 Gamma/dphibar(a) dphibar(b) at phibar = 0 and fixed G = D0^-1 - Sigma[G] (the skeletons, Hartree included), orders 0-3",
  zeroByOrder[SetSourcesToZero[FunctionalDerivative[gammaPhi, phiMean[a], phiMean[b]]] - OperatorKernel[Power[D0, -1]][a, b] +
    SelfEnergy[phiB3, Gb, {a, b}, "Statistics" -> "Boson"], lambda, {0, 1, 2, 3}], {0, 0, 0, 0}];

(* ------------------------------------------------------------------------- *)
section["tadpoles: phi^3 and phi^4 at phibar != 0, the self-consistent representation with the one-point counterterm j"];
(* Gamma2 = 1/2 Tr(k G) - log<exp(-S_int[chi; phibar] + j chi + chi k chi/2)>_G with j = S'''[phibar] G/2 + dGamma2/dphibar and
   k = -Sigma[phibar, G]: j cancels every tadpole, k every self-energy insertion *)
Vc = c3/6 IntegralOver[phi[z]^3, {z}];
gammaC = EffectiveAction2PI[Zb, Vc, 4, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}];
gamma2C = SimplifyDiagrams[gammaC - (1/2 IntegralOver[OperatorKernel[Power[D0, -1]][x, y] phiMean[x] phiMean[y], {x, y}] + c3/6 IntegralOver[phiMean[z]^3, {z}] +
      1/2 TraceLog[Power[Gb, -1]] + 1/2 OperatorTrace[Power[D0, -1] . Gb - 1] + c3/2 IntegralOver[phiMean[z] Gb[z, z], {z}])];
check["phi^3: D^-1[phibar] = D0^-1 + c3 phibar, and Gamma2 does not depend on phibar", FreeQ[gamma2C, phiMean | D0 | TraceLog | OperatorTrace], True];
sigmaC = SelfEnergy[gamma2C, Gb, {a, b}, "Statistics" -> "Boson"];
tadpoleSides[vertices_, j_, k_, gamma2_, nv_] := Module[{lhs, trace},
   lhs = ConnectedVacuumDiagrams[ZbG, vertices - If[j === 0, 0, IntegralOver[Jc[y] phi[y], {y}]] - 1/2 IntegralOver[Kc[x, y] phi[x] phi[y], {x, y}], nv] /.
     {Jc -> LabelFunction[j, {y1}], Kc -> LabelFunction[k, {a, b}]};
   trace = 1/2 IntegralOver[(k /. {a -> u, b -> w}) Gb[w, u], {u, w}];
   SimplifyDiagrams[trace - lhs - gamma2]];
check["phi^3: with j = c3 G(y, y)/2 the representation holds at c3^2 and c3^4",
  zeroByOrder[tadpoleSides[Vc, c3/2 Gb[y1, y1], -sigmaC, gamma2C, 4], c3, {2, 4}], {0, 0}];
check["phi^3: without j the tadpoles (the dumbbell, c3^2) survive",
  SimplifyDiagrams[OrderPart[tadpoleSides[Vc, 0, -sigmaC, gamma2C, 2], c3, 2]] =!= 0, True];
jPhb = (lambda/2 phiMean[y1] Gb[y1, y1] + FunctionalDerivative[gamma2Phi, phiMean[y1]]) /. phiMean -> Phb;
check["phi^4 at phibar != 0: j = lambda phibar G(y, y)/2 + dGamma2/dphibar, orders 1-3",
  zeroByOrder[tadpoleSides[vertsPhb, jPhb, -sigmaPhi /. phiMean -> Phb, gamma2Phi /. phiMean -> Phb, 3], lambda, {1, 2, 3}], {0, 0, 0}];

(* ------------------------------------------------------------------------- *)
section["the electron gas with Grassmann-odd means Psi, Psibar"];
gammaPsi = EffectiveAction2PI[Zf, Vf, 3, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}];
phiF3 = LuttingerWardFunctional[Zf, Vf, 3, G0 -> G];
check["Psi = Psibar = 0: Gamma[0, 0, G] is Gamma[G] on the Luttinger-Ward functional", SimplifyDiagrams[SetSourcesToZero[gammaPsi] - EffectiveAction2PI[phiF3, G0 -> G]], 0];
check["classical action: -Psibar G0^-1 Psi + S_int[Psibar, Psi]",
  {SimplifyDiagrams[OrderPart[gammaPsi, V, 0] - EffectiveAction2PI[0, G0 -> G] + IntegralOver[OperatorKernel[Power[G0, -1]][x, y] Psibar[x] ** Psi[y], {x, y}]],
   SimplifyDiagrams[EffectiveAction2PI[Zf, Vf, 1, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}, "MeanFieldDegree" -> {4}] -
     (Vf /. {psi -> Psi, psibar -> Psibar})]}, {0, 0}];
check["on shell the means vanish: dGamma/dPsibar = 0 at Psi = Psibar = 0 (odd)", SetSourcesToZero[FunctionalDerivative[gammaPsi, Psibar[a]]], 0];
check["d^2 Gamma/dPsibar(a) dPsi(b) at zero means and fixed G = -G0^-1 + Sigma[G] (Hartree-Fock included), orders 0-3",
  zeroByOrder[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[gammaPsi, Psibar[a]], Psi[b]]] + OperatorKernel[Power[G0, -1]][a, b] -
    SelfEnergy[phiF3, G, {a, b}], V, {0, 1, 2, 3}], {0, 0, 0, 0}];
(* the forms written in the usage messages and the manual, with the order of FunctionalDerivative (the rightmost first) *)
check["FunctionalDerivative[Gamma, Psibar[a], Psi[b]] = G0^-1 - Sigma[G], FunctionalDerivative[Gamma, G[x4, x3], Psibar[x1], Psi[x2]] = -Gamma0, to V^2",
  {SimplifyDiagrams[OrderPart[SetSourcesToZero[FunctionalDerivative[gammaPsi, Psibar[a], Psi[b]]], V, {0, 1, 2}] - OperatorKernel[Power[G0, -1]][a, b] +
     OrderPart[SelfEnergy[phiF3, G, {a, b}], V, {1, 2}]],
   SimplifyDiagrams[OrderPart[SetSourcesToZero[FunctionalDerivative[OrderPart[gammaPsi, V, {1, 2}], G[x4, x3], Psibar[x1], Psi[x2]]], V, {1, 2}] +
     OrderPart[FunctionalDerivative[phiF3, G[x4, x3], G[x2, x1]], V, {1, 2}]]}, {0, 0}];

(* ------------------------------------------------------------------------- *)
section["the fully irreducible vertex Lambda = d^4 Gamma/dphibar^4 at fixed G (the parquet equation from two Legendre transforms)"];
(* the references: the fully irreducible part of PerturbativeFullVertex by ParquetDecomposition, phi^4 to lambda^5 (20 s)
   and the electron gas to V^4 (18 s), stored in FullyIrreducibleVertices.m; computed and written again with
   RECOMPUTE_REFERENCES=1 in the environment, or when the file is missing *)
referenceFile = FileNameJoin[{testDirectory, "FullyIrreducibleVertices.m"}];
If[Environment["RECOMPUTE_REFERENCES"] === "1" || ! FileExistsQ[referenceFile],
  envelopeB = ParquetDecomposition[PerturbativeFullVertex[Zb, Vb, 5, {phi}, labels4, D0 -> Gb], labels4, {Gb}, "Statistics" -> "Boson"]["FullyIrreducible"];
  envelopeF = ParquetDecomposition[PerturbativeFullVertex[Zf, Vf, {1, 4}, {psi, psibar}, labels4, G0 -> G], labels4, {G}]["FullyIrreducible"];
  referenceStream = OpenWrite[referenceFile, PageWidth -> 120];
  WriteString[referenceStream, "(* Written by Tests/EffectiveActions.wl (RECOMPUTE_REFERENCES=1): {phi^4 to lambda^5, electron gas to V^4},\n",
    "   the \"FullyIrreducible\" part of ParquetDecomposition of PerturbativeFullVertex. *)\n"];
  Write[referenceStream, {envelopeB, envelopeF}]; Close[referenceStream];
  Print["  (references written to ", referenceFile, ")"],
  {envelopeB, envelopeF} = Get[referenceFile]];
(* the full vertex to lambda^3, for the 1PI effective action below *)
fullB3 = PerturbativeFullVertex[Zb, Vb, 3, {phi}, labels4, D0 -> Gb];
t0 = AbsoluteTime[];
gamma45 = EffectiveAction2PI[Zb, Vb, 5, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}, "MeanFieldDegree" -> {4}];
lambdaB = SetSourcesToZero[FunctionalDerivative[gamma45, phiMean[x1], phiMean[x2], phiMean[x3], phiMean[x4]]];
check["phi^4: Lambda = FullyIrreducible of ParquetDecomposition (stored), orders 1-5 (bare, 0, 0, envelope, 15 diagrams)",
  zeroByOrder[lambdaB - envelopeB, lambda, Range[5]], Table[0, {5}]];
Print["  (", Length[DiagramTerms[OrderPart[envelopeB, lambda, #]]] & /@ Range[5], " diagrams by order; ", Round[AbsoluteTime[] - t0, 0.1], " s)"];
t0 = AbsoluteTime[];
d4F[e_] := SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[e, Psi[x4]], Psibar[x3]], Psi[x2]], Psibar[x1]]];
gammaF4 = EffectiveAction2PI[Zf, Vf, 4, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}, "MeanFieldDegree" -> {4}];
lambdaF = d4F[gammaF4];
check["electron gas: d/dPsibar(x1) d/dPsi(x2) d/dPsibar(x3) d/dPsi(x4) Gamma = FullyIrreducible (stored), orders 1-4 (bare, 0, 0, envelope)",
  zeroByOrder[lambdaF - envelopeF, V, Range[4]], Table[0, {4}]];
Print["  (", Length[DiagramTerms[OrderPart[envelopeF, V, #]]] & /@ Range[4], " diagrams by order; ", Round[AbsoluteTime[] - t0, 0.1], " s)"];

(* ------------------------------------------------------------------------- *)
section["the 1PI effective action Gamma[phibar] (EffectiveAction1PI)"];
(* Gamma[phibar] = S[phibar] + 1/2 Tr ln Dm^-1 - (1PI vacuum diagrams with lines Dm = D[phibar]); dDm/dphibar declared *)
gamma1 = EffectiveAction1PI[Zb, Vb, 3, phi -> phiMean, D0 -> Dm];
vacuumB3 = ConnectedVacuumDiagrams[Zb, Vb, 3];
checkMessage[EffectiveAction1PI[Zf, Vf, 1, psi -> Psi1, G0 -> Gm1], "EffectiveAction1PI refuses a Grassmann field (message onepi, $Failed)",
  EffectiveAction1PI::onepi, $Failed];
check["at phibar = 0 (Dm = D0): -log Z = 1/2 Tr ln D0^-1 - (connected vacuum diagrams, all 1PI in phi^4)",
  SimplifyDiagrams[(SetSourcesToZero[gamma1] /. Dm -> D0) - 1/2 TraceLog[Power[D0, -1]] + vacuumB3], 0];
check["dDm(x, y)/dphibar(a) = -lambda Dm(x, a) phibar(a) Dm(a, y)", SimplifyDiagrams[FunctionalDerivative[Dm[x, y], phiMean[a]] + lambda Dm[x, a] phiMean[a] Dm[a, y]], 0];
dGamma1 = ExpandTrace[FunctionalDerivative[gamma1, phiMean[a]]];
check["the exact equation for the mean field, lambda^1: D0^-1 phibar + lambda phibar^3/6 + lambda phibar(a) Dm(a, a)/2 (+ lambda^2 ...)",
  SimplifyDiagrams[OrderPart[dGamma1, lambda, {0, 1}] - IntegralOver[OperatorKernel[Power[D0, -1]][a, y] phiMean[y], {y}] - lambda/6 phiMean[a]^3 -
    lambda/2 phiMean[a] Dm[a, a]], 0];
atZero[e_] := ExpandTrace[SetSourcesToZero[e]] /. Dm -> D0;
gamma1Two = atZero[FunctionalDerivative[gamma1, phiMean[a], phiMean[b]]];
greenB3 = PerturbativeCorrelationFunction[Zb, {phi[a], phi[b]}, Vb, 3];
sigmaB3 = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenB3, 1, {a, b}, {D0}], {a, b}, {D0}];
check["Gamma^(2) at phibar = 0 = D0^-1 - Sigma (the 1PI self-energy with lines D0), orders 0-3",
  zeroByOrder[gamma1Two - OperatorKernel[Power[D0, -1]][a, b] + sigmaB3, lambda, {0, 1, 2, 3}], {0, 0, 0, 0}];
t0 = AbsoluteTime[];
gamma1Four = atZero[FunctionalDerivative[gamma1, phiMean[x1], phiMean[x2], phiMean[x3], phiMean[x4]]];
check["Gamma^(4) at phibar = 0: its skeletons (lines D0 -> G) are PerturbativeFullVertex, orders 1-3",
  zeroByOrder[(SkeletonDiagrams[gamma1Four, labels4, {D0}] /. D0 -> Gb) - OrderPart[fullB3, lambda, {1, 2, 3}], lambda, {1, 2, 3}], {0, 0, 0}];
amputated4 = AmputateExternalLegs[ParticleIrreducibleDiagrams[ConnectedCorrelationFunction[Zb, phi /@ labels4, Vb, 3], 1, labels4, {D0}], labels4, {D0}];
check["Gamma^(4) at phibar = 0 = -(amputated 1PI connected four-point function, lines D0), orders 1-3",
  zeroByOrder[gamma1Four + amputated4, lambda, {1, 2, 3}], {0, 0, 0}];
Print["  (", Length[DiagramTerms[gamma1Four]], " diagrams in Gamma^(4) to lambda^3, ", Length[DiagramTerms[SkeletonDiagrams[gamma1Four, labels4, {D0}]]], " skeletons; ",
  Round[AbsoluteTime[] - t0, 0.1], " s)"];
(* the trees of ConnectedFunctionFromVertices with these vertex functions: the connected four-point function *)
trees = ConnectedFunctionFromVertices[labels4, "Boson"];
sigmaFromGamma = LabelFunction[SimplifyDiagrams[OperatorKernel[Power[D0, -1]][a, b] - gamma1Two], {a, b}];
treesValue = trees /. {VertexFunction[p_, q_, r_] :> 0, VertexFunction[p__] /; Length[{p}] == 4 :> (gamma1Four /. Thread[labels4 -> {p}]),
     FullPropagator[p_, q_] :> OperatorKernel[Power[Power[D0, -1] - sigmaFromGamma, -1]][p, q]};
check["ConnectedFunctionFromVertices with VertexFunction = derivatives of Gamma[phibar] and FullPropagator = (Gamma^(2))^-1: the connected four-point function, orders 1-2",
  zeroByOrder[ExpandTrace[treesValue, D0, 2, "MaximumOrder" -> {lambda, 2}] - ConnectedCorrelationFunction[Zb, phi /@ labels4, Vb, 2], lambda, {1, 2}], {0, 0}];

(* ------------------------------------------------------------------------- *)
section["the 1PI effective action with bare lines: phi^4 and Gamma[Psibar, Psi] of fermions"];
(* bare lines (D0 -> D0, G0 -> G0): the vertices of degree two stay vertices; for phi^4 the same series as the lines D[phibar]
   expanded in D0; for fermions the only form *)
gamma1Bare = EffectiveAction1PI[Zb, Vb, 3, phi -> phiMean, D0 -> D0];
check["phi^4 with bare lines = the lines D[phibar] = (D0^-1 + lambda phibar^2/2)^-1 expanded in D0, orders 0-3",
  zeroByOrder[gamma1Bare - ExpandTrace[gamma1 /. Dm -> Power[Power[D0, -1] - LabelFunction[-lambda/2 phiMean[a]^2 DeltaFunction[a, b], {a, b}], -1],
     D0, 3, "MaximumOrder" -> {lambda, 3}], lambda, {0, 1, 2, 3}], {0, 0, 0, 0}];
checkMessage[EffectiveAction1PI[Zf, Vf, 1, {psi -> Psi, psibar -> Psibar}, G0 -> G], "fermions with lines other than G0 are refused (message onepi, $Failed)",
  EffectiveAction1PI::onepi, $Failed];
t0 = AbsoluteTime[];
gammaPsi1PI = EffectiveAction1PI[Zf, Vf, 3, {psi -> Psi, psibar -> Psibar}, G0 -> G0];
check["Gamma[Psibar, Psi] at zero means: -Tr ln(-G0^-1) - (connected vacuum diagrams, all 1PI for fermions)",
  SimplifyDiagrams[SetSourcesToZero[gammaPsi1PI] + TraceLog[-Power[G0, -1]] + ConnectedVacuumDiagrams[Zf, Vf, 3]], 0];
check["Gamma[Psibar, Psi]: tree -Psibar G0^-1 Psi + S_int[Psibar, Psi]",
  SimplifyDiagrams[Total[Select[DiagramTerms[gammaPsi1PI], FreeQ[#, G0[_, _]] && FreeQ[#, TraceLog] &]] +
    IntegralOver[OperatorKernel[Power[G0, -1]][x, y] Psibar[x] ** Psi[y], {x, y}] - (Vf /. {psi -> Psi, psibar -> Psibar})], 0];
check["Gamma[Psibar, Psi]: d^2/dPsibar(a) dPsi(b) at zero means = -G0^-1 + Sigma (perturbative, lines G0), orders 0-3",
  zeroByOrder[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[gammaPsi1PI, Psibar[a]], Psi[b]]] +
    OperatorKernel[Power[G0, -1]][a, b] - sigmaF, V, {0, 1, 2, 3}], {0, 0, 0, 0}];
gammaPsi1PIFour = SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[
       OrderPart[gammaPsi1PI, V, {1, 2}], Psi[x4]], Psibar[x3]], Psi[x2]], Psibar[x1]]];
check["Gamma[Psibar, Psi]: FunctionalDerivative[Gamma, Psibar[a], Psi[b]] at zero means = G0^-1 - Sigma (the order of the usage message), orders 0-3",
  zeroByOrder[SetSourcesToZero[FunctionalDerivative[gammaPsi1PI, Psibar[a], Psi[b]]] - OperatorKernel[Power[G0, -1]][a, b] + sigmaF, V, {0, 1, 2, 3}],
  {0, 0, 0, 0}];
check["Gamma[Psibar, Psi]: the fourth derivative at zero means = F(x1, x2; x3, x4), the amputated 1PI connected four-point function, orders 1-2",
  zeroByOrder[gammaPsi1PIFour - AmputateExternalLegs[ParticleIrreducibleDiagrams[ConnectedCorrelationFunction[Zf, {psi[x1], psibar[x2], psibar[x4], psi[x3]}, Vf, 2],
       1, labels4, {G0}], labels4, {G0}], V, {1, 2}], {0, 0}];
check["Gamma[Psibar, Psi]: the skeletons of its fourth derivative (lines G0 -> G) are PerturbativeFullVertex, orders 1-2",
  zeroByOrder[(SkeletonDiagrams[gammaPsi1PIFour, labels4, {G0}] /. G0 -> G) - PerturbativeFullVertex[Zf, Vf, 2, {psi, psibar}, labels4, G0 -> G], V, {1, 2}], {0, 0}];
(* Gamma[Psibar, Psi, G] at the stationary G: the Dyson equation solved to first order is enough to third order (the error
   in G is O(V^2), that of Gamma O(V^4)); what is missing are the diagrams with an anomalous vertex, V Psi Psi or V Psibar Psibar *)
xPsi = SimplifyDiagrams[gammaPsi - (Vf /. {psi -> Psi, psibar -> Psibar}) + IntegralOver[OperatorKernel[Power[G0, -1]][x, y] Psibar[x] ** Psi[y], {x, y}] -
    EffectiveAction2PI[0, G0 -> G]];
sigmaPsi1 = SimplifyDiagrams[OrderPart[SelfEnergy[xPsi, G, {a, b}], V, 1] /. G -> G0];
gStar = Power[Power[G0, -1] - LabelFunction[sigmaPsi1, {a, b}], -1];
backPsi = Sum[ExpandTrace[OrderPart[xPsi, V, m] /. G -> gStar, G0, 3 - m, "MaximumOrder" -> {V, 3}], {m, 1, 3}] +
   ExpandTrace[SimplifyDiagrams[gammaPsi - xPsi] /. G -> gStar, G0, 3, "MaximumOrder" -> {V, 3}];
anomalousVertexQ[t_] := Module[{ps = Cases[t, Psi[u_] :> u, Infinity], pbs = Cases[t, Psibar[u_] :> u, Infinity]},
   AnyTrue[Cases[t, V[u_, w_] :> {u, w}, Infinity], {Count[ps, Alternatives @@ #], Count[pbs, Alternatives @@ #]} === {2, 0} ||
      {Count[ps, Alternatives @@ #], Count[pbs, Alternatives @@ #]} === {0, 2} &]];
anomalousPsi = Total[Select[DiagramTerms[gammaPsi1PI], anomalousVertexQ]];
check["Gamma[Psibar, Psi, G] at the stationary G = Gamma[Psibar, Psi] minus its diagrams with an anomalous vertex, orders 0-3",
  zeroByOrder[backPsi - gammaPsi1PI + anomalousPsi, V, {0, 1, 2, 3}], {0, 0, 0, 0}];
check["the diagrams with an anomalous vertex by order (V, V^2, V^3)", Table[Length[DiagramTerms[OrderPart[anomalousPsi, V, m]]], {m, 1, 3}], {0, 1, 11}];
Print["  (Gamma[Psibar, Psi]: ", Table[Length[DiagramTerms[OrderPart[gammaPsi1PI, V, m]]], {m, 1, 3}], " diagrams at V, V^2, V^3; ", Round[AbsoluteTime[] - t0, 0.1], " s)"];

(* ------------------------------------------------------------------------- *)
section["zero dimensions: the exact effective actions of phi^3 + phi^4"];
(* Z(J, K) = Integral dphi/Sqrt[2 Pi] exp(-phi^2/(2 d0) - V(phi) + J phi + K phi^2/2), V = eps (c3 phi^3/6 + lambda phi^4/24).
   With phi = ph + chi, chi Gaussian of mean dm and variance Gt: Gamma(ph, g) = -log Z + J ph + K (ph^2 + g)/2 at <chi> = 0,
   <chi^2> = g, = -1/2 log Gt - dm^2/(2 Gt) - log A0 + (ph^2 + g)/(2 d0) - g/(2 Gt), A0 = <exp(-V(ph + chi))>; the 1PI
   Gamma(ph) at K = 0 (Gt = d0, only <chi> = 0) = -1/2 log d0 + ph^2/(2 d0) - dm^2/(2 d0) - log A0. Polynomials truncated at
   eps^nEps (no Series); the logarithms of g and d0 are left out (added back below). *)
exactZeroD[nEps_Integer, onePI_] := Module[{eps, ch, trunc, dmKnown = 0, tKnown = 0, var, averages, s, mm, tt, a0, logSeries, geometric, weights},
   trunc[e_, k_] := Sum[Coefficient[e, eps, i] eps^i, {i, 0, k}];
   weights = trunc[Expand[Sum[(-eps (c3 (ph + ch)^3/6 + lambda (ph + ch)^4/24))^k/k!, {k, 0, nEps}]], nEps];
   var = If[onePI, d0, g];
   averages[m_, t_, k_] := Module[{mpow, tpow, moment},
     mpow = NestList[trunc[Expand[# m], k] &, 1, k];
     tpow = NestList[trunc[Expand[# (1 + t)], k] &, 1, 2 k + 2];
     moment[p_] := Sum[If[p - j > k, 0, Binomial[p, j] mpow[[p - j + 1]] var^(j/2) tpow[[j/2 + 1]] (j - 1)!!], {j, 0, p, 2}];
     Table[With[{cl = CoefficientList[trunc[Expand[ch^q weights], k], ch]},
       trunc[Expand[Sum[If[cl[[p + 1]] === 0, 0, cl[[p + 1]] moment[p]], {p, 0, Length[cl] - 1}]], k]], {q, 0, 2}]];
   Do[If[onePI,
     With[{av = averages[dmKnown + mm eps^k, 0, k]}, s = First[Solve[Coefficient[av[[2]], eps, k] == 0, mm]]; dmKnown = dmKnown + (mm /. s) eps^k],
     With[{av = averages[dmKnown + mm eps^k, tKnown + tt eps^k, k]},
      s = First[Solve[{Coefficient[av[[2]], eps, k] == 0, Coefficient[av[[3]] - g av[[1]], eps, k] == 0}, {mm, tt}]];
      dmKnown = dmKnown + (mm /. s) eps^k; tKnown = tKnown + (tt /. s) eps^k]], {k, 1, nEps}];
   a0 = averages[dmKnown, tKnown, nEps][[1]];
   logSeries[x_] := trunc[Expand[Sum[(-1)^(j + 1) x^j/j, {j, 1, nEps}]], nEps];
   geometric = If[onePI, 1, trunc[Expand[1 + Sum[(-tKnown)^j, {j, 1, nEps}]], nEps]];
   Expand[If[onePI, ph^2/(2 d0) - trunc[Expand[dmKnown^2], nEps]/(2 d0) - logSeries[a0 - 1],
      -1/2 logSeries[tKnown] - trunc[Expand[dmKnown^2 geometric], nEps]/(2 g) - logSeries[a0 - 1] + (ph^2 + g)/(2 d0) - 1/2 geometric] /. eps -> 1]];
upToDegree[e_, k_] := With[{s = Expand[e /. {c3 -> sc c3, lambda -> sc lambda}]}, Sum[Coefficient[s, sc, i], {i, 0, k}] /. sc -> 1];
t0 = AbsoluteTime[];
V34 = c3/6 IntegralOver[phi[z]^3, {z}] + Vb;
gamma2PI0d = ZeroDimensionalValue[EffectiveAction2PI[Zb, V34, 4, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}],
     {D0 -> d0, Gb -> g, phiMean -> ph}] /. Log[Power[g, -1]] -> -Log[g];
exact2PI0d = exactZeroD[4, False];
check["Gamma(ph, g) = exact double Legendre transform: all terms with up to four vertices (c3, lambda)",
  Expand[upToDegree[gamma2PI0d + Log[g]/2 - exact2PI0d, 4]], 0];
gamma1PI0d = ZeroDimensionalValue[EffectiveAction1PI[Zb, V34, 4, phi -> phiMean, D0 -> Dm],
   {D0 -> d0, Dm -> dm, phiMean -> ph}];
(* the lines D(ph) = (1/d0 + c3 ph + lambda ph^2/2)^-1 and the trace-log expanded in the couplings *)
gamma1PI0dSeries = PowerExpand[Normal[Series[(gamma1PI0d /. {Log[Power[dm, -1]] -> Log[1/d0 + c3 ph + lambda ph^2/2], dm -> 1/(1/d0 + c3 ph + lambda ph^2/2)}) /.
       {c3 -> sc c3, lambda -> sc lambda}, {sc, 0, 4}]] /. sc -> 1];
check["Gamma(ph) (1PI) = exact Legendre transform, to fourth order in the couplings",
  Expand[upToDegree[gamma1PI0dSeries + Log[d0]/2 - exactZeroD[4, True], 4]], 0];
(* Gamma(ph) = ext_g Gamma(ph, g): the stationary g(ph) order by order (sc counts the couplings), and the Schur complement *)
scaled[e_] := e /. {c3 -> sc c3, lambda -> sc lambda};
series4[e_] := Normal[Series[e, {sc, 0, 4}]];
g2s = scaled[gamma2PI0d];
gSolution = d0; Do[With[{trial = gSolution + gk sc^k},
   gSolution = gSolution + (gk /. First[Solve[Coefficient[series4[D[g2s, g] /. g -> trial], sc, k] == 0, gk]]) sc^k], {k, 1, 4}];
g1s = scaled[gamma1PI0dSeries];
check["Gamma(ph) = Gamma(ph, g(ph)) with dGamma/dg = 0 (the 1PI action from the 2PI one), to fourth order",
  Expand[PowerExpand[series4[g2s /. g -> gSolution] - series4[g1s]]], 0];
check["d^2 Gamma(ph)/dph^2 = Gamma_phph - Gamma_phg Gamma_gg^-1 Gamma_gph at g(ph) (Schur complement), to fourth order",
  Expand[series4[(D[g2s, ph, ph] - D[g2s, ph, g]^2/D[g2s, g, g]) /. g -> gSolution] - series4[D[g1s, ph, ph]]], 0];
(* the stationary point of Gamma(ph): the mean field <phi> (the tadpoles of phi^3) and -log Z *)
weights0d = Expand[Sum[(-sc (c3 phx^3/6 + lambda phx^4/24))^k/k!, {k, 0, 4}]];
gaussian0d[e_] := Expand[e] /. phx^p_. :> If[EvenQ[p], d0^(p/2) (p - 1)!!, 0] /. phx -> 0;
meanExact = series4[gaussian0d[phx weights0d]/gaussian0d[weights0d]];
phSolution = 0; Do[With[{trial = phSolution + pk sc^k},
   phSolution = phSolution + (pk /. First[Solve[Coefficient[series4[D[g1s, ph] /. ph -> trial], sc, k] == 0, pk]]) sc^k], {k, 1, 4}];
check["dGamma/dph = 0 at ph = <phi> = -c3 d0^2/2 + ... (the tadpoles of phi^3 come from the field equation), to fourth order",
  Expand[phSolution - meanExact], 0];
check["Gamma at the stationary point = -log Z = -1/2 log d0 - log<exp(-V)>, to fourth order",
  Expand[PowerExpand[series4[g1s /. ph -> phSolution] + Log[d0]/2 + series4[Log[gaussian0d[weights0d]]]]], 0];
Print["  (", Round[AbsoluteTime[] - t0, 0.1], " s)"];

(* ------------------------------------------------------------------------- *)
section["vertex divergences and the branches of Sigma[G]: one site, spin 1/2, at a = i nu"];
(* Z = a_up a_dn - u, g_s = d log Z/d a_s, Sigma_s = a_s - 1/g_s = u/a_(-s). The irreducible vertex Gamma0 = dSigma/dg (a
   2 x 2 matrix in spin) is the Bethe-Salpeter kernel: L^-1 = L0^-1 - Gamma0 with L = -dg/da (the response to a one-body
   source, a -> a - U) and L0 = g^2, so Gamma0 diverges where L has a zero eigenvalue: there a -> g is not locally
   invertible and two branches of Sigma[g] meet. With a = i nu (a fermionic Matsubara frequency at half filling;
   g = 1/(i nu - u/(i nu)) is the propagator of the Hubbard atom at half filling for u = U^2/4) this happens for a
   repulsive u, in the charge channel, at u = nu^2: the mechanism of the divergences of the irreducible vertex found in
   DMFT (Schaefer et al. 2013) and of the multivaluedness of the Luttinger-Ward functional (Kozik, Ferrero and Georges
   2015; Gunnarsson et al. 2017). *)
t0 = AbsoluteTime[];
zSite = a1 a2 - u;
gSite = {D[Log[zSite], a1], D[Log[zSite], a2]};
sigmaSite = Simplify[{a1, a2} - 1/gSite];
jacSite = D[gSite, {{a1, a2}}];
gamma0Site = Simplify[D[sigmaSite, {{a1, a2}}] . Inverse[jacSite]];
check["Sigma_s = u/a_(-s), and Gamma0 = dSigma/dg = L0^-1 - L^-1 (the Bethe-Salpeter equation inverted, L = -dg/da, L0 = g^2)",
  {sigmaSite, Simplify[gamma0Site - (DiagonalMatrix[1/gSite^2] + Inverse[jacSite])]}, {{u/a2, u/a1}, {{0, 0}, {0, 0}}}];
symSite = {a1 -> a, a2 -> a};
channels[m_] := Simplify[{m[[1, 1]] + m[[1, 2]], m[[1, 1]] - m[[1, 2]]} /. symSite];
check["charge and spin channels: L_c = (a^2 + u)/(a^2 - u)^2, L_s = 1/(a^2 - u); Gamma0_c = u (a^2 - u)^2/(a^2 (a^2 + u)), Gamma0_s = -u (a^2 - u)/a^2",
  Simplify[{-channels[jacSite] - {(a^2 + u)/(a^2 - u)^2, 1/(a^2 - u)}, channels[gamma0Site] - {u (a^2 - u)^2/(a^2 (a^2 + u)), -u (a^2 - u)/a^2}}], {{0, 0}, {0, 0}}];
(* the skeleton Sigma[g] of the package (Luttinger-Ward functional to u^4) *)
If[! ValueQ[sigma4], sigma4 = ZeroDimensionalValue[SelfEnergy[LuttingerWardFunctional[Zf, Vf, 4, G0 -> G], G, {x, y}], {G0 -> 1/a, V -> u, G -> g}, "LoopFactor" -> 2]];
gSite0[aa_] := aa/(aa^2 - u);
check["Gamma0_c = d Sigma_skel/dg of the package's skeleton series at the exact g, to u^4",
  Normal[Series[(D[sigma4, g] /. g -> gSite0[a]) - u (a^2 - u)^2/(a^2 (a^2 + u)), {u, 0, 4}]], 0];
gammaChargeSite[nu_] := Simplify[channels[gamma0Site][[1]] /. a -> I nu];
gammaSpinSite[nu_] := Simplify[channels[gamma0Site][[2]] /. a -> I nu];
check["a = i nu: Gamma0_c = u (nu^2 + u)^2/(nu^2 (nu^2 - u)) diverges at u = nu^2 (repulsive), where L_c = 0; Gamma0_s = -u (nu^2 + u)/nu^2 stays finite",
  {Simplify[gammaChargeSite[nu] - u (nu^2 + u)^2/(nu^2 (nu^2 - u))], Simplify[gammaSpinSite[nu] + u (nu^2 + u)/nu^2], Simplify[channels[jacSite][[1]] /. a -> I nu /. u -> nu^2]}, {0, 0, 0}];
check["there the two branches Sigma_+-(g) = (-1 +- Sqrt[1 + 4 u g^2])/(2 g) meet: 1 + 4 u g^2 = ((nu^2 - u)/(nu^2 + u))^2",
  Simplify[1 + 4 u gSite0[I nu]^2 - ((nu^2 - u)/(nu^2 + u))^2], 0];
sigmaBranch[s_, gg_] := (-1 + s Sqrt[1 + 4 u gg^2])/(2 gg);
check["nu = 1: the physical Sigma = u/(i nu) is on the + branch (connected to u = 0) for u = 1/2, on the - branch for u = 2",
  Table[Simplify[{sigmaBranch[1, gSite0[I]], sigmaBranch[-1, gSite0[I]]} - u/I] /. u -> uu, {uu, {1/2, 2}}] // Simplify,
  {{0, -I/2}, {I, 0}}];
catalanPartial[gg_, uu_, k_] := Sum[(-1)^j Binomial[2 j, j]/(j + 1) uu^(j + 1) gg^(2 j + 1), {j, 0, k}];
check["the skeleton series at the exact g converges (|4 u g^2| <= 1) to the + branch: u = 2 > nu^2 = 1, partial sum to u^201 = -i (+ branch), physical -2 i",
  With[{p = N[catalanPartial[gSite0[I] /. u -> 2, 2, 200], 20]}, {Abs[p - N[sigmaBranch[1, gSite0[I]] /. u -> 2, 20]] < 10^-10, Abs[p - (-2 I)] > 9/10}], {True, True}];
check["the self-consistent skeleton equation g = 1/(i nu - Sigma_+[g]) (nu = 1) has the exact g as solution for u = 1/2 and no solution for u = 2",
  Table[Solve[(g (I - sigmaBranch[1, g]) == 1) /. u -> uu, g], {uu, {1/2, 2}}], {{{g -> -2 I/3}}, {}}];
Print["  (", Round[AbsoluteTime[] - t0, 0.1], " s)"];

(* ------------------------------------------------------------------------- *)
(* edge cases: odd interactions, powers of operators, loops through kernels of operators, coefficients, complex bosons,
 several means, G[S] *)

(* ------------------------------------------------------------------------- *)
section["odd interactions at zero mean field (phi^3)"];
(* without the Z2 symmetry d^2Gamma/dphibar dG does not vanish at phibar = 0: d^2Gamma/dphibar^2 at fixed G is D0^-1, the bubble of
   Sigma[G] coming through the mixed derivative (the 1PI Gamma^(2) is the Schur complement, checked in zero dimensions) *)
V3fix = c3/6 IntegralOver[phi[z]^3, {z}];
gamma3fix = EffectiveAction2PI[Zb, V3fix, 2, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}];
check["phi^3: d2Gamma/dphibar(a) dphibar(b) at phibar = 0, fixed G = D0^-1 (not D0^-1 - Sigma[G])",
  SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gamma3fix, phiMean[a], phiMean[b]]]], OperatorKernel[Power[D0, -1]][a, b]];
check["phi^3: Sigma[G] = c3^2 G^2/2 is not zero (the bubble)",
  SelfEnergy[LuttingerWardFunctional[Zb, V3fix, 2, D0 -> Gb], Gb, {a, b}, "Statistics" -> "Boson"], c3^2 Gb[a, b]^2/2];
check["phi^3: d2Gamma/dphibar(a) dG(c, d) at phibar = 0 is not zero",
  SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[gamma3fix, phiMean[a]], Gb[c, d]]]] =!= 0, True];
check["phi^4: d2Gamma/dphibar(a) dG(c, d) at phibar = 0 vanishes",
  SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[
     EffectiveAction2PI[Zb, Vb, 2, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}], phiMean[a]], Gb[c, d]]]], 0];

(* ------------------------------------------------------------------------- *)
section["powers of operators, unknown variations"];
check["d Tr(B^-2)/dB(a, b) = d Tr(B^-1 . B^-1)/dB(a, b)",
  SimplifyDiagrams[FunctionalDerivative[OperatorTrace[Power[B, -2]], B[a, b]] - FunctionalDerivative[OperatorTrace[Power[B, -1] . Power[B, -1]], B[a, b]]], 0];
check["d Tr(B^-2)/dB(a, b) = -2 (B^-3)(b, a)", SimplifyDiagrams[FunctionalDerivative[OperatorTrace[Power[B, -2]], B[a, b]] -
    (-2 ExpandTrace[OperatorKernel[Power[B, -1] . Power[B, -1] . Power[B, -1]][b, a]])], 0];
check["d Tr ln(G^-2)/dG(a, b) = -2 G^-1(b, a)", FunctionalDerivative[TraceLog[Power[G, -2]], G[a, b]], -2 OperatorKernel[Power[G, -1]][b, a]];
check["d (G^-2)(x, y)/dG(a, b) is not zero", FunctionalDerivative[OperatorKernel[Power[G, -2]][x, y], G[a, b]] =!= 0, True];
check["d Tr(B^(1/2))/dB: message opvariation", messageGivenQ[FunctionalDerivative[OperatorTrace[Power[B, 1/2]], B[a, b]], FunctionalDiagrams::opvariation], True];
(* the input of operators: G0^-1 . G, which Mathematica reads as G0^(-(1 . G)), and Inverse[G] *)
check["G0^-1 . G as typed: message opdotpower, read as G0^(-1) . G",
  {messageGivenQ[OperatorTrace[G0^-1 . G - 1], FunctionalDiagrams::opdotpower], Quiet[OperatorTrace[G0^-1 . G - 1]] === OperatorTrace[Power[G0, -1] . G - 1]},
  {True, True}];
check["d Tr(G0^-1 . G - 1)/dG(a, b) as typed = G0^-1(b, a)", Quiet[FunctionalDerivative[OperatorTrace[G0^-1 . G - 1], G[a, b]]], OperatorKernel[Power[G0, -1]][b, a]];
check["Inverse[G] is G^-1", {TraceLog[-Inverse[G]], FunctionalDerivative[TraceLog[-Inverse[G]], G[a, b]]}, {TraceLog[-Power[G, -1]], -OperatorKernel[Power[G, -1]][b, a]}];
(* a source inside a LabelFunction: the one-loop action 1/2 Tr ln(D0^-1 + lambda/2 pb^2) of phi^4, differentiated before
   and after ExpandTrace *)
DeclareSource[pb];
hessianPb = Power[D0, -1] + LabelFunction[lambda/2 pb[u]^2 DeltaFunction[u, v], {u, v}];
check["d^2/dpb^2 1/2 Tr ln(D0^-1 + lambda/2 pb^2) at pb = 0: lambda/2 D0(a, a) delta(a, b)",
  ExpandTrace[SetSourcesToZero[FunctionalDerivative[1/2 TraceLog[hessianPb], pb[a], pb[b]]], D0, 1], lambda/2 D0[a, a] DeltaFunction[a, b]];
check["d^4/dpb^4 of 1/2 Tr ln(D0^-1 + lambda/2 pb^2) at pb = 0: the three bubbles, as after ExpandTrace",
  SimplifyDiagrams[ExpandTrace[SetSourcesToZero[FunctionalDerivative[1/2 TraceLog[hessianPb], pb[a], pb[b], pb[c], pb[d]]], D0, 2] -
    OrderPart[SetSourcesToZero[FunctionalDerivative[ExpandTrace[1/2 TraceLog[hessianPb], D0, 2], pb[a], pb[b], pb[c], pb[d]]], lambda, 2]], 0];
check["a source in an operator outside a kernel (pb without labels): message opsource",
  messageGivenQ[FunctionalDerivative[TraceLog[Power[D0, -1] + lambda/2 pb^2], pb[a]], FunctionalDiagrams::opsource], True];

(* ------------------------------------------------------------------------- *)
section["zero dimensions, loops through kernels of operators"];
zeroFix[e_] := ZeroDimensionalValue[e, {G0 -> 1/a, G -> g, S -> s}, "LoopFactor" -> 2];
check["Integral G0^-1(x, y) G(y, x) = Tr(G0^-1 . G) = 2 a g (spin 1/2)",
  {zeroFix[IntegralOver[OperatorKernel[Power[G0, -1]][x, y] G[y, x], {x, y}]], zeroFix[OperatorTrace[Power[G0, -1] . G]]}, {2 a g, 2 a g}];
check["Integral (G0^-1 - S)^-1(x, y) S(y, x) = 2 s/(a - s) (S undeclared)",
  Simplify[zeroFix[IntegralOver[OperatorKernel[Power[Power[G0, -1] - S, -1]][x, y] S[y, x], {x, y}]] - 2 s/(a - s)], 0];
check["ZeroDimensionalValue of Gamma[G] before and after ExpandTrace",
  Expand[zeroFix[ExpandTrace[EffectiveAction2PI[0, G0 -> G]]] - zeroFix[EffectiveAction2PI[0, G0 -> G]]], 0];
check["\"Loops\" -> {G0, G} as Automatic, {G} misses the loop through the kernel of G0^-1",
  Table[ZeroDimensionalValue[IntegralOver[OperatorKernel[Power[G0, -1]][x, y] G[y, x], {x, y}], {G0 -> 1/a, G -> g}, "LoopFactor" -> 2, "Loops" -> l], {l, {{G0, G}, {G}}}],
  {2 a g, a g}];

(* ------------------------------------------------------------------------- *)
section["derivatives with respect to kernels absent from the expression"];
check["d/dG(a, b) of an expression without G: 0 and no message", Check[FunctionalDerivative[IntegralOver[V[x, y], {x, y}], G[a, b]], "message"], 0];
check["d/dG(a, b) of a number: 0 and no message", Check[FunctionalDerivative[7, G[a, b]], "message"], 0];
check["d/dV(a, b) of Tr ln(-G^-1): 0 and no message", Check[FunctionalDerivative[TraceLog[-Power[G, -1]], V[a, b]], "message"], 0];

(* ------------------------------------------------------------------------- *)
section["symbolic coefficients in operators"];
check["d Tr ln(G0^-1 - lam S)/dS(a, b) = -lam (G0^-1 - lam S)^-1(b, a) (S the variable)",
  FunctionalDerivative[TraceLog[Power[G0, -1] - lam S], S[a, b]], -lam OperatorKernel[Power[Power[G0, -1] - lam S, -1]][b, a]];
check["ExpandTrace of Tr ln(G0^-1 - lam Sg), Sg declared: lam^k on the k-th ring",
  SimplifyDiagrams[ExpandTrace[TraceLog[Power[G0, -1] - lam Sg], G0, 2] - (TraceLog[Power[G0, -1]] -
     ExpandTrace[lam OperatorTrace[G0 . Sg] + lam^2/2 OperatorTrace[G0 . Sg . G0 . Sg]])], 0];
check["lam S with neither declared: message opscalar and $Failed, no abort",
  {messageGivenQ[ExpandTrace[TraceLog[Power[G0, -1] - lam S], G0, 2], FunctionalDiagrams::opscalar], Quiet[ExpandTrace[TraceLog[Power[G0, -1] - lam S], G0, 2]]},
  {True, $Failed}];
(* a chemical potential as a kernel: U = -mu delta, N = -dOmega/dmu = Tr G (2 g for one site with spin 1/2) *)
gammaMuFix = ZeroDimensionalValue[ExpandTrace[EffectiveAction2PI[0, G0 -> G, "Source" -> LabelFunction[-mu DeltaFunction[p, q], {p, q}]]],
   {G0 -> 1/a, G -> g}, "LoopFactor" -> 2];
check["\"Source\" -> LabelFunction[-mu delta]: -dGamma/dmu = Tr G = 2 g", -D[gammaMuFix, mu], 2 g];

(* ------------------------------------------------------------------------- *)
section["complex bosons, the source and the barred mean"];
DeclareSource[Jcf, Jcfbar]; DeclareField[phicf, Jcfbar]; DeclareField[phicfbar, Jcf];
Zcf = Exp[IntegralOver[Jcfbar[x] Dcf[x, y] Jcf[y], {x, y}]];
Vcf = 1/2 IntegralOver[V[x, y] phicfbar[x] phicfbar[y] phicf[y] phicf[x], {x, y}];
check["complex bosons: dGamma/dU(a, b) = G(b, a) = <phibar(a) phi(b)> (G0^-1 + U)",
  FunctionalDerivative[EffectiveAction2PI[0, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "Source" -> U], U[a, b]], Gcf[b, a]];
check["fermions: dGamma/dU(a, b) = G(b, a) = <psibar(a) psi(b)> (G0^-1 - U)",
  FunctionalDerivative[EffectiveAction2PI[0, G0 -> G, "Source" -> U], U[a, b]], G[b, a]];
freeCf1 = EffectiveAction2PI[Zcf, 0, 0, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "MeanFields" -> {phicf -> Pc, phicfbar -> Pcbar}];
freeCf2 = EffectiveAction2PI[Zcf, 0, 0, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "MeanFields" -> {phicfbar -> Pcbar, phicf -> Pc}];
check["complex bosons: the classical action Pcbar G0^-1 Pc whatever the order of the means", {freeCf1 === freeCf2,
   SimplifyDiagrams[freeCf1 - IntegralOver[OperatorKernel[Power[Dcf, -1]][x, y] Pcbar[x] Pc[y], {x, y}]]},
  {True, OperatorTrace[Power[Dcf, -1] . Gcf - 1] + TraceLog[Power[Gcf, -1]]}];
check["complex bosons without Z0: message barred and $Failed",
  {messageGivenQ[EffectiveAction2PI[0, 0, 0, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "MeanFields" -> {phicf -> Pc, phicfbar -> Pcbar}], EffectiveAction2PI::barred],
   Quiet[EffectiveAction2PI[0, 0, 0, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "MeanFields" -> {phicf -> Pc, phicfbar -> Pcbar}]]}, {True, $Failed}];
gammaCf = EffectiveAction2PI[Zcf, Vcf, 2, Dcf -> Gcf, "Statistics" -> "ComplexBoson", "MeanFields" -> {phicfbar -> Pcbar, phicf -> Pc}];
sigmaCf = SelfEnergy[LuttingerWardFunctional[Zcf, Vcf, 2, Dcf -> Gcf], Gcf, {a, b}, "Statistics" -> "ComplexBoson"];
check["complex bosons, barred mean listed first: d2Gamma/dPcbar(a) dPc(b) at zero means = G0^-1 - Sigma[G], to V^2",
  SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[gammaCf, Pcbar[a]], Pc[b]]] - OperatorKernel[Power[Dcf, -1]][a, b] + sigmaCf], 0];
check["fermions without Z0, psibar listed first: -Psibar G0^-1 Psi", EffectiveAction2PI[0, 0, 0, G0 -> G, "MeanFields" -> {psibar -> Psibar, psi -> Psi}] ===
  EffectiveAction2PI[Zf, 0, 0, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}], True];

(* ------------------------------------------------------------------------- *)
section["several real means refused"];
DeclareSource[J1f, J2f]; DeclareField[phi1f, J1f]; DeclareField[phi2f, J2f];
check["two real-boson means: message meanrule and $Failed",
  {messageGivenQ[EffectiveAction2PI[0, 0, 0, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi1f -> P1f, phi2f -> P2f}], EffectiveAction2PI::meanrule],
   Quiet[EffectiveAction2PI[0, 0, 0, D0 -> Gb, "Statistics" -> "Boson", "MeanFields" -> {phi1f -> P1f, phi2f -> P2f}]]}, {True, $Failed}];

(* ------------------------------------------------------------------------- *)
section["G[S] of Potthoff's F[S] (LegendrePropagator)"];
phi1Fix = LuttingerWardFunctional[Zf, Vf, 1, G0 -> G];
fFix = LegendreTransform[phi1Fix, G -> Sg];
check["dF/dS(a, b) = -G[S](b, a), G[S] depending on S and V", FunctionalDerivative[fFix, Sg[a, b]], -LegendrePropagator[G -> Sg, {V}][b, a]];
check["dF/dG = 0 (F depends on S)", FunctionalDerivative[fFix, G[a, b]], 0];
check["dF/dV(a, b) = dphibar/dV(a, b) at G = G[S] (stationarity in G)", FunctionalDerivative[fFix, V[a, b]],
  FunctionalDerivative[phi1Fix, V[a, b]] /. G[x_, y_] :> LegendrePropagator[G -> Sg, {V}][x, y]];
check["d2F/dS dS: message derivative, a $Failed term", {messageGivenQ[FunctionalDerivative[fFix, Sg[a, b], Sg[c, d]], LegendrePropagator::derivative],
   ! FreeQ[Quiet[FunctionalDerivative[fFix, Sg[a, b], Sg[c, d]]], $Failed]}, {True, True}];
check["d2F/dS dV: message derivative", messageGivenQ[FunctionalDerivative[fFix, Sg[a, b], V[c, d]], LegendrePropagator::derivative], True];
check["d/dG0 of dOmega/dS: G[S] does not depend on G0 (no message, only the trace-log term)",
  Check[FunctionalDerivative[FunctionalDerivative[SelfEnergyFunctional[phi1Fix, G0 -> Sg, G], Sg[a, b]], G0[c, d]] ===
    FunctionalDerivative[FunctionalDerivative[-TraceLog[-(Power[G0, -1] - Sg)], Sg[a, b]], G0[c, d]], "message"], True];
check["ExpandTrace writes G[S] as G", ExpandTrace[FunctionalDerivative[fFix, Sg[a, b]]], -G[b, a]];
(* zero dimensions: F(s) = log(1 - s^2/u) (spin 1/2), dF/du at fixed s = s^2/(u (u - s^2)) = sig^2/(1 - u sig^2) for s = u sig *)
If[! ValueQ[phi4], phi4 = LuttingerWardFunctional[Zf, Vf, 4, G0 -> G]];
dfdvFix = ZeroDimensionalValue[FunctionalDerivative[LegendreTransform[phi4, G -> Sg], V[a, b]], {G -> sig Sum[(u sig^2)^k, {k, 0, 4}], Sg -> u sig, V -> u}, "LoopFactor" -> 2];
check["0d: dF/dV at fixed S = dF/du = s^2/(u (u - s^2)) (Hellmann-Feynman), to u^3",
  Expand[Sum[Coefficient[Expand[dfdvFix], u, k] u^k, {k, 0, 3}] - Sum[u^k sig^(2 k + 2), {k, 0, 3}]], 0];
check["the symbol G[S] in LaTeX", ExpressionToLaTeX[LegendrePropagator[G -> Sg, {V}][x, y]], "G[\\mathrm{Sg}](x,y)"];

(* ------------------------------------------------------------------------- *)
If[TrueQ[effectiveActionsAlone],
  Print["\n", passed, " passed, ", failed, " failed", If[failed > 0, ": " <> StringRiffle[failures, ", "], ""], "   (",
    Round[AbsoluteTime[] - startTime], " s)"]];
