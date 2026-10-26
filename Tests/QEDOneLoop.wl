(* ::Package:: *)

(* One-loop QED in dimensional regularization with the diagrams of the package: the electron self-energy, the vacuum
   polarization and the vertex, with the conventions of Peskin and Schroeder (P&S) except for the sign of the charge:
   Minkowski metric (+,-,-,-), the electron of charge -e with e > 0, L = psibar (i dslash - m) psi - 1/4 F^2
   + e psibar gamma^mu psi A_mu (P&S write -e psibar gamma^mu psi A_mu with e < 0 for the electron), vertex
   +i e gamma^mu, fermion line i (pslash + m)/(p^2 - m^2), photon -i [g - (1 - xi) q q/q^2]/q^2 (Feynman gauge xi = 1).
   d = 4 - 2 eps (P&S write d = 4 - eps: their 2/eps is our 1/eps) with the MS-bar measure mu^(2 eps) (e^gammaE/(4 pi))^eps
   d^dk/(2 pi)^d: P&S's 2/eps - gammaE + ln 4 pi + ln mu^2 is our 1/eps + ln mu^2.
   Run: wolfram -script QEDOneLoop.wl > QEDOneLoop.out (the report). Loaded by RunTests.wl with $qedReport = False: then it
   only defines qedOneLoop[] (the results, as rules). *)

If[! ValueQ[$FunctionalDiagramsVersion], Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "FunctionalDiagrams.wl"}]]];
If[! ValueQ[$qedReport], $qedReport = True];
(* HoldAll: without the report, its arguments (FullSimplify, NIntegrate of the references) are not computed *)
SetAttributes[report, HoldAll];
report[s__] := If[$qedReport, Print[s]];

(* ---- the theory in condensed notation: a label is a point with its spinor or Lorentz index ---- *)
DeclareSource[Jphoton]; DeclareGrassmannSource[eta, etabar];
DeclareField[photon, Jphoton]; DeclareField[electron, etabar]; DeclareField[electronbar, eta, -1];
DeclareSymmetric[PhotonPropagator]; DeclareLocalVertex[QEDVertex];
DeclareLineStyle[PhotonPropagator, "Wavy"]; DeclareDirectedPropagator[ElectronPropagator];
(* ElectronPropagator[x, y] = <T psi(x) psibar(y)>, PhotonPropagator[x, y] = <T A(x) A(y)> *)
qedZ0 = Exp[1/2 IntegralOver[Jphoton[x] PhotonPropagator[x, y] Jphoton[y], {x, y}] +
    IntegralOver[ElectronPropagator[x, y] etabar[x] ** eta[y], {x, y}]];
(* the package expands exp(-S); for exp(i S_Minkowski), S_int = e Int psibar gamma^mu psi A_mu, the interaction is
   -i S_int = e Int V psibar psi A with the local kernel V[x, y, z] = -i gamma^mu_z delta(x - y) delta(x - z): each vertex
   is then (-1) e (-i gamma^mu) = i e gamma^mu *)
qedInteraction = e IntegralOver[QEDVertex[x, y, z] electronbar[x] ** electron[y] photon[z], {x, y, z}];
qedLines = {PhotonPropagator, ElectronPropagator};
qedRules[xi_, lambda_: 0] := {ElectronPropagator -> {"Fermion", m}, PhotonPropagator -> {"Photon", {xi, lambda}}, QEDVertex -> {"Vertex", -I}};
(* the 1PI function of the fields at order e^n: the one-particle-irreducible part of the connected function, with its legs
   removed *)
qedVertexFunction[fields_List, labels_List, n_Integer] := ParticleIrreducibleDiagrams[AmputateExternalLegs[
     ConnectedCorrelationFunction[qedZ0, fields, qedInteraction, n, "CouplingOrder" -> {e, n}], labels, qedLines], 1, labels, qedLines];

alpha = e^2/(4 Pi);
L = -Log[m^2/RenormalizationScale^2];  (* ln(mu^2/m^2), in the form EpsilonExpand writes *)
zero[x_List] := AllTrue[x, zero];
zero[x_] := Simplify[x] === 0;
(* the coefficient of a structure s (a product of Dirac and tensor objects) in an expanded expression, without Coefficient
   (whose "variables" here are compound) *)
structureHeads = DiracProduct | DiracTrace | SpinorChain | GammaMatrix | SlashedMomentum | LorentzMetric | FourVector;
coefficientOf[e_, s_] := With[{x = Expand[e]}, Total[Select[Table[t/s, {t, If[Head[x] === Plus, List @@ x, {x}]}], FreeQ[#, structureHeads] &]]];

qedOneLoop[] := Module[{t0 = AbsoluteTime[], sigmaDiagrams, piDiagrams, vertexDiagrams, treeVertex, sigmaInt, sigma, sa, sb, pole,
    delta2MS, deltaMMS, zm, sap, sbp, dmOS, dz2OS, deltaMCountertermOS, dz2OSexpand, dmOSexpand, dz2OSlambda, piInt, piMuNu, gCoeff, qqCoeff, piQ,
    transverse, piPole, delta3, betaE, piHat, vertexInt, dGamma, chain, ff, f1, f2, f3, f2zero, f1pole, f1zero, f1lambda, ffLambda, f2q, results},
   (* ---- the diagrams ---- *)
   sigmaDiagrams = qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 2];
   piDiagrams = qedVertexFunction[{photon[x], photon[y]}, {x, y}, 2];
   treeVertex = qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 1];
   vertexDiagrams = qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 3];
   report["== diagrams of the package (1PI, amputated) =="];
   report["-i Sigma_2: ", InputForm[DiagramTerms[sigmaDiagrams]]];
   report["i Pi_2: ", InputForm[DiagramTerms[piDiagrams]]];
   report["tree vertex: ", InputForm[treeVertex], ";  one loop: ", Length[DiagramTerms[vertexDiagrams]], " term"];
   report["tree vertex in momentum space: ", InputForm[QEDIntegrand[treeVertex, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[1], "Indices" -> {x3 -> mu}]]];

   (* ---- electron self-energy: the diagram is -i Sigma_2(p) ---- *)
   sigmaInt = QEDIntegrand[sigmaDiagrams, {y -> p, x -> -p}, qedRules[xi]];
   report["\n== electron self-energy ==\n-i Sigma_2 = ", InputForm[sigmaInt]];
   sigma = DiracSimplify[I IntegrateOneLoop[sigmaInt, eps]];
   sa = coefficientOf[sigma, DiracProduct[]]/m; sb = coefficientOf[sigma, DiracProduct[SlashedMomentum[p]]];
   report["Sigma_2 = m A + pslash B, rest: ", Expand[sigma - m sa DiracProduct[] - sb DiracProduct[SlashedMomentum[p]]]];
   pole = {ExpandParameterIntegrals[sa, eps, -1], ExpandParameterIntegrals[sb, eps, -1]};
   report["pole parts {A, B}: ", pole, "   expected (alpha/(4 pi eps)) {3 + xi, -xi}: ", zero[pole - alpha/(4 Pi eps) {3 + xi, -xi}]];
   (* MS-bar counterterms (P&S sec. 10.3): the counterterm adds i(pslash delta2 - deltam) to -i Sigma, Sigma_R = Sigma_2 -
      pslash delta2 + deltam, with deltam = Z2 m0 - m (the counterterm, not the mass shift m - m0) *)
   delta2MS = pole[[2]]; deltaMMS = -m pole[[1]];
   zm = Expand[deltaMMS/m - delta2MS];  (* m0 = Zm m, deltam = Z2 m0 - m = m (delta2 + deltaZm) *)
   report["MS-bar: delta2 = ", delta2MS, ",  counterterm deltam = Z2 m0 - m = ", deltaMMS, ",  Zm - 1 = ", zm, " (gauge independent: ", FreeQ[zm, xi],
    "),  m - m0 = -(Zm - 1) m = ", -zm m];
   report["B finite part (Feynman gauge): ", InputForm[ExpandParameterIntegrals[sb /. xi -> 1, eps, 0]]];
   (* on shell: the mass shift m - m0 = Sigma(pslash = m) = m (A + B) (P&S eq. 7.27), delta Z2 = dSigma/dpslash = B + 2 m^2 (A' + B')
      at p^2 = m^2 (P&S eq. 7.26); the on-shell counterterm of P&S sec. 10.3 is deltam = Z2 m0 - m = m delta Z2 - Sigma(m) *)
   {sap, sbp} = {sa, sb} /. ScalarProduct[p, p] -> p2;
   dmOS = m (sap + sbp) /. p2 -> m^2;
   dz2OS = (sbp + 2 m^2 DifferentiateUnderIntegral[sap + sbp, p2]) /. p2 -> m^2;
   dmOSexpand = ExpandParameterIntegrals[dmOS/m, eps, 0];
   dz2OSexpand = ExpandParameterIntegrals[dz2OS, eps, 0];
   deltaMCountertermOS = Expand[m dz2OSexpand - m dmOSexpand];
   report["on shell (IR in dimensional regularization, eps_UV = eps_IR): mass shift (m - m0)/m = Sigma(m)/m = ", Collect[dmOSexpand, {eps, e}, Simplify]];
   report["   delta Z2 = ", Collect[dz2OSexpand, {eps, e}, Simplify], ";  counterterm deltam = Z2 m0 - m = m delta Z2 - Sigma(m) = ", Collect[deltaMCountertermOS, {eps, e}, Simplify]];
   report["   expected (m - m0)/m = -delta Z2 = (alpha/(4 pi)) (3/eps + 4 + 3 ln(mu^2/m^2)): ",
    zero[dmOSexpand - alpha/(4 Pi) (3/eps + 4 + 3 L)], ", ", zero[dz2OSexpand + alpha/(4 Pi) (3/eps + 4 + 3 L)]];
   (* with a photon mass lambda (Feynman gauge): the IR divergence is a logarithm of lambda *)
   dz2OSlambda = Module[{s = DiracSimplify[I IntegrateOneLoop[QEDIntegrand[sigmaDiagrams, {y -> p, x -> -p}, qedRules[1, lambda]], eps]], a, b},
     a = coefficientOf[s, DiracProduct[]]/m /. ScalarProduct[p, p] -> p2; b = coefficientOf[s, DiracProduct[SlashedMomentum[p]]] /. ScalarProduct[p, p] -> p2;
     ExpandParameterIntegrals[(b + 2 m^2 DifferentiateUnderIntegral[a + b, p2]) /. p2 -> m^2, eps, 0]];
   report["   delta Z2 with a photon mass lambda (Feynman gauge): ", InputForm[dz2OSlambda]];

   (* ---- vacuum polarization: the diagram is i Pi_2^{mu nu}(q) ---- *)
   piInt = QEDIntegrand[piDiagrams, {y -> q, x -> -q}, qedRules[xi], "Indices" -> {x -> mu, y -> nu}];
   report["\n== vacuum polarization ==\ni Pi_2 = ", InputForm[piInt]];
   piMuNu = DiracSimplify[-I IntegrateOneLoop[piInt, eps]];
   gCoeff = coefficientOf[piMuNu, LorentzMetric[mu, nu]]; qqCoeff = coefficientOf[piMuNu, FourVector[q, mu] FourVector[q, nu]];
   report["Pi^{mu nu} = g A + q q B, rest: ", Expand[piMuNu - gCoeff LorentzMetric[mu, nu] - qqCoeff FourVector[q, mu] FourVector[q, nu]],
    ", independent of xi: ", FreeQ[piMuNu, xi]];
   (* transversality: A + q^2 B = 0, exactly in d (checked numerically in d = 4 - 2 eps at eps = 3/10 and 0.6 + 0.2 i) *)
   transverse = ReduceParameterIntegrals[(gCoeff + ScalarProduct[q, q] qqCoeff) /. {ScalarProduct[q, q] -> -27/10, m -> 1, RenormalizationScale -> 1}];
   report["transversality A + q^2 B at q^2 = -2.7 m^2: eps = 3/10: ", Chop[Quiet[N[transverse /. eps -> 3/10], {NIntegrate::slwcon, NIntegrate::ncvb}]],
    ";  eps = -0.2: ", Chop[Quiet[N[transverse /. eps -> -1/5], {NIntegrate::slwcon, NIntegrate::ncvb}]],
    ";  exactly (integrand by FullSimplify, any q^2 and eps): ", Simplify[ReduceParameterIntegrals[gCoeff + ScalarProduct[q, q] qqCoeff] /.
      (h : ParameterIntegral | SimplexIntegral)[f_, x_] :> h[FullSimplify[f], x]]];
   piQ = -qqCoeff;  (* Pi^{mu nu} = (q^2 g - q q) Pi(q^2) *)
   piPole = ExpandParameterIntegrals[piQ, eps, -1];
   report["Pi_2(q^2) pole: ", piPole, "  expected -e^2/(12 pi^2 eps) = -alpha/(3 pi eps): ", zero[piPole + e^2/(12 Pi^2 eps)]];
   report["Pi_2(q^2) to eps^0: ", InputForm[ExpandParameterIntegrals[piQ, eps, 0]]];
   delta3 = piPole;  (* Pi_R = Pi_2 - delta3 *)
   (* beta(e): e0 = mu^eps Z1/(Z2 Sqrt[Z3]) e = mu^eps Z3^(-1/2) e (Ward identity Z1 = Z2, checked below) *)
   betaE = BetaFunctionFromZ[1 - delta3/2, e, eps, 3, "ScalePower" -> 1];
   report["delta3 = ", delta3, ";  beta(e) = ", betaE, "  expected e^3/(12 pi^2): ", zero[betaE - e^3/(12 Pi^2)]];
   (* the renormalized (on shell) Pi-hat(q^2) = Pi_2(q^2) - Pi_2(0) (P&S sec. 7.5) *)
   piHat = ExpandParameterIntegrals[piQ - (piQ /. ScalarProduct[q, q] -> 0), eps, 0];
   report["Pi-hat_2(q^2) = ", InputForm[piHat]];
   report["Pi-hat_2(-3 m^2): ", N[piHat /. {ScalarProduct[q, q] -> -3, m -> 1, RenormalizationScale -> 1}], "  P&S sec. 7.5: ",
    N[-2 alpha/Pi NIntegrate[x (1 - x) Log[1/(1 + 3 x (1 - x))], {x, 0, 1}]]];

   (* ---- the vertex: the diagram is i e delta Gamma^mu(p2, p1), q = p2 - p1 the photon momentum (in) ---- *)
   vertexInt = QEDIntegrand[vertexDiagrams, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi], "Indices" -> {x3 -> mu}];
   report["\n== vertex ==\ni e delta Gamma = ", InputForm[vertexInt]];
   dGamma = -I/e IntegrateOneLoop[vertexInt, eps];
   chain = DiracSimplify[SpinorChain[{p2, m}, dGamma, {p1, m}]] /. ScalarProduct[p1, p2] -> m^2 - q2/2;
   ff = VertexFormFactors[chain, mu, {p1, p2, m}];
   {f1, f2, f3} = ff;
   (* F3 = 0: at q^2 = 0 in closed form, at q^2 != 0 its integrand is odd under the exchange of the parameters of the two
      electron lines *)
   f3 = ReduceParameterIntegrals[f3];
   report["F3 (the q^mu part, 0 by the Ward identity) at q^2 = 0: ", Simplify[ReduceParameterIntegrals[f3 /. q2 -> 0]],
    ";  odd integrand at q^2 != 0: ", Module[{g = Total[Cases[{f3}, SimplexIntegral[h_, _] :> h, Infinity]]},
     Select[{{1, 2}, {1, 3}, {2, 3}}, Simplify[g + (g /. {FeynmanParameter[#[[1]]] -> FeynmanParameter[#[[2]]], FeynmanParameter[#[[2]]] -> FeynmanParameter[#[[1]]]})] === 0 &]]];
   f2zero = ExpandParameterIntegrals[f2 /. q2 -> 0, eps, 0];
   report["F2(0) = ", f2zero, "  expected alpha/(2 pi) = e^2/(8 pi^2), in every gauge: ", zero[f2zero - alpha/(2 Pi)]];
   (* the UV pole of F1: with a photon mass lambda (no IR divergence), at q^2 = 0 *)
   ffLambda = Module[{v = -I/e IntegrateOneLoop[QEDIntegrand[vertexDiagrams, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi, lambda], "Indices" -> {x3 -> mu}], eps]},
     VertexFormFactors[DiracSimplify[SpinorChain[{p2, m}, v, {p1, m}]] /. ScalarProduct[p1, p2] -> m^2 - q2/2, mu, {p1, p2, m}]];
   f1lambda = ffLambda[[1]] /. q2 -> 0;
   f1pole = ExpandParameterIntegrals[f1lambda, eps, -1];
   report["UV pole of F1 (photon mass, general xi): ", f1pole, " expected xi alpha/(4 pi eps): ", zero[f1pole - xi alpha/(4 Pi eps)], ";  Z1 = Z2 in MS-bar (pole of F1 = -delta2): ", zero[f1pole + delta2MS]];
   (* F2(q^2) at q^2 = -m^2 (Feynman gauge) against P&S sec. 6.3: (alpha/2pi) Int dx dy dz delta(1-x-y-z) 2 m^2 z(1-z)/(m^2(1-z)^2 - q^2 x y) *)
   f2q = ExpandParameterIntegrals[f2 /. {xi -> 1, q2 -> -m^2}, eps, 0];
   report["F2(-m^2) (Feynman gauge): ", N[f2q /. {m -> 1}], " P&S sec. 6.3: ", N[e^2/(8 Pi^2) NIntegrate[2 z (1 - z)/((1 - z)^2 + x (1 - x - z)), {z, 0, 1}, {x, 0, 1 - z}]],
    ";  its xi-dependent part (photon mass 1/10, exact in d at eps = -3/10, q^2 = -m^2): ",
    Chop[Quiet[N[ReduceParameterIntegrals[DifferentiateUnderIntegral[ffLambda[[2]], xi]] /. {q2 -> -1, m -> 1, lambda -> 1/10, RenormalizationScale -> 1, eps -> -3/10}],
      {NIntegrate::slwcon, NIntegrate::ncvb, NIntegrate::izero}], 10^-10], " (NIntegrate of an integrand that is 0 up to rounding)"];
   report["F1(0) - 1 with a photon mass (Feynman gauge), + delta Z2: ", Chop[N[(ExpandParameterIntegrals[f1lambda /. xi -> 1, eps, 0] + dz2OSlambda) /.
        {eps -> 1, m -> 1, lambda -> 1/1000, RenormalizationScale -> 1}]], " (the eps^-1 coefficient is replaced by 1: the poles cancel too)"];
   f1zero = ExpandParameterIntegrals[f1 /. q2 -> 0, eps, 0];
   report["F1(0) - 1 (IR in dim. reg.): ", Collect[f1zero, {eps, e}, Simplify], ";  F1(0) - 1 + delta Z2(on shell) = ", Simplify[f1zero + dz2OSexpand]];
   (* deltam: the counterterm Z2 m0 - m of P&S sec. 10.3, in both schemes; massShiftOS: (m - m0)/m (P&S eq. 7.27) *)
   results = {"Sigma" -> sigma, "SigmaPole" -> pole, "delta2MS" -> delta2MS, "deltamCountertermMS" -> deltaMMS, "ZmMinus1" -> zm,
     "massShiftOS" -> dmOSexpand, "deltaZ2OS" -> dz2OSexpand, "deltamCountertermOS" -> deltaMCountertermOS, "deltaZ2OSPhotonMass" -> dz2OSlambda,
     "PiMuNu" -> piMuNu, "Pi" -> piQ, "PiPole" -> piPole, "Transversality" -> transverse, "beta" -> betaE, "PiHat" -> piHat,
     "FormFactors" -> ff, "F2zero" -> f2zero, "F1zero" -> f1zero, "F1pole" -> f1pole, "F2minusM2" -> f2q, "FormFactorsPhotonMass" -> ffLambda, "Seconds" -> AbsoluteTime[] - t0};
   report["\nseconds: ", N[Round[10 (AbsoluteTime[] - t0)]/10]];
   results];

If[$qedReport, qedResults = qedOneLoop[]];
