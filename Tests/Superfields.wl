(* ::Package:: *)

(* ========================================================================= *)
(*  Tests of superfields: the Nambu field (psi, psibar) as one Grassmann     *)
(*  field with an antisymmetric propagator calG = -<T psi_a psi_b> (normal   *)
(*  block G, anomalous blocks -<T psi psi> and -<T psibar psibar>), the      *)
(*  derivatives with respect to antisymmetric kernels, the self-energy and   *)
(*  the kernels of the three channels from one functional, the projection on *)
(*  the components, the 2PI action at finite means and zero dimensions.      *)
(*  Read by RunTests.wl after EffectiveActions.wl, or run alone:             *)
(*    wolfram -script Superfields.wl                                         *)
(* ========================================================================= *)

If[! ValueQ[passed],
  (* alone *)
  testDirectory = If[StringQ[$InputFileName] && $InputFileName =!= "", DirectoryName[$InputFileName], Directory[]];
  Get[FileNameJoin[{testDirectory, "..", "FunctionalDiagrams.wl"}]];
  Get[FileNameJoin[{testDirectory, "Harness.wl"}]];
  superfieldsAlone = True,
  (* read by RunTests.wl: a fresh start, as in a new session *)
  Quiet[Scan[(Unprotect[#]; ClearAll[#]) &, Complement[Names["Global`*"], harnessNames]]];
  FunctionalDiagrams`ResetDeclarations[]];

(* ------------------------------------------------------------------------- *)
section["derivatives with respect to antisymmetric kernels"];
DeclareAntisymmetric[Apair, Gpp, Gbb, Nambu];
DeclareSymmetric[V]; DeclareInteractionLine[V];
(* dA(u, v)/dA(x, y) = (delta(u, x) delta(v, y) - delta(u, y) delta(v, x))/2 *)
checkZero["d/dA(x, y) of Integral A(u, v) f(u) q(v), A antisymmetric: antisymmetrised",
  FunctionalDerivative[IntegralOver[Apair[u, v] f[u] q[v], {u, v}], Apair[x, y]] - (f[x] q[y] - f[y] q[x])/2];
checkZero["the same written as -Integral A(v, u) f(u) q(v): the same derivative",
  FunctionalDerivative[-IntegralOver[Apair[v, u] f[u] q[v], {u, v}], Apair[x, y]] - (f[x] q[y] - f[y] q[x])/2];
check["the labels out of canonical order: d/dA(y, x) = -d/dA(x, y) (A[y, x] has evaluated to -A[x, y]), no message",
  {SimplifyDiagrams[FunctionalDerivative[IntegralOver[V[u, w] Gbb[u, w] Gbb[u, w], {u, w}], Gbb[x3, x1]] +
     FunctionalDerivative[IntegralOver[V[u, w] Gbb[u, w] Gbb[u, w], {u, w}], Gbb[x1, x3]]],
   SimplifyDiagrams[FunctionalDerivative[IntegralOver[V[u, w] Gbb[u, w] Gbb[u, w], {u, w}], Gbb[x1, x3]] - 2 V[x1, x3] Gbb[x1, x3]]}, {0, 0}];
(* the Bogoliubov term of the first-order functional, -1/2 Integral V Gbb Gpp *)
checkZero["second derivative of -1/2 Integral V Gbb Gpp with respect to Gbb(x3, x4) and Gpp(x1, x2)",
  FunctionalDerivative[FunctionalDerivative[-1/2 IntegralOver[V[u, w] Gbb[u, w] Gpp[u, w], {u, w}], Gbb[x3, x4]], Gpp[x1, x2]] -
   (DeltaFunction[x1, x4] DeltaFunction[x2, x3] - DeltaFunction[x1, x3] DeltaFunction[x2, x4]) V[x1, x2]/4];
check["the kernel of the inverse of an antisymmetric operator is antisymmetric",
  {SimplifyDiagrams[OperatorKernel[Power[Nambu, -1]][a, b] + OperatorKernel[Power[Nambu, -1]][b, a]], OperatorKernel[Power[Nambu, -1]][a, a]}, {0, 0}];
check["ExpandTrace contracts an antisymmetric kernel with its inverse in every orientation",
  ExpandTrace /@ {IntegralOver[OperatorKernel[Power[Nambu, -1]][a, u] Nambu[u, b], {u}], IntegralOver[OperatorKernel[Power[Nambu, -1]][a, u] Nambu[b, u], {u}],
    IntegralOver[OperatorKernel[Power[Nambu, -1]][u, a] Nambu[u, b], {u}], IntegralOver[OperatorKernel[Power[Nambu, -1]][u, a] Nambu[b, u], {u}]},
  {DeltaFunction[a, b], -DeltaFunction[a, b], -DeltaFunction[a, b], DeltaFunction[a, b]}];

(* ------------------------------------------------------------------------- *)
(* the superfield chi_a = (psi, psibar): one Grassmann source, one antisymmetric propagator calG = -<T chi chi>, one totally
   antisymmetric vertex: written as for phi^4 *)
section["the superfield: Luttinger-Ward functional, self-energy, kernel"];
DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex]; DeclareLocalVertex[NambuVertex];
nambuZ0 = Exp[1/2 IntegralOver[BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b], {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d] NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
phiNambu = LuttingerWardFunctional[nambuZ0, nambuInteraction, 3, BareNambuPropagator -> NambuPropagator];
check["Phi of the superfield: one diagram at each order to V^3", Table[Length[DiagramTerms[OrderPart[phiNambu, NambuVertex, k]]], {k, 3}], {1, 1, 1}];
checkZero["Phi of the superfield at first order: 1/8 Integral calV calG calG", OrderPart[phiNambu, NambuVertex, 1] -
   1/8 IntegralOver[NambuVertex[a, b, c, d] NambuPropagator[a, b] NambuPropagator[c, d], {a, b, c, d}]];
sigmaNambu = SelfEnergy[phiNambu, NambuPropagator, {a, b}, "Statistics" -> "Superfield"];
checkZero["the superfield self-energy at first order: -1/2 Integral calV(a, b, c, d) calG(c, d)", OrderPart[sigmaNambu, NambuVertex, 1] +
   1/2 IntegralOver[NambuVertex[a, b, c, d] NambuPropagator[c, d], {c, d}]];
checkZero["the superfield self-energy is antisymmetric", sigmaNambu + (sigmaNambu /. {a -> b, b -> a})];
kernelNambu = IrreducibleVertex[phiNambu, NambuPropagator, {a, b, c, d}, "Statistics" -> "Superfield"];
check["the superfield kernel 4 d^2Phi/dcalG dcalG: calV at first order; 1, 2, 6 diagrams to V^3",
  {OrderPart[kernelNambu, NambuVertex, 1], Table[Length[DiagramTerms[OrderPart[kernelNambu, NambuVertex, k]]], {k, 3}]}, {NambuVertex[a, b, c, d], {1, 2, 6}}];
check["\"Superfield\" with a propagator that is not antisymmetric: a message",
  Module[{gn}, DeclareDirectedPropagator[gn]; Quiet[Check[SelfEnergy[IntegralOver[gn[u, w] gn[w, u], {u, w}], gn, {x, y}, "Statistics" -> "Superfield"]; "none",
     "message", FunctionalDiagrams::superfield], FunctionalDiagrams::superfield]], "message"];

(* ------------------------------------------------------------------------- *)
(* the kernels of the three channels from one functional: Gamma_r(a, b, c, d) = sgn(pi_r) Gamma0(pi_r(a, b, c, d)) *)
section["the superfield: the kernels of the three channels, the parquet equations"];
channelsNambu = IrreducibleVertex[phiNambu, NambuPropagator, {a, b, c, d}, "Statistics" -> "Superfield", "Channel" -> All];
fullNambu = PerturbativeFullVertex[nambuZ0, nambuInteraction, 3, {NambuField}, {a, b, c, d}, BareNambuPropagator -> NambuPropagator];
parquetNambu = ParquetDecomposition[fullNambu, {a, b, c, d}, {NambuPropagator}, "Statistics" -> "Superfield"];
check["Gamma_r = F - R_r in the channels s, t, u to V^3", Table[SimplifyDiagrams[fullNambu - parquetNambu[r] - channelsNambu[r]], {r, {"s", "t", "u"}}], {0, 0, 0}];
check["Gamma_s = the kernel; Gamma_t = -Gamma0(a, c, b, d), Gamma_u = Gamma0(a, d, b, c)",
  {SimplifyDiagrams[channelsNambu["s"] - kernelNambu], SimplifyDiagrams[channelsNambu["t"] + (kernelNambu /. {b -> c, c -> b})],
   SimplifyDiagrams[channelsNambu["u"] - (kernelNambu /. {b -> d, c -> b, d -> c})]}, {0, 0, 0}];
parquetCheckNambu = CheckParquetEquations[fullNambu, {a, b, c, d}, NambuPropagator, NambuVertex, 3, "Statistics" -> "Superfield"];
check["CheckParquetEquations for the superfield (ladders +1/2, -1/2, +1/2 in s, t, u): residuals 0 to V^3", Union[Flatten[Values[parquetCheckNambu["Residuals"]]]], {0}];
check["the superfield vertex by class: fully irreducible {1, 0, 0}, s, t, u {0, 1, 3}",
  Values[parquetCheckNambu["DiagramCounts"]], {{1, 0, 0}, {0, 1, 3}, {0, 1, 3}, {0, 1, 3}, {0, 0, 0}}];
checkZero["the superfield vertex amputated with external labels that sort late", (PerturbativeFullVertex[nambuZ0, nambuInteraction, 2, {NambuField},
     {z1, z2, z3, z4}, BareNambuPropagator -> NambuPropagator] /. {z1 -> a, z2 -> b, z3 -> c, z4 -> d}) - OrderPart[fullNambu, NambuVertex, {1, 2}]];

section["the kernels of the channels of phi^4 and of the electron gas"];
DeclareSource[J]; DeclareField[phi, J]; DeclareSymmetric[D0, Gb];
Zb = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]]; Vb = lambda/24 IntegralOver[phi[z]^4, {z}];
phiB = LuttingerWardFunctional[Zb, Vb, 2, D0 -> Gb];
channelsB = IrreducibleVertex[phiB, Gb, {x1, x2, x3, x4}, "Statistics" -> "Boson", "Channel" -> All];
fullB = PerturbativeFullVertex[Zb, Vb, 2, {phi}, {x1, x2, x3, x4}, D0 -> Gb];
parquetB = ParquetDecomposition[fullB, {x1, x2, x3, x4}, {Gb}, "Statistics" -> "Boson"];
check["phi^4: Gamma_r = Gamma^(4) - R_r in the channels s, t, u to lambda^2; \"s\" is the default",
  Append[Table[SimplifyDiagrams[fullB - parquetB[r] - channelsB[r]], {r, {"s", "t", "u"}}],
   SimplifyDiagrams[channelsB["s"] - IrreducibleVertex[phiB, Gb, {x1, x2, x3, x4}, "Statistics" -> "Boson"]]], {0, 0, 0, 0}];
DeclareGrassmannSource[eta, etabar]; DeclareDirectedPropagator[G0, G];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
DeclareInteractionLine[V];
Zf = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
Vf = 1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
phiF2 = LuttingerWardFunctional[Zf, Vf, 2, G0 -> G];
fullF2 = PerturbativeFullVertex[Zf, Vf, 2, {psi, psibar}, {x1, x2, x3, x4}, G0 -> G];
parquetF2 = ParquetDecomposition[fullF2, {x1, x2, x3, x4}, {G}];
check["electron gas: the particle-hole kernel (the default) and the transverse one, -Gamma0(x3, x2; x1, x4) = F - R_phbar, to V^2",
  {SimplifyDiagrams[fullF2 - parquetF2["ParticleHole"] - IrreducibleVertex[phiF2, G, {x1, x2, x3, x4}]],
   SimplifyDiagrams[fullF2 - parquetF2["TransverseParticleHole"] - IrreducibleVertex[phiF2, G, {x1, x2, x3, x4}, "Channel" -> "TransverseParticleHole"]]}, {0, 0}];
check["electron gas: the particle-particle kernel is not a derivative with respect to G (message ppkernel)",
  Quiet[Check[IrreducibleVertex[phiF2, G, {x1, x2, x3, x4}, "Channel" -> "ParticleParticle"], "message", FunctionalDiagrams::ppkernel], FunctionalDiagrams::ppkernel], "message"];

(* ------------------------------------------------------------------------- *)
(* the components: calG = [[calG_psipsi, G], [-G^T, calG_psibarpsibar]], the vertex from the interaction of the electron gas *)
section["SuperfieldComponents: the Nambu-Gor'kov functional, the gap function, the pp kernel"];
DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar, BareAnomalousGreenFunction, BareAnomalousGreenFunctionBar, GapFunction, GapFunctionBar, NambuSelfEnergy];
DeclareDirectedPropagator[NormalSelfEnergy];
nambuRules = {NambuField -> {psi, psibar}, NambuPropagator -> {G, AnomalousGreenFunction, AnomalousGreenFunctionBar}, NambuVertex -> Vf};
normalRules = {NambuField -> {psi, psibar}, NambuPropagator -> {G, 0, 0}, NambuVertex -> Vf};
phiNambu2 = OrderPart[phiNambu, NambuVertex, {1, 2}];
phiComponents = SuperfieldComponents[phiNambu2, {}, nambuRules];
(* the component route: Z0 with the pairing terms written by hand *)
zPairing = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}] + 1/2 IntegralOver[BareAnomalousGreenFunction[x, y] etabar[x] ** etabar[y], {x, y}] +
    1/2 IntegralOver[BareAnomalousGreenFunctionBar[x, y] eta[x] ** eta[y], {x, y}]];
check["the components of Phi of the superfield: 3 and 7 diagrams at V, V^2, the Phi of Z0 with pairing terms",
  {Table[Length[DiagramTerms[OrderPart[phiComponents, V, k]]], {k, 2}], SimplifyDiagrams[phiComponents - LuttingerWardFunctional[zPairing, Vf, 2,
      {G0 -> G, BareAnomalousGreenFunction -> AnomalousGreenFunction, BareAnomalousGreenFunctionBar -> AnomalousGreenFunctionBar}]]}, {{3, 7}, 0}];
checkZero["at zero anomalous propagators: today's Phi[G]", SuperfieldComponents[phiNambu2, {}, normalRules] - phiF2];
sigmaNambu1 = OrderPart[sigmaNambu, NambuVertex, 1];
check["first order: Sigma = Hartree + Fock, the gap function Delta = V calG_psipsi, Deltabar = V calG_psibarpsibar (Hartree-Fock-Bogoliubov, BCS)",
  {SimplifyDiagrams[SuperfieldComponents[sigmaNambu1, {a -> psibar[x1], b -> psi[x2]}, nambuRules] - OrderPart[SelfEnergy[phiF2, G, {x1, x2}], V, 1]],
   SuperfieldComponents[sigmaNambu1, {a -> psibar[x1], b -> psibar[x2]}, nambuRules], SuperfieldComponents[sigmaNambu1, {a -> psi[x1], b -> psi[x2]}, nambuRules]},
  {0, V[x1, x2] AnomalousGreenFunction[x1, x2], V[x1, x2] AnomalousGreenFunctionBar[x1, x2]}];
nambuLegs = {a -> psibar[x1], b -> psi[x2], c -> psibar[x3], d -> psi[x4]};
channelsNambu2 = (OrderPart[#, NambuVertex, {1, 2}] &) /@ channelsNambu;
ppKernel = SuperfieldComponents[channelsNambu2["t"], nambuLegs, normalRules];
check["the channels s, t, u projected (normal state): Gamma0 (ph), F - R_pp, F - R_phbar of the electron gas, to V^2",
  {SimplifyDiagrams[SuperfieldComponents[channelsNambu2["s"], nambuLegs, normalRules] - IrreducibleVertex[phiF2, G, {x1, x2, x3, x4}]],
   SimplifyDiagrams[ppKernel - (fullF2 - parquetF2["ParticleParticle"])],
   SimplifyDiagrams[SuperfieldComponents[channelsNambu2["u"], nambuLegs, normalRules] - (fullF2 - parquetF2["TransverseParticleHole"])]}, {0, 0, 0}];
checkZero["the pp kernel = -4 d^2Phi/dcalG_psibarpsibar(x1, x3) dcalG_psipsi(x2, x4) at zero anomalous propagators, to V^2",
  (-4 FunctionalDerivative[FunctionalDerivative[phiComponents, AnomalousGreenFunctionBar[x1, x3]], AnomalousGreenFunction[x2, x4]] /.
     {AnomalousGreenFunction[__] -> 0, AnomalousGreenFunctionBar[__] -> 0}) - ppKernel];
check["the components of the vertex from the interaction: V(x1, x2) (delta14 delta23 - delta13 delta24) at (psibar, psibar, psi, psi), 0 at (psibar, psi, psi, psi)",
  {SimplifyDiagrams[SuperfieldComponents[NambuVertex[a, b, c, d], {a -> psibar[x1], b -> psibar[x2], c -> psi[x3], d -> psi[x4]}, nambuRules] -
     V[x1, x2] (DeltaFunction[x1, x4] DeltaFunction[x2, x3] - DeltaFunction[x1, x3] DeltaFunction[x2, x4])],
   SuperfieldComponents[NambuVertex[a, b, c, d], {a -> psibar[x1], b -> psi[x2], c -> psi[x3], d -> psi[x4]}, nambuRules]}, {0, 0}];
gorkovRules = {NambuField -> {psi, psibar}, NambuPropagator -> {G, AnomalousGreenFunction, AnomalousGreenFunctionBar}, BareNambuPropagator -> {G0, 0, 0},
   NambuSelfEnergy -> {{GapFunctionBar, -NormalSelfEnergy[#2, #1] &}, {NormalSelfEnergy, GapFunction}}};
nambuDyson = IntegralOver[BareNambuPropagator[a, u] NambuSelfEnergy[u, v] NambuPropagator[v, b], {u, v}];
check["the Dyson equation of the superfield projected: the Gor'kov equations, G0 Sigma G + G0 Delta calG_psibarpsibar and G0 Sigma calG_psipsi - G0 Delta G^T",
  {SimplifyDiagrams[SuperfieldComponents[nambuDyson, {a -> psi[x], b -> psibar[y]}, gorkovRules] -
     (IntegralOver[G0[x, u] NormalSelfEnergy[u, v] G[v, y], {u, v}] + IntegralOver[G0[x, u] GapFunction[u, v] AnomalousGreenFunctionBar[v, y], {u, v}])],
   SimplifyDiagrams[SuperfieldComponents[nambuDyson, {a -> psi[x], b -> psi[y]}, gorkovRules] -
     (IntegralOver[G0[x, u] NormalSelfEnergy[u, v] AnomalousGreenFunction[v, y], {u, v}] - IntegralOver[G0[x, u] GapFunction[u, v] G[y, v], {u, v}])]}, {0, 0}];
check["the trace terms and the kernel of the inverse at zero anomalous propagators: -1/2 Tr ln(-calG^-1) - 1/2 Tr(calG0^-1 calG - 1) = today's, calG^-1(x psi, y psibar) = -G^-1(y, x)",
  {SimplifyDiagrams[SuperfieldComponents[-1/2 TraceLog[-Power[NambuPropagator, -1]] - 1/2 OperatorTrace[Dot[Power[BareNambuPropagator, -1], NambuPropagator] - 1], {},
      {NambuField -> {psi, psibar}, NambuPropagator -> {G, 0, 0}, BareNambuPropagator -> {G0, 0, 0}}] - EffectiveAction2PI[0, G0 -> G]],
   SuperfieldComponents[OperatorKernel[Power[NambuPropagator, -1]][a, b], {a -> psi[x], b -> psibar[y]}, normalRules]}, {0, -OperatorKernel[Power[G, -1]][y, x]}];
check["SuperfieldComponents: messages head, labels ($Failed), trace (the trace kept as it is)",
  Quiet[{SuperfieldComponents[phiNambu2, {}, {NambuField -> {psi, psibar}, NambuVertex -> Vf}], SuperfieldComponents[sigmaNambu1, {a -> psibar[x1]}, nambuRules],
    SuperfieldComponents[TraceLog[-Power[NambuPropagator, -1]], {}, nambuRules]}, {SuperfieldComponents::head, SuperfieldComponents::labels, SuperfieldComponents::trace}],
  {$Failed, $Failed, TraceLog[-Power[NambuPropagator, -1]]}];

(* ------------------------------------------------------------------------- *)
(* Gamma[X, calG] = S[X] - 1/2 Tr ln(-calG^-1) - 1/2 Tr(S''[X] calG - 1) + Gamma2, S[X] = -1/2 X calG0^-1 X + S_int[X] *)
section["EffectiveAction2PI and EffectiveAction1PI of the superfield: Nambu-Gor'kov, exact at finite means"];
DeclareAntisymmetric[NambuTwoPointSource, NambuInsertion];
gammaNambu = EffectiveAction2PI[phiNambu2, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield"];
check["Gamma[calG] = Phi - 1/2 Tr ln(-calG^-1) - 1/2 Tr(calG0^-1 calG - 1); at zero anomalous propagators today's Gamma[G] of fermions",
  {SimplifyDiagrams[gammaNambu - (phiNambu2 - 1/2 TraceLog[-Power[NambuPropagator, -1]] - 1/2 OperatorTrace[Dot[Power[BareNambuPropagator, -1], NambuPropagator] - 1])],
   SimplifyDiagrams[SuperfieldComponents[gammaNambu, {}, {NambuField -> {psi, psibar}, NambuPropagator -> {G, 0, 0}, BareNambuPropagator -> {G0, 0, 0}, NambuVertex -> Vf}] -
     EffectiveAction2PI[phiF2, G0 -> G]]}, {0, 0}];
check["-2 d(trace terms)/dcalG at calG = (calG0^-1 - S)^-1 is -S, to S^2",
  ExpandTrace[SelfEnergy[gammaNambu - phiNambu2, NambuPropagator, {a, b}, "Statistics" -> "Superfield"] /.
    NambuPropagator -> Power[Power[BareNambuPropagator, -1] - NambuInsertion, -1], BareNambuPropagator, 2], -NambuInsertion[a, b]];
check["with the two-point source K (pairing sources included): stationary where calG^-1 = calG0^-1 - K - Sigma",
  SimplifyDiagrams[SelfEnergy[EffectiveAction2PI[0, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield", "Source" -> NambuTwoPointSource],
     NambuPropagator, {a, b}, "Statistics" -> "Superfield"] - (OperatorKernel[Power[NambuPropagator, -1]][a, b] - OperatorKernel[Power[BareNambuPropagator, -1]][a, b] + NambuTwoPointSource[a, b])], 0];
gammaMean = EffectiveAction2PI[nambuZ0, nambuInteraction, 2, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield", "MeanFields" -> {NambuField -> NambuMean}];
check["Gamma[X, calG] at X = 0 is Gamma[calG]", SimplifyDiagrams[SetSourcesToZero[gammaMean] - gammaNambu], 0];
(* S''(a, b) = FunctionalDerivative[S, X[a], X[b]] = calG0^-1 - Sigma1 with -X X in place of calG: the vertices of degree two,
   normal and anomalous, closed by calG in the trace term *)
classicalS = -1/2 IntegralOver[OperatorKernel[Power[BareNambuPropagator, -1]][u, w] NambuMean[u] ** NambuMean[w], {u, w}] + (nambuInteraction /. NambuField -> NambuMean);
fluctuationS = FunctionalDerivative[classicalS, NambuMean[a], NambuMean[b]];
check["superfield: S''[X] = calG0^-1 - Sigma1[calG -> -X X]; to first order Gamma = S - 1/2 Tr ln(-calG^-1) - 1/2 Tr(S'' calG - 1) + Phi[calG]",
  {SimplifyDiagrams[fluctuationS - OperatorKernel[Power[BareNambuPropagator, -1]][a, b] +
     (OrderPart[sigmaNambu, NambuVertex, 1] /. NambuPropagator[p_, q_] :> -GrassmannProduct[NambuMean[p], NambuMean[q]])],
   SimplifyDiagrams[ExpandTrace[OrderPart[gammaMean, NambuVertex, {0, 1}] - classicalS + 1/2 TraceLog[-Power[NambuPropagator, -1]] +
      1/2 IntegralOver[(fluctuationS /. {a -> u, b -> w}) NambuPropagator[w, u], {u, w}] - 1/2 OperatorTrace[1] - OrderPart[phiNambu, NambuVertex, 1]]]}, {0, 0}];
(* the stationary calG[X] to first order is enough to V^2 *)
sigmaMean1 = OrderPart[SelfEnergy[gammaMean, NambuPropagator, {a, b}, "Statistics" -> "Superfield"], NambuVertex, 1] /. NambuPropagator -> BareNambuPropagator;
gammaAtStationary = SimplifyDiagrams[Sum[ExpandTrace[OrderPart[gammaMean, NambuVertex, m] /. NambuPropagator -> Power[Power[BareNambuPropagator, -1] - LabelFunction[sigmaMean1, {a, b}], -1],
      BareNambuPropagator, 2 - m, "MaximumOrder" -> {NambuVertex, 2}], {m, 0, 2}]];
onePINambu = EffectiveAction1PI[nambuZ0, nambuInteraction, 2, NambuField -> NambuMean, BareNambuPropagator -> BareNambuPropagator, "Statistics" -> "Superfield"];
check["at its stationary calG[X], Gamma[X, calG] is the 1PI action Gamma[X] (bare lines), orders 0-2",
  Table[SimplifyDiagrams[OrderPart[gammaAtStationary - onePINambu, NambuVertex, k]], {k, 0, 2}], {0, 0, 0}];
onePIF2 = EffectiveAction1PI[Zf, Vf, 2, {psi -> Psi, psibar -> Psibar}, G0 -> G0];
meanRules = {NambuField -> {psi, psibar}, NambuMean -> {Psi, Psibar}, BareNambuPropagator -> {G0, 0, 0}, NambuVertex -> Vf};
check["Gamma[X] projected: Gamma[Psibar, Psi] of the electron gas, its anomalous diagram included (5 and 15 diagrams at V, V^2)",
  {SimplifyDiagrams[SuperfieldComponents[onePINambu, {}, meanRules] - onePIF2], Table[Length[DiagramTerms[OrderPart[onePIF2, V, k]]], {k, 2}]}, {0, {5, 15}}];
check["at X = 0 and fixed calG: d^2 Gamma/dX dX = calG0^-1 - Sigma[calG], d^4 Gamma/dX^4 = calV (the fully irreducible vertex to V^2)",
  {SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaMean, NambuMean[a], NambuMean[b]]] - OperatorKernel[Power[BareNambuPropagator, -1]][a, b] +
     OrderPart[sigmaNambu, NambuVertex, {1, 2}]], SetSourcesToZero[FunctionalDerivative[gammaMean, NambuMean[a], NambuMean[b], NambuMean[c], NambuMean[d]]]},
  {0, NambuVertex[a, b, c, d]}];
check["at zero anomalous propagators Gamma[X, calG] projected is today's Gamma[Psibar, Psi, G]",
  SimplifyDiagrams[SuperfieldComponents[gammaMean, {}, Append[meanRules, NambuPropagator -> {G, 0, 0}]] -
    EffectiveAction2PI[Zf, Vf, 2, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}]], 0];
(* with symbolic anomalous blocks: Tr ln(-calG^-1) has no component form and is kept (message trace), Tr(calG0^-1 calG - 1) is
   written as rings, the rest projected *)
anomalousRules = Append[meanRules, NambuPropagator -> {G, AnomalousGreenFunction, AnomalousGreenFunctionBar}];
gammaMeanAnomalous = Quiet[SuperfieldComponents[gammaMean, {}, anomalousRules], SuperfieldComponents::trace];
check["Gamma[X, calG] with symbolic anomalous blocks: message trace, -1/2 Tr ln(-calG^-1) kept, the rest projected; at zero anomalous blocks the normal-state components",
  {Quiet[Check[SuperfieldComponents[gammaMean, {}, anomalousRules]; False, True, SuperfieldComponents::trace], SuperfieldComponents::trace],
   SimplifyDiagrams[gammaMeanAnomalous - (gammaMeanAnomalous /. TraceLog[_] -> 0)],
   SimplifyDiagrams[ExpandTrace[(gammaMeanAnomalous /. TraceLog[_] -> 0) /. {AnomalousGreenFunction[__] -> 0, AnomalousGreenFunctionBar[__] -> 0}] -
     ExpandTrace[SuperfieldComponents[gammaMean, {}, Append[meanRules, NambuPropagator -> {G, 0, 0}]] /. TraceLog[_] -> 0]]},
  {True, -1/2 TraceLog[-Power[NambuPropagator, -1]], 0}];
checkZero["the pp kernel from the means: -2 FunctionalDerivative[Gamma, Gpsipsi[x2, x4], Psibar[x1], Psibar[x3]] at zero means and anomalous blocks = F - R_pp, to V^2",
  (-2 SetSourcesToZero[FunctionalDerivative[gammaMeanAnomalous, AnomalousGreenFunction[x2, x4], Psibar[x1], Psibar[x3]]] /.
     {AnomalousGreenFunction[__] -> 0, AnomalousGreenFunctionBar[__] -> 0}) - ppKernel];
(* the kept Tr ln(-calG^-1) depends on the anomalous blocks through calG: a derivative with respect to them gives the message
   supertrace; the route of the message, the derivative of the superfield trace first, then its components at zero anomalous
   blocks, gives d^2/dGbb(x1, x3) dGpsipsi(x2, x4) of -1/2 Tr ln(-calG^-1) = 1/4 (G^-1(x1, x4) G^-1(x3, x2) - G^-1(x1, x2) G^-1(x3, x4)),
   which on two sites is 1/4 d^2/dk dkbar of 1/2 log det calG at k = kbar = 0, -1/(4 det G) *)
check["a derivative of the kept trace with respect to an anomalous block: message supertrace; the superfield derivative first, then its components",
  {Quiet[Check[FunctionalDerivative[gammaMeanAnomalous, AnomalousGreenFunctionBar[x1, x3], AnomalousGreenFunction[x2, x4]]; False, True,
      FunctionalDerivative::supertrace], FunctionalDerivative::supertrace],
   SimplifyDiagrams[SuperfieldComponents[FunctionalDerivative[-1/2 TraceLog[-Power[NambuPropagator, -1]], NambuPropagator[c, d], NambuPropagator[a, b]],
      {a -> psi[x2], b -> psi[x4], c -> psibar[x1], d -> psibar[x3]}, {NambuField -> {psi, psibar}, NambuPropagator -> {G, 0, 0}}] -
     1/4 (OperatorKernel[Power[G, -1]][x1, x4] OperatorKernel[Power[G, -1]][x3, x2] - OperatorKernel[Power[G, -1]][x1, x2] OperatorKernel[Power[G, -1]][x3, x4])]},
  {True, 0}];
(* the fermionic counterpart of Eckhardt et al.: the third derivative of Gamma[Psibar, Psi, G] *)
gammaPsiDegree2 = EffectiveAction2PI[Zf, Vf, 2, G0 -> G, "MeanFields" -> {psi -> Psi, psibar -> Psibar}, "MeanFieldDegree" -> {2}];
checkZero["electron gas: d^3 Gamma/dPsibar(x1) dPsi(x2) dG(x4, x3) at zero means = Gamma0, to V^2",
  FunctionalDerivative[SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[gammaPsiDegree2, Psibar[x1]], Psi[x2]]], G[x4, x3]] - IrreducibleVertex[phiF2, G, {x1, x2, x3, x4}]];

(* ------------------------------------------------------------------------- *)
(* one site, spin 1/2: chi = (psiUp, psiDn, psibarUp, psibarDn), S_int = u n_up n_dn, Z(a, k, kbar) = a^2 + k kbar - u with the
   pairing sources; calG with g = G per spin, kappa = calG(psiUp, psiDn), kappabar = calG(psibarUp, psibarDn) *)
section["zero dimensions with arrays: the one-site check with pairing"];
siteG = {{0, kappa, g, 0}, {-kappa, 0, 0, g}, {-g, 0, 0, kappab}, {0, -g, -kappab, 0}};
siteValues = {NambuPropagator -> siteG, BareNambuPropagator -> (siteG /. {kappa -> 0, kappab -> 0, g -> 1/s}), NambuTwoPointSource -> {{0, -kb, 0, 0}, {kb, 0, 0, 0}, {0, 0, 0, k}, {0, 0, -k, 0}},
   NambuVertex -> Table[If[Sort[{i, j, l, m}] === {1, 2, 3, 4}, -u Signature[{i, j, l, m}], 0], {i, 4}, {j, 4}, {l, 4}, {m, 4}]};
siteRoot = Sqrt[1 + 4 u (g^2 - kappa kappab)];
check["Phi of the superfield in zero dimensions = the exact Phi(g, kappa, kappabar) (Phi(g) with g^2 -> g^2 - kappa kappabar), to u^3",
  Simplify[ZeroDimensionalValue[phiNambu, siteValues] - Normal[Series[siteRoot - 1 - Log[(1 + siteRoot)/2], {u, 0, 3}]]], 0];
check["the trace terms in zero dimensions: log(g^2 - kappa kappabar) - 2 (s g - 1) (compared by their derivatives)",
  Simplify[D[ZeroDimensionalValue[-1/2 TraceLog[-Power[NambuPropagator, -1]] - 1/2 OperatorTrace[Dot[Power[BareNambuPropagator, -1], NambuPropagator] - 1], siteValues] -
     (Log[g^2 - kappa kappab] - 2 (s g - 1)), #] & /@ {g, kappa, kappab, s}], {0, 0, 0, 0}];
siteGamma = ZeroDimensionalValue[EffectiveAction2PI[phiNambu, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield", "Source" -> NambuTwoPointSource], siteValues];
siteExact = {g -> s/(s^2 + k kb - u), kappa -> -k/(s^2 + k kb - u), kappab -> kb/(s^2 + k kb - u)};
check["with the pairing sources Gamma is stationary at the exact calG (g = s/Z, kappa = -k/Z, kappabar = kbar/Z), to u^3",
  Simplify[Normal[Series[(D[siteGamma, #] & /@ {g, kappa, kappab}) /. siteExact, {u, 0, 3}]]], {0, 0, 0}];
check["and its value there is -log Z, to u^3 (at s = 2, k = 1/3, kbar = 1/5, u = 1/10)",
  Chop[N[Normal[Series[(siteGamma /. siteExact) + Log[s^2 + k kb - u] /. {s -> 2, k -> 1/3, kb -> 1/5}, {u, 0, 3}]] /. u -> 1/10, 30]], 0];
siteSigma = SelfEnergy[phiNambu, NambuPropagator, {p, q}, "Statistics" -> "Superfield"];
check["Sigma(psibarUp, psiUp) at kappa = kappabar = 0 = the skeleton series (Sqrt[1 + 4 u g^2] - 1)/(2 g); Delta(psibarUp, psibarDn) = u kappa at first order",
  {Simplify[Normal[Series[(ZeroDimensionalValue[siteSigma, siteValues, "Indices" -> {p -> 3, q -> 1}] /. {kappa -> 0, kappab -> 0}) - (Sqrt[1 + 4 u g^2] - 1)/(2 g), {u, 0, 3}]]],
   Normal[Series[ZeroDimensionalValue[siteSigma, siteValues, "Indices" -> {p -> 3, q -> 4}], {u, 0, 1}]]}, {0, kappa u}];
checkMessage[ZeroDimensionalValue[siteSigma, siteValues], "arrays: a free label without an internal state gives the message indices",
  ZeroDimensionalValue::indices, $Failed];

(* ------------------------------------------------------------------------- *)
(* two sites with spin, x = 1..4 = (site, spin), chi = (psi_1..psi_4, psibar_1..psibar_4), S_int = 1/2 Sum V(x, y) psibar_x
   psibar_y psi_y psi_x with a generic symmetric V, a generic two-point source (normal, pairing, spin-flip): here the normal and
   the anomalous entries of calG are independent, so a wrong factor between them in the projection would show. The exact side is a
   Grassmann algebra of 8 generators written here (a monomial is a sorted list of generators); Z is a polynomial in u, and the
   comparison is exact, order by order in u, to u^2 *)
section["zero dimensions with arrays: two sites with spin, normal and anomalous entries independent"];
Module[{gi, gj, gCollect, gTimes, gExp, gCoefficient, mm, a0, kRaw, kk, aa, vBase, quadratic, quartic, weight, s0, z, cg, cg0, cg1, cg2,
   i0, sigmaExact, vals, phiExact, phiPackage, sigma2, components, entries},
 gCollect[l_] := Select[{#[[1, 1]], Expand[Total[#[[All, 2]]]]} & /@ GatherBy[l, First], #[[2]] =!= 0 &];
 gTimes[gx_, gy_] := gCollect[Flatten[Table[If[Intersection[gx[[gi, 1]], gy[[gj, 1]]] === {},
      {{Sort[Join[gx[[gi, 1]], gy[[gj, 1]]]], Signature[Join[gx[[gi, 1]], gy[[gj, 1]]]] gx[[gi, 2]] gy[[gj, 2]]}}, {}], {gi, Length[gx]}, {gj, Length[gy]}], 2]];
 gExp[e_] := Module[{t = {{{}, 1}}, s = {{{}, 1}}}, Do[t = {#[[1]], #[[2]]/k} & /@ gTimes[t, e]; s = gCollect[Join[s, t]], {k, 4}]; s];
 gCoefficient[x_, m_] := Total[Cases[x, {m, c_} :> c]];
 mm = {{3, 1/2, 1/3, 0}, {0, 3, 0, 1/3}, {1/3, 0, 5/2, 1/2}, {0, 1/3, 1/5, 5/2}};
 a0 = ArrayFlatten[{{0, -Transpose[mm]}, {mm, 0}}];
 kRaw = Table[If[i < j, (Mod[3 i + 5 j, 7] - 3)/10, 0], {i, 8}, {j, 8}]; kk = kRaw - Transpose[kRaw];
 aa = a0 - kk;
 vBase = {{2, 1, 1/2, 1/3}, {1, 2, 1/3, 1/2}, {1/2, 1/3, 2, 1}, {1/3, 1/2, 1, 2}};
 (* exp(1/2 chi A chi - S_int) *)
 quadratic = Flatten[Table[If[i < j && aa[[i, j]] =!= 0, {{{i, j}, aa[[i, j]]}}, {}], {i, 8}, {j, 8}], 2];
 quartic = gCollect[Flatten[Table[If[x =!= y, {{Sort[{4 + x, 4 + y, y, x}], -Signature[{4 + x, 4 + y, y, x}] u vBase[[x, y]]/2}}, {}], {x, 4}, {y, 4}], 2]];
 weight = gExp[gCollect[Join[quadratic, quartic]]];
 s0 = Sign[gCoefficient[gExp[quadratic], Range[8]]];
 z = s0 gCoefficient[weight, Range[8]];
 cg = Table[If[i == j, 0, With[{rest = Complement[Range[8], {i, j}]}, -s0 Signature[Join[{i, j}, rest]] gCoefficient[weight, rest]/z]], {i, 8}, {j, 8}];
 cg = Normal[Series[cg, {u, 0, 2}]];
 {cg0, cg1, cg2} = Table[Coefficient[cg, u, k], {k, 0, 2}] /. u -> 0;
 i0 = Inverse[cg0];
 sigmaExact = aa - (i0 - u i0 . cg1 . i0 + u^2 (i0 . cg1 . i0 . cg1 . i0 - i0 . cg2 . i0));
 vals = {G -> cg[[1 ;; 4, 5 ;; 8]], AnomalousGreenFunction -> cg[[1 ;; 4, 1 ;; 4]], AnomalousGreenFunctionBar -> cg[[5 ;; 8, 5 ;; 8]], V -> u vBase};
 phiExact = Normal[Series[-Log[z] - 1/2 Log[Det[cg]] + 1/2 (Tr[aa . cg] - 8), {u, 0, 2}]];
 phiPackage = Normal[Series[ZeroDimensionalValue[phiComponents, vals], {u, 0, 2}]];
 sigma2 = OrderPart[sigmaNambu, NambuVertex, {1, 2}];
 (* Sigma(x1, x2) = Sigma_(x1 psibar, x2 psi), Delta(x1, x2) = Sigma_(x1 psibar, x2 psibar), Deltabar(x1, x2) = Sigma_(x1 psi, x2 psi) *)
 components = {{SuperfieldComponents[sigma2, {a -> psibar[x1], b -> psi[x2]}, nambuRules], {4, 0}},
   {SuperfieldComponents[sigma2, {a -> psibar[x1], b -> psibar[x2]}, nambuRules], {4, 4}},
   {SuperfieldComponents[sigma2, {a -> psi[x1], b -> psi[x2]}, nambuRules], {0, 0}}};
 entries = {{1, 2}, {3, 1}, {2, 4}};
 check["two sites with spin: Phi to V^2 (SuperfieldComponents, G, calG_psipsi, calG_psibarpsibar as 4 x 4 arrays) = the exact Phi to u^2; Phi at u = 0 is 0",
  {Expand[phiPackage - phiExact], phiExact /. u -> 0}, {0, 0}];
 check["two sites with spin: the components of Sigma (normal, Delta, Deltabar) to V^2 = the exact calG0^-1 - K - calG^-1 to u^2",
  Union[Flatten[Table[Expand[Normal[Series[ZeroDimensionalValue[c[[1]], vals, "Indices" -> {x1 -> e[[1]], x2 -> e[[2]]}], {u, 0, 2}]] -
      sigmaExact[[c[[2, 1]] + e[[1]], c[[2, 2]] + e[[2]]]]], {c, components}, {e, entries}]]], {0}]];

(* ------------------------------------------------------------------------- *)
(* the anomalous lines in pictures: -<T psi psi> with two arrowheads to its ends, -<T psibar psibar> with two to its middle;
   the fermion chains run through them (the arrows turn inside the anomalous lines) *)
section["pictures with anomalous propagators"];
DeclareLineStyle[AnomalousGreenFunction, "DivergingArrows"]; DeclareLineStyle[AnomalousGreenFunctionBar, "ConvergingArrows"];
pictureItems[e_, ext_] := First[FunctionalDiagrams`Private`geometriesOf[e, ext, {}]][[2]];
deltaItems = pictureItems[V[x1, x2] AnomalousGreenFunction[x1, x2], {x1, x2}];
check["the gap function V calG_psipsi: legs Out, Out (two psibar ends), the line on the axis",
  {Cases[deltaItems, {"L", _, _, dir_} :> dir], Union[Cases[deltaItems, {"E", {"segment", p_, q_}, "DivergingArrows"} :> Chop[p[[2]] - q[[2]]]]]}, {{"Out", "Out"}, {0}}];
check["the Bogoliubov vacuum diagram V calG_psipsi calG_psibarpsibar: the geometry of the Fock diagram, a loop of two arcs",
  Sort[Cases[pictureItems[IntegralOver[V[u, w] AnomalousGreenFunction[u, w] AnomalousGreenFunctionBar[u, w], {u, w}], {}], {"E", path_, st_} :> {path[[1]], st}]],
  {{"arc", "ConvergingArrows"}, {"arc", "DivergingArrows"}, {"segment", "Wavy"}}];
check["a Gor'kov chain G calG_psipsi G^T from x to y: on the axis",
  Union[Cases[pictureItems[IntegralOver[G[x, u] AnomalousGreenFunction[u, w] G[y, w], {u, w}], {x, y}], {"E", {"segment", p_, q_}, _} :> Chop[{p[[2]], q[[2]]}]]], {{0, 0}}];
check["no density point at two psi ends (G entering and calG_psipsi at z), one at a psi and a psibar end",
  {Cases[pictureItems[IntegralOver[G[z, x] AnomalousGreenFunction[z, w] G[y, w], {w}], {x, y, z}], {"S", __}],
   Length[Cases[pictureItems[IntegralOver[G[x, z] AnomalousGreenFunction[z, w] G[y, w], {w}], {x, y, z}], {"S", __}]]}, {{}, 1}];

(* an anomalous line shows no orientation: the weight takes it read along its fermion line (a loop in either sense, an open
   line from its first external label), whatever the orientation of the canonical term *)
pictureWeights[e_, ext_] := First /@ FunctionalDiagrams`Private`geometriesOf[e, ext, {}];
DeclareBlob[GapFunction, "\\Delta"];
check["the weights read the anomalous lines along the fermion lines: V calG_psipsi calG_psibarpsibar (a loop), V calG_psipsi(x1, x2) from x1 or from x2, G calG_psipsi G^T both ways, G0 Delta calG_psibarpsibar",
  {pictureWeights[-1/2 IntegralOver[V[u, w] AnomalousGreenFunction[u, w] AnomalousGreenFunctionBar[u, w], {u, w}], {}],
   pictureWeights[V[x1, x2] AnomalousGreenFunction[x1, x2], {x1, x2}], pictureWeights[V[x1, x2] AnomalousGreenFunction[x1, x2], {x2, x1}],
   pictureWeights[IntegralOver[G[x, u] AnomalousGreenFunction[u, w] G[y, w], {u, w}], {x, y}],
   pictureWeights[IntegralOver[G[x, u] AnomalousGreenFunction[w, u] G[y, w], {u, w}], {x, y}],
   pictureWeights[IntegralOver[G0[x, u] GapFunction[u, w] AnomalousGreenFunctionBar[w, y], {u, w}], {x, y}]},
  {{-1/2}, {-1}, {1}, {1}, {-1}, {1}}];

DeclareMatsubaraFrequency[w, "Fermion"];
check["EvaluateMatsubara refuses an anomalous propagator (message anomalous), with valid rules and external points",
  Module[{given = False, value},
   value = Quiet[Check[EvaluateMatsubara[V[x1, x2] AnomalousGreenFunction[x1, x2], {x1 -> {k, I w}, x2 -> {-k, -I w}},
       {AnomalousGreenFunction -> {"Fermion", xi}, V -> {"Instantaneous", v}}], given = True; $Failed, EvaluateMatsubara::anomalous],
     EvaluateMatsubara::anomalous];
   {given, value}], {True, $Failed}];

(* ------------------------------------------------------------------------- *)
If[TrueQ[superfieldsAlone],
  Print["\n", passed, " passed, ", failed, " failed", If[failed > 0, ": " <> StringRiffle[failures, ", "], ""], "   (",
    Round[AbsoluteTime[] - startTime], " s)"]];
