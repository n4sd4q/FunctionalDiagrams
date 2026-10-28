(* ::Package:: *)

(* ========================================================================= *)
(*  Test suite of FunctionalDiagrams                                         *)
(*    wolfram -script RunTests.wl         (or wolframscript -file)           *)
(*  Every check prints PASS or FAIL; the summary is printed at the end.      *)
(*  The helpers (check, section, the checks of messages) are in Harness.wl. *)
(* ========================================================================= *)

testDirectory = If[StringQ[$InputFileName] && $InputFileName =!= "", DirectoryName[$InputFileName], Directory[]];
Get[FileNameJoin[{testDirectory, "..", "FunctionalDiagrams.wl"}]];
Get[FileNameJoin[{testDirectory, "Harness.wl"}]];

(* ------------------------------------------------------------------------- *)
section["functional derivatives"];
DeclareSource[J];
check["derivative of a source", FunctionalDerivative[J[x], J[y]], DeltaFunction[x, y]];
check["delta integrated", FunctionalDerivative[IntegralOver[f[x] J[x], {x}], J[y]], f[y]];
check["power", FunctionalDerivative[IntegralOver[J[x]^2 f[x], {x}], J[y]], 2 f[y] J[y]];
check["renaming of integration variables", SimplifyDiagrams[IntegralOver[f[x] g[x, y], {x}] - IntegralOver[f[u] g[u, y], {u}]], 0];
check["chain rule", SimplifyDiagrams[FunctionalDerivative[Sin[IntegralOver[g[x] J[x], {x}]], J[y]] - g[y] Cos[IntegralOver[g[x] J[x], {x}]]], 0];
check["order of derivatives", SimplifyDiagrams[FunctionalDerivative[IntegralOver[h[x, u] J[x] J[u], {x, u}], J[a], J[b]] - h[a, b] - h[b, a]], 0];
check["delta between external labels", SimplifyDiagrams[DeltaFunction[a, b] (f[a] - f[b])], 0];

(* ------------------------------------------------------------------------- *)
section["Wick's theorem"];
DeclareSymmetric[D0];
Z0 = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
DeclareField[phi, J];
check["boson two-point", FieldExpectationValue[Z0, {phi[a], phi[b]}], D0[a, b]];
check["boson four-point", Length[DiagramTerms[GaussianExpectationValue[Z0, {phi[a], phi[b], phi[c], phi[d]}]]], 3];
sixPairings = GaussianExpectationValue[Z0, {phi[a], phi[b], phi[c], phi[d], phi[e], phi[f]}];
check["boson six-point: 15 pairings", Length[DiagramTerms[sixPairings]], 15];
checkZero["boson six-point: pairings = differentiation", sixPairings -
   GaussianExpectationValue[Z0, {phi[a], phi[b], phi[c], phi[d], phi[e], phi[f]}, "Method" -> "Differentiation"]];
check["boson odd function vanishes", GaussianExpectationValue[Z0, {phi[a], phi[b], phi[c]}], 0];
DeclareGrassmannSource[eta, etabar]; DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Zf = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
check["fermion two-point (G0 = -<psi psibar>)", FieldExpectationValue[Zf, {psi[a], psibar[b]}], -G0[a, b]];
check["fermion two-point, other order", FieldExpectationValue[Zf, {psibar[b], psi[a]}], G0[a, b]];
checkZero["fermion four-point", GaussianExpectationValue[Zf, {psi[a], psi[b], psibar[c], psibar[d]}] - (G0[a, d] G0[b, c] - G0[a, c] G0[b, d])];
checkZero["fermion six-point = determinant", GaussianExpectationValue[Zf, {psi[a], psi[b], psi[c], psibar[d], psibar[e], psibar[f]}] -
   Det[Table[G0[i, j], {i, {a, b, c}}, {j, {d, e, f}}]]];
checkZero["fermion six-point, mixed order: pairings = differentiation",
  GaussianExpectationValue[Zf, {psi[a], psibar[d], psi[b], psibar[e], psi[c], psibar[f]}] -
   GaussianExpectationValue[Zf, {psi[a], psibar[d], psi[b], psibar[e], psi[c], psibar[f]}, "Method" -> "Differentiation"]];
DeclareDiscreteIndexValues[{up, down}];
Zh = Exp[-IntegralOver[G0[x, y] etabar[x, s] ** eta[y, s], {x, y, s}]];
check["discrete spin index", FieldExpectationValue[Zh, {psi[a, up], psibar[b, down]}], 0];

(* ------------------------------------------------------------------------- *)
section["perturbation theory: phi^4"];
Phi4Vertex = lambda/24 IntegralOver[phi[z]^4, {z}];
vacuumPhi4 = ConnectedVacuumDiagrams[Z0, Phi4Vertex, 2];
checkZero["phi^4 vacuum: eight, melon, cactus", vacuumPhi4 - (-lambda/8 IntegralOver[D0[z1, z1]^2, {z1}] + lambda^2/48 IntegralOver[D0[z1, z2]^4, {z1, z2}] +
     lambda^2/16 IntegralOver[D0[z1, z1] D0[z1, z2]^2 D0[z2, z2], {z1, z2}])];
propagatorPhi4 = PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 2];
checkZero["phi^4 propagator to second order", propagatorPhi4 - (D0[a, b] - lambda/2 IntegralOver[D0[a, z] D0[z, z] D0[z, b], {z}] +
     lambda^2/6 IntegralOver[D0[a, z1] D0[z1, z2]^3 D0[z2, b], {z1, z2}] + lambda^2/4 IntegralOver[D0[a, z2] D0[b, z2] D0[z1, z1] D0[z1, z2]^2, {z1, z2}] +
     lambda^2/4 IntegralOver[D0[a, z1] D0[z1, z1] D0[z1, z2] D0[z2, z2] D0[z2, b], {z1, z2}])];
checkZero["phi^4 propagator: pairings = differentiation (first order)", PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 1] -
   PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 1, "Method" -> "Differentiation"]];
checkZero["phi^4 connected four-point to first order", ConnectedCorrelationFunction[Z0, {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 1] +
   lambda IntegralOver[D0[a, z] D0[b, z] D0[c, z] D0[d, z], {z}]];
(* SetSourcesToZero holds a derivative written in it and drops the terms that cannot survive at zero sources early *)
z1Phi4 = SimplifyDiagrams[-lambda/24 IntegralOver[FunctionalDerivative[Z0, J[u], J[u], J[u], J[u]], {u}]];
d4Phi4 = FunctionalDerivative[Z0 + z1Phi4, J[a], J[b], J[c], J[d]];
checkZero["SetSourcesToZero of a held derivative = of the whole derivative", SetSourcesToZero[FunctionalDerivative[Z0 + z1Phi4, J[a], J[b], J[c], J[d]]] -
   SetSourcesToZero[d4Phi4]];
checkZero["SetSourcesToZero of a nested derivative with a minus sign", SetSourcesToZero[FunctionalDerivative[-FunctionalDerivative[Z0 + z1Phi4, J[b]], J[a]]] +
   SetSourcesToZero[Evaluate[FunctionalDerivative[Z0 + z1Phi4, J[a], J[b]]]]];
(* "MaximumOrder": the orders up to n, the higher ones dropped after each derivative; Log of a sum expanded in the coupling *)
checkZero["MaximumOrder: W = log Z to lambda", SetSourcesToZero[FunctionalDerivative[Log[Z0 + z1Phi4], J[a], J[b], "MaximumOrder" -> {lambda, 1}]] -
   D0[a, b] + lambda/2 IntegralOver[D0[a, z] D0[z, z] D0[z, b], {z}]];
checkZero["MaximumOrder = OrderPart, line head D0", FunctionalDerivative[Z0 + z1Phi4, J[a], J[b], "MaximumOrder" -> {D0, 2}] -
   OrderPart[FunctionalDerivative[Z0 + z1Phi4, J[a], J[b]], D0, {0, 1, 2}]];
checkZero["MaximumOrder = OrderPart, a coupling in an exponent", FunctionalDerivative[Exp[lambda IntegralOver[J[x] D0[x, y] J[y], {x, y}]], J[a], J[b],
    "MaximumOrder" -> {lambda, 1}] - 2 lambda D0[a, b] Exp[lambda IntegralOver[J[x] D0[x, y] J[y], {x, y}]]];
DeclareSource[KsMO]; DeclareSymmetric[GmMO]; DeclareFunctionalDerivative[MfMO[p_], KsMO[q_], GmMO[p, q]];
checkZero["MaximumOrder of a head with a declared derivative (lowered by it) = OrderPart", FunctionalDerivative[MfMO[x]^2 MfMO[y], KsMO[r],
    "MaximumOrder" -> {MfMO, 2}] - GmMO[r, y] MfMO[x]^2 - 2 GmMO[r, x] MfMO[x] MfMO[y]];
check["MaximumOrder: the series of a logarithm to order 10", TimeConstrained[Expand[FunctionalDiagrams`Private`logPowerSeries[1 + lambda q1 +
     lambda^2 q2, lambda, 10] - Normal[Series[Log[1 + lambda q1 + lambda^2 q2], {lambda, 0, 10}]]], 30, $Aborted], 0];
check["phi^4 2PI vacuum diagrams", Length[DiagramTerms[ParticleIrreducibleDiagrams[vacuumPhi4, 2, {}, {D0}]]], 2];
check["OrderPart with a coupling", Length[DiagramTerms[OrderPart[propagatorPhi4, lambda, 2]]], 3];

(* ------------------------------------------------------------------------- *)
section["perturbation theory: interacting fermions"];
DeclareSymmetric[V];
CoulombVertex = 1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
vacuumFermions = ConnectedVacuumDiagrams[Zf, CoulombVertex, 2];
check["fermion vacuum diagrams to second order", Length[DiagramTerms[vacuumFermions]], 7];
checkZero["fermion vacuum, first order: Hartree and Fock", OrderPart[vacuumFermions, V, 1] -
   (-1/2 IntegralOver[G0[z1, z1] G0[z2, z2] V[z1, z2], {z1, z2}] + 1/2 IntegralOver[G0[z1, z2] G0[z2, z1] V[z1, z2], {z1, z2}])];
greenFunction = -PerturbativeCorrelationFunction[Zf, {psi[a], psibar[b]}, CoulombVertex, 2];
check["Green function to second order: 13 diagrams", Length[DiagramTerms[greenFunction]], 13];
checkZero["Green function to first order: pairings = differentiation", OrderPart[greenFunction, V, 1] + OrderPart[
    PerturbativeCorrelationFunction[Zf, {psi[a], psibar[b]}, CoulombVertex, {1}, "Method" -> "Differentiation"], V, 1]];
oneParticleIrreducible = ParticleIrreducibleDiagrams[greenFunction, 1, {a, b}, {G0}];
check["1PI diagrams", Length[DiagramTerms[oneParticleIrreducible]], 8];
selfEnergyPerturbative = AmputateExternalLegs[oneParticleIrreducible, {a, b}, {G0}];
checkZero["Dyson equation to second order", DysonSeries[selfEnergyPerturbative /. {a -> s1, b -> s2}, {s1, s2}, G0, {a, b}, 2,
    "MaximumOrder" -> {V, 2}] - greenFunction];
skeletonSelfEnergy = SkeletonDiagrams[selfEnergyPerturbative, {a, b}, {G0}];
check["skeleton self-energy diagrams", Length[DiagramTerms[skeletonSelfEnergy]], 4];
checkZero["skeletons of G = skeletons of Sigma with legs", SkeletonDiagrams[greenFunction, {a, b}, {G0}] -
   IntegralOver[G0[a, u] (skeletonSelfEnergy /. {a -> u, b -> v}) G0[v, b], {u, v}]];

(* ------------------------------------------------------------------------- *)
section["Luttinger-Ward functional, self-energy, irreducible vertex"];
luttingerWard = LuttingerWardFunctional[Zf, CoulombVertex, 2, G0 -> G];
checkZero["Luttinger-Ward functional", luttingerWard - (-1/2 IntegralOver[G[z1, z2] G[z2, z1] V[z1, z2], {z1, z2}] +
     1/2 IntegralOver[G[z1, z1] G[z2, z2] V[z1, z2], {z1, z2}] - 1/4 IntegralOver[G[z1, z2] G[z2, z1] G[z3, z4] G[z4, z3] V[z1, z3] V[z2, z4], {z1, z2, z3, z4}] +
     1/4 IntegralOver[G[z1, z3] G[z2, z1] G[z3, z4] G[z4, z2] V[z1, z4] V[z2, z3], {z1, z2, z3, z4}])];
selfEnergy = SelfEnergy[luttingerWard, G, {a, b}];
checkZero["Sigma = dPhi/dG = skeleton self-energy", (selfEnergy /. G -> G0) - skeletonSelfEnergy];
irreducibleVertex = IrreducibleVertex[luttingerWard, G, {x1, x2, x3, x4}];
check["irreducible vertex: 8 diagrams", Length[DiagramTerms[irreducibleVertex]], 8];
checkZero["irreducible vertex symmetric under exchange of the pairs", irreducibleVertex - (irreducibleVertex /. {x1 -> x3, x2 -> x4, x3 -> x1, x4 -> x2})];
sigma12 = SelfEnergy[luttingerWard, G, {x1, x2}];
Do[checkZero["homogeneity, order " <> ToString[m], IntegralOver[G[x4, x3] OrderPart[irreducibleVertex, V, m], {x3, x4}] - (2 m - 1) OrderPart[sigma12, V, m]], {m, 1, 2}];
checkZero["Ward identity of charge conservation", IntegralOver[irreducibleVertex (theta[x4] - theta[x3]) G[x4, x3], {x3, x4}] - (theta[x1] - theta[x2]) sigma12];

(* ------------------------------------------------------------------------- *)
section["full vertex, Bethe-Salpeter equation, parquet"];
fullVertex = PerturbativeFullVertex[Zf, CoulombVertex, 2, {psi, psibar}, {x1, x2, x3, x4}, G0 -> G];
check["full vertex to second order: 2 + 10 diagrams", {Length[DiagramTerms[OrderPart[fullVertex, V, 1]]], Length[DiagramTerms[OrderPart[fullVertex, V, 2]]]}, {2, 10}];
checkZero["F = Gamma0 to first order", OrderPart[fullVertex - irreducibleVertex, V, 1]];
decomposition = ParquetDecomposition[fullVertex, {x1, x2, x3, x4}, {G}];
check["channel counts", Length[DiagramTerms[decomposition[#]]] & /@ {"FullyIrreducible", "ParticleHole", "TransverseParticleHole", "ParticleParticle", "SeveralChannels"},
  {2, 4, 4, 2, 0}];
checkZero["particle-hole irreducible part of F = Gamma0", fullVertex - decomposition["ParticleHole"] - irreducibleVertex];
checkZero["Bethe-Salpeter equation in the particle-hole channel", fullVertex - irreducibleVertex -
   BetheSalpeterLadder[irreducibleVertex, fullVertex, {x1, x2, x3, x4}, "ParticleHole", G, "MaximumOrder" -> {V, 2}]];
parquetResult = CheckParquetEquations[fullVertex, {x1, x2, x3, x4}, G, V, 2];
check["parquet equations (fermions, second order)", Values[parquetResult["Residuals"]], {{0, 0}, {0, 0}, {0, 0}}];
DeclareSymmetric[Gb];
vertexPhi4 = PerturbativeFullVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, c, d}, D0 -> Gb];
checkZero["phi^4 vertex: lambda and one bubble per channel", vertexPhi4 - (lambda DeltaFunction[a, b] DeltaFunction[a, c] DeltaFunction[a, d] -
     lambda^2/2 DeltaFunction[a, b] DeltaFunction[c, d] Gb[a, c]^2 - lambda^2/2 DeltaFunction[a, c] DeltaFunction[b, d] Gb[a, b]^2 -
     lambda^2/2 DeltaFunction[a, d] DeltaFunction[b, c] Gb[a, b]^2)];
check["parquet equations (phi^4, second order)", Values[CheckParquetEquations[vertexPhi4, {a, b, c, d}, Gb, lambda, 2, "Statistics" -> "Boson"]["Residuals"]],
  {{0, 0}, {0, 0}, {0, 0}}];
gamma2 = LuttingerWardFunctional[Z0, Phi4Vertex, 2, D0 -> Gb];
checkZero["phi^4 self-energy from Gamma2", SelfEnergy[gamma2, Gb, {a, b}, "Statistics" -> "Boson"] - (-lambda/2 DeltaFunction[a, b] Gb[a, a] + lambda^2/6 Gb[a, b]^3)];
phi4Gamma0 = IrreducibleVertex[gamma2, Gb, {a, b, c, d}, "Statistics" -> "Boson"];
bsObjects = {"F" -> LabelFunction[vertexPhi4, {a, b, c, d}], "Gamma0" -> LabelFunction[phi4Gamma0, {a, b, c, d}], "G" -> Gb};
check["exact Bethe-Salpeter equation (phi^4, second order), whole and with \"MaximumOrder\"",
  CheckEquation[ExactEquation["BetheSalpeter", bsObjects, {a, b, c, d}, "Statistics" -> "Boson", #], lambda, 2] & /@
   {"MaximumOrder" -> None, "MaximumOrder" -> {lambda, 2}}, {{0, 0, 0}, {0, 0, 0}}];
check["CheckEquation: the residual of each order, only up to the order asked",
  {CheckEquation[vertexPhi4 == phi4Gamma0, lambda, 1], CheckEquation[vertexPhi4 == phi4Gamma0, lambda, 2] ===
    {0, 0, OrderPart[vertexPhi4 - phi4Gamma0, lambda, 2]}, OrderPart[vertexPhi4 - phi4Gamma0, lambda, 2] =!= 0}, {{0, 0}, True, True}];

(* ------------------------------------------------------------------------- *)
section["response functions, loops"];
densityCorrelation = ConnectedCorrelationFunction[Zf, {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1];
check["density correlation to first order", Length[DiagramTerms[densityCorrelation]], 7];
check["RPA-reducible part", Length[DiagramTerms[ReducibleDiagrams[densityCorrelation, {{a}, {b}}, 1, {V}]]], 1];
check["closed loops", ClosedLoopCount[IntegralOver[G0[a, z1] G0[z1, b] G0[z2, z2] V[z1, z2], {z1, z2}], {G0}], 1];
check["no closed loops", ClosedLoopCount[IntegralOver[G0[a, z1] G0[z1, z2] G0[z2, b] V[z1, z2], {z1, z2}], {G0}], 0];

(* ------------------------------------------------------------------------- *)
section["tree expansion"];
check["bosonic four-point: 4 trees", Length[DiagramTerms[ConnectedFunctionFromVertices[{a, b, c, d}, "Boson"]]], 4];
check["bosonic five-point: 26 trees", Length[DiagramTerms[ConnectedFunctionFromVertices[{a, b, c, d, e}, "Boson"]]], 26];
check["fermionic four-point: one vertex", Length[DiagramTerms[ConnectedFunctionFromVertices[{a, b, c, d}, "Fermion"]]], 1];

(* ------------------------------------------------------------------------- *)
(* fermion lines: the 36 diagrams of the connected six-point function of the electron gas at second order are the nine
   trees of two bare vertices *)
lineTrees6 = ConnectedFunctionFromVertices[{x1, x2, x3, x4, x5, x6}, "FermionLines"];
check["tree expansion with fermion lines: 1 and 10 trees", {Length[DiagramTerms[ConnectedFunctionFromVertices[{x1, x2, x3, x4}, "FermionLines"]]],
   Length[DiagramTerms[lineTrees6]]}, {1, 10}];
bareGamma4 = AmputateExternalLegs[ConnectedCorrelationFunction[Zf, {psi[x1], psibar[x2], psi[x3], psibar[x4]}, CoulombVertex, {1}],
   {x1, x2, x3, x4}, {G0}];
checkZero["tree expansion with fermion lines = the electron gas at second order",
  ConnectedCorrelationFunction[Zf, {psi[x1], psibar[x2], psi[x3], psibar[x4], psi[x5], psibar[x6]}, CoulombVertex, {2}] -
   (Total[Select[DiagramTerms[lineTrees6], Count[#, _VertexFunction, Infinity] == 2 &]] /.
     {FullPropagator -> G0, VertexFunction[o1_, o2_, i1_, i2_] :> (bareGamma4 /. {x1 -> o1, x3 -> o2, x2 -> i1, x4 -> i2})})];

(* ------------------------------------------------------------------------- *)
section["pictures"];
DeclareDirectedPropagator[G]; DeclareInteractionLine[V];
picture = DiagramPlot[selfEnergy, {a, b}];
check["DiagramPlot gives Graphics", Head[picture], Graphics];
check["arrows on the legs and the lines of the Fock diagram", Count[DrawDiagram[-G[a, b] V[a, b], {a, b}], _Polygon, Infinity], 3];
check["one point for labels joined by delta functions", Count[DrawDiagram[DeltaFunction[a, b] DeltaFunction[c, d] V[a, c], {a, b, c, d}], _Disk, Infinity], 2];
check["legs without arrows on density vertices", Count[DrawDiagram[-G[a, b] G[b, a], {a, b}], _Polygon, Infinity], 2];
check["open circles at the ends of an interaction line", Count[DrawDiagram[V[a, b], {a, b}], _Disk, Infinity], 2];
tikz = DiagramToTikZ[selfEnergy, {a, b}];
pictureCount[s_String] := StringCount[s, "\\begin{tikzpicture}"];
check["TikZ export", {StringQ[tikz], pictureCount[tikz]}, {True, 4}];
check["TikZ: a number is written without a picture", DiagramToTikZ[1 - lambda/8 IntegralOver[D0[z, z]^2, {z}], {}, "Output" -> "List"][[1]], {"1", ""}];
occurrences[s_String, t_String] := StringCount[s, t];
(* the weights written with the pictures: an interaction line stands for -V, a vertex for minus its coupling *)
weightsOf[e_, ext_, opts___] := DiagramToTikZ[e, ext, "Output" -> "List", opts][[All, 1]];
check["weights: Fock and Hartree at first order, each line standing for -V; \"Coefficients\" gives the coefficients",
  With[{sigma1 = -G[a, b] V[a, b] + DeltaFunction[a, b] IntegralOver[V[a, z] G[z, z], {z}]},
   {weightsOf[sigma1, {a, b}], weightsOf[sigma1, {a, b}, "Weights" -> "Coefficients"]}], {{"1", "-1"}, {"-1", "1"}}];
check["weights: rings of two and three bubbles (two and three lines)",
  {weightsOf[-1/4 IntegralOver[V[u1, u2] V[u3, u4] G[u1, u3] G[u3, u1] G[u2, u4] G[u4, u2], {u1, u2, u3, u4}], {}],
   weightsOf[1/6 IntegralOver[V[u1, u2] V[u3, u4] V[u5, u6] G[u2, u3] G[u3, u2] G[u4, u5] G[u5, u4] G[u6, u1] G[u1, u6],
      {u1, u2, u3, u4, u5, u6}], {}]}, {{"-\\frac{1}{4}"}, {"-\\frac{1}{6}"}}];
DeclareCoupling[lambda];
check["weights: the phi^4 tadpole and sunset, each vertex standing for -lambda (DeclareCoupling)",
  {weightsOf[-lambda/2 IntegralOver[D0[a, z] D0[z, z] D0[z, b], {z}], {a, b}],
   weightsOf[lambda^2/6 IntegralOver[D0[a, z] D0[z, w]^3 D0[w, b], {z, w}], {a, b}],
   weightsOf[-lambda/2 IntegralOver[D0[a, z] D0[z, z] D0[z, b], {z}], {a, b}, "Weights" -> "Coefficients"]},
  {{"\\frac{1}{2}"}, {"\\frac{1}{6}"}, {"-\\frac{1}{2}\\,\\lambda"}}];
DeclareInteractionLine[Wscr, "DoubleWavy"];
check["DeclareInteractionLine with a style: a screened interaction drawn double, standing for -W",
  {FunctionalDiagrams`Private`lineStyleOf[Wscr], weightsOf[G[a, b] Wscr[a, b], {a, b}], weightsOf[G[a, b] U0[a, b], {a, b}, "Interactions" -> {U0}]},
  {"DoubleWavy", {"-1"}, {"-1"}}];
check["the option \"Couplings\": a coupling absorbed into the vertices of one picture only",
  {weightsOf[gc^2 IntegralOver[G[a, u] G[u, v] G[v, b] D0[u, v], {u, v}], {a, b}, "Couplings" -> {gc}],
   weightsOf[gc^2 IntegralOver[G[a, u] G[u, v] G[v, b] D0[u, v], {u, v}], {a, b}]}, {{"1"}, {"\\mathrm{gc}^{2}"}}];
(* the items of the picture of a term: {"E", path, style} for its lines, ... *)
geometryItems[e_, ext_, opts___] := First[FunctionalDiagrams`Private`geometriesOf[e, ext, {opts}]][[2]];
(* the heights of the middle points of the wavy arcs *)
wavyArcHeights[items_] := Cases[items, {"E", {"arc", c_, r_, a0_, da_}, "Wavy"} :> c[[2]] + r Sin[a0 + da/2]];
check["of two crossing arcs of the self-energy one is below the axis",
  Count[wavyArcHeights[geometryItems[IntegralOver[G[a, z1] G[z1, z2] G[z2, b] V[a, z2] V[z1, b], {z1, z2}], {a, b}]], h_ /; h < 0], 1];
check["a fermion loop of three vertices is a circle (with a tadpole hanging from it)",
  occurrences[DiagramToTikZ[IntegralOver[G[a, b] V[a, z1] V[b, z2] G[z1, z2] G[z2, z3] G[z3, z1] V[z3, z4] G[z4, z4], {z1, z2, z3, z4}], {a, b}], "arc["] >= 3, True];
check["a built-in symbol as a name is reported", quietCheck[DeclareLineStyle[Gamma, "Wavy"]; "silent", "reported"], "reported"];
check["two fermion loops joined by three lines: two circles, one inside the other",
  occurrences[DiagramToTikZ[IntegralOver[G[z1, z3] G[z2, z1] G[z3, z2] G[z4, z6] G[z5, z4] G[z6, z5] V[z1, z4] V[z2, z5] V[z3, z6],
     {z1, z2, z3, z4, z5, z6}], {}], "arc["], 6];

check["TikZ: line width and font of the labels", {occurrences[DiagramToTikZ[V[a, b], {a, b}, "LineWidth" -> 0.4], "line width=0.400pt"],
   occurrences[DiagramToTikZ[V[a, b], {a, b}, "LabelFont" -> "\\tiny"], "font=\\tiny"]}, {1, 2}];
(* the layout of fermion lines: the private functions are tested directly *)
arrowSegments[e_, ext_, opts___] := Cases[First[FunctionalDiagrams`Private`geometriesOf[e, ext, {opts}]][[2]], {"E", {"segment", p_, q_}, "Arrow"} :> {p, q}];
check["exchange diagram of the particle-particle channel: parallel fermion lines with \"Uncrossed\"",
  FunctionalDiagrams`Private`mutualCrossingCount[arrowSegments[G[x1, x2] G[x3, x4] V[x1, x3] V[x2, x4], {x1, x2, x3, x4},
     "ExternalOrder" -> {"Uncrossed", {x4, x2, x3, x1}}]], 0];
(* the labels at the corners top left, bottom left, bottom right, top right of every picture of a sum: their text in the
   four quadrants around the mean of the four *)
cornerLabels[e_, ext_, opts___] := Table[With[{ps = Table[First[Cases[g[[2]], {"T", p_, l, ___} :> p], {0., 0.}], {l, ext}]},
     With[{q = Table[With[{dd = p - Mean[ps]}, Which[dd[[1]] < 0 && dd[[2]] > 0, 1, dd[[1]] < 0 && dd[[2]] < 0, 2, dd[[1]] > 0 && dd[[2]] < 0, 3, True, 4]],
         {p, ps}]}, ext[[Ordering[q]]]]], {g, FunctionalDiagrams`Private`geometriesOf[e, ext, {opts}]}];
check["four points: the labels at the corners in the order given, for bosons (Gamma4 of phi^4, its channel t crossed) and fermions (F)",
  {DeleteDuplicates[cornerLabels[vertexPhi4, {a, b, c, d}]], DeleteDuplicates[cornerLabels[fullVertex, {x1, x2, x3, x4}]],
   DeleteDuplicates[cornerLabels[G[x1, x2] G[x3, x4] V[x1, x3] V[x2, x4], {x1, x2, x3, x4}, "ExternalOrder" -> {x4, x2, x3, x1}]]},
  {{{a, b, c, d}}, {{x1, x2, x3, x4}}, {{x4, x2, x3, x1}}}];
check["four points with \"Uncrossed\": the bubbles of phi^4 drawn planar, the channel t with c and d exchanged",
  Sort[cornerLabels[OrderPart[vertexPhi4, lambda, 2], {a, b, c, d}, "ExternalOrder" -> "Uncrossed"]], {{a, b, c, d}, {a, b, c, d}, {a, b, d, c}}];
check["four points: the trees of W^(4) with their leaves in the order given, the crossed one included",
  DeleteDuplicates[cornerLabels[ConnectedFunctionFromVertices[{a, b, c, d}, "Boson"], {a, b, c, d}]], {{a, b, c, d}}];
(* x2 -> z1 -> z2 -> x1 on the left, x4 -> z3 -> z4 -> x3 on the right: the rungs z1-z4 and z2-z3 are horizontal *)
ladderNodes = First[FunctionalDiagrams`Private`layoutRailUnits[{{x2, z1, z2, x1}, {x4, z3, z4, x3}}, {}, {{z1, z4}, {z2, z3}},
    {x1, x2, x3, x4}, {x1, x2, x3, x4}, {True, True, True, True}, {x1, x2, x3, x4}]];
check["a ladder between two rails: its rungs are perpendicular to the rails",
  Round[100 {(z1 /. Rule @@@ ladderNodes) - (z4 /. Rule @@@ ladderNodes), (z2 /. Rule @@@ ladderNodes) - (z3 /. Rule @@@ ladderNodes)}][[All, 2]], {0, 0}];
axisNodes = First[FunctionalDiagrams`Private`layoutAxisUnits[{x, z1, z2, z3, y}, {{z4, z5}, {z6}}, {{z1, z4}, {z3, z5}, {z2, z6}},
    {x, y}, {x, y}, {False, False}]];
check["a tadpole under a bubble that sits on the axis goes to the other side",
  Sign[(z6 /. Rule @@@ axisNodes)[[2]]] == -Sign[(z4 /. Rule @@@ axisNodes)[[2]]], True];
tableLoop = First[FunctionalDiagrams`Private`placeLoopOnLine[{u1, u2, u3}, {{u1, {-0.5, 0.}}, {u2, {0., 0.}}, {u3, {0.5, 0.}}}, {0., 1.}, {}]];
check["a loop joined to three close points of a line: the lines are perpendicular to it",
  Round[100 Table[(v /. Rule @@@ tableLoop)[[1]], {v, {u1, u2, u3}}]], {-50, 0, 50}];
tableLoop = First[FunctionalDiagrams`Private`placeLoopOnLine[{u1, u2, u3}, {{u1, {-1., 0.}}, {u2, {0., 0.}}, {u3, {1., 0.}}}, {0., 1.}, {}]];
check["a loop joined to three points of a line further apart than it is wide: the outer lines lean inwards, symmetric",
  With[{xs = Table[(v /. Rule @@@ tableLoop)[[1]], {v, {u1, u2, u3}}]}, {Round[100 xs[[2]]], Round[100 (xs[[1]] + xs[[3]])], 0.3 < xs[[3]] < 0.9}],
  {0, 0, True}];
treeBlobs = Cases[geometryItems[IntegralOver[treeLine[a, z1] treeLine[b, z2] treeVertex[z1, z2, z3] treeLine[z3, z4] *
      treeVertex[z4, z5, z6] treeLine[c, z5] treeLine[d, z6], {z1, z2, z3, z4, z5, z6}], {a, b, c, d}], {"N", p_, "blob"} :> p];
check["a tree of two vertex functions: the vertices side by side", {Round[100 treeBlobs[[All, 2]]], Round[100 Abs[treeBlobs[[1, 1]] - treeBlobs[[2, 1]]]]},
  {{0, 0}, 100}];
(* the same tree with its labels integrated over: the leaves are free ends, where the external points were *)
treeFreeNodes = Cases[geometryItems[IntegralOver[treeLine[a, z1] treeLine[b, z2] treeVertex[z1, z2, z3] treeLine[z3, z4] *
      treeVertex[z4, z5, z6] treeLine[c, z5] treeLine[d, z6], {a, b, c, d, z1, z2, z3, z4, z5, z6}], {}], {"N", p_, k_} :> {k, Round[100 p]}];
check["a tree whose leaves are free ends: drawn by the tree layout, mirror symmetric",
  {Sort[Cases[treeFreeNodes, {"blob", p_} :> p]], Sort[treeFreeNodes] === Sort[{#[[1]], {-#[[2, 1]], #[[2, 2]]}} & /@ treeFreeNodes],
   Length[treeFreeNodes]}, {{{-50, 0}, {50, 0}}, True, 6}];
(* a bare vertex with free ends is an inner node of the tree layout too: symmetric about the vertical axis *)
treeStar = Cases[geometryItems[IntegralOver[D0[a, z] D0[b, z] D0[c, z] D0[d, z] D0[e, z], {a, b, c, d, e, z}], {}], {"N", p_, _} :> Round[100 p]];
check["a vertex with five free ends: a star, one end at the top, mirror symmetric",
  {Length[treeStar], MemberQ[treeStar, {0, 72}], Sort[treeStar] === Sort[{-#[[1]], #[[2]]} & /@ treeStar]}, {6, True, True}];
threeDiameters = geometryItems[IntegralOver[G[z1, z2] G[z2, z3] G[z3, z4] G[z4, z5] G[z5, z6] G[z6, z1] V[z1, z4] V[z2, z5] V[z3, z6],
    {z1, z2, z3, z4, z5, z6}], {}];
check["three diameters of a fermion loop of six: one outside, the other two straight, crossing at 70 degrees",
  With[{segs = Cases[threeDiameters, {"E", {"segment", p_, q_}, "Wavy"} :> {p, q}]},
   {Length[segs], Count[threeDiameters, {"E", {"arc", __}, "Wavy"}],
    If[Length[segs] == 2, Round[Abs[Dot @@ Table[FunctionalDiagrams`Private`unitVectorOf[s[[2]] - s[[1]]], {s, segs}]], 0.01], None]}], {2, 1, 0.34}];
check["three diameters in TikZ: explicit waves (no snake decoration), flattened where the two inner ones cross",
  With[{tikz = DiagramToTikZ[IntegralOver[G[z1, z2] G[z2, z3] G[z3, z4] G[z4, z5] G[z5, z6] G[z6, z1] V[z1, z4] V[z2, z5] V[z3, z6],
       {z1, z2, z3, z4, z5, z6}], {}]},
   {StringFreeQ[tikz, "snake"], Length[Select[FunctionalDiagrams`Private`waveCrossingParameters[Cases[threeDiameters, {"E", __}]], # =!= {} &]]}],
  {True, 2}];
bubblePairDots = Cases[geometryItems[IntegralOver[G[z1, z2] G[z2, z1] G[z3, z4] G[z4, z3] V[z1, z3] V[z2, z4], {z1, z2, z3, z4}], {}],
   {"N", p_, "dot"} :> p];
check["two bubbles joined at both vertices: the vertices on the facing sides, 50 degrees from the line of the centres",
  Round[100 Sort[Flatten[Table[FunctionalDiagrams`Private`vectorNorm[bubblePairDots[[i]] - bubblePairDots[[j]]], {i, 4}, {j, i + 1, 4}]]]],
  {77, 77, 146, 146, 165, 165}];
check["a bubble between two points far apart on the axis: its interaction lines are bent",
  Count[geometryItems[IntegralOver[G[x, z2] G[z1, y] G[z2, z4] G[z3, z1] G[z4, z3] V[z3, z4] G[z5, z6] G[z6, z5] V[z1, z5] V[z2, z6],
     {z1, z2, z3, z4, z5, z6}], {x, y}], {"E", {"arc", __}, "Wavy"}], 3];
DeclareLocalVertex[Vqg]; DeclareLineStyle[Gluon, "Curly"]; DeclareLineStyle[Ghost, "DottedArrow"];
quarkLoop = geometryItems[IntegralOver[Vqg[z1, z2, x] Vqg[z3, z4, y] Gq[z2, z3] Gq[z4, z1], {z1, z2, z3, z4}], {x, y}, "Directed" -> {Gq}];
check["a local vertex is a point, not a blob", {Count[quarkLoop, {"N", _, "blob"}], Count[quarkLoop, {"N", _, "dot"}]}, {0, 2}];
check["weights: a local vertex stands for minus its kernel",
  {weightsOf[g^2 IntegralOver[Vqg[z1, z2, x] Vqg[z3, z4, y] Gq[z2, z3] Gq[z4, z1], {z1, z2, z3, z4}], {x, y}, "Directed" -> {Gq}],
   weightsOf[g IntegralOver[Vqg[u, v, w] Gq[x, u] Gq[v, y], {u, v}], {x, y, w}, "Directed" -> {Gq}]}, {{"g^{2}"}, {"-g"}}];
check["gluons and ghosts in TikZ", {StringFreeQ[DiagramToTikZ[Gluon[a, b], {a, b}], "plot[smooth"], StringFreeQ[DiagramToTikZ[Ghost[a, b], {a, b}],
    "dotted"]}, {False, False}];
check["three open lines joined in a chain: three horizontal rails, the interaction lines vertical",
  With[{items = geometryItems[IntegralOver[G[x1, u1] G[u1, x2] V[u1, u2] G[x3, u2] G[u2, u3] G[u3, x4] V[u3, u4] G[x5, u4] G[u4, x6],
       {u1, u2, u3, u4}], {x1, x2, x3, x4, x5, x6}]},
   {Length[DeleteDuplicates[Cases[items, {"N", p_, _} :> Round[p[[2]], 0.01]]]],
    Cases[items, {"E", {"segment", p_, q_}, "Wavy"} :> Abs[p[[1]] - q[[1]]] < 0.01]}], {3, {True, True}}];
check["a density point: a crossed circle at the point, without a leg",
  With[{items = geometryItems[G[a, b] G[b, a], {a, b}]}, {Count[items, {"S", {p_, p_}}], Count[items, {"L", __}]}], {2, 0}];
DeclareLocalVertex[Vt3, Vt4];
check["a bubble of a three-point vertex: the two legs of one vertex on the left, the third on the right, in every order",
  Table[Sort[Sign[Cases[geometryItems[IntegralOver[Vt4[l[[1]], l[[2]], z1, z2] Gt[z1, z3] Gt[z2, z4] Vt3[z3, z4, l[[3]]], {z1, z2, z3, z4}],
       {x1, x2, x3}], {"L", _, e_, _} :> Round[e[[1]], 0.01]]]], {l, {{x1, x2, x3}, {x2, x3, x1}, {x3, x1, x2}}}],
  Table[{-1, -1, 1}, {3}]];
check["TikZ: arrow heads drawn without decorations", StringFreeQ[DiagramToTikZ[-G[a, b] V[a, b], {a, b}], "markings"], True];
check["formulas in LaTeX: elementary functions", {ExpressionToLaTeX[Cos[x]], ExpressionToLaTeX[Sqrt[y]]}, {"\\cos\\left(x\\right)", "\\sqrt{y}"}];
check["formulas in LaTeX", {ExpressionToLaTeX[FunctionalDerivative[IntegralOver[f[x] J[x]^2, {x}], J[a]]],
   ExpressionToLaTeX[-1/2 IntegralOver[G0[x, z] h[z, y], {z}], "Names" -> {G0 -> "G_0"}], ExpressionToLaTeX[etabar[a]], ExpressionToLaTeX[lambda2]},
  {"2\\,f(a)\\,J(a)", "-\\frac{1}{2}\\,\\int_{z}G_0(x,z)\\,h(z,y)", "\\bar{\\eta}(a)", "\\lambda_{2}"}];
check["formulas in LaTeX: complex and inexact coefficients, Pi and E", {ExpressionToLaTeX[-I x], ExpressionToLaTeX[2.5 x],
   ExpressionToLaTeX[-I y/2], ExpressionToLaTeX[Pi x], ExpressionToLaTeX[E y], DiagramToTikZ[I G[a, b], {a, b}, "Output" -> "List"][[1, 1]]},
  {"-i\\,x", "2.5\\,x", "-\\frac{1}{2}\\,i\\,y", "\\pi\\,x", "e\\,y", "i"}];
check["TikZ: a label made by Module is written without its $", Module[{u}, occurrences[DiagramToTikZ[G[a, u], {a, u}], "{$u$}"]], 1];
DeclareBlob[SgBlob, "\\Sigma"]; DeclareBlob[FBx, "F", "Shape" -> "Box", "Fill" -> "CrossHatched"];
check["DeclareBlob: a two-label blob is drawn as a blob with its label, not as a line",
  With[{t = DiagramToTikZ[IntegralOver[G[a, u] SgBlob[u, v] G[v, b], {u, v}], {a, b}]}, {occurrences[t, "{$\\Sigma$}"],
    occurrences[t, "\\fill[white]"]}], {1, 1}];
check["DeclareBlob: hatching drawn as chords, without the patterns library",
  With[{t = DiagramToTikZ[FBx[a, b, c, d], {a, b, c, d}]}, {StringFreeQ[t, "pattern"], occurrences[t, "rectangle"] >= 2}], {True, True}];
check["DeclareBlob: a four-point box has its legs at its corners",
  Sort[Cases[geometryItems[FBx[a, b, c, d], {a, b, c, d}], {"L", q_, __} :> Round[q/FunctionalDiagrams`Private`$declaredBlobSize]]],
  {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}}];
check["DeclareBlob: a two-label blob is a vertex for the cuts",
  ParticleIrreducibleDiagrams[IntegralOver[G[a, u] SgBlob[u, v] G[v, w] SgBlob[w, z] G[z, b], {u, v, w, z}], 1, {a, b}, {G}], 0];
DeclareBlob[TriTilde, "\\tilde\\Lambda", "Shape" -> "Triangle", "Fill" -> "CrossHatched"];
DeclareBlob[WideBox, "\\Gamma_{\\rm ph}", "Shape" -> "Box", "Fill" -> "CrossHatched"];
check["DeclareBlob: a blob whose label needs room is larger (its label never smaller, but in a triangle); a short label keeps the size",
  FunctionalDiagrams`Private`blobSpecOf /@ {TriTilde, WideBox, FBx, SgBlob} // #[[All, 4]] & // {#[[1]] > 0.6, #[[2]] > 0.6, #[[3]], #[[4]]} &,
  {True, True, 0.6, 0.6}];
check["DeclareBlob: the label of a triangle, with its clearing, lies inside it with a band of hatching around",
  With[{sp = FunctionalDiagrams`Private`blobSpecOf[TriTilde]}, With[{box = FunctionalDiagrams`Private`blobLabelBox[sp[[1]], sp[[3]], sp[[2]]]},
    FunctionalDiagrams`Private`blobBoxMargin[sp[[2]], sp[[4]], box, FunctionalDiagrams`Private`blobLabelPoint[{0., 0.}, sp]] >= 0.099]], True];
check["DeclareBlob: the label of a triangle written 0.8 times smaller, in TikZ and in the size of the triangle",
  {occurrences[DiagramToTikZ[TriTilde[a, b, c], {a, b, c}], "scale=0.8"],
   FunctionalDiagrams`Private`declaredBlobSize["\\tilde\\Lambda", "Triangle", "CrossHatched"] < 0.65}, {1, True}];
check["DeclareBlob: in the pictures of Mathematica a tilde is written above its letter, not as a combining character",
  {FunctionalDiagrams`Private`plainBlobLabel["\\tilde\\Lambda"], FunctionalDiagrams`Private`plainBlobLabel["\\Gamma^0"]},
  {Row[{Overscript["\[CapitalLambda]", "~"]}], "\[CapitalGamma]0"}];
(* Gamma0 L0 Lambda: the lines leave from the two corners of the triangle, and the box is as wide as the side between them *)
boxTriangle = Last[FunctionalDiagrams`Private`geometriesOf[DeltaFunction[a, z] DeltaFunction[b, z] +
     IntegralOver[FBx[a, b, u1, u2] G[u2, u3] G[u4, u1] TriTilde[u3, u4, z], {u1, u2, u3, u4}], {a, b, z}, {}]][[2]];
check["DeclareBlob: a box joined by two lines to a triangle is as wide as its side, the lines from its corners parallel",
  With[{bl = Cases[boxTriangle, {"B", p_, sp_} :> {p, sp}], es = Cases[boxTriangle, {"E", {"segment", p_, q_}, "Arrow"} :> {p, q}]},
   With[{tri = First[Select[bl, #[[2, 2]] === "Triangle" &]], box = First[Select[bl, #[[2, 2]] === "Box" &]]},
    {Abs[box[[2, 4]] - Sqrt[3.] 1.45 tri[[2, 4]]/2] < 0.01, Length[es],
     AllTrue[es, Abs[#[[1, 2]] - #[[2, 2]]] < 0.01 &],
     AllTrue[Flatten[es, 1], Function[q, AnyTrue[Join[FunctionalDiagrams`Private`triangleCorners[tri[[1]], tri[[2, 4]]],
         Flatten[Table[box[[1]] + box[[2, 4]] {sx, sy}, {sx, {-1, 1}}, {sy, {-1, 1}}], 1]], Norm[# - q] < 0.01 &]]]}]],
  {True, 2, True, True}];
(* the lines of a box leave from its corners: the gap function closed on the pp vertex, Delta = -1/2 Gamma_pp G G Delta *)
DeclareBlob[GapBl, "\\Delta", "AxialPorts" -> {1, 2}]; DeclareBlob[LBx, "L", "Shape" -> "Box", "Fill" -> "White"];
boxCorners[{p_, sp_}] := Flatten[Table[p + sp[[4]] {sx, sy}, {sx, {-1, 1}}, {sy, {-1, 1}}], 1];
circleBox = geometryItems[IntegralOver[FBx[u1, u2, c, a] G[u1, u3] G[u2, u4] GapBl[u3, u4], {u1, u2, u3, u4}], {a, c},
   "LegDirections" -> {"Out", "Out"}];
check["DeclareBlob: a circle joined by two lines to a box: the lines end at the corners of the box, and leave the circle 55 degrees off the line of the centres",
  With[{box = First[Cases[circleBox, {"B", p_, sp_} /; sp[[2]] === "Box" :> {p, sp}]],
    circ = First[Cases[circleBox, {"B", p_, sp_} /; sp[[2]] === "Circle" :> {p, sp}]],
    es = Cases[circleBox, {"E", {"segment", p_, q_}, "Arrow"} :> {p, q}]},
   {Length[es], AllTrue[es, Function[e, AnyTrue[boxCorners[box], Min[Norm[# - e[[1]]], Norm[# - e[[2]]]] < 0.01 &]]],
    Sort[Round[Table[With[{q = First[SortBy[e, Norm[# - circ[[1]]] &]]}, ArcTan[Abs[(q - circ[[1]])[[1]]], (q - circ[[1]])[[2]]]/Degree], {e, es}]]]}],
  {2, True, {-55, 55}}];
(* a point twice a port of a box: the susceptibility chi = gamma L gamma, and the end of an interaction line on L *)
chiItems = geometryItems[LBx[z1, z1, z2, z2], {z1, z2}];
check["DeclareBlob: a point twice a port of a box (chi = L(z1, z1; z2, z2)) is joined to the two corners of the facing side, a density point",
  With[{box = First[Cases[chiItems, {"B", p_, sp_} :> {p, sp}]], es = Cases[chiItems, {"E", {"segment", p_, q_}, _} :> {p, q}]},
   {Length[es], Count[es, e_ /; AnyTrue[boxCorners[box], Norm[# - e[[1]]] < 0.01 &]],
    Sort[Round[10 Cases[chiItems, {"S", {p_, p_}} :> p[[1]]]]]}], {4, 4, {-16, 16}}];
chainItems = geometryItems[IntegralOver[V[a, w] LBx[w, w, c, d] DeltaFunction[a, b], {w}], {a, b, c, d}];
check["DeclareBlob: an interaction line ending on two ports of a box: a dot at each end, the two legs of its vertex fanned, a above b",
  {Count[chainItems, {"N", _, "dot"}], Round[Cases[chainItems, {"L", q_, e_, _} :> ArcTan @@ (e - q)/Degree][[{1, 2}]]]}, {2, {135, -135}}];
(* a box joined by three corners to one vertex (Sigma = lambda/6 G G G Gamma4 in phi^4 theory): turned by 45 degrees *)
turnedBox[items_] := First[Cases[items, {"B", p_, sp_} :> {p, sp}]];
turnedItems = geometryItems[IntegralOver[D0[a, u] D0[a, v] D0[a, w] FBx[v, u, b, w], {u, v, w}], {a, b}];
check["DeclareBlob: a box joined by three corners to one vertex is turned by 45 degrees (a diamond as large as the square), b at its right corner, the vertex on the diagonal through it, the lines to the top and bottom corners arcs mirror to each other",
  With[{bl = turnedBox[turnedItems], dot = First[Cases[turnedItems, {"N", p_, "dot"} :> p]],
    arcs = Cases[turnedItems, {"E", {"arc", c_, r_, __}, _} :> {c, r}]},
   With[{corners = FunctionalDiagrams`Private`triangleCorners[bl[[1]], bl[[2, 4]], "Diamond"]},
    {bl[[2, 2]], Abs[1.3 bl[[2, 4]] - Sqrt[2.] FunctionalDiagrams`Private`blobSizeOfHead[FBx]] < 0.01,
     Abs[dot[[2]] - bl[[1, 2]]] < 10.^-6 && dot[[1]] < corners[[1, 1]] - 0.5,
     Sort[Round[Cases[turnedItems, {"L", q_, e_, _} :> ArcTan @@ (e - q)/Degree]]],
     Length[arcs], AllTrue[arcs, Abs[#[[1, 2]] - bl[[1, 2]]] < 10.^-6 &], Abs[arcs[[1, 2]] - arcs[[2, 2]]] < 10.^-6,
     Count[Cases[turnedItems, {"E", path_, st_} /; st =!= "Spoke" :> FunctionalDiagrams`Private`pathPoints[path, {0., 1.}, 0.]],
      ends_ /; AnyTrue[corners[[{2, 4}]], Min[Norm[# - ends[[1]]], Norm[# - ends[[2]]]] < 0.01 &]]}]],
  {"Diamond", True, True, {0, 180}, 2, True, True, 2}];
check["DeclareBlob: a turned box: its fourth corner on the left when its label comes first; the label of a turned box makes it larger, upright",
  {With[{items = geometryItems[IntegralOver[D0[a, u] D0[a, v] D0[a, w] FBx[v, u, b, w], {u, v, w}], {b, a}]},
     With[{bl = turnedBox[items]}, First[Cases[items, {"N", p_, "dot"} :> p]][[1]] > bl[[1, 1]]]],
   With[{bl = turnedBox[geometryItems[IntegralOver[D0[a, u] D0[a, v] D0[a, w] WideBox[v, u, b, w], {u, v, w}], {a, b}]]},
    {bl[[2, 4]] > Sqrt[2.] FunctionalDiagrams`Private`blobSizeOfHead[WideBox]/1.3 + 0.05,
     FunctionalDiagrams`Private`blobBoxMargin["Diamond", bl[[2, 4]], FunctionalDiagrams`Private`blobLabelBox[bl[[2, 1]], bl[[2, 3]], "Box"], {0., 0.}] >= 0.099}]},
  {True, {True, True}}];
check["DeclareBlob: a box joined by two corners to one point, or by three corners to three points, keeps its sides upright",
  {turnedBox[geometryItems[IntegralOver[D0[a, u] D0[a, v] FBx[u, v, c, d], {u, v}], {a, c, d}]][[2, 2]],
   turnedBox[geometryItems[IntegralOver[D0[a, u] D0[c, v] D0[d, w] FBx[u, v, b, w], {u, v, w}], {a, b, c, d}]][[2, 2]]}, {"Box", "Box"}];
check["DeclareBlob: in one picture the blobs of one shape have one size (F beside a wider label)",
  Union[Cases[geometryItems[IntegralOver[WideBox[a, b, u, v] FBx[v, u, c, d], {u, v}], {a, b, c, d}], {"B", _, sp_} :> sp[[4]]]] // Length, 1];
check["DeclareBlob: in a sum drawn together a blob has one size in every term (F alone as large as beside the wider label)",
  Union[Cases[FunctionalDiagrams`Private`geometriesOf[FBx[a, b, c, d] + IntegralOver[WideBox[a, b, u, v] FBx[v, u, c, d], {u, v}],
      {a, b, c, d}, {}][[All, 2]], {"B", _, sp_} :> sp[[4]], Infinity]] // {Length[#], First[#] > 0.6} &, {1, True}];
check["DoubleWavy: one wave drawn double, its strokes apart in proportion to the line width",
  occurrences[DiagramToTikZ[Wscr[a, b], {a, b}], "double distance=1.2\\pgflinewidth"], 1];
check["DeclareBlob: an unknown shape: a message",quietCheck[DeclareBlob[SgBlob, "\\Sigma", "Shape" -> "Square"], "message"], "message"];
(* the Graphics output, what a notebook shows: every line style, the blobs, a sum as a grid *)
lineStyles = {{lsPlain, "Plain"}, {lsArrow, "Arrow"}, {lsDoubleArrow, "DoubleArrow"}, {lsDottedArrow, "DottedArrow"}, {lsDouble, "Double"},
   {lsWavy, "Wavy"}, {lsDoubleWavy, "DoubleWavy"}, {lsCurly, "Curly"}, {lsDashed, "Dashed"}, {lsDotted, "Dotted"}, {lsZigzag, "Zigzag"},
   {lsImpurity, "Impurity"}, {lsDiverging, "DivergingArrows"}, {lsConverging, "ConvergingArrows"}};
Scan[DeclareLineStyle[#[[1]], #[[2]]] &, lineStyles];
check["DrawDiagram with each of the 14 line styles (a line, a line to a tadpole): Graphics, no message",
  Table[Check[Head[DrawDiagram[h[a, b] h[b, c] h[c, c], {a}]], "message"], {h, lineStyles[[All, 1]]}], ConstantArray[Graphics, 14]];
(* the amplitude of the waves of the Graphics (0.055 units), not the relative factor of the TikZ waves *)
check["a wavy line of the Graphics: its waves at most 0.06 units off the straight line between its ends",
  With[{pts = First[Cases[DrawDiagram[lsWavy[a, b], {a, b}], Line[p_ /; Length[p] > 20] :> p, Infinity]]},
   With[{u = Normalize[Last[pts] - First[pts]]}, Max[Abs[Cross[u] . (# - First[pts])] & /@ pts] < 0.06]], True];
DeclareBlob[TrBlob, "\\Lambda", "Shape" -> "Triangle", "Fill" -> "Hatched"];
check["DiagramPlot of blobs (a circle between two lines, a cross-hatched box, a hatched triangle): Graphics, no message",
  Table[Check[Head[DiagramPlot[bl[[1]], bl[[2]]]], "message"], {bl, {{IntegralOver[G[a, u] SgBlob[u, v] G[v, b], {u, v}], {a, b}},
     {FBx[a, b, c, d], {a, b, c, d}}, {TrBlob[a, b, c], {a, b, c}}}}], {Graphics, Graphics, Graphics}];
check["DiagramPlot with \"Output\" -> \"Grid\": a Grid, no message",
  Check[Head[DiagramPlot[G[a, b] - IntegralOver[G[a, u] V[u, v] G[u, v] G[v, b], {u, v}], {a, b}, "Output" -> "Grid"]], "message"], Grid];
(* the colours of the Graphics follow the notebook: LightDarkSwitched (Mathematica 14.3 or later) with "Appearance" ->
   Automatic, else the light colours; "Light" and "Dark" force them, on a background; the TikZ code has no colours *)
DeclareBlob[GreyFilled, "P", "Fill" -> "Grey"];
colourPicture[opts___] := DrawDiagram[IntegralOver[G[a, u] SgBlob[u, v] G[v, b] FBx[u, v, c, c], {u, v, c}], {a, b}, opts];
colourDirectives[g_] := Union[Cases[g, (GrayLevel | RGBColor)[__], Infinity]];
switchedColours[g_] := Union[Cases[g, h_Symbol[l : GrayLevel[_], d_] /; SymbolName[h] === "LightDarkSwitched" :> {l, d}, Infinity]];
check["\"Appearance\" -> Automatic: with LightDarkSwitched every colour is a light one and a dark one (lines and text; the paper, dark as the notebook), no Background; before 14.3 the light colours",
  With[{g = colourPicture[]}, {FreeQ[Rest[List @@ g], Background], If[Names["System`LightDarkSwitched"] =!= {},
     {switchedColours[g], FreeQ[g /. h_Symbol[__] /; SymbolName[h] === "LightDarkSwitched" -> "switched", GrayLevel]},
     {{{GrayLevel[0], GrayLevel[0.9]}, {GrayLevel[1], Automatic}}, True}]}], {True, {{{GrayLevel[0], GrayLevel[0.9]}, {GrayLevel[1], Automatic}}, True}}];
check["\"Appearance\" -> Automatic before Mathematica 14.3 (no LightDarkSwitched): the colours of a light notebook, as before",
  Block[{Names = ({} &)}, colourDirectives[colourPicture[]]], {GrayLevel[0], GrayLevel[1]}];
check["\"Appearance\" -> \"Dark\": light lines on a dark background, the clearings of the background colour; \"Light\": the light colours on white",
  {colourDirectives[colourPicture["Appearance" -> "Dark"]], Cases[List @@ colourPicture["Appearance" -> "Dark"], (Background -> c_) :> c],
   colourDirectives[colourPicture["Appearance" -> "Light"]], Cases[List @@ colourPicture["Appearance" -> "Light"], (Background -> c_) :> c],
   FreeQ[colourPicture["Appearance" -> "Dark"], h_Symbol /; SymbolName[h] === "LightDarkSwitched"]},
  {{GrayLevel[0.1], GrayLevel[0.9]}, {GrayLevel[0.1]}, {GrayLevel[0], GrayLevel[1]}, {GrayLevel[1]}, True}];
check["\"Appearance\": the grey vertex blobs, the grey fill, the frames and the weights of DiagramPlot in the dark colours; a Grid with a background",
  {colourDirectives[DiagramPlot[GreyBlob3[a, b, c] + IntegralOver[G[a, u] GreyFilled[u, v] G[v, b], {u, v}], {a, b, c}, "Appearance" -> "Dark"]],
   Cases[List @@ DiagramPlot[G[a, b] - G[a, b] V[a, b], {a, b}, "Output" -> "Grid", "Appearance" -> "Dark"], (Background -> c_) :> c]},
  {{GrayLevel[0.1], GrayLevel[0.3], GrayLevel[0.42], GrayLevel[0.48], GrayLevel[0.9]}, {GrayLevel[0.1]}}];
check["\"Appearance\" -> \"Bright\": message appearance, Automatic used",
  {messageGivenQ[colourPicture["Appearance" -> "Bright"], FunctionalDiagrams::appearance],
   Quiet[colourPicture["Appearance" -> "Bright"]] === colourPicture[]}, {True, True}];
check["the TikZ code ignores \"Appearance\": it stays for white pages (the clearings white)",
  With[{t = DiagramToTikZ[IntegralOver[G[a, u] SgBlob[u, v] G[v, b] FBx[u, v, c, c], {u, v, c}], {a, b}]},
   {t === DiagramToTikZ[IntegralOver[G[a, u] SgBlob[u, v] G[v, b] FBx[u, v, c, c], {u, v, c}], {a, b}, "Appearance" -> "Dark"],
    StringContainsQ[t, "fill=white"], StringContainsQ[t, "LightDark" | "gray" | "black"]}], {True, True, False}];
(* the smallest distance between two points of a list *)
minimumDistance[ps_List] := Min[Flatten[Table[Sqrt[(ps[[i]] - ps[[j]]) . (ps[[i]] - ps[[j]])], {i, Length[ps]}, {j, i + 1, Length[ps]}]]];
tadpoleChain[n_Integer] := IntegralOver[Product[G[t[i], t[i]], {i, n}] Product[V[t[i], t[i + 1]], {i, n - 1}], Table[t[i], {i, n}]];
check["vacuum trees of tadpoles (chains and a star): no two vertices at one point",
  Table[minimumDistance[Cases[geometryItems[e, {}], {"N", p_, _} :> p]] > 0.5, {e, {tadpoleChain[3], tadpoleChain[4], tadpoleChain[6],
     IntegralOver[G[t0, t0] G[t1, t1] G[t2, t2] G[t3, t3] V[t0, t1] V[t0, t2] V[t0, t3], {t0, t1, t2, t3}]}}], {True, True, True, True}];
coil = FunctionalDiagrams`Private`coilPoints[{"segment", {0., 0.}, {1., 0.}}, 5, 0.06, 0.05];
check["a gluon coil starts and ends on its line and is mirror symmetric",
  {Round[1000 {First[coil], Last[coil]}], Max[Abs[Flatten[coil - Reverse[{1 - #[[1]], #[[2]]} & /@ coil]]]] < 10.^-9}, {{{0, 0}, {1000, 0}}, True}];
gluonTadpole = geometryItems[IntegralOver[Gq[a, z] Gluon[z, z] Gq[z, b], {z}], {a, b}, "Directed" -> {Gq}];
check["a gluon self-loop: a balloon whose coils stay on one side of the line it hangs from",
  With[{ys = Flatten[Cases[gluonTadpole, {"E", path : {"balloon", __}, "Curly"} :> FunctionalDiagrams`Private`pathCurly[path][[All, 2]]]]},
   {ys =!= {}, Min[ys] > -10.^-6 || Max[ys] < 10.^-6, Max[Abs[ys]] > 0.8}], {True, True, True}];
DeclareLocalVertex[V3g];
(* the one-loop insertions into the gluon line of the quark self-energy: a gluon loop and a quark loop *)
insertionItems[loop_] := geometryItems[IntegralOver[Vqg[x, z1, w1] Gq[z1, z2] Vqg[z2, y, w2] Gluon[w1, u1] Gluon[u2, w2] loop,
    {z1, z2, w1, w2, u1, u2, v1, v2, v3, v4}], {x, y}, "Directed" -> {Gq}];
insertionShape[items_] := With[{dots = Cases[items, {"N", p_, "dot"} :> p]}, With[{low = Select[dots, Abs[#[[2]]] < 10.^-6 &],
     high = Select[dots, Abs[#[[2]]] > 0.3 &]}, {Length[low], Length[high], Round[100 Abs[high[[1, 1]] - high[[2, 1]]]] < Round[100 Abs[low[[1, 1]] - low[[2, 1]]]],
     Count[items, {"E", {"arc", __}, "Curly"}] >= 2}]];
check["a gluon loop and a quark loop in a gluon line: as wide as each other, narrower than the points they hang from, their lines bent",
  {insertionShape[insertionItems[V3g[u1, v1, v2] Gluon[v1, v3] Gluon[v2, v4] V3g[u2, v3, v4]]],
   insertionShape[insertionItems[Vqg[v1, v2, u1] Gq[v2, v3] Vqg[v3, v4, u2] Gq[v4, v1]]]}, {{2, 2, True, True}, {2, 2, True, True}}];

(* ------------------------------------------------------------------------- *)
section["input checks"];
check["OutputForm: no recursive formatting", StringFreeQ[ToString[2 DeltaFunction[a, b] IntegralOver[f[x] g[x, b], {x}], OutputForm],
   "Interpretation"], True];
DeclareField[chi, zetabar]; DeclareField[chibar, zeta, -1]; DeclareGrassmannSource[zeta, zetabar];
checkZero["fields declared before their Grassmann source are odd",
  GaussianExpectationValue[Exp[-IntegralOver[Gc[x, y] zetabar[x] ** zeta[y], {x, y}]], {chi[a], chi[b], chibar[c], chibar[d]}] -
   (Gc[a, d] Gc[b, c] - Gc[a, c] Gc[b, d])];
check["derivative with respect to an undeclared source: a message", quietCheck[FunctionalDerivative[IntegralOver[f[x] K2[x]^2, {x}], K2[a]], "message"],
  "message"];
check["generating functional without sources: a message", quietCheck[GaussianExpectationValue[Exp[IntegralOver[J2[x] D2[x, y] J2[y], {x, y}]],
    {phi2[a], phi2[b]}], "message"], "message"];
checkZero["a composite operator written with **", ConnectedCorrelationFunction[Zf, {psibar[a] ** psi[a], psibar[b] ** psi[b]}, CoulombVertex, 1] -
   ConnectedCorrelationFunction[Zf, {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1]];
check["Grassmann fields multiplied with Times: a message, no result", quietCheck[ConnectedCorrelationFunction[Zf, {psibar[a] psi[a], psibar[b] psi[b]},
    CoulombVertex, 1], "message"], "message"];
check["a single integration variable without braces", SimplifyDiagrams[IntegralOver[f[x] g[x, y], x] - IntegralOver[f[u] g[u, y], {u}]], 0];
checkZero["OrderPart with a list of orders", OrderPart[greenFunction, V, {0, 2}] - OrderPart[greenFunction, V, 0] - OrderPart[greenFunction, V, 2]];
DeclareSymmetric[Dd];
checkZero["PerturbativeFullVertex: only the orders asked for", PerturbativeFullVertex[Z0, Phi4Vertex, {2}, {phi}, {a, b, c, d}, D0 -> Dd] -
   OrderPart[PerturbativeFullVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, c, d}, D0 -> Dd], lambda, 2]];
check["phi^4 vertex at two loops: no vertex with two legs at the centre (opposite corners are exchanged)",
  AllTrue[DiagramTerms[PerturbativeFullVertex[Z0, Phi4Vertex, {3}, {phi}, {a, b, c, d}, D0 -> Dd]],
   Min[Norm /@ Cases[geometryItems[#, {a, b, c, d}], {"L", q_, _, _} :> q]] > 0.2 &], True];
check["nested integrals: the canonical form does not depend on the names of the outer variables",
  SimplifyDiagrams[IntegralOver[aa[x] bb[y] Exp[IntegralOver[gg[x, u] gg[y, v] kk[u, v], {u, v}]], {x, y}] -
    IntegralOver[aa[y] bb[x] Exp[IntegralOver[gg[y, u] gg[x, v] kk[u, v], {u, v}]], {x, y}]], 0];
check["an order that is not an integer: a message", quietCheck[PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 1.], "message"],
  "message"];
check["Grassmann factors multiplied with Times: the evaluation is aborted",
  Quiet[CheckAbort[FieldExpectationValue[Exp[-IntegralOver[G0[x, y] etabar[x] eta[y], {x, y}]], {psi[a], psibar[b]}], "aborted"]], "aborted"];
check["labels in the order of version 1: a message", quietCheck[ParticleIrreducibleDiagrams[greenFunction, 1, {G0}, {a, b}], "message"], "message"];
check["an unknown drawing option: a message", quietCheck[DiagramToTikZ[-G[a, b] V[a, b], {a, b}, "Scal" -> 0.5], "message"], "message"];
SetOptions[DiagramToTikZ, "Scale" -> 0.5];
check["SetOptions changes the defaults of the pictures", StringFreeQ[DiagramToTikZ[-G[a, b] V[a, b], {a, b}], "x=0.500cm"], False];
SetOptions[DiagramToTikZ, "Scale" -> 0.7];
check["\"Columns\" -> 0 and \"Scale\" -> Automatic: the automatic choice and the default",
  {Check[Head[DiagramPlot[G[a, b] - G[a, b] V[a, b], {a, b}, "Columns" -> 0]], "message"],
   Check[StringFreeQ[DiagramToTikZ[G[a, b], {a, b}, "Scale" -> Automatic], "x=0.700cm"], "message"]}, {Graphics, False}];
check["a local vertex without labels: drawn without errors", Check[Head[DrawDiagram[Vqg[x, y, z]]], "message"], Graphics];
check["a line head with four arguments: a message", quietCheck[ParticleIrreducibleDiagrams[IntegralOver[Gs[a, up, z, up] V[z, z] Gs[z, up, b, up], {z}],
    1, {a, b}, {Gs}], "message"], "message"];

check["numeric labels: a message", quietCheck[PerturbativeCorrelationFunction[Z0, {phi[1], phi[2]}, Phi4Vertex, 1], "message"], "message"];
check["closed loops at a local vertex (Hubbard)", ClosedLoopCount[IntegralOver[Gu[a, x] Gu[x, b] Gd[x, x], {x}], {Gu, Gd}], 1];
check["skeleton vacuum diagrams are the 2PI ones", Length[DiagramTerms[SkeletonDiagrams[vacuumPhi4, {}, {D0}]]], 2];
check["a Grassmann source squared vanishes", SimplifyDiagrams[eta[x] eta[x] f[x]], 0];
check["powers of a delta between external labels", SimplifyDiagrams[DeltaFunction[a, b]^2 (f[a] - f[b])], 0];
check["an unknown value of \"Statistics\": a message", quietCheck[ParquetDecomposition[0, {a, b, c, d}, {G}, "Statistics" -> "boson"], "message"],
  "message"];
check["tree expansion with one label: a message", quietCheck[ConnectedFunctionFromVertices[{a}], "message"], "message"];
DeclareSource[Jc, Jcbar]; DeclareField[phic, Jcbar]; DeclareField[phicbar, Jc];
Zc = Exp[IntegralOver[Jcbar[x] Dc[x, y] Jc[y], {x, y}]];
complexInteraction = 1/2 IntegralOver[V[x, y] phicbar[x] phicbar[y] phic[y] phic[x], {x, y}];
complexSigma = AmputateExternalLegs[ParticleIrreducibleDiagrams[PerturbativeCorrelationFunction[Zc, {phic[a], phicbar[b]}, complexInteraction, 2],
    1, {a, b}, {Dc}], {a, b}, {Dc}];
checkZero["complex bosons: Sigma = -dPhi/dG(y, x) is the skeleton part of the perturbative Sigma",
  (SelfEnergy[LuttingerWardFunctional[Zc, complexInteraction, 2, Dc -> Dcd], Dcd, {a, b}, "Statistics" -> "ComplexBoson"] /. Dcd -> Dc) -
   SkeletonDiagrams[complexSigma, {a, b}, {Dc}]];
check["\"Boson\" with a propagator that is not symmetric: a message",
  quietCheck[SelfEnergy[LuttingerWardFunctional[Zc, complexInteraction, 1, Dc -> Dcd], Dcd, {a, b}, "Statistics" -> "Boson"], "message"], "message"];
(* the default "Statistics" -> "Fermion" on the symmetric propagator of a real boson: a message; labels counted *)
DeclareSymmetric[Gs4]; phiGs4 = LuttingerWardFunctional[Z0, Phi4Vertex, 2, D0 -> Gs4];
check["SelfEnergy and IrreducibleVertex on a symmetric G without \"Statistics\": message statisticsdefault; none with it",
  {messageGivenQ[SelfEnergy[phiGs4, Gs4, {a, b}], FunctionalDiagrams::statisticsdefault],
   messageGivenQ[IrreducibleVertex[phiGs4, Gs4, {a, b, c, d}], FunctionalDiagrams::statisticsdefault],
   Check[SelfEnergy[phiGs4, Gs4, {a, b}, "Statistics" -> "Boson"]; "no message", "message"],
   Check[IrreducibleVertex[phiGs4, Gs4, {a, b, c, d}, "Statistics" -> "Boson"]; "no message", "message"]}, {True, True, "no message", "no message"}];
check["SelfEnergy, IrreducibleVertex and PerturbativeFullVertex with a wrong number of labels: message vertexlabels and $Failed",
  Quiet[{messageGivenQ[IrreducibleVertex[phiGs4, Gs4, {a, b, c}, "Statistics" -> "Boson"], FunctionalDiagrams::vertexlabels],
    IrreducibleVertex[phiGs4, Gs4, {a, b, c}, "Statistics" -> "Boson"], SelfEnergy[phiGs4, Gs4, {a}, "Statistics" -> "Boson"],
    PerturbativeFullVertex[Z0, Phi4Vertex, 1, {phi}, {a, b}, D0 -> Gs4]}], {True, $Failed, $Failed, $Failed}];
check["EffectiveAction2PI on a symmetric G without \"Statistics\": message statisticsdefault; none with it",
  {messageGivenQ[EffectiveAction2PI[phiGs4, D0 -> Gs4], EffectiveAction2PI::statisticsdefault],
   Check[EffectiveAction2PI[phiGs4, D0 -> Gs4, "Statistics" -> "Boson"]; "no message", "message"]}, {True, "no message"}];
check["ParquetDecomposition: one line head without a list; a wrong call gives a message and $Failed",
  {Keys[ParquetDecomposition[OrderPart[IrreducibleVertex[phiGs4, Gs4, {a, b, c, d}, "Statistics" -> "Boson"], lambda, 1], {a, b, c, d}, Gs4,
      "Statistics" -> "Boson"]],
   messageGivenQ[ParquetDecomposition[phiGs4, phiGs4, Gs4, {a, b, c, d}], ParquetDecomposition::args],
   Quiet[ParquetDecomposition[phiGs4, phiGs4, Gs4, {a, b, c, d}]],
   messageGivenQ[ParquetDecomposition[phiGs4, {a, b, c}, {Gs4}], FunctionalDiagrams::vertexlabels]},
  {{"FullyIrreducible", "s", "t", "u", "SeveralChannels"}, True, $Failed, True}];
check["LabelFunction: its variables print without the private context of the package",
  StringFreeQ[ToString[InputForm[LabelFunction[lambda DeltaFunction[u, v] J[u]^2, {u, v}]]], "Private`"], True];
check["a field without the spin index of its source: message fieldarity",
  {messageGivenQ[FieldExpectationValue[Zh, {psi[a], psibar[b]}], FunctionalDiagrams::fieldarity],
   messageGivenQ[ConnectedCorrelationFunction[Zh, {psi[a], psibar[b]}, 0, 0], FunctionalDiagrams::fieldarity],
   Check[FieldExpectationValue[Zh, {psi[a, up], psibar[b, up]}]; "no message", "message"]}, {True, True, "no message"}];
check["LuttingerWardFunctional with a symmetric D0 and an undeclared G: message rulesymmetry",
  messageGivenQ[LuttingerWardFunctional[Z0, Phi4Vertex, 1, D0 -> Gundeclared], FunctionalDiagrams::rulesymmetry], True];
gamma2IK = SetSourcesToZero[FunctionalDerivative[EffectiveAction1PI[Z0, Phi4Vertex, 1, phi -> phiMeanIK, D0 -> DmIK],
    phiMeanIK[a], phiMeanIK[b]]];
check["DiagramPlot of a term with OperatorKernel[D^-1] and lines D (before ExpandTrace): message inversekernel; none after it",
  {messageGivenQ[DiagramPlot[gamma2IK, {a, b}], FunctionalDiagrams::inversekernel],
   Check[DiagramPlot[ExpandTrace[gamma2IK], {a, b}]; "no message", "message"]}, {True, "no message"}];
check["PerturbativeFullVertex with a list of rules", PerturbativeFullVertex[Zf, CoulombVertex, 1, {psi, psibar}, {x1, x2, x3, x4}, {G0 -> G}] ===
   PerturbativeFullVertex[Zf, CoulombVertex, 1, {psi, psibar}, {x1, x2, x3, x4}, G0 -> G], True];
DeclareSymmetric[Vs]; DeclareAntisymmetric[Ua]; ResetDeclarations[];
check["ResetDeclarations undoes the symmetries", ToString[{Vs[b, a], Ua[b, a]}, InputForm], "{Vs[b, a], Ua[b, a]}"];
check["ResetDeclarations removes the blobs", StringFreeQ[DiagramToTikZ[SgBlob[a, b], {a, b}], "Sigma"], True];
check["ResetDeclarations removes the interaction lines and the couplings",
  {FunctionalDiagrams`Private`$interactionLineHeads, FunctionalDiagrams`Private`$couplingSymbols}, {{}, {}}];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: closed forms"];
(* Matsubara sums (section 17 of the package). Brute-force sums: symmetric truncation, Richardson extrapolation in the
   number of frequencies, and the convergence factor added explicitly (T Sum e^(i eta w 0+) g = symmetric sum + eta c/2,
   c = lim z g(z)). The declarations of the electron gas are those of the showcase. *)
scientific[x_] := If[x == 0, "0", With[{ex = Floor[Log10[N[x]]]}, ToString[N[Round[x/10^ex, 1/10]]] <> "e" <> ToString[ex]]];
checkSmall[name_String, error_, tolerance_] := check[name <> "   [error " <> scientific[error] <> "]", error < tolerance, True];
{bruteForceFrequencies, bruteForceTolerance} = {20000, 10^-6};
(* the coefficient of a loop frequency x in the exponent of the convergence factor, by substitution *)
loopCoefficient[exponent_, x_, all_List] := (exponent /. Table[y -> If[y === x, 1, 0], {y, all}]) - (exponent /. Table[y -> 0, {y, all}]);
thermalZeroQ = FunctionalDiagrams`Private`thermalZeroQ;
deltaCoefficients = FunctionalDiagrams`Private`deltaCoefficients;
sameThermalQ[a_, b_] := thermalZeroQ[a - b];
withoutMomentumSum[e_] := e /. MomentumSum[f_, _] :> f;
(* two sums of delta functions with the same coefficients (statistical factors compared through e^(beta e)) *)
sameDeltaExpansionQ[a_, b_] := AllTrue[deltaCoefficients[Expand[withoutMomentumSum[a] - withoutMomentumSum[b]]], thermalZeroQ[#[[2]]] &];

(* temperature = 0.37, numeric (Fermi and Bose functions at that temperature), frequencyValues, bruteForceDiagram and
   bruteForceErrors (the brute-force sums of the integrand of a diagram), shared with Showcase/make_figures.wl *)
Get[FileNameJoin[{testDirectory, "MatsubaraBruteForce.wl"}]];
relativeError[a_, b_] := Abs[a - b]/Max[Abs[b], 10^-3];

(* T Sum_n f(z = i w_n) e^(i eta w_n 0+), z a symbol: symmetric truncation with n and 2n frequencies, Richardson *)
bruteForceSum[f_, z_, statistics_, eta_, n_] := Module[{s},
   s[m_] := temperature Total[f /. z -> I frequencyValues[statistics, m]] + eta/2 ((z f) /. z -> I 10.^8);
   2 s[2 n] - s[n]];

check["fermion, e^(i w 0+): n_F", MatsubaraSum[1/(z - a), z, "Fermion", "ConvergenceFactor" -> 1], FermiFunction[a]];
check["fermion, e^(-i w 0+): n_F - 1", MatsubaraSum[1/(z - a), z, "Fermion", "ConvergenceFactor" -> -1], -1 + FermiFunction[a]];
check["fermion, symmetric: n_F - 1/2", MatsubaraSum[1/(z - a), z, "Fermion", "ConvergenceFactor" -> 0], -1/2 + FermiFunction[a]];
check["boson, e^(i nu 0+): -n_B", MatsubaraSum[1/(z - a), z, "Boson", "ConvergenceFactor" -> 1], -BoseFunction[a]];
check["boson, e^(-i nu 0+): -n_B - 1", MatsubaraSum[1/(z - a), z, "Boson", "ConvergenceFactor" -> -1], -1 - BoseFunction[a]];
check["boson, symmetric: -n_B - 1/2", MatsubaraSum[1/(z - a), z, "Boson", "ConvergenceFactor" -> 0], -1/2 - BoseFunction[a]];
check["two poles", Together[MatsubaraSum[1/((z - a) (z - b)), z, "Fermion"] - (FermiFunction[a] - FermiFunction[b])/(a - b)], 0];
check["double pole: derivative of n_F", MatsubaraSum[1/(z - a)^2, z, "Fermion"], Derivative[1][FermiFunction][a]];
check["triple pole", MatsubaraSum[1/(z - a)^3, z, "Fermion"], Derivative[2][FermiFunction][a]/2];
check["reversed denominator", Together[MatsubaraSum[1/((a - z) (z - b)), z, "Boson"] - (BoseFunction[a] - BoseFunction[b])/(a - b)], 0];
check["quadratic denominator split", Together[MatsubaraSum[1/(z^2 - a^2), z, "Boson"] - (-BoseFunction[a] + BoseFunction[-a])/(2 a) /.
     BoseFunction[-a] -> -1 - BoseFunction[a]], 0];
DeclareMatsubaraFrequency[nu, "Boson"]; DeclareMatsubaraFrequency[w, "Fermion"];
check["periodicity, bosonic shift", MatsubaraSum[1/((z - a) (z + I nu - b)), z, "Fermion"] // FreeQ[Cases[#, _FermiFunction | _BoseFunction, Infinity], nu] &, True];
check["periodicity, fermionic shift: n_F(e + i w) = -n_B(e)", FunctionalDiagrams`Private`simplifyStatisticalFactors[
   {FermiFunction[e + I w], BoseFunction[e - I w], FermiFunction[e + 2 I w], Derivative[1][FermiFunction][e + I w + I nu]}],
  {-BoseFunction[e], -FermiFunction[e], FermiFunction[e], -Derivative[1][BoseFunction][e]}];
check["negative arguments made positive", FunctionalDiagrams`Private`simplifyStatisticalFactors[{FermiFunction[-e], BoseFunction[-e],
    Derivative[1][FermiFunction][-e], Derivative[2][FermiFunction][-e]}],
  {1 - FermiFunction[e], -1 - BoseFunction[e], Derivative[1][FermiFunction][e], -Derivative[2][FermiFunction][e]}];
(* the Migdal sum: T Sum_nu D(i nu) G(i w - i nu), D = 2 W/(W^2 - (i nu)^2) the <phi phi> of the package *)
migdalSum = MatsubaraSum[2 W/(W^2 - z^2) 1/(I w - z - e), z, "Boson"];
check["Migdal sum, textbook form", sameThermalQ[migdalSum,
   (BoseFunction[W] + 1 - FermiFunction[e])/(I w - e - W) + (BoseFunction[W] + FermiFunction[e])/(I w - e + W)], True];
check["convergence message when needed", Quiet[Check[MatsubaraSum[1/(z - a), z, "Fermion"]; "no message", "message", MatsubaraSum::conv], MatsubaraSum::conv], "message"];
check["no convergence message for 1/z^2", Check[MatsubaraSum[1/((z - a) (z - b)), z, "Fermion"]; "no message", "message"], "no message"];
check["a pole on the Matsubara axis: a message", Quiet[Check[MatsubaraSum[1/(z (z - a)), z, "Boson"]; "no message", "message",
    MatsubaraSum::axis], MatsubaraSum::axis], "message"];
check["an unknown statistics: a message", Quiet[Check[DeclareMatsubaraFrequency[w2, "fermion"]; "no message", "message"]], "message"];
check["continuation in a frequency without statistics: a message", Quiet[Check[AnalyticContinuation[MatsubaraSum[1/((z - a) (z + I w3 - b)), z,
      "Fermion"], w3 -> omega]; "no message", "message"]], "message"];
(* the behaviour at large z is read from the degrees: a summand that does not decay diverges, terms that cancel at
   leading order are taken together, and numbers do not leave a spurious convergence term *)
check["a summand that does not decay: a message and $Failed", {messageGivenQ[MatsubaraSum[z, z, "Fermion"], MatsubaraSum::diverge],
   Quiet[MatsubaraSum[z^2, z, "Boson"], MatsubaraSum::diverge], Quiet[MatsubaraSum[z/(z - a), z, "Fermion"], MatsubaraSum::diverge],
   Quiet[MatsubaraSum[z^2/((z - a) (z - b)), z, "Boson"], MatsubaraSum::diverge]}, {True, $Failed, $Failed, $Failed}];
check["terms that cancel at large z: the sum of z/(z - a) - z/(z - b)", MatsubaraSum[z/(z - a) - z/(z - b), z, "Fermion", "ConvergenceFactor" -> 1],
  a FermiFunction[a] - b FermiFunction[b]];
check["numbers: no convergence message for 1/z^3, no rounding error added",
  {messageGivenQ[MatsubaraSum[1/((z - 0.1) (z - 0.2) (z - 0.37)), z, "Fermion"], MatsubaraSum::conv],
   Count[List @@ MatsubaraSum[1/((z - 0.1) (z - 0.2) (z - 0.37)), z, "Fermion"], _?NumberQ],
   messageGivenQ[MatsubaraSum[2 0.8/(0.8^2 - z^2) 1/(z - 0.3), z, "Boson"], MatsubaraSum::conv]}, {False, 0, False}];
check["a denominator that is not split into poles: $Failed", Quiet[{MatsubaraSum[1/(z^3 - 2), z, "Fermion"], RetardedImaginaryPart[1/(omega^3 - 2), omega]},
   {MatsubaraSum::degree, MatsubaraSum::pole}], {$Failed, $Failed}];
(* a pole at a frequency of the other statistics is an ordinary pole: n_F(0) = n_F(i nu) = 1/2, n_B(i w) = -1/2 *)
check["a pole at a bosonic frequency in a fermionic sum, and the converse: no message",
  {messageGivenQ[MatsubaraSum[1/(z (z - a)), z, "Fermion"], MatsubaraSum::axis], messageGivenQ[MatsubaraSum[1/((z - I nu) (z - a)), z, "Fermion"],
    MatsubaraSum::axis], messageGivenQ[MatsubaraSum[1/((z - I w) (z - a)), z, "Boson"], MatsubaraSum::axis]}, {False, False, False}];
check["a pole at z = 0 in a fermionic sum", Together[MatsubaraSum[1/(z (z - a)), z, "Fermion"] - (FermiFunction[a] - 1/2)/a], 0];
check["a pole on the axis of the fermionic frequencies: a message", messageGivenQ[MatsubaraSum[1/((z - I w) (z - a)), z, "Fermion"], MatsubaraSum::axis], True];
check["n_F(0) = 1/2, also at T = 0", {FermiFunction[0], ZeroTemperatureLimit[FermiFunction[0]], ZeroTemperatureLimit[FermiFunction[e]]},
  {1/2, 1/2, UnitStep[-e]}];
check["a constant in the argument does not decide its sign", FunctionalDiagrams`Private`simplifyStatisticalFactors[{FermiFunction[eps[p] - mu],
    FermiFunction[mu - eps[p]], FermiFunction[p^2/(2 m) - mu], BoseFunction[mu - Om[p]], FermiFunction[a - b], FermiFunction[b - a]}],
  {FermiFunction[eps[p] - mu], 1 - FermiFunction[eps[p] - mu], FermiFunction[p^2/(2 m) - mu], -1 - BoseFunction[Om[p] - mu],
   FermiFunction[a - b], 1 - FermiFunction[a - b]}];
(* a loop frequency of DiagramIntegrand in the summand: it is a symbol during the sum, and periodicity applies to it *)
check["a loop frequency in the summand", With[{s = MatsubaraSum[1/((z - a) (z + I LoopFrequency[2, "Boson"] - b)), z, "Fermion"]},
   {FreeQ[Cases[s, _FermiFunction, Infinity], LoopFrequency], Together[(s - (FermiFunction[a] - FermiFunction[b])/(a - b + I LoopFrequency[2, "Boson"])) /.
       LoopFrequency[2, "Boson"] -> nu2]}], {True, 0}];
check["invalid arguments: messages and $Failed", {
   messageGivenQ[MatsubaraSum[1/(z - a), z, "Fermion", "ConvergenceFactor" -> 2], MatsubaraSum::factor],
   Quiet[MatsubaraSum[1/(z - a), z, "Fermion", "ConvergenceFactor" -> 2], MatsubaraSum::factor],
   Quiet[MatsubaraSum[1/(z[1] - a), z[1], "Fermion"], MatsubaraSum::variable],
   Quiet[MatsubaraSum[1/(z - a), z, Fermion], MatsubaraSum::statistics],
   messageGivenQ[MatsubaraSum[1/(z - a), z, "Fermion", "Convergence" -> 1, "ConvergenceFactor" -> 1], FunctionalDiagrams::optx],
   messageGivenQ[DeclareMatsubaraFrequency[3, "Fermion"], DeclareMatsubaraFrequency::symbol]}, {True, $Failed, $Failed, $Failed, True, True}];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: brute-force sums"];
numbers = {a -> 0.3, b -> -0.7, c -> 1.1, e -> 0.45, W -> 0.8, w -> 3 Pi temperature, nu -> 4 Pi temperature};
sumCases = {
   {1/(z - a), "Fermion", 1}, {1/(z - a), "Fermion", -1}, {1/(z - a), "Boson", 1}, {1/(z - a), "Boson", 0},
   {1/((z - a) (z - b)), "Fermion", 0}, {1/(z - a)^2, "Fermion", 0}, {1/((z - a)^2 (z - b)), "Boson", 0},
   {1/((z - a)^3 (z - b)), "Fermion", 0}, {z/((z - a) (z - b) (z - c)), "Fermion", 0}, {z^2/((z - a)^2 (z - b)^2 (z - c)), "Boson", 0},
   {1/((z - a) (z + I nu - b)), "Fermion", 0}, {1/((z - a)^2 (z + I w - b)), "Boson", 0},
   {2 W/(W^2 - z^2) 1/(I w - z - e), "Boson", 0}, {1/((z^2 - a^2) (z - b)^2), "Boson", 0},
   {1/(z (z - a)), "Fermion", 0}, {1/((z - I w) (z - a)), "Boson", 0}, {1/((z - 0.1) (z - 0.2) (z - 0.37)), "Fermion", 0}};
Do[With[{f = cs[[1]], s = cs[[2]], eta = cs[[3]]},
   checkSmall["brute force: " <> ToString[InputForm[f]] <> ", " <> s <> ", eta = " <> ToString[eta],
    relativeError[numeric[MatsubaraSum[f, z, s, "ConvergenceFactor" -> eta] /. numbers], bruteForceSum[f /. numbers, z, s, eta, bruteForceFrequencies]],
    bruteForceTolerance]],
  {cs, sumCases}];
(* nested: T Sum_nu D(i nu) T Sum_w G(i w) G(i w + i nu); the inner result contains n_F(b - i nu) = n_F(b) *)
inner = MatsubaraSum[1/((z - a) (z + I nu - b)), z, "Fermion"];
nested = MatsubaraSum[(inner 2 W/(W^2 + nu^2)) /. nu -> -I z, z, "Boson"];
nestedBrute[n_] := Module[{s, f = N[1/((z1 - a) (z1 + z2 - b)) 2 W/(W^2 - z2^2) /. numbers]},
   s[m_] := With[{w1 = I frequencyValues["Fermion", m], w2 = I frequencyValues["Boson", m]},
     temperature^2 Total[Flatten[f /. {z1 -> Transpose[ConstantArray[w1, Length[w2]]], z2 -> ConstantArray[w2, Length[w1]]}]]];
   2 s[2 n] - s[n]];
checkSmall["nested sum, brute force", relativeError[numeric[nested /. numbers], nestedBrute[200]], 10^-5];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: diagrams of the electron gas"];
DeclareGrassmannSource[eta, etabar]; DeclareDirectedPropagator[BareGreenFunction];
FreeFermionGeneratingFunctional = Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
DeclareSymmetric[InteractionPotential]; DeclareInteractionLine[InteractionPotential];
CoulombVertex = 1/2 IntegralOver[InteractionPotential[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
greenFunction = -PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x], psibar[y]}, CoulombVertex, 2];
DeclareDirectedPropagator[DressedGreenFunction];
luttingerWard = LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 2, BareGreenFunction -> DressedGreenFunction];
selfEnergy = SelfEnergy[luttingerWard, DressedGreenFunction, {x, y}];
densityCorrelation = ConnectedCorrelationFunction[FreeFermionGeneratingFunctional, {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1];
gasRules = {BareGreenFunction -> {"Fermion", xi}, DressedGreenFunction -> {"Fermion", xi}, InteractionPotential -> {"Instantaneous", v}};
twoPoint = {y -> {k, I w}, x -> {-k, -I w}};
bubbleExternals = {b -> {q, I nu}, a -> {-q, -I nu}};
gasValues = {xi -> Function[p, p^2/2 - 0.35], v -> Function[p, 1/(p^2 + 0.6)], k -> 0.45, q -> 0.8, LoopMomentum[1] -> 0.3,
   LoopMomentum[2] -> -0.55, p -> 0.3, p1 -> 0.3, p2 -> -0.55, w -> 3 Pi temperature, nu -> 4 Pi temperature};

check["Hartree and Fock", EvaluateMatsubara[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, "LoopMomenta" -> {p},
   "SpinDegeneracy" -> 2], 2 v[0] MomentumSum[FermiFunction[xi[p]], {p}] - MomentumSum[v[k - p] FermiFunction[xi[p]], {p}]];
(* the ordering of the lines at equal times: G(0-) = n_F (the default), G(0+) = n_F - 1, their mean n_F - 1/2; the
   Hartree-Fock self-energy changes by a constant, (1 - eta)/2 times the Hartree and Fock terms with n_F -> -1 *)
hartreeFock[opts___] := EvaluateMatsubara[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, "LoopMomenta" -> {p},
   "SpinDegeneracy" -> 2, opts];
check["Hartree and Fock: \"ConvergenceFactor\" -> 1 is the default", hartreeFock["ConvergenceFactor" -> 1], hartreeFock[]];
check["Hartree and Fock with e^(-i w 0+) and with the symmetric sum: n_F - 1, n_F - 1/2",
  Table[Expand[withoutMomentumSum[hartreeFock["ConvergenceFactor" -> factor] - hartreeFock[]]], {factor, {-1, 0}}],
  {Expand[v[k - p] - 2 v[0]], Expand[(v[k - p] - 2 v[0])/2]}];
check["\"ConvergenceFactor\" -> 2: a message and $Failed", {
   messageGivenQ[hartreeFock["ConvergenceFactor" -> 2], EvaluateMatsubara::convergence],
   Quiet[hartreeFock["ConvergenceFactor" -> Automatic], EvaluateMatsubara::convergence],
   Quiet[CheckCuttingRules[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, w -> omega, "ConvergenceFactor" -> 2],
    EvaluateMatsubara::convergence],
   messageGivenQ[DiagramIntegrand[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, "ConvergenceFactor" -> 1],
    FunctionalDiagrams::optx]}, {True, $Failed, $Failed, True}];

(* a. the Lindhard function: the package's <rho rho>_c = -G G is minus chi_0 *)
lindhard = EvaluateMatsubara[OrderPart[densityCorrelation, InteractionPotential, 0], bubbleExternals, gasRules, "SpinDegeneracy" -> 2,
   "LoopMomenta" -> {p}];
check["Lindhard: one denominator", lindhard, -2 MomentumSum[(FermiFunction[xi[p]] - FermiFunction[xi[p + q]])/(I nu + xi[p] - xi[p + q]), {p}]];
check["Lindhard: <rho rho>_c = -chi_0", Together[withoutMomentumSum[lindhard +
      2 MomentumSum[(FermiFunction[xi[p]] - FermiFunction[xi[p + q]])/(I nu + xi[p] - xi[p + q]), {p}]]], 0];
check["Lindhard: Im of the retarded function", sameDeltaExpansionQ[RetardedImaginaryPart[AnalyticContinuation[lindhard, nu -> omega], omega],
   2 Pi MomentumSum[(FermiFunction[xi[p]] - FermiFunction[xi[p + q]]) EnergyDelta[omega + xi[p] - xi[p + q]], {p}]], True];
check["Lindhard: cutting rules", CheckCuttingRules[OrderPart[densityCorrelation, InteractionPotential, 0], bubbleExternals, gasRules, nu -> omega,
   "SpinDegeneracy" -> 2], 0];
checkSmall["Lindhard: brute force", Max[bruteForceErrors[OrderPart[densityCorrelation, InteractionPotential, 0], bubbleExternals, gasRules,
     gasValues, bruteForceFrequencies]], bruteForceTolerance];
(* T = 0, one dimension, k_F = m = 1, nu = 0: <rho rho>(q, 0) = (2/(Pi q)) Log|(2 + q)/(2 - q)| *)
staticLindhardZeroT = withoutMomentumSum[ZeroTemperatureLimit[lindhard /. nu -> 0]] /. {xi -> Function[s, s^2/2 - 1/2], q -> 4/5};
check["Lindhard at T = 0 in one dimension", Abs[NIntegrate[staticLindhardZeroT, {p, -3, -9/5, -1, 1/5, 1, 3}]/(2 Pi) -
      2/(Pi 4/5) Log[(2 + 4/5)/(2 - 4/5)]] < 10^-7, True];
check["Lindhard at low T tends to T = 0", Module[{t = 0.004},
   Abs[NIntegrate[withoutMomentumSum[lindhard /. nu -> 0] /. {FermiFunction -> Function[e, 1/(Exp[e/t] + 1)], xi -> Function[s, s^2/2 - 1/2],
         q -> 4/5}, {p, -3, -9/5, -1, 1/5, 1, 3}]/(2 Pi) - 2/(Pi 4/5) Log[(2 + 4/5)/(2 - 4/5)]] < 10^-3], True];

(* b. the second-order self-energy (direct with a bubble, and exchange): Im Sigma^R = -Pi Sum [(2s + 1) v_q^2 - v_q v_q']
   delta(omega + xi_3 - xi_1 - xi_2) [n_3 (1 - n_1) (1 - n_2) + (1 - n_3) n_1 n_2] *)
selfEnergy2 = OrderPart[selfEnergy, InteractionPotential, 2];
sigma2 = EvaluateMatsubara[selfEnergy2, twoPoint, gasRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}];
imSigma2 = RetardedImaginaryPart[AnalyticContinuation[sigma2, w -> omega], omega];
textbookImSigma2 = With[{n1 = FermiFunction[xi[p1]], n2 = FermiFunction[xi[k - p1 + p2]], n3 = FermiFunction[xi[p2]]},
   -Pi MomentumSum[(2 v[p1 - p2]^2 - v[p1 - p2] v[k - p1]) EnergyDelta[omega + xi[p2] - xi[p1] - xi[k - p1 + p2]]
      (n3 (1 - n1) (1 - n2) + (1 - n3) n1 n2), {p1, p2}]];
check["second-order self-energy: textbook Im Sigma^R", sameDeltaExpansionQ[imSigma2, textbookImSigma2], True];
(* the result written as in the textbooks: one denominator, the occupation factors of the processes *)
check["second-order self-energy: textbook form", sigma2, With[{n1 = FermiFunction[xi[p1]], n2 = FermiFunction[xi[k - p1 + p2]],
    n3 = FermiFunction[xi[p2]]}, (2 MomentumSum[v[p1 - p2]^2 #, {p1, p2}] - MomentumSum[v[k - p1] v[p1 - p2] #, {p1, p2}]) &[
    (n3 (1 - n1) (1 - n2) + (1 - n3) n1 n2)/(I w - xi[p1] + xi[p2] - xi[k - p1 + p2])]]];
check["second-order self-energy: the same imaginary part from residues and cutting rules", imSigma2 ===
   CuttingRuleImaginaryPart[selfEnergy2, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}], True];
check["second-order self-energy: cutting rules", CheckCuttingRules[selfEnergy2, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2], 0];
check["second-order self-energy: cut side alone", sameDeltaExpansionQ[CuttingRuleImaginaryPart[selfEnergy2, twoPoint, gasRules, w -> omega,
    "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}], textbookImSigma2], True];
(* the two-loop brute-force sums: outer levels 60, 120, 240, the inner sums at levels 1200 and 600 (errors about
   1.5 10^-6, a sixth of the tolerance; at level 100 the errors are 4 10^-7 but the sums take three times longer) *)
checkSmall["second-order self-energy: brute force", Max[bruteForceErrors[selfEnergy2, twoPoint, gasRules, gasValues, 60, 60]], 10^-5];
check["Hartree and Fock have no imaginary part", CheckCuttingRules[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, w -> omega], 0];

(* c. self-energy insertions: double poles, derivatives of n_F *)
greenFunction2 = OrderPart[greenFunction, InteractionPotential, 2];
densityCorrelation1 = OrderPart[densityCorrelation, InteractionPotential, 1];
check["G at second order: derivatives of n_F appear", ! FreeQ[EvaluateMatsubara[greenFunction2, twoPoint, gasRules], Derivative[1][FermiFunction]], True];
check["density correlation at first order: derivatives of n_F appear",
  ! FreeQ[EvaluateMatsubara[densityCorrelation1, bubbleExternals, gasRules], Derivative[1][FermiFunction]], True];
checkSmall["G at second order (10 diagrams): brute force", Max[bruteForceErrors[greenFunction2, twoPoint, gasRules, gasValues, 60, 60]], 10^-5];
checkSmall["density correlation at first order: brute force", Max[bruteForceErrors[densityCorrelation1, bubbleExternals, gasRules, gasValues, 60, 60]], 10^-5];
(* the other orderings of the lines at equal times on nested sums (Hartree and Fock insertions, double poles): the
   brute-force sum with the exponent of the convergence factor multiplied by the chosen value *)
checkSmall["G at second order, e^(-i w 0+) and symmetric sums: brute force", Max[Table[With[{value = numeric[
       withoutMomentumSum[EvaluateMatsubara[t, twoPoint, gasRules, "ConvergenceFactor" -> factor]] /. gasValues],
      data = First[DiagramIntegrand[t, twoPoint, gasRules]]}, relativeError[value, bruteForceDiagram[
       data /. ("EqualTimeFrequency" -> x_) :> ("EqualTimeFrequency" -> factor x), gasValues, 60, 60]]],
    {factor, {-1, 0}}, {t, DiagramTerms[greenFunction2]}]], 10^-5];
(* cutting rules with static self-energy insertions on the cut lines (double poles, derivatives of delta); the vertex
   correction (an interaction between the two lines of the bubble) is not of the implemented kind *)
cutResults = Quiet[CheckCuttingRules[#, bubbleExternals, gasRules, nu -> omega, "SpinDegeneracy" -> 2] & /@ DiagramTerms[densityCorrelation1],
   CuttingRuleImaginaryPart::class];
(* six diagrams: two Hartree and two Fock insertions (cutting rules), the vertex correction and the two bubbles joined by
   an interaction (a vertex with four propagating lines: not implemented) *)
check["density correlation at first order: cutting rules with Hartree and Fock insertions", {Count[cutResults, 0], Count[cutResults, {$Failed}]}, {4, 2}];
(* the static insertions with the other orderings of the lines at equal times: n_F - 1, n_F - 1/2 on both sides *)
check["cutting rules with Hartree and Fock insertions, e^(-i w 0+) and symmetric sum", Table[Count[Quiet[
      CheckCuttingRules[#, bubbleExternals, gasRules, nu -> omega, "SpinDegeneracy" -> 2, "ConvergenceFactor" -> factor] & /@
       DiagramTerms[densityCorrelation1], CuttingRuleImaginaryPart::class], 0], {factor, {-1, 0}}], {4, 4}];
check["bubble with a Fock insertion: delta' in the imaginary part", ! FreeQ[RetardedImaginaryPart[AnalyticContinuation[
      EvaluateMatsubara[densityCorrelation1, bubbleExternals, gasRules], nu -> omega], omega], Derivative[1][EnergyDelta]], True];
selfEnergyPerturbative2 = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction2, 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}];
check["perturbative self-energy at second order: cutting rules", CheckCuttingRules[selfEnergyPerturbative2, twoPoint, gasRules, w -> omega,
   "SpinDegeneracy" -> 2], 0];

(* three loops: the third-order skeleton self-energy; the frequency sums done in the opposite order give the same result *)
section["Matsubara sums: third order"];
luttingerWard3 = LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 3, BareGreenFunction -> DressedGreenFunction];
selfEnergy3 = OrderPart[SelfEnergy[luttingerWard3, DressedGreenFunction, {x, y}], InteractionPotential, 3];
reversedSums[data_List] := Module[{g = "Integrand" /. data, exponent = "EqualTimeFrequency" /. data, zz},
   Do[With[{c = loopCoefficient[exponent, f, "LoopFrequencies" /. data]},
     g = MatsubaraSum[g /. f -> -I zz, zz, f[[2]], "ConvergenceFactor" -> If[c == 0, Automatic, Sign[c]]]], {f, Reverse["LoopFrequencies" /. data]}];
   ("Weight" /. data) g];
values3 = Join[gasValues, {LoopMomentum[3] -> 0.71}];
timing3 = AbsoluteTiming[evaluated3 = Table[withoutMomentumSum[EvaluateMatsubara[t, twoPoint, gasRules]], {t, DiagramTerms[selfEnergy3]}]][[1]];
Print["third-order self-energy: ", Length[evaluated3], " diagrams evaluated in ", N[Round[10 timing3]/10], " s"];
checkSmall["third-order self-energy: sums in the opposite order", Max[Table[relativeError[numeric[evaluated3[[i]] /. values3],
      numeric[reversedSums[First[DiagramIntegrand[DiagramTerms[selfEnergy3][[i]], twoPoint, gasRules]]] /. values3]], {i, Length[evaluated3]}]], 10^-10];

(* the perturbative self-energy at third order: second-order sunsets with Hartree and Fock insertions on their lines
   (double poles in the cut lines) satisfy the cutting rules *)
greenFunction3 = OrderPart[-PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x], psibar[y]}, CoulombVertex, 3], InteractionPotential, 3];
selfEnergyPerturbative3 = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction3, 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}];
cutResults3 = Quiet[CheckCuttingRules[#, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2] & /@ DiagramTerms[selfEnergyPerturbative3],
   CuttingRuleImaginaryPart::class];
Print["perturbative self-energy at third order: ", Length[cutResults3], " diagrams, ", Count[cutResults3, 0], " checked by the cutting rules, ",
  Count[cutResults3, {$Failed}], " not of the implemented kind; ",
  Count[Quiet[CuttingRuleImaginaryPart[#, twoPoint, gasRules, w -> omega] & /@ DiagramTerms[selfEnergyPerturbative3], CuttingRuleImaginaryPart::class],
   e_ /; e =!= 0 && FreeQ[e, $Failed]], " with a non-zero imaginary part"];
check["perturbative self-energy at third order: cutting rules where implemented", Count[cutResults3, 0] + Count[cutResults3, {$Failed}] ==
   Length[cutResults3] && Count[cutResults3, 0] > Length[DiagramTerms[selfEnergy3]], True];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: functions that are not amputated, invalid input"];
(* the partial fractions in the external frequency are taken between the lines that carry it alone (the external legs,
   the lines between irreducible parts), which stay as factors: no part is evaluated on the poles of another (on shell),
   where denominators free of the frequency would vanish *)
greenFunction2Evaluated = EvaluateMatsubara[greenFunction2, twoPoint, gasRules];
check["G at second order: the external legs as factors, no denominator free of w with xi(k)",
  {AllTrue[List @@ greenFunction2Evaluated, ! FreeQ[#, Power[I w - xi[k], n_ /; n <= -2]] &],
   Cases[greenFunction2Evaluated, Power[s_Plus, _?Negative] /; FreeQ[s, w] && ! FreeQ[s, xi[k]], Infinity]}, {True, {}}];
check["G at first order: G0(k)^2 times the self-energy", Together[withoutMomentumSum[EvaluateMatsubara[OrderPart[greenFunction, InteractionPotential, 1],
      twoPoint, gasRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}] - (2 v[0] - v[k - p]) FermiFunction[xi[p]]/(I w - xi[k])^2]], 0];
bubbleChain = -IntegralOver[BareGreenFunction[a, u1] BareGreenFunction[u1, a] InteractionPotential[u1, u2] BareGreenFunction[b, u2] BareGreenFunction[u2, b], {u1, u2}];
check["two bubbles joined by an interaction: no denominator free of nu", Cases[EvaluateMatsubara[bubbleChain, bubbleExternals, gasRules],
   Power[s_Plus, _?Negative] /; FreeQ[s, nu], Infinity], {}];
(* the phi^4 propagator at second order (the boson line is the <phi phi> of the package): the sunset with its legs *)
sunsetPhi4 = First[Select[DiagramTerms[propagatorPhi4], ! FreeQ[#, Power[D0[_, _], 3]] &]];
phi4Rules = {D0 -> {"Boson", Function[{s, zz}, 2 Om[s]/(Om[s]^2 - zz^2)]}};
phi4Externals = {b -> {k, I nu}, a -> {-k, -I nu}};
phi4Values = {Om -> Function[s, 0.6 + 0.3 s^2], k -> 0.4, nu -> 4 Pi temperature, LoopMomentum[1] -> 0.25, LoopMomentum[2] -> -0.6, lambda -> 0.9};
sunsetEvaluated = EvaluateMatsubara[sunsetPhi4, phi4Externals, phi4Rules];
check["phi^4 propagator, sunset with its legs: no denominator free of nu with Om(k)",
  Cases[sunsetEvaluated, Power[s_Plus, _?Negative] /; FreeQ[s, nu] && ! FreeQ[s, Om[k]], Infinity], {}];
checkSmall["phi^4 propagator, sunset with its legs: sums in the opposite order", relativeError[numeric[withoutMomentumSum[sunsetEvaluated] /. phi4Values],
   numeric[reversedSums[First[DiagramIntegrand[sunsetPhi4, phi4Externals, phi4Rules]]] /. phi4Values]], 10^-10];
(* the public form of the loop frequencies; the symbols that stand for them during the sums do not appear *)
check["DiagramIntegrand: the loop frequencies LoopFrequency[i, statistics]", "LoopFrequencies" /. First[DiagramIntegrand[
     OrderPart[densityCorrelation, InteractionPotential, 0], bubbleExternals, gasRules]], {LoopFrequency[1, "Fermion"]}];
check["no symbol of the sums in the results", Cases[{lindhard, sigma2, evaluated3, greenFunction2Evaluated},
   s_Symbol /; StringLength[SymbolName[s]] > 13 && StringTake[SymbolName[s], 13] === "loopFrequency", Infinity], {}];
(* an explicit dispersion with a constant: the constant does not decide the sign of the argument of n_F *)
check["a dispersion p^2/2 - mu: n_F(p^2/2 - mu)", With[{r = EvaluateMatsubara[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint,
     {DressedGreenFunction -> {"Fermion", Function[s, s^2/2 - mu]}, InteractionPotential -> {"Instantaneous", v}}, "LoopMomenta" -> {p}]},
   {! FreeQ[r, FermiFunction[p^2/2 - mu]], FreeQ[r, FermiFunction[mu - p^2/2]]}], {True, True}];
(* a boson line that does not vanish at large frequency: the Fock term has no convergence factor, or diverges *)
check["a boson line that tends to a constant: a message of EvaluateMatsubara", With[{fock = OrderPart[selfEnergy, InteractionPotential, 1],
    constantLine = {DressedGreenFunction -> {"Fermion", xi}, InteractionPotential -> {"Boson", Function[{s, zz}, v[s]]}}},
   {messageGivenQ[EvaluateMatsubara[fock, twoPoint, constantLine, "LoopMomenta" -> {p}], EvaluateMatsubara::conv],
    Quiet[messageGivenQ[EvaluateMatsubara[fock, twoPoint, constantLine, "LoopMomenta" -> {p}], MatsubaraSum::conv], EvaluateMatsubara::conv],
    Quiet[EvaluateMatsubara[fock, twoPoint, {DressedGreenFunction -> {"Fermion", xi}, InteractionPotential -> {"Boson", Function[{s, zz}, zz^2 v[s]]}},
      "LoopMomenta" -> {p}], EvaluateMatsubara::diverge]}], {True, False, $Failed}];
check["EvaluateMatsubara, invalid input: messages and $Failed", {
   Quiet[EvaluateMatsubara[selfEnergy2, twoPoint, gasRules, "LoopMomenta" -> {p}], EvaluateMatsubara::loops],
   Quiet[EvaluateMatsubara[selfEnergy2, twoPoint, gasRules, "LoopMomenta" -> p], EvaluateMatsubara::loopnames],
   Quiet[EvaluateMatsubara[selfEnergy2, {y -> k, x -> -k}, gasRules], EvaluateMatsubara::external],
   Quiet[EvaluateMatsubara[selfEnergy2, {y -> {k, w}, x -> {-k, -w}}, gasRules], EvaluateMatsubara::external],
   messageGivenQ[EvaluateMatsubara[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, "Spin" -> 2], FunctionalDiagrams::optx],
   messageGivenQ[DiagramIntegrand[OrderPart[selfEnergy, InteractionPotential, 1], twoPoint, gasRules, "Spin" -> 2], FunctionalDiagrams::optx]},
  {$Failed, $Failed, $Failed, $Failed, True, True}];
check["a term that fails: $Failed for the whole sum", {
   Quiet[EvaluateMatsubara[BareGreenFunction[x, y] + f[x, y], twoPoint, gasRules], EvaluateMatsubara::factor],
   Quiet[EvaluateMatsubara[selfEnergy2, twoPoint, {DressedGreenFunction -> {"Fermion", xi}}], EvaluateMatsubara::factor],
   Quiet[CuttingRuleImaginaryPart[selfEnergy2 + f[x, y], twoPoint, gasRules, w -> omega], EvaluateMatsubara::factor]}, {$Failed, $Failed, $Failed}];
check["cutting rules for a frequency that does not cross the diagram: a message and $Failed", {
   messageGivenQ[CuttingRuleImaginaryPart[selfEnergy2, twoPoint, gasRules, nu -> omega], CuttingRuleImaginaryPart::frequency],
   Quiet[CheckCuttingRules[selfEnergy2, twoPoint, gasRules, w5 -> omega], CuttingRuleImaginaryPart::frequency],
   Quiet[CheckCuttingRules[OrderPart[luttingerWard, InteractionPotential, 2], {}, gasRules, w -> omega], CuttingRuleImaginaryPart::frequency]},
  {True, $Failed, $Failed}];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: electron-phonon (Migdal)"];
DeclareSource[Jphonon]; DeclareField[phonon, Jphonon]; DeclareSymmetric[PhononPropagator]; DeclareLineStyle[PhononPropagator, "Dashed"];
FreeElectronPhononGeneratingFunctional = FreeFermionGeneratingFunctional Exp[1/2 IntegralOver[Jphonon[x] PhononPropagator[x, y] Jphonon[y], {x, y}]];
ElectronPhononVertex = g IntegralOver[phonon[z] psibar[z] ** psi[z], {z}];
phononGreen = -PerturbativeCorrelationFunction[FreeElectronPhononGeneratingFunctional, {psi[x], psibar[y]}, ElectronPhononVertex, 2];
phononSelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[OrderPart[phononGreen, g, 2], 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}];
(* <phi phi> of Exp[1/2 J D J] is minus the textbook D0 = 2 Omega/((i nu)^2 - Omega^2) *)
phononRules = {BareGreenFunction -> {"Fermion", xi}, PhononPropagator -> {"Boson", Function[{s, zz}, 2 Om[s]/(Om[s]^2 - zz^2)]}};
migdal = EvaluateMatsubara[phononSelfEnergy, twoPoint, phononRules, "LoopMomenta" -> {p}];
check["Migdal self-energy: textbook statistical factors", sameThermalQ[withoutMomentumSum[migdal], -2 g^2 FermiFunction[xi[p]]/Om[0] +
    g^2 ((BoseFunction[Om[k - p]] + 1 - FermiFunction[xi[p]])/(I w - xi[p] - Om[k - p]) + (BoseFunction[Om[k - p]] + FermiFunction[xi[p]])/(I w - xi[p] + Om[k - p]))], True];
check["Migdal self-energy: textbook form", migdal, g^2 MomentumSum[(1 + BoseFunction[Om[k - p]] - FermiFunction[xi[p]])/(I w - Om[k - p] - xi[p]) +
     (BoseFunction[Om[k - p]] + FermiFunction[xi[p]])/(I w + Om[k - p] - xi[p]), {p}] - 2 g^2 MomentumSum[FermiFunction[xi[p]], {p}]/Om[0]];
check["Migdal self-energy: cutting rules", CheckCuttingRules[phononSelfEnergy, twoPoint, phononRules, w -> omega], 0];
phononValues = Join[gasValues, {Om -> Function[s, 0.5 + 0.2 s^2], g -> 0.7}];
checkSmall["Migdal self-energy: brute force", Max[bruteForceErrors[phononSelfEnergy, twoPoint, phononRules, phononValues, bruteForceFrequencies]],
  bruteForceTolerance];
check["Migdal self-energy at T = 0", Module[{s = withoutMomentumSum[ZeroTemperatureLimit[migdal]] /. {xi[p] -> 0.3, Om[k - p] -> 0.5, Om[0] -> 0.5, w -> 1.7}},
   Abs[s - g^2 (1/(1.7 I - 0.3 - 0.5) + 0/(1.7 I - 0.3 + 0.5))] < 10^-12 /. g -> 0.7], True];
(* a boson with two modes, a rational function of degree four in the frequency *)
twoModeRules = {BareGreenFunction -> {"Fermion", xi}, PhononPropagator -> {"Boson", Function[{s, zz}, 2 Om[s]/(Om[s]^2 - zz^2) Om2[s]^2/(Om2[s]^2 - zz^2)]}};
twoModeValues = Join[phononValues, {Om2 -> Function[s, 1.3 + 0.1 s^2]}];
check["two-mode boson: cutting rules", CheckCuttingRules[phononSelfEnergy, twoPoint, twoModeRules, w -> omega], 0];
checkSmall["two-mode boson: brute force", Max[bruteForceErrors[phononSelfEnergy, twoPoint, twoModeRules, twoModeValues, bruteForceFrequencies]],
  bruteForceTolerance];
check["T -> 0 of the derivative of n_F", ZeroTemperatureLimit[Derivative[1][FermiFunction][e]], -EnergyDelta[e]];

(* ------------------------------------------------------------------------- *)
section["Matsubara sums: LaTeX, messages"];
(* the package's <rho rho>_c is -chi_0: its imaginary part is +2 Pi Sum (n_p - n_{p+q}) delta(...) *)
check["LaTeX of n_F''", ExpressionToLaTeX[MatsubaraSum[1/(z - a)^3, z, "Fermion"]], "\\frac{1}{2}\\,n_F^{\\prime\\prime}(a)"];
check["LaTeX of the Lindhard imaginary part", ExpressionToLaTeX[RetardedImaginaryPart[AnalyticContinuation[lindhard, nu -> omega], omega]],
  "2\\,\\pi\\,\\sum_{p}\\delta(\\omega + \\xi(p) - \\xi(p + q))\\,\\left(n_F(\\xi(p)) - n_F(\\xi(p + q))\\right)"];
check["LaTeX: named subexpressions and subscripts", ExpressionToLaTeX[FermiFunction[xi[p1]] v[k - p1]/(I w - xi[p1]) + UnitStep[-xi[p1]],
   "Names" -> {FermiFunction[xi[p1]] -> "n_1", xi[p1] -> "\\xi_1", w -> "\\omega_n"}, "Subscripts" -> {v}],
  "\\theta(-\\xi_1) + \\frac{n_1\\,v_{k - p_{1}}}{i\\,\\omega_n - \\xi_1}"];
check["LaTeX: a factor outside a momentum sum comes first", ExpressionToLaTeX[2 v[0] MomentumSum[FermiFunction[xi[p]], {p}]],
  "2\\,v(0)\\,\\sum_{p}n_F(\\xi(p))"];
check["a rule that is not a line rule: a message", Quiet[Check[EvaluateMatsubara[selfEnergy2, twoPoint, {BareGreenFunction -> xi}]; "no message",
    "message"]], "message"];
ResetDeclarations[];
check["ResetDeclarations removes the Matsubara frequencies", Quiet[Check[AnalyticContinuation[MatsubaraSum[1/((z - a) (z + I nu - b)), z,
      "Fermion"], nu -> omega]; "no message", "message"]], "message"];

(* ------------------------------------------------------------------------- *)
(* Dimensional regularization (section 18 of the package). Series and NIntegrate are used here only, as independent
   references. kappa = 1/(16 pi^2) is the factor of one loop. *)
checkSimplified[name_String, expr_] := check[name, Simplify[expr], 0];
(* relative error of value against expected (numbers or lists of numbers: the largest) *)
checkNumeric[name_String, value_, expected_, tolerance_] :=
  With[{err = Max[Abs[Flatten[{N[value] - N[expected]}]]]/Max[1, Max[Abs[Flatten[{N[expected]}]]]]},
   If[NumberQ[err] && err < tolerance, passed++; Print["PASS ", name, "   [error ", scientific[err], "]"],
    failed++; AppendTo[failures, name]; Print["FAIL ", name, "\n     got:      ", N[value], "\n     expected: ", N[expected]]]];
mu2 = RenormalizationScale^2;
kappa = 1/(16 Pi^2);
(* the plain integral Int d^dk/(2 pi)^d from the MS-bar measure of one loop *)
plainMeasure[eps_] := (4 Pi)^eps Exp[-eps EulerGamma] (RenormalizationScale^2)^(-eps);
(* the tadpoles A(a; m) *)
tadpole[a_, m_] := TadpoleIntegral[a, m, eps];

section["dimensional regularization: integrands of the diagrams"];
DeclareSource[J]; DeclareSymmetric[FreePropagator]; DeclareField[phi, J];
Zscalar = Exp[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}]];
phi4Interaction = lambda/24 IntegralOver[phi[z]^4, {z}];
prop = PerturbativeCorrelationFunction[Zscalar, {phi[a], phi[b]}, phi4Interaction, 2];
phi4SelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[prop, 1, {a, b}, {FreePropagator}], {a, b}, {FreePropagator}];
sunset = OrderPart[phi4SelfEnergy, lambda, 2] /. DeltaFunction[__] -> 0;
massless = {FreePropagator -> {"Scalar", 0}};
check["sunset of Sigma as a loop integral", DiagramLoopIntegral[sunset, {a -> p, b -> -p}, {FreePropagator -> {"Scalar", m}}, "LoopMomenta" -> {k, l}],
  lambda^2/6 LoopIntegral[ScalarPropagator[k, m] ScalarPropagator[l, m] ScalarPropagator[p + k + l, m], {k, l}]];
check["tadpole of Sigma as a loop integral", DiagramLoopIntegral[OrderPart[phi4SelfEnergy, lambda, 1], {a -> p, b -> -p}, {FreePropagator -> {"Scalar", m}}],
  -lambda/2 LoopIntegral[ScalarPropagator[LoopMomentum[1], m], {LoopMomentum[1]}]];
check["a tree has no loop integral", DiagramLoopIntegral[lambda DeltaFunction[a, b] DeltaFunction[a, c] DeltaFunction[a, d],
   {a -> p, b -> -p, c -> 0, d -> 0}, {FreePropagator -> {"Scalar", 0}}], lambda];
check["a kernel line (counterterm insertion)", DiagramLoopIntegral[IntegralOver[FreePropagator[a, u] Q[u, v] FreePropagator[v, a], {u, v}],
   {a -> 0}, {FreePropagator -> {"Scalar", m}, Q -> {"Kernel", Function[q, c2 MomentumSquared[q] + c0]}}, "LoopMomenta" -> {k}],
  LoopIntegral[ScalarPropagator[k, m]^2 (c0 + c2 k^2), {k}]];
check["a kernel line q^2 in the numerator: reduced to tadpoles", Simplify[IntegrateLoops[LoopIntegral[ScalarPropagator[k, m]^2 (c0 + c2 k^2), {k}], eps] -
    (c0 tadpole[2, m] + c2 (tadpole[1, m] - m^2 tadpole[2, m]))], 0];
check["a massless numerator cancels a line (scaleless: 0) or gives G(-1, 1) = 0", {IntegrateLoops[LoopIntegral[MomentumSquared[k] ScalarPropagator[k, 0]^2 ScalarPropagator[k + p, 0], {k}], eps] -
     BubbleIntegral[{1, 1}, p^2, {0, 0}, eps], IntegrateLoops[LoopIntegral[MomentumSquared[k] ScalarPropagator[k + p, 0], {k}], eps]}, {0, 0}];
(* numerators next to a massive line of another momentum: shifted and reduced to tadpoles (the k.p terms vanish) *)
check["(k + p)^2/(k^2 + m^2)^2 = A(1) - m^2 A(2) + p^2 A(2)", Simplify[IntegrateLoops[LoopIntegral[ScalarPropagator[k, m]^2 MomentumSquared[k + p], {k}], eps] -
    (tadpole[1, m] - m^2 tadpole[2, m] + p^2 tadpole[2, m])], 0];
check["... its pole (p^2 - 2 m^2)/eps", Simplify[PolePart[IntegrateLoops[LoopIntegral[ScalarPropagator[k, m]^2 MomentumSquared[k + p], {k}], eps], eps]/kappa], (p^2 - 2 m^2)/eps];
check["k^2/((k + p)^2 + m^2) = (p^2 - m^2) A(1)", Simplify[IntegrateLoops[LoopIntegral[MomentumSquared[k] ScalarPropagator[k + p, m], {k}], eps] -
    (p^2 - m^2) tadpole[1, m]], 0];
check["... the same from a diagram with a kernel line q^2", Simplify[IntegrateLoops[DiagramLoopIntegral[FreePropagator[a, b] Q[b, a], {a -> p, b -> -p},
      {FreePropagator -> {"Scalar", m}, Q -> {"Kernel", Function[q, MomentumSquared[q]]}}], eps] - (p^2 - m^2) tadpole[1, m]], 0];
(* ((k+p)^2)^2 = k^4 + 4 (k.p)^2 + p^4 + 2 k^2 p^2 + odd terms, <(k.p)^2> = k^2 p^2/d, d = 4 - 2 eps *)
check["((k + p)^2)^2/(k^2 + m^2)^3: the angular average of (k.p)^2", Simplify[
   IntegrateLoops[LoopIntegral[ScalarPropagator[k, m]^3 MomentumSquared[k + p]^2, {k}], eps] -
    ((tadpole[1, m] - 2 m^2 tadpole[2, m] + m^4 tadpole[3, m]) + (2 + 4/(4 - 2 eps)) p^2 (tadpole[2, m] - m^2 tadpole[3, m]) + p^4 tadpole[3, m])], 0];
check["BubbleIntegral with a power 0 or -1 (a numerator) is reduced", Simplify[{BubbleIntegral[{2, -1}, p^2, {m, 0}, eps] -
     (tadpole[1, m] - m^2 tadpole[2, m] + p^2 tadpole[2, m]), BubbleIntegral[{3, 0}, p^2, {m, M}, eps] - tadpole[3, m],
    BubbleIntegral[{-1, 2}, 0, {m1, m2}, eps] - (tadpole[1, m2] + (m1^2 - m2^2) tadpole[2, m2])}], {0, 0, 0}];
(* the general reduction in d = 3.3 against a radial integral, the angular average done by hand: with A = k^2 + p^2 + mn^2,
   B = 2 k p, <(A + B cos)^2> = A^2 + B^2/d *)
With[{d = 33/10, pv = 7/10, mn = 1/2, md = 6/5}, With[{eps = (4 - d)/2},
   checkNumeric["((k + p)^2 + mn^2)^2/(k^2 + md^2)^4 in d = 3.3: reduction vs radial integral",
    plainMeasure[eps] BubbleIntegral[{4, -2}, pv^2, {md, mn}, eps],
    2 Pi^(d/2)/(Gamma[d/2] (2 Pi)^d) NIntegrate[k^(d - 1) ((k^2 + pv^2 + mn^2)^2 + 4 k^2 pv^2/d)/(k^2 + md^2)^4, {k, 0, Infinity},
       WorkingPrecision -> 25, PrecisionGoal -> 15], 10^-12]]];
check["a rule that is not a line rule: a message", messageGivenQ[DiagramLoopIntegral[sunset, {a -> p, b -> -p}, {FreePropagator -> {"Fermion", m}}],
   DiagramLoopIntegral::rule], True];
check["momentum squared of a combination", {MomentumSquared[-p], MomentumSquared[-p - q], MomentumSquared[0], MomentumSquared[2 p], MomentumSquared[-3 p]},
  {p^2, MomentumSquared[p + q], 0, 4 p^2, 9 p^2}];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: messages of the routing"];
check["external momenta that do not add up to zero", messageGivenQ[DiagramLoopIntegral[FreePropagator[a, b]^2, {a -> p, b -> q}, massless],
   DiagramLoopIntegral::conservation], True];
check["too few loop names", messageGivenQ[DiagramLoopIntegral[FreePropagator[a, b]^3, {a -> p, b -> -p}, massless, "LoopMomenta" -> {k}],
   DiagramLoopIntegral::loops], True];
check["a factor that is not a line", messageGivenQ[DiagramLoopIntegral[FreePropagator[a, b]^2 Q[a, b], {a -> p, b -> -p}, massless],
   DiagramLoopIntegral::factor], True];
check["loop names that are not a list", messageGivenQ[DiagramLoopIntegral[FreePropagator[a, b]^2, {a -> p, b -> -p}, massless, "LoopMomenta" -> k],
   DiagramLoopIntegral::loopnames], True];
check["inconsistent poles in AnomalousDimensionFromZ: its own message", messageGivenQ[AnomalousDimensionFromZ[1 + g/eps + 3 g^2/eps^2, g, eps, -2 eps g, 2],
   AnomalousDimensionFromZ::pole], True];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: one-loop integrals"];
check["massive tadpole, Laurent expansion", Simplify[EpsilonExpand[tadpole[1, m], eps, 0] - kappa m^2 (-1/eps - 1 + Log[m^2/mu2])], 0];
check["massless bubble: 1/eps + 2 - ln(p^2/mu^2)", Simplify[EpsilonExpand[BubbleIntegral[{1, 1}, p^2, {0, 0}, eps], eps, 0] - kappa (1/eps + 2 - Log[p^2/mu2])], 0];
check["massless bubble in d = 3: 1/(8 |p|)", Simplify[BubbleIntegral[{1, 1}, 9, {0, 0}, 1/2] plainMeasure[1/2]], 1/24];
check["bubble at p = 0 with equal masses: 1/eps - ln(m^2/mu^2)", Simplify[EpsilonExpand[BubbleIntegral[{1, 1}, 0, {m, m}, eps], eps, 0] - kappa (1/eps - Log[m^2/mu2])], 0];
check["massless line at p = 0: 1/eps + 1 - ln(m^2/mu^2)", Simplify[EpsilonExpand[BubbleIntegral[{1, 1}, 0, {0, m}, eps], eps, 0] - kappa (1/eps + 1 - Log[m^2/mu2])], 0];
check["scaleless integrals vanish", {BubbleIntegral[{1, 2}, 0, {0, 0}, eps], TadpoleIntegral[2, 0, eps], TadpoleIntegral[-1, m, eps],
   Quiet[IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0]^2, {k}], eps]]}, {0, 0, 0, 0}];
(* the Laurent expansion against Mathematica's Series (independent); integer powers 3 and 4 of factors with a pole. A
   third element is the reference stored: Collect[FunctionExpand[Normal[Series[e, {eps, 0, n}]]], eps, Simplify],
   computed once (Mathematica 15.0.1; these six Series take 0.4-1.8 s each, 6 s in all); the others are expanded here *)
seriesReference[e_, n_] := Normal[Series[e, {eps, 0, n}]];
bubbleNormalized = 16 Pi^2 BubbleIntegral[{1, 1}, p^2, {0, 0}, eps];
Do[With[{e = t[[1]], n = t[[2]]}, checkSimplified["EpsilonExpand = Series to eps^" <> ToString[n] <> ": " <> ToString[InputForm[e]],
    FunctionExpand[PowerExpand[EpsilonExpand[e, eps, n] - If[Length[t] == 3, t[[3]], seriesReference[e, n]]]]]],
  {t, {{Gamma[eps] Gamma[1 - eps]^2/Gamma[2 - 2 eps], 2,
      2 + eps^(-1) - EulerGamma + eps (4 - 2 EulerGamma + EulerGamma^2/2 - Pi^2/12) +
       eps^2 (8 + EulerGamma^2 - EulerGamma^3/6 - Pi^2/6 + (EulerGamma (-48 + Pi^2))/12 - (7 Zeta[3])/3)},
    {Gamma[-1 + 2 eps] Gamma[1 - eps]^3/Gamma[3 - 3 eps], 2,
      -1/4 1/eps + (-13 + 4 EulerGamma)/8 + (eps (-345 + 156 EulerGamma - 24 EulerGamma^2 + 2 Pi^2))/48 +
       (eps^2 (-2595 - 312 EulerGamma^2 + 32 EulerGamma^3 + 26 Pi^2 + EulerGamma (1380 - 8 Pi^2) + 256 Zeta[3]))/96},
    {Exp[2 eps EulerGamma] Gamma[2 eps] Gamma[eps]/(eps^2 Gamma[1 + eps]), 2,
      1/(2 eps^4) + Pi^2/(6 eps^2) + Pi^4/20 - (4 Zeta[3])/(3 eps) + eps^2 ((61 Pi^6)/3780 + (16 Zeta[3]^2)/9) - (4 eps (5 Pi^2 Zeta[3] + 36 Zeta[5]))/45},
    {(1 - 2 eps)^-3 Gamma[1/2 + eps]/(3 + eps), 2}, {Gamma[-2 + eps], 2},
    {Gamma[eps]^3, 0}, {Gamma[eps]^4, 0}, {Gamma[1 + eps]/eps^3, -1}, {1/(eps^3 (1 + 2 eps)^2), 1}, {(Exp[eps EulerGamma] Gamma[1 + eps] - 1)/eps^3, 0},
    {Gamma[eps]^2, 1}, {(1 + 2 eps)^-3, 2},
    {Gamma[-1 + eps]^3 Gamma[1 - eps]^2/Gamma[2 - 3 eps], 0,
      -eps^(-3) + (-6 + 4 EulerGamma)/eps^2 + (-24 + 24 EulerGamma - 8 EulerGamma^2 + Pi^2/3)/eps +
       (2 (-123 - 72 EulerGamma^2 + 16 EulerGamma^3 + 3 Pi^2 - 2 EulerGamma (-72 + Pi^2) + 14 Zeta[3]))/3},
    {bubbleNormalized^3, 0,
      eps^(-3) + (6 - 3 Log[p^2] + 3 Log[mu2])/eps^2 + (24 - Pi^2/4 + (9 Log[p^2]^2)/2 + 18 Log[mu2] + (9 Log[mu2]^2)/2 - 9 Log[p^2] (2 + Log[mu2]))/eps +
       (320 - 6 Pi^2 - 18 Log[p^2]^3 - 3 (-96 + Pi^2) Log[mu2] + 108 Log[mu2]^2 + 18 Log[mu2]^3 + 54 Log[p^2]^2 (2 + Log[mu2]) +
         3 Log[p^2] (-96 + Pi^2 - 72 Log[mu2] - 18 Log[mu2]^2) - 28 Zeta[3])/4},
    {bubbleNormalized^4, -1,
      eps^(-4) + (8 - 4 Log[p^2] + 4 Log[mu2])/eps^3 + (40 - Pi^2/3 + 8 Log[p^2]^2 + 32 Log[mu2] + 8 Log[mu2]^2 - 16 Log[p^2] (2 + Log[mu2]))/eps^2 +
       (4 (120 - 2 Pi^2 - 8 Log[p^2]^3 - (-120 + Pi^2) Log[mu2] + 48 Log[mu2]^2 + 8 Log[mu2]^3 + 24 Log[p^2]^2 (2 + Log[mu2]) +
         Log[p^2] (-120 + Pi^2 - 96 Log[mu2] - 24 Log[mu2]^2) - 7 Zeta[3]))/(3 eps)}}}];
(* the cube of the massless bubble (the chain of three bubbles in Gamma4 at lambda^4): its pole part by hand,
   B = 1/eps + 2 - L + (4 - pi^2/12 - 2 L + L^2/2) eps + O(eps^2), L = ln(p^2/mu^2) *)
With[{bseries = 1/eps + 2 - Lp + (4 - Pi^2/12 - 2 Lp + Lp^2/2) eps},
  check["pole part of the cube of the massless bubble", Simplify[(PolePart[bubbleNormalized^3, eps] /. Log[p^2/mu2] -> Lp) -
     Sum[Coefficient[Expand[bseries^3], eps, -j] eps^-j, {j, 1, 3}]], 0]];
checkMessage[EpsilonExpand[Sin[eps]/eps, eps, 0], "EpsilonExpand of an unknown function: a message, $Failed", EpsilonExpand::function, $Failed];
massiveSunset = LoopIntegral[ScalarPropagator[k, m] ScalarPropagator[l, m] ScalarPropagator[p + k + l, m], {k, l}];
check["EpsilonExpand and PolePart of an unevaluated LoopIntegral: a message, $Failed", {messageGivenQ[PolePart[massiveSunset, eps], EpsilonExpand::loop],
   Quiet[EpsilonExpand[massiveSunset, eps, 0]]}, {True, $Failed}];
checkMessage[EpsilonExpand[BubbleIntegral[{1, 1}, 5, {1, 2}, delta], eps, 0], "a bubble in another variable: a message, $Failed",
  EpsilonExpand::variable, $Failed];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: massive bubbles"];
fp[a_, b_, m1_, m2_, p2_, f_] := Quiet[NIntegrate[x^(a - 1) (1 - x)^(b - 1) f[x (1 - x) p2 + x m1^2 + (1 - x) m2^2], {x, 0, 1}, WorkingPrecision -> 30], {NIntegrate::izero}];
cases = Select[Tuples[{{0, 1, 2}, {0, 1, 3/2}, {0, 1/3, 5}}], ! (#[[1]] == 0 && #[[2]] == 0 && #[[3]] == 0) &];
errors11 = Table[With[{m1 = c[[1]], m2 = c[[2]], p2 = c[[3]]},
     Abs[N[Coefficient[EpsilonExpand[BubbleIntegral[{1, 1}, p2, {m1, m2}, eps], eps, 0], eps, 0]/kappa /. RenormalizationScale -> 1, 30] +
       fp[1, 1, m1, m2, p2, Log]]], {c, cases}];
checkNumeric["B(1,1) finite part, 26 cases of masses and momenta (largest error)", Max[errors11], 0, 10^-15];
check["B(1,1) pole is 1/eps for all masses", Union[Table[Coefficient[EpsilonExpand[BubbleIntegral[{1, 1}, c[[3]], {c[[1]], c[[2]]}, eps], eps, 0], eps, -1]/kappa, {c, cases}]], {1}];
(* B(2,1) is finite unless its first line (power 2) is massless, B(1,2) unless its second line is: 18 cases each *)
cases21 = Select[cases, #[[1]] =!= 0 &];
cases12 = Select[cases, #[[2]] =!= 0 &];
errors21 = Table[With[{m1 = c[[1]], m2 = c[[2]], p2 = c[[3]]},
     Abs[N[EpsilonExpand[BubbleIntegral[{2, 1}, p2, {m1, m2}, eps], eps, 0]/kappa, 30] - fp[2, 1, m1, m2, p2, 1/# &]]], {c, cases21}];
  errors12 = Table[With[{m1 = c[[1]], m2 = c[[2]], p2 = c[[3]]},
     Abs[N[EpsilonExpand[BubbleIntegral[{1, 2}, p2, {m1, m2}, eps], eps, 0]/kappa, 30] - fp[1, 2, m1, m2, p2, 1/# &]]], {c, cases12}];
checkNumeric["B(2,1) (finite) in closed form, 18 cases, 6 with a massless line (largest error)", Max[errors21], 0, 10^-15];
checkNumeric["B(1,2) (finite) in closed form, 18 cases, 6 with a massless line (largest error)", Max[errors12], 0, 10^-15];
check["B(2,1) and B(1,2) with one massless line and p^2 != 0: closed forms (no FeynmanParameterIntegral)",
  FreeQ[{EpsilonExpand[BubbleIntegral[{2, 1}, 5, {1, 0}, eps], eps, 0], EpsilonExpand[BubbleIntegral[{1, 2}, 1/3, {0, 3/2}, eps], eps, 0]}, FeynmanParameterIntegral], True];
check["... B(2,1) with m2 = 0 is ln((p^2 + m1^2)/m1^2)/p^2", Table[Simplify[EpsilonExpand[BubbleIntegral[{2, 1}, v[[1]], {v[[2]], 0}, eps], eps, 0]/kappa -
     Log[(v[[1]] + v[[2]]^2)/v[[2]]^2]/v[[1]]], {v, {{5, 1}, {1/3, 3/2}}}], {0, 0}];
check["an infrared-divergent bubble (massless line of power 2): a message, $Failed", {messageGivenQ[EpsilonExpand[BubbleIntegral[{2, 1}, 5, {0, 1}, eps], eps, 0],
    EpsilonExpand::bubble], Quiet[EpsilonExpand[BubbleIntegral[{1, 2}, 5, {1, 0}, eps], eps, 0]]}, {True, $Failed}];
checkSimplified["equal masses: the ArcTanh form", Coefficient[EpsilonExpand[BubbleIntegral[{1, 1}, p2, {m, m}, eps], eps, 0], eps, 0]/kappa -
  (2 - Log[m^2/mu2] - 2 Sqrt[1 + 4 m^2/p2] ArcTanh[1/Sqrt[1 + 4 m^2/p2]])];
With[{e = EpsilonExpand[BubbleIntegral[{1, 1}, 5, {1, 3/2}, eps], eps, 1]},
  check["order eps of the massive bubble: an inert FeynmanParameterIntegral of FeynmanParameter", Union[Cases[e, FeynmanParameterIntegral[_, y_] :> y, Infinity]], {FeynmanParameter}];
  checkNumeric["... whose numerical value is the eps term of the parameter integral", N[Coefficient[e, eps, 1]/kappa /. RenormalizationScale -> 1],
   fp[1, 1, 1, 3/2, 5, Log[#]^2/2 &] + Pi^2/12, 10^-8];
  check["... and N[., 30] has 30 digits", Precision[N[Cases[e, _FeynmanParameterIntegral, Infinity][[1]] /. RenormalizationScale -> 1, 30]], 30.]];
(* the exact Feynman-parameter form against the Laurent expansion at a small eps (exactness of the closed forms in d) *)
With[{epsv = 1/1000, m1 = 1, m2 = 3/2, p2 = 5},
  checkNumeric["massive bubble, exact parameter integral at eps = 10^-3 vs expansion to eps^1",
   N[Exp[epsv EulerGamma] Gamma[epsv] NIntegrate[(x (1 - x) p2 + x m1^2 + (1 - x) m2^2)^-epsv, {x, 0, 1}, WorkingPrecision -> 30], 20],
   N[EpsilonExpand[BubbleIntegral[{1, 1}, p2, {m1, m2}, eps], eps, 1]/kappa /. RenormalizationScale -> 1 /. eps -> epsv, 20], 10^-6]];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: tadpoles in non-integer d"];
(* Int d^dk/(2 pi)^d 1/(k^2 + m^2)^a by radial integration where it converges *)
radial[d_, f_] := 2 Pi^(d/2)/Gamma[d/2]/(2 Pi)^d NIntegrate[k^(d - 1) f[k], {k, 0, Infinity}, WorkingPrecision -> 25, MaxRecursion -> 20];
With[{d = 33/10, m = 7/10}, With[{eps = (4 - d)/2},
   checkNumeric["A(2; m) in d = 3.3: closed form vs radial integral", TadpoleIntegral[2, m, eps] plainMeasure[eps], radial[d, 1/(#^2 + m^2)^2 &], 10^-12]]];
With[{d = 13/10, m = 7/10}, With[{eps = (4 - d)/2},
   checkNumeric["A(1; m) in d = 1.3: closed form vs radial integral", TadpoleIntegral[1, m, eps] plainMeasure[eps], radial[d, 1/(#^2 + m^2) &], 10^-12]]];
With[{epsv = 1/1000, m = 7/10},
  checkNumeric["A(1; m): expansion to eps^2 vs the exact Gamma function at eps = 10^-3", N[EpsilonExpand[tadpole[1, m], eps, 2] /. RenormalizationScale -> 1 /. eps -> epsv, 25],
   N[TadpoleIntegral[1, m, epsv] /. RenormalizationScale -> 1, 25], 10^-9]];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: G-functions and the sunset"];
(* G(a, b) from its Feynman-parameter integral Gamma(a+b-d/2)/(Gamma(a) Gamma(b)) Int x^(d/2-b-1) (1-x)^(d/2-a-1) dx, numerically *)
gFeynman[a_, b_, d_] := Gamma[a + b - d/2]/(Gamma[a] Gamma[b]) NIntegrate[x^(d/2 - b - 1) (1 - x)^(d/2 - a - 1), {x, 0, 1}, WorkingPrecision -> 25];
Do[checkNumeric["G(" <> ToString[ab[[1]], InputForm] <> ", " <> ToString[ab[[2]], InputForm] <> ") at d = " <> ToString[ab[[3]], InputForm] <> " vs Feynman parameters",
   GFunction[ab[[1]], ab[[2]], (4 - ab[[3]])/2], gFeynman @@ ab, 10^-12], {ab, {{1, 1, 33/10}, {7/10, 13/10, 37/10}, {1, 6/5, 31/10}, {3/2, 1, 7/2}}}];
(* bubbles in d = 3.3 by direct integration over |k|, the angle done in closed form:
   Int_0^pi sin^(d-2) t (A + B cos t)^-1 dt = sqrt(pi) Gamma((d-1)/2)/Gamma(d/2) A^-1 2F1(1/2, 1; d/2; B^2/A^2), A = k^2 + p^2 + m2^2, B = 2 k p,
   so Int d^dk/(2 pi)^d f(k^2)/((k + p)^2 + m2^2) = 2 pi^(d/2)/(Gamma(d/2) (2 pi)^d) Int dk k^(d-1) f(k^2) 2F1(...)/A *)
angular[d_, k_, p_, m2_] := Hypergeometric2F1[1/2, 1, d/2, 4 k^2 p^2/(k^2 + p^2 + m2^2)^2]/(k^2 + p^2 + m2^2);
With[{d = 33/10, p = 1}, With[{eps = (4 - d)/2},
   checkNumeric["massless bubble in d = 3.3: G-function vs direct integration in |k|",
    BubbleIntegral[{1, 1}, p^2, {0, 0}, eps] plainMeasure[eps],
    2 Pi^(d/2)/(Gamma[d/2] (2 Pi)^d) NIntegrate[k^(d - 3) angular[d, k, p, 0], {k, 0, p, Infinity}, WorkingPrecision -> 20, PrecisionGoal -> 10], 10^-8]]];
With[{d = 33/10, p = 1, m1 = 1/2, m2 = 3/4}, With[{eps = (4 - d)/2},
   checkNumeric["massive bubble in d = 3.3: Feynman parameters vs direct integration in |k|",
    Gamma[2 - d/2] (4 Pi)^(-d/2) NIntegrate[(x (1 - x) p^2 + x m1^2 + (1 - x) m2^2)^(d/2 - 2), {x, 0, 1}, WorkingPrecision -> 20],
    2 Pi^(d/2)/(Gamma[d/2] (2 Pi)^d) NIntegrate[k^(d - 1)/(k^2 + m1^2) angular[d, k, p, m2], {k, 0, Infinity}, WorkingPrecision -> 20, PrecisionGoal -> 10], 10^-8]]];
(* the massless sunset, from IntegrateLoops (nested G-functions), against position space: the cube of the massless propagator
   Delta(x) = Gamma(d/2-1)/(4 pi^(d/2)) (x^2)^(1-d/2), Fourier-transformed with
   Int d^dx e^(ipx) (x^2)^(-al) = pi^(d/2) 2^(d - 2 al) Gamma(d/2 - al)/Gamma(al) (p^2)^(al - d/2) *)
sunsetLoops = IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0] ScalarPropagator[l, 0] ScalarPropagator[p + k + l, 0], {k, l}], eps];
sunsetPosition[d_] := With[{al = 3 (d/2 - 1)}, (Gamma[d/2 - 1]/(4 Pi^(d/2)))^3 Pi^(d/2) 2^(d - 2 al) Gamma[d/2 - al]/Gamma[al] (p^2)^(al - d/2)];
Do[checkNumeric["massless sunset at eps = " <> ToString[e, InputForm] <> ": nested G-functions vs position space",
   (sunsetLoops plainMeasure[eps]^2 /. eps -> e /. p -> 13/10), sunsetPosition[4 - 2 e] /. p -> 13/10, 10^-12], {e, {1/7, 0.3, -0.4}}];
check["massless sunset: -p^2/(4 eps) - 13 p^2/8 + p^2 ln(p^2/mu^2)/2", Simplify[EpsilonExpand[sunsetLoops, eps, 0] - kappa^2 p^2 (-1/(4 eps) - 13/8 + Log[p^2/mu2]/2)], 0];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: two-loop four-point diagrams"];
c4 = ConnectedCorrelationFunction[Zscalar, {phi[a], phi[b], phi[c], phi[d]}, phi4Interaction, 3];
gamma4 = -AmputateExternalLegs[ParticleIrreducibleDiagrams[c4, 1, {a, b, c, d}, {FreePropagator}], {a, b, c, d}, {FreePropagator}];
routed4 = InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps, "Routing" -> True];
check["Gamma4 to lambda^3: 16 terms, all evaluated", {Length[routed4], FreeQ[routed4, LoopIntegral | $Failed]}, {16, True}];
eyes = Select[routed4, Exponent[#[[1]], lambda] == 3 && Count[#[[1]], DeltaFunction[__], Infinity] == 1 &];
check["the 6 eye diagrams: p routed from the vertex with two legs", {Length[eyes], Union[Table[With[{dl = Cases[e[[1]], DeltaFunction[u_, v_] :> {u, v}, Infinity][[1]]},
      Length[Intersection[dl, e[[2]]]]], {e, eyes}]]}, {6, {1}}];
loopMeasure2 = Exp[2 eps EulerGamma] (RenormalizationScale^2)^(2 eps)/(16 Pi^2)^2;
check["eye = lambda^3/2 G(1,1) G(1+eps,1) (p^2)^(-2 eps)", Union[Table[Simplify[e[[3]] - lambda^3/2 loopMeasure2 GFunction[1, 1, eps] GFunction[1 + eps, 1, eps] (p^2)^(-2 eps)],
    {e, eyes}]], {0}];
check["eye integral: 1/(2 eps^2) + (5/2 - ln(p^2/mu^2))/eps", Simplify[PolePart[eyes[[1, 3]]/(lambda^3/2), eps] - kappa^2 (1/(2 eps^2) + (5/2 - Log[p^2/mu2])/eps)], 0];
check["tadpole insertions vanish (massless)", Union[Table[r[[3]], {r, Select[routed4, ! FreeQ[#[[1]], FreePropagator[z_, z_]] &]}]], {0}];
check["a routing is found by a change of loop momenta (the eye with the bubble on k1, k2)",
  Simplify[IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0] ScalarPropagator[p - k - l, 0] ScalarPropagator[l, 0] ScalarPropagator[k + l, 0], {k, l}], eps] -
    loopMeasure2 GFunction[1, 1, eps] GFunction[1 + eps, 1, eps] (p^2)^(-2 eps)], 0];
check["an integral that is not nested one-loop (massive sunset): a message, left unevaluated",
  {messageGivenQ[IntegrateLoops[massiveSunset, eps], IntegrateLoops::irreducible], Quiet[IntegrateLoops[massiveSunset, eps]]}, {True, massiveSunset}];
check["a numerator k.p: a message, left unevaluated", {messageGivenQ[IntegrateLoops[LoopIntegral[k p ScalarPropagator[k, 0] ScalarPropagator[k + p, 0], {k}], eps],
    IntegrateLoops::numerator], Quiet[IntegrateLoops[LoopIntegral[k p ScalarPropagator[k, 0] ScalarPropagator[k + p, 0], {k}], eps]]},
  {True, LoopIntegral[k p ScalarPropagator[k, 0] ScalarPropagator[k + p, 0], {k}]}];
check["the non-skeleton terms of Gamma4 are the tadpole insertions (PerturbativeFullVertex)",
  Length[DiagramTerms[SimplifyDiagrams[gamma4 - PerturbativeFullVertex[Zscalar, phi4Interaction, 3, {phi}, {a, b, c, d}, FreePropagator -> FreePropagator]]]], 3];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: infrared and scaleless divergences"];
zeroMomentumBubble = DiagramLoopIntegral[-1/2 lambda^2 DeltaFunction[a, d] DeltaFunction[b, c] FreePropagator[a, b]^2, {a -> 0, b -> 0, c -> 0, d -> 0}, massless];
check["a bubble at zero momenta: 0 with a message (its ultraviolet pole is lost)", {messageGivenQ[IntegrateLoops[zeroMomentumBubble, eps], IntegrateLoops::scaleless],
   Quiet[IntegrateLoops[zeroMomentumBubble, eps]]}, {True, 0}];
check["G(2, 1): a message for the infrared pole", messageGivenQ[IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0]^2 ScalarPropagator[k + p, 0], {k}], eps], IntegrateLoops::ir], True];
check["a massless tree line at zero momentum: a message, left as ScalarPropagator[0, 0]", {messageGivenQ[IntegrateLoops[DiagramLoopIntegral[FreePropagator[a, b],
      {a -> 0, b -> 0}, massless], eps], IntegrateLoops::zero], Quiet[IntegrateLoops[DiagramLoopIntegral[FreePropagator[a, b], {a -> 0, b -> 0}, massless], eps]]},
  {True, ScalarPropagator[0, 0]}];
check["a massless tadpole (power divergent) is 0 without a message", {messageGivenQ[IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0], {k}], eps], IntegrateLoops::scaleless],
   IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0], {k}], eps]}, {False, 0}];
check["no global flags", Names["FunctionalDiagrams`Private`$loopFlags"], {}];
term11 = DeltaFunction[a, d] DeltaFunction[b, c] IntegralOver[FreePropagator[a, u] Q[u, b], {u}] FreePropagator[a, b];
rules11 = {FreePropagator -> {"Scalar", 0}, Q -> {"Kernel", Function[q, c2 MomentumSquared[q] + c0]}};
check["a kernel c2 q^2 + c0: its q^2 part is a massless tadpole (0), its c0 part a bubble", Simplify[InfraredRearrangedIntegral[term11, {a, b, c, d}, p, rules11, eps] -
    c0 BubbleIntegral[{1, 1}, p^2, {0, 0}, eps]], 0];
logScaleless = lambda/2 DeltaFunction[a, b] IntegralOver[FreePropagator[a, w]^2, {w}];
check["a logarithmically divergent scaleless term with no routing: a message, $Failed",
  {messageGivenQ[InfraredRearrangedIntegral[logScaleless, {a, b}, p, massless, eps], InfraredRearrangedIntegral::norouting],
   Quiet[InfraredRearrangedIntegral[logScaleless, {a, b}, p, massless, eps]]}, {True, $Failed}];
check["... the same with w external: its pole lambda/(32 pi^2 eps)", PolePart[InfraredRearrangedIntegral[lambda/2 DeltaFunction[a, b] FreePropagator[a, w]^2,
    {a, b, w}, p, massless, eps], eps], lambda/(32 Pi^2 eps)];
check["a numerator k.p in a rearranged term: one message, $Failed", {messageGivenQ[InfraredRearrangedIntegral[term11 /. IntegralOver[__] -> Q[a, b], {a, b, c, d}, p,
     {FreePropagator -> {"Scalar", 0}, Q -> {"Kernel", Function[q, q]}}, eps], InfraredRearrangedIntegral::norouting],
   Quiet[messageGivenQ[InfraredRearrangedIntegral[term11 /. IntegralOver[__] -> Q[a, b], {a, b, c, d}, p, {FreePropagator -> {"Scalar", 0}, Q -> {"Kernel", Function[q, q]}}, eps],
    IntegrateLoops::numerator], InfraredRearrangedIntegral::norouting]}, {True, False}];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: mass insertion"];
sigma21 = -MassInsertion[phi4SelfEnergy, FreePropagator, w];
check["dSigma/dm^2 of the tadpole and of the sunset", OrderPart[sigma21, lambda, 1] /. DeltaFunction[a, b] -> 1, lambda/2 FreePropagator[a, w]^2];
check["dSigma/dm^2: 4 terms (sunset x 3 lines, double tadpole x 2 kinds)", Length[DiagramTerms[sigma21]], 4];
check["the m^2 derivative of the one-loop massive tadpole matches the insertion (pole)",
  Simplify[PolePart[-lambda/2 D[tadpole[1, m], m]/(2 m), eps] -
    PolePart[InfraredRearrangedIntegral[OrderPart[sigma21, lambda, 1], {a, b, w}, p, massless, eps], eps]], 0];
check["an insertion at a label of the diagrams: a message, $Failed", {messageGivenQ[MassInsertion[FreePropagator[a, b]^3, FreePropagator, a], MassInsertion::label],
   Quiet[MassInsertion[FreePropagator[a, b]^3, FreePropagator, a]]}, {True, $Failed}];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: MS-bar renormalization of phi^4 to two loops"];
(* phi4Renormalization[]: Z_phi, Z_m2, Z_lambda from the diagrams of the package, the results as rules: "Routed" (the evaluated terms of Sigma, Gamma4, dSigma/dm2 with their routings), "Solution"
   (the pole coefficients), "Residual" (poles left in Gamma2, Gamma4, Gamma(2,1)), "Zphi", "Z4", "Zlambda", "Zm2",
   "Beta", "GammaPhi", "GammaM2", "MassiveCheck" (pole of Z_m2 m^2 - Sigma at one loop, massive), "Terms" (numbers of
   terms of Sigma, Gamma4, dSigma/dm2) *)
phi4Renormalization[] := Module[{Zscalar, phi4Interaction, prop, phi4SelfEnergy, c4, gamma4, sigma21, massless, routed, sigmaB, gamma4B, sigma21B,
    kappa = 1/(16 Pi^2), zLambda, zPhi, zM2, truncate, renormalized2, renormalized4, renormalized21, poleEquations, solution = {},
    residual, zLambdaS, zPhiS, zM2S, z4S, beta, gammaPhi, gammaM2, sigmaMassive, massiveCheck, Lp},
   (* the unknowns zl1, zf1, zm1, zl22, zl21, zf22, zf21, zm22, zm21 are global symbols (they name the solution) *)
   DeclareSource[J]; DeclareSymmetric[FreePropagator]; DeclareField[phi, J];
   Zscalar = Exp[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}]];
   phi4Interaction = lambda/24 IntegralOver[phi[z]^4, {z}];
   (* the package's self-energy (G = G0 + G0 Sigma G0 + ...) and 1PI vertex Gamma4 = -(amputated 1PI part of the connected
      four-point function) *)
   prop = PerturbativeCorrelationFunction[Zscalar, {phi[a], phi[b]}, phi4Interaction, 2];
   phi4SelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[prop, 1, {a, b}, {FreePropagator}], {a, b}, {FreePropagator}];
   c4 = ConnectedCorrelationFunction[Zscalar, {phi[a], phi[b], phi[c], phi[d]}, phi4Interaction, 3];
   gamma4 = -AmputateExternalLegs[ParticleIrreducibleDiagrams[c4, 1, {a, b, c, d}, {FreePropagator}], {a, b, c, d}, {FreePropagator}];
   (* dSigma/dm^2 = -MassInsertion: the line G0 -> G0 G0 at the new external point w *)
   sigma21 = -MassInsertion[phi4SelfEnergy, FreePropagator, w];
   massless = {FreePropagator -> {"Scalar", 0}};
   routed = {"Sigma" -> InfraredRearrangedIntegral[phi4SelfEnergy, {a, b}, p, massless, eps, "Routing" -> True],
     "Gamma4" -> InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps, "Routing" -> True],
     "dSigma/dm2" -> InfraredRearrangedIntegral[sigma21, {a, b, w}, p, massless, eps, "Routing" -> True]};
   sigmaB = Total[("Sigma" /. routed)[[All, 3]]];
   gamma4B = Total[("Gamma4" /. routed)[[All, 3]]];
   sigma21B = Total[("dSigma/dm2" /. routed)[[All, 3]]];
   (* the renormalization constants, with unknown pole coefficients (units: kappa = 1/(16 pi^2) per loop) *)
   zLambda = 1 + lambda kappa zl1/eps + lambda^2 kappa^2 (zl22/eps^2 + zl21/eps);
   zPhi = 1 + lambda kappa zf1/eps + lambda^2 kappa^2 (zf22/eps^2 + zf21/eps);
   zM2 = 1 + lambda kappa zm1/eps + lambda^2 kappa^2 (zm22/eps^2 + zm21/eps);
   truncate[e_, n_] := Sum[Coefficient[e, lambda, k] lambda^k, {k, 0, n}];
   renormalized2 = truncate[Expand[zPhi (p^2 - (sigmaB /. lambda -> lambda zLambda))], 2];
   renormalized4 = truncate[Expand[zPhi^2 (gamma4B /. lambda -> lambda zLambda)], 3];
   renormalized21 = truncate[Expand[zM2 (1 - (sigma21B /. lambda -> lambda zLambda))], 2];
   (* the pole part at each order must vanish for every value of ln(p^2/mu^2): linear equations for the unknowns *)
   poleEquations[e_] := With[{poles = Expand[PolePart[e, eps] /. Log[p^2/RenormalizationScale^2] -> Lp]},
     Union[Flatten[Table[CoefficientList[Coefficient[poles, eps, -j], {Lp, p}], {j, 1, 4}]]]];
   Do[Module[{eqs},
     eqs = DeleteCases[Expand[Flatten[{poleEquations[Coefficient[renormalized2, lambda, k] /. solution],
          poleEquations[Coefficient[renormalized4, lambda, k + 1] /. solution],
          poleEquations[Coefficient[renormalized21, lambda, k] /. solution]}]], 0];
     solution = Join[solution, First[Solve[Thread[eqs == 0], Select[{zl1, zf1, zm1, zl22, zl21, zf22, zf21, zm22, zm21},
          ! FreeQ[eqs, #] &]]]]], {k, 1, 2}];
   residual = {Simplify[PolePart[renormalized2 /. solution, eps]], Simplify[PolePart[renormalized4 /. solution, eps]],
     Simplify[PolePart[renormalized21 /. solution, eps]]};
   zLambdaS = zLambda /. solution; zPhiS = zPhi /. solution; zM2S = zM2 /. solution;
   z4S = truncate[Expand[zLambdaS zPhiS^2], 2];
   beta = BetaFunctionFromZ[zLambdaS, lambda, eps, 3];
   gammaPhi = 1/2 AnomalousDimensionFromZ[zPhiS, lambda, eps, -2 eps lambda + beta, 2];
   gammaM2 = -AnomalousDimensionFromZ[zM2S/zPhiS, lambda, eps, -2 eps lambda + beta, 2];
   (* cross-check of the one-loop mass counterterm in the massive theory: Z_m2 m^2 - Sigma(m) finite *)
   sigmaMassive = IntegrateLoops[DiagramLoopIntegral[OrderPart[phi4SelfEnergy, lambda, 1], {a -> p, b -> -p}, {FreePropagator -> {"Scalar", m}}], eps];
   massiveCheck = Simplify[PolePart[Expand[(zM2S - 1) m^2 /. lambda^2 -> 0] - sigmaMassive, eps]];
   {"Routed" -> routed, "Solution" -> solution,
    "Residual" -> residual, "Zphi" -> zPhiS, "Z4" -> z4S, "Zlambda" -> zLambdaS, "Zm2" -> zM2S, "Beta" -> beta,
    "GammaPhi" -> gammaPhi, "GammaM2" -> gammaM2, "MassiveCheck" -> massiveCheck,
    "Terms" -> {Length[DiagramTerms[phi4SelfEnergy]], Length[DiagramTerms[gamma4]], Length[DiagramTerms[sigma21]]}}];
phi4Results = phi4Renormalization[];
check["no poles left in Gamma2, Gamma4, Gamma(2,1)", "Residual" /. phi4Results, {0, 0, 0}];
check["Z_phi", Simplify[("Zphi" /. phi4Results) - (1 - lambda^2 kappa^2/(24 eps))], 0];
check["Z_4 (vertex)", Simplify[("Z4" /. phi4Results) - (1 + 3 lambda kappa/(2 eps) + lambda^2 kappa^2 (9/(4 eps^2) - 3/(2 eps)))], 0];
check["Z_m2", Simplify[("Zm2" /. phi4Results) - (1 + lambda kappa/(2 eps) + lambda^2 kappa^2 (1/(2 eps^2) - 1/(4 eps)))], 0];
check["beta = 3 lambda^2/(16 pi^2) - 17/3 lambda^3/(16 pi^2)^2", Simplify[("Beta" /. phi4Results) - (3 lambda^2 kappa - 17/3 lambda^3 kappa^2)], 0];
check["gamma_phi = lambda^2/(12 (16 pi^2)^2)", Simplify[("GammaPhi" /. phi4Results) - lambda^2 kappa^2/12], 0];
check["gamma_m2 = lambda/(16 pi^2) - 5/6 lambda^2/(16 pi^2)^2", Simplify[("GammaM2" /. phi4Results) - (lambda kappa - 5/6 lambda^2 kappa^2)], 0];
check["'t Hooft consistency: double poles from the single ones", Simplify[{zl22 - zl1^2, zm22 - zm1 (zm1 + zl1)/2} /. ("Solution" /. phi4Results)], {0, 0}];
check["massive one loop: Z_m2 m^2 - Sigma finite", "MassiveCheck" /. phi4Results, 0];
checkMessage[BetaFunctionFromZ[1 + g/eps + 3 g^2/eps^2, g, eps, 3], "inconsistent poles: BetaFunctionFromZ gives a message", BetaFunctionFromZ::pole,
  $Failed];
check["consistent poles (double pole = square of the single one): a finite beta", BetaFunctionFromZ[1 + g/eps + g^2/eps^2, g, eps, 3], 2 g^2];
(* g_B = mu^eps Z g, Z = 1 + c g^2/eps (a gauge coupling): beta = 2 c g^3; c = -b0/(2 (16 pi^2)) gives -b0 g^3/(16 pi^2) *)
check["\"ScalePower\" -> 1 (g_B = mu^eps Z g): beta = 2 c g^3", BetaFunctionFromZ[1 + c g^2/eps, g, eps, 3, "ScalePower" -> 1], 2 c g^3];
(* the Wilson-Fisher fixed point lambda_s in d = 4 - e (e = 2 eps): -e lambda_s + beta(lambda_s) = 0, eta = 2 gamma_phi(lambda_s),
   nu = 1/(2 - gamma_m2(lambda_s)), omega = d(-e lambda + beta)/d lambda at lambda_s, each to e^2 *)
wilsonFisher[beta_, gammaPhi_, gammaM2_] := Module[{a1, a2, star, series2, g},
   series2[f_] := Sum[Coefficient[Expand[f], e, j] e^j, {j, 0, 2}];
   star = (a1 e + a2 e^2)/kappa;
   star = star /. First[Select[Solve[Thread[Table[Coefficient[Expand[(-e lambda + beta) /. lambda -> star], e, j], {j, 2, 3}] == 0], {a1, a2}],
       (a1 /. #) =!= 0 &]];
   g = series2[gammaM2 /. lambda -> star];
   {series2[2 gammaPhi /. lambda -> star], series2[1/2 + Coefficient[g, e, 1] e/4 + (Coefficient[g, e, 1]^2/8 + Coefficient[g, e, 2]/4) e^2],
    series2[D[-e lambda + beta, lambda] /. lambda -> star]}];
check["Wilson-Fisher: eta = e^2/54, nu = 1/2 + e/12 + 7 e^2/162, omega = e - 17 e^2/27 (Ising, d = 4 - e)",
  Expand[wilsonFisher["Beta" /. phi4Results, "GammaPhi" /. phi4Results, "GammaM2" /. phi4Results] - {e^2/54, 1/2 + e/12 + 7 e^2/162, e - 17 e^2/27}], {0, 0, 0}];

(* ------------------------------------------------------------------------- *)
section["dimensional regularization: LaTeX and display"];
check["LaTeX of a loop integral: one fraction", ExpressionToLaTeX[lambda^2/6 LoopIntegral[ScalarPropagator[k, m] ScalarPropagator[l, m] ScalarPropagator[p + k + l, m], {k, l}]],
  "\\frac{1}{6}\\,\\lambda^{2}\\,\\int_{k\\,l}\\frac{1}{(k^{2} + m^{2})\\,(l^{2} + m^{2})\\,((k + l + p)^{2} + m^{2})}"];
check["LaTeX of the expanded bubble, with mu", ExpressionToLaTeX[EpsilonExpand[BubbleIntegral[{1, 1}, p^2, {0, 0}, eps], eps, 0], "Names" -> {eps -> "\\varepsilon"}],
  "\\frac{1}{8\\,\\pi^{2}} + \\frac{1}{16\\,\\varepsilon\\,\\pi^{2}} - \\frac{\\log\\left(\\frac{p^{2}}{\\mu^{2}}\\right)}{16\\,\\pi^{2}}"];
check["LaTeX of a power of a power, a massive bubble, a line with a power", ExpressionToLaTeX[#, "Names" -> {eps -> "\\varepsilon"}] & /@
   {(p^2)^(1 - 2 eps), BubbleIntegral[{1, 1}, p^2, {m1, m2}, eps], ScalarPropagator[k + p, m]^2, EulerGamma},
  {"\\left(p^{2}\\right)^{1 - 2\\,\\varepsilon}", "B_{1,1}(p^{2};m_{1},m_{2})", "\\frac{1}{\\left((k + p)^{2} + m^{2}\\right)^{2}}", "\\gamma_E"}];
check["zeta(3) instead of PolyGamma(2, 1)", With[{e3 = EpsilonExpand[Gamma[eps]^3, eps, 0]}, {FreeQ[e3, PolyGamma], Coefficient[e3, Zeta[3]]}], {True, -1}];
check["display of a line as a fraction", FreeQ[ToBoxes[ScalarPropagator[k + p, m]], FractionBox], False];

(* ------------------------------------------------------------------------- *)
section["Hubbard-Stratonovich and integrating out a field"];
DeclareGrassmannSource[hsEta, hsEtabar]; DeclareDirectedPropagator[hsG0];
DeclareField[hsPsi, hsEtabar]; DeclareField[hsPsibar, hsEta, -1];
hsZf = Exp[-IntegralOver[hsG0[x, y] hsEtabar[x] ** hsEta[y], {x, y}]];
DeclareSymmetric[hsV];
hsCoulomb = 1/2 IntegralOver[hsV[x, y] hsPsibar[x] ** hsPsibar[y] ** hsPsi[y] ** hsPsi[x], {x, y}];
hsTheory = HubbardStratonovich[{hsZf, hsCoulomb}, hsPsibar[#] ** hsPsi[#] &, {hsPhi, hsJ}, Automatic];
check["HS: the propagator of the auxiliary field is the interaction", hsTheory["Propagator"], hsV];
checkZero["HS: the coupling -i Integral phi psibar psi", hsTheory["Interaction"] + I IntegralOver[hsPhi[z] hsPsibar[z] ** hsPsi[z], {z}]];
checkZero["HS: the same Green function to second order", PerturbativeCorrelationFunction[hsTheory["FreeGeneratingFunctional"],
    {hsPsi[x], hsPsibar[y]}, hsTheory["Interaction"], 4] - PerturbativeCorrelationFunction[hsZf, {hsPsi[x], hsPsibar[y]}, hsCoulomb, 2]];
hsBack = IntegrateOutField[{hsTheory["FreeGeneratingFunctional"], hsTheory["Interaction"]}, hsPhi, hsJ];
checkZero["integrating the auxiliary field out gives the interaction back", hsBack["Interaction"] - hsCoulomb];
checkZero["... and the free generating functional of the fermions", hsBack["FreeGeneratingFunctional"] - hsZf];
DeclareSource[hsJb]; DeclareField[hsChi, hsJb]; DeclareSymmetric[hsD0];
hsZb = Exp[1/2 IntegralOver[hsJb[x] hsD0[x, y] hsJb[y], {x, y}]];
hsPhi4 = lambda/24 IntegralOver[hsChi[z]^4, {z}];
hsTheory4 = HubbardStratonovich[{hsZb, hsPhi4}, hsChi[#]^2 &, {hsSigma, hsJs}, hsSigmaD];
checkZero["HS of phi^4: the kernel lambda/12 delta", hsTheory4["Kernel"][a, b] - lambda/12 DeltaFunction[a, b]];
hsLocal[e_] := SimplifyDiagrams[e /. hsSigmaD[p_, q_] :> hsTheory4["Kernel"][p, q]];
checkZero["HS of phi^4: the same propagator to second order", hsLocal[PerturbativeCorrelationFunction[hsTheory4["FreeGeneratingFunctional"],
    {hsChi[a], hsChi[b]}, hsTheory4["Interaction"], 4]] - PerturbativeCorrelationFunction[hsZb, {hsChi[a], hsChi[b]}, hsPhi4, 2]];
checkZero["HS of phi^4: back to lambda/4! phi^4", hsLocal[IntegrateOutField[{hsTheory4["FreeGeneratingFunctional"],
     hsTheory4["Interaction"]}, hsSigma, hsJs]["Interaction"]] - hsPhi4];
DeclareSource[hsJp]; DeclareField[hsPhonon, hsJp]; DeclareSymmetric[hsDp];
hsZep = hsZf Exp[1/2 IntegralOver[hsJp[x] hsDp[x, y] hsJp[y], {x, y}]];
hsEp = g IntegralOver[hsPhonon[z] hsPsibar[z] ** hsPsi[z], {z}];
hsEff = IntegrateOutField[{hsZep, hsEp}, hsPhonon, hsJp];
checkZero["phonons integrated out: -g^2/2 Integral rho D rho", hsEff["Interaction"] +
   g^2/2 IntegralOver[hsDp[x, y] hsPsibar[x] ** hsPsi[x] ** hsPsibar[y] ** hsPsi[y], {x, y}]];
checkZero["phonons integrated out: the same electron Green function to g^4", PerturbativeCorrelationFunction[hsZep, {hsPsi[x], hsPsibar[y]},
    hsEp, 4] - PerturbativeCorrelationFunction[hsEff["FreeGeneratingFunctional"], {hsPsi[x], hsPsibar[y]}, hsEff["Interaction"], 2]];
check["a term that is not of the form 1/2 O K O is kept, with a message", Quiet[HubbardStratonovich[{hsZb, hsPhi4 +
     IntegralOver[hsChi[z]^3, {z}]}, hsChi[#]^2 &, {hsSigma2, hsJs2}, hsSigmaD2]["Interaction"], FunctionalDiagrams::hsform] =!= $Failed, True];

(* ------------------------------------------------------------------------- *)
(* QED at one loop in dimensional regularization (section 19 of the package): Peskin and Schroeder's conventions with the
   charge -e of the electron, e > 0 (vertex +i e gamma^mu), d = 4 - 2 eps.
   Explicit Dirac matrices (d = 4 and d = 6), explicit spinors and NIntegrate are used here only, as independent references
   The direct four-dimensional momentum integrals are tabulated. QEDOneLoop.wl is the
   worked computation (the self-energy, the vacuum polarization and the vertex). *)
(* the arguments of the messages m given by e *)
SetAttributes[messageArguments, HoldAll];
messageArguments[e_, m_] := Module[{args = {}}, Internal`HandlerBlock[{"Message", Function[h, If[MatchQ[h, Hold[Message[m, ___], _]],
       AppendTo[args, Replace[h, Hold[Message[_, a___], _] :> {a}]]]]}, Quiet[e, m]]; args];
checkZeroGamma[name_String, expr_] := check[name, Simplify[FunctionExpand[expr]], 0];
d = SpacetimeDimension;
g[mu_] := GammaMatrix[mu]; sl[p_] := SlashedMomentum[p]; dp = DiracProduct;

(* ---------------- 1. Dirac algebra: identities ---------------- *)
section["QED: Dirac algebra"];
check["gamma^mu gamma_mu = d", DiracSimplify[dp[g[mu], g[mu]]], d dp[]];
checkSimplified["gamma^mu a gamma_mu = (2 - d) a", DiracOrder[dp[g[mu], sl[a], g[mu]] - (2 - d) dp[sl[a]]]];
checkSimplified["gamma^mu a b gamma_mu = 4 a.b - (4 - d) a b", DiracOrder[dp[g[mu], sl[a], sl[b], g[mu]] - (4 ScalarProduct[a, b] dp[] - (4 - d) dp[sl[a], sl[b]])]];
checkSimplified["gamma^mu a b c gamma_mu = -2 c b a + (4 - d) a b c", DiracOrder[dp[g[mu], sl[a], sl[b], sl[c], g[mu]] -
    (-2 dp[sl[c], sl[b], sl[a]] + (4 - d) dp[sl[a], sl[b], sl[c]])]];
checkSimplified["gamma^mu a b c e gamma_mu = 2 (e a b c + c b a e) - (4 - d) a b c e", DiracOrder[dp[g[mu], sl[a], sl[b], sl[c], sl[f], g[mu]] -
    (2 (dp[sl[f], sl[a], sl[b], sl[c]] + dp[sl[c], sl[b], sl[a], sl[f]]) - (4 - d) dp[sl[a], sl[b], sl[c], sl[f]])]];
checkSimplified["gamma^mu gamma^nu gamma_mu gamma_nu = d (2 - d)", DiracSimplify[dp[g[mu], g[nu], g[mu], g[nu]]] - d (2 - d) dp[]];
checkSimplified["anticommutator {gamma^mu, gamma^nu} = 2 g^{mu nu}", DiracOrder[dp[g[mu], g[nu]] + dp[g[nu], g[mu]] - 2 LorentzMetric[mu, nu] dp[]]];
checkSimplified["pslash pslash = p^2", DiracSimplify[dp[sl[p], sl[p]]] - ScalarProduct[p, p] dp[]];
check["linearity: slash of 2p - 3q", DiracSimplify[dp[sl[2 p - 3 q]]], 2 dp[sl[p]] - 3 dp[sl[q]]];
check["metric and four-vector contractions", DiracSimplify[LorentzMetric[mu, nu] FourVector[p, nu] FourVector[q, mu] + LorentzMetric[mu, mu] + LorentzMetric[mu, nu] LorentzMetric[nu, rho] FourVector[p, rho] FourVector[p, mu]],
  ScalarProduct[p, q] + d + ScalarProduct[p, p]];
check["four-vector into a gamma: p_mu gamma^mu = pslash", DiracSimplify[FourVector[p, mu] dp[g[nu], g[mu]]], dp[g[nu], sl[p]]];
check["scalar product bilinear", DiracSimplify[ScalarProduct[p + 2 q, 3 p]], 3 ScalarProduct[p, p] + 6 ScalarProduct[p, q]];
(* momenta that are not symbols are momenta too; a symbolic coefficient is ambiguous: left as it is *)
check["indexed momenta p[1], P[1]: linearity", DiracSimplify[FourVector[p[1] + 2 P[1] - q, mu] + dp[sl[p[1] - Subscript[p, 2]]]],
  FourVector[p[1], mu] + 2 FourVector[P[1], mu] - FourVector[q, mu] + dp[sl[p[1]]] - dp[sl[Subscript[p, 2]]]];
check["indexed momenta: Tr pslash[1] pslash[2] = 4 p[1].p[2]", DiracSimplify[DiracTrace[dp[sl[p[1]], sl[p[2]]]]], 4 ScalarProduct[p[1], p[2]]];
checkMessage[DiracSimplify[dp[sl[x p]]], "symbolic coefficient x p: message, left as it is", DiracSimplify::momentum, dp[sl[x p]]];
checkMessage[DiracSimplify[{dp[sl[m p]], dp[sl[p/m]]}], "m p and p/m: the same (message, left as they are)", DiracSimplify::momentum, {dp[sl[m p]], dp[sl[p/m]]}];
checkMessage[DiracSimplify[FourVector[x p + q, mu] FourVector[q, mu]], "a four-vector that is not split stays, the rest is contracted", DiracSimplify::momentum,
  ScalarProduct[q, x p + q]];

(* ---------------- 2. input that is refused ---------------- *)
section["QED: refused input"];
checkMessage[DiracSimplify[DiracTrace[dp[g[5], g[5]]]], "GammaMatrix[5] (no gamma5): message, $Failed", GammaMatrix::gamma5, $Failed];
checkMessage[IntegrateOneLoop[MinkowskiLoopIntegral[DiracTrace[dp[g[5], sl[k], g[mu], sl[k]]] FeynmanDenominator[k, m]^3, {k}], eps],
  "GammaMatrix[5] in a loop integral: message, unevaluated", GammaMatrix::gamma5, MinkowskiLoopIntegral[DiracTrace[dp[g[5], sl[k], g[mu], sl[k]]] FeynmanDenominator[k, m]^3, {k}]];
checkMessage[DiracSimplify[LorentzMetric[0, 0]], "explicit component index: message, $Failed", DiracSimplify::component, $Failed];
checkMessage[DiracSimplify[GammaMatrix[mu] GammaMatrix[mu]], "Times of matrices: message, $Failed", DiracSimplify::product, $Failed];
checkMessage[DiracSimplify[dp[sl[p]^2]], "Power of a matrix inside a product: message, $Failed", DiracSimplify::product, $Failed];
checkMessage[DiracSimplify[dp[g[mu], g[mu], g[mu]]], "an index three times: message, $Failed", DiracSimplify::index, $Failed];
(* the indices of an unevaluated loop integral are counted in its integrand, term by term; outside it counts as its free
   indices; the elements of a list are independent *)
check["a list whose elements share an index: each element on its own", Expand[DiracSimplify[{dp[g[mu], g[mu]], dp[g[mu], sl[p], g[mu]]}] -
    {d dp[], (2 - d) dp[sl[p]]}], {0, 0}];
check["a sum of indexed terms inside a loop integral: expanded and contracted (no ::index)",
  DiracSimplify[MinkowskiLoopIntegral[(FourVector[k, mu] + FourVector[p, mu]) FourVector[k, mu] FeynmanDenominator[k, m]^3, {k}]],
  MinkowskiLoopIntegral[FeynmanDenominator[k, m]^3 ScalarProduct[k, k] + FeynmanDenominator[k, m]^3 ScalarProduct[k, p], {k}]];
check["a dummy index used inside and outside a loop integral", DiracSimplify[FourVector[p, mu] FourVector[q, mu] MinkowskiLoopIntegral[FourVector[k, mu] FourVector[r, mu] FeynmanDenominator[k, m]^3, {k}]],
  ScalarProduct[p, q] MinkowskiLoopIntegral[FeynmanDenominator[k, m]^3 ScalarProduct[k, r], {k}]];
checkMessage[DiracSimplify[MinkowskiLoopIntegral[FourVector[k, mu] FourVector[k, mu] FourVector[p, mu] FeynmanDenominator[k, m]^3, {k}]],
  "an index three times inside a loop integral: message, $Failed", DiracSimplify::index, $Failed];
checkMessage[DiracSimplify[FourVector[p, mu] FourVector[q, mu] MinkowskiLoopIntegral[FourVector[k, mu] FeynmanDenominator[k, m]^3, {k}]],
  "an index free inside a loop integral and twice outside: message, $Failed", DiracSimplify::index, $Failed];
check["the trace table is local to DiracSimplify (no definitions left)", (DiracSimplify[DiracTrace[dp[sl[a], sl[b], sl[c], sl[f], sl[h], sl[a]]]];
   DownValues[FunctionalDiagrams`Private`traceTable]), {}];

(* ---------------- 3. traces ---------------- *)
section["QED: traces"];
check["Tr 1 = 4", DiracSimplify[DiracTrace[dp[]]], 4];
check["Tr of an odd number of gammas = 0", DiracSimplify[DiracTrace[dp[g[mu], sl[p], g[nu]]]], 0];
check["Tr gamma^mu gamma^nu = 4 g", DiracSimplify[DiracTrace[dp[g[mu], g[nu]]]], 4 LorentzMetric[mu, nu]];
check["Tr a b c e = 4 [(ab)(ce) - (ac)(be) + (ae)(bc)]", DiracSimplify[DiracTrace[dp[sl[a], sl[b], sl[c], sl[f]]]],
  Expand[4 (ScalarProduct[a, b] ScalarProduct[c, f] - ScalarProduct[a, c] ScalarProduct[b, f] + ScalarProduct[a, f] ScalarProduct[b, c])]];
checkSimplified["Tr gamma^mu a gamma_mu b = 4 (2 - d) a.b", DiracSimplify[DiracTrace[dp[g[mu], sl[a], g[mu], sl[b]]]] - 4 (2 - d) ScalarProduct[a, b]];
checkSimplified["Tr of six: cyclicity", DiracSimplify[DiracTrace[dp[sl[a], sl[b], g[mu], sl[c], g[nu], sl[f]]] - DiracTrace[dp[sl[b], g[mu], sl[c], g[nu], sl[f], sl[a]]]]];
checkSimplified["Tr (pslash + m)(qslash + m) = 4 (p.q + m^2)", DiracSimplify[DiracTrace[dp[sl[p] + m, sl[q] + m]]] - 4 (ScalarProduct[p, q] + m^2)];
checkSimplified["Tr gamma^mu (k + m) gamma^nu (k + q + m) (vacuum polarization numerator)",
  DiracSimplify[DiracTrace[dp[g[mu], sl[k] + m, g[nu], sl[k] + sl[q] + m]]] - 4 (FourVector[k, mu] FourVector[k + q, nu] + FourVector[k, nu] FourVector[k + q, mu] -
      LorentzMetric[mu, nu] (ScalarProduct[k, k + q] - m^2)) /. ScalarProduct[k, k + q] -> ScalarProduct[k, k] + ScalarProduct[k, q] /. FourVector[k + q, nu_] :> FourVector[k, nu] + FourVector[q, nu]];

(* ---------------- 4. explicit Dirac matrices: d = 4 (4x4) and d = 6 (8x8, Tr 1 normalized to 4) ---------------- *)
section["QED: explicit Dirac matrices"];
pauli = {{{0, 1}, {1, 0}}, {{0, -I}, {I, 0}}, {{1, 0}, {0, -1}}};
gammaNumeric = Join[{DiagonalMatrix[{1, 1, -1, -1}]}, Table[ArrayFlatten[{{0, pauli[[i]]}, {-pauli[[i]], 0}}], {i, 3}]];
metric = DiagonalMatrix[{1, -1, -1, -1}];
SeedRandom[7];
vectors = Table[v -> RandomInteger[{-5, 5}, 4]/3, {v, {a, b, c, f, h, k}}];
vec[v_] := v /. vectors;
slashNumeric[v_] := Sum[metric[[i, i]] gammaNumeric[[i]] vec[v][[i]], {i, 4}];
numericForm[e_, indexValues_] := e /. indexValues /. {ScalarProduct[u_, v_] :> vec[u].metric.vec[v], FourVector[u_, n_Integer] :> vec[u][[n + 1]],
     LorentzMetric[n1_Integer, n2_Integer] :> metric[[n1 + 1, n2 + 1]], SpacetimeDimension -> 4} /.
   {DiracProduct[] -> IdentityMatrix[4], DiracProduct[x__] :> Dot @@ (matrixOf /@ {x})} /. DiracTrace[x_] :> Tr[x];
(* a factor of a product: matrices, and scalars times the unit matrix *)
matrixOf[fa_] := With[{sc = fa /. {_GammaMatrix -> 0, _SlashedMomentum -> 0}},
   ((fa - sc) /. {GammaMatrix[n_Integer] :> gammaNumeric[[n + 1]], SlashedMomentum[u_] :> slashNumeric[u]}) + sc IdentityMatrix[4]];
sandwich[ub_, mat_, u_] := If[MatrixQ[mat], ub.mat.u, mat (ub.u)];
traceTest = DiracTrace[dp[sl[a], sl[b], g[mu], sl[c], g[nu], sl[f], sl[h], sl[k]]];
check["Tr of eight against explicit matrices (all mu, nu)", Union[Flatten[Table[Simplify[numericForm[DiracSimplify[traceTest], {mu -> i, nu -> j}] -
       numericForm[traceTest, {mu -> i, nu -> j}]], {i, 0, 3}, {j, 0, 3}]]], {0}];
check["Tr of six slashes against explicit matrices", Simplify[numericForm[DiracSimplify[DiracTrace[dp[sl[a], sl[b], sl[c], sl[f], sl[h], sl[k]]]], {}] -
    numericForm[DiracTrace[dp[sl[a], sl[b], sl[c], sl[f], sl[h], sl[k]]], {}]], 0];
contractionTest = dp[g[rho], sl[a], g[mu], sl[b], g[rho]];
check["gamma^rho a gamma^mu b gamma_rho against explicit matrices", Union[Flatten[Table[Simplify[numericForm[DiracSimplify[contractionTest], {mu -> i}] -
       Sum[metric[[r + 1, r + 1]] numericForm[contractionTest, {mu -> i, rho -> r}], {r, 0, 3}]], {i, 0, 3}]]], {0}];
(* on-shell spinors in the Dirac representation: u(p) = (Sqrt[E + m] chi, sigma.p chi/Sqrt[E + m]) *)
spinor[pv_, mass_, s_] := With[{en = pv[[1]], chi = If[s == 1, {1, 0}, {0, 1}]},
   Join[Sqrt[en + mass] chi, (Sum[pauli[[i]] pv[[i + 1]], {i, 3}].chi)/Sqrt[en + mass]]];
onShell[v3_, mass_] := Prepend[v3, Sqrt[mass^2 + v3.v3]];
chainVectors = N[{p1 -> onShell[{1/2, -1, 2/3}, 1], p2 -> onShell[{-1/3, 1/4, 1}, 1]}, 40];
chainNumeric[e_, indexValues_, s1_, s2_] := Block[{vectors = Join[vectors, chainVectors]},
   numericForm[e /. SpinorChain[{u2_, m2_}, x_, {u1_, m1_}] :> chainHold[Conjugate[spinor[vec[u2], 1, s2]].gammaNumeric[[1]], x, spinor[vec[u1], 1, s1]], indexValues] /.
    chainHold -> sandwich /. m -> 1];
chainTest = SpinorChain[{p2, m}, dp[sl[p1], g[mu], sl[p2] + m, g[nu], sl[p1], sl[p2]], {p1, m}];
check["spinor chain with the Dirac equation against explicit spinors (all mu, nu, spins)",
  Union[Flatten[Table[Chop[chainNumeric[DiracSimplify[chainTest], {mu -> i, nu -> j}, s1, s2] - chainNumeric[chainTest, {mu -> i, nu -> j}, s1, s2], 10^-25],
     {i, 0, 3}, {j, 0, 3}, {s1, 2}, {s2, 2}]]], {0}];
gordonA = SpinorChain[{p2, m}, dp[g[mu]], {p1, m}] - FourVector[p1 + p2, mu]/(2 m) SpinorChain[{p2, m}, dp[], {p1, m}];
gordonB = 1/(2 m) SpinorChain[{p2, m}, I (I/2) (dp[g[mu], g[nu]] - dp[g[nu], g[mu]]) FourVector[p2 - p1, nu], {p1, m}];
check["Gordon identity (symbolic)", DiracSimplify[gordonA - gordonB], 0];
check["Gordon identity against explicit spinors", Union[Flatten[Table[Chop[chainNumeric[gordonA, {mu -> i}, s1, s2] -
       Sum[metric[[n + 1, n + 1]] chainNumeric[gordonB, {mu -> i, nu -> n}, s1, s2], {n, 0, 3}], 10^-25], {i, 0, 3}, {s1, 2}, {s2, 2}]]], {0}];
(* d = 6: 8x8 Clifford matrices (Tr 1 normalized to 4), so that the d-dependence of the contractions is tested: words with
   free and contracted indices, products, traces, spinor chains (between the on-shell projectors (p2slash + m) X (p1slash + m),
   which the reductions of DiracSimplify leave unchanged) and DiracOrder, against the explicit sums over the components *)
kron[ms__] := Fold[KroneckerProduct, {{1}}, {ms}];
id2 = IdentityMatrix[2];
gammas6 = With[{eu = {kron[pauli[[1]], id2, id2], kron[pauli[[2]], id2, id2], kron[pauli[[3]], pauli[[1]], id2], kron[pauli[[3]], pauli[[2]], id2],
      kron[pauli[[3]], pauli[[3]], pauli[[1]]], kron[pauli[[3]], pauli[[3]], pauli[[2]]]}}, Join[{eu[[1]]}, I Rest[eu]]];
eta6 = DiagonalMatrix[{1, -1, -1, -1, -1, -1}];
check["d = 6 Clifford relations of the reference matrices", And @@ Flatten[Table[gammas6[[i]].gammas6[[j]] + gammas6[[j]].gammas6[[i]] ==
     2 eta6[[i, j]] IdentityMatrix[8], {i, 6}, {j, 6}]], True];
SeedRandom[20261018];
vectors6 = Join[Table[v -> RandomInteger[{-4, 4}, 6]/3, {v, {va, vb, vc, vf}}], {p1 -> {5/4, 1/4, 1/2, 1/2, 0, 0}, p2 -> {13/12, 1/4, 1/3, 0, 0, 0}}];
slash6[v_] := With[{cv = v /. vectors6}, Sum[eta6[[i, i]] cv[[i]] gammas6[[i]], {i, 6}]];
element6[el_, iv_] := Which[MatchQ[el, GammaMatrix[_]], gammas6[[(el[[1]] /. iv) + 1]], MatchQ[el, SlashedMomentum[_]], slash6[el[[1]]],
   True, slash6[el[[2, 1]]] + el[[1]] IdentityMatrix[8]];
projector6[v_] := slash6[v] + IdentityMatrix[8];
numeric6[e_, iv_] := Module[{r},
   r = e /. iv /. {ScalarProduct[u_, v_] :> (u /. vectors6).eta6.(v /. vectors6), FourVector[u_, i_Integer] :> (u /. vectors6)[[i + 1]],
      LorentzMetric[i_Integer, j_Integer] :> eta6[[i + 1, j + 1]], SpacetimeDimension -> 6, m -> 1};
   r = r /. {DiracTrace[x_] :> traceHold[x], SpinorChain[{q2_, _}, x_, {q1_, _}] :> chainHold[q2, x, q1]};
   r = r /. {DiracProduct[] :> IdentityMatrix[8], DiracProduct[xs__] :> Dot @@ (element6[#, {}] & /@ {xs})};
   r /. {traceHold[x_] :> Tr[x]/2, chainHold[q2_, x_, q1_] :> projector6[q2].x.projector6[q1]}];
explicit6[wrap_, word_List, freeVals_, contracted_List] := Sum[With[{iv = Join[freeVals, Thread[contracted -> cv]]},
    Times @@ Table[eta6[[cc + 1, cc + 1]], {cc, cv}] wrap[Dot @@ (element6[#, iv] & /@ word)]], {cv, Tuples[Range[0, 5], Length[contracted]]}];
randomWord6[len_, nFree_, nContracted_, slashes_List] := RandomSample[Join[Table[GammaMatrix[{mu, nu}[[i]]], {i, nFree}],
    Flatten[Table[{GammaMatrix[{rho, sigma}[[i]]], GammaMatrix[{rho, sigma}[[i]]]}, {i, nContracted}]],
    Table[If[RandomReal[] < 0.2, SlashedMomentum[RandomChoice[slashes]] + RandomChoice[{2/3, -1/2}], SlashedMomentum[RandomChoice[slashes]]],
     {Max[0, len - nFree - 2 nContracted]}]]];
case6[kind_] := Module[{nFree = RandomInteger[{0, 2}], nContracted = RandomInteger[{0, 2}], len = RandomInteger[{1, 8}], word, expr, sym, iv},
   word = randomWord6[len, nFree, nContracted, If[kind === "chain", {va, vb, p1, p2}, {va, vb, vc, vf}]];
   expr = Switch[kind, "trace", DiracTrace[DiracProduct @@ word], "chain", SpinorChain[{p2, m}, DiracProduct @@ word, {p1, m}], _, DiracProduct @@ word];
   sym = If[kind === "order", DiracOrder[expr], DiracSimplify[expr]];
   iv = Thread[Take[{mu, nu}, nFree] -> RandomInteger[{0, 5}, nFree]];
   FreeQ[sym, rho | sigma] && Union[Flatten[{Simplify[numeric6[sym, iv] - explicit6[Switch[kind, "trace", Tr[#]/2 &, "chain", projector6[p2].#.projector6[p1] &, _, Identity],
          word, iv, Take[{rho, sigma}, nContracted]]]}]] === {0}];
check["d = 6: 60 random products, traces, spinor chains and DiracOrder against the explicit matrices",
  Count[Flatten[Table[case6[kind], {kind, {"product", "trace", "chain", "order"}}, {15}]], True], 60];

(* ---------------- 5. one-loop integrals ---------------- *)
section["QED: one-loop integrals"];
fd = FeynmanDenominator; ml[f_] := MinkowskiLoopIntegral[f, {k}];
(* Wick rotation: Int d^dk 1/(k^2 - m^2)^n = i (-1)^n Int d^dk_E 1/(k_E^2 + m^2)^n *)
checkSimplified["tadpole = -i A(1; m) (exact in d)", IntegrateOneLoop[ml[fd[k, m]], eps] + I TadpoleIntegral[1, m, eps]];
checkSimplified["1/(k^2 - m^2)^3 = -i A(3; m)", IntegrateOneLoop[ml[fd[k, m]^3], eps] + I TadpoleIntegral[3, m, eps]];
checkZeroGamma["k^2/(k^2 - m^2)^3 = 1/(k^2 - m^2)^2 + m^2/(k^2 - m^2)^3 (exact in d)", IntegrateOneLoop[ml[ScalarProduct[k, k] fd[k, m]^3], eps] -
   IntegrateOneLoop[ml[fd[k, m]^2 + m^2 fd[k, m]^3], eps]];
checkZeroGamma["k^mu k^nu/(k^2 - m^2)^3 = g^{mu nu}/d k^2/(...)", IntegrateOneLoop[ml[FourVector[k, mu] FourVector[k, nu] fd[k, m]^3], eps] -
   LorentzMetric[mu, nu]/(4 - 2 eps) IntegrateOneLoop[ml[ScalarProduct[k, k] fd[k, m]^3], eps]];
check["odd numerator: k^mu/(k^2 - m^2)^2 = 0", IntegrateOneLoop[ml[FourVector[k, mu] fd[k, m]^2], eps], 0];
check["massless tadpole = 0", IntegrateOneLoop[ml[fd[k, 0]^2], eps], 0];
checkZeroGamma["k^mu k^nu k^rho k^sigma/(k^2 - m^2)^5 = (g g + g g + g g)/(d (d + 2)) (k^2)^2/(...) (exact in d)",
  IntegrateOneLoop[ml[FourVector[k, mu] FourVector[k, nu] FourVector[k, rho] FourVector[k, sigma] fd[k, m]^5], eps] -
   (LorentzMetric[mu, nu] LorentzMetric[rho, sigma] + LorentzMetric[mu, rho] LorentzMetric[nu, sigma] + LorentzMetric[mu, sigma] LorentzMetric[nu, rho])/
     ((4 - 2 eps) (6 - 2 eps)) IntegrateOneLoop[ml[ScalarProduct[k, k]^2 fd[k, m]^5], eps]];
checkSimplified["a negative power of a denominator is a numerator: (k^2 - m^2)/((k + p)^2 - m^2)^3",
  IntegrateOneLoop[ml[fd[k, m]^-1 fd[k + p, m]^3], eps] - IntegrateOneLoop[ml[(ScalarProduct[k, k] - m^2) fd[k + p, m]^3], eps]];
checkMessage[IntegrateOneLoop[ml[fd[k, m]^a], eps], "symbolic power of the only denominator: message, unevaluated", IntegrateOneLoop::power, ml[fd[k, m]^a]];
checkMessage[IntegrateOneLoop[ml[Sqrt[fd[k, m]] fd[k, m]^2], eps], "power 5/2: message, unevaluated", IntegrateOneLoop::power, ml[fd[k, m]^(5/2)]];
checkMessage[IntegrateOneLoop[ml[Sin[ScalarProduct[k, k]]], eps], "a numerator that is not polynomial, no denominator: message, unevaluated",
  IntegrateOneLoop::numerator, ml[Sin[ScalarProduct[k, k]]]];
checkMessage[IntegrateOneLoop[ml[Sin[ScalarProduct[k, k]] fd[k, m]^3], eps], "the same next to a denominator", IntegrateOneLoop::numerator, ml[Sin[ScalarProduct[k, k]] fd[k, m]^3]];
check["the factor of the ::numerator message is written with the loop momentum of the user",
  messageArguments[IntegrateOneLoop[ml[Sin[ScalarProduct[k, k]] fd[k, m]^3], eps], IntegrateOneLoop::numerator], {{Sin[ScalarProduct[k, k]], k}}];
checkMessage[IntegrateOneLoop[MinkowskiLoopIntegral[fd[k, m] fd[l, m], {k, l}], eps], "two loops: message, unevaluated", IntegrateOneLoop::loops,
  MinkowskiLoopIntegral[fd[k, m] fd[l, m], {k, l}]];
check["a denominator 2 k: the ::momentum message is written with the momenta of the user",
  messageArguments[IntegrateOneLoop[ml[fd[2 k, m] fd[k, m]], eps], IntegrateOneLoop::momentum], {{2 k, k}}];
(* an unevaluated Minkowski integral depends on eps through d: it is not expanded as a constant *)
checkMessage[ExpandParameterIntegrals[MinkowskiLoopIntegral[fd[k, m] fd[l, m], {k, l}], eps, 0], "ExpandParameterIntegrals of an unevaluated (two-loop) Minkowski integral: message, $Failed",
  EpsilonExpand::loop, $Failed];
checkMessage[EpsilonExpand[Gamma[eps] ml[fd[k, m]^a], eps, 0], "EpsilonExpand of an unevaluated Minkowski integral: message, $Failed", EpsilonExpand::loop, $Failed];
(* the bubble against the Euclidean BubbleIntegral of section 18: p^2 = -P2 < 0 *)
bubbleM = IntegrateOneLoop[ml[fd[k, m1] fd[k + p, m2]], eps];
bubbleE = I BubbleIntegral[{1, 1}, P2, {m1, m2}, eps];
checkNumeric["bubble = i B(1,1) (Euclidean), pole", ExpandParameterIntegrals[bubbleM, eps, -1] eps, I/(16 Pi^2), 10^-14];
With[{vals = {m1 -> 1, m2 -> 3/2, P2 -> 7/3, RenormalizationScale -> 2}},
  checkNumeric["bubble = i B(1,1) (Euclidean), finite part", Coefficient[ExpandParameterIntegrals[bubbleM, eps, 0], eps, 0] /. ScalarProduct[p, p] -> -P2 /. vals,
   Coefficient[EpsilonExpand[bubbleE, eps, 0], eps, 0] /. vals, 10^-10];
  checkNumeric["bubble exact in d at eps = 0.37 (against the Euclidean Feynman-parameter form of BubbleIntegral)",
   ReduceParameterIntegrals[bubbleM /. ScalarProduct[p, p] -> -P2] /. vals /. eps -> 37/100,
   I Exp[eps EulerGamma] RenormalizationScale^(2 eps) Gamma[eps]/(16 Pi^2) NIntegrate[(x (1 - x) P2 + x m1^2 + (1 - x) m2^2)^(-eps) /. vals /. eps -> 37/100, {x, 0, 1}] /.
     vals /. eps -> 37/100, 10^-10]];
checkNumeric["external momentum P[1] (indexed): the bubble is that of a symbol P (eps^0 at P^2 = -2 m^2)",
  Coefficient[ExpandParameterIntegrals[IntegrateOneLoop[ml[fd[k, m] fd[k + P[1], m]], eps], eps, 0], eps, 0] /. {ScalarProduct[P[1], P[1]] -> -2, m -> 1, RenormalizationScale -> 1},
  Coefficient[ExpandParameterIntegrals[IntegrateOneLoop[ml[fd[k, m] fd[k + P, m]], eps], eps, 0], eps, 0] /. {ScalarProduct[P, P] -> -2, m -> 1, RenormalizationScale -> 1}, 10^-12];
(* the tensor bubble: k^mu -> -x p^mu *)
tensorBubble = IntegrateOneLoop[ml[FourVector[k, mu] fd[k, m] fd[k + p, m]], eps];
checkSimplified["k^mu bubble = -(1/2) p^mu bubble (equal masses)", ReduceParameterIntegrals[tensorBubble + 1/2 FourVector[p, mu] IntegrateOneLoop[ml[fd[k, m] fd[k + p, m]], eps]] /.
   {ParameterIntegral[fi_, x_] :> ParameterIntegral[Simplify[fi + (fi /. x -> 1 - x)]/2, x]}];
(* finite integrals with three denominators (external momenta spatial: p^0 = 0), the package's values (N at
   precision 20) against two tabulated references computed independently of 10_qed.wl:
   - Schwinger parameters: the Gaussian k integral of the explicit 4-vectors, the scale integral exact, NIntegrate over the
     simplex at 40 digits (agrees with the package's N at 20 digits to all of them);
   - a direct NIntegrate over (k1, k2, rho) of the Euclidean integrand (several minutes), PrecisionGoal 11, accurate to about
     4e-7 (relative) only: a coarse check of the Wick rotation and of the routing of the momenta.
   The errors are relative. *)
checkRelative[name_String, value_, expected_, tolerance_] :=
  With[{err = With[{v = If[NumberQ[value], value, N[value]], x = If[NumberQ[expected], expected, N[expected]]}, Abs[v - x]/Abs[x]]},
   If[NumberQ[err] && err < tolerance, passed++; Print["PASS ", name, "   [relative error ", scientific[err], "]"],
    failed++; AppendTo[failures, name]; Print["FAIL ", name, "\n     got:      ", N[value], "\n     expected: ", N[expected]]]];
loopReferences = {
   (* {name, integral, Schwinger reference (20 digits), direct 4D reference} *)
   {"scalar triangle 1/((k^2-m^2)((k+p)^2-m^2)((k+r)^2-m^2))", ml[fd[k, m] fd[k + p, m] fd[k + r, m]],
    -0.0023181058371422828325 I, -0.00231810588446454 I},
   {"rank 2, four denominators, a squared one: (k.r)^2/((k^2-m^2)^2 ...)", ml[ScalarProduct[k, r]^2 fd[k, m]^2 fd[k + p, m] fd[k + r, m]],
    0.0010916993647381051338 I, 0.00109169979674817 I},
   {"rank 3, five denominators: (k.r)^2 (k.p)/(...)", ml[ScalarProduct[k, r]^2 ScalarProduct[k, p] fd[k, m]^2 fd[k + p, m]^2 fd[k + r, m]],
    -0.00022395944972023802585 I, -0.000223959458478569 I}};
pE = {1, 0}; rE = {3/4, 6/5};
eRules = {ScalarProduct[p, p] -> -pE.pE, ScalarProduct[r, r] -> -rE.rE, ScalarProduct[p, r] -> -pE.rE, m -> 1, RenormalizationScale -> 1};
Do[With[{value = N[ExpandParameterIntegrals[IntegrateOneLoop[c[[2]], eps], eps, 0] /. eRules, 20]},
   checkRelative[c[[1]] <> " against Schwinger parameters (N to 20 digits)", value, c[[3]], 10^-17];
   checkRelative[c[[1]] <> " against direct 4D integration", value, c[[4]], 10^-6]], {c, loopReferences}];

(* ---------------- 6. Feynman-parameter integrals ---------------- *)
section["QED: Feynman-parameter integrals"];
fp1 = FeynmanParameter[1]; fp2 = FeynmanParameter[2]; fp3 = FeynmanParameter[3];
(* Feynman-parameter closed forms (Beta functions) against NIntegrate at a numerical eps *)
betaTest = ParameterIntegral[fp1^2 (1 - fp1) Power[m^2 fp1^2, -1 - eps], fp1];
checkNumeric["Beta-function closed form (IR divergent at eps = 0) at eps = -3/10", ReduceParameterIntegrals[betaTest] /. {m -> 2, eps -> -3/10},
  NIntegrate[x^2 (1 - x) (4 x^2)^(-1 + 3/10), {x, 0, 1}], 10^-10];
simplexTest = SimplexIntegral[fp2 fp3^2 Power[(1 - fp1)^2 + fp1/3, -eps - 1], {fp1, fp2, fp3}];
checkNumeric["simplex integral reduced to one parameter, against N of the two-dimensional integral", N[ReduceParameterIntegrals[simplexTest] /. eps -> 1/5], N[simplexTest /. eps -> 1/5], 10^-8];
check["a factor that is not polynomial in the variables integrated out: the simplex integral stays",
  ReduceParameterIntegrals[SimplexIntegral[Power[1 + fp2, -1] Power[2 - fp1, -eps], {fp1, fp2, fp3}]], SimplexIntegral[Power[1 + fp2, -1] Power[2 - fp1, -eps], {fp1, fp2, fp3}]];
checkNumeric["its N (cube map) against NIntegrate over the triangle", N[SimplexIntegral[Power[1 + fp2, -1] Power[2 - fp1, -1/5], {fp1, fp2, fp3}]],
  NIntegrate[1/((1 + y) (2 - x)^(1/5)), {x, 0, 1}, {y, 0, 1 - x}], 10^-10];
check["DifferentiateUnderIntegral", DifferentiateUnderIntegral[s ParameterIntegral[s x^2, x], s], ParameterIntegral[s x^2, x] + s ParameterIntegral[x^2, x]];
check["an exponent that depends on the variable (E^t, t^t, 2^t t): not a Beta function, the integral stays",
  Table[ReduceParameterIntegrals[i], {i, {ParameterIntegral[E^t, t], ParameterIntegral[t^t, t], ParameterIntegral[2^t t, t]}}],
  {ParameterIntegral[E^t, t], ParameterIntegral[t^t, t], ParameterIntegral[2^t t, t]}];
checkNumeric["... on a simplex, E^x1 is integrated over the other parameters only (N: e - 2)", N[ReduceParameterIntegrals[SimplexIntegral[E^fp1, {fp1, fp2, fp3}]]], E - 2, 10^-12];
checkMessage[ExpandParameterIntegrals[ParameterIntegral[x^(-1 - eps) E^x, x], eps, 0], "... and Int x^(-1-eps) E^x is not expanded (::ir, $Failed; it was -E^x/eps)",
  ExpandParameterIntegrals::ir, $Failed];
check["ExpandParameterIntegrals of a list: each element", ExpandParameterIntegrals[{Gamma[eps] ParameterIntegral[(1 + x)^-eps, x],
    ParameterIntegral[x^(-1 - eps) (1 + x)^(-eps), x]}, eps, -1], {1/eps, -1/eps}];
check["N[ParameterIntegral, 30] to 30 digits", With[{v = N[ParameterIntegral[Sqrt[1 + x], x], 30]}, {Precision[v] >= 30, Abs[v - 2/3 (2^(3/2) - 1)] < 10^-29}], {True, True}];
check["polynomial coefficients of a simplex integral integrated exactly (1/(2 eps))",
  ExpandParameterIntegrals[SimplexIntegral[Gamma[eps] (fp1 + fp2 + fp3) Power[1 + fp1 fp2, -eps], {fp1, fp2, fp3}], eps, -1], 1/(2 eps)];
(* the order of the expansion under the integral sign: what multiplies the integral may have a pole of any order *)
checkNumeric["Int (1 + x)^-eps / eps^3 at eps^0 = -(1/6) Int ln^3(1 + x)", Coefficient[ExpandParameterIntegrals[ParameterIntegral[(1 + x)^(-eps), x]/eps^3, eps, 0], eps, 0],
  -(2 Log[2]^3 - 6 Log[2]^2 + 12 Log[2] - 6)/6, 10^-12];
(* infrared divergences at the endpoints: the singular part subtracted (Beta functions), the rest expanded *)
check["Int x^(-1-eps) (1 + x)^-eps = -1/eps + 0 + O(eps)", ExpandParameterIntegrals[ParameterIntegral[x^(-1 - eps) (1 + x)^(-eps), x], eps, 0], -1/eps];
checkNumeric["... its eps^1 coefficient = -Int ln(1 + x)/x = -pi^2/12", Coefficient[ExpandParameterIntegrals[ParameterIntegral[x^(-1 - eps) (1 + x)^(-eps), x], eps, 1], eps, 1],
  -Pi^2/12, 10^-12];
checkNumeric["a power divergence: Int x^(-2-eps) (1 + x)^-eps = 0 + 2 ln 2 eps + O(eps^2)", Module[{r = ExpandParameterIntegrals[ParameterIntegral[x^(-2 - eps) (1 + x)^(-eps), x], eps, 1]},
   {Coefficient[r, eps, 0], Coefficient[r, eps, 1]}], {0, 2 Log[2]}, 10^-12];
(* the reference: at eps = -c, Int x^(c-1) (1-x)^(c-1) w(x), each half by x = t^(1/c) (x^(c-1) dx = dt/c: a smooth integrand) *)
endpointReference[c_, w_] := 1/c (NIntegrate[(1 - t^(1/c))^(c - 1) w[t^(1/c)], {t, 0, (1/2)^c}, WorkingPrecision -> 30] +
    NIntegrate[(1 - t^(1/c))^(c - 1) w[1 - t^(1/c)], {t, 0, (1/2)^c}, WorkingPrecision -> 30]);
With[{f = x^(-1 - eps) (1 - x)^(-1 - eps) (1 + x^2)^(-eps)},
  checkNumeric["both endpoints singular: the Laurent series to eps^2 against the integral at eps = -1/100",
   Normal[ExpandParameterIntegrals[ParameterIntegral[f, x], eps, 2]] /. eps -> -1/100, endpointReference[1/100, (1 + #^2)^(1/100) &], 10^-5]];
checkMessage[ExpandParameterIntegrals[ParameterIntegral[1/x, x], eps, 0], "an infrared divergence that eps does not regulate: message, $Failed", ExpandParameterIntegrals::ir, $Failed];
checkMessage[ExpandParameterIntegrals[SimplexIntegral[fp1^(-1 - eps) fp2^(-1 - eps) Power[1 + fp1 fp2, -eps], {fp1, fp2, fp3}], eps, 0],
  "more than one singular face of a simplex: message, $Failed", ExpandParameterIntegrals::ir, $Failed];

(* ---------------- 7. above threshold: Delta - i0 ---------------- *)
section["QED: above threshold"];
bubbleCoefficient = Coefficient[ExpandParameterIntegrals[IntegrateOneLoop[ml[fd[k, m] fd[k + p, m]], eps], eps, 0], eps, 0];
checkNumeric["bubble at p^2 = 5 m^2, eps^0: (i/16 pi^2)(2 - beta ln((1 + beta)/(1 - beta)) + i pi beta)", bubbleCoefficient /. {ScalarProduct[p, p] -> 5, m -> 1, RenormalizationScale -> 1},
  With[{beta = Sqrt[1 - 4/5]}, I/(16 Pi^2) (2 - beta Log[(1 + beta)/(1 - beta)] + I Pi beta)], 10^-12];
checkNumeric["the same below threshold, p^2 = 3 m^2: (i/16 pi^2)(2 - 2 Sqrt[3] ArcTan[1/Sqrt[3]])", bubbleCoefficient /. {ScalarProduct[p, p] -> 3, m -> 1, RenormalizationScale -> 1},
  I/(16 Pi^2) (2 - 2 Sqrt[4/3 - 1] ArcTan[1/Sqrt[4/3 - 1]]), 10^-12];
checkMessage[N[ParameterIntegral[1/(x - 1/2), x]], "1/Delta with a root inside: message, Indeterminate", ParameterIntegral::threshold, Indeterminate];
checkMessage[N[SimplexIntegral[1/(fp1 - fp2), {fp1, fp2, fp3}]], "the same on a simplex", SimplexIntegral::threshold, Indeterminate];
(* one NIntegrate for each piece between the roots, the roots to the working precision. The reference is
   Integrate[Log[Abs[x^2 - 3 x + 1]], {x, 0, 1}] - I Pi (Sqrt[5] - 1)/2 (the logarithm of a negative Delta - i0 on the
   root interval); the closed form below is what that Integrate returns (6 s), stored *)
check["N to 25 digits above a threshold (Log[x^2 - 3 x + 1], a root at x = 0.38): no message, all digits",
  Module[{v}, {Quiet[Check[v = N[ParameterIntegral[Log[x^2 - 3 x + 1], x], 25]; "no message", "message"]],
    Abs[v - ((-4 + Sqrt[5] Log[-5 (-3 + Sqrt[5])/2] + Sqrt[5] Log[(7 + 3 Sqrt[5])/10])/2 - I Pi (Sqrt[5] - 1)/2)] < 10^-23}],
  {"no message", True}];

(* ---------------- 8. diagrams of the package, results (Tests/QEDOneLoop.wl) ---------------- *)
section["QED: the self-energy, the vacuum polarization and the vertex"];
$qedReport = False;
Get[FileNameJoin[{testDirectory, "QEDOneLoop.wl"}]];
qedResults = qedOneLoop[];
res[name_] := name /. qedResults;
sigmaDiagrams = qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 2];
checkMessage[QEDIntegrand[sigmaDiagrams, {y -> p, x -> -p}, {ElectronPropagator -> {"Dirac", m}}], "bad rule: message, $Failed", QEDIntegrand::rule, $Failed];
checkMessage[QEDIntegrand[sigmaDiagrams, {y -> p, x -> -p}, {ElectronPropagator -> {"Fermion", m}, PhotonPropagator -> {"Photon", 1, 0}, QEDVertex -> {"Vertex", -I}}],
  "photon rule with three elements (old form): message, $Failed", QEDIntegrand::rule, $Failed];
checkMessage[QEDIntegrand[sigmaDiagrams, {y -> p, x -> p}, qedRules[1]], "external momenta that do not add up to zero: message, $Failed", QEDIntegrand::conservation, $Failed];
checkMessage[QEDIntegrand[qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 4], {y -> p, x -> -p}, qedRules[1]], "two-loop self-energy: message, $Failed",
  QEDIntegrand::loops, $Failed];
amputated[fields_, labels_, n_] := AmputateExternalLegs[ConnectedCorrelationFunction[qedZ0, fields, qedInteraction, n, "CouplingOrder" -> {e, n}], labels, qedLines];
checkMessage[QEDIntegrand[amputated[{electron[x1], electronbar[x2], electron[x3], electronbar[x4]}, {x1, x2, x3, x4}, 2],
   {x1 -> -p3, x2 -> p1, x3 -> p3 - p1 - p2, x4 -> p2}, qedRules[1]], "Moller/Bhabha tree (two open lines): message, $Failed", QEDIntegrand::spinor, $Failed];
check["tree vertex = +i e gamma^mu (electron charge -e, e > 0)", QEDIntegrand[qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 1], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[1],
    "Indices" -> {x3 -> mu}] // DiracSimplify, I e dp[g[mu]]];
check["closed loop: one trace, sign -1 in the weight", Module[{t = QEDIntegrand[qedVertexFunction[{photon[x], photon[y]}, {x, y}, 2], {y -> q, x -> -q}, qedRules[1]]},
   {Count[t, _DiracTrace, Infinity], Sign[t /. MinkowskiLoopIntegral[__] -> 1 /. e -> 1]}], {1, -1}];
(* Compton scattering at tree level: k1_mu M^{mu nu} = 0 between on-shell spinors (photon k1 in, k1^2 = 0) *)
check["Compton tree: 2 diagrams, Ward identity k1_mu M^{mu nu} = 0 on shell",
  Module[{dg = amputated[{electron[x1], electronbar[x2], photon[x3], photon[x4]}, {x1, x2, x3, x4}, 2], amp, ward},
   amp = QEDIntegrand[dg, {x2 -> p1, x3 -> k1, x4 -> p2 - p1 - k1, x1 -> -p2}, qedRules[1], "Indices" -> {x3 -> mu, x4 -> nu}];
   ward = DiracSimplify[FourVector[k1, mu] SpinorChain[{p2, m}, amp, {p1, m}]] /. ScalarProduct[k1, k1] -> 0;
   ward = ward /. FeynmanDenominator[qq_, mm_] :> 1/(FunctionalDiagrams`Private`scalarProductOf[qq, qq] - mm^2);
   {Length[DiagramTerms[dg]], Simplify[DiracSimplify[ward] /. {ScalarProduct[p1, p1] -> m^2, ScalarProduct[p2, p2] -> m^2, ScalarProduct[k1, k1] -> 0}]}], {2, 0}];
(* DiracSimplify of the unintegrated integrands: no message, and the same integrals *)
Module[{sInt = QEDIntegrand[sigmaDiagrams, {y -> p, x -> -p}, qedRules[xi]],
   pInt = QEDIntegrand[qedVertexFunction[{photon[x], photon[y]}, {x, y}, 2], {y -> q, x -> -q}, qedRules[xi], "Indices" -> {x -> mu, y -> nu}],
   vInt = QEDIntegrand[qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 3], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi],
     "Indices" -> {x3 -> mu}], simplified},
  check["DiracSimplify of the unintegrated self-energy, vacuum polarization and vertex (general xi): no message",
   Quiet[Check[simplified = DiracSimplify /@ {sInt, pInt, vInt}; "no message", "message"]], "no message"];
  (* the second route is qedOneLoop (Tests/QEDOneLoop.wl), which integrates the same integrands without simplifying them
     first: res["Sigma"] = DiracSimplify[I IntegrateOneLoop[sInt, eps]], res["PiMuNu"] = DiracSimplify[-I IntegrateOneLoop[pInt, eps]] *)
  check["... and the self-energy and the vacuum polarization integrate to the same results",
   Table[Expand[DiracSimplify[IntegrateOneLoop[simplified[[i]], eps]] - {-I res["Sigma"], I res["PiMuNu"]}[[i]]], {i, 2}], {0, 0}];
  (* a loop integral inside a spinor chain or a trace is taken out of it, so its indices are counted term by term *)
  check["DiracSimplify of the unintegrated vertex in a spinor chain: no message, not $Failed",
   Quiet[Check[If[DiracSimplify[SpinorChain[{p2, m}, vInt, {p1, m}]] === $Failed, "$Failed", "no message"], "message"]], "no message"];
  (* the second route: res["FormFactors"], the form factors of the integrated vertex in the chain, by qedOneLoop *)
  check["... and integrated it gives the same F1(0) and F2(0) as the integrated vertex in the chain (to eps^0, general xi)",
   Module[{ff = ExpandParameterIntegrals[VertexFormFactors[# /. ScalarProduct[p1, p2] -> m^2 - q2/2, mu, {p1, p2, m}][[{1, 2}]] /. q2 -> 0, eps, 0] &},
    Simplify[ff[DiracSimplify[-I/e IntegrateOneLoop[DiracSimplify[SpinorChain[{p2, m}, vInt, {p1, m}]], eps]]] -
      ExpandParameterIntegrals[res["FormFactors"][[{1, 2}]] /. q2 -> 0, eps, 0]]], {0, 0}]];
check["a loop momentum with the name of a momentum of the spinors is renamed when the integral is taken out of the chain",
  Module[{r = DiracSimplify[SpinorChain[{k, m}, ml[dp[sl[k]] fd[k, m]^3], {p, m}]]},
   MatchQ[r, MinkowskiLoopIntegral[fd[s_, m]^3 SpinorChain[{k, m}, dp[sl[s_]], {p, m}], {s_}] /; s =!= k]], True];
check["a loop integral whose integrand is 0 is 0: the trace of three gamma matrices inside it",
  DiracSimplify[DiracTrace[ml[dp[g[mu], sl[k], g[nu]] fd[k, m]^2]]], 0];
check["DiracTrace of an unintegrated loop integral: the trace taken inside it",
  DiracSimplify[DiracTrace[ml[dp[g[mu], sl[k], g[mu], sl[k]] fd[k, m]^3]]], ml[8 ScalarProduct[k, k] fd[k, m]^3 - 4 d ScalarProduct[k, k] fd[k, m]^3]];
sigmaPoleOf[externals_, opts___] := Module[{s = DiracSimplify[I IntegrateOneLoop[QEDIntegrand[sigmaDiagrams, externals, qedRules[1], opts], eps]]},
   Simplify[ExpandParameterIntegrals[s, eps, -1] eps/alpha]];
check["Sigma pole with the loop momentum kk[1] and with the external momentum P[1] (indexed): (alpha/(4 pi eps))(4 m - pslash)",
  Simplify[{sigmaPoleOf[{y -> p, x -> -p}, "LoopMomentum" -> kk[1]], sigmaPoleOf[{y -> P[1], x -> -P[1]}] /. P[1] -> p} - (4 m dp[] - dp[sl[p]])/(4 Pi)], {0, 0}];
check["Sigma_2 pole (alpha/(4 pi eps)) (m (3 + xi) - xi pslash)  [P&S sec. 10.3 at xi = 1, Grozin eq. 3.64 for any xi]", Simplify[res["SigmaPole"] - alpha/(4 Pi eps) {3 + xi, -xi}], {0, 0}];
checkSimplified["Z_m - 1 = -3 alpha/(4 pi eps), gauge independent [Grozin eq. 3.104]", res["ZmMinus1"] + 3 alpha/(4 Pi eps)];
checkSimplified["MS-bar delta2 = -xi alpha/(4 pi eps) (gauge dependent; 0 in Landau gauge)", res["delta2MS"] + xi alpha/(4 Pi eps)];
checkSimplified["MS-bar counterterm deltam = Z2 m0 - m = -(3 + xi) m alpha/(4 pi eps) [P&S sec. 10.3 at xi = 1]", res["deltamCountertermMS"] + (3 + xi) m alpha/(4 Pi eps)];
checkSimplified["on-shell mass shift (m - m0)/m = Sigma(m)/m = (alpha/(4 pi)) (3/eps + 4 + 3 ln(mu^2/m^2)) [P&S sec. 10.3]", res["massShiftOS"] - alpha/(4 Pi) (3/eps + 4 + 3 L)];
checkSimplified["on-shell delta Z2 = -(alpha/(4 pi)) (3/eps + 4 + 3 ln(mu^2/m^2)) (IR in dim. reg., any xi)", res["deltaZ2OS"] + alpha/(4 Pi) (3/eps + 4 + 3 L)];
checkSimplified["on-shell counterterm deltam = Z2 m0 - m = m delta Z2 - Sigma(m) = -2 m (alpha/(4 pi))(3/eps + 4 + 3 ln(mu^2/m^2))",
  res["deltamCountertermOS"] + 2 m alpha/(4 Pi) (3/eps + 4 + 3 L)];
checkNumeric["on-shell delta Z2 with a photon mass: -(alpha/4pi)(1/eps + 4 + ln(mu^2/m^2) + 2 ln(lambda^2/m^2)) at lambda = 10^-4 m",
  Coefficient[res["deltaZ2OSPhotonMass"], eps, 0] /. {m -> 1, RenormalizationScale -> 3, lambda -> 10^-4, e -> 1},
  -1/(16 Pi^2) (4 + Log[9] + 2 Log[10^-8]), 10^-5];
check["Pi^{mu nu} independent of xi", FreeQ[res["PiMuNu"], xi], True];
(* NIntegrate of an integrand that is 0 up to rounding: its convergence messages are expected *)
check["transversality q_mu Pi^{mu nu} = 0 (exact in d, numerically at eps = 3/10)", Chop[Quiet[N[res["Transversality"] /. eps -> 3/10], {NIntegrate::slwcon, NIntegrate::ncvb}]], 0];
check["transversality exact in d (integrand of A + q^2 B by FullSimplify)", Module[{pmn = res["PiMuNu"]},
   Simplify[ReduceParameterIntegrals[coefficientOf[pmn, LorentzMetric[mu, nu]] + ScalarProduct[q, q] coefficientOf[pmn, FourVector[q, mu] FourVector[q, nu]]] /.
     (h : ParameterIntegral | SimplexIntegral)[fi_, xx_] :> h[FullSimplify[fi], xx]]], 0];
checkSimplified["Pi_2 pole = -alpha/(3 pi eps) = delta3", res["PiPole"] + alpha/(3 Pi eps)];
checkSimplified["beta(e) = e^3/(12 pi^2)", res["beta"] - e^3/(12 Pi^2)];
checkNumeric["Pi-hat_2(-3 m^2) against P&S sec. 7.5", res["PiHat"] /. {ScalarProduct[q, q] -> -3, m -> 1, RenormalizationScale -> 1, e -> 1},
  -2/(4 Pi^2) NIntegrate[x (1 - x) Log[1/(1 + 3 x (1 - x))], {x, 0, 1}], 10^-10];
(* above threshold: Im Pi-hat(q^2 + i0) = -(alpha/3) Sqrt[1 - 4 m^2/q^2] (1 + 2 m^2/q^2) (P&S eq. 7.92); the real part computed
   independently, with the interval split at the branch points *)
checkNumeric["Pi-hat_2(5 m^2 + i0) = -0.0157393521358 - (alpha/3) Sqrt[1 - 4/5] (1 + 2/5) i  [P&S eq. 7.92]",
  res["PiHat"] /. {ScalarProduct[q, q] -> 5, m -> 1, RenormalizationScale -> 1, e -> 1}, -0.0157393521358 - I (1/(4 Pi))/3 Sqrt[1 - 4/5] (1 + 2/5), 10^-12];
checkNumeric["Im B of Sigma_2 at p^2 = 2 m^2 (Feynman gauge) = -(alpha/2) Int_(1/2)^1 x dx", Im[Coefficient[ExpandParameterIntegrals[coefficientOf[res["Sigma"] /. xi -> 1,
       dp[sl[p]]], eps, 0], eps, 0] /. {ScalarProduct[p, p] -> 2, m -> 1, RenormalizationScale -> 1, e -> 1}], -(1/(4 Pi))/2 (1/2 - 1/8), 10^-12];
checkSimplified["F2(0) = alpha/(2 pi) (Schwinger), any xi", res["F2zero"] - alpha/(2 Pi)];
checkSimplified["UV pole of F1 = xi alpha/(4 pi eps) = -delta2 (Z1 = Z2)", res["F1pole"] - xi alpha/(4 Pi eps)];
checkSimplified["F1(0) - 1 = -delta Z2 on shell (Ward identity, IR in dim. reg.)", res["F1zero"] + res["deltaZ2OS"]];
checkSimplified["F3(0) = 0", ReduceParameterIntegrals[res["FormFactors"][[3]] /. q2 -> 0]];
checkNumeric["F2(-m^2) against P&S sec. 6.3 (Feynman gauge)", res["F2minusM2"] /. {m -> 1, e -> 1},
  1/(8 Pi^2) NIntegrate[2 z (1 - z)/((1 - z)^2 + x (1 - x - z)), {z, 0, 1}, {x, 0, 1 - z}, PrecisionGoal -> 10], 10^-5];
checkNumeric["Ward identity with a photon mass: F1(0) - 1 + delta Z2 = 0 (lambda = 10^-3 m, Feynman gauge)",
  Coefficient[ExpandParameterIntegrals[res["FormFactorsPhotonMass"][[1]] /. {q2 -> 0, xi -> 1}, eps, 0] + res["deltaZ2OSPhotonMass"], eps, 0] /.
   {m -> 1, lambda -> 10^-3, RenormalizationScale -> 1, e -> 1}, 0, 10^-8];
(* F1(q^2 = -m^2) with a massless photon: infrared divergent; the singular corner of the simplex (photon parameter -> 1) is
   subtracted. Exact values (x = s t, y = s (1 - t), the s integral done exactly): eps^-1
   0.0226861527334153 e^2, eps^0 0.0275666622484165 e^2 (mu = m); the pole is UV + IR, P&S eq. 6.65 translated; independent of xi *)
(* F1 of the vertex of qedOneLoop (general xi, massless photon), at xi = 1 and 2/5 *)
f1Massless[xiv_] := ExpandParameterIntegrals[res["FormFactors"][[1]] /. {xi -> xiv, q2 -> -1, m -> 1, RenormalizationScale -> 1, e -> 1}, eps, 0];
Do[With[{f = f1Massless[xiv]}, checkNumeric["F1(-m^2) - 1, massless photon (IR in dim. reg.), xi = " <> ToString[xiv, InputForm] <> ": {eps^-1, eps^0} against the exact expansion",
    {Coefficient[f, eps, -1], Coefficient[f, eps, 0]}, {0.0226861527334153, 0.0275666622484165}, 10^-9]], {xiv, {1, 2/5}}];

(* ---------------- 9. formulas in LaTeX (ExpressionToLaTeX; \slashed needs \usepackage{slashed}) ---------------- *)
section["QED: LaTeX"];
check["LaTeX of a product of Dirac matrices: pslash, p1slash, a slashed sum written as its terms",
  ExpressionToLaTeX[DiracProduct[GammaMatrix[mu], SlashedMomentum[p1], m + SlashedMomentum[p - k]]],
  "\\gamma^{\\mu}\\,\\slashed{p}{}_{1}\\,\\left(m + \\slashed{p} - \\slashed{k}\\right)"];
check["LaTeX of a trace, the metric, the unit matrix and d",
  ExpressionToLaTeX[DiracTrace[DiracProduct[I GammaMatrix[mu], SlashedMomentum[q]]] + SpacetimeDimension LorentzMetric[mu, nu] DiracProduct[]],
  "\\operatorname{tr}\\left[\\left(i\\,\\gamma^{\\mu}\\right)\\,\\slashed{q}\\right] + d\\,g^{\\mu\\nu}\\,\\mathbf{1}"];
check["LaTeX: the unit matrix after a number and a scalar product (4 a.b 1, not 4 1 a.b)", ExpressionToLaTeX[DiracOrder[DiracProduct[GammaMatrix[mu], SlashedMomentum[a],
     SlashedMomentum[b], GammaMatrix[mu]]]], "d\\,\\slashed{a}\\,\\slashed{b} + 4\\,a\\cdot b\\,\\mathbf{1} - 4\\,\\slashed{a}\\,\\slashed{b}"];
check["LaTeX of a spinor chain, a four-vector and scalar products",
  ExpressionToLaTeX[SpinorChain[{p2, m}, DiracProduct[GammaMatrix[mu]], {p1, m}] FourVector[p1 + p2, nu] + ScalarProduct[p1, p2] ScalarProduct[q, q]],
  "p_{1}\\cdot p_{2}\\,q^{2} + (p_{1} + p_{2})^{\\nu}\\,\\bar{u}(p_{2})\\,\\gamma^{\\mu}\\,u(p_{1})"];
check["LaTeX of a Minkowski loop integral: the lines in one fraction, the number outside",
  ExpressionToLaTeX[e^2 MinkowskiLoopIntegral[-I DiracProduct[GammaMatrix[rho], m + SlashedMomentum[p - k], GammaMatrix[rho]] FeynmanDenominator[k, 0] FeynmanDenominator[p - k, m], {k}]],
  "-i\\,e^{2}\\,\\int_{k}\\frac{\\gamma^{\\rho}\\,\\left(m + \\slashed{p} - \\slashed{k}\\right)\\,\\gamma^{\\rho}}{\\left((p - k)^{2} - m^{2}\\right)\\,k^{2}}"];
check["LaTeX of Feynman-parameter integrals: the fraction before the integral; the simplex",
  {ExpressionToLaTeX[e^2 ParameterIntegral[FeynmanParameter[1] Log[(m^2 - FeynmanParameter[1] ScalarProduct[p, p])/RenormalizationScale^2]/(8 Pi^2), FeynmanParameter[1]]],
   ExpressionToLaTeX[SimplexIntegral[FeynmanParameter[1]/(FeynmanParameter[2] + FeynmanParameter[3]), {FeynmanParameter[1], FeynmanParameter[2], FeynmanParameter[3]}]]},
  {"\\frac{e^{2}}{8\\,\\pi^{2}}\\,\\int_0^1 dx_{1}\\,x_{1}\\,\\log\\left(\\frac{m^{2} - x_{1}\\,p^{2}}{\\mu^{2}}\\right)",
   "\\int_0^1 dx_{1}\\,dx_{2}\\,dx_{3}\\,\\delta(1 - x_{1} - x_{2} - x_{3})\\,\\frac{x_{1}}{x_{2} + x_{3}}"}];
check["LaTeX of the default loop momentum LoopMomentum[1]: ell_1 (p_1 is an external momentum); a name given wins; p_1 in a momentum sum",
  {ExpressionToLaTeX[#], ExpressionToLaTeX[#, "Names" -> {LoopMomentum[1] -> "k"}]} &[MinkowskiLoopIntegral[FourVector[LoopMomentum[1], mu] FeynmanDenominator[p1 - LoopMomentum[1], m] FeynmanDenominator[LoopMomentum[1], 0],
     {LoopMomentum[1]}]] ~Join~ {ExpressionToLaTeX[MomentumSum[FourVector[LoopMomentum[1], mu], {LoopMomentum[1]}]]},
  {"\\int_{\\ell_{1}}\\frac{\\ell_{1}^{\\mu}}{\\left((p_{1} - \\ell_{1})^{2} - m^{2}\\right)\\,\\ell_{1}^{2}}",
   "\\int_{k}\\frac{k^{\\mu}}{\\left((p_{1} - k)^{2} - m^{2}\\right)\\,k^{2}}", "\\sum_{p_{1}}p_{1}^{\\mu}"}];
check["LaTeX of the slashed default loop momentum: the slash on the letter ell, the subscript after it (as for p1); a name given wins",
  {ExpressionToLaTeX[#], ExpressionToLaTeX[#, "Names" -> {LoopMomentum[1] -> "k"}]} &[MinkowskiLoopIntegral[DiracProduct[SlashedMomentum[LoopMomentum[1]],
      SlashedMomentum[p1 - LoopMomentum[1]]] FeynmanDenominator[LoopMomentum[1], m], {LoopMomentum[1]}]],
  {"\\int_{\\ell_{1}}\\frac{\\slashed{\\ell}{}_{1}\\,\\left(\\slashed{p}{}_{1} - \\slashed{\\ell}{}_{1}\\right)}{\\ell_{1}^{2} - m^{2}}",
   "\\int_{k}\\frac{\\slashed{k}\\,\\left(\\slashed{p}{}_{1} - \\slashed{k}\\right)}{k^{2} - m^{2}}"}];
check["LaTeX of the default loop momentum in an integrand taken out of its integral: ell_1 (Dirac and Lorentz objects)",
  ExpressionToLaTeX /@ {DiracProduct[SlashedMomentum[LoopMomentum[1]], GammaMatrix[mu]], ScalarProduct[LoopMomentum[1], p1]},
  {"\\slashed{\\ell}{}_{1}\\,\\gamma^{\\mu}", "p_{1}\\cdot \\ell_{1}"}];
check["LaTeX of a squared line", ExpressionToLaTeX[FourVector[k, mu] FeynmanDenominator[k, m]^2 FeynmanDenominator[k + q, m]],
  "\\frac{k^{\\mu}}{\\left(k^{2} - m^{2}\\right)^{2}\\,\\left((k + q)^{2} - m^{2}\\right)}"];
check["LaTeX of the tree vertex of the package: i e gamma^mu", ExpressionToLaTeX[DiracSimplify[QEDIntegrand[qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]},
     {x1, x2, x3}, 1], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[1], "Indices" -> {x3 -> mu}]]], "i\\,e\\,\\gamma^{\\mu}"];

(* ------------------------------------------------------------------------- *)
(* the effective actions and operator traces (section 20 of the package), in their own file, also runnable alone; it starts
   afresh, as in a new session: the symbols of the sections above are cleared and the declarations reset *)
section["effective actions (EffectiveActions.wl)"];
Get[FileNameJoin[{testDirectory, "EffectiveActions.wl"}]];

(* ------------------------------------------------------------------------- *)
(* superfields (the Nambu field (psi, psibar) with an antisymmetric propagator), in their own file, also runnable alone;
   a fresh start as above *)
section["superfields (Superfields.wl)"];
Get[FileNameJoin[{testDirectory, "Superfields.wl"}]];

(* ------------------------------------------------------------------------- *)
section["loading twice"];
Get[FileNameJoin[{testDirectory, "..", "FunctionalDiagrams.wl"}]];
check["package loaded again", $FunctionalDiagramsVersion, "2.4.0"];
(* the banner printed when the package is loaded in a notebook (never by a script): built here without printing it *)
With[{banner = FunctionalDiagrams`Private`loadBanner[]},
  check["load banner: logo, the attribution of term (b) of NOTICE, version, license and links",
    {Head[banner], Count[banner, _Graphics, Infinity],
     StringContainsQ[StringJoin[Cases[banner, _String, Infinity]], "FunctionalDiagrams, by Nazzareno Africani"],
     !FreeQ[banner, s_String /; StringContainsQ[s, $FunctionalDiagramsVersion]], !FreeQ[banner, s_String /; StringMatchQ[s, "GPL*3.0*or*later"]],
     Cases[banner, Hyperlink[_, url_] :> url, Infinity]},
    {Style, 1, True, True, True, {"https://github.com/n4sd4q/FunctionalDiagrams/blob/main/CITATION.cff",
      "https://github.com/n4sd4q/FunctionalDiagrams",
      "https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Documentation/FunctionalDiagrams_Manual.pdf"}}]];
(* the full logo (make_logo.py): two arrow heads on the loop, each wider at its base than twice the line of the loop, so
   that the arrows read at small sizes; the small badge of the banner has no arrows *)
With[{full = FunctionalDiagrams`Private`packageLogo[64, False], small = FunctionalDiagrams`Private`packageLogo[30]},
  check["logo: arrow heads wider than twice the loop; the small badge without them",
    {Length[Cases[full, _Polygon, Infinity]],
     Min[Cases[full, Polygon[{_, b_, c_}] :> Norm[b - c], Infinity]] > 2 128 Last[Cases[full, Thickness[t_] :> t, Infinity]],
     Count[small, _Polygon, Infinity]},
    {2, True, 0}]];

Print["\n", passed, " passed, ", failed, " failed", If[failed > 0, ": " <> StringRiffle[failures, ", "], ""], "   (", Round[AbsoluteTime[] - startTime], " s)"];
