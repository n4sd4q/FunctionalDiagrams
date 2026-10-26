(* ::Package:: *)

(* The examples of the reference manual (FunctionalDiagrams_Manual.pdf). Every example is a piece of code, kept here as a
   string, evaluated, and written to examples/: NAME.wl (the code, as shown in the manual), and its output as
   NAME.tex (a formula, by ExpressionToLaTeX), NAME_pictures.tex (the diagrams, by DiagramToTikZ) or NAME.txt (any other
   output, in InputForm). build_manual.sh typesets the manual.

   Run it in a fresh kernel: open this file in Mathematica and choose Run All Code, or evaluate
   Get["<path of this folder>/make_examples.wl"], or run  wolfram -script make_examples.wl  (or wolframscript -file) in this
   folder; it takes about twenty seconds. *)

$IterationLimit = 10^8;
SetDirectory[If[$InputFileName =!= "", DirectoryName[$InputFileName], NotebookDirectory[]]];
Get[FileNameJoin[{ParentDirectory[Directory[]], "FunctionalDiagrams.wl"}]];
If[! DirectoryQ["examples"], CreateDirectory["examples"]];

pictureOptions = {"Scale" -> 0.6, "LineWidth" -> 0.55, "LabelFont" -> "\\scriptsize"};
(* example[name, code, output, ext, options]: output is "Formula", "Pictures", "Both" or "Text"; ext are the external
   labels of the pictures; options go to ExpressionToLaTeX and DiagramToTikZ *)
example[name_String, code_String, output_String, ext_List : {}, opts___] := Module[{result},
   Export["examples/" <> name <> ".wl", code, "Text"];
   result = ToExpression[code];
   If[MemberQ[{"Formula", "Both"}, output],
    Export["examples/" <> name <> ".tex", ExpressionToLaTeX[result, Sequence @@ Select[{opts}, MemberQ[{"Names", "Labels", "Subscripts"}, #[[1]]] &]],
     "Text"]];
   If[MemberQ[{"Pictures", "Both"}, output],
    Export["examples/" <> name <> "_pictures.tex", DiagramToTikZ[result, ext, Sequence @@ Select[{opts}, ! MemberQ[{"Names", "Subscripts"}, #[[1]]] &],
      Sequence @@ pictureOptions], "Text"]];
   (* a string as it is; anything else in InputForm (with -> for rules, as Mathematica writes it) *)
   If[output === "Text", Export["examples/" <> name <> ".txt",
     If[StringQ[result], result, StringReplace[ToString[result, InputForm], {"\[Rule]" -> "->", FromCharacterCode[8702] -> "->"}]], "Text"]];
   Print[name, " done"];
   result];

(* ---------------------------------------------------------------- expressions *)
example["canonical_names", "DeclareSource[J];
SimplifyDiagrams[IntegralOver[f[x, u] g[u, v] f[v, y], {u, v}]]", "Formula"];
example["delta_integrated", "SimplifyDiagrams[IntegralOver[DeltaFunction[x, u] f[u, y], {u}]]", "Formula"];
example["delta_external", "SimplifyDiagrams[DeltaFunction[a, b] f[a, b]]", "Formula"];
example["nested_integrals", "SimplifyDiagrams[IntegralOver[f[x, u] Exp[IntegralOver[g[u, v] J[v], {v}]], {u}]]", "Formula"];

(* ---------------------------------------------------------------- declarations *)
example["symmetric", "DeclareSymmetric[Q];
SimplifyDiagrams[IntegralOver[Q[u, x] Q[y, u], {u}] - IntegralOver[Q[x, v] Q[v, y],
    {v}]]", "Formula"];
example["antisymmetric", "DeclareAntisymmetric[A];
{A[b, a], A[a, a], A[c, a, b]}", "Text"];
example["discrete_values", "DeclareDiscreteIndexValues[{up, down}];
{DeltaFunction[up, down], FunctionalDerivative[IntegralOver[J[s] h[s], {s}], J[up]]}", "Text"];
example["grassmann_odd", "DeclareGrassmannOdd[c];
SimplifyDiagrams[c[a] ** c[b] + c[b] ** c[a]]", "Formula"];
example["qcd_gluon_self_energy", "DeclareSource[Jgluon]; DeclareGrassmannSource[chi, chibar, sigma, sigmabar];
DeclareField[gluon, Jgluon]; DeclareField[quark, chibar];
DeclareField[quarkbar, chi, -1]; DeclareField[ghost, sigmabar];
DeclareField[ghostbar, sigma, -1];
DeclareSymmetric[Dg, V3, V4]; DeclareLocalVertex[Vq, V3, V4, Vc];
DeclareLineStyle[Dg, \"Curly\"]; DeclareLineStyle[Dc, \"DottedArrow\"];
DeclareDirectedPropagator[Sq];
Zqcd = Exp[1/2 IntegralOver[Jgluon[x] Dg[x, y] Jgluon[y], {x, y}] +
    IntegralOver[Sq[x, y] chibar[x] ** chi[y], {x, y}] +
    IntegralOver[Dc[x, y] sigmabar[x] ** sigma[y], {x, y}]];
qcd = IntegralOver[g Vq[x, y, z] quarkbar[x] ** quark[y] gluon[z] +
     g Vc[x, y, z] ghostbar[x] ** ghost[y] gluon[z] +
     g/6 V3[x, y, z] gluon[x] gluon[y] gluon[z], {x, y, z}] +
   g^2/24 IntegralOver[V4[w, x, y, z] gluon[w] gluon[x] gluon[y] gluon[z],
     {w, x, y, z}];
lines = {Dg, Sq, Dc};
correlation = OrderPart[ConnectedCorrelationFunction[Zqcd, {gluon[a], gluon[b]},
    qcd, 2], g, 2];
ParticleIrreducibleDiagrams[AmputateExternalLegs[correlation, {a, b}, lines], 1,
  {a, b}, lines]", "Pictures", {a, b}];
example["blobs", "DeclareDirectedPropagator[Gb];
DeclareBlob[Kb, \"\\\\Gamma^0\", \"Shape\" -> \"Box\", \"Fill\" -> \"CrossHatched\"];
DeclareBlob[Fb, \"F\", \"Shape\" -> \"Box\"];
Kb[x1, x2, x3, x4] + BetheSalpeterLadder[Kb[x1, x2, x3, x4], Fb[x1, x2, x3, x4],
  {x1, x2, x3, x4}, \"ParticleHole\", Gb]", "Pictures", {x1, x2, x3, x4}, "LegDirections" -> "Alternating"];

(* ---------------------------------------------------------------- derivatives *)
example["derivative_source", "FunctionalDerivative[J[x], J[y]]", "Formula"];
example["derivative_power", "FunctionalDerivative[IntegralOver[f[x] J[x]^3, {x}], J[a]]", "Formula"];
example["derivative_exp", "FunctionalDerivative[Exp[IntegralOver[f[x] J[x], {x}]], J[a], J[b]]", "Formula"];
example["derivative_function", "FunctionalDerivative[Sin[IntegralOver[f[x] J[x], {x}]], J[a]]", "Formula"];
example["derivative_nested", "FunctionalDerivative[IntegralOver[f[x] Exp[IntegralOver[g[x, y] J[y], {y}]], {x}],
    J[a]]", "Formula"];
example["derivative_list", "FunctionalDerivative[IntegralOver[Q[x, y] J[x] J[y], {x, y}], {J[a], J[b]}]", "Formula"];
example["grassmann_sources", "DeclareGrassmannSource[eta, etabar];
{FunctionalDerivative[IntegralOver[M[x, y] etabar[x] ** eta[y], {x, y}], etabar[a],
    eta[b]],
 FunctionalDerivative[IntegralOver[M[x, y] etabar[x] ** eta[y], {x, y}], eta[b],
     etabar[a]]}", "Text"];
example["implicit_rule", "DeclareSymmetric[PropagatorOfJ, V];
DeclareFunctionalDerivative[PropagatorOfJ[p_, q_], J[r_],
    PropagatorOfJ[p, r] PropagatorOfJ[r, q]];
FunctionalDerivative[IntegralOver[V[u, v] PropagatorOfJ[u, v], {u, v}], J[a], J[b]]", "Formula", {}, "Names" -> {PropagatorOfJ -> "G"}];
example["derivative_propagator", "DeclareDirectedPropagator[G];
PhiHF = 1/2 IntegralOver[V[u, v] G[u, u] G[v, v],
    {u, v}] - 1/2 IntegralOver[V[u, v] G[u, v] G[v, u], {u, v}];
FunctionalDerivative[PhiHF, G[y, x]]", "Formula"];
example["derivative_two_index", "FunctionalDerivative[PhiHF, V[x, y]]", "Formula"];
example["derivative_maximum_order", "FunctionalDerivative[Log[1 + lambda IntegralOver[f[x] J[x], {x}] +
    lambda^2 IntegralOver[g[x, y] J[x] J[y], {x, y}]], J[a], J[b], \"MaximumOrder\" -> {lambda, 2}]", "Formula"];
example["diagram_terms", "DiagramTerms[IntegralOver[f[x, u] g[u, y], {u}] + 2 IntegralOver[f[x, v] g[v, y],
    {v}] + h[x, y]]", "Text"];
example["sources_to_zero", "SetSourcesToZero[FunctionalDerivative[Exp[IntegralOver[f[x] J[x], {x}]], J[a]]]", "Formula"];

(* ---------------------------------------------------------------- fields and Wick's theorem *)
example["free_boson", "DeclareSymmetric[D0]; DeclareField[phi, J];
Z0 = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
FieldDerivative[Z0, {phi[a], phi[b]}]", "Formula"];
example["boson_two_point", "FieldExpectationValue[Z0, {phi[a], phi[b]}]", "Formula"];
example["boson_four_point", "GaussianExpectationValue[Z0, {phi[a], phi[b], phi[c], phi[d]}]", "Both", {a, b, c, d}];
example["free_fermion", "DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Zf = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
FieldExpectationValue[Zf, {psi[a], psibar[b]}]", "Formula"];
example["fermion_four_point", "GaussianExpectationValue[Zf, {psi[a], psi[b], psibar[c], psibar[d]}]", "Formula"];
example["non_gaussian", "DeclareSymmetric[W2];
Zw = Exp[IntegralOver[W1[x] J[x], {x}] + 1/2 IntegralOver[W2[x, y] J[x] J[y],
    {x, y}] +
    1/24 IntegralOver[W4[x] J[x]^4, {x}]];
FieldExpectationValue[Zw, {phi[a], phi[a]}]", "Formula"];

example["cumulants", "SetSourcesToZero[FunctionalDerivative[Log[Zw], J[a], J[b]]]", "Formula"];

(* ---------------------------------------------------------------- perturbation theory *)
example["phi4_propagator", "Phi4Vertex = lambda/24 IntegralOver[phi[z]^4, {z}]; DeclareCoupling[lambda];
PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 1]", "Both", {a, b}];
example["phi4_vacuum", "ConnectedVacuumDiagrams[Z0, Phi4Vertex, 2]", "Both"];
example["phi4_order_part", "OrderPart[ConnectedVacuumDiagrams[Z0, Phi4Vertex, 2], lambda, 2]", "Formula"];
example["electron_gas_first_order", "DeclareSymmetric[V]; DeclareDirectedPropagator[G0]; DeclareInteractionLine[V];
CoulombVertex = 1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x],
    {x, y}];
-PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]}, CoulombVertex, 1]", "Both", {x, y}];
(* <rho(a) rho(b)>_c = -dG(a, a)/dU(b), with a local potential U on the bare line, at first order *)
example["density_correlation", "DeclareSource[LocalPotential];
DeclareFunctionalDerivative[G0[p_, q_], LocalPotential[r_], G0[p, r] G0[r, q]];
green1 = -PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]}, CoulombVertex, 1];
rhoRho = -FunctionalDerivative[green1 /. {x -> a, y -> a}, LocalPotential[b]];
OrderPart[rhoRho, V, 1]", "Pictures", {a, b}];
(* the shortcut gives the same *)
example["density_correlation_derivative", "SimplifyDiagrams[rhoRho - ConnectedCorrelationFunction[Zf,
    {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1]]", "Text"];
example["order_by_head", "OrderPart[-PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]}, CoulombVertex,
    1], V, 1]", "Formula"];

(* ---------------------------------------------------------------- classes of diagrams *)
example["phi4_second_order", "phi4Propagator = PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex,
    {2}]", "Pictures", {a, b}];
example["one_particle_irreducible", "ParticleIrreducibleDiagrams[phi4Propagator, 1, {a, b}, {D0}]", "Pictures", {a, b}];
example["amputated", "AmputateExternalLegs[ParticleIrreducibleDiagrams[phi4Propagator, 1, {a, b}, {D0}],
    {a, b}, {D0}]", "Both", {a, b}];
example["electron_gas_second_order", "greenFunction = -PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]},
    CoulombVertex, {2}];
selfEnergy2 = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction, 1,
    {x, y}, {G0}], {x, y}, {G0}]", "Pictures", {x, y}];
example["skeleton", "SkeletonDiagrams[selfEnergy2, {x, y}, {G0}]", "Pictures", {x, y}];
example["density_reducible", "ReducibleDiagrams[ConnectedCorrelationFunction[Zf,
    {psibar[a], psi[a], psibar[b], psi[b]},
   CoulombVertex, {1}], {{a}, {b}}, 1, {V}]", "Pictures", {a, b}];
example["closed_loops", "ClosedLoopCount[#, {G0}] & /@ DiagramTerms[greenFunction]", "Text"];

(* ---------------------------------------------------------------- functionals and vertices *)
example["luttinger_ward", "DeclareDirectedPropagator[G];
Phi = LuttingerWardFunctional[Zf, CoulombVertex, 2, G0 -> G]", "Both"];
example["self_energy", "sigmaG = FunctionalDerivative[Phi, G[y, x]]", "Both", {x, y}];
example["self_energy_shortcut", "SimplifyDiagrams[SelfEnergy[Phi, G, {x, y}] - sigmaG]", "Text"];
example["irreducible_vertex", "FunctionalDerivative[OrderPart[Phi, V, 1], G[x4, x3], G[x2, x1]]", "Both", {x1, x2, x3, x4}, "LegDirections" -> "Alternating"];
example["irreducible_vertex_shortcut", "SimplifyDiagrams[IrreducibleVertex[Phi, G, {x1, x2, x3, x4}] -
  FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]]]", "Text"];
example["dyson_series", "DysonSeries[sigmaG /. {x -> s1, y -> s2, G -> G0}, {s1, s2}, G0, {x, y}, 2,
  \"MaximumOrder\" -> {V, 2}]", "Pictures", {x, y}];
(* the skeletons of the fourth derivative of the 1PI action *)
example["full_vertex_phi4", "DeclareSymmetric[DressedPropagator];
gammaPhi = EffectiveAction1PI[Z0, Phi4Vertex, 2, phi -> phiMean, D0 -> D0];
gamma4Phi = SetSourcesToZero[FunctionalDerivative[gammaPhi, phiMean[a], phiMean[b],
   phiMean[c], phiMean[d]]];
vertexPhi = SkeletonDiagrams[gamma4Phi, {a, b, c, d},
    {D0}] /. D0 -> DressedPropagator", "Both", {a, b, c, d},
  "Names" -> {DressedPropagator -> "D"}];
(* the shortcut gives the same *)
example["full_vertex_derivative", "SimplifyDiagrams[vertexPhi - PerturbativeFullVertex[Z0, Phi4Vertex, 2, {phi},
   {a, b, c, d}, D0 -> DressedPropagator]]", "Text"];

(* ---------------------------------------------------------------- channels and parquet *)
example["ladder_ph", "kernel = DeltaFunction[x1, x2] DeltaFunction[x3, x4] V[x1, x3];
BetheSalpeterLadder[kernel, kernel, {x1, x2, x3, x4}, \"ParticleHole\", G]", "Both", {x1, x2, x3, x4}, "LegDirections" -> "Alternating"];
example["parquet_phi4", "vertex = PerturbativeFullVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, c, d},
    D0 -> DressedPropagator];
Map[Length[DiagramTerms[#]] &, ParquetDecomposition[OrderPart[vertex, lambda, 2],
    {a, b, c, d}, {DressedPropagator},
  \"Statistics\" -> \"Boson\"]]", "Text"];
example["check_parquet_phi4", "CheckParquetEquations[vertex, {a, b, c, d}, DressedPropagator, lambda, 2,
  \"Statistics\" -> \"Boson\"][\"Residuals\"]", "Text"];
(* the superfield (psi, psibar): one source, one antisymmetric propagator, one antisymmetric vertex, as for phi^4 *)
nambuNames = {NambuPropagator -> "\\mathcal G", NambuVertex -> "\\mathcal V", AnomalousGreenFunction -> "\\mathcal G_{\\psi\\psi}",
   AnomalousGreenFunctionBar -> "\\mathcal G_{\\bar\\psi\\bar\\psi}"};
example["superfield_phi", "DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex];
DeclareLocalVertex[NambuVertex];
Znambu = Exp[1/2 IntegralOver[
    BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b], {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d]
    NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
PhiNambu = LuttingerWardFunctional[Znambu, nambuInteraction, 2,
  BareNambuPropagator -> NambuPropagator]", "Both", {},
  "Names" -> nambuNames];
example["superfield_components", "DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar];
DeclareLineStyle[AnomalousGreenFunction, \"DivergingArrows\"];
DeclareLineStyle[AnomalousGreenFunctionBar, \"ConvergingArrows\"];
nambuRules = {NambuField -> {psi, psibar},
   NambuPropagator -> {G, AnomalousGreenFunction, AnomalousGreenFunctionBar},
   NambuVertex -> CoulombVertex};
SuperfieldComponents[OrderPart[PhiNambu, NambuVertex, 1], {}, nambuRules]", "Both", {}, "Names" -> nambuNames];
example["superfield_pp_derivative", "PhiComponents = SuperfieldComponents[PhiNambu, {}, nambuRules];
ppKernel = -4 FunctionalDerivative[PhiComponents, AnomalousGreenFunction[x2, x4],
     AnomalousGreenFunctionBar[x1, x3]] /. {AnomalousGreenFunction[__] -> 0,
     AnomalousGreenFunctionBar[__] -> 0};
OrderPart[ppKernel, V, 1]", "Both", {x1, x2, x3, x4},
  "LegDirections" -> "Alternating", "ExternalOrder" -> {x4, x2, x3, x1}];
example["superfield_gap", "sigmaNambu = -2 FunctionalDerivative[PhiNambu, NambuPropagator[a, b]];
SuperfieldComponents[OrderPart[sigmaNambu, NambuVertex, 1],
  {a -> psibar[x1], b -> psibar[x2]}, nambuRules]", "Both", {x1, x2},
  "Names" -> nambuNames];
example["superfield_channels", "gammaS = 4 FunctionalDerivative[PhiNambu, NambuPropagator[a, b],
   NambuPropagator[c, d]];
channels = <|\"s\" -> gammaS, \"t\" -> (-gammaS /. {b -> c, c -> b}),
   \"u\" -> (gammaS /. {b -> d, c -> b, d -> c})|>;
shortcut = IrreducibleVertex[PhiNambu, NambuPropagator, {a, b, c, d},
   \"Statistics\" -> \"Superfield\", \"Channel\" -> All];
{Length[DiagramTerms[OrderPart[#, NambuVertex, 2]]] & /@ channels,
 Table[SimplifyDiagrams[channels[k] - shortcut[k]], {k, {\"s\", \"t\", \"u\"}}]}", "Text"];
example["superfield_pp_kernel", "SimplifyDiagrams[SuperfieldComponents[channels[\"t\"],
   {a -> psibar[x1], b -> psi[x2], c -> psibar[x3], d -> psi[x4]},
   {NambuField -> {psi, psibar}, NambuPropagator -> {G, 0, 0},
    NambuVertex -> CoulombVertex}] - ppKernel]", "Text"];

(* ---------------------------------------------------------------- exact equations *)
(* the derivative gamma + dSigma/dU, with the local potential of density_correlation *)
example["density_vertex", "sigma1 = AmputateExternalLegs[ParticleIrreducibleDiagrams[green1, 1, {x, y}, {G0}],
   {x, y}, {G0}];
dSigma = FunctionalDerivative[sigma1 /. {x -> x1, y -> x2}, LocalPotential[z]];
lambdaU = DeltaFunction[x1, z] DeltaFunction[x2, z] +
   (SkeletonDiagrams[dSigma, {x1, x2, z}, {G0}] /. G0 -> G)", "Pictures", {x1, x2, z}];
(* the shortcut gives the same *)
example["density_vertex_derivative", "SimplifyDiagrams[lambdaU - PerturbativeDensityVertex[Zf, CoulombVertex, 1,
   {psi, psibar}, {x1, x2, z}, G0 -> G]]", "Text"];
example["exact_dyson", "DeclareBlob[SigmaBlob, \"\\\\Sigma\"]; DeclareLineStyle[FullG, \"DoubleArrow\"];
Last[ExactEquation[\"Dyson\", {\"G\" -> FullG, \"G0\" -> G0, \"Sigma\" -> SigmaBlob},
    {x, y}]]", "Pictures", {x, y}];
example["exact_response", "DeclareBlob[Lbox, \"L\", \"Shape\" -> \"Box\"];
DeclareBlob[K0box, \"\\\\Gamma^0\", \"Shape\" -> \"Box\", \"Fill\" -> \"CrossHatched\"];
Last[ExactEquation[\"Response\", {\"L\" -> Lbox, \"Gamma0\" -> K0box, \"G\" -> G},
    {x1, x2, x3, x4}]]",
  "Pictures", {x1, x2, x3, x4}];
example["check_vertex_phi4", "lambdaRho = PerturbativeDensityVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, z},
    D0 -> DressedPropagator];
gamma2 = LuttingerWardFunctional[Z0, Phi4Vertex, 2, D0 -> DressedPropagator];
objects = {\"G\" -> DressedPropagator, \"Lambda\" -> LabelFunction[lambdaRho,
    {a, b, z}], \"Gamma0\" ->
   LabelFunction[4 FunctionalDerivative[gamma2, DressedPropagator[a, b],
       DressedPropagator[c, d]], {a, b, c, d}]};
CheckEquation[ExactEquation[\"Vertex\", objects, {a, b, z}, \"Statistics\" -> \"Boson\",
    \"MaximumOrder\" -> {lambda, 2}], lambda, 2]", "Text"];

example["hs_electron_gas", "hs = HubbardStratonovich[{Zf, CoulombVertex}, psibar[#] ** psi[#] &, {hsField, Jhs},
  Automatic];
hs[\"Interaction\"]", "Formula"];
example["integrate_out_phonons", "DeclareSource[Jph]; DeclareField[phonon, Jph]; DeclareSymmetric[Dph];
Zep = Zf Exp[1/2 IntegralOver[Jph[x] Dph[x, y] Jph[y], {x, y}]];
IntegrateOutField[{Zep, g IntegralOver[phonon[z] psibar[z] ** psi[z], {z}]}, phonon,
    Jph][\"Interaction\"]", "Formula"];

(* ---------------------------------------------------------------- tree expansion *)
example["tree_four_point", "ConnectedFunctionFromVertices[{a, b, c, d}, \"Boson\"]", "Both", {a, b, c, d}];
example["tree_shapes", "shapes = SimplifyDiagrams[IntegralOver[
    ConnectedFunctionFromVertices[{x1, x2, x3, x4, x5}, \"Boson\"],
   {x1, x2, x3, x4, x5}]];
{# /. IntegralOver[__] -> 1, Count[#, _VertexFunction, Infinity]} & /@
  DiagramTerms[shapes]", "Text"];
example["tree_fermion", "ConnectedFunctionFromVertices[{a, b, c, d}, \"Fermion\"]", "Both", {a, b, c, d}];
example["tree_fermion_lines", "ConnectedFunctionFromVertices[{x1, x2, x3, x4, x5, x6}, \"FermionLines\"]", "Pictures", {x1, x2, x3, x4, x5, x6}];

(* ---------------------------------------------------------------- pictures and formulas *)
example["tikz_fock", "DiagramToTikZ[-G[x, y] V[x, y], {x, y}]", "Text"];
example["vertex_labels", "-IntegralOver[G0[x, z] G0[z, w] G0[w, y] V[z, w], {z, w}]", "Pictures", {x, y}, "VertexLabels" -> All];
example["interaction_style", "-IntegralOver[G0[x, z] G0[z, w] G0[w, y] V[z, w], {z, w}]", "Pictures", {x, y}, "InteractionStyle" -> "Dashed"];
example["latex_names", "ExpressionToLaTeX[-IntegralOver[G0[x, z] G0[z, y] V[x, z], {z}],
    \"Names\" -> {G0 -> \"G_0\", V -> \"v\"},
  \"Labels\" -> {x -> \"1\", y -> \"2\"}]", "Text"];

(* ---------------------------------------------------------------- Matsubara sums *)
example["matsubara_sum", "MatsubaraSum[1/(z - a), z, \"Fermion\", \"ConvergenceFactor\" -> 1]", "Formula"];
example["matsubara_two_poles", "MatsubaraSum[1/((z - a) (z - b)), z, \"Boson\"]", "Formula"];
example["matsubara_double_pole", "MatsubaraSum[1/((z - a)^2 (z - b)), z, \"Fermion\"]", "Formula"];
example["matsubara_periodicity", "DeclareMatsubaraFrequency[w, \"Fermion\"]; DeclareMatsubaraFrequency[nu, \"Boson\"];
MatsubaraSum[1/((z - a) (z + I nu - b)), z, \"Fermion\"]", "Formula"];
example["matsubara_log", "MatsubaraSum[Log[(z - a)/(z - b)], z, \"Fermion\", \"ConvergenceFactor\" -> 1]", "Formula"];
example["evaluate_hartree_fock", "rules = {G0 -> {\"Fermion\", xi}, G -> {\"Fermion\", xi}, V -> {\"Instantaneous\", v}};
EvaluateMatsubara[OrderPart[sigmaG, V, 1],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, \"SpinDegeneracy\" -> 2]", "Formula"];
example["evaluate_hartree_fock_symmetric", "EvaluateMatsubara[OrderPart[sigmaG, V, 1],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, \"SpinDegeneracy\" -> 2,
  \"ConvergenceFactor\" -> 0]", "Formula"];
example["evaluate_lindhard", "bubble = ConnectedCorrelationFunction[Zf, {psibar[a], psi[a], psibar[b], psi[b]},
   CoulombVertex, {0}];
lindhard = EvaluateMatsubara[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules,
  \"SpinDegeneracy\" -> 2, \"LoopMomenta\" -> {p}]", "Formula"];
example["diagram_integrand", "DiagramIntegrand[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules]", "Text"];
example["lindhard_imaginary_part", "RetardedImaginaryPart[AnalyticContinuation[lindhard, nu -> omega], omega]", "Formula"];
example["lindhard_zero_temperature", "ZeroTemperatureLimit[lindhard]", "Formula"];
example["lindhard_limits", "Table[EvaluateMatsubara[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules,
  \"SpinDegeneracy\" -> 2, \"LoopMomenta\" -> {p}, \"Limit\" -> limit],
  {limit, {\"Static\", \"Dynamic\"}}]", "Text"];
example["evaluate_grand_potential", "EvaluateMatsubara[EffectiveAction2PI[0, G0 -> G], {}, rules,
  \"SpinDegeneracy\" -> 2, \"LoopMomenta\" -> {p}]", "Formula"];
(* the energies and occupations of the lines of the second-order self-energy, by their names *)
energyNames = {xi[p1] -> "\\xi_1", xi[p2] -> "\\xi_2", xi[k - p1 + p2] -> "\\xi_3", FermiFunction[xi[p1]] -> "n_1",
   FermiFunction[xi[p2]] -> "n_2", FermiFunction[xi[k - p1 + p2]] -> "n_3"};
(* the code is written in lines of at most 85 characters, the width of the boxes of the manual *)
example["sigma_second_order", "sigma2 = EvaluateMatsubara[OrderPart[sigmaG, V, 2],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, \"SpinDegeneracy\" -> 2,
  \"LoopMomenta\" -> {p1, p2}]", "Formula", {}, "Subscripts" -> {v}, "Names" -> energyNames];
example["sigma_cutting_rules", "CuttingRuleImaginaryPart[OrderPart[sigmaG, V, 2],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, w -> omega, \"SpinDegeneracy\" -> 2,
  \"LoopMomenta\" -> {p1, p2}]", "Formula", {}, "Subscripts" -> {v}, "Names" -> energyNames];
example["check_cutting_rules", "{CheckCuttingRules[OrderPart[sigmaG, V, 2],
   {y -> {k, I w}, x -> {-k, -I w}}, rules, w -> omega],
 CheckCuttingRules[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules, nu -> omega]}", "Text"];
example["fock_insertion", "fockInsertion = IntegralOver[G0[x, u] G0[u, v] G0[v, y] V[x, y] V[u, v], {u, v}]", "Pictures", {x, y}];
example["double_pole_diagram", "EvaluateMatsubara[fockInsertion, {y -> {k, I w}, x -> {-k, -I w}}, rules,
  \"LoopMomenta\" -> {p1, p2}]", "Formula", {}, "Subscripts" -> {v}, "Names" -> energyNames];
example["phonon_self_energy", "DeclareSource[Jph]; DeclareField[ph, Jph]; DeclareSymmetric[Dph];
Zep = Zf Exp[1/2 IntegralOver[Jph[x] Dph[x, y] Jph[y], {x, y}]];
greenPhonon = -PerturbativeCorrelationFunction[Zep, {psi[x], psibar[y]},
    g IntegralOver[ph[z] psibar[z] ** psi[z], {z}], {2}];
sigmaPhonon = AmputateExternalLegs[
   ParticleIrreducibleDiagrams[greenPhonon, 1, {x, y}, {G0}], {x, y}, {G0}];
phononRules = {G0 -> {\"Fermion\", xi},
   Dph -> {\"Boson\", Function[{s, zz}, 2 Om[s]/(Om[s]^2 - zz^2)]}};
EvaluateMatsubara[sigmaPhonon, {y -> {k, I w}, x -> {-k, -I w}}, phononRules,
  \"SpinDegeneracy\" -> 2, \"LoopMomenta\" -> {p}]", "Formula", {},
  "Subscripts" -> {xi, Om}, "Names" -> {Om -> "\\Omega"}];

(* ---------------------------------------------------------------- dimensional regularization *)
(* phi^4 theory of the examples above (Z0, D0, Phi4Vertex); eps is written \varepsilon *)
epsName = {eps -> "\\varepsilon"};
(* Sigma = D0^-1 - Gamma^(2), the second derivative of the 1PI action at zero mean field *)
example["dimreg_sigma", "gammaPhi4 = EffectiveAction1PI[Z0, Phi4Vertex, 2, phi -> phiMean, D0 -> D0];
sigmaPhi4 = OrderPart[ExpandTrace[OperatorKernel[Power[D0, -1]][a, b] -
    SetSourcesToZero[FunctionalDerivative[gammaPhi4, phiMean[a],
        phiMean[b]]]], lambda, {1, 2}]",
  "Pictures", {a, b}];
(* the amputated 1PI part of the propagator gives the same *)
example["dimreg_sigma_check", "SimplifyDiagrams[sigmaPhi4 - AmputateExternalLegs[ParticleIrreducibleDiagrams[
    PerturbativeCorrelationFunction[Z0, {phi[a], phi[b]}, Phi4Vertex, 2], 1, {a, b},
    {D0}], {a, b}, {D0}]]", "Text"];
example["dimreg_integrand", "DiagramLoopIntegral[sigmaPhi4, {a -> p, b -> -p}, {D0 -> {\"Scalar\", m}},
  \"LoopMomenta\" -> {k, l}]", "Formula"];
example["dimreg_kernel", "DiagramLoopIntegral[IntegralOver[D0[a, u] Q[u, v] D0[v, a], {u, v}], {a -> 0},
  {D0 -> {\"Scalar\", m}, Q -> {\"Kernel\", Function[q, c MomentumSquared[q]]}},
  \"LoopMomenta\" -> {k}]", "Formula"];
example["dimreg_momentum_squared", "{MomentumSquared[-p], MomentumSquared[-p - q], MomentumSquared[3 p]}", "Text"];
example["dimreg_tadpole", "IntegrateLoops[LoopIntegral[ScalarPropagator[k, m], {k}], eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_sunset", "sunset = IntegrateLoops[DiagramLoopIntegral[OrderPart[sigmaPhi4, lambda, 2],
   {a -> p, b -> -p}, {D0 -> {\"Scalar\", 0}}], eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_massive_bubble", "IntegrateLoops[LoopIntegral[ScalarPropagator[k, m] ScalarPropagator[k + p, m], {k}],
  eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_numerator", "IntegrateLoops[LoopIntegral[MomentumSquared[k + p] ScalarPropagator[k, m]^2, {k}],
  eps]",
  "Formula", {}, "Names" -> epsName];
example["dimreg_g_function", "GFunction[1, 1, eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_tadpole_integral", "TadpoleIntegral[2, m, eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_bubble_integral", "BubbleIntegral[{1, 1}, p^2, {0, 0}, eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_expand_bubble", "EpsilonExpand[16 Pi^2 BubbleIntegral[{1, 1}, p^2, {0, 0}, eps], eps, 1]", "Formula", {}, "Names" -> epsName];
example["dimreg_expand_sunset", "EpsilonExpand[(16 Pi^2)^2 sunset, eps, 0]", "Formula", {}, "Names" -> epsName];
example["dimreg_expand_massive", "EpsilonExpand[16 Pi^2 BubbleIntegral[{1, 1}, p^2, {m, m}, eps], eps, 0]", "Formula", {}, "Names" -> epsName];
example["dimreg_zeta", "EpsilonExpand[Gamma[eps]^3, eps, 0]", "Formula", {}, "Names" -> epsName];
example["dimreg_feynman_parameter", "e1 = Coefficient[EpsilonExpand[BubbleIntegral[{1, 1}, 5, {1, 2}, eps], eps, 1],
   eps, 1];
{Cases[e1, _FeynmanParameterIntegral, Infinity], N[e1 /. RenormalizationScale -> 1]}", "Text"];
example["dimreg_pole_part", "PolePart[sunset, eps]", "Formula", {}, "Names" -> epsName];
example["dimreg_gamma4", "gamma4 = SetSourcesToZero[FunctionalDerivative[EffectiveAction1PI[Z0, Phi4Vertex, 3,
    phi -> phiMean, D0 -> D0], phiMean[a], phiMean[b], phiMean[c], phiMean[d]]];
OrderPart[gamma4, lambda, 3]", "Pictures", {a, b, c, d}];
example["dimreg_rearranged_one_loop", "massless = {D0 -> {\"Scalar\", 0}};
InfraredRearrangedIntegral[OrderPart[gamma4, lambda, 2], {a, b, c, d}, p, massless,
  eps]", "Formula", {},
  "Names" -> epsName];
example["dimreg_rearranged_routing", "routed = InfraredRearrangedIntegral[OrderPart[gamma4, lambda, 3], {a, b, c, d}, p,
   massless, eps, \"Routing\" -> True];
Tally[Simplify[(16 Pi^2)^2 PolePart[#[[3]], eps]/lambda^3 /.
     Log[p^2/RenormalizationScale^2] -> L] & /@ routed]", "Text"];
example["dimreg_mass_insertion", "sigma21 = -MassInsertion[sigmaPhi4, D0, u]", "Pictures", {a, b, u}];
(* the renormalization of phi^4 to two loops *)
example["dimreg_bare", "sigmaB = InfraredRearrangedIntegral[sigmaPhi4, {a, b}, p, massless, eps];
gamma4B = InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps];
sigma21B = InfraredRearrangedIntegral[sigma21, {a, b, u}, p, massless, eps];
Expand[PolePart[sigma21B, eps] /. lambda -> 16 Pi^2 lambda]", "Formula", {}, "Names" -> epsName];
example["dimreg_z_solution", "kappa = 1/(16 Pi^2);
z[c_] := 1 + c[1] lambda kappa/eps + lambda^2 kappa^2 (c[2]/eps^2 + c[3]/eps);
{zPhi, zLambda, zM2} = {z[zf], z[zl], z[zm]};
upTo[e_, n_] := Sum[Coefficient[e, lambda, j] lambda^j, {j, 0, n}];
finite = {upTo[zPhi (p^2 - (sigmaB /. lambda -> lambda zLambda)), 2],
   upTo[zPhi^2 (gamma4B /. lambda -> lambda zLambda), 3],
   upTo[zM2 (1 - (sigma21B /. lambda -> lambda zLambda)), 2]};
equations = Flatten[CoefficientList[Expand[eps^4 PolePart[#, eps]] /.
      Log[p^2/RenormalizationScale^2] -> L, {lambda, eps, L, p}] & /@ finite];
solution = First[Solve[Thread[DeleteCases[equations, 0] == 0],
   Flatten[Table[c[i], {c, {zf, zl, zm}}, {i, 3}]]]]", "Text"];
example["dimreg_z_lambda", "Expand[zLambda /. solution]", "Formula", {}, "Names" -> epsName];
example["dimreg_beta", "beta = BetaFunctionFromZ[zLambda /. solution, lambda, eps, 3]", "Formula"];
example["dimreg_gamma_phi", "1/2 AnomalousDimensionFromZ[zPhi /. solution, lambda, eps, -2 eps lambda + beta, 2]",
  "Formula"];
example["dimreg_gamma_m2", "-AnomalousDimensionFromZ[zM2/zPhi /. solution, lambda, eps, -2 eps lambda + beta, 2]",
  "Formula"];

(* ---------------------------------------------------------------- QED at one loop *)
(* the theory of Tests/QEDOneLoop.wl, with the conventions of Peskin and Schroeder and the charge -e of the electron, e > 0
   (vertex +i e gamma^mu) *)
example["qed_contraction", "DiracSimplify[DiracProduct[GammaMatrix[mu], SlashedMomentum[a], GammaMatrix[mu]]]",
  "Formula"];
example["qed_order", "DiracOrder[DiracProduct[GammaMatrix[mu], SlashedMomentum[a], SlashedMomentum[b],
  GammaMatrix[mu]]]", "Formula"];
example["qed_trace", "DiracSimplify[DiracTrace[DiracProduct[GammaMatrix[mu], SlashedMomentum[p] + m,
  GammaMatrix[nu], SlashedMomentum[q] + m]]]", "Formula"];
example["qed_spinor_chain", "DiracSimplify[SpinorChain[{p2, m}, DiracProduct[SlashedMomentum[p1],
  GammaMatrix[mu]], {p1, m}]]", "Formula"];
example["qed_vectors", "DiracSimplify[ScalarProduct[p + 2 q, p - q] +
  FourVector[p + q, mu] FourVector[k, mu] +
  LorentzMetric[mu, nu] LorentzMetric[mu, nu]]", "Formula"];
(* the electron line S, the photon D and the vertex V in the formulas of the diagrams *)
qedNames = {ElectronPropagator -> "S", PhotonPropagator -> "D", QEDVertex -> "V"};
example["qed_theory", "DeclareSource[Jphoton]; DeclareGrassmannSource[eta, etabar];
DeclareField[photon, Jphoton]; DeclareField[electron, etabar];
DeclareField[electronbar, eta, -1];
DeclareSymmetric[PhotonPropagator]; DeclareLineStyle[PhotonPropagator, \"Wavy\"];
DeclareDirectedPropagator[ElectronPropagator]; DeclareLocalVertex[QEDVertex];
qedZ0 = Exp[1/2 IntegralOver[Jphoton[x] PhotonPropagator[x, y] Jphoton[y], {x, y}] +
    IntegralOver[ElectronPropagator[x, y] etabar[x] ** eta[y], {x, y}]];
qedInteraction = e IntegralOver[QEDVertex[x, y, z] electronbar[x] ** electron[y]
    photon[z], {x, y, z}];
qedLines = {PhotonPropagator, ElectronPropagator};
qedVertexFunction[fields_, labels_, n_] := ParticleIrreducibleDiagrams[
   AmputateExternalLegs[ConnectedCorrelationFunction[qedZ0, fields, qedInteraction,
     n, \"CouplingOrder\" -> {e, n}], labels, qedLines], 1, labels, qedLines];
sigmaQED = qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 2]", "Both", {x, y}, "Names" -> qedNames];
example["qed_polarization", "polarizationQED = qedVertexFunction[{photon[x], photon[y]}, {x, y}, 2]", "Both", {x, y},
  "Names" -> qedNames];
example["qed_vertex", "vertexQED = qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]},
  {x1, x2, x3}, 3]", "Both", {x1, x2, x3}, "Names" -> qedNames];
example["qed_tree_vertex", "qedRules = {ElectronPropagator -> {\"Fermion\", m},
   PhotonPropagator -> {\"Photon\", xi}, QEDVertex -> {\"Vertex\", -I}};
QEDIntegrand[qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]},
   {x1, x2, x3}, 1], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules,
  \"Indices\" -> {x3 -> mu}]", "Formula"];
example["qed_sigma_integrand", "sigmaIntegrand = QEDIntegrand[sigmaQED, {y -> p, x -> -p}, qedRules,
  \"LoopMomentum\" -> k]", "Formula"];
example["qed_polarization_integrand", "piIntegrand = QEDIntegrand[polarizationQED, {y -> q, x -> -q}, qedRules,
  \"LoopMomentum\" -> k, \"Indices\" -> {x -> mu, y -> nu}]", "Formula"];
(* one-loop integrals and Feynman-parameter integrals *)
example["qed_tadpole", "IntegrateOneLoop[MinkowskiLoopIntegral[FeynmanDenominator[k, m]^2, {k}], eps]", "Formula", {},
  "Names" -> epsName];
example["qed_tensor", "FullSimplify[IntegrateOneLoop[MinkowskiLoopIntegral[
   FourVector[k, mu] FourVector[k, nu] FeynmanDenominator[k, m]^3, {k}], eps]]", "Formula", {},
  "Names" -> epsName];
example["qed_bubble", "IntegrateOneLoop[MinkowskiLoopIntegral[FeynmanDenominator[k, m]
   FeynmanDenominator[k + p, m], {k}], eps]", "Formula", {}, "Names" -> epsName];
example["qed_triangle", "triangle = IntegrateOneLoop[MinkowskiLoopIntegral[FeynmanDenominator[k, 0]
   FeynmanDenominator[k + p1, m] FeynmanDenominator[k + p2, m], {k}], eps];
onShell = {ScalarProduct[p1, p1] -> 1, ScalarProduct[p2, p2] -> 1,
   ScalarProduct[p1, p2] -> 1, m -> 1};
triangle /. onShell", "Formula", {}, "Names" -> epsName];
example["qed_triangle_expand", "ExpandParameterIntegrals[triangle /. onShell, eps, 0]", "Formula", {},
  "Names" -> epsName];
example["qed_reduce", "ReduceParameterIntegrals[ParameterIntegral[t^-eps (1 - t)^(1 - 2 eps), t]]", "Formula", {},
  "Names" -> epsName];
example["qed_combine", "CombineParameterIntegrals[e^2 ParameterIntegral[t, t] +
  2 e^2 ParameterIntegral[t^2 Log[1 + t], t]]", "Formula"];
(* the electron self-energy *)
example["qed_sigma_pole", "sigmaOneLoop = DiracSimplify[I IntegrateOneLoop[sigmaIntegrand, eps]];
sigmaPole = ExpandParameterIntegrals[sigmaOneLoop, eps, -1];
Expand[16 Pi^2 eps sigmaPole/e^2]", "Formula"];
example["qed_counterterms", "delta2 = Coefficient[sigmaPole, DiracProduct[SlashedMomentum[p]]];
deltaM = -Coefficient[sigmaPole, DiracProduct[]];
{delta2, Expand[deltaM/m - delta2]}", "Text"];
example["qed_sigma_finite", "Expand[ExpandParameterIntegrals[sigmaOneLoop /. xi -> 1, eps, 0]]", "Formula", {},
  "Names" -> epsName];
example["qed_delta_z2", "{sa, sb} = {Coefficient[sigmaOneLoop, DiracProduct[]]/m, Coefficient[sigmaOneLoop,
     DiracProduct[SlashedMomentum[p]]]} /. ScalarProduct[p, p] -> p2;
deltaZ2 = ExpandParameterIntegrals[
   (sb + 2 m^2 DifferentiateUnderIntegral[sa + sb, p2]) /. p2 -> m^2, eps, 0];
Expand[16 Pi^2 deltaZ2/e^2]", "Formula", {}, "Names" -> epsName];
(* the vacuum polarization *)
example["qed_transverse", "piMuNu = DiracSimplify[-I IntegrateOneLoop[piIntegrand, eps]];
{piG, piQQ} = {Coefficient[piMuNu, LorentzMetric[mu, nu]],
   Coefficient[piMuNu, FourVector[q, mu] FourVector[q, nu]]};
Simplify[ReduceParameterIntegrals[piG + ScalarProduct[q, q] piQQ] /.
  ParameterIntegral[f_, x_] :> ParameterIntegral[FullSimplify[f], x]]", "Formula"];
example["qed_delta3", "piQ = -piQQ;
delta3 = ExpandParameterIntegrals[piQ, eps, -1]", "Formula", {}, "Names" -> epsName];
example["qed_beta", "BetaFunctionFromZ[1 - delta3/2, e, eps, 3, \"ScalePower\" -> 1]", "Formula"];
example["qed_pi_hat", "piHat = ExpandParameterIntegrals[piQ - (piQ /. ScalarProduct[q, q] -> 0), eps, 0]",
  "Formula"];
example["qed_pi_hat_values", "N[piHat /. {m -> 1, RenormalizationScale -> 1} /.
   {{ScalarProduct[q, q] -> -3}, {ScalarProduct[q, q] -> 5}}]", "Text"];
(* the vertex *)
example["qed_vertex_integrand", "vertexIntegrand = QEDIntegrand[vertexQED, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2},
  qedRules, \"LoopMomentum\" -> k, \"Indices\" -> {x3 -> mu}]", "Formula"];
example["qed_form_factors", "deltaGamma = -I/e IntegrateOneLoop[vertexIntegrand, eps];
chain = DiracSimplify[SpinorChain[{p2, m}, deltaGamma, {p1, m}]] /.
   ScalarProduct[p1, p2] -> m^2 - q2/2;
{f1, f2, f3} = VertexFormFactors[chain, mu, {p1, p2, m}];
ExpandParameterIntegrals[f2 /. q2 -> 0, eps, 0]", "Formula"];
example["qed_ward_identity", "{Simplify[ReduceParameterIntegrals[f3 /. q2 -> 0]],
 Simplify[ExpandParameterIntegrals[f1 /. q2 -> 0, eps, 0] + deltaZ2]}", "Text"];

(* ---------------------------------------------------------------- effective actions and operator traces *)
example["trace_log_rings", "DeclareDirectedPropagator[Sg]; DeclareBlob[Sg, \"\\\\Sigma\"];
ExpandTrace[-TraceLog[-(G0^-1 - Sg)], G0, 3] + TraceLog[-G0^-1]", "Both", {}, "Names" -> {Sg -> "\\Sigma"}];
example["derivative_trace_log", "FunctionalDerivative[-TraceLog[-G^-1], G[x, y]]", "Formula"];
example["trace_log_label_function", "DeclareSource[phibar];
oneLoop = 1/2 TraceLog[D0^-1 +
    LabelFunction[lambda/2 phibar[u]^2 DeltaFunction[u, v], {u, v}]];
ExpandTrace[SetSourcesToZero[FunctionalDerivative[oneLoop,
    phibar[a], phibar[b], phibar[c], phibar[d]]], D0, 2]", "Formula", {}, "Names" -> {phibar -> "\\bar\\phi"}];
example["operator_kernel", "ExpandTrace[IntegralOver[OperatorKernel[G^-1][x, u] G[u, y], {u}]]", "Formula"];
example["effective_action_2pi", "gammaHF = EffectiveAction2PI[OrderPart[Phi, V, 1], G0 -> G]", "Formula"];
example["effective_action_dyson", "FunctionalDerivative[gammaHF, G[y, x]]", "Formula"];
example["effective_action_source", "FunctionalDerivative[EffectiveAction2PI[OrderPart[Phi, V, 1], G0 -> G,
  \"Source\" -> U], U[x, y]]", "Formula"];
example["effective_action_means_derivatives", "gammaMeans = EffectiveAction2PI[Zf, CoulombVertex, 2, G0 -> G,
   \"MeanFields\" -> {psi -> Psi, psibar -> Psibar}];
{SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaMeans, Psibar[x],
    Psi[y]]] -
   (OperatorKernel[G0^(-1)][x, y] - FunctionalDerivative[Phi, G[y, x]])],
 SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaMeans,
    G[x4, x3], Psibar[x1], Psi[x2]]] +
   FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]]]}", "Text"];
example["effective_action_mean_field", "EffectiveAction2PI[Z0, Phi4Vertex, 1, D0 -> DressedPropagator,
  \"Statistics\" -> \"Boson\", \"MeanFields\" -> {phi -> phiMean}]", "Formula", {},
  "Names" -> {DressedPropagator -> "G", phiMean -> "\\bar\\phi"}];
example["effective_action_1pi", "EffectiveAction1PI[Z0, Phi4Vertex, 2, phi -> phiMean, D0 -> Dm]", "Formula", {},
  "Names" -> {Dm -> "D", phiMean -> "\\bar\\phi"}];
example["effective_action_1pi_vertex", "gamma1PI = EffectiveAction1PI[Z0, Phi4Vertex, 1, phi -> phiMean, D0 -> Dm];
OrderPart[ExpandTrace[SetSourcesToZero[FunctionalDerivative[gamma1PI,
    phiMean[a], phiMean[b]]] /. Dm -> D0], lambda, {0, 1}]", "Formula"];
example["effective_action_1pi_fermions", "EffectiveAction1PI[Zf, CoulombVertex, 1, {psi -> Psi, psibar -> Psibar}, G0 -> G0]", "Formula", {},
  "Names" -> {Psi -> "\\Psi", Psibar -> "\\bar\\Psi"}];
example["effective_action_1pi_fermions_derivative", "SetSourcesToZero[FunctionalDerivative[EffectiveAction1PI[Zf, CoulombVertex, 1,
   {psi -> Psi, psibar -> Psibar}, G0 -> G0], Psibar[x], Psi[y]]]", "Formula"];
example["self_energy_functional", "omega = SelfEnergyFunctional[Phi, G0 -> Sg, G]", "Formula", {},
  "Names" -> {Sg -> "\\Sigma"}];
example["self_energy_functional_derivative", "FunctionalDerivative[omega, Sg[x, y]]", "Formula", {},
  "Names" -> {Sg -> "\\Sigma"}];
example["legendre_interaction", "FunctionalDerivative[LegendreTransform[OrderPart[Phi, V, 1], G -> Sg], V[x, y]]",
  "Formula", {}, "Names" -> {Sg -> "\\Sigma"}];
example["zero_dimensional_value", "{ZeroDimensionalValue[sigmaG, {G -> g, V -> u},
   \"LoopFactor\" -> 2],
 ZeroDimensionalValue[EffectiveAction2PI[Phi, G0 -> G], {G0 -> 1/a, G -> g, V -> u},
   \"LoopFactor\" -> 2]}", "Text"];
(* one site with spin 1/2 and pairing: the superfield (psiUp, psiDn, psibarUp, psibarDn), arrays for the kernels *)
example["zero_dimensional_arrays", "site = {{0, pair, g, 0}, {-pair, 0, 0, g}, {-g, 0, 0, pairb}, {0, -g, -pairb, 0}};
siteVertex = Table[If[Sort[{i, j, k, l}] === {1, 2, 3, 4},
    -u Signature[{i, j, k, l}], 0], {i, 4}, {j, 4}, {k, 4}, {l, 4}];
Factor[ZeroDimensionalValue[PhiNambu, {NambuPropagator -> site,
    NambuVertex -> siteVertex}]]", "Formula", {},
  "Names" -> {pair -> "\\kappa", pairb -> "\\bar\\kappa"}];

Print["Done: the examples are in ", FileNameJoin[{Directory[], "examples"}]];
ResetDirectory[];
