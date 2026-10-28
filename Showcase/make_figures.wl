(* ::Package:: *)

(* The figures and the numbers of the showcase (FunctionalDiagrams_Showcase.pdf). Every picture is drawn by the package
   (DiagramToTikZ) and written to figures/NAME.terms, one term per block: the weight in LaTeX, a separator line, the
   tikzpicture. A sum with several orders is written order by order, to NAME_0.terms, NAME_1.terms, ... Formulas are
   written by ExpressionToLaTeX to figures/formula_NAME.tex. The checks, the numbers of diagrams and the timings go to
   figures/values.txt (lines "name :: value"). assemble.py turns all this into LaTeX, and build_pdf.sh typesets the
   showcase.

   Run it in a fresh kernel: open this file in Mathematica and choose Run All Code, or evaluate
   Get["<path of this folder>/make_figures.wl"], or run  wolframscript -file make_figures.wl  in this folder. It takes
   about two minutes. The electron gas is computed to third order in the interaction (maximumOrder). *)

If[! ValueQ[maximumOrder], maximumOrder = 3];
$IterationLimit = 10^8;
SetDirectory[If[$InputFileName =!= "", DirectoryName[$InputFileName], NotebookDirectory[]]];
Get[FileNameJoin[{ParentDirectory[Directory[]], "FunctionalDiagrams.wl"}]];
If[! DirectoryQ["figures"], CreateDirectory["figures"]];
(* the blocks of a .terms file *)
joinBlocks[blocks_List] := StringJoin[First[blocks], Table["\n%%TERM%%\n" <> b, {b, Rest[blocks]}]];
startTime = AbsoluteTime[];

(* ---------------- output ---------------- *)
valueLines = {};
writeValue[name_String, value_] := (
   AppendTo[valueLines, name <> " :: " <> ToString[value, InputForm]];
   Export["figures/values.txt", StringJoin[Table[v <> "\n", {v, valueLines}]] <> "\n", "Text"];
   Print[name, " :: ", ToString[value, InputForm]]);
SetAttributes[timed, HoldRest];
timed[name_String, e_] := Module[{t = AbsoluteTime[], r},
   r = e;
   writeValue["seconds " <> name, N[Round[10 (AbsoluteTime[] - t)]/10]];
   r];
(* the size of the pictures: centimetres per unit of the drawing, line width in pt, font of the labels *)
pictureOptions = {"Scale" -> 0.34, "LineWidth" -> 0.42, "LabelFont" -> "\\tiny"};
writeTerms[name_String, e_, ext_List, opts___] := Module[{parts = DiagramToTikZ[e, ext, "Output" -> "List", opts, Sequence @@ pictureOptions]},
   Export["figures/" <> name <> ".terms", joinBlocks[(#[[1]] <> "\n%%PICTURE%%\n" <> #[[2]]) & /@ parts], "Text"];
   Print[name, ": ", Length[parts], " terms"]];
(* a single diagram without its weight: a factor of the products written in the text *)
writePicture[name_String, e_, ext_List, opts___] := Module[{parts = DiagramToTikZ[e, ext, "Output" -> "List", opts, Sequence @@ pictureOptions]},
   Export["figures/" <> name <> ".terms", "1\n%%PICTURE%%\n" <> parts[[1, 2]], "Text"];
   Print[name, ": picture"]];
(* a formula, written by ExpressionToLaTeX *)
writeFormula[name_String, e_, opts___] := With[{tex = ExpressionToLaTeX[e, opts]},
   Export["figures/formula_" <> name <> ".tex", tex, "Text"]; Print[name, ": ", tex]];
(* orderPart: OrderPart remembered, as most sums are both counted and written *)
orderPart[e_, v_, m_] := orderPart[e, v, m] = OrderPart[e, v, m];
(* the terms of the orders ms in the coupling or interaction line v, written to NAME_m.terms. The whole sum is drawn at
   once, as DiagramPlot draws it (the directions of the legs, the density points and the sizes of the blobs depend on
   all its terms), and its pictures are sorted by order: DiagramToTikZ draws the terms of DiagramTerms in the order of
   their numbers of integration variables *)
integrationCount[t_] := Total[Cases[{t}, IntegralOver[_, vars_List] :> Length[vars], Infinity]];
orderOfTerm[t_, v_] := SelectFirst[Range[0, 12], OrderPart[t, v, #] =!= 0 &];
writeOrders[name_String, e_, ext_List, v_, ms_List, opts___] := Module[{terms = DiagramTerms[e], parts, orders},
   parts = DiagramToTikZ[e, ext, "Output" -> "List", opts, Sequence @@ pictureOptions];
   orders = orderOfTerm[#, v] & /@ terms[[Ordering[integrationCount /@ terms]]];
   Do[With[{part = Pick[parts, orders, m]},
     If[Length[parts] =!= Length[terms] || Length[part] =!= Length[DiagramTerms[orderPart[e, v, m]]],
      Print["writeOrders: the pictures of ", name, " do not match its orders"]; Abort[]];
     If[part =!= {}, Export["figures/" <> name <> "_" <> ToString[m] <> ".terms",
       joinBlocks[(#[[1]] <> "\n%%PICTURE%%\n" <> #[[2]]) & /@ part], "Text"]; Print[name <> "_" <> ToString[m], ": ", Length[part], " terms"]]],
    {m, ms}]];
(* the residual of a check order by order: e is evaluated once (byOrder has no Hold attribute), then the part of each
   order is simplified *)
byOrder[e_, v_, ms_List] := Table[SimplifyDiagrams[OrderPart[e, v, m]], {m, ms}];
orderCounts[e_, v_, ms_List] := Table[Length[DiagramTerms[orderPart[e, v, m]]], {m, ms}];
writeValue["version", {$FunctionalDiagramsVersion, $Version}];
writeValue["maximum order", maximumOrder];

(* ---------------- the engine: functional derivatives ---------------- *)
(* delta functions, canonical names, equal terms, a term odd under a symmetry of its own *)
DeclareSymmetric[Q];
writeFormula["delta_integral", SimplifyDiagrams[IntegralOver[M[x, u] DeltaFunction[u, v] M[v, y], {u, v}]]];
writeFormula["delta_external", SimplifyDiagrams[DeltaFunction[a, b] M[a, b] M[b, c]]];
writeFormula["same_diagram", SimplifyDiagrams[IntegralOver[M[x, u] Q[u, v] M[v, y], {u, v}] + IntegralOver[M[x, s] Q[t, s] M[t, y], {s, t}]]];
DeclareGrassmannSource[eta, etabar];
writeFormula["odd_term", SimplifyDiagrams[IntegralOver[Q[u, v] etabar[u] ** etabar[v], {u, v}]]];
(* commuting sources: product rule, chain rule *)
DeclareSource[J]; DeclareSymmetric[Q, Q3];
W = 1/2 IntegralOver[Q[x, y] J[x] J[y], {x, y}] + 1/6 IntegralOver[Q3[x, y, z] J[x] J[y] J[z], {x, y, z}];
writeFormula["first_derivative", FunctionalDerivative[W, J[a]]];
writeFormula["second_derivative", FunctionalDerivative[W, J[a], J[b]]];
writeFormula["chain_rule", FunctionalDerivative[f[IntegralOver[g[x] J[x], {x}]], J[a], J[b]]];
writeFormula["logarithm", FunctionalDerivative[Log[1 + IntegralOver[g[x] J[x], {x}]], J[a], J[b]]];
writeFormula["square_root", FunctionalDerivative[Sqrt[IntegralOver[g[x] J[x]^2, {x}]], J[a]]];
(* a source with two arguments, one of them a spin projection *)
DeclareDiscreteIndexValues[{up, down}]; DeclareSymmetric[U];
spinFunctional = 1/2 IntegralOver[U[x, y] J[x, s] J[y, s], {x, y, s}];
writeFormula["spin_same", FunctionalDerivative[spinFunctional, J[a, up], J[b, up]]];
writeFormula["spin_opposite", FunctionalDerivative[spinFunctional, J[a, up], J[b, down]]];
(* Grassmann sources *)
bilinear = IntegralOver[M[x, y] etabar[x] ** eta[y], {x, y}];
writeFormula["grassmann_ab", FunctionalDerivative[bilinear, etabar[a], eta[b]]];
writeFormula["grassmann_ba", FunctionalDerivative[bilinear, eta[b], etabar[a]]];
writeFormula["grassmann_pair", FunctionalDerivative[Exp[bilinear], etabar[a], eta[b]]];
(* implicit dependence: G = (G0^-1 - J)^-1, dG(a, b)/dJ(c) = G(a, c) G(c, b) *)
DeclareSymmetric[PropagatorOfJ];
DeclareFunctionalDerivative[PropagatorOfJ[p_, q_], J[r_], PropagatorOfJ[p, r] PropagatorOfJ[r, q]];
writeFormula["implicit", FunctionalDerivative[PropagatorOfJ[x, y], J[u], J[v]], "Names" -> {PropagatorOfJ -> "G"}];
(* the propagator as a source: the Hartree-Fock functional, its self-energy and the bare vertex *)
DeclareSource[G]; DeclareSymmetric[V];
phiHF = 1/2 IntegralOver[V[u, v] G[u, u] G[v, v], {u, v}] - 1/2 IntegralOver[V[u, v] G[u, v] G[v, u], {u, v}];
writeFormula["hartree_fock", FunctionalDerivative[phiHF, G[y, x]]];
writeFormula["bare_vertex", FunctionalDerivative[phiHF, G[x4, x3], G[x2, x1]]];
(* moments and cumulants of any generating functional *)
DeclareField[phi, J]; DeclareSymmetric[W2, W3];
Z = Exp[IntegralOver[W1[x] J[x], {x}] + 1/2 IntegralOver[W2[x, y] J[x] J[y], {x, y}] +
    1/6 IntegralOver[W3[x, y, z] J[x] J[y] J[z], {x, y, z}]];
writeFormula["moments", FieldExpectationValue[Z, {phi[a], phi[b], phi[c]}]];
writeFormula["cumulants", SetSourcesToZero[FunctionalDerivative[Log[Z], J[a], J[b], J[c]]]];

(* ---------------- Wick's theorem ---------------- *)
DeclareSymmetric[FreePropagator];
FreeBosonGeneratingFunctional = Exp[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}]];
writeTerms["pairings", SetSourcesToZero[FunctionalDerivative[FreeBosonGeneratingFunctional, J[a], J[b], J[c], J[d]]], {a, b, c, d}];
DeclareGrassmannSource[eta, etabar]; DeclareDirectedPropagator[BareGreenFunction];
FreeFermionGeneratingFunctional = Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
writeTerms["fermion_four_point", GaussianExpectationValue[FreeFermionGeneratingFunctional, {psi[a], psi[b], psibar[c], psibar[d]}], {a, b, c, d}];

(* ---------------- the electron gas: G, Sigma, Dyson ---------------- *)
(* G0(x, y) = -<T psi(x) psibar(y)>, S_int = 1/2 Integral V(x, y) psibar(x) psibar(y) psi(y) psi(x) *)
DeclareSymmetric[InteractionPotential]; DeclareInteractionLine[InteractionPotential];
CoulombVertex = 1/2 IntegralOver[InteractionPotential[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
orders = Range[maximumOrder];
greenFunction = timed["G", -PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x], psibar[y]}, CoulombVertex, maximumOrder]];
writeValue["counts G", orderCounts[greenFunction, InteractionPotential, Range[0, maximumOrder]]];
writeOrders["green_function", greenFunction, {x, y}, InteractionPotential, Range[0, maximumOrder]];
selfEnergyPerturbative = timed["Sigma perturbative", AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction, 1, {x, y}, {BareGreenFunction}],
     {x, y}, {BareGreenFunction}]];
writeValue["counts Sigma perturbative", orderCounts[selfEnergyPerturbative, InteractionPotential, orders]];
writeOrders["self_energy_perturbative", selfEnergyPerturbative, {x, y}, InteractionPotential, orders];
writeValue["check Dyson", SimplifyDiagrams[DysonSeries[selfEnergyPerturbative /. {x -> s1, y -> s2}, {s1, s2}, BareGreenFunction, {x, y},
     maximumOrder, "MaximumOrder" -> {InteractionPotential, maximumOrder}] - greenFunction]];

(* ---------------- vacuum diagrams, Phi, Sigma = dPhi/dG ---------------- *)
vacuumDiagrams = timed["vacuum", ConnectedVacuumDiagrams[FreeFermionGeneratingFunctional, CoulombVertex, maximumOrder]];
writeValue["counts vacuum", orderCounts[vacuumDiagrams, InteractionPotential, orders]];
writeOrders["vacuum", vacuumDiagrams, {}, InteractionPotential, orders];
(* Z[eta, etabar] = exp(-S_int[d/d sources]) Z0, applied literally as for phi^4 below: psi <-> d/d etabar and psibar <->
   -d/d eta, so that S_int[d] = 1/2 Integral V(u, v) d/d eta(u) d/d eta(v) d/d etabar(v) d/d etabar(u) *)
applyCoulomb[F_] := SimplifyDiagrams[-1/2 IntegralOver[InteractionPotential[u, v] FunctionalDerivative[F, eta[u], eta[v], etabar[v],
       etabar[u]], {u, v}]];
coulombFirst = applyCoulomb[FreeFermionGeneratingFunctional];
coulombSecond = timed["electron Z second order", applyCoulomb[coulombFirst]];
(* the diagrams at a non-zero source, Z = Z0 (1 + z1 + z2 + ...) and W = log Z = log Z0 + w1 + w2 + ... *)
stripGaussian[e_] := SimplifyDiagrams[e /. Exp[_] -> 1];
electronZSource1 = stripGaussian[coulombFirst]; electronZSource2 = stripGaussian[coulombSecond/2];
electronWSource2 = SimplifyDiagrams[electronZSource2 - electronZSource1^2/2];
writeTerms["electron_z_source_1", electronZSource1, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
writeTerms["electron_z_source_2", electronZSource2, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
writeTerms["electron_w_source_2", electronWSource2, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
writeValue["counts electron Z and W at a source", Length[DiagramTerms[#]] & /@ {electronZSource1, electronZSource2, electronWSource2}];
writeValue["check electron W at a source connected", SimplifyDiagrams[electronWSource2 - ConnectedDiagrams[electronWSource2, {}]]];
(* Z/Z0 at zero source, disconnected diagrams included. fewSources: the terms with at most n sources (the factor
   Z0[sources] aside). The third order is used only at zero source: of the second order only the terms with at most four
   sources matter (four more derivatives, then zero source), and after each of the four derivatives only those with at
   most 3, 2, 1, 0 sources; the zero source is set before SimplifyDiagrams, which then sees a few terms. The results are
   those of the literal application of applyCoulomb, in a fraction of the time *)
fewSources[e_, n_, sources_: (_eta | _etabar)] := With[{t = Expand[e]}, Total[Select[If[Head[t] === Plus, List @@ t, {t}],
     Count[# /. Exp[_] -> 1, sources, Infinity] <= n &]]];
electronVacuumOrders = {SetSourcesToZero[coulombFirst], SetSourcesToZero[coulombSecond]/2,
   If[maximumOrder >= 3, timed["electron Z third order", Module[{f = fewSources[coulombSecond, 4]},
      Do[f = fewSources[FunctionalDerivative[f, sk[[1]]], sk[[2]]], {sk, {{etabar[u], 3}, {etabar[v], 2}, {eta[v], 1}}}];
      SetSourcesToZero[-1/2 IntegralOver[InteractionPotential[u, v] FunctionalDerivative[f, eta[u]], {u, v}]]/6]], 0]};
writeOrders["electron_vacuum_full", 1 + Total[electronVacuumOrders], {}, InteractionPotential, Range[0, maximumOrder]];
writeValue["counts electron vacuum full", orderCounts[Total[electronVacuumOrders], InteractionPotential, orders]];
(* the linked-cluster theorem: log(1 + z1 + z2 + z3) = z1 + z2 + z3 - z1^2/2 - z1 z2 + z1^3/3 to third order is the sum of
   the connected diagrams among those of Z/Z0 (ConnectedDiagrams only picks them); ConnectedVacuumDiagrams gives them directly *)
With[{z1 = electronVacuumOrders[[1]], z2 = electronVacuumOrders[[2]], z3 = electronVacuumOrders[[3]]},
  electronConnectedVacuum = ConnectedDiagrams[z1 + z2 + z3, {}];
  writeValue["check electron linked cluster", SimplifyDiagrams[OrderPart[z1 + z2 + z3 - z1^2/2 - z1 z2 + z1^3/3, InteractionPotential, orders] -
      electronConnectedVacuum]]];
writeValue["check electron connected vacuum shortcut", SimplifyDiagrams[vacuumDiagrams - electronConnectedVacuum]];
(* the two vacuum diagrams of first order, Fock (one loop) and Hartree (two loops), drawn alone: the factors of the
   products of second order *)
With[{t = List @@ Expand[electronVacuumOrders[[1]]]},
  writePicture["electron_vacuum_fock", First[Select[t, FreeQ[#, BareGreenFunction[p_, p_]] &]], {}];
  writePicture["electron_vacuum_hartree", First[Select[t, ! FreeQ[#, BareGreenFunction[p_, p_]] &]], {}]];
(* the numerator of G, -<psi(x) psibar(y) exp(-S_int)>_0 = d/d etabar(x) d/d eta(y) Z at zero source, to second order: its
   diagrams without vacuum bubbles times Z/Z0 *)
electronNumerator = SetSourcesToZero[FunctionalDerivative[FreeFermionGeneratingFunctional + coulombFirst + coulombSecond/2, etabar[x], eta[y]]];
writeOrders["electron_numerator", electronNumerator, {x, y}, InteractionPotential, {0, 1, 2}];
writeValue["counts electron numerator", orderCounts[electronNumerator, InteractionPotential, {0, 1, 2}]];
writeValue["check electron numerator factorizes", byOrder[SimplifyDiagrams[electronNumerator -
       DropVacuumBubbles[electronNumerator, {x, y}] (1 + electronVacuumOrders[[1]] + electronVacuumOrders[[2]])], InteractionPotential, Range[0, 2]]];
writeValue["check electron G without bubbles", SimplifyDiagrams[OrderPart[DropVacuumBubbles[electronNumerator, {x, y}] - greenFunction,
     InteractionPotential, {0, 1, 2}]]];
DeclareDirectedPropagator[DressedGreenFunction];
luttingerWard = timed["Phi", LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, maximumOrder,
     BareGreenFunction -> DressedGreenFunction]];
writeValue["counts Phi", orderCounts[luttingerWard, InteractionPotential, orders]];
writeOrders["luttinger_ward", luttingerWard, {}, InteractionPotential, orders];
(* Sigma(x, y) = dPhi/dG(y, x); SelfEnergy is the shortcut (checked below) *)
selfEnergy = timed["Sigma", SimplifyDiagrams[FunctionalDerivative[luttingerWard, DressedGreenFunction[y, x]]]];
writeValue["counts Sigma", orderCounts[selfEnergy, InteractionPotential, orders]];
writeOrders["self_energy", selfEnergy, {x, y}, InteractionPotential, orders];
(* Sigma[G] is the sum of the skeleton diagrams of the perturbative self-energy, with G0 -> G *)
skeletonSelfEnergy = SkeletonDiagrams[selfEnergyPerturbative, {x, y}, {BareGreenFunction}];
writeValue["counts Sigma skeleton", orderCounts[skeletonSelfEnergy, InteractionPotential, orders]];
writeValue["check Sigma = skeleton part of the perturbative Sigma",
  SimplifyDiagrams[(selfEnergy /. DressedGreenFunction -> BareGreenFunction) - skeletonSelfEnergy]];
(* Phi of order m has 2m lines (Euler): Tr Sigma(m) G = 2m Phi(m) *)
writeValue["check Tr Sigma G = 2m Phi", Table[SimplifyDiagrams[IntegralOver[OrderPart[selfEnergy, InteractionPotential, m] DressedGreenFunction[y, x], {x, y}] -
     2 m OrderPart[luttingerWard, InteractionPotential, m]], {m, orders}]];

(* ---------------- Gamma0 = dSigma/dG and the Ward identity ---------------- *)
(* Gamma0(x1, x2; x3, x4) = d^2 Phi/dG(x4, x3) dG(x2, x1); IrreducibleVertex is the shortcut (checked below) *)
irreducibleVertex = timed["Gamma0", SimplifyDiagrams[FunctionalDerivative[luttingerWard, DressedGreenFunction[x4, x3], DressedGreenFunction[x2, x1]]]];
writeValue["counts Gamma0", orderCounts[irreducibleVertex, InteractionPotential, orders]];
(* as DiagramPlot draws it, the legs x1 (out), x2 (in), x3 (out), x4 (in) counterclockwise from the top left *)
writeOrders["irreducible_vertex", irreducibleVertex, {x1, x2, x3, x4}, InteractionPotential, orders];
(* by channel: Gamma0 is particle-hole irreducible, so only the fully irreducible (bare) vertex and the diagrams reducible
   in the transverse particle-hole and particle-particle channels appear; the last drawn with the incoming legs x4, x2
   on the left, x1 and x3 exchanged where the particle entering at x2 leaves at x1 *)
gamma0Channels = ParquetDecomposition[irreducibleVertex, {x1, x2, x3, x4}, {DressedGreenFunction}];
writeValue["counts Gamma0 channels", Map[orderCounts[#, InteractionPotential, orders] &, gamma0Channels]];
writeOrders["irreducible_vertex_phbar", gamma0Channels["TransverseParticleHole"], {x1, x2, x3, x4}, InteractionPotential, orders];
writeOrders["irreducible_vertex_pp", gamma0Channels["ParticleParticle"], {x1, x2, x3, x4}, InteractionPotential, orders,
  "ExternalOrder" -> {"Uncrossed", {x4, x2, x3, x1}}];
(* Gamma0_{12;34} = d^2 Phi/dG_{21} dG_{43} is symmetric under 1 <-> 3, 2 <-> 4 *)
writeValue["check Gamma0 symmetry", SimplifyDiagrams[irreducibleVertex - (irreducibleVertex /. {x1 -> x3, x2 -> x4, x3 -> x1, x4 -> x2})]];
(* Sigma(m) has 2m - 1 lines: Gamma0(m) . G = (2m - 1) Sigma(m) *)
sigma12 = SimplifyDiagrams[selfEnergy /. {x -> x1, y -> x2}];
writeValue["check Gamma0 G = (2m-1) Sigma", Table[SimplifyDiagrams[IntegralOver[DressedGreenFunction[x4, x3] OrderPart[irreducibleVertex, InteractionPotential, m],
      {x3, x4}] - (2 m - 1) OrderPart[sigma12, InteractionPotential, m]], {m, orders}]];
(* Ward identity of charge conservation: Gamma0 . dG = dSigma for dG(x4, x3) = (theta(x4) - theta(x3)) G(x4, x3) *)
writeValue["check Ward identity", SimplifyDiagrams[IntegralOver[irreducibleVertex (theta[x4] - theta[x3]) DressedGreenFunction[x4, x3], {x3, x4}] -
    (theta[x1] - theta[x2]) sigma12]];

(* ---------------- two particles: F, Bethe-Salpeter, parquet ---------------- *)
twoParticle = PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x1], psibar[x2], psibar[x4], psi[x3]}, CoulombVertex, 1];
writeOrders["two_particle", twoParticle, {x1, x2, x3, x4}, InteractionPotential, {0, 1}];
(* the connected two-particle function, drawn to second order *)
twoParticleConnected = timed["two-particle connected", ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
     {psi[x1], psibar[x2], psibar[x4], psi[x3]}, CoulombVertex, maximumOrder]];
writeOrders["two_particle_connected", twoParticleConnected, {x1, x2, x3, x4}, InteractionPotential, {1, 2}];
writeValue["counts two-particle connected", orderCounts[twoParticleConnected, InteractionPotential, orders]];
(* moments from cumulants, by derivatives in the sources, to second order: W = log Z = -etabar.G0.eta + log(1 + s1 + s2)
   at a source (only the terms with at most four sources matter); the four-point moment, without its vacuum bubbles, is the
   cumulant plus the two pairings of G = -<psi psibar>_c, with the sign of the exchange *)
Module[{four = fewSources[#, 4] &, s1, s2, wJ, zJ, pts = {psi[x1], psibar[x2], psibar[x4], psi[x3]}, moment, w4, g},
  s1 = stripGaussian[coulombFirst]; s2 = stripGaussian[coulombSecond/2];
  wJ = SimplifyDiagrams[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}] + four[s1 + s2] - four[four[s1] four[s1]]/2];
  zJ = FreeFermionGeneratingFunctional + four[coulombFirst + coulombSecond/2];
  (* FieldDerivative takes the rightmost field first: one field at a time, keeping after each derivative only the terms
     with no more sources than derivatives left *)
  moment = Module[{f = zJ}, Do[f = fewSources[FieldDerivative[f, {pts[[k]]}], k - 1], {k, 4, 1, -1}];
    DropVacuumBubbles[SetSourcesToZero[f], {x1, x2, x3, x4}]];
  w4 = SetSourcesToZero[FieldDerivative[wJ, pts]];
  g[p_, q_] := -SetSourcesToZero[FieldDerivative[wJ, {psi[p], psibar[q]}]];
  writeValue["check two-particle cumulant connected", SimplifyDiagrams[w4 - ConnectedDiagrams[moment, {x1, x2, x3, x4}]]];
  writeValue["check two-particle moments from cumulants", SimplifyDiagrams[OrderPart[SimplifyDiagrams[moment - w4 - g[x1, x4] g[x3, x2] +
        g[x1, x2] g[x3, x4]], InteractionPotential, {0, 1, 2}]]];
  writeValue["check two-particle cumulant shortcut", SimplifyDiagrams[w4 - OrderPart[twoParticleConnected, InteractionPotential, {1, 2}]]]];
(* F = d^4 Gamma/dPsibar(x1) dPsi(x2) dPsibar(x3) dPsi(x4) at zero means, Gamma[Psibar, Psi] the 1PI action (the 1PI section, with bare
   lines), its skeleton diagrams with G0 -> G; PerturbativeFullVertex is the shortcut *)
gammaPsi1PI = timed["EA Gamma[Psibar, Psi]", EffectiveAction1PI[FreeFermionGeneratingFunctional, CoulombVertex, maximumOrder,
    {psi -> MeanPsi, psibar -> MeanPsibar}, BareGreenFunction -> BareGreenFunction]];
fullVertex = timed["F", SimplifyDiagrams[SkeletonDiagrams[SetSourcesToZero[FunctionalDerivative[gammaPsi1PI, MeanPsibar[x1], MeanPsi[x2],
        MeanPsibar[x3], MeanPsi[x4]]], {x1, x2, x3, x4}, {BareGreenFunction}] /. BareGreenFunction -> DressedGreenFunction]];
writeValue["check full vertex shortcut", SimplifyDiagrams[PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, maximumOrder,
     {psi, psibar}, {x1, x2, x3, x4}, BareGreenFunction -> DressedGreenFunction] - fullVertex]];
writeValue["counts F", orderCounts[fullVertex, InteractionPotential, orders]];
writeOrders["full_vertex", fullVertex, {x1, x2, x3, x4}, InteractionPotential, {1, 2}];
writeValue["check Bethe-Salpeter ph", SimplifyDiagrams[fullVertex - irreducibleVertex - BetheSalpeterLadder[irreducibleVertex, fullVertex,
     {x1, x2, x3, x4}, "ParticleHole", DressedGreenFunction, "MaximumOrder" -> {InteractionPotential, maximumOrder}]]];
directKernel = DeltaFunction[x1, x2] DeltaFunction[x3, x4] InteractionPotential[x1, x3];
oneBubble = BetheSalpeterLadder[directKernel, directKernel, {x1, x2, x3, x4}, "ParticleHole", DressedGreenFunction];
twoBubbles = BetheSalpeterLadder[directKernel, oneBubble, {x1, x2, x3, x4}, "ParticleHole", DressedGreenFunction];
writeTerms["rpa_chain", directKernel + oneBubble + twoBubbles, {x1, x2, x3, x4}, "LegDirections" -> "Alternating"];
bareVertex = OrderPart[fullVertex, InteractionPotential, 1];
writeTerms["pp_ladder", BetheSalpeterLadder[bareVertex, bareVertex, {x1, x2, x3, x4}, "ParticleParticle", DressedGreenFunction], {x1, x2, x3, x4},
  "LegDirections" -> "Alternating", "ExternalOrder" -> {"Uncrossed", {x4, x2, x3, x1}}];
fullVertexChannels = ParquetDecomposition[fullVertex, {x1, x2, x3, x4}, {DressedGreenFunction}];
writeValue["check Gamma0 = F - R_ph", SimplifyDiagrams[fullVertex - fullVertexChannels["ParticleHole"] - irreducibleVertex]];
writeOrders["vertex_ph", fullVertexChannels["ParticleHole"], {x1, x2, x3, x4}, InteractionPotential, orders];
parquetCheck = timed["parquet check", CheckParquetEquations[fullVertex, {x1, x2, x3, x4}, DressedGreenFunction, InteractionPotential, maximumOrder]];
(* the Schwinger-Dyson equation Sigma = Sigma_HF - V G G G F, from the second order on; Sigma at order m + 1 has as many
   diagrams as F at order m *)
schwingerDyson = -IntegralOver[InteractionPotential[x, u] DressedGreenFunction[x, a] DressedGreenFunction[d, u] DressedGreenFunction[u, c] *
    (fullVertex /. {x1 -> a, x2 -> y, x3 -> c, x4 -> d}), {u, a, c, d}];
writeValue["check Schwinger-Dyson", byOrder[selfEnergy - schwingerDyson, InteractionPotential, Range[2, maximumOrder]]];
(* crossing: F(1, 2; 3, 4) = -F(3, 2; 1, 4) = -F(1, 4; 3, 2), and R_ph(1, 2; 3, 4) = -R_phbar(3, 2; 1, 4) *)
writeValue["check crossing", {SimplifyDiagrams[fullVertex + (fullVertex /. {x1 -> x3, x3 -> x1})],
   SimplifyDiagrams[fullVertex + (fullVertex /. {x2 -> x4, x4 -> x2})],
   SimplifyDiagrams[fullVertexChannels["ParticleHole"] + (fullVertexChannels["TransverseParticleHole"] /. {x1 -> x3, x3 -> x1})]}];
writeValue["parquet residuals", parquetCheck["Residuals"]];
writeValue["parquet counts", parquetCheck["DiagramCounts"]];

(* ---------------- the exact equations, drawn with blobs ---------------- *)
(* in the Dyson equation the full propagator FullG is a double line and the bare one BareGreenFunction a single line; in the
   other exact equations every line is a full propagator, drawn as a plain line; Sigma, F, Gamma0, Lambda and
   the vertices Gamma_r irreducible in one channel are blobs with their names inside, and the four-point ones squares
   with the legs x1, x2, x3, x4 at the corners, counterclockwise from the top left. The fills follow one rule: a vertex
   function the more densely the more diagrams it contains (the fully irreducible vertex dotted, the irreducible parts
   hatched, the full vertex functions cross-hatched), the correlation and response functions white *)
DeclareLineStyle[FullG, "DoubleArrow"];
DeclareBlob[SigmaBlob, "\\Sigma"];
DeclareBlob[FBox, "F", "Shape" -> "Box", "Fill" -> "CrossHatched"];
DeclareBlob[KernelBox, "\\Gamma^0", "Shape" -> "Box"];
DeclareBlob[FullyIrreducibleBox, "\\Gamma_{\\rm irr}", "Shape" -> "Box", "Fill" -> "Dotted"];
DeclareBlob[IrreducibleBoxPh, "\\Gamma_{\\rm ph}", "Shape" -> "Box"];
DeclareBlob[IrreducibleBoxPhbar, "\\Gamma_{\\overline{\\rm ph}}", "Shape" -> "Box"];
DeclareBlob[IrreducibleBoxPp, "\\Gamma_{\\rm pp}", "Shape" -> "Box"];
blobOptions = Sequence["Scale" -> 0.55, "LineWidth" -> 0.45, "LabelFont" -> "\\footnotesize"];
fourLabels = {x1, x2, x3, x4};
writeTerms["exact_dyson_lhs", FullG[x, y], {x, y}, blobOptions];
writeTerms["exact_dyson", BareGreenFunction[x, y] + IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] FullG[v, y], {u, v}], {x, y},
  blobOptions];
writeTerms["exact_sigma_lhs", SigmaBlob[x, y], {x, y}, "LegDirections" -> {"Out", "In"}, blobOptions];
(* Hartree and Fock with the full propagator, and the term with F, as in the check of the Schwinger-Dyson equation *)
(* F(a, y; c, d) drawn with its ports in the order (c, a, y, d): a and y at the bottom, on the axis of the self-energy,
   c and d at the top *)
DeclareBlob[FBoxAxis, "F", "Shape" -> "Box", "Fill" -> "CrossHatched", "AxialPorts" -> {2, 3}];
writeTerms["exact_schwinger_dyson", OrderPart[selfEnergy, InteractionPotential, 1] -
   IntegralOver[InteractionPotential[x, u] DressedGreenFunction[x, a] DressedGreenFunction[d, u] DressedGreenFunction[u, c] FBoxAxis[c, a, y, d],
    {u, a, c, d}], {x, y},
  "LegDirections" -> {"Out", "In"}, blobOptions];
writeTerms["exact_vertex_lhs", FBox @@ fourLabels, fourLabels, "LegDirections" -> "Alternating", blobOptions];
writeTerms["exact_bethe_salpeter", KernelBox @@ fourLabels + BetheSalpeterLadder[KernelBox @@ fourLabels, FBox @@ fourLabels, fourLabels,
    "ParticleHole", DressedGreenFunction], fourLabels, "LegDirections" -> "Alternating", blobOptions];
writeTerms["exact_parquet", FullyIrreducibleBox @@ fourLabels +
   BetheSalpeterLadder[IrreducibleBoxPh @@ fourLabels, FBox @@ fourLabels, fourLabels, "ParticleHole", DressedGreenFunction] +
   BetheSalpeterLadder[IrreducibleBoxPhbar @@ fourLabels, FBox @@ fourLabels, fourLabels, "TransverseParticleHole", DressedGreenFunction] +
   (* the particle-particle term, -1/2 Gamma_pp(x1, 5; x3, 6) G(5, 7) G(6, 8) F(7, x2; 8, x4), with the boxes turned as in the
      ladder of the T-matrix: the incoming particles x4, x2 on the left, the two lines parallel from left to right (the
      corners of its ports, kept with "Uncrossed"; by default its legs x1 and x4 would cross back to the order x1, ..., x4) *)
   With[{p5 = Unique["p"], p6 = Unique["p"], p7 = Unique["p"], p8 = Unique["p"]},
    -1/2 IntegralOver[FBox[x4, x2, p8, p7] DressedGreenFunction[p5, p7] DressedGreenFunction[p6, p8] IrreducibleBoxPp[p5, p6, x3, x1],
      {p5, p6, p7, p8}]], fourLabels,
  "LegDirections" -> "Alternating", "ExternalOrder" -> "Uncrossed", blobOptions];
(* the pair of full propagators of the particle-hole channel, L0(x1, x2; x3, x4) = G(x1, x4) G(x3, x2): the two lines that the
   Bethe-Salpeter equation cuts, F = Gamma0 + Gamma0 L0 F *)
writeTerms["pair_l0", DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2], fourLabels, blobOptions];
(* a four-point function with its four legs, the full propagators L0 e L0: G(x1, a) G(b, x2) e(a, b; c, d) G(d, x4) G(x3, c) *)
withLegs[e_] := With[{p = Unique["p"], q = Unique["p"], r = Unique["p"], s = Unique["p"]},
   IntegralOver[DressedGreenFunction[x1, p] DressedGreenFunction[q, x2] (e /. Thread[fourLabels -> {p, q, r, s}]) *
     DressedGreenFunction[s, x4] DressedGreenFunction[x3, r], {p, q, r, s}]];
(* the connected two-particle function is F with its legs *)
writeTerms["two_particle_f_legs", withLegs[FBox @@ fourLabels], fourLabels, "LegDirections" -> "Alternating", blobOptions];
(* the two pairings of full propagators in the two-particle function, G(x1, x4) G(x3, x2) - G(x1, x2) G(x3, x4) *)
writeTerms["two_particle_pairs", DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2] - DressedGreenFunction[x1, x2] DressedGreenFunction[x3, x4],
  fourLabels, blobOptions];

(* ---------------- vertices of one-body operators and Hedin's equations ---------------- *)
(* a local potential U(z) coupled to the density (linear response): G0^-1 -> G0^-1 - U, dG0(p, q)/dU(z) = G0(p, z) G0(z, q) *)
DeclareSource[LocalPotential];
DeclareFunctionalDerivative[BareGreenFunction[p_, q_], LocalPotential[r_], BareGreenFunction[p, r] BareGreenFunction[r, q]];
(* the vertex of the density, Lambda(x1, x2; z) = -dG^-1(x1, x2)/dU(z) = gamma + dSigma(x1, x2)/dU(z): the perturbative Sigma
   differentiated through its bare lines, its skeleton diagrams with G0 -> G; PerturbativeDensityVertex is the shortcut *)
densityVertex = timed["Lambda", SimplifyDiagrams[DeltaFunction[x1, z] DeltaFunction[x2, z] + (SkeletonDiagrams[
       FunctionalDerivative[selfEnergyPerturbative /. {x -> x1, y -> x2}, LocalPotential[z]], {x1, x2, z}, {BareGreenFunction}] /.
      BareGreenFunction -> DressedGreenFunction)]];
writeValue["check density vertex shortcut", SimplifyDiagrams[PerturbativeDensityVertex[FreeFermionGeneratingFunctional, CoulombVertex,
     maximumOrder, {psi, psibar}, {x1, x2, z}, BareGreenFunction -> DressedGreenFunction] - densityVertex]];
writeValue["counts Lambda", orderCounts[densityVertex, InteractionPotential, Range[0, maximumOrder]]];
writeOrders["density_vertex", densityVertex, {x1, x2, z}, InteractionPotential, {0, 1, 2}];
exactObjects = {"G" -> DressedGreenFunction, "V" -> InteractionPotential, "Sigma" -> LabelFunction[selfEnergy, {x, y}],
   "Gamma0" -> LabelFunction[irreducibleVertex, {x1, x2, x3, x4}], "F" -> LabelFunction[fullVertex, {x1, x2, x3, x4}],
   "Lambda" -> LabelFunction[densityVertex, {x1, x2, z}]};
(* Lambda = gamma + Gamma0 L0 Lambda = gamma + F L0 gamma, Sigma = Sigma_H - V G Lambda, and the density response
   chi = -<rho rho>_c = gamma L0 Lambda (to one order less: it needs the vertex at the same order) *)
densityCorrelationSkeleton = SimplifyDiagrams[SkeletonDiagrams[ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
      {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, maximumOrder - 1], {a, b}, {BareGreenFunction}] /.
    BareGreenFunction -> DressedGreenFunction];
writeValue["check vertex equations", {
   CheckEquation[ExactEquation["Vertex", exactObjects, {x1, x2, z}, "MaximumOrder" -> {InteractionPotential, maximumOrder}],
    InteractionPotential, maximumOrder],
   CheckEquation[ExactEquation["VertexFromF", exactObjects, {x1, x2, z}, "MaximumOrder" -> {InteractionPotential, maximumOrder}],
    InteractionPotential, maximumOrder],
   CheckEquation[ExactEquation["SelfEnergyFromVertex", exactObjects, {x, y}, "MaximumOrder" -> {InteractionPotential, maximumOrder}],
    InteractionPotential, maximumOrder],
   byOrder[densityCorrelationSkeleton + IntegralOver[DressedGreenFunction[a, c] DressedGreenFunction[d, a] *
         (densityVertex /. {x1 -> c, x2 -> d, z -> b}), {c, d}], InteractionPotential, Range[0, maximumOrder - 1]]}];
(* Hedin's equations: the proper vertex (irreducible with respect to an interaction line), the polarization
   P = proper part of chi = -<rho rho>_c, the screened interaction W = V + V chi V, and Gamma_xc = Gamma0 without the
   Hartree term *)
properVertex = IrreducibleDiagrams[densityVertex, {{x1, x2}, {z}}, 1, {InteractionPotential}];
writeValue["counts proper vertex", orderCounts[properVertex, InteractionPotential, Range[0, maximumOrder]]];
polarization = -IrreducibleDiagrams[densityCorrelationSkeleton, {{a}, {b}}, 1, {InteractionPotential}];
screenedInteraction = SimplifyDiagrams[InteractionPotential[a, b] - IntegralOver[InteractionPotential[a, c] *
      (densityCorrelationSkeleton /. {a -> c, b -> d}) InteractionPotential[d, b], {c, d}]];
hedinEquations = ExactEquation["Hedin", {"G" -> DressedGreenFunction, "V" -> InteractionPotential, "Sigma" -> LabelFunction[selfEnergy, {x, y}],
    "W" -> LabelFunction[screenedInteraction, {a, b}], "P" -> LabelFunction[polarization, {a, b}],
    "Lambda" -> LabelFunction[properVertex, {x1, x2, z}], "GammaXC" -> LabelFunction[irreducibleVertex -
       DeltaFunction[x1, x2] DeltaFunction[x3, x4] InteractionPotential[x1, x3], {x1, x2, x3, x4}]}, {x, y, z},
   "MaximumOrder" -> {InteractionPotential, maximumOrder}];
writeValue["check Hedin", MapThread[CheckEquation[#1, InteractionPotential, #2] &, {hedinEquations,
    {maximumOrder, maximumOrder - 1, maximumOrder, maximumOrder}}]];
(* the exact equations drawn: Lambda a cross-hatched triangle (a full vertex), the proper vertex a hatched one (an
   irreducible part, as P and Gamma_xc), W a double wavy line *)
DeclareBlob[LambdaTriangle, "\\Lambda", "Shape" -> "Triangle", "Fill" -> "CrossHatched"];
DeclareBlob[ProperVertexTriangle, "\\tilde\\Lambda", "Shape" -> "Triangle"];
DeclareBlob[LambdaTriangleUp, "\\Lambda", "Shape" -> "TriangleUp", "Fill" -> "CrossHatched"];
DeclareBlob[ProperVertexTriangleUp, "\\tilde\\Lambda", "Shape" -> "TriangleUp"];
DeclareBlob[PolarizationBlob, "P"];
DeclareBlob[XCBox, "\\Gamma_{\\rm xc}", "Shape" -> "Box"];
DeclareSymmetric[ScreenedW]; DeclareInteractionLine[ScreenedW, "DoubleWavy"];
blobObjects = {"G" -> DressedGreenFunction, "Sigma" -> SigmaBlob, "Lambda" -> LambdaTriangle, "Gamma0" -> KernelBox, "F" -> FBox,
   "V" -> InteractionPotential, "W" -> ScreenedW, "P" -> PolarizationBlob, "GammaXC" -> XCBox};
vertexLegs = "LegDirections" -> {"Out", "In", None};
writeTerms["exact_lambda_lhs", LambdaTriangle[x1, x2, z], {x1, x2, z}, vertexLegs, blobOptions];
writeTerms["exact_vertex_equation", Last[ExactEquation["Vertex", blobObjects, {x1, x2, z}]], {x1, x2, z}, vertexLegs, blobOptions];
writeTerms["exact_vertex_from_f", Last[ExactEquation["VertexFromF", blobObjects, {x1, x2, z}]], {x1, x2, z}, vertexLegs, blobOptions];
writeTerms["exact_sigma_from_vertex", Last[ExactEquation["SelfEnergyFromVertex", Join[{"Lambda" -> LambdaTriangleUp}, blobObjects], {x, y}]], {x, y},
  "LegDirections" -> {"Out", "In"}, blobOptions];
With[{hedin = ExactEquation["Hedin", Join[{"Lambda" -> ProperVertexTriangle}, blobObjects], {x, y, z}]},
  writeTerms["hedin_sigma", Last[ExactEquation["Hedin", Join[{"Lambda" -> ProperVertexTriangleUp}, blobObjects], {x, y, z}][[1]]], {x, y},
   "LegDirections" -> {"Out", "In"}, blobOptions];
  writeTerms["hedin_p", Last[hedin[[2]]], {x, y}, blobOptions];
  (* the equation of W times -1: each wavy line stands for -V and -W, so that W itself is drawn without a sign *)
  writeTerms["hedin_w", -Last[hedin[[3]]], {x, y}, blobOptions];
  (* the proper vertex on x1, x2, z, as the other equations for vertices *)
  writeTerms["hedin_lambda", Last[ExactEquation["Hedin", Join[{"Lambda" -> ProperVertexTriangle}, blobObjects], {x1, x2, z}][[4]]], {x1, x2, z},
   vertexLegs, blobOptions]];
writeTerms["hedin_w_lhs", -ScreenedW[x, y], {x, y}, blobOptions];
writeTerms["hedin_lambda_lhs", ProperVertexTriangle[x1, x2, z], {x1, x2, z}, vertexLegs, blobOptions];
writeTerms["hedin_p_lhs", PolarizationBlob[x, y], {x, y}, blobOptions];
(* GW: the proper vertex replaced by the bare one *)
With[{gw = ExactEquation["Hedin", Join[{"Lambda" -> LabelFunction[DeltaFunction[x1, z] DeltaFunction[x2, z], {x1, x2, z}]}, blobObjects], {x, y, z}]},
  writeTerms["gw_sigma", Last[gw[[1]]], {x, y}, "LegDirections" -> {"Out", "In"}, blobOptions];
  writeTerms["gw_p", Last[gw[[2]]], {x, y}, blobOptions]];
(* the density response chi = delta<rho>/delta U = -<rho rho>_c: the pair L0 closed on a density point and on the vertex,
   chi = gamma L0 Lambda (checked above, with the vertex equations), and the screened interaction W = V + V chi V *)
DeclareBlob[ChiBlob, "\\chi", "Fill" -> "White"];
writeTerms["exact_chi_lhs", ChiBlob[x, y], {x, y}, blobOptions];
writeTerms["exact_chi_from_vertex", IntegralOver[DressedGreenFunction[x, c] DressedGreenFunction[d, x] LambdaTriangle[c, d, y], {c, d}], {x, y},
  blobOptions];
writeTerms["hedin_w_chi", -InteractionPotential[x, y] - IntegralOver[InteractionPotential[x, u] ChiBlob[u, v] InteractionPotential[v, y], {u, v}],
  {x, y}, blobOptions];
(* linear response: chi = chi0 + gamma L0 F L0 gamma (the bubble, and F with its legs closed on two density points); the
   two-particle correlation function L(x1, x2; x3, x4) = dG(x1, x2)/dU(x4, x3) = L0 + L0 F L0 and its Bethe-Salpeter
   equation L = L0 + L0 Gamma0 L, drawn iterated; the chain of proper parts chi = P + P V chi *)
DeclareBlob[LBox, "L", "Shape" -> "Box", "Fill" -> "White"];
(* the response of the propagator by differentiation: the lines G0 depend on the one-body source U, G0^-1 -> G0^-1 - U,
   dG0(1, 2)/dU(4, 3) = G0(1, 4) G0(3, 2); L = dG/dU is the pair of full propagators plus the connected two-particle function *)
DeclareSource[PotentialU];
DeclareFunctionalDerivative[BareGreenFunction[p_, q_], PotentialU[r_, s_], BareGreenFunction[p, r] BareGreenFunction[s, q]];
responseL = timed["L = dG/dU", SimplifyDiagrams[FunctionalDerivative[greenFunction /. {x -> x1, y -> x2}, PotentialU[x4, x3]]]];
writeValue["counts L", orderCounts[responseL, InteractionPotential, Range[0, maximumOrder]]];
writeOrders["response_dg_du", responseL, fourLabels, InteractionPotential, {0, 1}];
(* order by order: the part of order m of G G is the sum of the products of the parts of G of orders k and m - k *)
writeValue["check L = G G - C", Table[SimplifyDiagrams[orderPart[responseL, InteractionPotential, m] -
     Sum[Expand[(orderPart[greenFunction, InteractionPotential, k] /. {x -> x1, y -> x4}) (orderPart[greenFunction, InteractionPotential, m - k] /.
          {x -> x3, y -> x2})], {k, 0, m}] - orderPart[twoParticleConnected, InteractionPotential, m]], {m, 0, maximumOrder}]];
(* the chain rule: Sigma[G] depends on U through G, dG/dU = L, so that dSigma/dU = Gamma0 L *)
DeclareFunctionalDerivative[DressedGreenFunction[p_, q_], PotentialU[r_, s_], LBox[p, q, s, r]];
sigmaChain = timed["chain rule", SimplifyDiagrams[FunctionalDerivative[selfEnergy /. {x -> x1, y -> x2}, PotentialU[x4, x3]]]];
writeValue["check chain rule", SimplifyDiagrams[sigmaChain - IntegralOver[(irreducibleVertex /. {x3 -> s3, x4 -> s4}) LBox[s4, s3, x3, x4], {s3, s4}]]];
(* with the legs out, in, out, in, as the showcase draws it (DiagramPlot of the whole sum leaves the legs x1 and x2,
   which in some terms end at the box L, without arrows) *)
writeOrders["sigma_chain", sigmaChain, fourLabels, InteractionPotential, {1}, "LegDirections" -> "Alternating", blobOptions];
writeTerms["response_chi_f", DressedGreenFunction[x, y] DressedGreenFunction[y, x] + IntegralOver[DressedGreenFunction[x, a] DressedGreenFunction[b, x] *
    FBox[a, b, c, d] DressedGreenFunction[y, c] DressedGreenFunction[d, y], {a, b, c, d}], {x, y}, blobOptions];
writeTerms["response_l_lhs", LBox @@ fourLabels, fourLabels, "LegDirections" -> "Alternating", blobOptions];
writeTerms["response_l", DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2] + withLegs[FBox @@ fourLabels], fourLabels,
  "LegDirections" -> "Alternating", blobOptions];
writeTerms["response_l_ladder", DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2] + withLegs[KernelBox @@ fourLabels] +
   withLegs[BetheSalpeterLadder[KernelBox @@ fourLabels, KernelBox @@ fourLabels, fourLabels, "ParticleHole", DressedGreenFunction]], fourLabels,
  "LegDirections" -> "Alternating", blobOptions];
writeTerms["response_rpa", PolarizationBlob[x, y] + IntegralOver[PolarizationBlob[x, u] InteractionPotential[u, v] ChiBlob[v, y], {u, v}], {x, y},
  blobOptions];
(* the legend of the fills (section 4): Gamma_irr dotted, Gamma0 hatched, F cross-hatched, L white, drawn together (one
   size) without the labels of their legs, each written to fill_legend_NAME.terms *)
With[{parts = DiagramToTikZ[FullyIrreducibleBox @@ fourLabels + KernelBox @@ fourLabels + FBox @@ fourLabels + LBox @@ fourLabels, fourLabels,
    "Output" -> "List", "Labels" -> Thread[fourLabels -> ""], blobOptions, Sequence @@ pictureOptions]},
  Do[With[{pic = SelectFirst[parts[[All, 2]], StringContainsQ[#, "{$" <> f[[2]] <> "$}"] &]},
     Export["figures/fill_legend_" <> f[[1]] <> ".terms", "1\n%%PICTURE%%\n" <> pic, "Text"]],
   {f, {{"dotted", "\\Gamma_{\\rm irr}"}, {"hatched", "\\Gamma^0"}, {"crosshatched", "F"}, {"white", "L"}}}]];
With[{chiF = LabelFunction[-densityCorrelationSkeleton, {a, b}], pF = LabelFunction[polarization, {a, b}],
    fF = LabelFunction[fullVertex, fourLabels], gamma0F = LabelFunction[irreducibleVertex, fourLabels],
    lF = LabelFunction[DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2] + withLegs[fullVertex], fourLabels],
    g = DressedGreenFunction},
  writeValue["check response equations", timed["response equations", {
    CheckEquation[chiF[x, y] == g[x, y] g[y, x] + IntegralOver[g[x, u1] g[u2, x] fF[u1, u2, u3, u4] g[y, u3] g[u4, y], {u1, u2, u3, u4}],
     InteractionPotential, maximumOrder - 1],
    CheckEquation[lF[x1, x2, x3, x4] == g[x1, x4] g[x3, x2] + IntegralOver[g[x1, u1] g[u2, x2] gamma0F[u1, u2, u3, u4] lF[u4, u3, x3, x4],
       {u1, u2, u3, u4}], InteractionPotential, maximumOrder],
    CheckEquation[chiF[x, y] == pF[x, y] + IntegralOver[pF[x, u1] InteractionPotential[u1, u2] chiF[u2, y], {u1, u2}], InteractionPotential,
     maximumOrder - 1]}]]];

(* ---------------- Hubbard-Stratonovich: a functional of G and W ---------------- *)
(* the interaction 1/2 rho V rho as the exchange of a boson with <phi phi>_0 = V, coupled by -i Integral phi rho *)
DeclareSource[Jhs]; DeclareField[hsField, Jhs]; DeclareSymmetric[HSPropagator]; DeclareInteractionLine[HSPropagator];
FreeHSGeneratingFunctional = FreeFermionGeneratingFunctional Exp[1/2 IntegralOver[Jhs[x] HSPropagator[x, y] Jhs[y], {x, y}]];
HSVertex = -I IntegralOver[hsField[z] psibar[z] ** psi[z], {z}];
hsGreenFunction = timed["HS G", -PerturbativeCorrelationFunction[FreeHSGeneratingFunctional, {psi[x], psibar[y]}, HSVertex, 2 maximumOrder]];
writeValue["check HS G", byOrder[hsGreenFunction - (greenFunction /. InteractionPotential -> HSPropagator),
      HSPropagator, Range[0, maximumOrder]]];
(* the same, by HubbardStratonovich, and back by IntegrateOutField *)
hsTheory = HubbardStratonovich[{FreeFermionGeneratingFunctional, CoulombVertex}, psibar[#] ** psi[#] &, {hsField, Jhs}, HSPropagator];
writeValue["check HubbardStratonovich", {SimplifyDiagrams[hsTheory["Interaction"] - HSVertex],
   SimplifyDiagrams[hsTheory["FreeGeneratingFunctional"] - FreeHSGeneratingFunctional],
   SimplifyDiagrams[(IntegrateOutField[{FreeHSGeneratingFunctional, HSVertex}, hsField, Jhs]["Interaction"] /.
       HSPropagator -> InteractionPotential) - CoulombVertex]}];
(* the 2PI functional of G and W, Sigma = dPhi/dG(y, x) and the polarization P = -2 dPhi/dW *)
phiGW = timed["Phi[G,W]", LuttingerWardFunctional[FreeHSGeneratingFunctional, HSVertex, 2 maximumOrder,
     {BareGreenFunction -> DressedGreenFunction, HSPropagator -> ScreenedW}]];
writeValue["counts Phi[G,W]", orderCounts[phiGW, ScreenedW, orders]];
(* in Phi[G, W] and its derivatives every line is a full one: W, alone, is drawn with the plain wavy line, as G with the
   plain arrow; the double wavy line is kept for the equations in which V and W appear together (Hedin's) *)
skeletonW = "Interactions" -> {ScreenedW};
writeOrders["phi_gw", phiGW, {}, ScreenedW, orders, skeletonW];
sigmaGW = SimplifyDiagrams[FunctionalDerivative[phiGW, DressedGreenFunction[y, x]]];
polarizationGW = SimplifyDiagrams[-2 FunctionalDerivative[phiGW, ScreenedW[a, b]]];
writeValue["check GW derivatives", {SimplifyDiagrams[sigmaGW - SelfEnergy[phiGW, DressedGreenFunction, {x, y}]],
   SimplifyDiagrams[polarizationGW - SelfEnergy[phiGW, ScreenedW, {a, b}, "Statistics" -> "Boson"]]}];
writeValue["counts Sigma[G,W]", orderCounts[sigmaGW, ScreenedW, orders]];
writeValue["counts P[G,W]", orderCounts[polarizationGW, ScreenedW, Range[0, maximumOrder - 1]]];
writeOrders["sigma_gw", sigmaGW, {x, y}, ScreenedW, {1, 2}, skeletonW];
writeOrders["polarization_gw", polarizationGW, {a, b}, ScreenedW, {0, 1}, skeletonW];
(* W expressed by G (W = V - V <rho rho>_c V) substituted, each term of order W^k with W only to the order needed *)
substituteToOrder[e_, w_, replacement_, x_, n_] := SimplifyDiagrams[OrderPart[Total[Table[
      With[{f = LabelFunction[OrderPart[replacement, x, Range[1, n - k + 1]], {a, b}]}, OrderPart[e, w, k] /. w[p_, q_] :> f[p, q]],
      {k, 0, n}]], x, Range[0, n]]];
writeValue["check Sigma[G,W[G]]", byOrder[substituteToOrder[sigmaGW, ScreenedW, screenedInteraction, InteractionPotential,
       maximumOrder] + DeltaFunction[x, y] IntegralOver[InteractionPotential[x, u] DressedGreenFunction[u, u], {u}] - selfEnergy, InteractionPotential, Range[0, maximumOrder]]];
writeValue["check P[G,W[G]]", byOrder[substituteToOrder[polarizationGW, ScreenedW, screenedInteraction, InteractionPotential,
       maximumOrder - 1] - polarization, InteractionPotential, Range[0, maximumOrder - 1]]];

(* ---------------- linear response ---------------- *)
(* <rho(a) rho(b)>_c = -chi = -d<rho(a)>/dU(b) = -dG(a, a)/dU(b), with the local potential declared above;
   ConnectedCorrelationFunction is the shortcut *)
densityCorrelation = SimplifyDiagrams[-FunctionalDerivative[OrderPart[greenFunction, InteractionPotential, {0, 1}] /. {x -> a, y -> a},
     LocalPotential[b]]];
writeValue["check density correlation shortcut", SimplifyDiagrams[ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
     {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1] - densityCorrelation]];
writeOrders["density_correlation", densityCorrelation, {a, b}, InteractionPotential, {0, 1}];

(* ---------------- electron-phonon ---------------- *)
DeclareSource[Jphonon]; DeclareField[phonon, Jphonon]; DeclareSymmetric[PhononPropagator]; DeclareLineStyle[PhononPropagator, "Dashed"];
FreeElectronPhononGeneratingFunctional = FreeFermionGeneratingFunctional Exp[1/2 IntegralOver[Jphonon[x] PhononPropagator[x, y] Jphonon[y], {x, y}]];
ElectronPhononVertex = g IntegralOver[phonon[z] psibar[z] ** psi[z], {z}];
electronPhononGreen = -PerturbativeCorrelationFunction[FreeElectronPhononGeneratingFunctional, {psi[x], psibar[y]}, ElectronPhononVertex, 2];
(* the phonons integrated out: the retarded interaction -g^2/2 Integral rho D rho of the electrons, and the same Green function *)
phononEffective = IntegrateOutField[{FreeElectronPhononGeneratingFunctional, ElectronPhononVertex}, phonon, Jphonon];
writeFormula["phonon_effective_interaction", phononEffective["Interaction"], "Names" -> {PhononPropagator -> "D_0"}];
writeValue["check phonons integrated out", {SimplifyDiagrams[phononEffective["Interaction"] +
     g^2/2 IntegralOver[PhononPropagator[x, y] psibar[x] ** psi[x] ** psibar[y] ** psi[y], {x, y}]],
   SimplifyDiagrams[electronPhononGreen + PerturbativeCorrelationFunction[phononEffective["FreeGeneratingFunctional"], {psi[x], psibar[y]},
       phononEffective["Interaction"], 1]]}];
(* in the pictures each vertex stands for -g: the coupling is absorbed into the vertices (g is also the coupling of QCD
   below, whose local vertices stand for minus their kernels, so it is not declared with DeclareCoupling) *)
writeOrders["electron_phonon_green", electronPhononGreen, {x, y}, g, {0, 2}, "Couplings" -> {g}];
writeOrders["phonon_green", PerturbativeCorrelationFunction[FreeElectronPhononGeneratingFunctional, {phonon[x], phonon[y]}, ElectronPhononVertex, 2],
  {x, y}, g, {0, 2}, "Couplings" -> {g}];

(* ---------------- from diagrams to numbers: Matsubara sums ---------------- *)
(* the electron gas in momentum and Matsubara frequency: xi the dispersion, v the interaction, G0(k, i w) = 1/(i w - xi_k);
   a two-point function has k, i w entering at y, a density correlation q, i nu entering at b *)
DeclareMatsubaraFrequency[w, "Fermion"]; DeclareMatsubaraFrequency[nu, "Boson"];
gasRules = {BareGreenFunction -> {"Fermion", xi}, DressedGreenFunction -> {"Fermion", xi}, InteractionPotential -> {"Instantaneous", v}};
twoPoint = {y -> {k, I w}, x -> {-k, -I w}};
bubblePoints = {b -> {q, I nu}, a -> {-q, -I nu}};
(* the LaTeX of the frequencies, and of the energies and occupations of the lines of the second-order self-energy *)
frequencyNames = {w -> "\\omega_n", nu -> "\\nu_m"};
energyNames = {xi[p1] -> "\\xi_1", xi[p2] -> "\\xi_2", xi[k - p1 + p2] -> "\\xi_3", FermiFunction[xi[p1]] -> "n_1",
   FermiFunction[xi[p2]] -> "n_2", FermiFunction[xi[k - p1 + p2]] -> "n_3"};
withoutMomentumSum[e_] := e /. MomentumSum[f_, _] :> f;
(* the Lindhard function: the package's <rho rho>_c = -G G is minus chi_0 *)
lindhard = timed["Lindhard", EvaluateMatsubara[OrderPart[densityCorrelation, InteractionPotential, 0], bubblePoints, gasRules,
    "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]];
writeFormula["lindhard", lindhard, "Names" -> frequencyNames, "Subscripts" -> {xi}];
writeValue["check Lindhard = -chi0", Together[withoutMomentumSum[lindhard] +
     2 (FermiFunction[xi[p]] - FermiFunction[xi[p + q]])/(I nu + xi[p] - xi[p + q])]];
lindhardImaginary = RetardedImaginaryPart[AnalyticContinuation[lindhard, nu -> omega], omega];
writeFormula["lindhard_imaginary", lindhardImaginary, "Subscripts" -> {xi}];
(* the step function (UnitStep, written \theta by the package) as \Theta: theta is the gauge function of the Ward identity *)
With[{tex = StringReplace[ExpressionToLaTeX[ZeroTemperatureLimit[lindhard], "Names" -> frequencyNames, "Subscripts" -> {xi}], "\\theta(" -> "\\Theta("]},
  Export["figures/formula_lindhard_zero_temperature.tex", tex, "Text"]; Print["lindhard_zero_temperature: ", tex]];
(* at T = 0 in one dimension (k_F = m = 1), static: minus the Lindhard function, -chi0 = (2/(Pi q)) log|(2 + q)/(2 - q)| *)
With[{integrand = withoutMomentumSum[ZeroTemperatureLimit[lindhard /. nu -> 0]] /. {xi -> Function[s, s^2/2 - 1/2], q -> 4/5}},
  writeValue["check Lindhard T=0 one dimension", If[Abs[NIntegrate[integrand, {p, -3, -9/5, -1, 1/5, 1, 3}]/(2 Pi) -
        2/(Pi 4/5) Log[(2 + 4/5)/(2 - 4/5)]] < 10^-6, 0, 1]]];
(* the second-order self-energy: the two skeleton diagrams, their imaginary part by residues and by the cutting rules *)
selfEnergy2 = OrderPart[skeletonSelfEnergy, InteractionPotential, 2];
sigma2 = timed["Sigma(2) Matsubara", EvaluateMatsubara[selfEnergy2, twoPoint, gasRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}]];
writeFormula["sigma2_matsubara", sigma2, "Names" -> Join[energyNames, frequencyNames], "Subscripts" -> {v}];
sigma2Imaginary = RetardedImaginaryPart[AnalyticContinuation[sigma2, w -> omega], omega];
writeFormula["sigma2_imaginary", sigma2Imaginary, "Names" -> energyNames, "Subscripts" -> {v}];
sigma2Cut = timed["Sigma(2) cutting rules", CuttingRuleImaginaryPart[selfEnergy2, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2,
    "LoopMomenta" -> {p1, p2}]];
writeValue["check cutting rules Sigma2", {CheckCuttingRules[selfEnergy2, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2],
   If[sigma2Imaginary === sigma2Cut, 0, 1]}];
(* self-energy insertions: the second-order diagrams of the perturbative self-energy that are not skeletons have a double
   pole, which gives the derivative of the Fermi function *)
insertions2 = SimplifyDiagrams[OrderPart[selfEnergyPerturbative - skeletonSelfEnergy, InteractionPotential, 2]];
writeTerms["self_energy_insertions", insertions2, {x, y}, "Scale" -> 0.42, "LineWidth" -> 0.45];
insertions2Evaluated = EvaluateMatsubara[insertions2, twoPoint, gasRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1, p2}];
writeFormula["self_energy_insertions_matsubara", insertions2Evaluated, "Names" -> energyNames, "Subscripts" -> {v}];
(* the cutting rules where they apply: the density correlation at first order (Hartree and Fock insertions on the lines of
   the bubble give delta'), the perturbative self-energy at second and third order *)
cutCheck[e_, ext_, freq_] := Quiet[CheckCuttingRules[#, ext, gasRules, freq -> omega, "SpinDegeneracy" -> 2] & /@ DiagramTerms[e],
   CuttingRuleImaginaryPart::class];
cutDensity = timed["cutting rules density", cutCheck[OrderPart[densityCorrelation, InteractionPotential, 1], bubblePoints, nu]];
cutSigma = timed["cutting rules Sigma", Table[cutCheck[OrderPart[selfEnergyPerturbative, InteractionPotential, m], twoPoint, w], {m, 2, maximumOrder}]];
(* for each: the number of diagrams, of those checked, of those not of the implemented kind *)
writeValue["counts cutting rules density", {Length[cutDensity], Count[cutDensity, 0], Count[cutDensity, {$Failed}]}];
Do[writeValue["counts cutting rules Sigma " <> ToString[m], With[{c = cutSigma[[m - 1]]}, {Length[c], Count[c, 0], Count[c, {$Failed}]}]],
  {m, 2, maximumOrder}];
(* those of the highest order that are not of the implemented kind are exactly its skeleton diagrams *)
writeValue["check cutting rules skeletons", With[{terms = DiagramTerms[OrderPart[selfEnergyPerturbative, InteractionPotential, maximumOrder]],
     c = cutSigma[[maximumOrder - 1]]},
    SimplifyDiagrams[Total[Table[If[c[[i]] === {$Failed}, terms[[i]], 0], {i, Length[terms]}]] -
      OrderPart[skeletonSelfEnergy, InteractionPotential, maximumOrder]]]];
(* the diagrams of the third order with an imaginary part: sunsets with Hartree and Fock insertions on their lines *)
writeValue["counts imaginary part Sigma 3", Count[Quiet[CuttingRuleImaginaryPart[#, twoPoint, gasRules, w -> omega] & /@
     DiagramTerms[OrderPart[selfEnergyPerturbative, InteractionPotential, maximumOrder]], CuttingRuleImaginaryPart::class],
   e_ /; e =!= 0 && FreeQ[e, $Failed]]];
writeValue["check cutting rules", Count[Join[cutDensity, Join @@ cutSigma], c_ /; c =!= 0 && c =!= {$Failed}]];
writeValue["check delta prime", If[FreeQ[RetardedImaginaryPart[AnalyticContinuation[EvaluateMatsubara[OrderPart[densityCorrelation,
      InteractionPotential, 1], bubblePoints, gasRules], nu -> omega], omega], Derivative[1][EnergyDelta]], 1, 0]];
(* the brute-force check: the integrand of each diagram before the sums (DiagramIntegrand), summed over many Matsubara
   frequencies at T = 0.37, against the evaluated diagram, at given momenta (Tests/MatsubaraBruteForce.wl, shared with
   the tests: temperature, numeric, bruteForceErrors) *)
Get[FileNameJoin[{ParentDirectory[Directory[]], "Tests", "MatsubaraBruteForce.wl"}]];
gasValues = {xi -> Function[s, s^2/2 - 0.35], v -> Function[s, 1/(s^2 + 0.6)], Om -> Function[s, 0.5 + 0.2 s^2], g -> 0.7, k -> 0.45,
   q -> 0.8, LoopMomentum[1] -> 0.3, LoopMomentum[2] -> -0.55, w -> 3 Pi temperature, nu -> 4 Pi temperature};
(* the largest sums: 2 x 40000 frequencies for one frequency (n = 20000), 800 x 16000 for two (n = 100) *)
errorLaTeX[e_] := If[e == 0, "$0$", With[{ex = Floor[Log10[e]]}, "$" <> ToString[Round[e/10^ex]] <> "\\times10^{" <> ToString[ex] <> "}$"]];
bruteForceResults = timed["brute force", {Max[bruteForceErrors[OrderPart[densityCorrelation, InteractionPotential, 0], bubblePoints, gasRules,
      gasValues, 20000]], Max[bruteForceErrors[selfEnergy2, twoPoint, gasRules, gasValues, 100]],
    Max[bruteForceErrors[insertions2, twoPoint, gasRules, gasValues, 100]]}];
writeValue["errors brute force", errorLaTeX /@ bruteForceResults];
writeValue["check brute force", If[Max[bruteForceResults] < 10^-5, 0, Max[bruteForceResults]]];

(* ---------------- electrons and phonons: the Migdal self-energy ---------------- *)
(* at order g^2, for electrons of spin 1/2; the phonon line <phi phi> = 2 Om/(Om^2 - z^2) is minus the D0 of the textbooks *)
phononSelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[OrderPart[electronPhononGreen, g, 2], 1, {x, y}, {BareGreenFunction}],
   {x, y}, {BareGreenFunction}];
writeTerms["phonon_self_energy", phononSelfEnergy, {x, y}, "Scale" -> 0.5, "LineWidth" -> 0.45, "Couplings" -> {g}];
(* the 2PI functional of the full electron and phonon propagators, as Phi[G, W]: Sigma = dPhi/dG, Migdal's self-energy its
   first diagram; at bare lines it is the perturbative self-energy without the tadpole, the mean field of the phonon *)
DeclareSymmetric[DressedPhononPropagator]; DeclareLineStyle[DressedPhononPropagator, "Dashed"];
phiGD = timed["Phi[G,D]", LuttingerWardFunctional[FreeElectronPhononGeneratingFunctional, ElectronPhononVertex, 4,
    {BareGreenFunction -> DressedGreenFunction, PhononPropagator -> DressedPhononPropagator}]];
sigmaGD = SimplifyDiagrams[FunctionalDerivative[phiGD, DressedGreenFunction[y, x]]];
writeOrders["phi_gd", phiGD, {}, g, {2, 4}, "Couplings" -> {g}];
writeOrders["sigma_gd", sigmaGD, {x, y}, g, {2, 4}, "Couplings" -> {g}];
writeValue["check Migdal from Phi", SimplifyDiagrams[phononSelfEnergy - (OrderPart[sigmaGD, g, 2] /. {DressedGreenFunction -> BareGreenFunction,
      DressedPhononPropagator -> PhononPropagator}) + g^2 DeltaFunction[x, y] IntegralOver[PhononPropagator[x, u] BareGreenFunction[u, u], {u}]]];
phononRules = {BareGreenFunction -> {"Fermion", xi}, PhononPropagator -> {"Boson", Function[{q, z}, 2 Om[q]/(Om[q]^2 - z^2)]}};
migdal = timed["Migdal", EvaluateMatsubara[phononSelfEnergy, twoPoint, phononRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]];
phononNames = {Om -> "\\Omega", w -> "\\omega_n"};
writeFormula["migdal", migdal, "Names" -> phononNames, "Subscripts" -> {xi, Om}];
writeFormula["migdal_imaginary", RetardedImaginaryPart[AnalyticContinuation[migdal, w -> omega], omega], "Names" -> phononNames,
  "Subscripts" -> {xi, Om}];
writeValue["check cutting rules Migdal", CheckCuttingRules[phononSelfEnergy, twoPoint, phononRules, w -> omega, "SpinDegeneracy" -> 2]];
(* the brute-force check of the Migdal self-energy, as above (2 x 40000 frequencies) *)
bruteForceMigdal = timed["brute force Migdal", Max[bruteForceErrors[phononSelfEnergy, twoPoint, phononRules, gasValues, 20000]]];
writeValue["error brute force Migdal", errorLaTeX[bruteForceMigdal]];
writeValue["check brute force Migdal", If[bruteForceMigdal < 10^-5, 0, bruteForceMigdal]];

(* ---------------- disorder ---------------- *)
DeclareSource[Jimpurity]; DeclareField[impurityPotential, Jimpurity]; DeclareSymmetric[DisorderCorrelator]; DeclareLineStyle[DisorderCorrelator, "Impurity"];
FreeDisorderGeneratingFunctional = FreeFermionGeneratingFunctional Exp[1/2 IntegralOver[Jimpurity[x] DisorderCorrelator[x, y] Jimpurity[y], {x, y}]];
ImpurityVertex = IntegralOver[impurityPotential[z] psibar[z] ** psi[z], {z}];
disorderGreen = DropClosedLoops[-PerturbativeCorrelationFunction[FreeDisorderGeneratingFunctional, {psi[x], psibar[y]}, ImpurityVertex, 4], {BareGreenFunction}];
writeOrders["disorder_green", disorderGreen, {x, y}, DisorderCorrelator, {0, 1, 2}];
disorderSelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[disorderGreen, 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}];
writeOrders["disorder_self_energy", disorderSelfEnergy, {x, y}, DisorderCorrelator, {1, 2}];
(* the functional of the averaged propagator: 2PI in the fermion and in the impurity lines; the impurity line is not dressed
   (its self-energy is a closed loop), and Sigma = dPhi/dG without the diagrams with closed loops (none to Delta^2); its first
   diagram gives the self-consistent Born approximation, its second the crossed diagram. As in every skeleton expansion,
   the averaged propagator is drawn as a plain line *)
DeclareSymmetric[FullDisorderCorrelator];
phiDisorder = timed["Phi disorder", LuttingerWardFunctional[FreeDisorderGeneratingFunctional, ImpurityVertex, 4,
     {BareGreenFunction -> DressedGreenFunction, DisorderCorrelator -> FullDisorderCorrelator}] /. FullDisorderCorrelator -> DisorderCorrelator];
sigmaDisorder = DropClosedLoops[SimplifyDiagrams[FunctionalDerivative[phiDisorder, DressedGreenFunction[y, x]]], {DressedGreenFunction}];
writeOrders["disorder_phi", phiDisorder, {}, DisorderCorrelator, {1, 2}];
writeOrders["disorder_sigma_g", sigmaDisorder, {x, y}, DisorderCorrelator, {1, 2}];
(* the check by cutting: Sigma[G] at G = G0 + G0 Sigma_Born G0 (the rainbow) gives Sigma_dis to Delta^2 *)
disorderBornG[a_, b_] := BareGreenFunction[a, b] + IntegralOver[BareGreenFunction[a, u] DisorderCorrelator[u, v] BareGreenFunction[u, v] *
    BareGreenFunction[v, b], {u, v}];
writeValue["check SCBA", SimplifyDiagrams[OrderPart[disorderSelfEnergy, DisorderCorrelator, {1, 2}] -
    OrderPart[OrderPart[sigmaDisorder, DisorderCorrelator, 1] /. DressedGreenFunction[a_, b_] :> disorderBornG[a, b], DisorderCorrelator, {1, 2}] -
    (OrderPart[sigmaDisorder, DisorderCorrelator, 2] /. DressedGreenFunction -> BareGreenFunction)]];

(* ---------------- phi^4 ---------------- *)
DeclareField[phi, J];
(* in the pictures each vertex stands for -lambda *)
DeclareCoupling[lambda];
Phi4Vertex = lambda/24 IntegralOver[phi[z]^4, {z}];
applyInteraction[F_] := SimplifyDiagrams[-lambda/24 IntegralOver[FunctionalDerivative[F, J[z], J[z], J[z], J[z]], {z}]];
firstOrder = applyInteraction[FreeBosonGeneratingFunctional];
secondOrder = applyInteraction[firstOrder];
(* the third order is used only at zero source and after at most four derivatives (the numerator of the propagator and the
   four-point function, below): only the terms of the second order with at most eight sources contribute (four
   derivatives, then at most four more), after each of the four derivatives J[z] only those with at most 7, 6, 5, 4
   sources. The results are those of the literal application shown in the showcase (vacuum bubbles), in a fraction of the
   time *)
thirdOrderFour = timed["phi4 third order", Module[{f = fewSources[secondOrder, 8, _J]},
    Do[f = fewSources[FunctionalDerivative[f, J[z]], kk, _J], {kk, {7, 6, 5, 4}}];
    SimplifyDiagrams[-lambda/24 IntegralOver[f, {z}]]]];
thirdOrder = fewSources[thirdOrderFour, 2, _J];
(* Z[J] at a non-zero source, to second order: Z[J] = Z0[J] (1 + z1[J] + z2[J] + ...), every diagram with its source legs
   (the factor Z0[J] = exp(1/2 J.D0.J) set aside); and W[J] = log Z[J] = 1/2 J.D0.J + w1 + w2 + ..., the connected ones *)
zSource1 = stripGaussian[firstOrder]; zSource2 = stripGaussian[secondOrder/2];
writeTerms["phi4_z_source_1", zSource1, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
writeTerms["phi4_z_source_2", zSource2, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
wSource2 = SimplifyDiagrams[zSource2 - zSource1^2/2];
writeTerms["phi4_w_source_2", wSource2, {}, "Scale" -> 0.45, "LineWidth" -> 0.45];
writeValue["counts phi4 Z and W at a source", Length[DiagramTerms[#]] & /@ {zSource1, zSource2, wSource2}];
writeValue["check phi4 W at a source connected", SimplifyDiagrams[wSource2 - ConnectedDiagrams[wSource2, {}]]];
writeOrders["phi4_vacuum", SetSourcesToZero[FreeBosonGeneratingFunctional + firstOrder + secondOrder/2 + thirdOrder/6], {}, lambda, {0, 1, 2, 3}];
writeValue["counts phi4 vacuum", orderCounts[SetSourcesToZero[firstOrder + secondOrder/2 + thirdOrder/6], lambda, {1, 2, 3}]];
(* the linked-cluster theorem. z_n: the vacuum diagrams of order n in Z/Z0, from the derivatives above. log(1 + z1 + z2 + z3) =
   z1 + z2 + z3 - z1^2/2 - z1 z2 + z1^3/3 to third order is the sum of the connected diagrams among them (ConnectedDiagrams
   only picks them), and the exponential of these gives Z/Z0 back; ConnectedVacuumDiagrams gives them directly *)
{phi4Z1, phi4Z2, phi4Z3} = {SetSourcesToZero[firstOrder], SetSourcesToZero[secondOrder]/2, SetSourcesToZero[thirdOrder]/6};
phi4LogVacuum = ConnectedDiagrams[phi4Z1 + phi4Z2 + phi4Z3, {}];
writeOrders["phi4_log_vacuum", phi4LogVacuum, {}, lambda, {1, 2, 3}];
writeValue["counts phi4 log vacuum", orderCounts[phi4LogVacuum, lambda, {1, 2, 3}]];
writeValue["check phi4 linked cluster", SimplifyDiagrams[phi4Z1 + phi4Z2 + phi4Z3 - phi4Z1^2/2 - phi4Z1 phi4Z2 + phi4Z1^3/3 - phi4LogVacuum]];
With[{c1 = orderPart[phi4LogVacuum, lambda, 1], c2 = orderPart[phi4LogVacuum, lambda, 2], c3 = orderPart[phi4LogVacuum, lambda, 3]},
  writeValue["check phi4 exponential of the connected diagrams", SimplifyDiagrams[c1 + c2 + c3 + c1^2/2 + c1 c2 + c1^3/6 -
      (phi4Z1 + phi4Z2 + phi4Z3)]];
  (* the factors of the products in Z/Z0, drawn alone: the figure eight, and the connected diagrams of second and third order *)
  writePicture["phi4_eight", c1, {}];
  writeTerms["phi4_connected_second", c2, {}];
  writeTerms["phi4_connected_third", c3, {}]];
writeValue["check phi4 connected vacuum shortcut", SimplifyDiagrams[timed["phi4 log vacuum",
     ConnectedVacuumDiagrams[FreeBosonGeneratingFunctional, Phi4Vertex, 3]] - phi4LogVacuum]];
(* the numerator <phi phi exp(-S)>_0, with its vacuum bubbles: the diagrams without bubbles times Z/Z0 *)
phi4Numerator = SetSourcesToZero[FunctionalDerivative[FreeBosonGeneratingFunctional +
     fewSources[firstOrder + secondOrder/2 + thirdOrder/6, 2, _J], J[a], J[b]]];
writeOrders["phi4_numerator", phi4Numerator, {a, b}, lambda, {0, 1, 2, 3}];
writeValue["check phi4 numerator factorizes", byOrder[SimplifyDiagrams[phi4Numerator -
       DropVacuumBubbles[phi4Numerator, {a, b}] (1 + phi4Z1 + phi4Z2 + phi4Z3)], lambda, Range[0, 3]]];
phi4Propagator = PerturbativeCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b]}, Phi4Vertex, 3];
writeOrders["phi4_propagator", phi4Propagator, {a, b}, lambda, {0, 1, 2, 3}];
writeValue["check phi4 propagator shortcut", SimplifyDiagrams[phi4Propagator - DropVacuumBubbles[phi4Numerator, {a, b}]]];
(* moments and cumulants: the four-point function, all diagrams (drawn to first order) and the connected ones *)
phi4FourPoint = PerturbativeCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3];
writeOrders["phi4_four_point", phi4FourPoint, {a, b, c, d}, lambda, {0, 1}];
writeValue["counts phi4 four-point", orderCounts[phi4FourPoint, lambda, {0, 1, 2, 3}]];
phi4FourPointConnected = ConnectedCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3];
writeOrders["phi4_four_point_connected", phi4FourPointConnected, {a, b, c, d}, lambda, {1, 2, 3}];
(* the moments from the cumulants: the derivatives of exp(W) give every partition of the points into blocks, each block a
   cumulant; in phi^4 theory at zero source the odd ones vanish. W2 (the full propagator, a double line) and W4 (a blob) *)
DeclareSymmetric[ConnectedTwoPoint, ConnectedFourPoint]; DeclareLineStyle[ConnectedTwoPoint, "Double"];
DeclareBlob[ConnectedFourPoint, "\\scriptstyle W^{(4)}", "Fill" -> "White"];
writeTerms["phi4_partitions", FieldExpectationValue[Exp[1/2 IntegralOver[ConnectedTwoPoint[x, y] J[x] J[y], {x, y}] +
     1/24 IntegralOver[ConnectedFourPoint[x, y, u, v] J[x] J[y] J[u] J[v], {x, y, u, v}]], {phi[a], phi[b], phi[c], phi[d]}], {a, b, c, d},
  "Scale" -> 0.55, "LineWidth" -> 0.45, "LabelFont" -> "\\footnotesize"];
(* the same by derivatives in J, to third order: W[J] = 1/2 J.D0.J + log(1 + s1 + s2 + s3), with Z[J] = Z0[J] (1 + s1 + s2 +
   s3) at a source (only the terms with at most four sources matter); the fourth derivative of W at J = 0 is the connected
   part of the fourth moment (without its vacuum bubbles), and the moment is W4 plus the three pairings of W2 *)
timed["phi4 moments and cumulants", Module[{four = fewSources[#, 4, _J] &, s1, s2, s3, wJ, moment, w4, w2},
  {s1, s2, s3} = {stripGaussian[firstOrder], stripGaussian[secondOrder/2], stripGaussian[thirdOrderFour/6]};
  wJ = SimplifyDiagrams[OrderPart[SimplifyDiagrams[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}] + four[s1 + s2 + s3] -
       four[four[s1] four[s1 + 2 s2]]/2 + four[four[s1]^2 four[s1]]/3], lambda, {0, 1, 2, 3}]];
  moment = DropVacuumBubbles[SetSourcesToZero[FunctionalDerivative[FreeBosonGeneratingFunctional +
       four[firstOrder + secondOrder/2 + thirdOrderFour/6], J[a], J[b], J[c], J[d]]], {a, b, c, d}];
  w4 = SetSourcesToZero[FunctionalDerivative[wJ, J[a], J[b], J[c], J[d]]];
  w2[p_, q_] := SetSourcesToZero[FunctionalDerivative[wJ, J[p], J[q]]];
  writeValue["check phi4 cumulant connected", SimplifyDiagrams[w4 - ConnectedDiagrams[moment, {a, b, c, d}]]];
  writeValue["check phi4 moments from cumulants", SimplifyDiagrams[moment - OrderPart[SimplifyDiagrams[w4 + w2[a, b] w2[c, d] +
         w2[a, c] w2[b, d] + w2[a, d] w2[b, c]], lambda, {0, 1, 2, 3}]]];
  writeValue["check phi4 cumulant shortcut", SimplifyDiagrams[w4 - phi4FourPointConnected]];
  writeValue["check phi4 moment shortcut", SimplifyDiagrams[moment - phi4FourPoint]]]];
writeValue["counts phi4 four-point connected", orderCounts[phi4FourPointConnected, lambda, {1, 2, 3}]];
(* the self-energy to third order, and the Dyson equation *)
phi4SelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[phi4Propagator, 1, {a, b}, {FreePropagator}], {a, b}, {FreePropagator}];
writeOrders["phi4_self_energy", phi4SelfEnergy, {a, b}, lambda, {1, 2, 3}];
writeValue["check phi4 Dyson", SimplifyDiagrams[DysonSeries[phi4SelfEnergy /. {a -> s1, b -> s2}, {s1, s2}, FreePropagator, {a, b}, 3,
     "MaximumOrder" -> {lambda, 3}] - phi4Propagator]];
(* the 1PI effective action Gamma[phibar] (the 1PI section), with the lines D = (D0^-1 + lambda phibar^2/2)^-1 in the background, and
   its vertex functions at phibar = 0 (where D = D0): Gamma^(2) = D0^-1 - Sigma and Gamma^(4) = -(amputated 1PI four-point
   function), to lambda^3; sigma1PI and gamma4 are the self-energy and the vertex of the sections that follow (the four-point
   vertex, dimensional regularization), the same as phi4SelfEnergy and the amputated 1PI part of the connected function *)
DeclareDisplayName[BackgroundPropagator, "D"]; DeclareLineStyle[BackgroundPropagator, "Double"];
gamma1PI = timed["EA Gamma[phibar]", EffectiveAction1PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3, phi -> phiMean, FreePropagator -> BackgroundPropagator]];
eaAtZero[e_] := ExpandTrace[SetSourcesToZero[e]] /. BackgroundPropagator -> FreePropagator;
eaGamma1Two = eaAtZero[FunctionalDerivative[gamma1PI, phiMean[a], phiMean[b]]];
sigma1PI = OrderPart[SimplifyDiagrams[OperatorKernel[Power[FreePropagator, -1]][a, b] - eaGamma1Two], lambda, {1, 2, 3}];
(* the four derivatives one at a time, the orders above lambda^3 dropped after each one: a derivative of D brings a coupling,
   so the orders never decrease (the result is that of the four derivatives at once, in a fraction of the time) *)
(* the lines D[phibar] hold incomplete higher orders: the derivatives are taken up to lambda^3 *)
gamma4 = timed["EA Gamma^(4)", eaAtZero[OrderPart[FunctionalDerivative[gamma1PI, phiMean[a], phiMean[b], phiMean[c], phiMean[d],
     "MaximumOrder" -> {lambda, 3}], lambda, {1, 2, 3}]]];
eaGamma1Four = SimplifyDiagrams[gamma4 /. Thread[{a, b, c, d} -> fourLabels]];
(* the four-point vertex to two loops (third order): the skeleton diagrams of gamma4 with D0 -> G (PerturbativeFullVertex is the
   shortcut), and the parquet equations for real bosons, channels s, t, u ("Statistics" -> "Boson") *)
DeclareSymmetric[DressedPropagator];
vertexPhi4 = timed["phi4 vertex", SimplifyDiagrams[SkeletonDiagrams[gamma4, {a, b, c, d}, {FreePropagator}] /. FreePropagator -> DressedPropagator]];
writeValue["check phi4 vertex shortcut", SimplifyDiagrams[PerturbativeFullVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 3, {phi}, {a, b, c, d},
    FreePropagator -> DressedPropagator] - vertexPhi4]];
writeOrders["phi4_vertex", vertexPhi4, {a, b, c, d}, lambda, {1, 2, 3}];
(* the same with the legs of one side exchanged where they cross (the diagrams of the channel t, which pairs opposite
   corners, with c and d exchanged) *)
writeOrders["phi4_vertex_uncrossed", vertexPhi4, {a, b, c, d}, lambda, {1, 2, 3}, "ExternalOrder" -> "Uncrossed"];
writeValue["counts phi4 vertex", orderCounts[vertexPhi4, lambda, {1, 2, 3}]];
phi4Parquet = CheckParquetEquations[vertexPhi4, {a, b, c, d}, DressedPropagator, lambda, 3, "Statistics" -> "Boson"];
writeValue["check parquet phi4", phi4Parquet["Residuals"]];
phi4Channels = ParquetDecomposition[vertexPhi4, {a, b, c, d}, {DressedPropagator}, "Statistics" -> "Boson"];
writeValue["counts phi4 R_s", orderCounts[phi4Channels["s"], lambda, {2, 3}]];
writeValue["counts phi4 fully irreducible", orderCounts[phi4Channels["FullyIrreducible"], lambda, {1, 2, 3}]];
writeOrders["phi4_vertex_s", phi4Channels["s"], {a, b, c, d}, lambda, {2, 3}];
(* the channel t pairs opposite corners: drawn with its legs c and d exchanged *)
writeOrders["phi4_vertex_t", phi4Channels["t"], {a, b, c, d}, lambda, {2, 3}, "ExternalOrder" -> "Uncrossed"];
(* the fourth order: its only fully irreducible diagram is the envelope, the first correction to the parquet
   approximation *)
vertexPhi4Fourth = timed["phi4 vertex fourth order", OrderPart[PerturbativeFullVertex[FreeBosonGeneratingFunctional,
    Phi4Vertex, 4, {phi}, {a, b, c, d}, FreePropagator -> DressedPropagator], lambda, 4]];
phi4Envelope = ParquetDecomposition[vertexPhi4Fourth, {a, b, c, d}, {DressedPropagator}, "Statistics" -> "Boson"]["FullyIrreducible"];
writeValue["counts phi4 vertex 4", Length[DiagramTerms[vertexPhi4Fourth]]];
writeValue["counts phi4 fully irreducible 4", Length[DiagramTerms[phi4Envelope]]];
writeTerms["phi4_envelope", phi4Envelope, {a, b, c, d}];
(* crossing: Gamma4 is symmetric, and the channels are exchanged by permutations of the legs *)
writeValue["check phi4 crossing", {SimplifyDiagrams[vertexPhi4 - (vertexPhi4 /. {a -> b, b -> a})],
   SimplifyDiagrams[vertexPhi4 - (vertexPhi4 /. {b -> c, c -> b})], SimplifyDiagrams[phi4Channels["s"] - (phi4Channels["t"] /. {b -> c, c -> b})]}];
(* the bubble chain of the s channel: the bare vertex (a point) iterated *)
pointVertex = lambda DeltaFunction[a, b] DeltaFunction[a, c] DeltaFunction[a, d];
phi4OneBubble = BetheSalpeterLadder[pointVertex, pointVertex, {a, b, c, d}, "s", DressedPropagator, "Statistics" -> "Boson"];
writeTerms["phi4_bubble_chain", pointVertex + phi4OneBubble + BetheSalpeterLadder[pointVertex, phi4OneBubble, {a, b, c, d}, "s",
    DressedPropagator, "Statistics" -> "Boson"], {a, b, c, d}];
(* the 2PI functional Gamma2 (Phi of real bosons) and Sigma[G] = -2 dGamma2/dG: the skeleton part of the self-energy *)
phi4Gamma2 = timed["phi4 Gamma2", LuttingerWardFunctional[FreeBosonGeneratingFunctional, Phi4Vertex, 3, FreePropagator -> DressedPropagator]];
writeOrders["phi4_gamma2", phi4Gamma2, {}, lambda, {1, 2, 3}];
phi4SelfEnergyG = timed["phi4 Sigma[G]", SimplifyDiagrams[-2 FunctionalDerivative[phi4Gamma2, DressedPropagator[a, b]]]];
writeOrders["phi4_self_energy_g", phi4SelfEnergyG, {a, b}, lambda, {1, 2, 3}];
writeValue["check phi4 Sigma = skeleton part", SimplifyDiagrams[OrderPart[phi4SelfEnergyG, lambda, {1, 2, 3}] -
    (SkeletonDiagrams[phi4SelfEnergy, {a, b}, {FreePropagator}] /. FreePropagator -> DressedPropagator)]];
(* Gamma0 = -2 dSigma/dG of real bosons, irreducible in the channel s (a b|c d): Gamma4 = Gamma0 + R_s, and the
   Bethe-Salpeter equation of the channel s *)
phi4Gamma0 = timed["phi4 Gamma0", SimplifyDiagrams[4 FunctionalDerivative[phi4Gamma2, DressedPropagator[a, b], DressedPropagator[c, d]]]];
writeOrders["phi4_gamma0", phi4Gamma0, {a, b, c, d}, lambda, {1, 2, 3}];
writeValue["counts phi4 Gamma0", orderCounts[phi4Gamma0, lambda, {1, 2, 3}]];
writeValue["check phi4 Gamma0", {SimplifyDiagrams[phi4Gamma0 - (phi4Gamma0 /. {a -> b, b -> a})],
   SimplifyDiagrams[phi4Gamma0 - (phi4Gamma0 /. {a -> c, b -> d, c -> a, d -> b})],
   Table[SimplifyDiagrams[IntegralOver[DressedPropagator[c, d] OrderPart[phi4Gamma0, lambda, m], {c, d}] +
      2 (2 m - 1) OrderPart[phi4SelfEnergyG, lambda, m]], {m, 1, 3}],
   SimplifyDiagrams[vertexPhi4 - phi4Channels["s"] - phi4Gamma0],
   SimplifyDiagrams[vertexPhi4 - phi4Gamma0 - BetheSalpeterLadder[phi4Gamma0, vertexPhi4, {a, b, c, d}, "s", DressedPropagator,
      "Statistics" -> "Boson", "MaximumOrder" -> {lambda, 3}]]}];
(* the Schwinger-Dyson equation of phi^4 theory: Sigma = the tadpole + lambda/6 G G G Gamma4, drawn with blobs *)
writeValue["check phi4 Schwinger-Dyson", byOrder[phi4SelfEnergyG - lambda/6 IntegralOver[DressedPropagator[a, u] *
        DressedPropagator[a, v] DressedPropagator[a, w] (vertexPhi4 /. {a -> u, b -> v, c -> w, d -> b}), {u, v, w}], lambda, Range[2, 3]]];
(* Gamma4 the box of the Bethe-Salpeter equation below: joined by three corners to the vertex at a, it is drawn turned by
   45 degrees, b at its right corner, a on the diagonal through it, the lines to the top and bottom corners arcs *)
DeclareBlob[Gamma4Square, "\\Gamma^{(4)}", "Shape" -> "Box", "Fill" -> "CrossHatched"];
writeTerms["exact_schwinger_dyson_phi4", OrderPart[phi4SelfEnergyG, lambda, 1] +
   lambda/6 IntegralOver[DressedPropagator[a, u] DressedPropagator[a, v] DressedPropagator[a, w] Gamma4Square[v, u, b, w], {u, v, w}], {a, b},
  blobOptions];
writeTerms["exact_sigma_lhs_phi4", SigmaBlob[a, b], {a, b}, blobOptions];
(* the Bethe-Salpeter equation of the channel s, Gamma4 = Gamma0 - 1/2 Gamma0 L0 Gamma4, with Gamma4 a square, the lines
   of the pair drawn parallel (the vertices are symmetric) *)
writeTerms["exact_bethe_salpeter_phi4_lhs", Gamma4Square[a, b, c, d], {a, b, c, d}, blobOptions];
writeTerms["exact_bethe_salpeter_phi4", KernelBox[a, b, c, d] - 1/2 IntegralOver[KernelBox[a, b, u1, u2] DressedPropagator[u2, u3] DressedPropagator[u1, u4] *
     Gamma4Square[u3, u4, c, d], {u1, u2, u3, u4}], {a, b, c, d}, blobOptions];
writeValue["check phi4 Bethe-Salpeter", CheckEquation[ExactEquation["BetheSalpeter", {"F" -> LabelFunction[vertexPhi4, {a, b, c, d}],
     "Gamma0" -> LabelFunction[phi4Gamma0, {a, b, c, d}], "G" -> DressedPropagator}, {a, b, c, d}, "Statistics" -> "Boson",
    "MaximumOrder" -> {lambda, 3}], lambda, 3]];
(* the vertex of phi^2/2, Lambda = gamma - 1/2 Gamma0 L0 Lambda = gamma - 1/2 Gamma4 L0 gamma *)
writeTerms["phi4_vertex_lhs", LambdaTriangle[a, b, z], {a, b, z}, blobOptions];
writeTerms["phi4_vertex_equation", DeltaFunction[a, z] DeltaFunction[b, z] - 1/2 IntegralOver[KernelBox[a, b, u1, u2] DressedPropagator[u2, u3] *
     DressedPropagator[u1, u4] LambdaTriangle[u3, u4, z], {u1, u2, u3, u4}], {a, b, z}, blobOptions];
writeTerms["phi4_vertex_from_f", DeltaFunction[a, z] DeltaFunction[b, z] - 1/2 IntegralOver[Gamma4Square[a, b, u1, u2] DressedPropagator[u2, z] *
     DressedPropagator[u1, z], {u1, u2}], {a, b, z}, blobOptions];
(* the vertex of phi^2/2 and its equations, checked against the series *)
(* a local source U(z) coupled to phi^2/2 (linear response): D0^-1 -> D0^-1 + U, dD0(p, q)/dU(z) = -D0(p, z) D0(z, q); the vertex
   Lambda = dG^-1/dU = gamma - dSigma/dU, its skeleton diagrams with D0 -> G; PerturbativeDensityVertex is the shortcut *)
DeclareSource[MassSource];
DeclareFunctionalDerivative[FreePropagator[p_, q_], MassSource[r_], -FreePropagator[p, r] FreePropagator[r, q]];
phi4DensityVertex = SimplifyDiagrams[DeltaFunction[a, z] DeltaFunction[b, z] - (SkeletonDiagrams[FunctionalDerivative[phi4SelfEnergy, MassSource[z]],
       {a, b, z}, {FreePropagator}] /. FreePropagator -> DressedPropagator)];
writeValue["check phi4 density vertex shortcut", SimplifyDiagrams[PerturbativeDensityVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 3, {phi},
     {a, b, z}, FreePropagator -> DressedPropagator] - phi4DensityVertex]];
writeValue["counts phi4 Lambda", orderCounts[phi4DensityVertex, lambda, {0, 1, 2, 3}]];
writeOrders["phi4_density_vertex", phi4DensityVertex, {a, b, z}, lambda, {0, 1, 2}];
With[{objects = {"G" -> DressedPropagator, "Lambda" -> LabelFunction[phi4DensityVertex, {a, b, z}],
     "Gamma0" -> LabelFunction[phi4Gamma0, {a, b, c, d}], "F" -> LabelFunction[vertexPhi4, {a, b, c, d}]}},
  writeValue["check phi4 vertex equations", {
     CheckEquation[ExactEquation["Vertex", objects, {a, b, z}, "Statistics" -> "Boson", "MaximumOrder" -> {lambda, 3}], lambda, 3],
     CheckEquation[ExactEquation["VertexFromF", objects, {a, b, z}, "Statistics" -> "Boson", "MaximumOrder" -> {lambda, 3}], lambda, 3]}]];
(* Hubbard-Stratonovich for phi^4: a field sigma with <sigma sigma>_0 = lambda/3 delta (the head SigmaPropagator), coupled
   by -i/2 Integral sigma phi^2; the 2PI functional of G and of the full D = <sigma sigma> *)
DeclareSource[Jsigma]; DeclareField[sigmaField, Jsigma]; DeclareSymmetric[SigmaPropagator, FullSigmaPropagator];
DeclareInteractionLine[SigmaPropagator]; DeclareInteractionLine[FullSigmaPropagator, "DoubleWavy"];
FreeSigmaGeneratingFunctional = FreeBosonGeneratingFunctional Exp[1/2 IntegralOver[Jsigma[x] SigmaPropagator[x, y] Jsigma[y], {x, y}]];
SigmaVertex = -I/2 IntegralOver[sigmaField[w] phi[w]^2, {w}];
localSigma[e_] := SimplifyDiagrams[e /. SigmaPropagator[p_, q_] :> lambda/3 DeltaFunction[p, q]];
writeValue["check phi4 HS G", byOrder[localSigma[PerturbativeCorrelationFunction[FreeSigmaGeneratingFunctional,
        {phi[a], phi[b]}, SigmaVertex, 6]] - phi4Propagator, lambda, Range[0, 3]]];
writeValue["check phi4 sigma integrated out", SimplifyDiagrams[localSigma[IntegrateOutField[{FreeSigmaGeneratingFunctional, SigmaVertex},
      sigmaField, Jsigma]["Interaction"]] - Phi4Vertex]];
gamma2GD = timed["phi4 Gamma2[G,D]", LuttingerWardFunctional[FreeSigmaGeneratingFunctional, SigmaVertex, 6,
     {FreePropagator -> DressedPropagator, SigmaPropagator -> FullSigmaPropagator}]];
writeValue["counts phi4 Gamma2[G,D]", orderCounts[gamma2GD, FullSigmaPropagator, {1, 2, 3}]];
(* only the full D_sigma appears: the plain wavy line, as for W in Phi[G, W] *)
writeOrders["phi4_gamma2_gd", gamma2GD, {}, FullSigmaPropagator, {1, 2, 3}, "Interactions" -> {FullSigmaPropagator}];
sigmaGD = SimplifyDiagrams[-2 FunctionalDerivative[gamma2GD, DressedPropagator[a, b]]];
writeOrders["phi4_sigma_gd", sigmaGD, {a, b}, FullSigmaPropagator, {1, 2}, "Interactions" -> {FullSigmaPropagator}];
(* D[G] = D0 - D0 <phi^2/2 phi^2/2>_c D0 with dressed lines; the mean field of sigma gives a third of the tadpole *)
phi2CorrelationSkeleton = SimplifyDiagrams[SkeletonDiagrams[1/4 ConnectedCorrelationFunction[FreeBosonGeneratingFunctional,
      {phi[a], phi[a], phi[b], phi[b]}, Phi4Vertex, 2], {a, b}, {FreePropagator}] /. FreePropagator -> DressedPropagator];
writeValue["check phi4 Sigma[G,D[G]]", byOrder[substituteToOrder[sigmaGD, FullSigmaPropagator,
        SimplifyDiagrams[lambda/3 DeltaFunction[a, b] - lambda^2/9 phi2CorrelationSkeleton], lambda, 3] +
       OrderPart[phi4SelfEnergyG, lambda, 1]/3 - phi4SelfEnergyG, lambda, Range[0, 3]]];
(* the correlation function of the composite operator phi^2 *)
(* <phi^2(a) phi^2(b)>_c = -2 dG(a, a)/dU(b), with the source U of phi^2/2 declared above; ConnectedCorrelationFunction is the shortcut *)
phi2Correlation = SimplifyDiagrams[-2 FunctionalDerivative[OrderPart[phi4Propagator, lambda, {0, 1, 2}] /. b -> a, MassSource[b]]];
writeValue["check phi2 correlation shortcut", SimplifyDiagrams[ConnectedCorrelationFunction[FreeBosonGeneratingFunctional,
     {phi[a], phi[a], phi[b], phi[b]}, Phi4Vertex, 2] - phi2Correlation]];
writeOrders["phi4_phi2_correlation", phi2Correlation, {a, b}, lambda, {0, 1, 2}];
writeValue["counts phi2 correlation", orderCounts[phi2Correlation, lambda, {0, 1, 2}]];

(* ---------------- phi^4 theory in dimensional regularization ---------------- *)
(* the diagrams of the phi^4 section in d = 4 - 2 eps (Euclidean, MS-bar measure), with the code of the showcase: the
   expansions and the Z factors are written with the symbol kappa for 1/(16 pi^2), the factor of each loop (toKappa,
   put in before the expansion, and Collect); the textbook values (Peskin-Schroeder, Zinn-Justin,
   Kleinert-Schulte-Frohlinde) are checked *)
dimregStart = AbsoluteTime[];
dimregNames = {eps -> "\\varepsilon"};
kappaValue = {kappa -> 1/(16 Pi^2)};
massive = {FreePropagator -> {"Scalar", m}}; massless = {FreePropagator -> {"Scalar", 0}};
(* one loop: the tadpole of Sigma (massive) and the bubble of Gamma4 (massless, the momentum p through it) *)
tadpoleSigma = IntegrateLoops[DiagramLoopIntegral[OrderPart[sigma1PI, lambda, 1], {a -> p, b -> -p}, massive], eps];
writeFormula["dimreg_tadpole", tadpoleSigma, "Names" -> dimregNames];
bubbleLoop = IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0] ScalarPropagator[k + p, 0], {k}], eps];
writeFormula["dimreg_bubble", bubbleLoop, "Names" -> dimregNames];
toKappa = Pi -> 1/(4 Sqrt[kappa]);
oneLoopExpanded = Collect[{EpsilonExpand[tadpoleSigma /. toKappa, eps, 0], EpsilonExpand[bubbleLoop /. toKappa, eps, 1]},
   {kappa, lambda, m, eps}];
writeFormula["dimreg_tadpole_expanded", oneLoopExpanded[[1]], "Names" -> dimregNames];
writeFormula["dimreg_bubble_expanded", oneLoopExpanded[[2]], "Names" -> dimregNames];
writeValue["check dimreg tadpole", Simplify[(oneLoopExpanded[[1]] /. kappaValue) -
    lambda m^2/(32 Pi^2) (1/eps + 1 - Log[m^2/RenormalizationScale^2])]];
writeValue["check dimreg bubble", Simplify[(oneLoopExpanded[[2]] /. kappaValue) -
    (1/eps + 2 - Log[p^2/RenormalizationScale^2] + eps (4 - Pi^2/12 - 2 Log[p^2/RenormalizationScale^2] +
       Log[p^2/RenormalizationScale^2]^2/2))/(16 Pi^2)]];
(* two loops: the massless sunset, nested G-functions *)
sunsetSigma = IntegrateLoops[DiagramLoopIntegral[OrderPart[sigma1PI, lambda, 2], {a -> p, b -> -p}, massless], eps];
writeFormula["dimreg_sunset", sunsetSigma, "Names" -> dimregNames];
sunsetExpanded = Collect[EpsilonExpand[sunsetSigma /. toKappa, eps, 0], {kappa, lambda, p, eps}];
writeFormula["dimreg_sunset_expanded", sunsetExpanded, "Names" -> dimregNames];
writeValue["check dimreg sunset", Simplify[(sunsetExpanded /. kappaValue) -
    lambda^2/6 p^2/(16 Pi^2)^2 (-1/(4 eps) - 13/8 + Log[p^2/RenormalizationScale^2]/2)]];
(* Gamma4 = d^4 Gamma/dphibar^4 at phibar = 0 (gamma4, computed in the phi^4 section), to lambda^3, with the infrared rearrangement *)
writeOrders["dimreg_gamma4", gamma4, {a, b, c, d}, lambda, {3}];
writeValue["counts dimreg Gamma4", orderCounts[gamma4, lambda, {1, 2, 3}]];
routed4 = InfraredRearrangedIntegral[OrderPart[gamma4, lambda, 3], {a, b, c, d}, p, massless, eps, "Routing" -> True];
(* the pole parts by topology, in units of lambda^3 kappa^2: the eyes, the chains, the tadpoles on a line *)
poles4 = Tally[Simplify[(16 Pi^2)^2 PolePart[#[[3]], eps]/lambda^3] & /@ routed4];
writeValue["counts dimreg Gamma4 topologies", poles4[[All, 2]]];
writeFormula["dimreg_eye", poles4[[1, 1]], "Names" -> dimregNames];
writeFormula["dimreg_chain", poles4[[2, 1]], "Names" -> dimregNames];
writeValue["check dimreg Gamma4 poles", Simplify[poles4[[All, 1]] - {(1 + eps (5 - 2 Log[p^2/RenormalizationScale^2]))/(4 eps^2),
     (1 - 2 eps (Log[p^2/RenormalizationScale^2] - 2))/(4 eps^2), 0}]];
(* the bare functions; dSigma/dm^2 = -MassInsertion, with the momentum entering at the insertion u *)
sigma21 = -MassInsertion[OrderPart[sigma1PI, lambda, {1, 2}], FreePropagator, u];
writeTerms["dimreg_mass_insertion", sigma21, {a, b, u}];
sigmaB = InfraredRearrangedIntegral[OrderPart[sigma1PI, lambda, {1, 2}], {a, b}, p, massless, eps];
gamma4B = InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps];
sigma21B = InfraredRearrangedIntegral[sigma21, {a, b, u}, p, massless, eps];
(* Z_phi (p^2 - Sigma), Z_phi^2 Gamma4 and Z_m2 (1 - dSigma/dm^2) finite, at lambda_B = mu^(2 eps) (e^gammaE/4 pi)^eps Z_lambda lambda;
   kappa stays a symbol in the Z's and has its value in the functions that must be finite *)
z[c_] := 1 + c[1] lambda kappa/eps + lambda^2 kappa^2 (c[2]/eps^2 + c[3]/eps);
{zPhi, zLambda, zM2} = {z[zf], z[zl], z[zm]};
upTo[e_, n_] := Sum[Coefficient[e, lambda, j] lambda^j, {j, 0, n}];
finite = {upTo[zPhi (p^2 - (sigmaB /. lambda -> lambda zLambda)), 2], upTo[zPhi^2 (gamma4B /. lambda -> lambda zLambda), 3],
    upTo[zM2 (1 - (sigma21B /. lambda -> lambda zLambda)), 2]} /. kappaValue;
equations = Flatten[CoefficientList[Expand[eps^4 PolePart[#, eps]] /. Log[p^2/RenormalizationScale^2] -> L, {lambda, eps, L, p}] & /@ finite];
solution = First[Solve[Thread[DeleteCases[equations, 0] == 0], Flatten[Table[c[i], {c, {zf, zl, zm}}, {i, 3}]]]];
{zPhi, zLambda, zM2} = {zPhi, zLambda, zM2} /. solution;
writeValue["check dimreg residual poles", Simplify[PolePart[# /. solution, eps]] & /@ finite];
writeFormula["dimreg_z_phi", zPhi, "Names" -> dimregNames];
writeFormula["dimreg_z_lambda", zLambda, "Names" -> dimregNames];
writeFormula["dimreg_z_m2", zM2, "Names" -> dimregNames];
writeValue["check dimreg Z factors", Simplify[{zPhi - (1 - lambda^2 kappa^2/(24 eps)),
     zLambda - (1 + 3 lambda kappa/(2 eps) + lambda^2 kappa^2 (9/(4 eps^2) - 17/(12 eps))),
     zM2 - (1 + lambda kappa/(2 eps) + lambda^2 kappa^2 (1/(2 eps^2) - 1/(4 eps)))}]];
writeValue["check dimreg double poles", {zl[2] - zl[1]^2, zm[2] - zm[1] (zm[1] + zl[1])/2} /. solution];
(* the one-loop mass counterterm against the massive tadpole: Z_m2 m^2 - Sigma finite *)
writeValue["check dimreg massive mass counterterm", Simplify[PolePart[(Expand[(zM2 - 1) m^2] /. lambda^2 -> 0 /. kappaValue) - tadpoleSigma, eps]]];
(* the renormalization-group functions, in kappa as the Z's *)
beta = BetaFunctionFromZ[zLambda, lambda, eps, 3];
betaEps = -2 eps lambda + beta;
gammaPhi = 1/2 AnomalousDimensionFromZ[zPhi, lambda, eps, betaEps, 2];
gammaM2 = -AnomalousDimensionFromZ[zM2/zPhi, lambda, eps, betaEps, 2];
writeFormula["dimreg_beta", beta];
writeFormula["dimreg_gamma_phi", gammaPhi];
writeFormula["dimreg_gamma_m2", gammaM2];
writeValue["check dimreg RG functions", Simplify[{beta - (3 lambda^2 kappa - 17/3 lambda^3 kappa^2), gammaPhi - lambda^2 kappa^2/12,
     gammaM2 - (lambda kappa - 5/6 lambda^2 kappa^2)}]];
(* the Wilson-Fisher fixed point in d = 4 - epsilon (epsilon = 2 eps, written \epsilon): epsilon = beta/lambda, inverted
   by InverseSeries; eta = 2 gamma_phi, nu = 1/(2 - gamma_m2), omega = d(-epsilon lambda + beta)/d lambda there, to
   epsilon^2 *)
lambdaStar = Normal[InverseSeries[Series[beta/lambda, {lambda, 0, 2}], epsilon]];
exponents = Normal[Series[{2 gammaPhi, 1/(2 - gammaM2), D[-epsilon lambda + beta, lambda]} /. lambda -> lambdaStar, {epsilon, 0, 2}]];
writeFormula["dimreg_fixed_point", lambdaStar];
writeFormula["dimreg_eta", exponents[[1]]];
writeFormula["dimreg_nu", exponents[[2]]];
writeFormula["dimreg_omega", exponents[[3]]];
writeValue["check dimreg Wilson-Fisher", Expand[exponents - {epsilon^2/54, 1/2 + epsilon/12 + 7 epsilon^2/162, epsilon - 17 epsilon^2/27}]];
(* at epsilon = 1, as text with three decimals *)
writeValue["dimreg exponents in three dimensions", StringJoin[Riffle[ToString[N[Round[1000 #]/1000]] & /@ (exponents /. epsilon -> 1), ", "]]];
writeValue["seconds dimreg", N[Round[10 (AbsoluteTime[] - dimregStart)]/10]];

(* ---------------- QED at one loop in dimensional regularization ---------------- *)
(* the worked example Tests/QEDOneLoop.wl (Peskin-Schroeder conventions with e > 0, d = 4 - 2 eps, the MS-bar measure): the theory in
   condensed notation (qedZ0, qedInteraction, qedRules, qedVertexFunction) and qedOneLoop[], whose results are rules; the
   values of the textbooks are checked. The formulas are written by the code of the showcase (below) *)
qedStart = AbsoluteTime[];
$qedReport = False;
Get[FileNameJoin[{ParentDirectory[Directory[]], "Tests", "QEDOneLoop.wl"}]];
(* eps is written veps in the formulas only, so that it comes after pi *)
qedNames = {veps -> "\\varepsilon", q2 -> "q^{2}"};
qedFormula[name_String, x_] := writeFormula[name, x /. eps -> veps, "Names" -> qedNames];
(* the photon waves need the size of the pictures of QCD *)
qedOptions = Sequence["Scale" -> 0.5, "LineWidth" -> 0.45];
(* the 1PI functions: -i Sigma_2, i Pi_2, i e delta Gamma^mu (and the tree vertex i e gamma^mu; e > 0, the electron has charge -e) *)
qedSigmaDiagrams = qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 2];
qedPiDiagrams = qedVertexFunction[{photon[x], photon[y]}, {x, y}, 2];
qedTreeVertex = qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 1];
qedVertexDiagrams = timed["QED vertex diagrams", qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]}, {x1, x2, x3}, 3]];
(* the self-energy larger: its photon arc spans one unit of the axis only *)
writeTerms["qed_self_energy", qedSigmaDiagrams, {x, y}, "Scale" -> 0.7, "LineWidth" -> 0.45];
writeTerms["qed_vacuum_polarization", qedPiDiagrams, {x, y}, qedOptions];
writeTerms["qed_vertex", qedVertexDiagrams, {x1, x2, x3}, qedOptions];
(* the amputated connected vertex of order e^3 has the corrections of the legs too: only one of its diagrams is 1PI *)
qedVertexConnected = AmputateExternalLegs[ConnectedCorrelationFunction[qedZ0, {electron[x1], electronbar[x2], photon[x3]}, qedInteraction, 3,
    "CouplingOrder" -> {e, 3}], {x1, x2, x3}, qedLines];
writeValue["counts QED", Length[DiagramTerms[#]] & /@ {qedSigmaDiagrams, qedPiDiagrams, qedVertexConnected, qedVertexDiagrams}];
(* in momentum space, with the loop momentum k and the contracted Lorentz indices called rho and sigma *)
qedIndexNames[f_] := With[{zs = Union[Cases[{f}, DummyVariable[__], Infinity]]}, f /. Thread[zs -> Take[{rho, sigma}, Length[zs]]]];
qedTree = DiracSimplify[QEDIntegrand[qedTreeVertex, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[1], "Indices" -> {x3 -> mu}]];
writeFormula["qed_tree_vertex", qedTree];
writeValue["check QED tree vertex", DiracSimplify[qedTree - I e DiracProduct[GammaMatrix[mu]]]];
writeFormula["qed_sigma_integrand", qedIndexNames[QEDIntegrand[qedSigmaDiagrams, {y -> p, x -> -p}, qedRules[xi], "LoopMomentum" -> k]],
  "Names" -> qedNames];
writeFormula["qed_pi_integrand", QEDIntegrand[qedPiDiagrams, {y -> q, x -> -q}, qedRules[xi], "Indices" -> {x -> mu, y -> nu}, "LoopMomentum" -> k],
  "Names" -> qedNames];
(* the vertex in the Feynman gauge, xi -> 1 *)
writeFormula["qed_vertex_integrand", qedIndexNames[QEDIntegrand[qedVertexDiagrams, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi],
    "Indices" -> {x3 -> mu}, "LoopMomentum" -> k] /. xi -> 1], "Names" -> qedNames];
(* the computation: Sigma_2, Pi_2 and the vertex exactly in d, their expansions, the counterterms, the form factors *)
(* qedOneLoop[] repeats the computation of the showcase (below) for the checks: its time is not counted in "seconds QED" *)
qedChecksStart = AbsoluteTime[];
qedResults = timed["QED one loop", qedOneLoop[]];
qedChecksSeconds = AbsoluteTime[] - qedChecksStart;
qv[key_String] := key /. qedResults;
(* the formulas: the code of the showcase, as shown, with the symbol alpha (toAlpha), which has the value e^2/(4 pi) here
   (QEDOneLoop.wl): hence the Block. qedShown keeps the results with alpha -> e^2/(4 pi), for the checks *)
Block[{alpha, toAlpha, electronSelfEnergy, sa, sb, pole, piMuNu, pi2, delta3, dGamma, chain, f1, f2, f3, vertexLambda, f1Lambda, shown},
  toAlpha = e -> Sqrt[4 Pi alpha];
  (* the self-energy: Sigma_2 = m A + pslash B; the poles of {A, B}, MS-bar delta2 and Zm; on shell (IR in dim. reg.) (m - m0)/m *)
  electronSelfEnergy = DiracSimplify[I IntegrateOneLoop[QEDIntegrand[qedSigmaDiagrams, {y -> p, x -> -p}, qedRules[xi],
       "LoopMomentum" -> k], eps]] /. toAlpha;
  {sa, sb} = {Coefficient[electronSelfEnergy, DiracProduct[]]/m, Coefficient[electronSelfEnergy, DiracProduct[SlashedMomentum[p]]]};
  pole = Simplify[ExpandParameterIntegrals[{sa, sb}, eps, -1]];
  shown["pole"] = pole; shown["delta2"] = pole[[2]]; shown["Zm"] = Expand[1 - pole[[1]] - pole[[2]]];
  shown["massShift"] = ExpandParameterIntegrals[sa + sb /. ScalarProduct[p, p] -> m^2, eps, 0];
  (* the vacuum polarization: Pi_2 and Pi-hat_2 in alpha, delta3 and beta(e) in e *)
  piMuNu = DiracSimplify[-I IntegrateOneLoop[QEDIntegrand[qedPiDiagrams, {y -> q, x -> -q}, qedRules[xi], "Indices" -> {x -> mu, y -> nu},
       "LoopMomentum" -> k], eps]];
  pi2 = -Coefficient[piMuNu, FourVector[q, mu] FourVector[q, nu]];
  shown["Pi"] = ExpandParameterIntegrals[pi2, eps, 0] /. toAlpha;
  shown["delta3"] = delta3 = ExpandParameterIntegrals[pi2, eps, -1];
  shown["beta"] = BetaFunctionFromZ[1 - delta3/2, e, eps, 3, "ScalePower" -> 1];
  shown["PiHat"] = ExpandParameterIntegrals[pi2 - (pi2 /. ScalarProduct[q, q] -> 0), eps, 0] /. toAlpha;
  (* the vertex: F2 in the Feynman gauge, F2(0), the UV pole of F1 (photon mass), F1(0) - 1 (IR in dim. reg.) *)
  dGamma = -I/e IntegrateOneLoop[QEDIntegrand[qedVertexDiagrams, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi], "Indices" -> {x3 -> mu},
       "LoopMomentum" -> k], eps] /. toAlpha;
  chain = DiracSimplify[SpinorChain[{p2, m}, dGamma, {p1, m}]] /. ScalarProduct[p1, p2] -> m^2 - q2/2;
  {f1, f2, f3} = VertexFormFactors[chain, mu, {p1, p2, m}];
  shown["F2"] = ExpandParameterIntegrals[f2 /. xi -> 1, eps, 0];
  shown["F2zero"] = ExpandParameterIntegrals[f2 /. q2 -> 0, eps, 0];
  vertexLambda = -I/e IntegrateOneLoop[QEDIntegrand[qedVertexDiagrams, {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules[xi, lambda],
       "Indices" -> {x3 -> mu}], eps] /. toAlpha;
  f1Lambda = First[VertexFormFactors[DiracSimplify[SpinorChain[{p2, m}, vertexLambda, {p1, m}]] /. ScalarProduct[p1, p2] -> m^2 - q2/2,
      mu, {p1, p2, m}]];
  shown["F1pole"] = ExpandParameterIntegrals[f1Lambda /. q2 -> 0, eps, -1];
  shown["F1zero"] = ExpandParameterIntegrals[f1 /. q2 -> 0, eps, 0];
  qedFormula["qed_sigma_pole_a", shown["pole"][[1]]];
  qedFormula["qed_sigma_pole_b", shown["pole"][[2]]];
  (* the pole of Sigma_2 = m A + pslash B, assembled for the demo video (Development/make_demo_video.py) *)
  qedFormula["qed_sigma_pole", Simplify[m shown["pole"][[1]] + shown["pole"][[2]] SlashedMomentum[p]]];
  qedFormula["qed_delta2", shown["delta2"]];
  qedFormula["qed_zm", shown["Zm"]];
  qedFormula["qed_mass_shift", shown["massShift"]];
  qedFormula["qed_pi", shown["Pi"]];
  qedFormula["qed_delta3", shown["delta3"]];
  qedFormula["qed_beta", shown["beta"]];
  qedFormula["qed_pi_hat", shown["PiHat"]];
  qedFormula["qed_F2", shown["F2"]];
  qedFormula["qed_F2_zero", shown["F2zero"]];
  qedFormula["qed_F1_pole", shown["F1pole"]];
  qedFormula["qed_F1_zero", shown["F1zero"]];
  qedShown = Association[Table[key -> (shown[key] /. alpha -> e^2/(4 Pi)), {key, {"pole", "delta2", "Zm", "massShift", "Pi", "delta3", "beta",
       "PiHat", "F2", "F2zero", "F1pole", "F1zero"}}]]];
qs[key_String] := qedShown[key];
(* the self-energy: the poles of {A, B}; MS-bar delta2 and Zm; on shell (IR in dim. reg.) (m - m0)/m = -delta Z2 *)
writeValue["check QED Sigma pole", Simplify[qs["pole"] - alpha/(4 Pi eps) {3 + xi, -xi}]];
writeValue["check QED delta2 and Zm", Simplify[{qs["delta2"] + xi alpha/(4 Pi eps), qs["Zm"] - 1 + 3 alpha/(4 Pi eps)}]];
writeValue["check QED on shell", Simplify[{qs["massShift"] - alpha/(4 Pi) (3/eps + 4 + 3 L), qv["deltaZ2OS"] + qs["massShift"]}]];
(* the vacuum polarization: transverse exactly in d (the integrand of A + q^2 B is 0), independent of xi; delta3, beta *)
qedPiMuNu = qv["PiMuNu"];
writeValue["check QED transversality", Simplify[ReduceParameterIntegrals[coefficientOf[qedPiMuNu, LorentzMetric[mu, nu]] +
       ScalarProduct[q, q] coefficientOf[qedPiMuNu, FourVector[q, mu] FourVector[q, nu]]] /. (h : ParameterIntegral | SimplexIntegral)[f_, v_] :>
      h[FullSimplify[f], v]]];
writeValue["check QED Pi independent of xi", Simplify[DifferentiateUnderIntegral[qedPiMuNu, xi]]];
writeValue["check QED delta3 and beta", Simplify[{qs["delta3"] + alpha/(3 Pi eps), qs["beta"] - e^3/(12 Pi^2)}]];
(* Pi-hat_2(q^2) at q^2 = -3 m^2 against Peskin-Schroeder's -(2 alpha/pi) Int x(1 - x) log(m^2/(m^2 - x(1 - x) q^2)) *)
qedPiHatValue = N[qs["PiHat"] /. {ScalarProduct[q, q] -> -3, m -> 1, RenormalizationScale -> 1}]/e^2;
writeValue["QED Pi-hat at -3 m2", ToString[NumberForm[qedPiHatValue, 10]]];
writeValue["check QED Pi-hat", Chop[qedPiHatValue + 1/(2 Pi^2) NIntegrate[x (1 - x) Log[1/(1 + 3 x (1 - x))], {x, 0, 1}], 10^-12]];
(* the vertex: F1, F2, F3 in a general gauge; F2(0) = alpha/(2 pi) (Schwinger), the UV pole of F1 = -delta2 (Z1 = Z2),
   F1(0) - 1 = -delta Z2 on shell, F3 = 0; F2(-m^2) against Peskin-Schroeder *)
{qedF1, qedF2, qedF3} = qv["FormFactors"];
writeValue["check QED F2(0)", Simplify[qs["F2zero"] - alpha/(2 Pi)]];
writeValue["check QED Z1 = Z2", Simplify[{qs["F1pole"] - xi alpha/(4 Pi eps), qs["F1pole"] + qs["delta2"]}]];
writeValue["check QED Ward identity on shell", Simplify[qs["F1zero"] + qv["deltaZ2OS"]]];
(* F3 = 0: at q^2 = 0 in closed form; at any q^2 its integrand is odd under the exchange of the parameters of the two
   electron lines, x2 <-> x3 (F3 = (F3 + F3 with x2 <-> x3)/2 = 0) *)
writeValue["check QED F3", Simplify[{ReduceParameterIntegrals[qedF3 /. q2 -> 0], ReduceParameterIntegrals[qedF3] /. SimplexIntegral[f_, v_] :>
      SimplexIntegral[Simplify[(f + (f /. {FeynmanParameter[2] -> FeynmanParameter[3], FeynmanParameter[3] -> FeynmanParameter[2]}))/2], v]}]];
qedF2Value = N[qv["F2minusM2"] /. m -> 1]/e^2;
writeValue["QED F2 at -m2", ToString[NumberForm[qedF2Value, 10]]];
writeValue["check QED F2(-m2)", Chop[qedF2Value - 1/(8 Pi^2) NIntegrate[2 z (1 - z)/((1 - z)^2 + x (1 - x - z)), {z, 0, 1}, {x, 0, 1 - z}],
    10^-10]];
(* the gauge dependence of F2 at q^2 = -m^2, with a photon mass lambda = m/10, exactly in d at eps = -3/10 (numerically 0) *)
writeValue["check QED F2 independent of xi", Chop[Quiet[N[ReduceParameterIntegrals[DifferentiateUnderIntegral[qv["FormFactorsPhotonMass"][[2]], xi]] /.
       {q2 -> -1, m -> 1, lambda -> 1/10, RenormalizationScale -> 1, eps -> -3/10}], {NIntegrate::slwcon, NIntegrate::ncvb, NIntegrate::izero}], 10^-10]];
(* Schwinger's a = (g - 2)/2 = alpha/(2 pi), with alpha = 1/137.035999177 *)
writeValue["QED Schwinger value", ToString[NumberForm[N[1/(2 Pi 137.035999177)], 5]]];
writeValue["seconds QED", N[Round[10 (AbsoluteTime[] - qedStart - qedChecksSeconds)]/10]];

(* ---------------- QCD ---------------- *)
(* gluons, quarks and ghosts in condensed notation: a label stands for a point, a Lorentz index and a colour index (and a
   spinor index for quarks); the vertices are local kernels, drawn as points *)
DeclareSource[Jgluon]; DeclareGrassmannSource[chi, chibar, sigma, sigmabar];
DeclareField[gluon, Jgluon]; DeclareField[quark, chibar]; DeclareField[quarkbar, chi, -1];
DeclareField[ghost, sigmabar]; DeclareField[ghostbar, sigma, -1];
DeclareSymmetric[GluonPropagator, ThreeGluonVertex, FourGluonVertex];
DeclareLocalVertex[QuarkGluonVertex, ThreeGluonVertex, FourGluonVertex, GhostGluonVertex];
DeclareLineStyle[GluonPropagator, "Curly"]; DeclareDirectedPropagator[QuarkPropagator];
DeclareLineStyle[GhostPropagator, "DottedArrow"];
(* D = <A A>, S = <q qbar>, the ghost propagator <c cbar> *)
FreeQCDGeneratingFunctional = Exp[1/2 IntegralOver[Jgluon[x] GluonPropagator[x, y] Jgluon[y], {x, y}] +
    IntegralOver[QuarkPropagator[x, y] chibar[x] ** chi[y], {x, y}] + IntegralOver[GhostPropagator[x, y] sigmabar[x] ** sigma[y], {x, y}]];
QCDVertices = g IntegralOver[QuarkGluonVertex[x, y, z] quarkbar[x] ** quark[y] gluon[z], {x, y, z}] +
   g/6 IntegralOver[ThreeGluonVertex[x, y, z] gluon[x] gluon[y] gluon[z], {x, y, z}] +
   g^2/24 IntegralOver[FourGluonVertex[w, x, y, z] gluon[w] gluon[x] gluon[y] gluon[z], {w, x, y, z}] +
   g IntegralOver[GhostGluonVertex[x, y, z] ghostbar[x] ** ghost[y] gluon[z], {x, y, z}];
qcdLines = {GluonPropagator, QuarkPropagator, GhostPropagator};
(* the coils of the gluons need larger pictures than the others *)
qcdOptions = Sequence["Scale" -> 0.5, "LineWidth" -> 0.45];
(* the one-particle-irreducible function of the fields at order g^m: the connected function, its legs removed *)
qcdVertexFunction[fields_List, labels_List, m_Integer] := ParticleIrreducibleDiagrams[AmputateExternalLegs[
     ConnectedCorrelationFunction[FreeQCDGeneratingFunctional, fields, QCDVertices, m, "CouplingOrder" -> {g, m}], labels, qcdLines], 1,
   labels, qcdLines];
gluonSelfEnergy = timed["QCD gluon self-energy", qcdVertexFunction[{gluon[x], gluon[y]}, {x, y}, 2]];
writeTerms["qcd_gluon_self_energy", gluonSelfEnergy, {x, y}, qcdOptions];
writeValue["check QCD gluon self-energy symmetric", SimplifyDiagrams[gluonSelfEnergy - (gluonSelfEnergy /. {x -> y, y -> x})]];
qcdQuarkSelfEnergy = qcdVertexFunction[{quark[x], quarkbar[y]}, {x, y}, 2];
writeTerms["qcd_quark_self_energy", qcdQuarkSelfEnergy, {x, y}, qcdOptions];
writeTerms["qcd_ghost_self_energy", qcdVertexFunction[{ghost[x], ghostbar[y]}, {x, y}, 2], {x, y}, qcdOptions];
qcdQuarkGluon = timed["QCD quark-gluon vertex", qcdVertexFunction[{quark[x1], quarkbar[x2], gluon[x3]}, {x1, x2, x3}, 3]];
writeTerms["qcd_quark_gluon_vertex", qcdQuarkGluon, {x1, x2, x3}, qcdOptions];
qcdThreeGluon = timed["QCD three-gluon vertex", qcdVertexFunction[{gluon[x1], gluon[x2], gluon[x3]}, {x1, x2, x3}, 3]];
writeTerms["qcd_three_gluon_vertex", qcdThreeGluon, {x1, x2, x3}, qcdOptions];
qcdQuarkSelfEnergyTwo = timed["QCD quark self-energy two loops", qcdVertexFunction[{quark[x], quarkbar[y]}, {x, y}, 4]];
writeTerms["qcd_quark_self_energy_two_loops", qcdQuarkSelfEnergyTwo, {x, y}, qcdOptions];
writeValue["counts QCD", Length[DiagramTerms[#]] & /@ {gluonSelfEnergy, qcdQuarkSelfEnergy, qcdQuarkGluon, qcdThreeGluon,
    qcdQuarkSelfEnergyTwo}];

(* ---------------- the tree expansion ---------------- *)
(* the differentiation rules, declared as ConnectedFunctionFromVertices declares them:
   dPhi_a/dJ_b = G_ab,  dG_ab/dJ_c = -G_ax G_by G_cz Gamma_xyz,  dGamma_a.../dJ_b = Gamma_a...x G_xb *)
DeclareSource[ExternalSource]; DeclareSymmetric[FullPropagator, VertexFunction];
DeclareFunctionalDerivative[MeanField[a_], ExternalSource[b_], FullPropagator[a, b]];
DeclareFunctionalDerivative[FullPropagator[a_, b_], ExternalSource[c_],
  -IntegralOver[FullPropagator[a, x] FullPropagator[b, y] FullPropagator[c, z] VertexFunction[x, y, z], {x, y, z}]];
DeclareFunctionalDerivative[VertexFunction[as__], ExternalSource[b_], IntegralOver[VertexFunction[as, x] FullPropagator[x, b], {x}]];
treeLabels = {x1, x2, x3, x4, x5, x6};
(* W^(n) = d/dJ(x1) ... d/dJ(x(n-2)) G(x(n-1), x(n)) *)
vertexTrees = Table[timed["trees " <> ToString[n], FunctionalDerivative[FullPropagator[treeLabels[[n - 1]], treeLabels[[n]]],
     Sequence @@ (ExternalSource /@ Take[treeLabels, n - 2])]], {n, 3, 6}];
writeValue["counts trees", Table[Length[DiagramTerms[w]], {w, vertexTrees}]];
(* the rule of a line, and the four-point function with its labels *)
(* the trees are drawn larger than the other pictures: their lines are short *)
treeOptions = Sequence["Scale" -> 0.5, "LineWidth" -> 0.45];
writeTerms["tree_rule_line_lhs", FullPropagator[a, b], {a, b}, treeOptions];
writeTerms["tree_rule_line", FunctionalDerivative[FullPropagator[a, b], ExternalSource[c]], {a, b, c}, treeOptions];
writeTerms["tree_four_point", vertexTrees[[2]] /. Thread[Take[treeLabels, 4] -> {a, b, c, d}], {a, b, c, d}, treeOptions];
(* the trees of the same shape: the labels are integrated over, and the canonical form adds them up; each picture, its
   leaves free ends, has the number of trees of its shape in front *)
Do[writeTerms["tree_shapes_" <> ToString[n], SimplifyDiagrams[IntegralOver[vertexTrees[[n - 2]], Take[treeLabels, n]]], {}, treeOptions],
  {n, 3, 6}];
(* zero dimensions: W^(n) = (G d/dPhi)^(n-2) G with G = 1/Gamma2(Phi), dG/dPhi = -G^2 Gamma3, dGamma_k/dPhi = Gamma_(k+1) *)
zeroDimensional[w_] := Expand[Total[(DiagramTerms[w] /. IntegralOver[f_, _] :> f) /.
     {FullPropagator[__] -> gZero, VertexFunction[as__] :> vertexZero[Length[{as}]]}]];
derivativeZero[e_] := Expand[gZero (D[e, gZero] (-gZero^2 vertexZero[3]) + Sum[D[e, vertexZero[k]] vertexZero[k + 1], {k, 3, 8}])];
writeValue["check trees zero dimensions", Table[Expand[zeroDimensional[vertexTrees[[n - 2]]] - Nest[derivativeZero, gZero, n - 2]], {n, 3, 6}]];
writeFormula["tree_zero_six", zeroDimensional[vertexTrees[[4]]] /. vertexZero[k_] :> Symbol["vertexZero" <> ToString[k]],
  "Names" -> Join[{gZero -> "G"}, Table[Symbol["vertexZero" <> ToString[k]] -> "{\\Gamma^{(" <> ToString[k] <> ")}}", {k, 3, 6}]]];
(* the same trees from ConnectedFunctionFromVertices; the fermionic trees (superfield rules, even vertices at zero source) *)
writeValue["check ConnectedFunctionFromVertices", SimplifyDiagrams[ConnectedFunctionFromVertices[Take[treeLabels, 5], "Boson"] - vertexTrees[[3]]]];
(* the tree expansion against perturbation theory: in phi^4 theory, with Gamma^(4) = lambda (a point) and G = D0, the
   trees of two vertices are the one-particle-reducible part of W^(6) at order lambda^2 *)
phi4SixPoint = timed["phi4 six-point", OrderPart[ConnectedCorrelationFunction[FreeBosonGeneratingFunctional, phi /@ treeLabels, Phi4Vertex, {2}],
    lambda, 2]];
phi4OnePIQ[t_] := TrueQ[ParticleIrreducibleQ[AmputateExternalLegs[t, treeLabels, {FreePropagator}], 1, treeLabels, {FreePropagator}]];
writeValue["counts phi4 six-point", {Length[DiagramTerms[phi4SixPoint]], Count[DiagramTerms[phi4SixPoint], t_ /; phi4OnePIQ[t]]}];
writeValue["check trees phi4 six-point", SimplifyDiagrams[Total[Select[DiagramTerms[phi4SixPoint], ! phi4OnePIQ[#] &]] -
    (Total[Select[DiagramTerms[vertexTrees[[4]]], Count[#, VertexFunction[_, _, _, _], Infinity] == 2 &]] /.
      {FullPropagator -> FreePropagator, VertexFunction[p_, q_, r_, t_] :> lambda DeltaFunction[p, q] DeltaFunction[p, r] DeltaFunction[p, t]})]];
fermionTrees = Table[ConnectedFunctionFromVertices[Take[treeLabels, n], "Fermion"], {n, {4, 6}}];
writeValue["counts fermion trees", Table[Length[DiagramTerms[w]], {w, fermionTrees}]];
writeTerms["tree_fermion_six", fermionTrees[[2]], treeLabels, treeOptions];
(* with fermion lines: the superfield trees projected on psi and psibar, <psi(x1) psibar(x2) psi(x3) ...>_c, with the
   many-body propagator G = -<psi psibar> *)
lineTrees = timed["fermion line trees", Table[ConnectedFunctionFromVertices[Take[treeLabels, n], "FermionLines"], {n, {4, 6}}]];
writeValue["counts fermion line trees", Length[DiagramTerms[#]] & /@ lineTrees];
writeTerms["tree_fermion_lines_four", lineTrees[[1]], Take[treeLabels, 4], treeOptions];
writeTerms["tree_fermion_lines_six", lineTrees[[2]], treeLabels, treeOptions];
(* check against the electron gas: with Gamma^(4) the bare vertex (direct minus exchange) and G = G0, the trees of two
   vertices are the whole connected six-point function at second order *)
electronSixPoint = timed["electron six-point", ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
    {psi[x1], psibar[x2], psi[x3], psibar[x4], psi[x5], psibar[x6]}, CoulombVertex, {2}]];
writeValue["counts electron six-point", Length[DiagramTerms[electronSixPoint]]];
bareVertexOut = AmputateExternalLegs[ConnectedCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x1], psibar[x2], psi[x3], psibar[x4]},
    CoulombVertex, {1}], {x1, x2, x3, x4}, {BareGreenFunction}];
writeValue["check fermion line trees", SimplifyDiagrams[electronSixPoint - (Total[Select[DiagramTerms[lineTrees[[2]]], Count[#, _VertexFunction, Infinity] == 2 &]] /.
      {FullPropagator -> BareGreenFunction, VertexFunction[o1_, o2_, i1_, i2_] :> (bareVertexOut /. {x1 -> o1, x3 -> o2, x2 -> i1, x4 -> i2})})]];

(* ---------------- effective actions as objects ---------------- *)
(* Gamma[G] = Phi - Tr ln(-G^-1) - Tr(G0^-1 G - 1) (fermions), Gamma2 + 1/2 Tr ln G^-1 + 1/2 Tr(D0^-1 G - 1) (phi^4); the
   traces are objects (TraceLog, OperatorTrace), FunctionalDerivative differentiates with respect to G[x, y] *)
eaStart = AbsoluteTime[];
eaNames = {BareGreenFunction -> "G_0", DressedGreenFunction -> "G", FreePropagator -> "D_0", DressedPropagator -> "G",
   InteractionPotential -> "V", SigmaInsertion -> "\\Sigma", SigmaInsertionB -> "\\Sigma", phiMean -> "\\bar\\phi", MeanPsi -> "\\Psi",
   MeanPsibar -> "\\bar\\Psi", BackgroundPropagator -> "D", OneBodySource -> "U", OneBodySourceB -> "U", TrialSelfEnergy -> "\\Sigma"};
gammaElectron = EffectiveAction2PI[luttingerWard, BareGreenFunction -> DressedGreenFunction];
gammaPhi4 = EffectiveAction2PI[phi4Gamma2, FreePropagator -> DressedPropagator, "Statistics" -> "Boson"];
writeFormula["ea_trace_terms", EffectiveAction2PI[0, BareGreenFunction -> DressedGreenFunction], "Names" -> eaNames];
writeFormula["ea_trace_terms_phi4", EffectiveAction2PI[0, FreePropagator -> DressedPropagator, "Statistics" -> "Boson"], "Names" -> eaNames];
writeFormula["ea_dgamma_first_order", FunctionalDerivative[EffectiveAction2PI[OrderPart[luttingerWard, InteractionPotential, 1],
    BareGreenFunction -> DressedGreenFunction], DressedGreenFunction[y, x]], "Names" -> eaNames];
writeValue["check EA stationarity", {
   SimplifyDiagrams[FunctionalDerivative[gammaElectron, DressedGreenFunction[y, x]] - selfEnergy - OperatorKernel[DressedGreenFunction^-1][x, y] +
     OperatorKernel[BareGreenFunction^-1][x, y]],
   SimplifyDiagrams[FunctionalDerivative[gammaPhi4, DressedPropagator[a, b]] - 1/2 (OperatorKernel[FreePropagator^-1][a, b] -
       OperatorKernel[DressedPropagator^-1][a, b] - phi4SelfEnergyG)]}];
(* the value at the perturbative G = (G0^-1 - Sigma_pert)^-1: -log Z0 - (connected vacuum diagrams), order by order *)
writeValue["check EA value", timed["EA value at the perturbative G", {
    byOrder[ExpandTrace[gammaElectron /. DressedGreenFunction -> Power[Power[BareGreenFunction, -1] -
            LabelFunction[selfEnergyPerturbative, {x, y}], -1], BareGreenFunction, maximumOrder, "MaximumOrder" -> {InteractionPotential, maximumOrder}] +
         vacuumDiagrams, InteractionPotential, Range[maximumOrder]],
    byOrder[ExpandTrace[gammaPhi4 /. DressedPropagator -> Power[Power[FreePropagator, -1] -
            LabelFunction[phi4SelfEnergy, {a, b}], -1], FreePropagator, 3, "MaximumOrder" -> {lambda, 3}] + phi4LogVacuum, lambda, Range[3]]}]];
(* the pieces: the rings of -Tr ln(-(G0^-1 - Sigma)) and the series of the trace term, with a self-energy blob *)
DeclareDirectedPropagator[SigmaInsertion]; DeclareBlob[SigmaInsertion, "\\Sigma"];
DeclareSymmetric[SigmaInsertionB]; DeclareBlob[SigmaInsertionB, "\\Sigma"];
writeTerms["ea_rings", ExpandTrace[-TraceLog[-(Power[BareGreenFunction, -1] - SigmaInsertion)], BareGreenFunction, 3] +
   TraceLog[-Power[BareGreenFunction, -1]], {}, blobOptions];
writeTerms["ea_trace_series", ExpandTrace[-OperatorTrace[Power[BareGreenFunction, -1] . Power[Power[BareGreenFunction, -1] - SigmaInsertion, -1] - 1],
   BareGreenFunction, 3], {}, blobOptions];
writeTerms["ea_rings_phi4", ExpandTrace[1/2 TraceLog[Power[FreePropagator, -1] - SigmaInsertionB], FreePropagator, 3] -
   1/2 TraceLog[Power[FreePropagator, -1]], {}, blobOptions];
writeTerms["ea_trace_series_phi4", ExpandTrace[1/2 OperatorTrace[Power[FreePropagator, -1] . Power[Power[FreePropagator, -1] - SigmaInsertionB, -1] - 1],
   FreePropagator, 3], {}, blobOptions];

(* derivatives with respect to two-index objects: Sigma, Gamma0, and the derivative with respect to the interaction *)
(* the shortcuts SelfEnergy and IrreducibleVertex minus the derivatives computed above *)
writeValue["check EA dPhi dG", {SimplifyDiagrams[SelfEnergy[luttingerWard, DressedGreenFunction, {x, y}] - selfEnergy],
   SimplifyDiagrams[SelfEnergy[phi4Gamma2, DressedPropagator, {a, b}, "Statistics" -> "Boson"] - phi4SelfEnergyG],
   SimplifyDiagrams[SelfEnergy[gamma2GD, DressedPropagator, {a, b}, "Statistics" -> "Boson"] - sigmaGD]}];
writeValue["check EA second derivative", timed["EA second derivatives", {
    SimplifyDiagrams[IrreducibleVertex[luttingerWard, DressedGreenFunction, {x1, x2, x3, x4}] - irreducibleVertex],
    SimplifyDiagrams[-2 FunctionalDerivative[phi4SelfEnergyG, DressedPropagator[c, d]] - phi4Gamma0],
    SimplifyDiagrams[IrreducibleVertex[phi4Gamma2, DressedPropagator, {a, b, c, d}, "Statistics" -> "Boson"] - phi4Gamma0]}]];
writeValue["check EA homogeneity", Table[SimplifyDiagrams[#] & /@ {
     IntegralOver[InteractionPotential[x, y] FunctionalDerivative[OrderPart[luttingerWard, InteractionPotential, m], InteractionPotential[x, y]], {x, y}] -
      m OrderPart[luttingerWard, InteractionPotential, m],
     IntegralOver[DressedGreenFunction[y, x] FunctionalDerivative[OrderPart[luttingerWard, InteractionPotential, m], DressedGreenFunction[y, x]], {x, y}] -
      2 m OrderPart[luttingerWard, InteractionPotential, m]}, {m, orders}]];
writeOrders["ea_dphi_dv", FunctionalDerivative[luttingerWard, InteractionPotential[x, y]], {x, y}, InteractionPotential, {0, 1}];

(* the field dependence: Gamma[phibar, G] of Cornwall, Jackiw and Tomboulis for phi^4 (phibar = <phi>), Gamma2[phibar, G] its 2PI part *)
gammaPhiG = timed["EA Gamma[phibar, G]", EffectiveAction2PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3, FreePropagator -> DressedPropagator,
    "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}]];
writeFormula["ea_gamma_phi_g_first_order", OrderPart[gammaPhiG, lambda, {0, 1}], "Names" -> eaNames];
(* the pieces (the 2PI section): S_int[phibar], 1/2 Tr(S_int''[phibar] G), and Gamma2[phibar, G], what is left of Gamma[phibar, G]
   after S[phibar] and the traces; Gamma2 must be 2PI, with at most one mean field at a vertex. S_int''[phibar] is minus the
   first-order Sigma[G] with phibar phibar in place of G (checked), as -Sigma_HF for fermions below *)
eaClassical = Phi4Vertex /. phi -> phiMean;
writeValue["check EA phi fluctuation operator", SimplifyDiagrams[FunctionalDerivative[eaClassical, phiMean[a], phiMean[b]] +
    (OrderPart[phi4SelfEnergyG, lambda, 1] /. DressedPropagator[p_, q_] :> phiMean[p] phiMean[q])]];
eaMass =SimplifyDiagrams[1/2 IntegralOver[FunctionalDerivative[eaClassical, phiMean[x], phiMean[y]] DressedPropagator[x, y], {x, y}]];
writeTerms["ea_piece_classical", eaClassical, {}];
writeTerms["ea_piece_mass", eaMass, {}];
gamma2PhiG = SimplifyDiagrams[gammaPhiG - 1/2 IntegralOver[OperatorKernel[Power[FreePropagator, -1]][x, y] phiMean[x] phiMean[y], {x, y}] -
    eaClassical - 1/2 TraceLog[Power[DressedPropagator, -1]] - 1/2 OperatorTrace[Power[FreePropagator, -1] . DressedPropagator - 1] - eaMass];
writeValue["check 2PI pieces", {SimplifyDiagrams[gamma2PhiG - ParticleIrreducibleDiagrams[gamma2PhiG, 2, {}, {DressedPropagator}]],
   Count[DiagramTerms[gamma2PhiG], t_ /; ! FreeQ[t, Power[phiMean[_], k_ /; k >= 2]]]}];
writeOrders["ea_gamma2_phi", gamma2PhiG, {}, lambda, {1, 2, 3}];
writeValue["counts EA gamma2 phi", orderCounts[gamma2PhiG, lambda, {1, 2, 3}]];
writeOrders["ea_field_equation", SimplifyDiagrams[FunctionalDerivative[gammaPhiG, phiMean[a]] -
    IntegralOver[OperatorKernel[Power[FreePropagator, -1]][a, y] phiMean[y], {y}]], {a}, lambda, {1, 2}];
(* Sigma[phibar, G] = -2 dGamma2/dG(a, b); SelfEnergy is the shortcut (checked) *)
sigmaPhiG = SimplifyDiagrams[-2 FunctionalDerivative[gamma2PhiG, DressedPropagator[a, b]]];
writeValue["check EA Sigma phibar G shortcut", SimplifyDiagrams[SelfEnergy[gamma2PhiG, DressedPropagator, {a, b}, "Statistics" -> "Boson"] - sigmaPhiG]];
writeOrders["ea_sigma_phi", SimplifyDiagrams[sigmaPhiG - SetSourcesToZero[sigmaPhiG]], {a, b}, lambda, {1, 2}];
writeValue["check EA zero mean field", SimplifyDiagrams[SetSourcesToZero[gammaPhiG] - gammaPhi4]];
writeValue["check EA mean field second derivative", byOrder[SetSourcesToZero[
       FunctionalDerivative[gammaPhiG, phiMean[a], phiMean[b]]] - OperatorKernel[Power[FreePropagator, -1]][a, b] + phi4SelfEnergyG, lambda, Range[0, 3]]];
(* the electron gas with Grassmann-odd means, to first order *)
gammaPsiG = EffectiveAction2PI[FreeFermionGeneratingFunctional, CoulombVertex, 1, BareGreenFunction -> DressedGreenFunction,
   "MeanFields" -> {psi -> MeanPsi, psibar -> MeanPsibar}];
(* d^2 Gamma/dPsibar(a) dPsi(b), the rightmost derivative first: G0^-1 - Sigma *)
writeValue["check EA fermion means", SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaPsiG, MeanPsibar[a], MeanPsi[b]]] -
    OperatorKernel[Power[BareGreenFunction, -1]][a, b] +
    OrderPart[SelfEnergy[luttingerWard, DressedGreenFunction, {a, b}], InteractionPotential, 1]]];
(* Gamma[Psibar, Psi, G] of the electron gas to third order (the 2PI section), piece by piece as Gamma[phibar, G] above:
   S_int[Psibar, Psi]; the part with the means of the trace term -Tr(D^-1[Psibar, Psi] G - 1), -Tr(S_int'' G), with
   D^-1(x, y) = FunctionalDerivative[S, Psibar[x], Psi[y]] = G0^-1 - Sigma_HF (the Hartree and Fock potentials of the means:
   the first-order self-energy with -Psi Psibar in place of G, checked); and Gamma2: the vertices with one mean come in
   pairs, from V^2 on; at zero means it is the Luttinger-Ward functional, and its second derivative there is Sigma[G]
   (beyond the first order, which is in the trace term); for phi^4 the same two checks of Gamma2[phibar, G] *)
gammaPsiG2 = timed["EA Gamma[Psibar, Psi, G]", EffectiveAction2PI[FreeFermionGeneratingFunctional, CoulombVertex, maximumOrder,
    BareGreenFunction -> DressedGreenFunction, "MeanFields" -> {psi -> MeanPsi, psibar -> MeanPsibar}]];
(* the showcase shows it to first order *)
writeFormula["ea_gamma_psi", OrderPart[gammaPsiG2, InteractionPotential, {0, 1}], "Names" -> eaNames];
eaPsiClassical = CoulombVertex /. {psi -> MeanPsi, psibar -> MeanPsibar};
eaPsiInteractionSecond = SimplifyDiagrams[FunctionalDerivative[eaPsiClassical, MeanPsibar[x], MeanPsi[y]]];
writeValue["check EA psi fluctuation operator", SimplifyDiagrams[eaPsiInteractionSecond +
    (OrderPart[selfEnergy, InteractionPotential, 1] /. DressedGreenFunction[p_, q_] :> -GrassmannProduct[MeanPsi[p], MeanPsibar[q]])]];
eaPsiMass = SimplifyDiagrams[-IntegralOver[eaPsiInteractionSecond DressedGreenFunction[y, x], {x, y}]];
writeTerms["ea_piece_psi_classical", eaPsiClassical, {}];
writeTerms["ea_piece_psi_mass", eaPsiMass, {}];
gamma2PsiG = SimplifyDiagrams[gammaPsiG2 - OrderPart[gammaPsiG2, InteractionPotential, 0] - eaPsiClassical - eaPsiMass];
writeOrders["ea_gamma2_psi", gamma2PsiG, {}, InteractionPotential, orders];
writeValue["counts EA gamma2 psi", orderCounts[gamma2PsiG, InteractionPotential, orders]];
(* a vertex of Gamma2 carries at most one mean: no interaction line with two means at its ends *)
meansAtLine[t_] := Module[{ms = Cases[t, (MeanPsi | MeanPsibar)[u_] :> u, Infinity]},
   Max[Append[(Count[ms, Alternatives @@ #] &) /@ Cases[t, InteractionPotential[u_, w_] :> {u, w}, Infinity], 0]]];
writeValue["check 2PI psi pieces", {SimplifyDiagrams[gamma2PsiG - ParticleIrreducibleDiagrams[gamma2PsiG, 2, {}, {DressedGreenFunction}]],
   Count[DiagramTerms[gamma2PsiG], t_ /; meansAtLine[t] >= 2]}];
writeValue["check 2PI means phi4", {SimplifyDiagrams[SetSourcesToZero[gamma2PhiG] - phi4Gamma2],
   byOrder[SetSourcesToZero[FunctionalDerivative[gamma2PhiG, phiMean[a], phiMean[b]]] + phi4SelfEnergyG, lambda, Range[2, 3]]}];
(* Eckhardt, Kappl, Kauch and Held (2023), Fig. 5: with Z = int e^{+S}, V2 = -D0^-1 + K and V4 = -lambda, their
   Lambda2^int = -Gamma + 1/2 Tr D0^-1 (G + phibar phibar) - 1/2 Tr ln G = -(lambda phibar^4/24 + lambda phibar^2 G/4 + Gamma2) + const *)
writeValue["check Eckhardt Fig5", byOrder[-gammaPhiG + 1/2 IntegralOver[OperatorKernel[Power[FreePropagator, -1]][x, y] *
       (DressedPropagator[x, y] + phiMean[x] phiMean[y]), {x, y}] - 1/2 TraceLog[DressedPropagator] +
     lambda/24 IntegralOver[phiMean[z]^4, {z}] + lambda/4 IntegralOver[phiMean[z]^2 DressedPropagator[z, z], {z}] + gamma2PhiG, lambda, Range[1, 3]]];
writeValue["check 2PI means electron gas", {SimplifyDiagrams[SetSourcesToZero[gamma2PsiG] - luttingerWard],
   byOrder[SetSourcesToZero[FunctionalDerivative[gamma2PsiG, MeanPsibar[x], MeanPsi[y]]] +
     selfEnergy, InteractionPotential, Range[2, maximumOrder]]}];
(* Lambda = d^4 Gamma2/dPhi^4 at Phi = 0 and fixed G: the fully irreducible vertex of the parquet decomposition *)
eaLambdaPhi4 = timed["EA Lambda phi^4", SetSourcesToZero[FunctionalDerivative[EffectiveAction2PI[FreeBosonGeneratingFunctional, Phi4Vertex, 4,
      FreePropagator -> DressedPropagator, "Statistics" -> "Boson", "MeanFields" -> {phi -> phiMean}, "MeanFieldDegree" -> {4}],
     phiMean[x1], phiMean[x2], phiMean[x3], phiMean[x4]]]];
eaFullVertexPhi4 = PerturbativeFullVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 4, {phi}, fourLabels, FreePropagator -> DressedPropagator];
writeValue["check EA Lambda phi4", timed["EA parquet phi^4", byOrder[eaLambdaPhi4 -
       ParquetDecomposition[eaFullVertexPhi4, fourLabels, {DressedPropagator}, "Statistics" -> "Boson"]["FullyIrreducible"], lambda, Range[4]]]];
writeOrders["ea_lambda_phi4", eaLambdaPhi4, fourLabels, lambda, {4}];
eaLambdaElectron = timed["EA Lambda electron gas", SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[
       EffectiveAction2PI[FreeFermionGeneratingFunctional, CoulombVertex, 4, BareGreenFunction -> DressedGreenFunction,
        "MeanFields" -> {psi -> MeanPsi, psibar -> MeanPsibar}, "MeanFieldDegree" -> {4}], MeanPsi[x4]], MeanPsibar[x3]], MeanPsi[x2]], MeanPsibar[x1]]]];
eaFullVertexElectron = timed["EA full vertex electron gas", PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, {1, 4},
    {psi, psibar}, fourLabels, BareGreenFunction -> DressedGreenFunction]];
writeValue["check EA Lambda electron gas", byOrder[eaLambdaElectron -
      ParquetDecomposition[eaFullVertexElectron, fourLabels, {DressedGreenFunction}]["FullyIrreducible"], InteractionPotential, {1, 4}]];
writeValue["counts EA Lambda", {Length[DiagramTerms[OrderPart[eaLambdaPhi4, lambda, 4]]], Length[DiagramTerms[OrderPart[eaLambdaElectron, InteractionPotential, 4]]]}];

(* the 1PI effective action Gamma[phibar], with the lines D = (D0^-1 + lambda phibar^2/2)^-1 in the background *)
(* gamma1PI, its vertex functions eaGamma1Two, eaGamma1Four and eaAtZero are computed in the phi^4 section above *)
writeFormula["ea_gamma_1pi", OrderPart[gamma1PI, lambda, {0, 1, 2}], "Names" -> eaNames];
(* Gamma[phibar] with the bare lines D0 (the 1PI section): every line D expanded, D^-1 = D0^-1 + lambda phibar^2/2, the logarithm a
   series of rings; the diagrams grouped by the number of loops L (lines - vertices + 1, unchanged by a mass insertion) *)
gamma1PIBare = timed["EA Gamma[phibar] with lines D0", ExpandTrace[gamma1PI /. BackgroundPropagator -> Power[Power[FreePropagator, -1] -
       LabelFunction[-lambda/2 phiMean[a]^2 DeltaFunction[a, b], {a, b}], -1], FreePropagator, 3, "MaximumOrder" -> {lambda, 3}]];
(* as in the showcase: the number of loops, lines - vertices + 1, and the diagrams grouped by it *)
loops[t_] := Total[Cases[{t}, IntegralOver[f_, v_List] :> Exponent[f /. p_FreePropagator :> mark p, mark] - Length[v], Infinity]] + 1;
gamma1PIPieces = KeySort[GroupBy[DiagramTerms[OrderPart[gamma1PIBare, lambda, {1, 2, 3}]], loops, Total]];
writeTerms["onepi_tree", gamma1PIPieces[0], {}];
writeTerms["onepi_rings", gamma1PIPieces[1], {}];
writeOrders["onepi_two_loops", gamma1PIPieces[2], {}, lambda, {1, 2, 3}];
writeOrders["onepi_three_loops", gamma1PIPieces[3], {}, lambda, {2, 3}];
writeOrders["onepi_four_loops", gamma1PIPieces[4], {}, lambda, {3}];
writeValue["counts 1PI bare", orderCounts[gamma1PIBare, lambda, {1, 2, 3}]];
(* checks: in zero dimensions (D0 = 1) the Legendre transform of log Z(J), Z(J) = Integral dphi exp(-phi^2/2 - lambda phi^4/24 + J phi)/Sqrt[2 Pi],
   computed as a series in lambda at fixed phibar; the diagrams 1PI with respect to D0; the one loop the rings of the logarithm *)
writeValue["check 1PI zero dimensions", Module[{w, js, cc, sol, jj, pp, moment},
   moment[k_] := If[OddQ[k], 0, (k - 1)!!];
   w = jj^2/2 + Normal[Series[Log[Sum[(-lambda/24)^k/k! Sum[Binomial[4 k, j] jj^(4 k - j) moment[j], {j, 0, 4 k}], {k, 0, 3}]], {lambda, 0, 3}]];
   js = pp + Sum[cc[k] lambda^k, {k, 3}];
   sol = First[Solve[Thread[Rest[CoefficientList[Normal[Series[(D[w, jj] /. jj -> js) - pp, {lambda, 0, 3}]], lambda]] == 0], Array[cc, 3]]];
   Expand[ZeroDimensionalValue[gamma1PIBare /. phiMean[_] -> pp, {FreePropagator -> 1}] - Normal[Series[(-w + jj pp) /. jj -> (js /. sol), {lambda, 0, 3}]]]]];
writeValue["check 1PI pieces", {SimplifyDiagrams[OrderPart[gamma1PIBare, lambda, {1, 2, 3}] -
     ParticleIrreducibleDiagrams[OrderPart[gamma1PIBare, lambda, {1, 2, 3}], 1, {}, {FreePropagator}]],
   SimplifyDiagrams[gamma1PIPieces[1] - ExpandTrace[1/2 TraceLog[Power[FreePropagator, -1] - LabelFunction[-lambda/2 phiMean[a]^2 DeltaFunction[a, b], {a, b}]],
       FreePropagator, 3] + 1/2 TraceLog[Power[FreePropagator, -1]]]}];
(* Gamma1[phibar] = Gamma - S[phibar] - 1/2 Tr ln S''[phibar] (S'' = D^-1, the lines D[phibar] the head BackgroundPropagator): minus the
   1PI vacuum diagrams with lines D[phibar] and the vertices lambda phibar phi^3/6 and lambda phi^4/24 (ConnectedVacuumDiagrams); at
   G = D[phibar], Gamma2[phibar, G] of the 2PI section is its part 2PI in these lines, and the others are 2PI reducible *)
gammaOne1PI = SimplifyDiagrams[gamma1PI - 1/2 IntegralOver[OperatorKernel[Power[FreePropagator, -1]][x, y] phiMean[x] phiMean[y], {x, y}] -
    (Phi4Vertex /. phi -> phiMean) - 1/2 TraceLog[Power[BackgroundPropagator, -1]]];
writeValue["check 1PI gamma one", byOrder[gammaOne1PI + ParticleIrreducibleDiagrams[ConnectedVacuumDiagrams[FreeBosonGeneratingFunctional /.
       FreePropagator -> BackgroundPropagator, lambda/6 IntegralOver[phiMean[z] phi[z]^3, {z}] + Phi4Vertex, 3], 1, {}, {BackgroundPropagator}], lambda, {1, 2, 3}]];
writeValue["check 2PI gamma one", byOrder[(gamma2PhiG /. DressedPropagator -> BackgroundPropagator) -
    ParticleIrreducibleDiagrams[gammaOne1PI, 2, {}, {BackgroundPropagator}], lambda, {1, 2, 3}]];
writeValue["counts 1PI not two PI", orderCounts[SimplifyDiagrams[gammaOne1PI - ParticleIrreducibleDiagrams[gammaOne1PI, 2, {}, {BackgroundPropagator}]],
    lambda, {1, 2, 3}]];
(* Gamma[phibar] = Gamma[phibar, G[phibar]] at the stationary G[phibar] = (d^2 Gamma[phibar]/d phibar d phibar)^-1 = (D0^-1 - Sigma[phibar])^-1,
   the full propagator in the background, expanded in D0 (the 2PI section) *)
eaSigmaBackground = SimplifyDiagrams[OperatorKernel[Power[FreePropagator, -1]][a, b] - FunctionalDerivative[gamma1PIBare, phiMean[a], phiMean[b]]];
writeValue["check 2PI at stationary G", timed["EA Gamma[phibar, G[phibar]]", Module[{back = ExpandTrace[gammaPhiG /. DressedPropagator ->
        Power[Power[FreePropagator, -1] - LabelFunction[eaSigmaBackground, {a, b}], -1], FreePropagator, 3, "MaximumOrder" -> {lambda, 3}]},
    byOrder[back - gamma1PIBare, lambda, Range[0, 3]]]]];
writeValue["check EA 1PI two point", byOrder[eaGamma1Two - OperatorKernel[Power[FreePropagator, -1]][a, b] +
      phi4SelfEnergy, lambda, Range[0, 3]]];
writeValue["check EA 1PI four point", byOrder[eaGamma1Four +
      (AmputateExternalLegs[ParticleIrreducibleDiagrams[phi4FourPointConnected, 1, {a, b, c, d}, {FreePropagator}], {a, b, c, d}, {FreePropagator}] /.
        Thread[{a, b, c, d} -> fourLabels]), lambda, Range[1, 3]]];
writeOrders["ea_gamma4_1pi", gamma4, {a, b, c, d}, lambda, {1, 2}];
writeOrders["ea_mean_field_equation", OrderPart[ExpandTrace[FunctionalDerivative[gamma1PI, phiMean[a]]] -
    IntegralOver[OperatorKernel[Power[FreePropagator, -1]][a, y] phiMean[y], {y}] - lambda/6 phiMean[a]^3, lambda, {1, 2}], {a}, lambda, {1, 2}];

(* Gamma[phibar] by the bare-line route of EffectiveAction1PI (D0 -> D0): the vertices of degree two kept, the same series *)
gamma1PIDirect = timed["EA Gamma[phibar] bare route", EffectiveAction1PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3, phi -> phiMean,
    FreePropagator -> FreePropagator]];
writeValue["check 1PI bare route", byOrder[gamma1PIDirect - gamma1PIBare, lambda, Range[0, 3]]];

(* the 1PI effective action of the electron gas, Gamma[Psibar, Psi] (the 1PI section), with bare lines: -Psibar G0^-1 Psi + S_int[Psibar, Psi]
   - Tr ln(-G0^-1) - (connected 1PI vacuum diagrams with the vertices of S_int[Psibar + psibar, Psi + psi] of degree two or more);
   grouped by the number of loops, L = lines - interaction lines + 1, as in the showcase *)
(* gammaPsi1PI is computed in the section of the two-particle function above *)
loopsPsi[t_] := Total[Cases[{t}, IntegralOver[f_, _List] :> Exponent[f /. p_BareGreenFunction :> mark p, mark] -
       Exponent[f /. p_InteractionPotential :> mark p, mark], Infinity]] + 1;
gammaPsi1PIPieces = KeySort[GroupBy[DiagramTerms[OrderPart[gammaPsi1PI, InteractionPotential, orders]], loopsPsi, Total]];
writeTerms["onepi_psi_tree", gammaPsi1PIPieces[0], {}];
writeOrders["onepi_psi_rings", gammaPsi1PIPieces[1], {}, InteractionPotential, orders];
writeOrders["onepi_psi_two_loops", gammaPsi1PIPieces[2], {}, InteractionPotential, {1, 2}];
writeOrders["onepi_psi_three_loops", gammaPsi1PIPieces[3], {}, InteractionPotential, {2}];
(* the numbers of diagrams of each piece (L loops) order by order, V to V^maximumOrder *)
Do[writeValue["counts 1PI psi " <> {"tree", "rings", "two loops", "three loops", "four loops"}[[l + 1]],
    orderCounts[Lookup[gammaPsi1PIPieces, l, 0], InteractionPotential, orders]], {l, 0, 4}];
writeValue["counts 1PI psi orders", orderCounts[gammaPsi1PI, InteractionPotential, orders]];
(* checks: 1PI; at zero means -log Z; the vertex functions: d^2 Gamma/dPsibar dPsi = G0^-1 - Sigma (perturbative) and F with bare lines *)
gammaPsi1PIFour = SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[
       orderPart[gammaPsi1PI, InteractionPotential, {1, 2}], MeanPsi[x4]], MeanPsibar[x3]], MeanPsi[x2]], MeanPsibar[x1]]];
writeValue["check 1PI psi", timed["EA Gamma[Psibar, Psi] checks", {
    SimplifyDiagrams[OrderPart[gammaPsi1PI, InteractionPotential, orders] - ParticleIrreducibleDiagrams[OrderPart[gammaPsi1PI, InteractionPotential, orders],
       1, {}, {BareGreenFunction}]],
    SimplifyDiagrams[SetSourcesToZero[gammaPsi1PI] + TraceLog[-Power[BareGreenFunction, -1]] + vacuumDiagrams],
    byOrder[SetSourcesToZero[FunctionalDerivative[gammaPsi1PI, MeanPsibar[x], MeanPsi[y]]] -
        OperatorKernel[Power[BareGreenFunction, -1]][x, y] + selfEnergyPerturbative, InteractionPotential, Range[0, maximumOrder]],
    byOrder[gammaPsi1PIFour - AmputateExternalLegs[ParticleIrreducibleDiagrams[ConnectedCorrelationFunction[
            FreeFermionGeneratingFunctional, {psi[x1], psibar[x2], psibar[x4], psi[x3]}, CoulombVertex, 2], 1, fourLabels, {BareGreenFunction}],
          fourLabels, {BareGreenFunction}], InteractionPotential, Range[1, 2]]}]];
(* Gamma[Psibar, Psi, G] at its stationary G: the Dyson equation solved to first order is enough to third order (the error in G
   is O(V^2), that of Gamma O(V^4), by stationarity). The normal G cannot carry the anomalous vertices V Psi Psi and V Psibar Psibar
   (Nambu's functional at zero anomalous propagator): the diagrams of Gamma[Psibar, Psi] with such a vertex are missing *)
eaPsiX = SimplifyDiagrams[eaPsiMass + gamma2PsiG];
eaPsiStar = Power[Power[BareGreenFunction, -1] - LabelFunction[SimplifyDiagrams[OrderPart[SelfEnergy[eaPsiX, DressedGreenFunction, {a, b}],
       InteractionPotential, 1] /. DressedGreenFunction -> BareGreenFunction], {a, b}], -1];
eaPsiBack = timed["EA Gamma[Psibar, Psi, G*]", Sum[ExpandTrace[OrderPart[eaPsiX, InteractionPotential, m] /. DressedGreenFunction -> eaPsiStar,
      BareGreenFunction, maximumOrder - m, "MaximumOrder" -> {InteractionPotential, maximumOrder}], {m, orders}] +
    ExpandTrace[SimplifyDiagrams[gammaPsiG2 - eaPsiX] /. DressedGreenFunction -> eaPsiStar, BareGreenFunction, maximumOrder,
     "MaximumOrder" -> {InteractionPotential, maximumOrder}]];
anomalousVertexQ[t_] := Module[{ps = Cases[t, MeanPsi[u_] :> u, Infinity], pbs = Cases[t, MeanPsibar[u_] :> u, Infinity]},
   AnyTrue[Cases[t, InteractionPotential[u_, w_] :> {u, w}, Infinity], MemberQ[{{2, 0}, {0, 2}}, {Count[ps, Alternatives @@ #], Count[pbs, Alternatives @@ #]}] &]];
eaPsiAnomalous = Total[Select[DiagramTerms[gammaPsi1PI], anomalousVertexQ]];
writeValue["check 2PI psi at stationary G", byOrder[eaPsiBack - gammaPsi1PI + eaPsiAnomalous, InteractionPotential, Range[0, maximumOrder]]];
writeValue["counts 1PI psi anomalous", orderCounts[eaPsiAnomalous, InteractionPotential, orders]];
writeTerms["onepi_psi_anomalous_2", OrderPart[eaPsiAnomalous, InteractionPotential, 2], {}];

(* ---------------- superfields: the functional of Nambu and Gor'kov (the section on pairing) ---------------- *)
(* the superfield (psi, psibar), a label a = (x, component): one Grassmann source, one antisymmetric propagator
   calG_ab = -<T psi_a psi_b> (the Nambu matrix [[calG_psipsi, G], [-G^T, calG_psibarpsibar]]) and one totally antisymmetric
   vertex, written as for phi^4. Gamma[calG] = Phi[calG] - 1/2 Tr ln(-calG^-1) - 1/2 Tr((calG0^-1 - K) calG - 1), with the
   pairing sources in K; Sigma = -2 dPhi/dcalG, Gamma0 = 4 d^2Phi/dcalG dcalG, the kernels of the channels s, t, u
   Gamma_r(a, b, c, d) = sgn(pi_r) Gamma0(pi_r(a, b, c, d)); SuperfieldComponents projects on psi and psibar (s -> ph, t -> pp,
   u -> phbar for the legs (x1 psibar, x2 psi, x3 psibar, x4 psi)) *)
nambuStart = AbsoluteTime[];
DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex, NambuTwoPointSource, NambuSelfEnergy]; DeclareLocalVertex[NambuVertex];
DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar, BareAnomalousGreenFunction, BareAnomalousGreenFunctionBar, GapFunction,
  GapFunctionBar];
DeclareLineStyle[AnomalousGreenFunction, "DivergingArrows"]; DeclareLineStyle[AnomalousGreenFunctionBar, "ConvergingArrows"];
nambuZ0 = Exp[1/2 IntegralOver[BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b], {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d] NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
nambuNames = {BareNambuPropagator -> "\\mathcal G_0", NambuPropagator -> "\\mathcal G", NambuVertex -> "\\mathcal V", NambuTwoPointSource -> "K",
   NambuMean -> "\\sfieldmean", AnomalousGreenFunction -> "\\mathcal G_{\\psi\\psi}", AnomalousGreenFunctionBar -> "\\mathcal G_{\\bar\\psi\\bar\\psi}",
   DressedGreenFunction -> "G", BareGreenFunction -> "G_0", InteractionPotential -> "V"};
(* the entries of the Nambu matrix, x = left, y = right *)
writeTerms["nambu_entry_pp", AnomalousGreenFunction[x, y], {x, y}, blobOptions];
writeTerms["nambu_entry_g", DressedGreenFunction[x, y], {x, y}, blobOptions];
writeTerms["nambu_entry_gt", -DressedGreenFunction[y, x], {x, y}, blobOptions];
writeTerms["nambu_entry_bb", AnomalousGreenFunctionBar[x, y], {x, y}, blobOptions];
writeFormula["nambu_trace_terms", EffectiveAction2PI[0, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield",
   "Source" -> NambuTwoPointSource], "Names" -> nambuNames];
(* Phi of the superfield: one diagram at each order; its self-energy and its kernel *)
nambuPhi = timed["Nambu Phi", LuttingerWardFunctional[nambuZ0, nambuInteraction, maximumOrder, BareNambuPropagator -> NambuPropagator]];
writeValue["counts Nambu Phi", orderCounts[nambuPhi, NambuVertex, orders]];
writeOrders["nambu_phi", nambuPhi, {}, NambuVertex, orders];
writeFormula["nambu_phi_first_order", OrderPart[nambuPhi, NambuVertex, 1], "Names" -> nambuNames];
(* Sigma = -2 dPhi/dcalG (the derivative with respect to the antisymmetric calG is antisymmetrised); SelfEnergy is its shortcut *)
nambuSigma = -2 FunctionalDerivative[nambuPhi, NambuPropagator[a, b]];
nambuSigmaShortcut = SimplifyDiagrams[nambuSigma - SelfEnergy[nambuPhi, NambuPropagator, {a, b}, "Statistics" -> "Superfield"]];
writeValue["counts Nambu Sigma", orderCounts[nambuSigma, NambuVertex, orders]];
writeOrders["nambu_sigma", nambuSigma, {a, b}, NambuVertex, orders];
writeFormula["nambu_sigma_first_order", OrderPart[nambuSigma, NambuVertex, 1], "Names" -> nambuNames];
(* the stationarity with the two-point source: -2 dGamma/dcalG = Sigma + calG^-1 - calG0^-1 + K *)
writeFormula["nambu_dgamma_first_order", -2 FunctionalDerivative[EffectiveAction2PI[OrderPart[nambuPhi, NambuVertex, 1], BareNambuPropagator -> NambuPropagator,
    "Statistics" -> "Superfield", "Source" -> NambuTwoPointSource], NambuPropagator[a, b]], "Names" -> nambuNames];
writeValue["check Nambu stationarity", SimplifyDiagrams[-2 FunctionalDerivative[EffectiveAction2PI[nambuPhi, BareNambuPropagator -> NambuPropagator,
      "Statistics" -> "Superfield", "Source" -> NambuTwoPointSource], NambuPropagator[a, b]] - nambuSigma -
    (OperatorKernel[Power[NambuPropagator, -1]][a, b] - OperatorKernel[Power[BareNambuPropagator, -1]][a, b] + NambuTwoPointSource[a, b])]];
(* the components: Phi[G, calG_psipsi, calG_psibarpsibar], and the same Phi from a Z0 with pairing terms written by hand *)
nambuRules = {NambuField -> {psi, psibar}, NambuPropagator -> {DressedGreenFunction, AnomalousGreenFunction, AnomalousGreenFunctionBar},
   NambuVertex -> CoulombVertex};
nambuNormalRules = {NambuField -> {psi, psibar}, NambuPropagator -> {DressedGreenFunction, 0, 0}, NambuVertex -> CoulombVertex};
nambuPhiComponents = timed["Nambu Phi components", SuperfieldComponents[nambuPhi, {}, nambuRules]];
writeValue["counts Nambu Phi components", orderCounts[nambuPhiComponents, InteractionPotential, orders]];
writeOrders["nambu_phi_components", nambuPhiComponents, {}, InteractionPotential, orders];
writeFormula["nambu_phi_components_first_order", OrderPart[nambuPhiComponents, InteractionPotential, 1], "Names" -> nambuNames];
nambuZPairing = Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}] +
    1/2 IntegralOver[BareAnomalousGreenFunction[x, y] etabar[x] ** etabar[y], {x, y}] + 1/2 IntegralOver[BareAnomalousGreenFunctionBar[x, y] eta[x] ** eta[y], {x, y}]];
nambuPhiPairing = timed["Phi with pairing terms", LuttingerWardFunctional[nambuZPairing, CoulombVertex, maximumOrder, {BareGreenFunction -> DressedGreenFunction,
     BareAnomalousGreenFunction -> AnomalousGreenFunction, BareAnomalousGreenFunctionBar -> AnomalousGreenFunctionBar}]];
writeValue["check Nambu Phi components", {byOrder[nambuPhiComponents - nambuPhiPairing, InteractionPotential, orders],
   SimplifyDiagrams[SuperfieldComponents[nambuPhi, {}, nambuNormalRules] - luttingerWard]}];
(* first order: Hartree, Fock and the gap function Delta = V calG_psipsi (Hartree-Fock-Bogoliubov, the BCS gap equation) *)
nambuSigma1 = OrderPart[nambuSigma, NambuVertex, 1];
nambuGap1 = SuperfieldComponents[nambuSigma1, {a -> psibar[x1], b -> psibar[x2]}, nambuRules];
writeValue["check Nambu first order", {SimplifyDiagrams[SuperfieldComponents[nambuSigma1, {a -> psibar[x1], b -> psi[x2]}, nambuRules] -
     OrderPart[sigma12, InteractionPotential, 1]], SimplifyDiagrams[nambuGap1 - InteractionPotential[x1, x2] AnomalousGreenFunction[x1, x2]],
   SimplifyDiagrams[SuperfieldComponents[nambuSigma1, {a -> psi[x1], b -> psi[x2]}, nambuRules] - InteractionPotential[x1, x2] AnomalousGreenFunctionBar[x1, x2]]}];
(* the pictures of the equations: GapPicture is the gap function read along the fermion line (a blob without symmetry, so
   that its first label stays on the side of x); every line a full propagator but the bare G0 of the Gor'kov equations *)
DeclareBlob[GapPicture, "\\Delta"]; DeclareBlob[GapPictureAxial, "\\Delta", "AxialPorts" -> {1, 2}];
writeTerms["nambu_gap_lhs", GapPicture[x1, x2], {x1, x2}, "LegDirections" -> {"Out", "Out"}, blobOptions];
writeTerms["nambu_gap", nambuGap1, {x1, x2}, blobOptions];
writeTerms["nambu_sigma_lhs", SigmaBlob[x1, x2], {x1, x2}, "LegDirections" -> {"Out", "In"}, blobOptions];
writeTerms["nambu_hartree_fock", OrderPart[sigma12, InteractionPotential, 1], {x1, x2}, blobOptions];
writeTerms["gorkov_g_lhs", FullG[x, y], {x, y}, blobOptions];
writeTerms["gorkov_g", BareGreenFunction[x, y] + IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] FullG[v, y], {u, v}] +
   IntegralOver[BareGreenFunction[x, u] GapPicture[u, v] AnomalousGreenFunctionBar[v, y], {u, v}], {x, y}, blobOptions];
writeTerms["gorkov_gpp_lhs", AnomalousGreenFunction[x, y], {x, y}, blobOptions];
writeTerms["gorkov_gpp", IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] AnomalousGreenFunction[v, y], {u, v}] -
   IntegralOver[BareGreenFunction[x, u] GapPicture[u, v] FullG[y, v], {u, v}], {x, y}, blobOptions];
(* the Dyson equation of the superfield, calG = calG0 + calG0 Sigma calG, projected: the Gor'kov equations *)
nambuGorkovRules = {NambuField -> {psi, psibar}, NambuPropagator -> {FullG, AnomalousGreenFunction, AnomalousGreenFunctionBar},
   BareNambuPropagator -> {BareGreenFunction, 0, 0}, NambuSelfEnergy -> {{GapFunctionBar, -SigmaBlob[#2, #1] &}, {SigmaBlob, GapFunction}}};
nambuDyson = BareNambuPropagator[a, b] + IntegralOver[BareNambuPropagator[a, u] NambuSelfEnergy[u, v] NambuPropagator[v, b], {u, v}];
writeValue["check Gorkov", {SimplifyDiagrams[SuperfieldComponents[nambuDyson, {a -> psi[x], b -> psibar[y]}, nambuGorkovRules] -
     (BareGreenFunction[x, y] + IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] FullG[v, y], {u, v}] +
      IntegralOver[BareGreenFunction[x, u] GapFunction[u, v] AnomalousGreenFunctionBar[v, y], {u, v}])],
   SimplifyDiagrams[SuperfieldComponents[nambuDyson, {a -> psi[x], b -> psi[y]}, nambuGorkovRules] -
     (IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] AnomalousGreenFunction[v, y], {u, v}] -
      IntegralOver[BareGreenFunction[x, u] GapFunction[u, v] FullG[y, v], {u, v}])]}];
(* the kernels of the three channels from one functional, and the parquet equations of the superfield *)
(* Gamma_s(a, b, c, d) = 4 d2Phi/dcalG(a, b) dcalG(c, d), Gamma_t(a, b, c, d) = -Gamma_s(a, c, b, d), Gamma_u(a, b, c, d) = Gamma_s(a, d, b, c);
   IrreducibleVertex with "Channel" -> All is the shortcut *)
nambuChannels = timed["Nambu channels", <|"s" -> 4 FunctionalDerivative[nambuPhi, NambuPropagator[a, b], NambuPropagator[c, d]],
    "t" -> -4 FunctionalDerivative[nambuPhi, NambuPropagator[a, c], NambuPropagator[b, d]],
    "u" -> 4 FunctionalDerivative[nambuPhi, NambuPropagator[a, d], NambuPropagator[b, c]]|>];
nambuChannelsShortcut = IrreducibleVertex[nambuPhi, NambuPropagator, {a, b, c, d}, "Statistics" -> "Superfield", "Channel" -> All];
writeValue["check Nambu shortcuts", Join[{nambuSigmaShortcut}, Table[SimplifyDiagrams[nambuChannels[r] - nambuChannelsShortcut[r]], {r, {"s", "t", "u"}}]]];
writeValue["counts Nambu kernel", orderCounts[nambuChannels["s"], NambuVertex, orders]];
writeOrders["nambu_kernel", nambuChannels["s"], {a, b, c, d}, NambuVertex, orders];
nambuFull = timed["Nambu F", PerturbativeFullVertex[nambuZ0, nambuInteraction, maximumOrder, {NambuField}, {a, b, c, d},
    BareNambuPropagator -> NambuPropagator]];
writeValue["counts Nambu F", orderCounts[nambuFull, NambuVertex, orders]];
nambuParquet = ParquetDecomposition[nambuFull, {a, b, c, d}, {NambuPropagator}, "Statistics" -> "Superfield"];
writeValue["check Nambu channels", Table[SimplifyDiagrams[nambuFull - nambuParquet[r] - nambuChannels[r]], {r, {"s", "t", "u"}}]];
nambuParquetCheck = timed["Nambu parquet check", CheckParquetEquations[nambuFull, {a, b, c, d}, NambuPropagator, NambuVertex, maximumOrder,
    "Statistics" -> "Superfield"]];
writeValue["parquet residuals Nambu", nambuParquetCheck["Residuals"]];
writeValue["counts Nambu parquet", nambuParquetCheck["DiagramCounts"]];
(* phi^4: the kernels of s, t, u from Gamma2[G] (Eckhardt, Kappl, Kauch and Held) as derivatives, Gamma_s = 4 d^2 Gamma2/dG_ab dG_cd,
   Gamma_t and Gamma_u with b, c or b, d exchanged; Gamma^(4) - R_r; IrreducibleVertex with "Channel" -> All is the shortcut (checked) *)
phi4KernelS = 4 FunctionalDerivative[phi4Gamma2, DressedPropagator[a, b], DressedPropagator[c, d]];
phi4Kernels = <|"s" -> phi4KernelS, "t" -> (phi4KernelS /. {b -> c, c -> b}), "u" -> (phi4KernelS /. {b -> d, d -> b})|>;
writeValue["check phi4 channels", Table[SimplifyDiagrams[vertexPhi4 - phi4Channels[r] - phi4Kernels[r]], {r, {"s", "t", "u"}}]];
phi4KernelsShortcut = IrreducibleVertex[phi4Gamma2, DressedPropagator, {a, b, c, d}, "Statistics" -> "Boson", "Channel" -> All];
writeValue["check phi4 channel derivatives", Table[SimplifyDiagrams[phi4Kernels[r] - phi4KernelsShortcut[r]], {r, {"s", "t", "u"}}]];
(* the components in the normal state: s -> ph (Gamma0), t -> pp, u -> phbar *)
nambuLegs = {a -> psibar[x1], b -> psi[x2], c -> psibar[x3], d -> psi[x4]};
(* the pp kernel is the derivative with respect to the anomalous blocks, -4 d^2 Phi/dcalG_psibarpsibar(x1, x3) dcalG_psipsi(x2, x4) at zero
   anomalous propagators; the component of Gamma_t is the same (checked below) *)
ppKernel = timed["pp kernel", SimplifyDiagrams[-4 FunctionalDerivative[nambuPhiComponents, AnomalousGreenFunction[x2, x4],
       AnomalousGreenFunctionBar[x1, x3]] /. {AnomalousGreenFunction[__] -> 0, AnomalousGreenFunctionBar[__] -> 0}]];
writeValue["counts pp kernel", orderCounts[ppKernel, InteractionPotential, orders]];
writeValue["check Nambu kernel components", {SimplifyDiagrams[SuperfieldComponents[nambuChannels["s"], nambuLegs, nambuNormalRules] - irreducibleVertex],
   SimplifyDiagrams[ppKernel - (fullVertex - fullVertexChannels["ParticleParticle"])],
   SimplifyDiagrams[SuperfieldComponents[nambuChannels["u"], nambuLegs, nambuNormalRules] - (fullVertex - fullVertexChannels["TransverseParticleHole"])]}];
writeValue["check pp kernel from Phi", SimplifyDiagrams[SuperfieldComponents[nambuChannels["t"], nambuLegs, nambuNormalRules] - ppKernel]];
writeValue["check Bethe-Salpeter pp", SimplifyDiagrams[fullVertex - ppKernel - BetheSalpeterLadder[ppKernel, fullVertex, {x1, x2, x3, x4},
     "ParticleParticle", DressedGreenFunction, "MaximumOrder" -> {InteractionPotential, maximumOrder}]]];
(* the pp kernel: the bare vertex, the diagrams reducible in ph and in phbar *)
writeOrders["pp_kernel", ppKernel, {x1, x2, x3, x4}, InteractionPotential, orders];
(* the linearized gap equation (Thouless): Delta(x1, x3) = -2 dPhi/dcalG_psibarpsibar(x1, x3), linear in calG_psipsi = -G Delta G^T, is
   the homogeneous Bethe-Salpeter equation of the pp channel, Delta = -1/2 Gamma_pp G G Delta *)
thoulessLinear = With[{p = Unique["p"], q = Unique["p"], s = Unique["p"], t = Unique["p"]},
   SimplifyDiagrams[(-2 FunctionalDerivative[nambuPhiComponents, AnomalousGreenFunctionBar[x1, x3]] /. AnomalousGreenFunctionBar[__] -> 0) /.
     AnomalousGreenFunction -> LabelFunction[-IntegralOver[DressedGreenFunction[p, s] GapFunction[s, t] DressedGreenFunction[q, t], {s, t}], {p, q}]]];
thoulessHomogeneous = With[{p5 = Unique["p"], p6 = Unique["p"], p7 = Unique["p"], p8 = Unique["p"]},
   SimplifyDiagrams[-1/2 IntegralOver[(ppKernel /. {x2 -> p5, x4 -> p6}) DressedGreenFunction[p5, p7] DressedGreenFunction[p6, p8] GapFunction[p7, p8],
      {p5, p6, p7, p8}]]];
writeValue["check Thouless", byOrder[thoulessLinear - thoulessHomogeneous, InteractionPotential, orders]];
writeValue["counts Thouless", orderCounts[thoulessLinear, InteractionPotential, orders]];
(* drawn with the particles from left to right, as the pp term of the parquet equation *)
writeTerms["thouless_lhs", GapPicture[x1, x3], {x1, x3}, "LegDirections" -> {"Out", "Out"}, blobOptions];
writeTerms["thouless", With[{p5 = Unique["p"], p6 = Unique["p"], p7 = Unique["p"], p8 = Unique["p"]},
   -1/2 IntegralOver[IrreducibleBoxPp[p5, p6, x3, x1] DressedGreenFunction[p5, p7] DressedGreenFunction[p6, p8] GapPictureAxial[p7, p8], {p5, p6, p7, p8}]],
  {x1, x3}, "LegDirections" -> {"Out", "Out"}, blobOptions];
(* the means X = (Psi, Psibar): Gamma[X, calG] at its stationary calG (the first order is enough to the third) is the 1PI action
   Gamma[X] with bare lines, and its components are Gamma[Psibar, Psi] of the 1PI section, the anomalous diagrams included *)
nambuGammaMeans = timed["Nambu Gamma[X, calG]", EffectiveAction2PI[nambuZ0, nambuInteraction, maximumOrder, BareNambuPropagator -> NambuPropagator,
    "Statistics" -> "Superfield", "MeanFields" -> {NambuField -> NambuMean}]];
writeFormula["nambu_gamma_means", OrderPart[nambuGammaMeans, NambuVertex, {0, 1}], "Names" -> nambuNames];
(* S''[X] = calG0^-1 - Sigma1 with -X X in place of calG: every vertex of degree two, normal and anomalous, in the trace term *)
writeValue["check Nambu fluctuation operator", SimplifyDiagrams[FunctionalDerivative[nambuInteraction /. NambuField -> NambuMean, NambuMean[a], NambuMean[b]] +
    (OrderPart[nambuSigma, NambuVertex, 1] /. NambuPropagator[p_, q_] :> -GrassmannProduct[NambuMean[p], NambuMean[q]])]];
nambuSigmaMeans1 = OrderPart[SelfEnergy[nambuGammaMeans, NambuPropagator, {a, b}, "Statistics" -> "Superfield"], NambuVertex, 1] /.
   NambuPropagator -> BareNambuPropagator;
nambuGammaStationary = timed["Nambu Gamma[X, calG*]", SimplifyDiagrams[Sum[ExpandTrace[OrderPart[nambuGammaMeans, NambuVertex, m] /.
       NambuPropagator -> Power[Power[BareNambuPropagator, -1] - LabelFunction[nambuSigmaMeans1, {a, b}], -1], BareNambuPropagator, maximumOrder - m,
      "MaximumOrder" -> {NambuVertex, maximumOrder}], {m, 0, maximumOrder}]]];
nambuOnePI = timed["Nambu Gamma[X]", EffectiveAction1PI[nambuZ0, nambuInteraction, maximumOrder, NambuField -> NambuMean,
    BareNambuPropagator -> BareNambuPropagator, "Statistics" -> "Superfield"]];
writeValue["counts Nambu 1PI", orderCounts[nambuOnePI, NambuVertex, orders]];
writeValue["check Nambu stationary means", byOrder[nambuGammaStationary - nambuOnePI, NambuVertex, Range[0, maximumOrder]]];
nambuOnePIComponents = timed["Nambu Gamma[X] components", SuperfieldComponents[nambuOnePI, {}, {NambuField -> {psi, psibar}, NambuMean -> {MeanPsi, MeanPsibar},
     BareNambuPropagator -> {BareGreenFunction, 0, 0}, NambuVertex -> CoulombVertex}]];
writeValue["check Nambu 1PI components", byOrder[nambuOnePIComponents - gammaPsi1PI, InteractionPotential, Range[0, maximumOrder]]];
(* Gamma[X] = S[X] - 1/2 Tr ln(-S''[X]) + Gamma1[X] (the 1PI sections): Gamma1[X] is minus the 1PI vacuum diagrams with lines S''[X]^-1
   (head NambuBackground) and the vertices of degree three or more, computed by the private meanIrreducibleVacuum, the routine of
   EffectiveAction1PI for vertices with Grassmann-odd means. In components, the one loop of Gamma[Psibar, Psi] (the diagrams with
   one loop of the electron gas) is the rings of -1/2 Tr ln(-S''[Psibar, Psi]) + Tr ln(-G0^-1), and its diagrams with two or more
   loops are Gamma1 with its lines expanded in G0. At calG = S''[X]^-1, Gamma2[X, calG] is the part of Gamma1[X] 2PI in these lines. *)
DeclareAntisymmetric[NambuBackground];
nambuInsertion = SimplifyDiagrams[FunctionalDerivative[nambuInteraction /. NambuField -> NambuMean, NambuMean[a], NambuMean[b]]];
nambuFluctuation = Power[BareNambuPropagator, -1] + LabelFunction[nambuInsertion, {a, b}];
nambuGammaOne = timed["Nambu Gamma1[X]", -FunctionalDiagrams`Private`meanIrreducibleVacuum[nambuZ0 /. BareNambuPropagator -> NambuBackground,
     Total[Drop[FunctionalDiagrams`Private`shiftedInteractionParts[nambuInteraction, {NambuField -> NambuMean}], 3]], orders, {NambuMean}, 1,
     NambuBackground]];
psiComponents[e_] := SuperfieldComponents[e, {}, {NambuField -> {psi, psibar}, NambuMean -> {MeanPsi, MeanPsibar},
    BareNambuPropagator -> {BareGreenFunction, 0, 0}, NambuVertex -> CoulombVertex}];
writeValue["check 1PI psi pieces", timed["Nambu Gamma1[X] components", {
    byOrder[psiComponents[ExpandTrace[-1/2 TraceLog[-nambuFluctuation], BareNambuPropagator, maximumOrder] +
        1/2 TraceLog[-Power[BareNambuPropagator, -1]]] - gammaPsi1PIPieces[1], InteractionPotential, orders],
    byOrder[psiComponents[ExpandTrace[nambuGammaOne /. NambuBackground -> Power[nambuFluctuation, -1], BareNambuPropagator, maximumOrder,
         "MaximumOrder" -> {NambuVertex, maximumOrder}]] - Total[KeyDrop[gammaPsi1PIPieces, {0, 1}]], InteractionPotential, orders]}]];
nambuGamma2Means = SimplifyDiagrams[nambuGammaMeans + 1/2 IntegralOver[OperatorKernel[Power[BareNambuPropagator, -1]][u, w] NambuMean[u] ** NambuMean[w], {u, w}] -
    (nambuInteraction /. NambuField -> NambuMean) + 1/2 TraceLog[-Power[NambuPropagator, -1]] + 1/2 OperatorTrace[Power[BareNambuPropagator, -1] . NambuPropagator - 1] +
    1/2 IntegralOver[(nambuInsertion /. {a -> u, b -> w}) NambuPropagator[w, u], {u, w}]];
writeValue["check Nambu gamma two", byOrder[(nambuGamma2Means /. NambuPropagator -> NambuBackground) -
    ParticleIrreducibleDiagrams[nambuGammaOne, 2, {}, {NambuBackground}], NambuVertex, orders]];
writeValue["check Nambu mean derivatives", {byOrder[SetSourcesToZero[FunctionalDerivative[nambuGammaMeans, NambuMean[a], NambuMean[b]]] -
       OperatorKernel[Power[BareNambuPropagator, -1]][a, b] + nambuSigma, NambuVertex, Range[0, maximumOrder]],
   SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[nambuGammaMeans, NambuMean[a], NambuMean[b], NambuMean[c], NambuMean[d]]] - NambuVertex[a, b, c, d]]}];
(* one site with spin 1/2 and pairing: the superfield (psiUp, psiDn, psibarUp, psibarDn), S_int = u n_up n_dn, the pairing sources
   k, kbar; Z = a^2 + k kbar - u; calG with g per spin and the anomalous kappa, kappabar; Phi(g, kappa, kappabar) is Phi(g) with g^2 -> g^2 - kappa kappabar *)
siteNambuG = {{0, kappa0n, g0n, 0}, {-kappa0n, 0, 0, g0n}, {-g0n, 0, 0, kappab0n}, {0, -g0n, -kappab0n, 0}};
siteNambuValues = {NambuPropagator -> siteNambuG, BareNambuPropagator -> (siteNambuG /. {kappa0n -> 0, kappab0n -> 0, g0n -> 1/a0n}),
   NambuTwoPointSource -> {{0, -kb0n, 0, 0}, {kb0n, 0, 0, 0}, {0, 0, 0, k0n}, {0, 0, -k0n, 0}},
   NambuVertex -> Table[If[Sort[{i, j, l, m}] === {1, 2, 3, 4}, -u0n Signature[{i, j, l, m}], 0], {i, 4}, {j, 4}, {l, 4}, {m, 4}]};
siteNambuRoot = Sqrt[1 + 4 u0n (g0n^2 - kappa0n kappab0n)];
siteNambuZ = a0n^2 + k0n kb0n - u0n;
siteNambuGamma = timed["Nambu one site", ZeroDimensionalValue[EffectiveAction2PI[nambuPhi, BareNambuPropagator -> NambuPropagator, "Statistics" -> "Superfield",
     "Source" -> NambuTwoPointSource], siteNambuValues]];
siteNambuExact = {g0n -> a0n/siteNambuZ, kappa0n -> -k0n/siteNambuZ, kappab0n -> kb0n/siteNambuZ};
writeValue["check Nambu zero dimensions", {
   Simplify[ZeroDimensionalValue[nambuPhi, siteNambuValues] - Normal[Series[siteNambuRoot - 1 - Log[(1 + siteNambuRoot)/2], {u0n, 0, maximumOrder}]]],
   Simplify[D[ZeroDimensionalValue[-1/2 TraceLog[-Power[NambuPropagator, -1]] - 1/2 OperatorTrace[Dot[Power[BareNambuPropagator, -1], NambuPropagator] - 1],
        siteNambuValues] - (Log[g0n^2 - kappa0n kappab0n] - 2 (a0n g0n - 1)), #] & /@ {g0n, kappa0n, kappab0n, a0n}],
   Simplify[Normal[Series[(D[siteNambuGamma, #] & /@ {g0n, kappa0n, kappab0n}) /. siteNambuExact, {u0n, 0, maximumOrder}]]],
   Chop[N[Normal[Series[(siteNambuGamma /. siteNambuExact) + Log[siteNambuZ] /. {a0n -> 2, k0n -> 1/3, kb0n -> 1/5}, {u0n, 0, maximumOrder}]] /. u0n -> 1/10, 30]]}];
writeValue["seconds Nambu", N[Round[10 (AbsoluteTime[] - nambuStart)]/10]];

(* the grand potential in momentum and Matsubara frequency, per unit of beta times the volume *)
eaHFRules = {BareGreenFunction -> {"Fermion", xi}, DressedGreenFunction -> {"Fermion", epsHF}, InteractionPotential -> {"Instantaneous", v}};
eaPhi1 = OrderPart[luttingerWard, InteractionPotential, 1];
eaOmegaFree = Table[EvaluateMatsubara[EffectiveAction2PI[0, BareGreenFunction -> DressedGreenFunction], {}, gasRules, "SpinDegeneracy" -> 2,
    "LoopMomenta" -> {p}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
Do[writeFormula["ea_omega_free_" <> ToString[i], eaOmegaFree[[i]], "Subscripts" -> {xi}], {i, 3}];
eaOmegaHF = EvaluateMatsubara[EffectiveAction2PI[eaPhi1, BareGreenFunction -> DressedGreenFunction], {}, eaHFRules, "SpinDegeneracy" -> 2,
   "LoopMomenta" -> {p1, p2}];
writeFormula["ea_omega_hf", eaOmegaHF, "Subscripts" -> {xi, epsHF, v}, "Names" -> {epsHF -> "\\epsilon"}];
eaPhi4Rules = {FreePropagator -> {"Boson", Function[{s, zz}, 1/(omB[s]^2 - zz^2)]}, DressedPropagator -> {"Boson", Function[{s, zz}, 1/(massH[s]^2 - zz^2)]}};
writeFormula["ea_omega_free_bosons", EvaluateMatsubara[EffectiveAction2PI[0, FreePropagator -> DressedPropagator, "Statistics" -> "Boson"], {},
   {FreePropagator -> {"Boson", Function[{s, zz}, 1/(omB[s]^2 - zz^2)]}, DressedPropagator -> {"Boson", Function[{s, zz}, 1/(omB[s]^2 - zz^2)]}},
   "LoopMomenta" -> {p}], "Subscripts" -> {omB}, "Names" -> {omB -> "\\omega"}];
writeFormula["ea_omega_hartree", EvaluateMatsubara[EffectiveAction2PI[OrderPart[phi4Gamma2, lambda, 1], FreePropagator -> DressedPropagator,
    "Statistics" -> "Boson"], {}, eaPhi4Rules, "LoopMomenta" -> {p1, p2}], "Subscripts" -> {omB, massH}, "Names" -> {omB -> "\\omega", massH -> "M"}];
(* expectation values as variations: a one-body source U in G0^-1 *)
DeclareDirectedPropagator[OneBodySource]; DeclareSymmetric[OneBodySourceB];
writeFormula["ea_dgamma_du", FunctionalDerivative[EffectiveAction2PI[eaPhi1, BareGreenFunction -> DressedGreenFunction, "Source" -> OneBodySource],
   OneBodySource[x, y]], "Names" -> eaNames];
writeFormula["ea_dgamma_du_phi4", FunctionalDerivative[EffectiveAction2PI[OrderPart[phi4Gamma2, lambda, 1], FreePropagator -> DressedPropagator,
    "Statistics" -> "Boson", "Source" -> OneBodySourceB], OneBodySourceB[x, y]], "Names" -> eaNames];
(* the response L = L0 + L0 Gamma0 L and chi = gamma L gamma, drawn with blobs *)
DeclareSymmetric[KernelBoxB]; DeclareBlob[KernelBoxB, "\\Gamma^0", "Shape" -> "Box"];
(* as it stands (Gamma0 and L share two points: joined by short links), and iterated twice, L0 + L0 Gamma0 L0 +
   L0 Gamma0 L0 Gamma0 L0, by the package's equation itself *)
writeTerms["ea_response_bs", Last[ExactEquation["Response", {"L" -> LBox, "Gamma0" -> KernelBox, "G" -> DressedGreenFunction}, fourLabels]],
  fourLabels, "LegDirections" -> "Alternating", blobOptions];
writeTerms["ea_response_bs_phi4", Last[ExactEquation["Response", {"L" -> LBox, "Gamma0" -> KernelBoxB, "G" -> DressedPropagator}, fourLabels,
    "Statistics" -> "Boson"]], fourLabels, blobOptions];
eaIterated[gamma0_, g_, l0_, opts___] := Nest[SimplifyDiagrams[ExactEquation["Response", {"L" -> LabelFunction[#, fourLabels],
       "Gamma0" -> gamma0, "G" -> g}, fourLabels, opts][[2]]] &, l0, 2];
writeTerms["ea_response_equation", eaIterated[KernelBox, DressedGreenFunction, DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2]],
  fourLabels, "LegDirections" -> "Alternating", blobOptions];
writeTerms["ea_response_equation_phi4", eaIterated[KernelBoxB, DressedPropagator, DressedPropagator[x1, x3] DressedPropagator[x2, x4] +
    DressedPropagator[x1, x4] DressedPropagator[x2, x3], "Statistics" -> "Boson"], fourLabels, blobOptions];
writeTerms["ea_susceptibility", Last[ExactEquation["Susceptibility", {"Chi" -> ChiBlob, "L" -> LBox}, {z1, z2}]], {z1, z2}, blobOptions];
(* time-dependent Hartree-Fock: the response of the first-order Phi, with its own kernel Gamma0 = d^2 Phi/dG(x4, x3) dG(x2, x1),
   order by order; IrreducibleVertex is the shortcut (checked) *)
eaGamma0HF = LabelFunction[SimplifyDiagrams[FunctionalDerivative[eaPhi1, DressedGreenFunction[x4, x3], DressedGreenFunction[x2, x1]]], fourLabels];
writeValue["check EA TDHF kernel shortcut", SimplifyDiagrams[IrreducibleVertex[eaPhi1, DressedGreenFunction, fourLabels] - eaGamma0HF @@ fourLabels]];
eaNextL[l_] := ExactEquation["Response", {"L" -> LabelFunction[l, fourLabels], "Gamma0" -> eaGamma0HF, "G" -> DressedGreenFunction}, fourLabels][[2]];
eaLTDHF = NestList[eaNextL, DressedGreenFunction[x1, x4] DressedGreenFunction[x3, x2], 2];
eaChiTDHF = Table[ExactEquation["Susceptibility", {"Chi" -> ChiBlob, "L" -> LabelFunction[l, fourLabels]}, {z1, z2}][[2]], {l, eaLTDHF}];
eaChiTDHFOrders = Prepend[Table[SimplifyDiagrams[eaChiTDHF[[i + 1]] - eaChiTDHF[[i]]], {i, 2}], eaChiTDHF[[1]]];
writeOrders["ea_chi_tdhf", Last[eaChiTDHF], {z1, z2}, InteractionPotential, {0, 1, 2}];
eaBubble = {z2 -> {q, I nu}, z1 -> {-q, -I nu}};
writeFormula["ea_chi_tdhf_static", EvaluateMatsubara[eaChiTDHFOrders[[1]] + eaChiTDHFOrders[[2]], eaBubble, eaHFRules, "SpinDegeneracy" -> 2,
   "LoopMomenta" -> {p1, p2}, "Limit" -> "Static"], "Subscripts" -> {epsHF, v}, "Names" -> {epsHF -> "\\epsilon"}];
writeValue["check EA TDHF dynamic limit", Table[EvaluateMatsubara[c, eaBubble, eaHFRules, "SpinDegeneracy" -> 2, "Limit" -> "Dynamic"], {c, eaChiTDHFOrders}]];
writeFormula["ea_lindhard_static", EvaluateMatsubara[eaChiTDHFOrders[[1]] /. DressedGreenFunction -> BareGreenFunction, eaBubble, gasRules,
   "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}, "Limit" -> "Static"], "Subscripts" -> {xi}];
(* phi^4, rho = phi^2/2: the Hartree response, chains of bubbles; phi^2 is not conserved. The kernel of real bosons,
   Gamma0 = 4 d^2 Gamma2/dG(x3, x4) dG(x1, x2); IrreducibleVertex is the shortcut (checked) *)
eaGamma0H = LabelFunction[SimplifyDiagrams[4 FunctionalDerivative[OrderPart[phi4Gamma2, lambda, 1], DressedPropagator[x3, x4],
     DressedPropagator[x1, x2]]], fourLabels];
writeValue["check EA Hartree kernel shortcut", SimplifyDiagrams[IrreducibleVertex[OrderPart[phi4Gamma2, lambda, 1], DressedPropagator, fourLabels,
     "Statistics" -> "Boson"] - eaGamma0H @@ fourLabels]];
eaNextLB[l_] := ExactEquation["Response", {"L" -> LabelFunction[l, fourLabels], "Gamma0" -> eaGamma0H, "G" -> DressedPropagator}, fourLabels,
    "Statistics" -> "Boson"][[2]];
eaLH = NestList[eaNextLB, DressedPropagator[x1, x3] DressedPropagator[x2, x4] + DressedPropagator[x1, x4] DressedPropagator[x2, x3], 2];
eaChiH = Last[ExactEquation["Susceptibility", {"Chi" -> ChiBlob, "L" -> LabelFunction[Last[eaLH], fourLabels]}, {z1, z2}, "Statistics" -> "Boson"]];
writeTerms["ea_chi_hartree", eaChiH, {z1, z2}];
writeFormula["ea_chi_hartree_static", EvaluateMatsubara[OrderPart[eaChiH, lambda, 0], eaBubble, eaPhi4Rules, "Limit" -> "Static"],
  "Subscripts" -> {massH}, "Names" -> {massH -> "M"}];
writeFormula["ea_chi_hartree_dynamic", EvaluateMatsubara[OrderPart[eaChiH, lambda, 0], eaBubble, eaPhi4Rules, "Limit" -> "Dynamic"],
  "Subscripts" -> {massH}, "Names" -> {massH -> "M"}];
(* numbers on a ring of 8 sites: xi = -2 cos k - mu, v(q) = 0.6 + 0.5 cos q, T = 0.25, mu = 0.3; the self-consistent
   Hartree-Fock energies by iteration; -dOmega/dmu = N with the three orderings; d^2 Omega/dmu^2 against the static TDHF
   response summed to all orders (a linear system) and against the bubble of Hartree-Fock lines alone *)
eaOmegaHFAll = Table[EvaluateMatsubara[EffectiveAction2PI[eaPhi1, BareGreenFunction -> DressedGreenFunction], {}, eaHFRules, "SpinDegeneracy" -> 2,
    "LoopMomenta" -> {p1, p2}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
eaDensityHF = Table[EvaluateMatsubara[OperatorTrace[DressedGreenFunction], {}, eaHFRules, "SpinDegeneracy" -> 2, "LoopMomenta" -> {p1},
    "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
eaSigmaHF = Table[EvaluateMatsubara[FunctionalDerivative[eaPhi1, DressedGreenFunction[y, x]], twoPoint, eaHFRules, "SpinDegeneracy" -> 2,
    "LoopMomenta" -> {p1}, "ConvergenceFactor" -> eta], {eta, {1, -1, 0}}];
ringM = 8; ringT = 1/4; ringMomenta = N[2 Pi Range[0, ringM - 1]/ringM]; ringMu = 0.3;
ringRules[mu_, ens_] := {xi -> Function[s, -2 Cos[s] - mu], v -> Function[s, 0.6 + 0.5 Cos[s]],
   epsHF -> Function[s, ens[[1 + Mod[Round[s ringM/(2 Pi)], ringM]]]], Temperature -> ringT,
   FermiFunction -> Function[e, 1/(Exp[e/ringT] + 1)], BoseFunction -> Function[e, 1/(Exp[e/ringT] - 1)]};
ringValue[e_, mu_, ens_] := Chop[N[e /. MomentumSum[f_, ps_] :> Total[Table[f /. Thread[ps -> t], {t, Tuples[ringMomenta, Length[ps]]}]]/
        ringM^Length[ps] /. ringRules[mu, ens]]];
ringHF[mu_, i_] := ringHF[mu, i] = Module[{ens = -2 Cos[ringMomenta] - mu, new, n = 0},
   While[n < 400, new = Table[-2 Cos[kk] - mu + ringValue[eaSigmaHF[[i]] /. k -> kk, mu, ens], {kk, ringMomenta}];
    If[Max[Abs[new - ens]] < 10^-14, ens = new; Break[]]; ens = 1/2 (ens + new); n++];
   ens];
ringOmega[mu_, i_] := ringValue[eaOmegaHFAll[[i]], mu, ringHF[mu, i]];
ringDensity[mu_, i_] := ringValue[eaDensityHF[[i]], mu, ringHF[mu, i]];
ringD1[f_, x0_, h_] := (-f[x0 + 2 h] + 8 f[x0 + h] - 8 f[x0 - h] + f[x0 - 2 h])/(12 h);
ringD2[f_, x0_, h_] := (-f[x0 + 2 h] + 16 f[x0 + h] - 30 f[x0] + 16 f[x0 - h] - f[x0 - 2 h])/(12 h^2);
writeValue["check EA ring density", Table[Chop[ringD1[ringOmega[#, i] &, ringMu, 10^-3] + ringDensity[ringMu, i], 10^-8], {i, 3}]];
ringNPrime = Table[ringValue[FermiFunction'[epsHF[kk]], ringMu, ringHF[ringMu, 1]], {kk, ringMomenta}];
ringKernel = Table[ringValue[2 v[0] - v[kk - pp], ringMu, ringHF[ringMu, 1]], {kk, ringMomenta}, {pp, ringMomenta}] . DiagonalMatrix[ringNPrime]/ringM;
ringChiTDHF = 2 ringNPrime . LinearSolve[IdentityMatrix[ringM] - ringKernel, ConstantArray[1., ringM]]/ringM;
ringOmega2 = ringD2[ringOmega[#, 1] &, ringMu, 10^-3];
writeValue["EA ring compressibility", ToString[NumberForm[ringOmega2, {6, 5}]]];
writeValue["EA ring TDHF static", ToString[NumberForm[ringChiTDHF, {6, 5}]]];
writeValue["EA ring bubble alone", ToString[NumberForm[2 Total[ringNPrime]/ringM, {5, 4}]]];
writeValue["check EA compressibility sum rule", Chop[ringOmega2 - ringChiTDHF, 10^-6]];

(* Potthoff: the universal functional F[Sigma] and the self-energy functional *)
DeclareDirectedPropagator[TrialSelfEnergy];
eaOmegaSigma = SelfEnergyFunctional[luttingerWard, BareGreenFunction -> TrialSelfEnergy, DressedGreenFunction];
writeFormula["ea_self_energy_functional", eaOmegaSigma, "Names" -> eaNames];
writeFormula["ea_self_energy_functional_derivative", FunctionalDerivative[eaOmegaSigma, TrialSelfEnergy[x, y]], "Names" -> eaNames];
writeFormula["ea_legendre_v", FunctionalDerivative[LegendreTransform[eaPhi1, DressedGreenFunction -> TrialSelfEnergy], InteractionPotential[x, y]],
  "Names" -> eaNames];
writeValue["check EA universality", If[FreeQ[LegendreTransform[luttingerWard, DressedGreenFunction -> TrialSelfEnergy], BareGreenFunction] &&
    FreeQ[phi4Gamma2, FreePropagator], 0, 1]];

(* zero dimensions: one level with spin 1/2, Z = a^2 - u, G0 = 1/a: G = a/(a^2 - u), Sigma = u/a; the skeleton series of
   Sigma[g] (Catalan numbers) is one branch of (-1 +- Sqrt[1 + 4 u g^2])/(2 g) *)
eaZeroD[e_] := ZeroDimensionalValue[e, {BareGreenFunction -> 1/a0d, InteractionPotential -> u0d, DressedGreenFunction -> g0d}, "LoopFactor" -> 2];
eaPhi4th = timed["EA Phi to fourth order", LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 4, BareGreenFunction -> DressedGreenFunction]];
eaSigma0d = eaZeroD[SelfEnergy[eaPhi4th, DressedGreenFunction, {x, y}]];
writeFormula["ea_sigma_0d", eaSigma0d, "Names" -> {u0d -> "u", g0d -> "g"}];
writeValue["check EA 0d Catalan", Expand[eaSigma0d - Normal[Series[(Sqrt[1 + 4 u0d g0d^2] - 1)/(2 g0d), {u0d, 0, 4}]]]];
writeValue["check EA 0d value", Normal[Series[(eaZeroD[EffectiveAction2PI[eaPhi4th, BareGreenFunction -> DressedGreenFunction]] /.
       g0d -> a0d/(a0d^2 - u0d)) + Log[a0d^2 - u0d], {u0d, 0, 4}]] // Simplify];
(* Potthoff in zero dimensions: F(Sigma) = log(1 - Sigma^2/u) for every a; Omega_a(Sigma) = F - log((a - Sigma)^2) stationary only at
   Sigma = u/a, with the value -log(a^2 - u) *)
eaF0d = Log[1 - s0d^2/u0d]; eaOmega0d = eaF0d - Log[(a0d - s0d)^2];
(* F from the package's Phi: Sigma = u sig, g[Sigma] = sig/(1 - u sig^2) as a series in u *)
eaFFromPhi = ZeroDimensionalValue[LegendreTransform[eaPhi4th, DressedGreenFunction -> TrialSelfEnergy], {DressedGreenFunction -> sig0d Sum[(u0d sig0d^2)^kk,
     {kk, 0, 4}], TrialSelfEnergy -> u0d sig0d, InteractionPotential -> u0d}, "LoopFactor" -> 2];
writeValue["check EA 0d Potthoff", {Chop[N[{D[eaOmega0d, s0d] /. s0d -> u0d/a0d, (eaOmega0d /. s0d -> u0d/a0d) + Log[a0d^2 - u0d]} /.
      {a0d -> 3, u0d -> 2}]], Simplify[Normal[Series[eaFFromPhi + Sum[(u0d sig0d^2)^kk/kk, {kk, 1, 4}], {u0d, 0, 4}]]]}];

(* the divergence of the irreducible vertex: one site at a = i nu. Z = a1 a2 - u, g_s = d log Z/d a_s, Sigma_s = a_s - 1/g_s;
   Gamma0 = dSigma/dg = L0^-1 - L^-1 with L = -dg/da and L0 = g^2; charge and spin channels *)
zSite = a1 a2 - u0d;
gSite = {D[Log[zSite], a1], D[Log[zSite], a2]};
sigmaSite = Simplify[{a1, a2} - 1/gSite];
jacSite = D[gSite, {{a1, a2}}];
gamma0Site = Simplify[D[sigmaSite, {{a1, a2}}] . Inverse[jacSite]];
siteChannels[mat_] := Simplify[{mat[[1, 1]] + mat[[1, 2]], mat[[1, 1]] - mat[[1, 2]]} /. {a1 -> a0d, a2 -> a0d}];
writeValue["check EA 0d inverse response", Simplify[gamma0Site - (DiagonalMatrix[1/gSite^2] + Inverse[jacSite])]];
writeFormula["ea_gamma0_charge", Simplify[siteChannels[gamma0Site][[1]] /. a0d -> I nu0d], "Names" -> {u0d -> "u", nu0d -> "\\nu"}];
writeFormula["ea_gamma0_spin", Simplify[siteChannels[gamma0Site][[2]] /. a0d -> I nu0d], "Names" -> {u0d -> "u", nu0d -> "\\nu"}];
writeFormula["ea_l_charge", Simplify[-siteChannels[jacSite][[1]] /. a0d -> I nu0d], "Names" -> {u0d -> "u", nu0d -> "\\nu"}];
writeValue["check EA 0d charge vertex", Normal[Series[(D[eaSigma0d, g0d] /. g0d -> a0d/(a0d^2 - u0d)) -
      siteChannels[gamma0Site][[1]], {u0d, 0, 3}]] // Simplify];
eaBranch[sgn_, gg_] := (-1 + sgn Sqrt[1 + 4 u0d gg^2])/(2 gg);
eaCatalan[gg_, uu_, kk_] := Sum[(-1)^j Binomial[2 j, j]/(j + 1) uu^(j + 1) gg^(2 j + 1), {j, 0, kk}];
writeValue["check EA 0d branch point", Simplify[1 + 4 u0d (I nu0d/(-nu0d^2 - u0d))^2 - ((nu0d^2 - u0d)/(nu0d^2 + u0d))^2]];
writeValue["check EA 0d wrong branch", With[{gg = I/(-1 - 2)},
   {Chop[N[eaCatalan[gg, 2, 200] - (eaBranch[1, gg] /. u0d -> 2), 20], 10^-10], If[Abs[N[eaCatalan[gg, 2, 200]] - (-2 I)] > 0.9, 0, 1]}]];
writeValue["check EA 0d no solution", Length[Solve[(g0d (I - eaBranch[1, g0d]) == 1) /. u0d -> 2, g0d]]];
(* the figure: -Im Sigma at nu = 1 (physical u, + branch min(u, 1), - branch max(u, 1), the skeleton series at the exact g)
   and the charge and spin vertices *)
eaCoordinates[pts_] := StringJoin[(" (" <> ToString[NumberForm[#[[1]], {4, 3}]] <> "," <> ToString[NumberForm[#[[2]], {5, 3}]] <> ")" &) /@ N[pts]];
eaClipped[f_] := Select[Table[{uu, f[uu]}, {uu, DeleteCases[Range[0, 3, 1/100], 1]}], Abs[#[[2]]] <= 30 &];
eaSeriesDots = Table[{uu, -Im[N[eaCatalan[-I/(1 + uu), uu, 60]]]}, {uu, DeleteCases[Range[1/5, 3, 1/5], 1]}];
Export["figures/vertex_divergence_plot.tex", StringJoin[
   "\\begin{tikzpicture}[x=1.45cm,y=0.95cm,font=\\footnotesize]\n",
   "\\draw[->] (0,0) -- (3.25,0) node[right] {$u/\\nu^2$}; \\draw[->] (0,0) -- (0,3.3) node[above] {$-\\mathrm{Im}\\,\\Sigma/\\nu$};\n",
   "\\foreach \\t in {1,2,3} {\\draw (\\t,0.05) -- (\\t,-0.05) node[below] {\\t}; \\draw (0.05,\\t) -- (-0.05,\\t) node[left] {\\t};}\n",
   "\\draw[thick] (0,0) -- (3,3) node[pos=0.9,above left] {physical};\n",
   "\\draw[dashed,thick,accent] (0,0) -- (1,1) -- (3,1) node[pos=0.8,below] {$\\Sigma_+$};\n",
   "\\draw[dotted,very thick,red!70!black] (0,1) -- (1,1) -- (3,3) node[pos=0.55,below right] {$\\Sigma_-$};\n",
   "\\foreach \\x/\\y in {", StringRiffle[(ToString[NumberForm[#[[1]], {3, 2}]] <> "/" <> ToString[NumberForm[#[[2]], {4, 3}]] &) /@ N[eaSeriesDots], ","],
   "} {\\fill (\\x,\\y) circle (1.3pt);}\n\\end{tikzpicture}\\qquad\n",
   "\\begin{tikzpicture}[x=1.45cm,y=0.045cm,font=\\footnotesize]\n",
   "\\draw[->] (0,0) -- (3.25,0) node[right] {$u/\\nu^2$}; \\draw[->] (0,-31) -- (0,33) node[above] {$\\Gamma^0/\\nu^2$};\n",
   "\\foreach \\t in {1,2,3} {\\draw (\\t,-1.2) -- (\\t,1.2) node[above right=-1pt] {\\t};} \\foreach \\t in {-20,20} {\\draw (0.05,\\t) -- (-0.05,\\t) node[left] {$\\t$};}\n",
   "\\draw[densely dashed,gray] (1,-30) -- (1,30);\n",
   "\\draw[thick] plot coordinates {", eaCoordinates[Select[eaClipped[#(1 + #)^2/(1 - #) &], #[[1]] < 1 &]], "};\n",
   "\\draw[thick] plot coordinates {", eaCoordinates[Select[eaClipped[#(1 + #)^2/(1 - #) &], #[[1]] > 1 &]], "};\n\\node[left] at (0.8,18) {$\\Gamma^0_c$}; \\node[below] at (2.3,-21) {$\\Gamma^0_c$};\n",
   "\\draw[dashed,thick] plot coordinates {", eaCoordinates[eaClipped[-#(1 + #) &]], "} node[right] {$\\Gamma^0_s$};\n",
   "\\end{tikzpicture}\n"], "Text"];
writeValue["seconds effective actions", N[Round[10 (AbsoluteTime[] - eaStart)]/10]];

(* ---------------- pictures for papers ---------------- *)
(* the TikZ code of the first-order self-energy (Hartree and Fock), with the labels x_1, x_2 *)
Export["figures/tikz_example.tex", DiagramToTikZ[OrderPart[selfEnergy, InteractionPotential, 1], {x, y}, "Labels" -> {x -> "x_1", y -> "x_2"}], "Text"];
(* the line styles of DeclareLineStyle: one line of each *)
lineStyleNames = {"Plain", "Arrow", "DoubleArrow", "DottedArrow", "Double", "Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag",
   "Impurity", "DivergingArrows", "ConvergingArrows"};
lineStylePictures = Table[With[{head = Symbol["LineStyle" <> s]}, DeclareLineStyle[head, s];
    DiagramToTikZ[head[x, y], {x, y}, "Output" -> "List", "VertexLabels" -> None, "Scale" -> 0.6, "LineWidth" -> 0.5][[1, 2]]], {s, lineStyleNames}];
Export["figures/line_styles.terms", joinBlocks[Table[lineStyleNames[[k]] <> "\n%%PICTURE%%\n" <> lineStylePictures[[k]], {k, Length[lineStyleNames]}]], "Text"];

writeValue["seconds total", N[Round[10 (AbsoluteTime[] - startTime)]/10]];
Print["Done: the results are in ", FileNameJoin[{Directory[], "figures"}]];
ResetDirectory[];
