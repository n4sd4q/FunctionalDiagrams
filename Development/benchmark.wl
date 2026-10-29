(* ::Package:: *)

(* ========================================================================= *)
(*  Benchmark of FunctionalDiagrams: timings of representative calculations, *)
(*  each with a reference of its output, so that a faster package can be     *)
(*  checked to give exactly the same results.                                *)
(*                                                                           *)
(*    wolfram -script Development/benchmark.wl [options] [case or group ...] *)
(*                                                                           *)
(*  The package is loaded from the repository that holds this file. Without *)
(*  names every case runs (about a minute) but the slow ones                 *)
(*  (nambu.PhiPairing4, twenty seconds alone), which run when named or       *)
(*  with --all; a name selects a case (gas.Phi4) or a group (gas: every      *)
(*  gas.* case but the slow ones). Options:                                  *)
(*    --list           the cases, with what they compute                     *)
(*    --all            the slow cases too                                    *)
(*    --save           save the outputs as the references                    *)
(*    --runs N         N runs, each in a fresh kernel, one after the other;   *)
(*                     the minimum time of each case is reported             *)
(*    --timings FILE   write the timings to FILE (tab-separated)             *)
(*    --summarize F... the minimum over timing files written by --timings     *)
(*    --reference DIR  the references (default build/optim/reference)        *)
(*  Without --save every output is compared with its reference, if there is *)
(*  one: "same" (the same InputForm: the hash, or the saved output), "same   *)
(*  to 1e-6" (pictures: the same up to rounding), "EQUIVALENT" (equal as     *)
(*  diagrams, SimplifyDiagrams of the difference is 0, but written           *)
(*  differently), "DIFFERENT"; the exit code is 1 if an output is not the    *)
(*  same. The reference of a case is its output in InputForm (NAME.m, with   *)
(*  the private temporary symbols name$123 renumbered in order of            *)
(*  appearance and the reals of pictures rounded to 1e-6) and the SHA-256 of *)
(*  that text (NAME.hash). A case gets its inputs (the objects it starts     *)
(*  from) before its timer starts; the timings are wall-clock and CPU        *)
(*  seconds, with the system caches cleared before each case.                *)
(* ========================================================================= *)

$IterationLimit = 10^8;
$HistoryLength = 0;
benchmarkFile = If[StringQ[$InputFileName] && $InputFileName =!= "", $InputFileName, FileNameJoin[{Directory[], "Development", "benchmark.wl"}]];
repository = ParentDirectory[DirectoryName[benchmarkFile]];
(* the arguments after the name of the script: $ScriptCommandLine (wolframscript), or $CommandLine after -script FILE *)
arguments = Which[ListQ[$ScriptCommandLine] && Length[$ScriptCommandLine] > 0, Rest[$ScriptCommandLine],
   MemberQ[$CommandLine, "-script" | "-file"], Drop[$CommandLine, First[First[Position[$CommandLine, "-script" | "-file", {1}]]] + 1],
   True, {}];

(* ---- the command line ---- *)
optionArgument[name_String, default_] := With[{p = Position[arguments, name, {1}]},
   If[p === {} || p[[1, 1]] == Length[arguments], default, arguments[[p[[1, 1]] + 1]]]];
flagQ[name_String] := MemberQ[arguments, name];
referenceDirectory = optionArgument["--reference", FileNameJoin[{repository, "build", "optim", "reference"}]];
timingsFile = optionArgument["--timings", None];
runs = ToExpression[optionArgument["--runs", "1"]];
saveQ = flagQ["--save"];
valueOptions = {"--reference", "--timings", "--runs"};
selection = Module[{rest = arguments, out = {}},
   While[rest =!= {},
    Which[
     MemberQ[valueOptions, First[rest]], rest = Drop[rest, Min[2, Length[rest]]],
     StringStartsQ[First[rest], "--"], rest = Rest[rest],
     True, AppendTo[out, First[rest]]; rest = Rest[rest]]];
   out];

(* ---- timing files: case, wall seconds, CPU seconds, leaf count, hash, status ---- *)
readTimings[file_String] := Select[StringSplit[#, "\t"] & /@ StringSplit[Import[file, "Text"], "\n"], Length[#] >= 6 &];
summarize[files_List] := Module[{tables = readTimings /@ files, names, rows},
   names = DeleteDuplicates[Flatten[tables[[All, All, 1]]]];
   rows = Table[With[{entries = Select[Join @@ tables, #[[1]] === n &]},
      {n, Min[ToExpression /@ entries[[All, 2]]], Min[ToExpression /@ entries[[All, 3]]], Length[entries],
       If[AllTrue[entries[[All, 6]], MemberQ[{"same", "same to 1e-6", "new", "saved"}, #] &], First[entries][[6]],
        StringRiffle[Union[entries[[All, 6]]], "/"]]}], {n, names}];
   Print[StringPadRight["case", 28], StringPadLeft["wall", 9], StringPadLeft["CPU", 9], "  runs  status"];
   Scan[Print[StringPadRight[#[[1]], 28], StringPadLeft[ToString[NumberForm[#[[2]], {6, 2}]], 9],
      StringPadLeft[ToString[NumberForm[#[[3]], {6, 2}]], 9], StringPadLeft[ToString[#[[4]]], 6], "  ", #[[5]]] &, rows];
   Print[StringPadRight["total", 28], StringPadLeft[ToString[NumberForm[Total[rows[[All, 2]]], {6, 1}]], 9],
    StringPadLeft[ToString[NumberForm[Total[rows[[All, 3]]], {6, 1}]], 9]];
   rows];

If[flagQ["--summarize"],
  summarize[Select[arguments, ! StringStartsQ[#, "--"] &]]; Exit[0]];

(* ---- several runs, each in a fresh kernel ---- *)
If[runs > 1,
  Module[{files, kernel = First[$CommandLine], childArguments, logs, codes},
   childArguments = Module[{rest = arguments, out = {}},
     While[rest =!= {}, If[MemberQ[{"--runs", "--timings"}, First[rest]], rest = Drop[rest, Min[2, Length[rest]]],
       AppendTo[out, First[rest]]; rest = Rest[rest]]]; out];
   If[! DirectoryQ[FileNameJoin[{repository, "build", "optim", "runs"}]], CreateDirectory[FileNameJoin[{repository, "build", "optim", "runs"}]]];
   files = Table[FileNameJoin[{repository, "build", "optim", "runs", "timings_" <> ToString[k] <> ".tsv"}], {k, runs}];
   logs = Table[FileNameJoin[{repository, "build", "optim", "runs", "run_" <> ToString[k] <> ".log"}], {k, runs}];
   codes = Table[
     Print["run ", k, " of ", runs, " (log ", logs[[k]], ")"];
     Run[StringRiffle[Join[{kernel, "-script", benchmarkFile}, If[saveQ && k > 1, DeleteCases[childArguments, "--save"], childArguments],
          {"--timings", files[[k]]}] /. s_String :> "'" <> StringReplace[s, "'" -> "'\\''"] <> "'", " "] <> " > '" <> logs[[k]] <> "' 2>&1"],
     {k, runs}];
   Print["minimum of ", runs, " runs:"];
   summarize[files];
   If[timingsFile =!= None, Export[timingsFile, StringRiffle[(StringRiffle[#, "\t"] &) /@ (Join @@ (readTimings /@ files)), "\n"], "Text"]];
   Exit[If[AllTrue[codes, # == 0 &], 0, 1]]]];

(* ---- the package ---- *)
Get[FileNameJoin[{repository, "FunctionalDiagrams.wl"}]];

(* ========================================================================= *)
(*  The theories (the names of the showcase), declared once for every case   *)
(* ========================================================================= *)

fourLabels = {x1, x2, x3, x4};
(* phi^4: D0 = <phi phi>_0, S_int = lambda/4! Integral phi^4 *)
DeclareSource[J]; DeclareField[phi, J]; DeclareSymmetric[FreePropagator, DressedPropagator]; DeclareCoupling[lambda];
FreeBosonGeneratingFunctional = Exp[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}]];
Phi4Vertex = lambda/24 IntegralOver[phi[z]^4, {z}];
(* the electron gas: G0 = -<psi psibar>_0, S_int = 1/2 Integral V psibar psibar psi psi *)
DeclareGrassmannSource[eta, etabar]; DeclareDirectedPropagator[BareGreenFunction, DressedGreenFunction];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
DeclareSymmetric[InteractionPotential]; DeclareInteractionLine[InteractionPotential];
FreeFermionGeneratingFunctional = Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}]];
CoulombVertex = 1/2 IntegralOver[InteractionPotential[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
(* a potential coupled to psibar psi: dG0(p, q)/dU(z) and dG0(p, q)/dU(r, s) *)
DeclareSource[LocalPotential, PotentialU];
DeclareFunctionalDerivative[BareGreenFunction[p_, q_], LocalPotential[r_], BareGreenFunction[p, r] BareGreenFunction[r, q]];
DeclareFunctionalDerivative[BareGreenFunction[p_, q_], PotentialU[r_, s_], BareGreenFunction[p, r] BareGreenFunction[s, q]];
(* the superfield (psi, psibar) of Nambu and Gor'kov *)
DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex]; DeclareLocalVertex[NambuVertex];
DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar];
nambuZ0 = Exp[1/2 IntegralOver[BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b], {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d] NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
nambuRules = {NambuField -> {psi, psibar}, NambuPropagator -> {DressedGreenFunction, AnomalousGreenFunction, AnomalousGreenFunctionBar},
   NambuVertex -> CoulombVertex};
(* the same theory in components: Z0 with the pairing terms written by hand *)
DeclareAntisymmetric[BareAnomalousGreenFunction, BareAnomalousGreenFunctionBar];
nambuZPairing = Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}] +
    1/2 IntegralOver[BareAnomalousGreenFunction[x, y] etabar[x] ** etabar[y], {x, y}] + 1/2 IntegralOver[BareAnomalousGreenFunctionBar[x, y] eta[x] ** eta[y], {x, y}]];
(* Matsubara: G0(k, i w) = 1/(i w - xi_k), an instantaneous interaction v *)
DeclareMatsubaraFrequency[w, "Fermion"]; DeclareMatsubaraFrequency[nu, "Boson"];
gasRules = {BareGreenFunction -> {"Fermion", xi}, DressedGreenFunction -> {"Fermion", xi}, InteractionPotential -> {"Instantaneous", v}};
twoPoint = {y -> {k, I w}, x -> {-k, -I w}};
(* QED at one loop (its theory, qedOneLoop[] and qedVertexFunction): Tests/QEDOneLoop.wl without its report *)
$qedReport = False;
Get[FileNameJoin[{repository, "Tests", "QEDOneLoop.wl"}]];

(* ========================================================================= *)
(*  Inputs: the objects the cases start from, computed once, before the      *)
(*  timers                                                                   *)
(* ========================================================================= *)

applyInteraction[f_] := SimplifyDiagrams[-lambda/24 IntegralOver[FunctionalDerivative[f, J[z], J[z], J[z], J[z]], {z}]];
applyCoulomb[f_] := SimplifyDiagrams[-1/2 IntegralOver[InteractionPotential[u, v] FunctionalDerivative[f, eta[u], eta[v], etabar[v],
       etabar[u]], {u, v}]];
stripGaussian[e_] := SimplifyDiagrams[e /. Exp[_] -> 1];
(* the terms with at most n sources (the factor Z0 aside) *)
fewSources[e_, n_, sources_] := With[{t = Expand[e]}, Total[Select[If[Head[t] === Plus, List @@ t, {t}],
     Count[# /. Exp[_] -> 1, sources, Infinity] <= n &]]];

in["phi4.first"] := in["phi4.first"] = applyInteraction[FreeBosonGeneratingFunctional];
in["phi4.second"] := in["phi4.second"] = applyInteraction[in["phi4.first"]];
in["phi4.thirdFour"] := in["phi4.thirdFour"] = Module[{f = fewSources[in["phi4.second"], 8, _J]},
   Do[f = fewSources[FunctionalDerivative[f, J[z]], kk, _J], {kk, {7, 6, 5, 4}}];
   SimplifyDiagrams[-lambda/24 IntegralOver[f, {z}]]];
in["phi4.Gamma2_4"] := in["phi4.Gamma2_4"] = LuttingerWardFunctional[FreeBosonGeneratingFunctional, Phi4Vertex, 4, FreePropagator -> DressedPropagator];
in["phi4.vertex3"] := in["phi4.vertex3"] = PerturbativeFullVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 3, {phi}, {a, b, c, d},
   FreePropagator -> DressedPropagator];
in["phi4.Gamma1PI"] := in["phi4.Gamma1PI"] = EffectiveAction1PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3, phi -> phiMean,
   FreePropagator -> BackgroundPropagator];
in["gas.coulombSecond"] := in["gas.coulombSecond"] = applyCoulomb[applyCoulomb[FreeFermionGeneratingFunctional]];
in["gas.G3"] := in["gas.G3"] = -PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x], psibar[y]}, CoulombVertex, 3];
in["gas.SigmaPert3"] := in["gas.SigmaPert3"] = AmputateExternalLegs[ParticleIrreducibleDiagrams[in["gas.G3"], 1, {x, y}, {BareGreenFunction}],
   {x, y}, {BareGreenFunction}];
in["gas.Phi3"] := in["gas.Phi3"] = LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 3, BareGreenFunction -> DressedGreenFunction];
in["gas.Phi4"] := in["gas.Phi4"] = LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 4, BareGreenFunction -> DressedGreenFunction];
in["gas.Sigma3"] := in["gas.Sigma3"] = SimplifyDiagrams[FunctionalDerivative[in["gas.Phi3"], DressedGreenFunction[y, x]]];
in["gas.Gamma03"] := in["gas.Gamma03"] = SimplifyDiagrams[FunctionalDerivative[in["gas.Phi3"], DressedGreenFunction[x4, x3], DressedGreenFunction[x2, x1]]];
in["gas.F3"] := in["gas.F3"] = PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, 3, {psi, psibar}, fourLabels,
   BareGreenFunction -> DressedGreenFunction];
in["gas.F4"] := in["gas.F4"] = PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, {1, 4}, {psi, psibar}, fourLabels,
   BareGreenFunction -> DressedGreenFunction];
in["gas.Lambda3"] := in["gas.Lambda3"] = PerturbativeDensityVertex[FreeFermionGeneratingFunctional, CoulombVertex, 3, {psi, psibar}, {x1, x2, z},
   BareGreenFunction -> DressedGreenFunction];
in["gas.chiSkeleton2"] := in["gas.chiSkeleton2"] = SimplifyDiagrams[SkeletonDiagrams[ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
      {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 2], {a, b}, {BareGreenFunction}] /. BareGreenFunction -> DressedGreenFunction];
in["nambu.Phi3"] := in["nambu.Phi3"] = LuttingerWardFunctional[nambuZ0, nambuInteraction, 3, BareNambuPropagator -> NambuPropagator];
in["nambu.Phi4"] := in["nambu.Phi4"] = LuttingerWardFunctional[nambuZ0, nambuInteraction, 4, BareNambuPropagator -> NambuPropagator];

(* ========================================================================= *)
(*  The cases: name, what it computes, its inputs, the computation (timed)   *)
(* ========================================================================= *)

cases = {};
SetAttributes[defineCase, HoldRest];
defineCase[name_String, description_String, inputs_List, body_] := AppendTo[cases, name -> {description, inputs, Hold[body]}];

(* ---- phi^4 theory ---- *)
defineCase["phi4.Z", "Z/Z0 of phi^4 by derivatives in J: orders 1 and 2 at any source, order 3 at zero source (as the showcase)", {},
  Module[{first = applyInteraction[FreeBosonGeneratingFunctional], second, f},
   second = applyInteraction[first];
   f = fewSources[second, 8, _J];
   Do[f = fewSources[FunctionalDerivative[f, J[z]], kk, _J], {kk, {7, 6, 5, 4}}];
   {first, second, SimplifyDiagrams[-lambda/24 IntegralOver[f, {z}]]}]];
defineCase["phi4.cumulants", "W = log Z at a source to lambda^3, its fourth derivative, the fourth moment (showcase)",
  {"phi4.first", "phi4.second", "phi4.thirdFour"},
  Module[{four = fewSources[#, 4, _J] &, s1, s2, s3, wJ, moment, w4},
   {s1, s2, s3} = {stripGaussian[in["phi4.first"]], stripGaussian[in["phi4.second"]/2], stripGaussian[in["phi4.thirdFour"]/6]};
   wJ = SimplifyDiagrams[OrderPart[SimplifyDiagrams[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}] + four[s1 + s2 + s3] -
        four[four[s1] four[s1 + 2 s2]]/2 + four[four[s1]^2 four[s1]]/3], lambda, {0, 1, 2, 3}]];
   moment = DropVacuumBubbles[SetSourcesToZero[FunctionalDerivative[FreeBosonGeneratingFunctional +
        four[in["phi4.first"] + in["phi4.second"]/2 + in["phi4.thirdFour"]/6], J[a], J[b], J[c], J[d]]], {a, b, c, d}];
   w4 = SetSourcesToZero[FunctionalDerivative[wJ, J[a], J[b], J[c], J[d]]];
   {wJ, moment, w4}]];
defineCase["phi4.vacuum4", "ConnectedVacuumDiagrams of phi^4 to lambda^4", {},
  ConnectedVacuumDiagrams[FreeBosonGeneratingFunctional, Phi4Vertex, 4]];
defineCase["phi4.connected4", "ConnectedCorrelationFunction <phi phi phi phi>_c to lambda^3", {},
  ConnectedCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3]];
defineCase["phi4.Gamma2", "LuttingerWardFunctional (Gamma2) of phi^4 to lambda^4", {},
  LuttingerWardFunctional[FreeBosonGeneratingFunctional, Phi4Vertex, 4, FreePropagator -> DressedPropagator]];
defineCase["phi4.Sigma", "Sigma[G] = -2 dGamma2/dG to lambda^4 (FunctionalDerivative)", {"phi4.Gamma2_4"},
  SimplifyDiagrams[-2 FunctionalDerivative[in["phi4.Gamma2_4"], DressedPropagator[a, b]]]];
defineCase["phi4.Gamma0", "Gamma0 = 4 d^2Gamma2/dG dG to lambda^4 (FunctionalDerivative)", {"phi4.Gamma2_4"},
  SimplifyDiagrams[4 FunctionalDerivative[in["phi4.Gamma2_4"], DressedPropagator[a, b], DressedPropagator[c, d]]]];
defineCase["phi4.vertex4", "PerturbativeFullVertex of phi^4 to lambda^4", {},
  PerturbativeFullVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 4, {phi}, {a, b, c, d}, FreePropagator -> DressedPropagator]];
defineCase["phi4.parquet", "ParquetDecomposition and CheckParquetEquations of the phi^4 vertex to lambda^3", {"phi4.vertex3"},
  {ParquetDecomposition[in["phi4.vertex3"], {a, b, c, d}, {DressedPropagator}, "Statistics" -> "Boson"],
   CheckParquetEquations[in["phi4.vertex3"], {a, b, c, d}, DressedPropagator, lambda, 3, "Statistics" -> "Boson"]}];

(* ---- the electron gas ---- *)
defineCase["gas.Z", "Z/Z0 of the electron gas by derivatives in eta, etabar: order 2 at any source, order 3 at zero source (as the showcase)", {},
  Module[{second = applyCoulomb[applyCoulomb[FreeFermionGeneratingFunctional]], f},
   f = fewSources[second, 4, _eta | _etabar];
   Do[f = fewSources[FunctionalDerivative[f, sk[[1]]], sk[[2]], _eta | _etabar], {sk, {{etabar[u], 3}, {etabar[v], 2}, {eta[v], 1}}}];
   {second, SetSourcesToZero[-1/2 IntegralOver[InteractionPotential[u, v] FunctionalDerivative[f, eta[u]], {u, v}]]/6}]];
defineCase["gas.G", "PerturbativeCorrelationFunction G = -<psi psibar> to V^3 (Wick pairings)", {},
  -PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x], psibar[y]}, CoulombVertex, 3]];
defineCase["gas.vacuum4", "ConnectedVacuumDiagrams of the electron gas to V^4", {},
  ConnectedVacuumDiagrams[FreeFermionGeneratingFunctional, CoulombVertex, 4]];
defineCase["gas.connected4", "ConnectedCorrelationFunction <psi psibar psibar psi>_c to V^3 (330 diagrams at V^3)", {},
  ConnectedCorrelationFunction[FreeFermionGeneratingFunctional, {psi[x1], psibar[x2], psibar[x4], psi[x3]}, CoulombVertex, 3]];
defineCase["gas.SigmaPerturbative", "AmputateExternalLegs of ParticleIrreducibleDiagrams of G to V^3, SkeletonDiagrams of the result", {"gas.G3"},
  With[{sigma = AmputateExternalLegs[ParticleIrreducibleDiagrams[in["gas.G3"], 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}]},
   {sigma, SkeletonDiagrams[sigma, {x, y}, {BareGreenFunction}]}]];
defineCase["gas.Phi3", "LuttingerWardFunctional of the electron gas to V^3", {},
  LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 3, BareGreenFunction -> DressedGreenFunction]];
defineCase["gas.Phi4", "LuttingerWardFunctional of the electron gas to V^4", {},
  LuttingerWardFunctional[FreeFermionGeneratingFunctional, CoulombVertex, 4, BareGreenFunction -> DressedGreenFunction]];
defineCase["gas.Sigma", "Sigma = dPhi/dG to V^4 (FunctionalDerivative, SimplifyDiagrams)", {"gas.Phi4"},
  SimplifyDiagrams[FunctionalDerivative[in["gas.Phi4"], DressedGreenFunction[y, x]]]];
defineCase["gas.Gamma0", "Gamma0 = d^2Phi/dG dG to V^4 (FunctionalDerivative, SimplifyDiagrams)", {"gas.Phi4"},
  SimplifyDiagrams[FunctionalDerivative[in["gas.Phi4"], DressedGreenFunction[x4, x3], DressedGreenFunction[x2, x1]]]];
defineCase["gas.F3", "PerturbativeFullVertex of the electron gas to V^3", {},
  PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, 3, {psi, psibar}, fourLabels, BareGreenFunction -> DressedGreenFunction]];
defineCase["gas.F4", "PerturbativeFullVertex of the electron gas, orders V to V^4 (the showcase's EA full vertex)", {},
  PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, {1, 4}, {psi, psibar}, fourLabels, BareGreenFunction -> DressedGreenFunction]];
defineCase["gas.Lambda", "PerturbativeDensityVertex to V^3", {},
  PerturbativeDensityVertex[FreeFermionGeneratingFunctional, CoulombVertex, 3, {psi, psibar}, {x1, x2, z}, BareGreenFunction -> DressedGreenFunction]];
defineCase["gas.L", "L = dG/dU(x4, x3) of G to V^3 (518 diagrams at V^3)", {"gas.G3"},
  SimplifyDiagrams[FunctionalDerivative[in["gas.G3"] /. {x -> x1, y -> x2}, PotentialU[x4, x3]]]];
defineCase["gas.checks", "SimplifyDiagrams of differences that vanish: crossing of F, symmetry and Ward identity of Gamma0 (V^3)",
  {"gas.F3", "gas.Gamma03", "gas.Sigma3"},
  {SimplifyDiagrams[in["gas.F3"] + (in["gas.F3"] /. {x1 -> x3, x3 -> x1})], SimplifyDiagrams[in["gas.F3"] + (in["gas.F3"] /. {x2 -> x4, x4 -> x2})],
   SimplifyDiagrams[in["gas.Gamma03"] - (in["gas.Gamma03"] /. {x1 -> x3, x2 -> x4, x3 -> x1, x4 -> x2})],
   SimplifyDiagrams[IntegralOver[in["gas.Gamma03"] (theta[x4] - theta[x3]) DressedGreenFunction[x4, x3], {x3, x4}] -
     (theta[x1] - theta[x2]) SimplifyDiagrams[in["gas.Sigma3"] /. {x -> x1, y -> x2}]]}];

(* ---- classes, parquet, exact equations ---- *)
defineCase["parquet.F3", "ParquetDecomposition and CheckParquetEquations of F to V^3", {"gas.F3"},
  {ParquetDecomposition[in["gas.F3"], fourLabels, {DressedGreenFunction}],
   CheckParquetEquations[in["gas.F3"], fourLabels, DressedGreenFunction, InteractionPotential, 3]}];
defineCase["parquet.F4", "ParquetDecomposition of F, orders V to V^4", {"gas.F4"},
  ParquetDecomposition[in["gas.F4"], fourLabels, {DressedGreenFunction}]];
defineCase["exact.equations", "CheckEquation of ExactEquation Vertex, VertexFromF, SelfEnergyFromVertex, BetheSalpeter (V^3)",
  {"gas.Sigma3", "gas.Gamma03", "gas.F3", "gas.Lambda3"},
  With[{objects = {"G" -> DressedGreenFunction, "V" -> InteractionPotential, "Sigma" -> LabelFunction[in["gas.Sigma3"], {x, y}],
      "Gamma0" -> LabelFunction[in["gas.Gamma03"], fourLabels], "F" -> LabelFunction[in["gas.F3"], fourLabels],
      "Lambda" -> LabelFunction[in["gas.Lambda3"], {x1, x2, z}]}},
   {CheckEquation[ExactEquation["Vertex", objects, {x1, x2, z}, "MaximumOrder" -> {InteractionPotential, 3}], InteractionPotential, 3],
    CheckEquation[ExactEquation["VertexFromF", objects, {x1, x2, z}, "MaximumOrder" -> {InteractionPotential, 3}], InteractionPotential, 3],
    CheckEquation[ExactEquation["SelfEnergyFromVertex", objects, {x, y}, "MaximumOrder" -> {InteractionPotential, 3}], InteractionPotential, 3],
    CheckEquation[ExactEquation["BetheSalpeter", objects, fourLabels, "MaximumOrder" -> {InteractionPotential, 3}], InteractionPotential, 3]}]];
defineCase["exact.hedin", "ExactEquation Hedin and CheckEquation (V^3): proper vertex, polarization, W by IrreducibleDiagrams",
  {"gas.Sigma3", "gas.Gamma03", "gas.Lambda3", "gas.chiSkeleton2"},
  Module[{proper, polarization, screened, equations},
   proper = IrreducibleDiagrams[in["gas.Lambda3"], {{x1, x2}, {z}}, 1, {InteractionPotential}];
   polarization = -IrreducibleDiagrams[in["gas.chiSkeleton2"], {{a}, {b}}, 1, {InteractionPotential}];
   screened = SimplifyDiagrams[InteractionPotential[a, b] - IntegralOver[InteractionPotential[a, c] (in["gas.chiSkeleton2"] /. {a -> c, b -> d}) *
        InteractionPotential[d, b], {c, d}]];
   equations = ExactEquation["Hedin", {"G" -> DressedGreenFunction, "V" -> InteractionPotential, "Sigma" -> LabelFunction[in["gas.Sigma3"], {x, y}],
      "W" -> LabelFunction[screened, {a, b}], "P" -> LabelFunction[polarization, {a, b}], "Lambda" -> LabelFunction[proper, {x1, x2, z}],
      "GammaXC" -> LabelFunction[in["gas.Gamma03"] - DeltaFunction[x1, x2] DeltaFunction[x3, x4] InteractionPotential[x1, x3], fourLabels]}, {x, y, z},
     "MaximumOrder" -> {InteractionPotential, 3}];
   {proper, polarization, MapThread[CheckEquation[#1, InteractionPotential, #2] &, {equations, {3, 2, 3, 3}}]}]];

(* ---- superfields ---- *)
defineCase["nambu.Phi3", "LuttingerWardFunctional of the superfield to V^3", {},
  LuttingerWardFunctional[nambuZ0, nambuInteraction, 3, BareNambuPropagator -> NambuPropagator]];
defineCase["nambu.Phi4", "LuttingerWardFunctional of the superfield to V^4", {},
  LuttingerWardFunctional[nambuZ0, nambuInteraction, 4, BareNambuPropagator -> NambuPropagator]];
defineCase["nambu.components", "SuperfieldComponents of Phi of the superfield (V^3) on psi, psibar", {"nambu.Phi3"},
  SuperfieldComponents[in["nambu.Phi3"], {}, nambuRules]];
defineCase["nambu.components4", "SuperfieldComponents of Phi of the superfield at V^4 (the components with anomalous lines)", {"nambu.Phi4"},
  SuperfieldComponents[in["nambu.Phi4"], {}, nambuRules]];
defineCase["nambu.PhiPairing4", "LuttingerWardFunctional with pairing terms in Z0 to V^4: the same Phi as nambu.components4, directly (the slowest case)", {},
  LuttingerWardFunctional[nambuZPairing, CoulombVertex, 4, {BareGreenFunction -> DressedGreenFunction,
    BareAnomalousGreenFunction -> AnomalousGreenFunction, BareAnomalousGreenFunctionBar -> AnomalousGreenFunctionBar}]];
defineCase["nambu.F", "PerturbativeFullVertex of the superfield to V^3", {},
  PerturbativeFullVertex[nambuZ0, nambuInteraction, 3, {NambuField}, {a, b, c, d}, BareNambuPropagator -> NambuPropagator]];

(* ---- pictures ---- *)
pictureOptions = Sequence["Scale" -> 0.34, "LineWidth" -> 0.42, "LabelFont" -> "\\tiny"];
defineCase["draw.plotGamma0", "DiagramPlot of Gamma0 of the electron gas, V to V^3 (58 diagrams)", {"gas.Gamma03"},
  DiagramPlot[in["gas.Gamma03"], fourLabels]];
defineCase["draw.plotG", "DiagramPlot of G of the electron gas, orders 0 to V^3 (87 diagrams)", {"gas.G3"},
  DiagramPlot[in["gas.G3"], {x, y}]];
defineCase["draw.tikzGamma0", "DiagramToTikZ of Gamma0 of the electron gas, V to V^3 (58 diagrams)", {"gas.Gamma03"},
  DiagramToTikZ[in["gas.Gamma03"], fourLabels, "Output" -> "List", pictureOptions]];
defineCase["draw.tikzF", "DiagramToTikZ of F of the electron gas, V to V^3", {"gas.F3"},
  DiagramToTikZ[in["gas.F3"], fourLabels, "Output" -> "List", pictureOptions]];
defineCase["draw.tikzLambda", "DiagramToTikZ of the density vertex Lambda, orders 0 to V^3", {"gas.Lambda3"},
  DiagramToTikZ[in["gas.Lambda3"], {x1, x2, z}, "Output" -> "List", pictureOptions]];
defineCase["draw.tikzVacuum", "DiagramToTikZ of Phi of the electron gas to V^4 (vacuum diagrams)", {"gas.Phi4"},
  DiagramToTikZ[in["gas.Phi4"], {}, "Output" -> "List", pictureOptions]];

(* ---- Matsubara sums ---- *)
defineCase["matsubara.sigma3", "EvaluateMatsubara of the 10 skeleton diagrams of Sigma at V^3, one by one", {"gas.Sigma3"},
  Table[EvaluateMatsubara[t, twoPoint, gasRules], {t, DiagramTerms[OrderPart[in["gas.Sigma3"], InteractionPotential, 3]]}]];
defineCase["matsubara.cutting3", "CheckCuttingRules of the 42 diagrams of the perturbative Sigma at V^3", {"gas.SigmaPert3"},
  Quiet[CheckCuttingRules[#, twoPoint, gasRules, w -> omega, "SpinDegeneracy" -> 2] & /@
    DiagramTerms[OrderPart[in["gas.SigmaPert3"], InteractionPotential, 3]], CuttingRuleImaginaryPart::class]];

(* ---- dimensional regularization: MS-bar renormalization of phi^4 to two loops (as Tests/RunTests.wl) ---- *)
phi4Renormalization[] := Module[{prop, phi4SelfEnergy, c4, gamma4, sigma21, massless, routed, sigmaB, gamma4B, sigma21B,
    kappa = 1/(16 Pi^2), zLambda, zPhi, zM2, truncate, renormalized2, renormalized4, renormalized21, poleEquations, solution = {},
    residual, zLambdaS, zPhiS, zM2S, beta, gammaPhi, gammaM2, zl1, zf1, zm1, zl22, zl21, zf22, zf21, zm22, zm21, lp},
   prop = PerturbativeCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b]}, Phi4Vertex, 2];
   phi4SelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[prop, 1, {a, b}, {FreePropagator}], {a, b}, {FreePropagator}];
   c4 = ConnectedCorrelationFunction[FreeBosonGeneratingFunctional, {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3];
   gamma4 = -AmputateExternalLegs[ParticleIrreducibleDiagrams[c4, 1, {a, b, c, d}, {FreePropagator}], {a, b, c, d}, {FreePropagator}];
   sigma21 = -MassInsertion[phi4SelfEnergy, FreePropagator, s];
   massless = {FreePropagator -> {"Scalar", 0}};
   routed = {InfraredRearrangedIntegral[phi4SelfEnergy, {a, b}, p, massless, eps, "Routing" -> True],
     InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps, "Routing" -> True],
     InfraredRearrangedIntegral[sigma21, {a, b, s}, p, massless, eps, "Routing" -> True]};
   {sigmaB, gamma4B, sigma21B} = Total[#[[All, 3]]] & /@ routed;
   zLambda = 1 + lambda kappa zl1/eps + lambda^2 kappa^2 (zl22/eps^2 + zl21/eps);
   zPhi = 1 + lambda kappa zf1/eps + lambda^2 kappa^2 (zf22/eps^2 + zf21/eps);
   zM2 = 1 + lambda kappa zm1/eps + lambda^2 kappa^2 (zm22/eps^2 + zm21/eps);
   truncate[e_, n_] := Sum[Coefficient[e, lambda, k] lambda^k, {k, 0, n}];
   renormalized2 = truncate[Expand[zPhi (p^2 - (sigmaB /. lambda -> lambda zLambda))], 2];
   renormalized4 = truncate[Expand[zPhi^2 (gamma4B /. lambda -> lambda zLambda)], 3];
   renormalized21 = truncate[Expand[zM2 (1 - (sigma21B /. lambda -> lambda zLambda))], 2];
   poleEquations[e_] := With[{poles = Expand[PolePart[e, eps] /. Log[p^2/RenormalizationScale^2] -> lp]},
     Union[Flatten[Table[CoefficientList[Coefficient[poles, eps, -j], {lp, p}], {j, 1, 4}]]]];
   Do[Module[{eqs},
     eqs = DeleteCases[Expand[Flatten[{poleEquations[Coefficient[renormalized2, lambda, k] /. solution],
          poleEquations[Coefficient[renormalized4, lambda, k + 1] /. solution], poleEquations[Coefficient[renormalized21, lambda, k] /. solution]}]], 0];
     solution = Join[solution, First[Solve[Thread[eqs == 0], Select[{zl1, zf1, zm1, zl22, zl21, zf22, zf21, zm22, zm21}, ! FreeQ[eqs, #] &]]]]],
    {k, 1, 2}];
   residual = {Simplify[PolePart[renormalized2 /. solution, eps]], Simplify[PolePart[renormalized4 /. solution, eps]],
     Simplify[PolePart[renormalized21 /. solution, eps]]};
   zLambdaS = zLambda /. solution; zPhiS = zPhi /. solution; zM2S = zM2 /. solution;
   beta = BetaFunctionFromZ[zLambdaS, lambda, eps, 3];
   gammaPhi = 1/2 AnomalousDimensionFromZ[zPhiS, lambda, eps, -2 eps lambda + beta, 2];
   gammaM2 = -AnomalousDimensionFromZ[zM2S/zPhiS, lambda, eps, -2 eps lambda + beta, 2];
   {routed, residual, zPhiS, zLambdaS, zM2S, beta, gammaPhi, gammaM2}];
defineCase["dimreg.phi4", "MS-bar renormalization of phi^4 to two loops: diagrams, InfraredRearrangedIntegral, Z factors, beta, gammas", {},
  phi4Renormalization[]];

(* ---- QED at one loop ---- *)
defineCase["qed.oneloop", "qedOneLoop[] of Tests/QEDOneLoop.wl: Sigma_2, Pi_2, the vertex, form factors (its own timing left out)", {},
  DeleteCases[qedOneLoop[], "Seconds" -> _]];

(* ---- effective actions ---- *)
defineCase["ea.Gamma4", "Gamma^(4) of EffectiveAction1PI of phi^4 at phibar = 0 to lambda^3, four derivatives at once (as the tests)",
  {"phi4.Gamma1PI"},
  ExpandTrace[SetSourcesToZero[FunctionalDerivative[in["phi4.Gamma1PI"], phiMean[x1], phiMean[x2], phiMean[x3], phiMean[x4]]]] /.
   BackgroundPropagator -> FreePropagator];
defineCase["ea.Psi1PI", "EffectiveAction1PI Gamma[Psibar, Psi] of the electron gas to V^3, and its fourth derivative at zero means (V^2)", {},
  With[{gamma = EffectiveAction1PI[FreeFermionGeneratingFunctional, CoulombVertex, 3, {psi -> MeanPsi, psibar -> MeanPsibar},
      BareGreenFunction -> BareGreenFunction]},
   {gamma, SetSourcesToZero[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[FunctionalDerivative[
          OrderPart[gamma, InteractionPotential, {1, 2}], MeanPsi[x4]], MeanPsibar[x3]], MeanPsi[x2]], MeanPsibar[x1]]]}]];
defineCase["ea.value2PI", "Gamma[G] of the electron gas at the perturbative G = (G0^-1 - Sigma)^-1, ExpandTrace to V^3", {"gas.Phi3", "gas.SigmaPert3"},
  With[{sigma = LabelFunction[in["gas.SigmaPert3"] /. {x -> a, y -> b}, {a, b}]},
   ExpandTrace[EffectiveAction2PI[in["gas.Phi3"], BareGreenFunction -> DressedGreenFunction] /.
     DressedGreenFunction -> Power[Power[BareGreenFunction, -1] - sigma, -1], BareGreenFunction, 3, "MaximumOrder" -> {InteractionPotential, 3}]]];
defineCase["ea.CJT", "EffectiveAction2PI Gamma[phibar, G] of phi^4 (Cornwall, Jackiw, Tomboulis) to lambda^3, its field equation", {},
  With[{gamma = EffectiveAction2PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3, FreePropagator -> DressedPropagator, "Statistics" -> "Boson",
      "MeanFields" -> {phi -> phiMean}]}, {gamma, FunctionalDerivative[gamma, phiMean[a]]}]];
defineCase["ea.oneSite", "zero dimensions, one site with spin: Phi, Sigma, Gamma[G] of the electron gas to u^4 (ZeroDimensionalValue)", {"gas.Phi4"},
  With[{zeroD = ZeroDimensionalValue[#, {BareGreenFunction -> 1/s, InteractionPotential -> u, DressedGreenFunction -> g}, "LoopFactor" -> 2] &},
   {zeroD[in["gas.Phi4"]], zeroD[SelfEnergy[in["gas.Phi4"], DressedGreenFunction, {x, y}]],
    zeroD[EffectiveAction2PI[in["gas.Phi4"], BareGreenFunction -> DressedGreenFunction]]}]];

caseNames = cases[[All, 1]];
(* the slow cases run when named, or with --all *)
slowCases = {"nambu.PhiPairing4"};

(* ========================================================================= *)
(*  Running and comparing                                                    *)
(* ========================================================================= *)

If[flagQ["--list"], Scan[Print[StringPadRight[#[[1]], 22], #[[2, 1]], If[MemberQ[slowCases, #[[1]]], " (slow)", ""]] &, cases]; Exit[0]];

selected = If[selection === {}, If[flagQ["--all"], caseNames, Select[caseNames, ! MemberQ[slowCases, #] &]],
   DeleteDuplicates[Flatten[Table[Select[caseNames, # === s || StringStartsQ[#, s <> "."] && (flagQ["--all"] || ! MemberQ[slowCases, #]) &],
      {s, selection}]]]];
unknown = Select[selection, Function[s, ! AnyTrue[caseNames, # === s || StringStartsQ[#, s <> "."] &]]];
If[unknown =!= {}, Print["unknown cases: ", unknown, "; --list lists them"]; Exit[2]];

(* the text of an output: temporary symbols name$123 renumbered in order of appearance, reals rounded to 1e-6 *)
temporarySymbolQ[s_Symbol] := StringMatchQ[SymbolName[Unevaluated[s]], __ ~~ "$" ~~ DigitCharacter ..];
normalized[e_] := Module[{temporary = DeleteDuplicates[Cases[e, s_Symbol /; temporarySymbolQ[s], {0, Infinity}, Heads -> True]], rules},
   rules = MapIndexed[#1 -> Symbol["Global`benchmarkTemporary" <> ToString[First[#2]]] &, temporary];
   (e /. rules) /. r_Real :> Round[r, 10^-6]];
outputText[e_String] := e;
outputText[e_] := ToString[normalized[e], InputForm, PageWidth -> Infinity];
hashOf[text_String] := IntegerString[Hash[text, "SHA256"], 16, 64];

(* numerical outputs and pictures: the same up to the rounding of their numbers (compared held, unevaluated) *)
numbersOf[e_] := Cases[Hold[e], x : (_Integer | _Rational | _Real), {0, Infinity}, Heads -> False];
skeletonOf[e_] := Hold[e] /. (_Integer | _Rational | _Real) :> 0;
numbersClose[a_, b_] := With[{na = numbersOf[a], nb = numbersOf[b]},
   Length[na] == Length[nb] && skeletonOf[a] === skeletonOf[b] && (na === {} || Max[Abs[N[na - nb]]] < 10^-5)];
diagramOutputQ[e_] := FreeQ[e, _String | _Graphics | _Real | _Complex];
comparison[name_, output_, text_, hash_] := Module[{file = FileNameJoin[{referenceDirectory, name <> ".m"}],
    hashFile = FileNameJoin[{referenceDirectory, name <> ".hash"}], reference, difference},
   Which[
    ! FileExistsQ[hashFile], "new",
    StringTrim[Import[hashFile, "Text"]] === hash, "same",
    ! FileExistsQ[file], "DIFFERENT",
    True,
    reference = Get[file];
    Which[
     normalized[output] === reference, "same",
     ! diagramOutputQ[output], If[numbersClose[normalized[output], reference], "same to 1e-6", "DIFFERENT"],
     True, difference = Quiet[TimeConstrained[SimplifyDiagrams[normalized[output] - reference], 600, $Aborted]];
     If[difference === 0 || (ListQ[difference] && AllTrue[Flatten[{difference}], # === 0 &]), "EQUIVALENT", "DIFFERENT"]]]];
save[name_, output_, text_, hash_] := (
   If[! DirectoryQ[referenceDirectory], CreateDirectory[referenceDirectory, CreateIntermediateDirectories -> True]];
   Export[FileNameJoin[{referenceDirectory, name <> ".hash"}], hash, "Text"];
   If[StringQ[output], Put[output, FileNameJoin[{referenceDirectory, name <> ".m"}]],
    Export[FileNameJoin[{referenceDirectory, name <> ".m"}], text, "Text"]]);

Print["FunctionalDiagrams ", $FunctionalDiagramsVersion, " (", repository, "), Mathematica ", $VersionNumber, ", ",
  Length[selected], " cases, ", DateString[{"Year", "-", "Month", "-", "Day", " ", "Hour", ":", "Minute"}]];
results = Table[Module[{entry = Association[cases][name], wall, cpu, output, text, hash, status},
    Scan[in, entry[[2]]];
    ClearSystemCache[];
    {wall, {cpu, output}} = AbsoluteTiming[Timing[ReleaseHold[entry[[3]]]]];
    text = outputText[output];
    hash = hashOf[text];
    status = If[saveQ, save[name, output, text, hash]; "saved", comparison[name, output, text, hash]];
    Print[StringPadRight[name, 22], StringPadLeft[ToString[NumberForm[wall, {6, 2}]], 9], " s wall", StringPadLeft[ToString[NumberForm[cpu, {6, 2}]], 9],
     " s CPU   ", StringPadLeft[ToString[If[StringQ[output], StringLength[output], LeafCount[output]]], 9], If[StringQ[output], " chars ", " leaves"],
     "   ", StringTake[hash, 12], "   ", status];
    {name, wall, cpu, If[StringQ[output], StringLength[output], LeafCount[output]], hash, status}],
   {name, selected}];
Print[StringPadRight["total", 22], StringPadLeft[ToString[NumberForm[Total[results[[All, 2]]], {6, 1}]], 9], " s wall",
  StringPadLeft[ToString[NumberForm[Total[results[[All, 3]]], {6, 1}]], 9], " s CPU"];
If[timingsFile =!= None,
  Export[timingsFile, StringRiffle[(StringRiffle[ToString /@ {#[[1]], Round[#[[2]], 0.001], Round[#[[3]], 0.001], #[[4]], #[[5]], #[[6]]}, "\t"] &) /@
     results, "\n"], "Text"]];
failed = Select[results, ! MemberQ[{"same", "same to 1e-6", "new", "saved"}, #[[6]]] &];
If[failed =!= {}, Print["not the same as the reference: ", failed[[All, 1]]]];
Exit[If[failed === {}, 0, 1]];
