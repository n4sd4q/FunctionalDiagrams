
(* ========================================================================= *)
(*  21. Load-time check and banner                                           *)
(* ========================================================================= *)

(* warn if an exported name is shadowed by a symbol of another context (e.g. a built-in of a later version) *)
FunctionalDiagrams::clash = "The names `1` are already defined in other contexts; the package versions are not used.";
$publicNames = {"IntegralOver", "DeltaFunction", "GrassmannProduct", "DummyVariable", "DeclareSource", "DeclareGrassmannSource",
   "DeclareGrassmannOdd", "DeclareGrassmannGraded", "DeclareSymmetric", "DeclareAntisymmetric",
   "DeclareFunctionalDerivative", "DeclareDiscreteIndexValues", "DeclareField", "DeclareLineStyle",
   "DeclareDirectedPropagator", "DeclareInteractionLine", "DeclareCoupling", "DeclareDisplayName", "DeclareLocalVertex", "DeclareBlob", "ResetDeclarations",
   "FunctionalDerivative", "SimplifyDiagrams", "DiagramTerms", "SetSourcesToZero", "FieldDerivative",
   "FieldExpectationValue", "GaussianExpectationValue", "PerturbativeCorrelationFunction", "ConnectedCorrelationFunction",
   "ConnectedVacuumDiagrams", "OrderPart", "ConnectedDiagrams", "DropVacuumBubbles", "ParticleIrreducibleQ",
   "ParticleIrreducibleDiagrams", "SkeletonDiagrams", "ClosedLoopCount", "DropClosedLoops", "AmputateExternalLegs",
   "LuttingerWardFunctional", "SelfEnergy", "IrreducibleVertex", "PerturbativeFullVertex", "TwoParticleReducibleQ",
   "ReducibleDiagrams", "IrreducibleDiagrams", "ParquetDecomposition", "BetheSalpeterLadder", "CheckParquetEquations", "PerturbativeDensityVertex", "LabelFunction", "ExactEquation", "CheckEquation", "HubbardStratonovich", "IntegrateOutField",
   "DysonSeries", "ConnectedFunctionFromVertices", "FullPropagator", "VertexFunction", "MeanField", "ExternalSource",
   "DrawDiagram", "DiagramPlot", "DiagramToTikZ", "ExpressionToLaTeX",
   "FermiFunction", "BoseFunction", "DeclareMatsubaraFrequency", "MatsubaraSum", "LoopMomentum", "LoopFrequency", "MomentumSum", "DiagramIntegrand",
   "EvaluateMatsubara", "AnalyticContinuation", "RetardedImaginaryPart", "EnergyDelta", "CuttingRuleImaginaryPart", "CheckCuttingRules", "ZeroTemperatureLimit", "Temperature",
   "ScalarPropagator", "MomentumSquared", "RenormalizationScale", "LoopIntegral", "DiagramLoopIntegral", "IntegrateLoops", "GFunction",
   "TadpoleIntegral", "BubbleIntegral", "FeynmanParameterIntegral", "FeynmanParameter", "EpsilonExpand", "PolePart",
   "InfraredRearrangedIntegral", "MassInsertion", "BetaFunctionFromZ", "AnomalousDimensionFromZ",
   "GammaMatrix", "SlashedMomentum", "DiracProduct", "DiracTrace", "SpinorChain", "LorentzMetric", "FourVector", "ScalarProduct", "SpacetimeDimension", "DiracSimplify", "DiracOrder", "QEDIntegrand", "FeynmanDenominator", "MinkowskiLoopIntegral", "IntegrateOneLoop", "SimplexIntegral", "ParameterIntegral", "ReduceParameterIntegrals", "CombineParameterIntegrals", "ExpandParameterIntegrals", "DifferentiateUnderIntegral", "VertexFormFactors",
   "EffectiveAction2PI", "EffectiveAction1PI", "TraceLog", "OperatorTrace", "OperatorKernel", "ExpandTrace", "LegendreTransform",
   "LegendrePropagator", "SelfEnergyFunctional", "ZeroDimensionalValue", "SuperfieldComponents"};
With[{bad = Select[$publicNames, With[{sym = Symbol[#]}, Context[sym]] =!= "FunctionalDiagrams`" &]},
  If[bad =!= {}, Message[FunctionalDiagrams::clash, bad]]];

(* the banner shown when the package is loaded in a notebook: the logo (the exchange vacuum diagram on a blue badge,
   the geometry of Development/make_logo.py in a 128 x 128 box, y upwards), the attribution "FunctionalDiagrams, by
   Nazzareno Africani" word for word (term (b) of NOTICE), the version, license and how to cite, with links to the
   repository and the manual; built here, printed by the footer only when $Notebooks *)
$logoAccent = RGBColor[31/255, 78/255, 121/255];
$logoLight = RGBColor[159/255, 197/255, 232/255];
logoWavyPoints[{ax_, ay_}, {bx_, by_}, amplitude_, halfwaves_, n_:120] :=
  Module[{length = Sqrt[(bx - ax)^2 + (by - ay)^2], nx, ny},
    {nx, ny} = {-(by - ay), bx - ax}/length;
    Table[{ax + (bx - ax) t, ay + (by - ay) t} + {nx, ny} amplitude Min[1, 6 t, 6 (1 - t)] Sin[Pi halfwaves t],
      {t, 0., 1., 1./n}]];
(* the arrow heads wider than twice the loop (make_logo.py: 2 x 0.58 x size long, 2 x 0.72 x size wide), so that they
   stand out of it at small sizes *)
logoArrowhead[r_, angle_, size_] :=
  Module[{a = angle Degree, p, tangent, normal},
    p = {64 + r Cos[a], 64 + r Sin[a]}; tangent = {-Sin[a], Cos[a]}; normal = {Cos[a], Sin[a]};
    Polygon[{p + 0.58 size tangent, p - 0.58 size tangent + 0.72 size normal, p - 0.58 size tangent - 0.72 size normal}]];
(* the badge as drawn for small sizes (Logo/FunctionalDiagrams_logo_small.svg: no arrows or dots, heavier lines),
   packageLogo[size, False] the full one *)
packageLogo[size_:30, small_:True] :=
  Module[{r, loop, wave, amplitude, halfwaves},
    {r, loop, wave, amplitude, halfwaves} = If[small, {42, 12, 10, 9, 4}, {38, 6.5, 6, 7.5, 6}];
    Graphics[{
       $logoAccent, Rectangle[{0, 0}, {128, 128}, RoundingRadius -> 28],
       {EdgeForm[{White, Opacity[0.16], Thickness[2/128]}], FaceForm[None],
        Rectangle[{1, 1}, {127, 127}, RoundingRadius -> 27]},
       CapForm["Round"], JoinForm["Round"], $logoLight, Thickness[wave/128],
       Line[logoWavyPoints[{64 - r, 64}, {64 + r, 64}, amplitude, halfwaves]],
       White, Thickness[loop/128], Circle[{64, 64}, r],
       If[small, {}, {logoArrowhead[r, 90, 13], logoArrowhead[r, -90, 13], Disk[{64 - r, 64}, 7], Disk[{64 + r, 64}, 7]}]},
      PlotRange -> {{0, 128}, {0, 128}}, PlotRangePadding -> None, ImagePadding -> None, ImageSize -> size,
      BaselinePosition -> Scaled[0.2]]];
(* lightDarkColor (a colour for light notebooks and one for dark notebooks) is defined with the pictures *)
$repositoryURL = "https://github.com/n4sd4q/FunctionalDiagrams";
(* the name in the blue of the logo (light blue in a dark notebook), the rest in grey (contrast above 4.5:1 on both
   backgrounds); U+2010 (a hyphen, not a minus sign), U+00B7 and U+00A0 by character code, so that the file stays plain
   ASCII. The front end may break a line between any two items of a Row, so the attribution, the version, the license,
   "please cite it: CITATION.cff" and GitHub are each one unbreakable group (LineBreakWithin -> False) ending with its
   separator and a breakable space: in a narrow window the lines wrap after a separator, never before one *)
loadBanner[] :=
  Module[{nbsp = FromCharacterCode[160], dot, group, blue = lightDarkColor[$logoAccent, $logoLight],
     grey = lightDarkColor[GrayLevel[0.45], GrayLevel[0.6]], file = $repositoryURL <> "/blob/main/"},
    dot = nbsp <> nbsp <> FromCharacterCode[183] <> nbsp <> " ";
    group[items__] := Style[Row[{items}], LineBreakWithin -> False];
    Style[Grid[{{packageLogo[30],
       Column[{
         Row[{group[Style["FunctionalDiagrams", Bold, 14, blue], Style[", by Nazzareno Africani", 14, blue],
             Style[dot, 12, grey]],
           Style["functional derivatives and the Feynman diagrams they generate", 12, grey]}],
         Style[Row[{group["version " <> $FunctionalDiagramsVersion, dot],
             group[StringReplace["GPL-3.0-or-later", "-" -> FromCharacterCode[8208]], dot],
             group["please cite it: ", Hyperlink["CITATION.cff", file <> "CITATION.cff"], dot],
             group[Hyperlink["GitHub", $repositoryURL], dot],
             Hyperlink["Manual", file <> "Documentation/FunctionalDiagrams_Manual.pdf"]}], 11, grey]},
        Spacings -> 0.1]}},
      Alignment -> {Left, Center}, Spacings -> {0.7, 0}], FontFamily -> "Source Sans Pro", LineIndent -> 0]];
(* in a notebook (not in scripts, which stay quiet), the banner *)
If[TrueQ[$Notebooks], Print[loadBanner[]]];

End[];
EndPackage[];
