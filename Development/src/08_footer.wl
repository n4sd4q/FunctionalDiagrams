
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
   "LegendrePropagator", "SelfEnergyFunctional", "ZeroDimensionalValue", "SuperfieldComponents",
   "FunctionalDiagramsUpdateCheck"};
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
   separator and a breakable space: in a narrow window the lines wrap after a separator, never before one.
   loadBanner[release] adds a third line, the notice of a newer release (updateNotice) *)
bannerDot[] := With[{nbsp = FromCharacterCode[160]}, nbsp <> nbsp <> FromCharacterCode[183] <> nbsp <> " "];
bannerGroup[items__] := Style[Row[{items}], LineBreakWithin -> False];
bannerBlue[] := lightDarkColor[$logoAccent, $logoLight];
bannerGrey[] := lightDarkColor[GrayLevel[0.45], GrayLevel[0.6]];
loadBanner[release_ : None] :=
  Module[{dot = bannerDot[], group = bannerGroup, blue = bannerBlue[], grey = bannerGrey[],
     file = $repositoryURL <> "/blob/main/"},
    Style[Grid[{{packageLogo[30],
       Column[{
         Row[{group[Style["FunctionalDiagrams", Bold, 14, blue], Style[", by Nazzareno Africani", 14, blue],
             Style[dot, 12, grey]],
           Style["functional derivatives and the Feynman diagrams they generate", 12, grey]}],
         Style[Row[{group["version " <> $FunctionalDiagramsVersion, dot],
             group[StringReplace["GPL-3.0-or-later", "-" -> FromCharacterCode[8208]], dot],
             group["please cite it: ", Hyperlink["CITATION.cff", file <> "CITATION.cff"], dot],
             group[Hyperlink["GitHub", $repositoryURL], dot],
             Hyperlink["Manual", file <> "Documentation/FunctionalDiagrams_Manual.pdf"]}], 11, grey],
         If[AssociationQ[release], updateNotice[release], Nothing]},
        Spacings -> 0.1]}},
      Alignment -> {Left, Center}, Spacings -> {0.7, 0}], FontFamily -> "Source Sans Pro", LineIndent -> 0]];

(* the update check. In a notebook (never in a script), at most once a day, the package asks GitHub for its latest
   release in the background (URLSubmit: loading never waits for the network) and, if it is newer than the version
   loaded, the banner gets a third line with the PacletInstall line of its paclet. Any failure (offline, a proxy, a
   time limit, the rate limit of GitHub, an answer without the fields) is silent. The handler of the answer runs after
   the evaluation that loaded the package, when a Print would go to the Messages window: it writes the banner again
   in its own cell, found by a tag given to it at load, without moving the selection (NotebookWrite to a CellObject).
   FunctionalDiagramsUpdateCheck[] asks by hand, and waits for the answer *)
$latestReleaseURL = "https://api.github.com/repos/n4sd4q/FunctionalDiagrams/releases/latest";
latestReleaseRequest[] := HTTPRequest[$latestReleaseURL, <|"Headers" -> {"Accept" -> "application/vnd.github+json"}|>];
(* the date of the last check at load, in a small file of the user's base directory *)
$updateCheckDirectory := FileNameJoin[{$UserBaseDirectory, "ApplicationData", "FunctionalDiagrams"}];
updateCheckDate[] := DateString[{"Year", "-", "Month", "-", "Day"}];
(* options of URLRead and URLSubmit, those that the version of Mathematica knows *)
networkOptions[f_, opts_List] := FilterRules[opts, Options[f]];

(* the numbers of a version "2.10.0" or of a tag "v2.10.0"; $Failed for anything else (a pre-release "v2.5.0-rc1") *)
versionNumbers[s_String] := If[StringMatchQ[s, RegularExpression["v?[0-9]+\\.[0-9]+\\.[0-9]+"]],
   FromDigits /@ StringSplit[StringReplace[s, StartOfString ~~ "v" -> ""], "."], $Failed];
versionNumbers[_] := $Failed;
(* whether version a is newer than version b, number by number (2.10.0 is newer than 2.9.0); False if either is not a
   version. The canonical order of two lists of three integers is that of their numbers, from the first *)
newerVersionQ[a_, b_] := With[{x = versionNumbers[a], y = versionNumbers[b]},
   ListQ[x] && ListQ[y] && Order[y, x] === 1];

(* the latest release in the answer of GitHub (JSON): <|"Version" -> "2.5.0", "Paclet" -> url of its .paclet,
   "Page" -> url of its page|>, or $Failed for a draft or a pre-release, a malformed tag or no paclet of this
   repository. ImportString reads bytes: the text (with the release notes) is encoded in UTF-8 first. The paclet's
   address must be one of this repository's downloads, made of plain characters, since it is shown inside a line of
   code to copy *)
latestReleaseOf[body_String] := Module[{json, tag, version, paclets},
   json = Quiet[ImportString[FromCharacterCode[ToCharacterCode[body, "UTF8"]], "RawJSON"]];
   If[!AssociationQ[json], Return[$Failed]];
   tag = Lookup[json, "tag_name"];
   If[TrueQ[Lookup[json, "draft"]] || TrueQ[Lookup[json, "prerelease"]] || !ListQ[versionNumbers[tag]], Return[$Failed]];
   version = StringReplace[tag, StartOfString ~~ "v" -> ""];
   paclets = Cases[Lookup[json, "assets", {}], KeyValuePattern["browser_download_url" -> url_String] /;
       StringMatchQ[url, RegularExpression[StringReplace[$repositoryURL, "." -> "\\."] <> "/releases/download/" <>
          StringReplace[tag, "." -> "\\."] <> "/[A-Za-z0-9._-]+\\.paclet"]] :> url];
   If[paclets === {}, $Failed,
    <|"Version" -> version, "Paclet" -> First[paclets], "Page" -> $repositoryURL <> "/releases/tag/" <> tag|>]];
latestReleaseOf[_] := $Failed;
installLine[release_Association] := "PacletInstall[\"" <> release["Paclet"] <> "\"]";

(* the third line of the banner: the newer version, a link to its page and the line that installs it, as code to copy
   (its hyphens not drawn as dashes: "OperatorSubstitution" -> False); in a script, the same as text *)
updateNotice[release_Association] := With[{dot = bannerDot[], group = bannerGroup},
   Style[Row[{group[Style["version " <> release["Version"] <> " is available", Bold, bannerBlue[]], dot],
      group[Hyperlink["what is new", release["Page"]], dot],
      With[{url = release["Paclet"]}, Style[Defer[PacletInstall[url]], ShowStringCharacters -> True,
        PrivateFontOptions -> {"OperatorSubstitution" -> False}]]}], 11, bannerGrey[]]];
updateNoticeText[release_Association] :=
  "FunctionalDiagrams " <> release["Version"] <> " is available (" <> $FunctionalDiagramsVersion <> " is loaded): " <>
   installLine[release];

(* whether the check at load is due today: the file LastUpdateCheck.txt of dir holds the date of the last check; the
   date of today is written into it before asking. Any error reading or writing it: the check is due *)
updateCheckDueQ[dir_String, today_String] := Module[{file = FileNameJoin[{dir, "LastUpdateCheck.txt"}], last, stream},
   last = Quiet[ReadString[file]];
   If[StringQ[last] && StringTrim[last] === today, Return[False]];
   Quiet[If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    stream = OpenWrite[file];
    If[Head[stream] === OutputStream, WriteString[stream, today <> "\n"]; Close[stream]]];
   True];

(* the asynchronous request at load; its handler writes the banner with the notice into the cell tagged tag of the
   notebook nb, if the latest release is newer. Every step is quiet: a handler's messages would go to the Messages
   window *)
$updateCheckTask = None;
submitUpdateCheck[nb_, tag_String] := Quiet[
   $updateCheckTask = URLSubmit[latestReleaseRequest[],
     HandlerFunctions -> <|"TaskFinished" -> (showUpdateNotice[nb, tag, #] &)|>,
     HandlerFunctionsKeys -> {"StatusCode", "Body"}, Sequence @@ networkOptions[URLSubmit, {TimeConstraint -> 30}]]];
showUpdateNotice[nb_, tag_String, answer_] := Quiet[Module[{release, cells},
    release = If[Lookup[answer, "StatusCode"] === 200, latestReleaseOf[Lookup[answer, "Body"]], $Failed];
    If[AssociationQ[release] && newerVersionQ[release["Version"], $FunctionalDiagramsVersion],
     cells = Cells[nb, CellTags -> tag];
     If[ListQ[cells] && Length[cells] == 1,
      NotebookWrite[First[cells], ReplacePart[NotebookRead[First[cells]], 1 -> BoxData[ToBoxes[loadBanner[release]]]]]]]]];

FunctionalDiagramsUpdateCheck::latest = "The latest release could not be read from GitHub (`1`).";
FunctionalDiagramsUpdateCheck[] := Module[{response, status, release},
   response = Quiet[URLRead[latestReleaseRequest[], Sequence @@ networkOptions[URLRead, {TimeConstraint -> 10}]]];
   status = If[Head[response] === HTTPResponse, response["StatusCode"], None];
   release = If[status === 200, latestReleaseOf[response["Body"]], $Failed];
   If[!AssociationQ[release],
    Message[FunctionalDiagramsUpdateCheck::latest, Which[status === None, "no answer", status =!= 200,
       "status " <> ToString[status], True, "no release with a paclet in the answer"]];
    $Failed,
    If[newerVersionQ[release["Version"], $FunctionalDiagramsVersion],
     Print[If[TrueQ[$Notebooks], Style[updateNotice[release], FontFamily -> "Source Sans Pro"], updateNoticeText[release]]]];
    <|"InstalledVersion" -> $FunctionalDiagramsVersion, "LatestVersion" -> release["Version"],
     "InstallLine" -> installLine[release]|>]];

(* the check at load is on unless $FunctionalDiagramsUpdateCheck was set before loading (kept by the start of the
   package, 00_header.wl) *)
If[!ValueQ[$FunctionalDiagramsUpdateCheck], $FunctionalDiagramsUpdateCheck = True];

(* in a notebook (not in scripts, which stay quiet), the banner, and the update check when it is due *)
If[TrueQ[$Notebooks],
  If[$FunctionalDiagramsUpdateCheck =!= False && $AllowInternet =!= False &&
     updateCheckDueQ[$updateCheckDirectory, updateCheckDate[]],
   With[{tag = "FunctionalDiagramsBanner" <> ToString[Round[10^6 AbsoluteTime[]]]},
    CellPrint[ExpressionCell[loadBanner[], "Print", CellTags -> tag]];
    submitUpdateCheck[EvaluationNotebook[], tag]],
   Print[loadBanner[]]]];

End[];
EndPackage[];
