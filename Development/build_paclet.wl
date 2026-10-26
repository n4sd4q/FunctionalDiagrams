(* ::Package:: *)

(* Builds the paclet archive build/FunctionalDiagrams-<version>.paclet: the package, PacletInfo.wl, the showcase, the
   manual, the guided tour, the license with its NOTICE and the changelog. Install it with PacletInstall["FunctionalDiagrams-<version>.paclet"] (or from
   the URL of a GitHub release) and load the package with Needs["FunctionalDiagrams`"].
   Run from any folder:  wolfram -script Development/build_paclet.wl *)

root = ParentDirectory[DirectoryName[$InputFileName]];
info = Get[FileNameJoin[{root, "PacletInfo.wl"}]];
version = info["Version"];
staging = FileNameJoin[{$TemporaryDirectory, "FunctionalDiagrams-paclet-" <> ToString[$ProcessID]}];
If[DirectoryQ[staging], DeleteDirectory[staging, DeleteContents -> True]];
target = FileNameJoin[{staging, "FunctionalDiagrams"}];
CreateDirectory[target, CreateIntermediateDirectories -> True];
files = {"PacletInfo.wl", "FunctionalDiagrams.wl", "LICENSE", "NOTICE", "README.md", "CHANGELOG.md", "CITATION.cff", "Examples.nb",
   FileNameJoin[{"Showcase", "FunctionalDiagrams_Showcase.pdf"}], FileNameJoin[{"Documentation", "FunctionalDiagrams_Manual.pdf"}]};
(* the PDFs keep their folders, so that the assets of PacletInfo.wl are found both in the paclet and in a clone *)
Do[If[! DirectoryQ[DirectoryName[FileNameJoin[{target, f}]]], CreateDirectory[DirectoryName[FileNameJoin[{target, f}]]]];
  CopyFile[FileNameJoin[{root, f}], FileNameJoin[{target, f}]], {f, files}];
output = FileNameJoin[{root, "build"}];
If[! DirectoryQ[output], CreateDirectory[output]];
archive = CreatePacletArchive[target, output];
DeleteDirectory[staging, DeleteContents -> True];
Print["paclet written: ", archive];
