(* ::Package:: *)

(* The paclet of FunctionalDiagrams: the package FunctionalDiagrams.wl, loaded with Needs["FunctionalDiagrams`"] once
   the paclet is installed (PacletInstall) or its folder is known to the paclet manager (PacletDirectoryLoad).
   Development/build_paclet.wl builds the archive FunctionalDiagrams-<version>.paclet. *)

PacletObject[<|
  "Name" -> "FunctionalDiagrams",
  "Version" -> "2.3.0",
  "WolframVersion" -> "12.1+",
  "Description" -> "Functional derivatives and Feynman diagrams: perturbation theory, effective actions and many-body physics",
  "Creator" -> "Nazzareno Africani",
  "License" -> "GPL-3.0-or-later",
  "URL" -> "https://github.com/n4sd4q/FunctionalDiagrams",
  "Keywords" -> {"functional derivatives", "Feynman diagrams", "perturbation theory", "effective action",
    "2PI effective action", "Luttinger-Ward functional", "parquet equations", "Nambu-Gor'kov", "Matsubara sums",
    "dimensional regularization", "QED", "TikZ"},
  "Extensions" -> {
    {"Kernel", "Root" -> ".", "Context" -> "FunctionalDiagrams`"},
    {"Asset", "Assets" -> {{"Showcase", "Showcase/FunctionalDiagrams_Showcase.pdf"},
      {"Manual", "Documentation/FunctionalDiagrams_Manual.pdf"}, {"Examples", "Examples.nb"}}}
  }
|>]
