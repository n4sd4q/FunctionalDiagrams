# FunctionalDiagrams

FunctionalDiagrams is a Mathematica package that derives Feynman diagrams by functional derivatives. You write a
generating functional and an interaction; the package differentiates them with respect to the sources and the
propagators, with the fermion signs of the Grassmann algebra and the symmetry factors, and returns the diagrams. It then
recognizes equal diagrams, sorts them into classes, checks them against exact equations (Dyson, Bethe–Salpeter, parquet,
Hedin), evaluates them in a few common cases (Matsubara sums, dimensional regularization, one-loop QED) and draws them,
in Mathematica or as TikZ for a paper.

This wiki collects practical guides and answers to common questions. The documents of the repository explain the package
itself:

| Document | What it gives |
|---|---|
| [README](https://github.com/n4sd4q/FunctionalDiagrams#readme) | an overview, a first example and a gallery |
| [Showcase (PDF)](https://n4sd4q.github.io/FunctionalDiagrams/Showcase/FunctionalDiagrams_Showcase.pdf) | the physics, step by step, for the electron gas and φ⁴ theory |
| [Manual (PDF)](https://n4sd4q.github.io/FunctionalDiagrams/Documentation/FunctionalDiagrams_Manual.pdf) | every function, option and convention, with examples |
| [Guided tour](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Examples.wl) | a notebook to run ([`Examples.nb`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Examples.nb)), and the same as a script |
| [Map of the functions](https://n4sd4q.github.io/FunctionalDiagrams/Documentation/function_map.html) | every public name by its role in a calculation, with its dependencies (interactive) |
| [Demo video](https://n4sd4q.github.io/FunctionalDiagrams/Media/demo.mp4) | the first example of the README, typed and run |
| [Releases](https://github.com/n4sd4q/FunctionalDiagrams/releases) | the paclet of every version, with the two PDFs; the history is in the [changelog](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/CHANGELOG.md) |

## Install

With Mathematica 12.1 or later, once:

<!-- not run: it downloads the paclet -->
```mathematica
PacletInstall["https://github.com/n4sd4q/FunctionalDiagrams/releases/download/v2.4.0/FunctionalDiagrams-2.4.0.paclet"];
```

and then in every session:

```mathematica
Needs["FunctionalDiagrams`"]
```

[Installation](Installation) has the other ways (a clone of the repository, a file downloaded by hand), updating and
the usual problems.

## Pages

- [Installation](Installation): installing, loading, updating and removing the package; which version is loaded
- [Conventions](Conventions): signs, propagators, weights, channels and units, in one place
- Recipes
  - [Declaring a theory](Declaring-a-Theory): fields, sources, propagators, interaction lines, couplings
  - [Reading the weights](Reading-the-Weights): what the number in front of a picture means
  - [Orders and simplification](Orders-and-Simplification): selecting orders, counting and comparing diagrams
  - [Exact equations](Exact-Equations): drawing an equation with blobs and checking it order by order
  - [Particle–particle diagrams](Particle-Particle-Diagrams): channels, and the order of the external points
  - [Evaluating diagrams](Evaluating-Diagrams): Matsubara sums and dimensional regularization
  - [Pictures in LaTeX](Pictures-in-LaTeX): TikZ code for a paper, and formulas
  - [Light and dark notebooks](Light-and-Dark-Notebooks): the colours of the pictures
  - [Working in scripts](Working-in-Scripts): running the package without a notebook
- [FAQ](FAQ)

## How the code on these pages is checked

Every block of Mathematica code on these pages is run, in a fresh kernel for each page, by
[`Development/check_wiki.wl`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Development/check_wiki.wl), and
gives exactly what is shown after it. Outputs are shown as a terminal session prints them (OutputForm: strings without
quotes, fractions over two lines); a notebook shows the same in its compact form, with the integrals as ∫ and the delta
functions as δ. Pictures are what `DiagramPlot` draws, typeset from the TikZ code of the same call. Blocks that download or name a
path of your machine are marked as not run. The pages describe version 2.4.0.
