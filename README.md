<p align="center">
  <img src="Logo/FunctionalDiagrams_logo.svg" width="120" alt="FunctionalDiagrams logo: the exchange vacuum diagram">
</p>

<h1 align="center">FunctionalDiagrams</h1>

<p align="center">
  <b>Feynman diagrams from functional derivatives</b><br>
  a Mathematica package for perturbation theory, effective actions and many-body physics
</p>

<p align="center">
  <a href="https://github.com/n4sd4q/FunctionalDiagrams/releases/latest"><img src="https://img.shields.io/github/v/release/n4sd4q/FunctionalDiagrams?label=release&color=1F4E79" alt="latest release"></a>
  <img src="https://img.shields.io/badge/Mathematica-12.1%2B-1F4E79" alt="Mathematica 12.1 or later">
  <a href="Tests/RunTests.out"><img src="https://img.shields.io/badge/tests-862%20passed-2E7D32" alt="862 tests passed"></a>
  <a href="https://github.com/n4sd4q/FunctionalDiagrams/actions/workflows/consistency.yml"><img src="https://github.com/n4sd4q/FunctionalDiagrams/actions/workflows/consistency.yml/badge.svg" alt="consistency check"></a>
  <img src="https://img.shields.io/badge/license-GPL--3.0--or--later-1F4E79" alt="GNU GPL v3 or later">
</p>

<p align="center">
  <a href="Showcase/FunctionalDiagrams_Showcase.pdf"><b>Showcase</b></a>
  ·&nbsp;<a href="Documentation/FunctionalDiagrams_Manual.pdf"><b>Manual</b></a>
  ·&nbsp;<a href="Examples.wl"><b>Guided&nbsp;tour</b></a>
  ·&nbsp;<a href="https://github.com/n4sd4q/FunctionalDiagrams/wiki"><b>Wiki</b></a>
  ·&nbsp;<a href="#installation"><b>Installation</b></a>
</p>

FunctionalDiagrams is a Mathematica package that derives Feynman diagrams the way one does on paper: from a generating
functional and an interaction, by functional derivatives with respect to the sources and the propagators, with the
fermion signs of the Grassmann algebra and the symmetry factors that the derivatives produce. It is meant for many-body
physics and effective actions (Luttinger–Ward functionals, vertices, Bethe–Salpeter, parquet and Hedin's equations, *GW*,
pairing), and also reaches relativistic field theory: φ⁴ theory and one-loop QED in dimensional regularization, and
the diagrams of QCD.

The diagrams it produces can be compared up to relabelling, sorted into classes (connected, one- and two-particle
irreducible, skeleton, reducible in a channel), differentiated again, checked against exact equations, evaluated in a
few common cases, and drawn in Mathematica or exported as TikZ. Every step is worked out for φ⁴ theory and for the
electron gas.

<p align="center">
  <a href="https://n4sd4q.github.io/FunctionalDiagrams/Media/demo.mp4"><img src="Media/demo_preview.webp" width="720" alt="Preview of the demo video: the
  Luttinger–Ward functional and its derivatives Sigma and Gamma0, the Bethe–Salpeter and parquet equations of the
  electron gas, Hedin's equations"></a><br>
  <sub>A preview of the <a href="https://n4sd4q.github.io/FunctionalDiagrams/Media/demo.mp4">demo video</a> (an MP4 file,
  also in the repository as <a href="Media/demo.mp4"><code>Media/demo.mp4</code></a>): the first example below typed and
  run, then a few other results.</sub>
</p>

## Installation

The package needs Mathematica 12.1 or later. Install the paclet of the latest
[release](https://github.com/n4sd4q/FunctionalDiagrams/releases) once (the button at the right of the box copies the
line):

```mathematica
PacletInstall["https://github.com/n4sd4q/FunctionalDiagrams/releases/download/v2.4.0/FunctionalDiagrams-2.4.0.paclet"]
```

then load it in every session:

```mathematica
Needs["FunctionalDiagrams`"]
```

The paclet also contains the showcase, the manual and the guided tour. The latest release is 2.4.0, which this page and
the documents describe (see the [changelog](CHANGELOG.md)). One can also download the `.paclet` file of the release
and pass its path to `PacletInstall`. A clone of the repository works as it is, with
`PacletDirectoryLoad["/path/to/FunctionalDiagrams"]` and ``Needs["FunctionalDiagrams`"]``, or simply
`Get["/path/to/FunctionalDiagrams/FunctionalDiagrams.wl"]`. ``?FunctionalDiagrams`*`` lists the functions and `?Name`
explains each one. Loaded in a notebook, the package contacts `api.github.com` once a day, in the background, and adds a
line to its banner when a newer release is out; `$FunctionalDiagramsUpdateCheck = False` before `Needs` turns this off.
Updating, uninstalling and the common problems: the wiki's
[Installation](https://github.com/n4sd4q/FunctionalDiagrams/wiki/Installation) page.

## A first example

```mathematica
Needs["FunctionalDiagrams`"]

(* free fermions, G0 = -<psi psibar>, and a two-body interaction V between densities *)
DeclareGrassmannSource[eta, etabar];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Z0 = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareSymmetric[V]; DeclareInteractionLine[V]; DeclareDirectedPropagator[G0, G];
interaction =
  1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];

(* the Green function to second order: 1 + 2 + 10 diagrams, no vacuum bubbles *)
DiagramPlot[
  -PerturbativeCorrelationFunction[Z0, {psi[x], psibar[y]}, interaction, 2], {x, y}]

(* the Luttinger–Ward functional and its derivatives: Sigma(x, y) = dPhi/dG(y, x) and
   Gamma0(x1, x2; x3, x4) = dSigma(x1, x2)/dG(x4, x3), the irreducible vertex
   (the shortcuts SelfEnergy and IrreducibleVertex give the same) *)
Phi = LuttingerWardFunctional[Z0, interaction, 3, G0 -> G];
DiagramPlot[Phi]
DiagramPlot[FunctionalDerivative[Phi, G[y, x]], {x, y}]
DiagramPlot[FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]], {x1, x2, x3, x4}]
```

<p align="center"><img src="Documentation/readme_1.svg" width="600" alt="The Luttinger–Ward functional Phi[G] of the
electron gas to second order, the self-energy Sigma = dPhi/dG and the irreducible vertex Gamma0 = dSigma/dG"></p>
<p align="center">The output of the last three lines, up to second order: Φ[G] and its derivatives Σ and Γ⁰. A wavy
line stands for −V, so that the number in front of a picture times the values of its lines is the term; the legs of Γ⁰
are x₁ (out), x₂ (in), x₃ (out), x₄ (in), counterclockwise from the top left.</p>

The [guided tour](Examples.nb) ([`Examples.wl`](Examples.wl), the same as a script, can be read on GitHub) goes on from
here in thirteen short sections: the effective actions, the Bethe–Salpeter and parquet equations, pairing, Hedin's
equations and *GW*, and then linear response, Matsubara sums, phonons, disorder, dimensional regularization, QED and
QCD. The [showcase](Showcase/FunctionalDiagrams_Showcase.pdf) covers everything in more depth; every picture and number
in it is computed by the package.

## Gallery

Each panel is drawn by the package from results it computes; the showcase derives and checks all of them.

<p align="center"><img src="Documentation/readme_2.svg" width="600" alt="Exact equations of many-body theory drawn by
the package: the Dyson equation, the Bethe–Salpeter equation, the equation of the vertex of the density and the parquet
decomposition of the full vertex"></p>
<p align="center"><b>Exact equations.</b> Drawn with blobs and checked order by order against perturbation theory:
Dyson, Bethe–Salpeter, the vertex Λ of the density, and the parquet decomposition of the full vertex F.</p>

<p align="center"><img src="Documentation/readme_3.svg" width="600" alt="Hedin's equations for Sigma, W, P and the vertex,
and the functional Phi[G, W] with its derivatives, the GW self-energy and the polarization"></p>
<p align="center"><b>Screening.</b> Hedin's equations (the double wavy line is the screened interaction <i>W</i>), and
the 2PI functional Φ[G, W] of the dressed lines (here the wavy ones are <i>W</i>) with its derivatives: the first diagram
gives the <i>GW</i> approximation, the next ones its vertex corrections.</p>

<p align="center"><img src="Documentation/readme_4.svg" width="600" alt="Pairing with the Nambu superfield: the first
order of Phi in the normal and anomalous propagators, the Gor'kov equations, the Hartree–Fock self-energy and the BCS gap"></p>
<p align="center"><b>Pairing.</b> With the Nambu superfield (ψ, ψ̄) the anomalous propagators (lines with two arrows)
are variables of Φ: the first order of Φ, the Gor'kov equations, and the Hartree–Fock self-energy and the BCS gap Δ.</p>

<p align="center"><img src="Documentation/readme_5.svg" width="600" alt="Field theory: the second-order self-energy of
phi^4 theory in dimensional regularization with its pole and the two-loop beta function; the one-loop electron
self-energy, vacuum polarization and vertex of QED with delta2, beta(e) and F2(0) = alpha/2pi; the one-loop gluon
self-energy of QCD"></p>
<p align="center"><b>Field theory.</b> φ⁴ theory in dimensional regularization: the two-loop self-energy with its pole,
and the β function. One-loop QED: the three 1PI functions, with δ₂, β(e) and the anomalous magnetic moment. QCD: the
one-loop gluon self-energy.</p>

## Documentation

| Document | Contents |
|---|---|
| [**Showcase**](Showcase/FunctionalDiagrams_Showcase.pdf) | an introduction to the diagrammatic methods through the package, in three parts. Many-body theory: perturbation theory, the self-energy, linear response, Matsubara sums, the 1PI and 2PI effective actions, the grand potential, the Bethe–Salpeter equation from the response to a one-body source, vertices and response functions, the parquet equations from the derivatives of the 2PI action, pairing with the superfield of Nambu and Gor'kov, Hedin's equations, Hubbard–Stratonovich and *GW*, Potthoff's functional and divergent vertices, phonons and disorder. Quantum field theory: dimensional regularization, one-loop QED, the vertices of QCD. Tools: pictures and formulas, and what the package does under the hood |
| [**Manual**](Documentation/FunctionalDiagrams_Manual.pdf) | every function, option and convention, with examples computed by the package |
| [**Guided tour**](Examples.nb) | a notebook to run in Mathematica (GitHub shows its source: download it with *Download raw file*); [`Examples.wl`](Examples.wl) is the same as a script, readable on GitHub |
| [**Wiki**](https://github.com/n4sd4q/FunctionalDiagrams/wiki) | practical guides and a FAQ: installing and updating, the conventions in one place, declaring a theory, reading the weights, exact equations, evaluating diagrams, pictures in LaTeX, scripts |

## What it does

**Diagrams from a generating functional**
- functional derivatives with bosonic and Grassmann sources, chain rule, implicit dependence; a canonical form in which
  equal diagrams are recognized (`FunctionalDerivative`, `SimplifyDiagrams`)
- Wick's theorem; Z[J] and W = log Z order by order, at zero or non-zero source; vacuum bubbles and the linked-cluster
  theorem (`PerturbativeCorrelationFunction`, `ConnectedCorrelationFunction`, `ConnectedVacuumDiagrams`)
- classes of diagrams: connected, one- and two-particle irreducible, skeleton, reducible in a channel; amputation;
  Dyson series (`ParticleIrreducibleDiagrams`, `SkeletonDiagrams`, `IrreducibleDiagrams`, `AmputateExternalLegs`)

**Effective actions and exact equations**
- 1PI vertices as derivatives of the 1PI action, and connected functions as trees of propagators and vertices, for
  bosons and fermions (`ConnectedFunctionFromVertices`; the shortcut `PerturbativeFullVertex` computes the vertices from
  the connected functions)
- the Luttinger–Ward functional Φ[G], with Σ = δΦ/δG and Γ⁰ = δΣ/δG taken by `FunctionalDerivative`, also for several
  propagators (`LuttingerWardFunctional`; the shortcuts `SelfEnergy`, `IrreducibleVertex`)
- the 2PI effective action Γ[G] and Γ[φ̄, G], the 1PI effective action Γ[φ̄] and Potthoff's self-energy functional as
  objects, with traces of operators, derivatives with respect to two-index objects, the grand potential and conserving
  responses evaluated in Matsubara frequency, and exact zero-dimensional checks (`EffectiveAction2PI`,
  `EffectiveAction1PI`, `SelfEnergyFunctional`, `TraceLog`, `ZeroDimensionalValue`)
- Bethe–Salpeter ladders and the parquet decomposition, for fermions (ph, ph̄, pp) and bosons (s, t, u): the fully
  irreducible vertex and the kernels of the channels as derivatives of the 2PI action, the classification by cutting
  lines as the check (`BetheSalpeterLadder`, `ParquetDecomposition`)
- superfields: the Nambu propagator of (ψ, ψ̄), anomalous parts included, as the variable of the 2PI functional, which
  gives the kernels of all three channels, is exact at finite means and yields Hartree–Fock–Bogoliubov and the BCS gap
  equation at first order (`"Statistics" -> "Superfield"`, `SuperfieldComponents`)
- Dyson, Bethe–Salpeter, parquet, Schwinger–Dyson and Hedin's equations and the vertex of a one-body operator, drawn
  with blobs or checked order by order against perturbation theory (`ExactEquation`, `CheckEquation`, `DeclareBlob`)
- Hubbard–Stratonovich fields in both directions, and the functional Φ[G, W] whose first diagram is the *GW*
  approximation (`HubbardStratonovich`, `IntegrateOutField`)

**Evaluating diagrams**
- momenta and Matsubara frequencies routed through a diagram, frequency sums by residues with Fermi and Bose
  functions, analytic continuation, and the cutting rules at finite temperature (`EvaluateMatsubara`, `MatsubaraSum`,
  `CheckCuttingRules`)
- loop integrals in dimensional regularization, done one loop after the other (tadpoles, bubbles, G-functions), with
  the Laurent expansion in ε, MS-bar Z factors and renormalization-group functions; φ⁴ theory to two loops
  (`DiagramLoopIntegral`, `IntegrateLoops`, `EpsilonExpand`, `BetaFunctionFromZ`)
- QED at one loop: Dirac algebra in d dimensions, Feynman parameters and tensor reduction, the electron self-energy, the
  vacuum polarization and the vertex, with their renormalization constants and β(e) (`QEDIntegrand`, `DiracSimplify`, `IntegrateOneLoop`,
  `VertexFormFactors`)

The evaluation part is deliberately small: it covers the integrals that come up in the examples, not general loop
calculations.

**Pictures and formulas**
- diagrams in the usual style of many-body papers (fermion lines straight, loops as circles, ladders as ladders,
  symmetric diagrams drawn symmetric), exact equations with blobs, and formulas as LaTeX (`DiagramPlot`,
  `DiagramToTikZ`, `ExpressionToLaTeX`); in a notebook the pictures follow its light or dark appearance

The theories are written in condensed notation, so the same functions work for real and complex bosons, fermions,
phonons, disorder and QCD (quarks, gluons, ghosts).

## Map of the functions

<p align="center"><img src="Documentation/function_map.svg" width="100%" alt="Map of the public functions of
FunctionalDiagrams in six stages of a calculation: declare the symbols, write a functional, differentiate it with
FunctionalDerivative (the hub, with the functions built on it hanging from it), then classify and check, draw and
evaluate the diagrams"></p>

The public functions by their role in a calculation: declare the symbols, write a functional, differentiate it, then
classify, draw and evaluate the diagrams. The colour of a name is its group in the manual; dashed names are inert heads
used as data. A name indented under another is built on it, and *uses* lists the other names it uses. The
[interactive version](https://n4sd4q.github.io/FunctionalDiagrams/Documentation/function_map.html) has every
dependency: select a name to read its usage message and to see the lines to the names it uses and from the names that
use it (the file is [`Documentation/function_map.html`](Documentation/function_map.html): GitHub shows only its source,
so download it and open it in a browser).

## Checks

The results are checked against each other, order by order, and the showcase reports every residual. For the electron
gas and φ⁴ theory to third order these include the Dyson equation and the linked-cluster theorem, Σ[G] against the
skeleton part of the perturbative Σ, the Ward identity, the Bethe–Salpeter and parquet equations in every channel, the
Schwinger–Dyson, vertex and Hedin equations, and a Hubbard–Stratonovich theory against the original one. The evaluation
is checked against numbers: frequency sums against brute-force sums, imaginary parts against the cutting rules, loop
integrals against numerical integration in non-integer dimension, and the renormalization of φ⁴ theory and of one-loop
QED against the textbook results (β functions, anomalous dimensions, the Ising exponents at the Wilson–Fisher fixed
point, Z₁ = Z₂, the anomalous magnetic moment).

## Related software

[FeynArts](https://feynarts.de) and [FeynCalc](https://feyncalc.github.io) are the standard Mathematica tools for
scattering amplitudes: FeynArts generates the diagrams of a process from a model's Feynman rules, and FeynCalc does the
Dirac and colour algebra and the reduction of loop integrals. [DoFun](https://github.com/markusqh/DoFun) derives
Dyson–Schwinger and functional renormalization group equations. FunctionalDiagrams starts from the generating functional
instead, and is meant for the objects of many-body theory and effective actions: correlation and vertex functions,
Φ[G] and its derivatives, channels and parquet equations, self-consistent approximations. Its evaluation tools are much
more limited than FeynCalc's, so for real loop calculations the two are better used together.

## Details

<details>
<summary>Conventions</summary>

* `IntegralOver[f, {x, y, ...}]` is the integral (or sum) over the labels `x, y, ...`, which stand for position, time,
  spin and any other index; `DeltaFunction[x, y]` is the delta function. Labels are symbols. The diagrams do not depend
  on what a label is: the same results hold in any dimension, at zero or finite temperature, and on the Keldysh contour.
* Grassmann-odd factors are multiplied with `**`; `FunctionalDerivative[F, s1, ..., sn]` is the left derivative
  δ/δs1 … δ/δsn F, the rightmost acting first.
* Results are in canonical form: the integration variables are renamed `z1, z2, ...`, so that equal diagrams are
  recognized and `SimplifyDiagrams[a - b]` is 0 exactly when `a` and `b` are equal.
* Fermions: `G0 = -<psi psibar>`, the free generating functional is `Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]]`,
  `psi` ↔ δ/δ`etabar`, `psibar` ↔ −δ/δ`eta`; `G[x, y]` is drawn as an arrow from `y` to `x`. The interaction is
  `1/2 ∫ V(x, y) psibar(x) psibar(y) psi(y) psi(x)`.
* The number written in front of a picture, times the values of its elements, is the term: a propagator stands for
  itself, an interaction line for −V and a vertex for minus its coupling (−λ in φ⁴ theory, `DeclareCoupling`): the minus
  sign comes from expanding exp(−S_int). So Fock has the weight +1, Hartree −1 (its closed fermion loop), the φ⁴ tadpole +½.
* QED: the electron has the charge −e, with e > 0 everywhere, and the vertex is +ieγ^μ (Peskin and Schroeder with e → −e).
* Σ(x, y) = δΦ/δG(y, x) and Γ⁰(x1, x2; x3, x4) = δΣ(x1, x2)/δG(x4, x3): fermions leave a vertex at `x1, x3` and enter at
  `x2, x4`. In code: `FunctionalDerivative[Phi, G[y, x]]` and `FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]]`.
* Real bosons (`G = <phi phi>`, declared symmetric): Σ(x, y) = −2δΦ/δG(x, y) and Γ⁰ = −2δΣ/δG = 4δ²Φ/δGδG, the kernel of
  the channel s; for the Nambu field (ψ, ψ̄) with its antisymmetric propagator 𝒢 the same, Σ = −2δΦ/δ𝒢. The shortcuts take
  `"Statistics" -> "Boson"` or `"Superfield"` (without it, on a symmetric G, a message).
* Names of propagators and fields must not be built-in symbols (`GreenFunction`, `Gamma`, `C`, `D`, `E`, `I`, `K`, `N`,
  `O`, …): the package warns. Hence `BareGreenFunction`, `DressedGreenFunction`.

</details>

<details>
<summary>Performance</summary>

Wick's theorem is applied as a sum over pairings in which the interchangeable legs of a vertex are contracted only once,
with their multiplicity, and the canonical form uses colour refinement and individualization, as graph-isomorphism
programs do; the refinement (of most terms) and the tests of irreducibility run on integers, in compiled functions. As
scripts, the guided tour runs in about 12 s, the test suite in about a minute (twenty-five seconds without the effective
actions), and the script that computes and draws everything in the showcase in a little over two minutes.

</details>

<details>
<summary>Contents of the repository</summary>

| File | Contents |
|---|---|
| `FunctionalDiagrams.wl` | the package, a single file |
| `Examples.nb`, `Examples.wl` | the guided tour (notebook, and the same as a script) |
| `Showcase/` | the showcase: `make_figures.wl` computes and draws everything in it, `build_pdf.sh` typesets it |
| `wiki/` | the pages of the wiki, published to it by `.github/workflows/wiki.yml` |
| `Documentation/` | the reference manual (`make_examples.wl` computes its examples, `build_manual.sh` typesets it) and the map of the functions |
| `Tests/` | the test suite `RunTests.wl` and its last output `RunTests.out`; `QEDOneLoop.wl`, the one-loop QED computation used by the tests; `MatsubaraBruteForce.wl`, the brute-force Matsubara sums used by the tests and the showcase; `EffectiveActions.wl`, the tests of the effective actions, with two slow references stored in `FullyIrreducibleVertices.m`; `Superfields.wl`, the tests of the superfields; `Harness.wl`, the helpers they share |
| `Media/` | the demo video and its preview |
| `Logo/` | the logo: the exchange (Fock) vacuum diagram, one of the two first-order diagrams of the Luttinger–Ward functional |
| `Development/` | the sources of the package, one file per part, and the scripts that assemble it, build the paclet and draw the pictures of this page |
| `PacletInfo.wl` | the paclet: the repository can be loaded as it is |
| `index.html` | the landing page of the [GitHub Pages site](https://n4sd4q.github.io/FunctionalDiagrams/) |
| `CHANGELOG.md`, `CITATION.cff`, `CONTRIBUTING.md`, `LICENSE`, `NOTICE` | the history, how to cite, how to report a problem or contribute, the text of the license, the copyright and the additional terms |

</details>

<details>
<summary>Running the tests</summary>

```sh
wolfram -script Tests/RunTests.wl     # or: wolframscript -file Tests/RunTests.wl
```

Every check prints PASS or FAIL; `Tests/RunTests.out` is the output of the last run.

</details>

## Citing

If you use FunctionalDiagrams in published work, please cite it as described in [`CITATION.cff`](CITATION.cff)
(GitHub shows a *Cite this repository* button), for example

```bibtex
@software{Africani_FunctionalDiagrams,
  author = {Africani, Nazzareno},
  title  = {{FunctionalDiagrams}: functional derivatives and {Feynman}
            diagrams in {Mathematica}},
  year   = {2026},
  url    = {https://github.com/n4sd4q/FunctionalDiagrams}
}
```

The history of the package is in [`CHANGELOG.md`](CHANGELOG.md). Problems, questions and suggestions are welcome as
issues: see [`CONTRIBUTING.md`](CONTRIBUTING.md).

## Star history

<p align="center"><a href="https://star-history.com/#n4sd4q/FunctionalDiagrams&amp;Date"><img
src="https://api.star-history.com/svg?repos=n4sd4q/functionaldiagrams&amp;type=Date" width="600" alt="Star history
of FunctionalDiagrams: the number of stars of the repository over time"></a></p>

## License

Released under the [GNU General Public License, version 3 or later](LICENSE), with two additional terms: the attribution
to the author must be kept, and modified versions must say that they are modified, by whom and when (see
[`NOTICE`](NOTICE), with the copyright and the terms). Author: Nazzareno Africani (SISSA).
