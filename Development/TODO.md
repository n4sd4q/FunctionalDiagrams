# To do, sooner or later

Things worth doing that are not done yet. A review of the code for speed and clarity (October 2026) is applied, except
the items marked "review" below and the changes without a measurable gain or of style only; a second round merged the
vertex automorphisms, pruned the canonical search by automorphisms and made the brute-force sums and the third orders of
the showcase cheaper; a third round (colour refinement, fast paths, orbit pruning, `SetSourcesToZero` of a held
derivative, `"MaximumOrder"`, the superfield components by orbits, lazy `ExpandTrace`, the TikZ export) halved the
showcase script and took a third off the test suite.

## Package

- **Usability proposals (October 2026).** `SimplifyDiagrams` contracting D . D⁻¹; spin indices in
  `PerturbativeFullVertex`; a component form of Tr ln(−𝒢⁻¹) with anomalous blocks that `FunctionalDerivative` knows (now
  the message `supertrace`); a `DiagramPlot` option for the value of a vertex (+λ).

- **Vertices with more than 5040 automorphisms.** `vertexSlotGroup` gives up (`$Failed`) and the Wick enumeration
  then uses the classes of identical commuting fields as orbits (a subgroup: exact, but less pruned; a vertex of
  twelve fields went from 1.4 to 1.8 s with the merge of the vertex automorphisms). None occurs in the tests or the showcase; generate the
  group from its generators if one does.
- **γ5.** Not implemented: `GammaMatrix[5]` is refused. An anticommuting or 't Hooft–Veltman scheme would be needed for
  anything chiral.
- **Cutting rules.** The finite-temperature cutting rules are checked for chains of lines; vertex corrections (the
  circling rules of Kobes and Semenoff) are not covered.
- **Speed with the full Wolfram Language.** Profile the hot spots and check whether functions beyond the basic ones
  (for example associations as lookup tables, compiled functions, graph functions, `Nearest`, `SparseArray`) would make
  them faster.
- **Options.** Functions of 07, 09 and 10 check the names of their options (`checkOptionNames`) without `Options[f]`;
  those of 03, 04 and 11 have `Options[f]` with literal defaults and no check, so a misspelt `"Statistics"`, `"Channel"`
  or `"MaximumOrder"` is ignored silently and `SetOptions` has no effect on most. One helper (names checked, defaults
  from `Options[f]`) for every function with options.
- **Closed Feynman-parameter integrals in QED (review, a feature).** `inertParameterIntegral` (`10_qed.wl`) leaves the
  one-dimensional coefficients of Σ₂ and Π₂ as `ParameterIntegral`; the closed forms of the massive bubble in
  `09_dimreg.wl` (∫ w Δ^n0 ln^k Δ, roots of the Källén function) could give them in closed form. It changes printed and
  tested results, so it is the author's decision. The two engines should stay separate otherwise (different i0, variables
  and output forms).
- **Display forms of the integral heads (review).** `09_dimreg.wl` and `10_qed.wl` declare display forms of the same
  shapes for their integral heads. Left as one-line declarations next to each head: a loop injecting the heads would be
  less clear. Merge them if a third family of integrals appears.

## Effective actions as objects (done in October 2026, `11_effective_actions.wl`; what is left)

Done: `EffectiveAction2PI` (Γ[G] for fermions, real and complex bosons; Γ[φ̄, G] of Cornwall, Jackiw and Tomboulis with
`"MeanFields"`, Grassmann-odd means for fermions; a one-body source `"Source"`), `EffectiveAction1PI` (real boson),
Potthoff's `LegendreTransform`, `LegendrePropagator` and `SelfEnergyFunctional`, the operator traces (`TraceLog`,
`OperatorTrace`, `OperatorKernel`, `ExpandTrace`), `ZeroDimensionalValue`, `FunctionalDerivative` with respect to
two-index objects, Tr ln in `MatsubaraSum` and `EvaluateMatsubara` (`Temperature`), the static and dynamic limits
(`"Limit"`), `ExactEquation` `"Response"` and `"Susceptibility"` with `"BareVertex"`. Checked (`Tests/EffectiveActions.wl`):
δΓ/δG = 0 is Dyson, Γ = −log Z order by order, −∂Ω/∂μ = N with the three orderings, the compressibility sum rule
(Hartree–Fock vs time-dependent Hartree–Fock on a ring), the counterterm representation of Φ, Galitskii–Migdal,
Γ_irr = δ⁴Γ/δφ̄⁴ at fixed G = the fully irreducible vertex to λ⁵ and V⁴, the 1PI vertices, exact zero-dimensional Legendre transforms,
the two branches of Σ[G] and the divergence of Γ⁰. Showcase sections and manual section written.

Left:
- The Hall conductance: matrix-valued G (bands, spin), the current vertex ∂G₀⁻¹/∂k of a lattice model and a
  Brillouin-zone integration (Kubo, or the topological formula in G of Ishikawa–Matsuyama and Volovik, on a two-band
  model such as Haldane or Qi–Wu–Zhang).
- The `"Dynamic"` limit beyond first order: the 1/ν terms of χ(q = 0, iν) cancel only after a change of loop momenta
  (the option gives the message `limitterms`); a rule for a general vertex γ(k; q) (a current) in `EvaluateMatsubara`.
- Γ[φ̄, G] evaluated in momentum and frequency at constant φ̄ (the effective potential); means of complex bosons with an
  interaction; the 1PI action of fermions and superfields with dressed (background) lines (today with bare lines only).
- The Hubbard atom: the irreducible vertex as a matrix in the fermionic frequencies, its divergences at repulsive U (the
  zero-dimensional model shows them only at a = iν).

## Superfields (done in October 2026; what is left)

Done: the superfield 𝛙 = (ψ, ψ̄) with its antisymmetric Nambu propagator as the variable of the 2PI functional
(`"Statistics" -> "Superfield"` in `SelfEnergy`, `IrreducibleVertex`, the parquet functions, `EffectiveAction2PI` with the
pairing sources and the means, `EffectiveAction1PI` with bare lines), the kernels of the three channels from one functional
(`"Channel"`), `SuperfieldComponents`, `ZeroDimensionalValue` with arrays, the anomalous line styles; checked in
`Tests/Superfields.wl`; showcase section 13, manual.

Left:
- BCS in momentum and frequency: anomalous lines in `EvaluateMatsubara` (today refused with the message `anomalous`), the
  gap equation at finite T and T_c from the Thouless criterion; then Eliashberg (phonons with anomalous lines).
- The pair susceptibility −δ²Ω/δK_ψψ δK_ψ̄ψ̄ (the response of the pp channel, L = L₀ + L₀Γ_pp L).
- `ExactEquation`, `SelfEnergyFunctional`, `LegendreTransform` and `LegendrePropagator` for superfields (they refuse
  `"Superfield"` with the message `statistics`).
- The closed component form of the trace-log, −½ Tr ln(−𝒢⁻¹) = −Tr ln(−G⁻¹) + ½ Tr ln(1 + G⁻ᵀ𝒢_ψ̄ψ̄G⁻¹𝒢_ψψ), computed
  by the package (needs a transpose of operators); today written in the text and checked in zero dimensions.
- The trees of `ConnectedFunctionFromVertices[..., "Fermion"]` rewritten on `SuperfieldComponents` (their superfield has
  FullPropagator = −𝒢, another sign convention).
- Spin-resolved pairing kernels with spin components (singlet pairing at a contact interaction, where 𝒢_ψψ(x, x) = 0
  without spin); a superfield of complex bosons with a condensate.
- Fourth order: Φ of the superfield takes 90 s at V⁴; the projections would need a selection by the number of anomalous
  lines.

## Further checks and functions (ranked)

Done with the effective actions: the counterterm representation of Φ, Galitskii–Migdal, `ZeroDimensionalValue` (Γ₂ of
φ³ + φ⁴ against the exact Legendre transform, the one-site fermion model), Γ_irr = δ⁴Γ/δφ̄⁴, the branches of Σ[G] and the
divergence of Γ⁰ in zero dimensions, the coupled equations for Φ and G and the tadpole cancellation in φ³.

Later: Ψ-derivable, De Dominicis–Martin, Klein vs Luttinger–Ward and nPI in the table of names; Γ[φ̄, G, W] with the
stationary value equal to Γ[G]; an exact zero-dimensional Grassmann test of the superfield trees (the odd components of
`ConnectedFunctionFromVertices[..., "Fermion"]` are untested); homogeneity of Φ[G, W] (two lines); a Luttinger-theorem
paragraph; Martin–Siggia–Rose / model A (needs a rule for directed cycles of the response function); Schwinger–Dyson
for any action. Not now: Keldysh, n-irreducibility, a second (Kleinert)
generator of diagrams.

## One object, many names (largely done)

The showcase has a table of other names (Part I) and gives them at the first definition of each object. Left: the
same names in the manual where useful, and the entries not yet in the table: the Klein functional (same stationary point
as Φ), Ψ-derivable, De Dominicis–Martin, nPI; RPA in condensed matter vs RPA = TDHF in nuclear physics; Γ as the Gibbs
free energy.

## Beyond one loop (discussed; not planned, by the author's decision of October 2026)

- Two-loop QED in MS-bar with massless electrons: β(e) to two loops and the anomalous dimensions. Needs the Dirac algebra
  of `10_qed.wl` on top of the massless machinery of `09_dimreg.wl`, and the integration-by-parts "triangle rule" for the
  overlapping diagrams. Effort comparable to the one-loop QED module.
- On-shell two loops (g − 2 at order α², Källén–Sabry): needs a general IBP reducer (Laporta) and a table of massive
  on-shell master integrals, or a numerical parametric approach. A real project.
- Cheap and nice: generate and count the g − 2 vertex diagrams to four or five loops (1, 7, 72, 891, 12672) by
  gauge-invariant class; export two-loop integrands to external tools (FIRE, LiteRed, pySecDec).

## Speed

- The test suite takes about 80–90 s alone on a core (805 checks; 23 s for 528 checks before the effective actions; it
  was 125 s before the third round), about 70 % in `Tests/EffectiveActions.wl`. The slowest checks left are Γ⁽⁴⁾ of
  `EffectiveAction1PI` to λ³ (11 s) and the one-site model to u⁴ (8 s); the fully irreducible vertices of the parquet
  check are stored (`Tests/FullyIrreducibleVertices.m`, recomputed with `RECOMPUTE_REFERENCES=1`).
- `Showcase/make_figures.wl` takes about 120 s alone on a core (it was 286 s): the effective actions 52 s (the full
  vertex of the electron gas 16 s), the Nambu section 17 s, the φ⁴ moments 7 s, the brute-force Matsubara sums 5 s.
- `"MaximumOrder"` with the logarithm of a sum at a high order: the series of Log[Z0 + Z1] to λ⁸ is written at once,
  but its powers of Z1/Z0, expanded before the derivatives, are too many (the second derivative does not finish in
  300 s); dropping the terms with too many sources before the expansion would help.
- **Review.** `ZeroDimensionalValue` on the raw terms (no canonical form) is ten times faster on vacuum expressions,
  but wrong with sources left (private variables, unexpanded exponentials) or antisymmetric kernels with scalar values
  (terms that vanish by an odd automorphism): only with a guard (no sources, no antisymmetric heads). The derivatives
  with respect to several sources could share one fold with `SetSourcesToZero` and `"MaximumOrder"` (no measurable
  gain alone).

## Pictures

- Minor layout items from the October review (the two tadpoles on one leg of the φ⁴ four-point function, which touched,
  are now on opposite sides of the leg):
  - some fermion-rail terms (vertical rails of ⟨ψψ̄ψ̄ψ⟩_c at V², section 4) are 1.3–1.5 times taller than their
    neighbours: the spacing of 0.75 along vertical rails is shared by every rail picture, so changing it moves them all;
  - coils are crowded at the four-gluon vertex (Σ_q at two loops, Γ_AAA): the coils of the lines start at full amplitude
    at the vertex (only balloons taper, in `coilPoints`); tapering them needs to know, at export time, that four coils
    meet there, or changes every gluon picture;
  - the Γ⁰ ph̄ V³ vertical chain of two bubbles sits, tall, on a last row with one other term: a matter of the line
    breaking of the order sums (`assemble.py`), not of the picture.
- **Leg directions at a blob.** A label at a port of a blob gets no vote in `legDirectionsFor` (`fermionImbalance` counts
  lines), so that in δΣ/δU = Γ⁰L the legs x1, x2, which in some terms end at the box L, are drawn without arrows when the
  whole sum is drawn; the showcase passes `"LegDirections" -> "Alternating"`. The directed ports of fermion blobs could
  vote.
- **Declared blobs in trees with free leaves.** The tree layout takes the free ends of trees of vertex functions and bare
  vertices (`freeEndTreeLayout`), but a tree of blobs declared with `DeclareBlob` (their ports are points of their own)
  whose leaves are free ends is still drawn by the general layout, the blobs in a row with the leaves sticking up; with
  labelled leaves `blobLayoutWithExternals` draws it, and could take the free ends as its external points.

## Documents

- **The code of the showcase as a test.** Every code block of `Showcase/FunctionalDiagrams_Showcase.tex`, extracted and
  run in order in one fresh kernel, one input per complete line as a notebook reads it, with the terms of each
  `DiagramPlot` compared with `figures/` (up to the sizing options): done once by hand in October 2026, it found three
  definitions broken over two lines, helpers of the script used without their definition and a name that already had a
  value (in October 2026 again, with the formulas of the dimensional-regularization and QED sections compared as
  strings with `figures/`). It could run with the tests or in `check_consistency.py`.
- Usage messages: those of `EffectiveAction2PI` (about 7800 characters) and `EffectiveAction1PI` (3700) are long; the
  details could move to the manual.

## Distribution

- Upload `Logo/social_preview.png` in the GitHub settings (Social preview); it can only be done by hand.
- A DOI for the releases (Zenodo), when wanted: switch the repository on at zenodo.org before a release, then add the
  DOI to `CITATION.cff` and a badge to the README.
- For every release: the steps in the section Versions of `CLAUDE.md`.

## Repository

- **Size of the history (review).** The two PDFs (showcase 1.6 MB, manual 0.65 MB) stay tracked (the README links
  them), and every rebuild that is committed adds a full copy of them to the history. Commit them only when their
  content changes (a rebuild that only changes the timings can be undone with `git checkout`), or only for releases, or
  publish them as release assets.
