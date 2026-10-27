# Changelog

## Unreleased

### Pictures

- **The lines of a box leave from its corners**, as those of a triangle already did. A box joined by two lines to a
  circle (the Thouless criterion, Δ = −½ Δ L₀ Γ_pp) no longer moves its ports along its side to the circle's spacing:
  the lines end at its corners, and the circle's ports face them 55 degrees (before 45) off the line of the centres, so
  that the lines are parallel when the circle is wide enough and otherwise spread only slightly. A point twice a port of
  one blob, which the general layout drew as a bubble from the middle of a side (χ = L(z₁, z₁; z₂, z₂), an interaction
  line ending on L in δΣ/δU), now stands outside the two corners of that side, joined to each by a line, as x◁F▷y in
  χ = χ₀ + γL₀FL₀γ: a density point when it is an external label and nothing else, otherwise a vertex with a dot; the
  two legs of the vertex at the other end of that interaction line are fanned 90 degrees apart.
- **A box joined by three corners to one vertex is turned by 45 degrees.** In the Schwinger–Dyson equation of φ⁴
  theory, Σ = ½ (tadpole) − ⅙ G G G Γ⁽⁴⁾, the box Γ⁽⁴⁾ was drawn upright on the axis, with a straight line and an arc
  from the vertex to two of its corners (a plain box, without `"AxialPorts"`, was drawn askew). Any box joined by single
  lines from three of its corners to one vertex, its fourth corner elsewhere, is now a diamond, symmetric about the axis:
  the fourth corner on the right (b, or the line to b), the vertex on the diagonal through it, joined straight to the left
  corner and by two arcs of one circle to the top and bottom corners. Its ports keep their order round it (the box
  turned), its label stays upright, its hatching is the box's turned with it (vertical, never along an edge), and it is
  as large as the square, or larger when the label needs it. The showcase draws Γ⁽⁴⁾ there as the plain box of the
  Bethe–Salpeter equation, and the manual shows the term under `DeclareBlob`.

### Documents

- **The showcase shows what its code gives.** The irreducible vertex Γ⁰ follows `DiagramPlot[irreducibleVertex, {x1, x2,
  x3, x4}]` as that command draws it, order by order (the diagrams reducible in the particle–particle channel with their
  two lines crossing); a second code block then sorts it by channel with `ParquetDecomposition` and draws the parts
  reducible in ph̄ and in pp, the latter with `"ExternalOrder" -> {x4, x2, x3, x1}` (the incoming particles on the left):
  these are the pictures by channel that followed the first command before. A sum written order by order is drawn as
  one sum and then split, so that every order looks as in `DiagramPlot` of the whole output. The first example of the
  README plots Φ[G] too (`DiagramPlot[Phi]`), and its picture shows Γ⁰ as `DiagramPlot` draws it, without channel marks.
- **Every code block of the showcase gives what follows it.** The introduction says once that the sizing options of the
  script are left out; every other option is now in the code: `"Interactions"` for W in Φ[G, W] and for D_σ, `"Couplings"
  -> {g}` for the phonons, `"LegDirections"` for δΣ/δU. The particle–particle kernel and the averaged propagator of
  disorder are drawn as their commands give them (no channel marks; the plain arrow of a skeleton expansion, not a double
  line), and the shapes of the trees W⁽ⁿ⁾ as the integrated sums are (their leaves free ends). The code computes what is
  shown: the four sums of tree shapes, the four- and six-point fermion trees, Z/Z₀ of the electron gas, the pieces of
  Γ[φ̄] and Γ[Ψ̄, Ψ] grouped by loops, R_ph, the pole parts of the rearranged Γ⁽⁴⁾, the checks of Hubbard–Stratonovich and
  each Hedin equation to its order, and in QED δ₂, Z_m, the mass shift, Π₂, δ₃, β(e), Π̂₂ and the form factors
  (δΓ^μ = −(i/e)·(ieδΓ^μ): the sign of the code shown is corrected). Assignments whose value is drawn no longer end with
  a semicolon, declarations used by the pictures are shown (`DeclareSymmetric[DressedPropagator]`, the anomalous
  propagators, the dashed full phonon line), a code block that checks comes after the pictures it does not draw
  (Hedin), the Nambu kernel follows its derivative, and the orders left out of a picture are said. The left-hand side
  of a sum written order by order no longer stays alone at the foot of a page, and a plain sum of pictures, like a
  displayed formula, no longer starts a page apart from the line that introduces it.

## 2.4.0 (2026-10-26)

Since 2.3.0: one new option, `"Appearance"` of `DiagramPlot` and `DrawDiagram`; no public name added, removed or renamed
(132 in all), 821 tests. In short: the pictures of Mathematica follow light and dark notebooks, a blob is as large as its
label, the lines between two blobs are parallel, and skeleton expansions are drawn with plain lines, as in the textbooks.

### New

- **Light and dark notebooks.** The pictures that `DiagramPlot` and `DrawDiagram` return follow the appearance of the
  notebook (Mathematica 14.3 and 15 have dark notebooks): dark lines and text on a light notebook, light ones on a dark
  notebook, through `LightDarkSwitched`; the inside of the blobs, of the external points and of the sources, and the
  clearings behind the labels of hatched blobs, take the background of the notebook, so that they hide no line there;
  grey fills and the frames of the cells are a darker grey. Before 14.3 the pictures keep the colours of a light
  notebook. The new option `"Appearance" -> Automatic | "Light" | "Dark"` forces one of them, on a background of its
  paper colour (for a picture exported for a page of the other kind; `SetOptions` for a session). The TikZ code, for white
  pages, is unchanged.

### Pictures

- **Labels of blobs.** A blob declared by `DeclareBlob` is as large as its label needs: the label, on a hatched blob
  in a white clearing, lies inside the blob with a band of hatching all around it, so that an accent, a subscript or a
  superscript is never cut by the outline (Λ̃ in a triangle, Γ_ph, Γ^(4)). The blob grows, never its label shrinks
  (but in a triangle, below); in one picture the blobs of one shape have one size, so that the lines between them stay
  parallel, and so do those of all the terms of a sum drawn together (Γ⁰ alone and beside Γ^(4) in one equation). The
  label of a triangle sits near its incentre, not at its centroid. In the pictures of Mathematica a tilde or a hat is
  written above its letter (it was a combining character, small and beside the apex of Λ̃).
- **Triangles.** `"Triangle"` is equilateral, the same triangle as `"TriangleUp"` turned (it had a narrow 30-degree tip,
  with little room for a label).
- **Lines between a box and a triangle** are parallel: the lines of a triangle always leave from its corners, and a box
  joined by two lines to two corners of a triangle (Γ⁰ L₀ Λ, Γ_xc L₀ Λ̃) is as wide as the side between them, in that
  picture and in every term of the sum drawn with it; the label inside a triangle is written 0.8 times smaller, so that
  Λ̃ keeps the size of Λ and the box does not grow large. So are those between a circle and a box: a
  circle with two ports, both joined to one other blob (the gap function closed on Γ_pp in the criterion of Thouless),
  has them on the side facing it, on either side of the line of the centres (one was on the far side, and the picture was
  lopsided), and the box takes their spacing on its side.
- **A line to the near corner of a blob** stays straight: a line that leaves a port of a blob outwards, 30 degrees or more
  from its outline, is bent only if it would cross the blob, not for passing near it (in the Schwinger–Dyson equation of
  φ⁴ the lines from the vertex to the two top corners of Γ^(4) were both bent, and left the vertex together).
- **Diamonds** are hatched vertically (cross-hatched vertically and horizontally): the hatching at 45 degrees ran along
  two of their edges.
- **The double wavy line** (a dressed interaction W) is one wavy line drawn double, its strokes 1.2 line widths apart along
  the wave: the two waves shifted along the line closed up on their slopes and read as one thick line in small pictures.
- **Skeleton expansions drawn with plain lines.** As in the textbooks, every line of Φ[G, W], Σ[G, W], P[G, W] and of
  Γ₂[G, D_σ] of φ⁴ theory is a dressed one, and is drawn in the plain style of its kind: the screened interaction W is an
  ordinary wavy line there (`"Interactions" -> {ScreenedW}` in one picture). The double wavy line marks W only where the
  bare V appears too, as in Hedin's equations, like the double arrow in the Dyson equation.

### Documents

- **Light and dark in the documents.** The manual (a paragraph in the section on pictures, the option in the table of
  options) and the guided tour (a picture forced dark) describe `"Appearance"`. The usage message of `DiagramPlot` lists
  every value of `"InteractionStyle"` (`"DoubleWavy"` and `"Curly"` were accepted, but not listed); those of
  `DeclareBlob`, `DeclareLineStyle` and `DeclareInteractionLine` give the sizes of the blobs and say where the double
  wavy line is used.
- **Showcase.** Γ^(4) of the Schwinger–Dyson equation of φ⁴ is the box of the Bethe–Salpeter equation, sitting on the line
  from a to b as F does in the equation of the electron gas (it was a diamond grown large by its label).
- **Logo: arrows that read.** The fermion loop and the wavy line of the logo are slimmer (6.5 and 6 units of 128, from
  8 and 7) and the arrow heads larger and wider (19 units across, more than twice the loop), so that the direction of
  the loop reads at 64 and 32 px too; in the Logo files, the title pages of the manual and of the showcase, the social
  preview and `packageLogo`. The small badge (16 to 32 px, the load banner) keeps its heavier lines and no arrows.
- **Gallery by theme.** The panels of the README tell one story each: the first example (Φ[G], Σ and Γ⁰); exact
  equations (Dyson, Bethe–Salpeter, the vertex of the density, parquet); screening (Hedin's equations and Φ[G, W] with
  its derivatives, the *GW* approximation); pairing with the Nambu superfield (Φ at first order, the Gor'kov equations,
  Hartree–Fock and the BCS gap); field theory (φ⁴ in dimensional regularization, the two-loop self-energy with its pole
  and the β function; the three 1PI functions of one-loop QED with δ₂, β(e) and F₂(0); the gluon self-energy of QCD),
  with the formulas computed by the showcase. QCD no longer shares a panel with Φ[G, W], and φ⁴ theory at a non-zero
  source left the gallery (it stays in the showcase).
- **The README in order for a first visit.** What the package is, the demo, the first example, the gallery,
  installation, the documents, then the list of features with the map of the functions, the checks, related software,
  details, citing (with a BibTeX entry) and the license. The demo video is no longer described by its duration (README,
  landing page).
- **Badges and a landing page.** The README shows the badge of the latest release (it had a fixed version) and that of
  the consistency check. GitHub Pages serves a landing page, `index.html`: the logo, what the package does in one
  sentence, and links to the repository, the latest release, the showcase, the manual, the map of the functions (in
  which the hub `FunctionalDerivative` is marked with δ) and the demo video.
- **Demo.** The looping preview at the top of the README is an animated WebP at 60 fps, `Media/demo_preview.webp`
  (the GIF it replaces ran at 12 fps: a GIF cannot run at 60, its delays being hundredths of a second). The video is
  1920×1080 at 60 fps (it was 1280×720 at 30 fps): the same frames, sharper, and smoother motion; the preview is reduced
  from them. In the video the notebook shows each input with its outputs in one cell group (In[n] and Out[n] on the
  left, the cell brackets and the bracket of the group on the right), the QED card points from each one-loop diagram to
  its result (the pole of Σ, the transverse Π with its logarithm, F₂(0) = α/2π), and the logo is the slimmer one.

## 2.3.0 (2026-10-26)

Since 2.2.0: 54 new public names (132 in all), about 800 tests (805 at the last run). In short: the effective actions are
objects of the package, every object of the documents is generated by a functional derivative of a functional the user
writes (the shortcut functions are the checks), the 2PI functional of the Nambu superfield gives the anomalous
propagators and the kernels of all three channels, and diagrams are evaluated in dimensional regularization and, for QED,
at one loop.

### Incompatible changes

- **Weights of the pictures.** The number written in front of a picture, times the values of its elements, is the term,
  with an interaction line standing for −V and a vertex for minus its coupling (the minus sign comes from expanding
  exp(−S_int)); in 2.2.0 the weights referred to V and to the couplings themselves. So Fock has the weight +1, Hartree −1
  (its fermion loop), the φ⁴ tadpole +½. Only the displayed weights change (`DiagramPlot`, `DrawDiagram`,
  `DiagramToTikZ`): the expressions are the same. `"Weights" -> "Coefficients"` shows the coefficients.
- **QED with e > 0.** The electron has the charge −e with e > 0: S_int = e∫ψ̄γ^μψA_μ, the vertex is +ieγ^μ
  (`QEDVertex -> {"Vertex", -I}` in the rules of `QEDIntegrand`), the 1PI vertex function +ieΓ^μ.
- **Mean field.** `MeanField` is displayed φ̄ = ⟨φ⟩, the stationary point of the effective action (it was Φ, the symbol of
  the Luttinger–Ward functional); the example symbol of the mean is `phiMean`.
- **Antisymmetric kernels.** The derivative with respect to an antisymmetric kernel, `FunctionalDerivative[F, h[x, y]]`, is
  antisymmetrized, and the kernel of the inverse of an antisymmetric operator is antisymmetric (nothing changes for the
  kernels used before).
- The message of a generating functional without sources is `FunctionalDiagrams::nosources`
  (`FunctionalDerivative::nosource` is unchanged).
- **License.** The GNU General Public License, version 3 or later, with two additional terms under its section 7: the
  attribution to the author and the file `NOTICE` must be kept, and modified versions must say that they are modified, by
  whom and when, and must not be distributed under the name FunctionalDiagrams alone. `LICENSE` holds the text of the
  GPL alone (so that GitHub recognizes it), `NOTICE` the copyright, the "or any later version" statement and the terms.

### New

**Effective actions as objects.** `EffectiveAction2PI[Phi, G0 -> G]` is the 2PI effective action
Γ[G] = Φ[G] − Tr ln(−G⁻¹) − Tr(G₀⁻¹G − 1) (for real bosons Γ₂ + ½Tr ln G⁻¹ + ½Tr(D₀⁻¹G − 1); also complex bosons),
stationary at the Dyson equation and equal to −log Z there. With `"MeanFields"` it is Γ[φ̄, G] of Cornwall, Jackiw and
Tomboulis, the Legendre transform of W[J, K] in the source J and in the two-point source K (Grassmann-odd means Ψ, Ψ̄ for
fermions); its fourth derivative with respect to the means at fixed G is the fully irreducible vertex Γ_irr, and with
`"Source" -> U` expectation values and responses are variations. `EffectiveAction1PI` is the 1PI action Γ[φ̄] of a real
boson, with lines in the background field or bare lines (`D0 -> D0`), and Γ[Ψ̄, Ψ] of fermions with bare lines; at zero
means `FunctionalDerivative[Gamma, Psibar[a], Psi[b]]` = G₀⁻¹ − Σ and its fourth derivative is the full vertex F.
`LegendreTransform[Phi, G -> S]` and `SelfEnergyFunctional` are Potthoff's universal functional 𝓕[Σ] and self-energy
functional Ω[Σ] (`LegendrePropagator` for G[Σ]). The traces are objects, `TraceLog`, `OperatorTrace`, `OperatorKernel`,
written out as rings and Dyson series by `ExpandTrace`; `FunctionalDerivative` differentiates with respect to any
two-index object `G[x, y]` and through traces of operators. `ZeroDimensionalValue` gives the value of any expression in
zero dimensions, also with array values (internal states, Tr ln → ln det), for exact checks of the weights.
`MatsubaraSum` sums logarithms and `EvaluateMatsubara` evaluates Tr ln and Tr (the grand potential; new symbol
`Temperature`) and the static and dynamic limits of a response (`"Limit"`). `ExactEquation` has the response
L = L₀ + L₀Γ⁰L (`"Response"`), the susceptibility (`"Susceptibility"`) and a general one-body vertex (`"BareVertex"`).
New test file `Tests/EffectiveActions.wl`, with the slow references in `Tests/FullyIrreducibleVertices.m`
(`RECOMPUTE_REFERENCES=1` computes them again).

**Superfields: the functional of Nambu and Gor'kov.** The Nambu field 𝛙 = (ψ, ψ̄), a label a = (x, component), is an
ordinary Grassmann field with one source, an antisymmetric propagator 𝒢 = −⟨T𝛙𝛙⟩ (the blocks G, 𝒢_ψψ = −⟨Tψψ⟩,
𝒢_ψ̄ψ̄ = −⟨Tψ̄ψ̄⟩) and a totally antisymmetric vertex, written as for φ⁴; its 2PI functional has the anomalous propagators
among its variables.
- `"Statistics" -> "Superfield"` in `SelfEnergy` (Σ = −2δΦ/δ𝒢), `IrreducibleVertex` (Γ⁰ = 4δ²Φ/δ𝒢δ𝒢),
  `ParquetDecomposition`, `BetheSalpeterLadder` (the ladders +½, −½, +½ in s, t, u), `CheckParquetEquations`,
  `EffectiveAction2PI` (Φ − ½Tr ln(−𝒢⁻¹) − ½Tr(𝒢₀⁻¹𝒢 − 1), with the pairing sources and the means; exact at its stationary
  point also at finite means) and `EffectiveAction1PI`.
- `IrreducibleVertex[..., "Channel" -> "s" | "t" | "u" | All]` for real bosons and superfields: the kernels of the three
  channels from one functional (s, t, u are ph, pp, ph̄ for the superfield); for fermions `"TransverseParticleHole"`, and
  `"ParticleParticle"` gives the message `ppkernel` (that kernel is a derivative with respect to the anomalous propagators).
- `SuperfieldComponents[expr, externals, rules]` writes a superfield expression in the components: Φ[G, 𝒢_ψψ, 𝒢_ψ̄ψ̄] (3, 7,
  52 diagrams to V³), the gap functions Δ = Σ_ψ̄ψ̄ and Δ̄ (the BCS gap equation Δ = V𝒢_ψψ and Hartree–Fock–Bogoliubov at
  first order), the Gor'kov equations, the particle–particle kernel −4δ²Φ/δ𝒢_ψ̄ψ̄δ𝒢_ψψ (equal to F − R_pp). Tr ln(−𝒢⁻¹) has
  no component form and is kept as it is (messages `trace`, `supertrace`).
- The line styles `"DivergingArrows"` (𝒢_ψψ) and `"ConvergingArrows"` (𝒢_ψ̄ψ̄); fermion lines run through them.
- Tests in `Tests/Superfields.wl`, among them an exact check on two sites with spin against a Grassmann algebra, and one
  site with pairing against the exact Z = a² + kk̄ − u.

**Hubbard–Stratonovich fields in both directions.** `HubbardStratonovich[{Z0, interaction}, O, {phi, J}, D]` decouples
an interaction ½∫K O O by a real auxiliary field with ⟨φφ⟩₀ = K and the coupling −i∫φO, for any local operator O (the
density, φ²); `IntegrateOutField[{Z0, interaction}, phi, J]` integrates a Gaussian field out (for phonons, the effective
retarded interaction of the electrons). `LuttingerWardFunctional` with several replacement rules gives Φ[G, W], whose
first diagram is the *GW* approximation, and Φ[G, D] of electrons and phonons (Migdal).

**Feynman integrals in dimensional regularization.** Euclidean momentum integrals in d = 4 − 2ε, every loop with the
MS-bar measure μ^(2ε)(e^γE/4π)^ε ∫d^dk/(2π)^d. `DiagramLoopIntegral` routes the momenta through the lines;
`IntegrateLoops` does the loops one after the other in closed form in d when each step is a one-loop integral (tadpoles
`TadpoleIntegral`, massless bubbles `GFunction`, massive bubbles `BubbleIntegral`, numerators reduced to tadpoles), with
messages for infrared divergences and for integrals that are not nested one-loop ones; `EpsilonExpand` and `PolePart`
expand in ε; `InfraredRearrangedIntegral`, `MassInsertion`, `BetaFunctionFromZ` and `AnomalousDimensionFromZ`. From the
package's own diagrams, φ⁴ theory to two loops: β = 3λ²/(16π²) − (17/3)λ³/(16π²)², γ_φ, γ_m², and the Ising exponents at
the Wilson–Fisher fixed point. 17 new public functions, 110 tests (against numerical integrals in non-integer d,
`Series`, position space and the textbooks).

**QED at one loop.** `QEDIntegrand` turns the diagrams of the package with spinor and Lorentz indices into momentum
integrals (open fermion lines as `DiracProduct`s, closed loops as `DiracTrace`s, the photon in a covariant gauge,
optionally with a mass as an infrared regulator); `DiracSimplify` and `DiracOrder` do the Dirac algebra in d dimensions
(no γ5, which is refused); `IntegrateOneLoop` does any one-loop integral by Feynman parameters and tensor reduction,
exactly in d; `ReduceParameterIntegrals`, `ExpandParameterIntegrals`, `DifferentiateUnderIntegral`, `VertexFormFactors`.
The electron self-energy, the vacuum polarization (transverse in d, β(e) = e³/12π²) and the vertex (Schwinger's
F₂(0) = α/2π in every gauge, Z₁ = Z₂). 22 new public functions, 146 tests, `Tests/QEDOneLoop.wl`.

**Smaller additions.**
- `FunctionalDerivative[F, s1, ..., sn, "MaximumOrder" -> {x, n}]`: the orders 0 to n in a coupling or the head of a line,
  as `OrderPart` of the whole derivative, with the higher orders dropped after each derivative; a logarithm of a sum
  polynomial in a coupling is expanded first, so that W = log(Z0 + Z1 + Z2/2 + ...) is differentiated order by order.
  `SetSourcesToZero` holds its argument: `SetSourcesToZero[FunctionalDerivative[...]]` drops after each derivative the
  terms that vanish at zero sources (the same result, much faster).
- `EvaluateMatsubara`, `CuttingRuleImaginaryPart` and `CheckCuttingRules` take `"ConvergenceFactor"` (1, the default,
  G(0⁻) with n_F; −1, G(0⁺) with n_F − 1; 0, the symmetric mean with n_F − ½), as `MatsubaraSum` does.
- `DeclareCoupling` and the option `"Couplings"` (couplings absorbed into the vertices), `DeclareInteractionLine[V, ...,
  style]` (the screened W is `"DoubleWavy"`), `DeclareLocalVertex` vertices standing for minus their kernels.
- `ExpressionToLaTeX` writes loop integrals as one fraction, μ, γE, ζ and artanh by their names, and the default loop
  momentum as ℓ_i also outside a loop integral.
- Messages where a mistake gave a wrong result silently: `statisticsdefault` (`SelfEnergy`, `IrreducibleVertex`,
  `EffectiveAction2PI` on a symmetric or antisymmetric G without `"Statistics"`), `fieldarity`, `rulesymmetry`,
  `vertexlabels`, `inversekernel`, `opdotpower` (`G0^-1 . G`, read by Mathematica as `G0^(-(1 . G))`, is taken as
  `G0^(-1) . G`; `Inverse[G]` is `G^-1`), `opsource`; the messages on operators show the user's kernel.
- In a notebook, loading the package prints a banner two lines high (it replaces a line of text): the logo, the
  attribution "FunctionalDiagrams, by Nazzareno Africani" (term (b) of `NOTICE`), the version, the license, how to cite it (`CITATION.cff`) and links to the repository and the manual.
  Scripts (`wolfram -script`) print nothing.

### Fixed

- `EvaluateMatsubara`, `DiagramIntegrand` and `CuttingRuleImaginaryPart` refuse anomalous propagators (message
  `anomalous`): the check never matched a head, and they were evaluated as normal fermion lines.
- A source inside a `LabelFunction` in a trace or kernel is differentiated (its derivative was 0).
- `PerturbativeFullVertex` takes a list of rules; `ParquetDecomposition` accepts one line head without a list and gives a
  message for a wrong call; `LabelFunction` makes its variables in the user's context.
- Messages no longer show private variables; `ResetDeclarations` also clears the rules of the tree expansion.
- The paclet keeps the showcase and the manual in their folders (`Showcase/`, `Documentation/`), so that
  `PacletObject["FunctionalDiagrams"]["AssetLocation", "Manual"]` finds them in a clone loaded with `PacletDirectoryLoad`
  too, not only in an installed paclet.
- The required version is Mathematica 12.1 (it was given as 12): `PacletDirectoryLoad` and the `PacletInfo.wl` format of
  the paclet need it. The loading cell of `Examples.nb` falls back to ``Needs["FunctionalDiagrams`"]`` (the installed
  paclet) when the notebook is opened without `FunctionalDiagrams.wl` beside it.

### Pictures

- Loops: closed loops of undirected propagators (φ⁴ bubbles, Hubbard–Stratonovich fields) are round, like fermion loops;
  two bubbles meeting at a vertex, or a bubble whose vertices carry a line that would run into it, are lenses; of two
  crossing chords the one outside runs over the top, a chord outside a loop is 0.4 + 0.3 (chord/diameter) outside the
  circle at its middle; three diameters of a loop of six: one drawn outside, the other two crossing at 70 degrees.
- Interaction lines: wavy arcs have a half-integer number of waves, shorter and lower on a short arc; straight wavy lines
  are explicit curves, and crossing wavy lines are flattened where they cross; an interaction line along the arc of a
  round loop stays clear of it.
- Four-point functions drawn as two rails keep the height of their neighbours; a bare vertex is a wide V; three or more
  open fermion lines (Z[η, η̄] with six sources) are stacked rails; trees and pictures without external points are
  upright.
- Density points (ψ̄ψ, φ²) are crossed circles carrying their labels, also at a bare vertex and at the third corner of a
  triangle blob; several means on a vertex form a V outside the loop; a tadpole on a leg hangs from a vertex nearer the
  external point; a gluon loop at the four-gluon vertex sits on top of it.
- Blobs: the ports of a symmetric blob are taken in a fixed order (the external points counterclockwise from the top
  left in every term); blobs that share points are drawn side by side; a line between two ports of one blob is bent
  outwards; the label of a hatched blob sits in a white clearing, also in TikZ, and `\tilde`, `\hat` are accents.
- The arrow head of a double line (G) is 1.45 times larger; labels and arrow heads avoid lines, crossings and blobs; a
  leading minus is set apart from the picture.

### Documents

- **Everything from functional derivatives.** The showcase, the manual, the guided tour, the README and the usage
  messages show every object as the derivative of a functional the user writes (Z, W = log Z, Γ[φ̄], Γ[φ̄, G], Φ[G], the
  superfield Φ[𝒢]) and the shortcut functions as the checks: Σ = δΦ/δG, Γ⁰ = δΣ/δG, F and Γ⁽⁴⁾ as fourth derivatives of
  the 1PI actions, the density correlation −δG(a, a)/δU(b), the vertex of the density γ + δΣ/δU, the kernels of the
  channels and Γ_irr from the 2PI action, Hedin's vertex and polarization as derivatives in the total potential,
  Migdal's self-energy from Φ[G, D] and the self-consistent Born approximation from the functional of the averaged
  propagator. One notation: F the full vertex, Γ_irr the fully irreducible vertex, Λ the vertex of the density, L₀ a pair
  of propagators, 𝓕 Potthoff's universal functional, φ̄ = ⟨φ⟩ and Ψ = ⟨ψ⟩, the superfield 𝛙 with the Nambu propagator 𝒢.
- **Showcase** in three parts: many-body theory in the order of a course (Wick's theorem and the linked-cluster theorem,
  explained and drawn; perturbation theory; the Dyson equation; linear response and Matsubara sums; the 1PI and 2PI
  effective actions, both expanded to third order piece by piece; the Bethe–Salpeter equation from the response to a
  one-body source; vertices and responses; parquet, with the derivation of Eckhardt, Kappl, Kauch and Held; the
  superfield; Hedin, Hubbard–Stratonovich and *GW*; Potthoff's functionals and the divergent vertices; phonons and
  disorder), quantum field theory (φ⁴ in dimensional regularization, QED, QCD) and the tools.
- **Manual**: the principle in the overview; the sections in the order functionals, effective actions, channels and
  parquet, exact equations, trees, pictures, evaluation; the new functions, messages and conventions; a subsection on
  superfields.
- **README**: one image file per picture, legible on light and dark pages; a demo video (the first example
  typed and run, then the Bethe–Salpeter and parquet equations of the electron gas, Hedin's equations, a Matsubara sum,
  QED at one loop and the TikZ export, between two cards with the logo animated: its fermion loop turns once, the arrow
  heads travelling in the direction they point); the map of the functions (`Documentation/function_map.svg` and the
  interactive `function_map.html`, drawn from the definitions of the package by `Development/make_function_map.wl` and
  `.py`): the public names by their role in a calculation, in six stages around `FunctionalDerivative` (declare, write a
  functional, differentiate, classify and check, draw, evaluate), a name built on another indented under it; the
  interactive page, light and dark, draws the lines of a selected name to the names it uses and from the names that use
  it, with search, keyboard and touch.
- `CONTRIBUTING.md` (how to report a problem, the generated files, the tests, the conventions) and an issue form for
  problem reports; the social preview of the repository is 1280×640, so that link cards do not crop it.

### Performance

The test suite runs in about a minute and a half (125 s before the last round of optimizations, 85 s after, alone on a
core), of which about 70% are the effective actions (the stationarity of Γ[G] is expanded once instead of once per
order, and the slow references are stored); the showcase script in about two minutes (286 s before, 122 s after). The
results and the pictures are the same:
- the canonical form refines the colours only for the variables that share a colour, computes the keys of a term once,
  skips the members of a cell in the orbit of one already searched, and brings each nested integral (the exponent of Z0,
  the integrals of Log[1 + z]) to canonical form once; products of plain factors and terms without delta functions take
  fast paths;
- `SetSourcesToZero[FunctionalDerivative[...]]` drops after each derivative the terms that vanish at zero sources (the
  moments of φ⁴ in the showcase: 28 s before, 7 s after), and `"MaximumOrder"` the terms of higher order (Γ⁽⁴⁾ of the 1PI
  action: 12 s before, 0.4 s after);
- the derivatives of the logarithm of a sum whose terms share a Gaussian factor, Log[Z0 + Z1 + ...], take the factor out
  (log(e^X p) = X + log p): their results are equal to those before, in a simpler form;
- `SuperfieldComponents` projects once the assignments of components related by an automorphism of the term, the
  Luttinger–Ward functional and the effective actions select the irreducible vacuum diagrams before the canonical form,
  and `ExpandTrace` with `"MaximumOrder"` writes out only the monomials whose lowest order fits (the Nambu section of the
  showcase: 52 s before, 17 s after);
- the TikZ export finds the crossings of wavy lines and the places of the arrows through bounding boxes and writes the
  coordinates of a curve at once; the demo video is drawn by several processes.

## 2.2.0 (October 2026)

**From diagrams to numbers: Matsubara sums.** The diagrams of a translation-invariant system are evaluated in momentum and
Matsubara frequency. `EvaluateMatsubara[diagrams, {y -> {k, I w}, x -> {-k, -I w}}, rules]` routes momenta and
frequencies through the lines — fermion lines 1/(iω − ξ_p), instantaneous interactions v(q), boson propagators rational
in z = iν — sums the frequency of every loop by residues, with the convergence factor e^{iω0⁺} of the lines at equal
times, and leaves the momentum integrals as `MomentumSum`. The results are written as in the textbooks: one denominator
per intermediate state, partial fractions in the external frequency, the occupation factors as a sum over processes
(n₂(1 − n₁)(1 − n₃) + n₁n₃(1 − n₂) in the second-order self-energy, n_B + 1 − n_F and n_B + n_F in the Migdal
self-energy). `MatsubaraSum` sums rational functions whose denominators are products of linear and quadratic factors
(propagators), poles of any order giving derivatives of `FermiFunction` and `BoseFunction`, without `Residue`, `Limit`
or `Apart`; `DeclareMatsubaraFrequency` declares the statistics of the external frequencies, whose statistical factors are simplified by periodicity (n_F(ε + iω) =
−n_B(ε)); `DiagramIntegrand` gives the integrand before the sums; `AnalyticContinuation`, `RetardedImaginaryPart`
(`EnergyDelta` and its derivatives) and `ZeroTemperatureLimit` go to real frequency. The imaginary part of a retarded
two-point function is also given by the cutting rules at finite temperature (`CuttingRuleImaginaryPart`,
`CheckCuttingRules`), derived independently in imaginary time; they are implemented for chains of propagating lines
between the two external time vertices (bubbles, sunsets, the exchange of a boson, with Hartree and Fock insertions on the
lines), not yet for vertex corrections, which need the circling rules of Kobes and Semenoff. The package's density
correlation is minus the Lindhard function, and its boson propagator ⟨φφ⟩ minus the D₀ of the textbooks. Checked against
brute-force frequency sums (poles of first to third order, one and two loops), the Lindhard function at finite and zero
temperature, the second-order self-energy of the electron gas, the Migdal self-energy and the third-order self-energy
(the frequency sums in either order); 15 new public functions, a section of the manual and a section of the showcase,
"From diagrams to numbers: Matsubara sums and the optical theorem".

**Exact equations and non-perturbative objects.** `ExactEquation[name, objects, labels]` writes the exact equations of
many-body theory with their indices contracted and the signs of the package's conventions — Dyson, Bethe–Salpeter (any
channel), parquet, Schwinger–Dyson (density–density interaction, or φ⁴ theory), the equation of the vertex of the density
(Λ = γ + Γ⁰GGΛ and Λ = γ + FGGγ), the self-energy from the vertex (Σ = Σ_H − VGΛ) and Hedin's equations (Σ = Σ_H − GWΛ̃,
P = GGΛ̃, W = V + VPW, Λ̃ = γ + Γ_xc GGΛ̃), also for real bosons. The objects are heads (blobs, to draw the equations) or
functions of their labels (`LabelFunction` of perturbative series), and `CheckEquation` checks an equation order by order.
`PerturbativeDensityVertex` computes the vertex of the density (of φ²/2 for real bosons). All the equations are checked in
the showcase to third order, for the electron gas and for φ⁴ theory.

**Hubbard–Stratonovich and GW.** `LuttingerWardFunctional[Z0, interaction, n, {G0 -> G, D0 -> W}]` makes several
propagators full at once: for the electron gas written as fermions exchanging a boson (⟨φφ⟩₀ = V, coupling −i∫φρ) it
gives the 2PI functional Φ[G, W], whose first diagram is the GW approximation; eliminating W gives back Σ[G] and P[G]
(checked to third order). The same for φ⁴ theory, with a field coupled to φ²/2.

**Both theories at every step.** The showcase shows φ⁴ theory and the electron gas at every step: Z and W at a non-zero
source, Z/Z₀, the numerator of G, the connected four-point functions, the full vertex, Γ⁰, the reducible parts by
channel, the Schwinger–Dyson equations, the vertices and Hedin's equations; new sections on vertices and Hedin's
equations and on Hubbard–Stratonovich and GW, paragraphs on symmetry factors and on why Legendre transforms remove
diagrams; more than eighty references (textbooks and original papers), verified. The guided tour has a section on vertices, Hedin's
equations and GW.

**Faster.** Wick pairings contract only one leg of each orbit of the symmetries of a vertex already opened, and stop as
soon as a pairing cannot be completed (the orbits of the legs of a vertex come from its group of automorphisms, defined once); the classes of diagrams select terms before bringing them to canonical form; the
parquet machinery brings them to canonical form once; colour refinement is cheaper. The option `"CouplingOrder" -> {g, k}`
of the perturbative functions computes only the order g^k. Results are unchanged; the showcase is computed in about a third
of the time, the two-loop self-energies of QCD up to a hundred times faster.

**Faster checks of exact equations, and a faster canonical form.** `CheckEquation[lhs == rhs, x, n]` brings into
canonical form only the terms of order n or less, and `ExactEquation` has the option `"MaximumOrder" -> {x, n}`, which
keeps both sides (and the ladders of `"BetheSalpeter"` and `"Parquet"`) only to order n: the vertex equation of the
electron gas is checked at third order in under 2 s instead of 30. The canonical form no longer recomputes the nested
integrals that a renaming cannot change, and individualises a fully symmetric class of integration variables at once
(Z[J] of φ⁴ at third order four times faster), and skips a branch of the search that an automorphism of the term maps onto one already searched (the integrated trees of six points, whose leaves can be exchanged only with their vertex, 40 % faster); a product of plain factors becomes one term directly; a picture computes
the elements of each diagram once. Results and pictures are unchanged. `GaussianExpectationValue` with an unknown
`"Method"` now says so, like the other functions.

**Faster one-loop QED.** `DiracSimplify` no longer expands the Feynman-parameter integrals of an expression to count its
indices, and keeps them aside during the algebra (only the on-shell rules of the spinor chains act on them); the
one-loop integrals run the Dirac algebra on the tensor structures alone; `ReduceParameterIntegrals` remembers the
reduction of each integral, and `EpsilonExpand` and `ExpandParameterIntegrals` the series of each factor within one
expansion. Results and messages are unchanged; the self-energy, vacuum polarization and vertex of `Tests/QEDOneLoop.wl`
are computed three times faster.

**Faster tests and documents.** The test suite runs in half the time (about 25 s): the references that took seconds of
symbolic work (an `Integrate`, six `Series`) are stored, with the code that computed them; the QED checks compare with the
results of `Tests/QEDOneLoop.wl` instead of integrating the same diagrams again; the two-loop brute-force Matsubara sums
use fewer frequencies (errors about 1.5·10⁻⁶ against a tolerance of 10⁻⁵), and evaluate the integrand once for both levels of the inner sum and, at a fixed inner level, once for the three outer levels. The brute-force sums are one file,
`Tests/MatsubaraBruteForce.wl`, loaded by the tests and by the showcase. New checks of the `Graphics` output: every line
style, the blobs, the `"Grid"` output. The showcase computes the third order of φ⁴ and of the electron gas only from the terms that survive at
zero source, pruned after each derivative (the same results; the electron gas five times faster); `build_pdf.sh` and `build_manual.sh` run pdflatex through latexmk, again only when a
reference has moved; `assemble.py` writes the single orders without a leading sign only for the pictures of the README;
the long formulas of the showcase and the manual are broken by one module, `Development/latex_lines.py`, and a broken
formula of the manual that has become short is no longer shown in its old form.

**Pictures.** `DeclareBlob` has the shape `"Triangle"`, for the vertex of a one-body operator (two fermion legs on the
left); the line style `"DoubleWavy"` draws a dressed interaction (W). A source at the end of a single line (the diagrams of
Z[J] at a non-zero source) is drawn at the point; fermion diagrams with sources are laid out on rails. The README has a
gallery of diagrams drawn by the package.

**Fixed.** A nested integral over variables with the same names as the outer ones (an expression in canonical form
substituted into another) kept its own variables only by chance; `LabelFunction` renames the integration variables of the
expression it wraps, so that its arguments are never captured.

**Formulas.** `ExpressionToLaTeX` also names subexpressions (`"Names" -> {FermiFunction[xi[p1]] -> "n_1"}`) and writes the
arguments of the heads given by `"Subscripts"` as subscripts (ξ_p instead of ξ(p)). The long formulas of the manual and of
the showcase are no longer broken inside parentheses. The load-time check of the package is in its own source file,
`Development/src/08_footer.wl`.

**QCD and other field theories.** `DeclareLocalVertex` declares kernels of several labels — the vertices of a gauge
theory in condensed notation, whose labels carry Lorentz, colour and spinor indices at one point — which the pictures
draw as points instead of grey blobs. New line styles: `"Curly"` (gluons, a coil computed point by point) and
`"DottedArrow"` (ghosts); a bubble of bosonic lines between neighbours on the axis is a circle. The showcase has a section
on QCD (the gluon, quark and ghost self-energies and the quark–gluon and three-gluon vertices at one loop, the quark
self-energy at two loops), the Schwinger–Dyson equation and the crossing symmetry of the full vertex of the electron
gas, checked to third order, and φ⁴ theory to two loops (the four-point vertex to λ³); the guided tour and the manual have
QCD examples.

**Tree expansion with fermion lines.** `ConnectedFunctionFromVertices[labels, "FermionLines"]` gives the connected
functions of fermions as trees of directed propagators G = −⟨ψψ̄⟩ and vertices with as many outgoing as incoming legs,
projected from the superfield trees; checked against the electron gas (the 36 diagrams of the connected six-point
function at second order are the nine trees of two bare vertices).

**The showcase, reorganized.** It follows the path of field theory, with a map on its second page: free fields and Wick's
theorem; vacuum bubbles and the linked-cluster theorem; moments and cumulants, W = log Z; the self-energy and the Dyson
equation; the 1PI effective action and the connected functions as trees of vertex functions (bosons and fermion lines,
checked against perturbation theory); the 2PI effective action and the Luttinger–Ward functional; the Bethe–Salpeter and
the parquet equations, explained before they are checked. Every step is shown for φ⁴ theory and for the electron gas.

**Complex bosons.** `SelfEnergy` and `IrreducibleVertex` take `"Statistics" -> "ComplexBoson"` (G = <φ φ̄>,
Σ(x, y) = −δΦ/δG(y, x)); `"Boson"` with a propagator that is not symmetric gives `FunctionalDiagrams::realboson`.

**Pictures.** `DiagramPlot`, `DrawDiagram` and `DiagramToTikZ` have `Options`, so that `SetOptions` changes their defaults,
and unknown option names give a message; `DiagramPlot` uses more columns for many diagrams. Gluon coils are mirror
symmetric (half-integer turns); a self-loop of coils or waves is a larger circle on two tangent lines, which leave its
vertex at an angle; a gluon, quark or ghost loop joined by coils to two neighbours on the axis is narrower than their
distance, its lines bent into one arc with it (the gluon loop as the others). Fixed: vacuum trees of tadpoles (two
vertices at one point); complex and inexact coefficients, π and e in LaTeX; labels made by `Module` in TikZ; a local
vertex without labels; `"Columns" -> 0` and `"Scale" -> Automatic` (the defaults). In the general layout of four-point functions a vertex
with two legs at opposite corners has the labels of the right corners exchanged (it was drawn at the centre, with
crossing legs: the two-loop vertex of φ⁴ theory). An insertion on one or the other line of a bubble is drawn above or below it (two
different diagrams of the density correlation looked alike); of two crossing arcs on a rail one is drawn on each side;
pairs of loops in vacuum diagrams are horizontal; the bubbles of a three-point vertex are drawn alike; the cross of
impurity lines is larger. `DeclareBlob[h, label]` draws a factor of two or more labels as a hatched, cross-hatched, grey
or white circle or square with its label inside (the legs at the corners of a square for four labels), so that the exact
equations — Dyson, Schwinger–Dyson, Bethe–Salpeter, parquet — are drawn by the package and shown in the showcase.

**More checks, fewer silently wrong results.** Grassmann-odd factors multiplied with `Times` abort the evaluation; an odd
factor squared vanishes. External labels that do not occur in the expression, or that are not symbols, give a message
(`FunctionalDiagrams::label`, `::labelsymbol`); so do unknown values of `"Statistics"` and `"Method"`, a `DysonSeries`
whose `"MaximumOrder"` leaves out a zeroth-order self-energy, and `ConnectedFunctionFromVertices` with fewer than two
labels. Fixed: closed loops at local (Hubbard) vertices are counted per line head; `SkeletonDiagrams` of vacuum diagrams
keeps the 2PI ones; powers of delta functions between external labels are normalized; `ResetDeclarations` undoes the
symmetries; the canonical form of a term with nested integrals no longer depends on the names of its outer variables
(`SimplifyDiagrams` could leave two equal terms apart).

**Repository.** A picture of diagrams at the top of the README (`Development/make_readme_figure.sh`);
`Development/check_consistency.py`, run by GitHub on every push, checks that the package, the notebook and the
documentation of the public names are up to date; `.gitattributes` marks the generated files.

**Names.** What the documentation called Kleinert's derivation rules and Kleinert's trees is the tree expansion of the
connected functions in full propagators and vertex functions: the relations are standard consequences of the Legendre
transform. Kleinert and coworkers are cited for building diagrams by differentiating with respect to the propagator
(`Σ = δΦ/δG`, `Γ⁰ = δΣ/δG`). The showcase and the manual have a list of references.

**Messages instead of silently wrong results.**

* Results print in a terminal session: the compact display forms are given for `StandardForm` and `TraditionalForm`
  only (in `OutputForm` they formatted themselves endlessly, `Format::forml`).
* Fields declared before their Grassmann source are Grassmann odd: the order of the declarations no longer changes the
  fermion signs.
* A derivative with respect to an undeclared source (`FunctionalDerivative::nosource`) and a generating functional
  without any declared source (`FunctionalDiagrams::nosource`) give a message.
* A composite operator may be written `psibar[a] ** psi[a]` in the list of fields; Grassmann fields multiplied with
  `Times` give `FieldDerivative::times` and `$Failed` instead of hanging the kernel.
* An order that is not a non-negative integer, `{k}` or `{kmin, kmax}` gives `PerturbativeCorrelationFunction::order`;
  `PerturbativeFullVertex` computes only the orders asked for with `{k}` and `{kmin, kmax}`.
* A line head with more than two arguments, such as a spin-dependent propagator, gives `FunctionalDiagrams::lineargs`:
  the cuts treat it as a vertex.
* `IntegralOver[f, x]` is `IntegralOver[f, {x}]`; `OrderPart` accepts a list of orders.

**Documentation.** Spin is summed explicitly with `Sum` (the manual suggested integrating over a spin label, which leaves
traces that the classes of diagrams do not count). Showcase: the irreducible vertex is reducible in the ph̄ or pp channel
beyond first order only up to third order; one- and two-particle irreducibility cut propagator lines; the parquet
equations of φ⁴ theory are checked (`"Statistics" -> "Boson"`); the Ward identity is written with its right-hand side;
the numbers of trees are the terms a(n−1) of Schröder's fourth problem; conventions for phonons and disorder; a paragraph
on the passage from labels to momenta. Usage messages and manual agree on the options (`ImageSize` is the width of the
picture, `"Labels"` applies to TikZ), the `"Method"` option and the forms of the order.

## 2.1.0 (October 2026)

**Pictures.** A new layout for diagrams of fermions with density-type couplings (two-body interactions, phonons,
impurities), in which every vertex lies on one fermion line, open or closed:

* the open lines are straight: the axis of a two-point function, or two parallel rails for a four-point function,
  horizontal or vertical according to which external points they join, with the legs pointing to the corners of a
  rectangle; a rail that would join opposite corners has the labels of the right corners exchanged, so that the lines
  stay parallel (no more crossing fermion lines in the exchange diagrams of the particle–particle channel);
* the interaction lines between the rails are perpendicular to them, so that ladders are ladders, and two that cross
  make a symmetric cross; arcs go to the side of their rail where they cross nothing;
* closed loops are circles (bubbles for two vertices) that hang from the lines and from each other pointing away from
  them; a loop joined to several vertices of a line sits above them, with its lines perpendicular to the line; chains
  of loops between two points are straight; the vertices along the lines are spaced so that no two loops overlap;
* the sides of arcs and loops are chosen counting conflicts, among them a loop hanging under a bubble or an arc under a
  loop, which are now avoided;
* a bubble joined to two points of a line that are further apart than its width (a dressed interaction line) keeps its
  size and hangs below them, its two interaction lines bent into one round arc with it;
* vacuum diagrams made of three or more loops joined as a tree are drawn around the most central loop, turned so that
  the lines leaving it are symmetric about the vertical axis; three or more interaction lines that would be diameters of
  a loop, all meeting at its centre, cross in pairs instead.

**Trees of vertex functions** (the tree expansion of `ConnectedFunctionFromVertices`) have a layout of their own: the central vertex, or the two central
vertices, in the middle, chains of vertices straight, the legs at the ends of a chain fanned outwards and those along it
perpendicular to it, the external points counterclockwise in the order of their labels. Other diagrams (bosons,
propagators alone) keep the general layout.

**TikZ.** Arrow heads are drawn as small filled polygons and wavy or zigzag arcs as curves through computed points, so
that the pictures compile at any scale (the markings decoration of TikZ failed with "Dimension too large" on small
arcs). The code needs only the TikZ library `decorations.pathmorphing`. Waves are 1.6 mm long at the default scale and
at least 1.3 mm in small pictures.

**Size of the pictures.** `DiagramToTikZ` has the options `"LineWidth"` (in pt; arrow heads, dashes and double lines
follow it) and `"LabelFont"` (the font command of the labels); `"LineWidth"` also sets the thickness in `DiagramPlot`
and `DrawDiagram`. Dashed lines scale with `"Scale"`.

**Formulas.** `ExpressionToLaTeX[expr]` writes an expression of the package as LaTeX: integrals with their variables as a
subscript, functions with their arguments, delta functions, Grassmann products in their order, fractions; options
`"Names"` and `"Labels"`. Display names given with `DeclareDisplayName` are used; square roots, logarithms and the
trigonometric and hyperbolic functions are written with their usual symbols.

**Documentation.** A reference manual in PDF, `Documentation/FunctionalDiagrams_Manual.pdf`, with every function,
option and convention, and examples computed by the package (`Documentation/make_examples.wl`); it replaces
`Documentation/Reference.md`. The showcase begins with the engine of the package, functional derivatives (canonical
form, commuting and Grassmann sources, discrete indices, implicit dependence, derivatives with respect to a propagator,
moments and cumulants), before the diagrams; it has a section on the tree expansion of connected functions, with all the trees up to six points
grouped by shape and checked against the zero-dimensional theory; its pictures are smaller.

**Fixes.** The examples no longer call a function `K`, a built-in symbol of Mathematica (the generic name of a summation
index), which made the declarations warn: it is `Q` now.

## 2.0.0 (October 2026)

First release as a standalone project.

**Names.** All public functions have explicit names. Correspondence with version 1:

| version 1 | version 2 |
|---|---|
| `FD` | `FunctionalDerivative` |
| `Int` | `IntegralOver` |
| `Dlt` | `DeltaFunction` |
| `GProd` | `GrassmannProduct` |
| `FDSimplify` | `SimplifyDiagrams` |
| `Terms` | `DiagramTerms` |
| `ZeroSources` | `SetSourcesToZero` |
| `FieldExpectation` | `FieldExpectationValue` |
| `WickExpectation` | `GaussianExpectationValue` |
| `PerturbativeCorrelator` | `PerturbativeCorrelationFunction` |
| `VacuumDiagrams` | `ConnectedVacuumDiagrams` |
| `ConnectedPart` | `ConnectedDiagrams` |
| `DropVacuum` | `DropVacuumBubbles` |
| `IrreducibleQ`, `IrreduciblePart` | `ParticleIrreducibleQ`, `ParticleIrreducibleDiagrams` (external labels now before the line heads) |
| `SkeletonPart` | `SkeletonDiagrams` |
| `ConnectedFrom1PI` | `ConnectedFunctionFromVertices` |
| `KG`, `KGamma`, `KPhi`, `KJ` | `FullPropagator`, `VertexFunction`, `MeanField`, `ExternalSource` |
| `DiagramGraph` | `DrawDiagram` |
| `DeclareOdd`, `DeclareGraded` | `DeclareGrassmannOdd`, `DeclareGrassmannGraded` |
| `DeclareDerivative` | `DeclareFunctionalDerivative` |
| `DeclareIndexValues` | `DeclareDiscreteIndexValues` |
| `DeclareDirected`, `DeclareInteraction` | `DeclareDirectedPropagator`, `DeclareInteractionLine` |
| `ZZ[d, k]` (integration variables) | `DummyVariable[d, k]` |

**New.**

* Wick's theorem as a sum over pairings (`"Method" -> "WickPairings"`, the default for Gaussian generating
  functionals), with interchangeable vertices and identical fields enumerated once: perturbation theory is much faster.
* `ConnectedCorrelationFunction` (fields at the same point form a composite operator, as in density correlations);
  `OrderPart`; orders `{k}` and `{kmin, kmax}` in the perturbative functions.
* `AmputateExternalLegs`; `SkeletonDiagrams` for amputated vertex functions; `ReducibleDiagrams`, `IrreducibleDiagrams`
  (cuts that separate given groups of external points); `ClosedLoopCount`, `DropClosedLoops`.
* `LuttingerWardFunctional`, `SelfEnergy`, `IrreducibleVertex` (Γ⁰ = δ²Φ/δGδG), `DysonSeries`.
* `PerturbativeFullVertex`, `TwoParticleReducibleQ`, `ParquetDecomposition`, `BetheSalpeterLadder`,
  `CheckParquetEquations`: the parquet equations, checked order by order, for fermions and real bosons.
* Delta functions between external labels are used to simplify (δ(a, b) f(a, b) = δ(a, b) f(a, a)), so that equal
  vertex diagrams are recognized.
* Pictures in a standard style: labels joined by delta functions are one point (no more lines of zero length);
  amputated functions are drawn with external legs, with arrows for fermions and without for density vertices;
  four-point vertices with the legs counterclockwise from the top left, the option `"ExternalOrder"` to change it;
  disconnected parts side by side; line styles (`DeclareLineStyle`: arrows, double lines, wavy, dashed, dotted,
  zigzag, impurity lines); `DeclareDisplayName`; `DiagramToTikZ` for LaTeX, with the external lines on the math axis.
  Closed fermion loops of three or more vertices are circles, placed where the lines joining them to the rest are
  short and cross nothing; vacuum parts made of several loops have them side by side or one inside the other. In
  two-point functions, of two crossing arcs one is drawn below the axis, and arcs and hanging parts are put above or
  below so as to cross least. In vertex functions, open fermion lines between neighbouring external points run
  straight along the sides, with arcs for the lines that join their vertices.
* The message `FunctionalDiagrams::builtin` when a declared name is a built-in symbol of the Wolfram Language (such as
  `GreenFunction`, a built-in function of Mathematica, whose definitions interfere): the examples use
  `DressedGreenFunction`.
* Among the layouts of equal quality the drawing takes the symmetric one, so that symmetric diagrams are drawn
  symmetric; in TikZ the start angles of arcs are between 0 and 360 degrees.
* The guided tour and the showcase (`Showcase/FunctionalDiagrams_Showcase.pdf`, computed and drawn by
  `Showcase/make_figures.wl`) work out the electron gas to third order: G, Σ, the vacuum diagrams, Φ, Σ[G], Γ⁰ and the
  full vertex, with the checks (Dyson equation, Σ is the skeleton self-energy, Ward identity, Γ⁰ = F − R_ph, the
  parquet equations).
* Loading the package twice in the same session no longer produces errors.
