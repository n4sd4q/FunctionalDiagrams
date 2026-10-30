# FunctionalDiagrams — notes for Claude Code

A Mathematica package for functional derivatives and the Feynman diagrams they generate (see `README.md`).
Author: Nazzareno Africani (SISSA). Code, comments and documentation are written in English. Personal notes of a
developer (language, machine, local workflow) go in `CLAUDE.local.md`, which is not versioned.

## Layout

- `FunctionalDiagrams.wl` — the package, a single file **generated** from `Development/src/*.wl` by
  `Development/build.sh`. Never edit it directly: edit the sources, then run `Development/build.sh`.
- `Development/src/` — `00_header` (usage messages, version), `01_notation_declarations`,
  `02_terms_derivative_canonical` (terms, functional derivative, canonical form), `03_wick_perturbation`,
  `04_classes_vertices` (classes of diagrams, Luttinger–Ward, vertices, parquet, exact equations: `ExactEquation`,
  `CheckEquation`, `LabelFunction`, `PerturbativeDensityVertex`), `05_drawing` (layout), `06_tikz_footer` (TikZ export,
  LaTeX formulas), `07_matsubara` (evaluation in momentum and Matsubara frequency, cutting rules), `09_dimreg`
  (Feynman integrals in dimensional regularization: loop integrals of the diagrams, one-loop integrals in closed form,
  Laurent expansion in ε, infrared rearrangement, renormalization-group functions from Z factors), `10_qed` (QED at one
  loop: Dirac algebra in d, `QEDIntegrand`, `IntegrateOneLoop`, Feynman-parameter integrals, form factors), `11_effective_actions` (the 1PI and 2PI
  effective actions, traces, Potthoff's functionals, zero dimensions; 09, 10 and 11 are concatenated before 08), `08_footer` (`$publicNames`, load-time checks, the banner and the update check of a notebook session, end of the
  package).
- `Examples.wl` — the guided tour as a script. `Examples.nb` is generated from it:
  `python3 Development/make_notebook.py Examples.wl Examples.nb`. Edit the `.wl`, never the `.nb`.
- `Documentation/` — the reference manual. `FunctionalDiagrams_Manual.tex` (edit it directly) uses the examples computed
  by `make_examples.wl` (every example is a code string, evaluated and written to
  `examples/NAME.wl`, with its output as `NAME.tex` (formula), `NAME_pictures.tex` (diagrams, as `DiagramPlot` draws the
  output, with the sizing options only: a code ending with `DiagramPlot[...]` is drawn with its arguments) or `NAME.txt`);
  `build_manual.sh` runs `make_index.py` (index of functions, long formulas broken into lines by
  `Development/latex_lines.py`, shared with `Showcase/assemble.py`) and pdflatex (through latexmk when installed). Keep the
  manual in sync with the usage messages.
- `Tests/RunTests.wl` — the test suite; `Tests/RunTests.out` is its last output. `Tests/QEDOneLoop.wl` is the one-loop QED
  computation (self-energy, vacuum polarization, vertex), loaded by the tests and runnable on its own (it prints a report);
  `Tests/MatsubaraBruteForce.wl`, the brute-force Matsubara sums of the integrand of a diagram, is loaded by the tests and
  by `Showcase/make_figures.wl`; `Tests/LatestRelease.json` is a saved answer of GitHub (its latest release, trimmed) for
  the tests of the update check, which need no network. `Tests/EffectiveActions.wl` and `Tests/Superfields.wl` are loaded by the suite too, with
  the shared helpers of `Tests/Harness.wl`; `Tests/FullyIrreducibleVertices.m` stores the reference vertices of the parquet
  check (`RECOMPUTE_REFERENCES=1` rebuilds them).
- `Showcase/` — the presentation of the package. `make_figures.wl` (Mathematica, about two minutes) computes and draws
  everything into `figures/`: `NAME.terms` (one term per block: weight, `%%PICTURE%%`, tikzpicture; a sum written
  order by order is drawn whole, as `DiagramPlot` draws it, and split into `NAME_m.terms`), `formula_NAME.tex` (formulas, by `ExpressionToLaTeX`) and `values.txt`
  (checks, numbers of diagrams, timings). `build_pdf.sh` runs `assemble.py` (→ `figures/*.tex`, `figures/values.tex`,
  `figures/parquet_table.tex`, long formulas split) and pdflatex. In the `.tex`, `\begin{ordersum}{lhs}
  \orderrow{V^2}{name_2} ... \end{ordersum}` draws a sum with one coloured band per order (its left-hand side never
  alone at the foot of a page), `\figuresum{lhs}{name}` a plain sum (as a displayed formula, never at the top of a page
  apart from the line that introduces it), `\formula{name}` a formula, `\val{Key}` a value of `values.txt`. The pictures are small:
  `pictureOptions` in `make_figures.wl`. `Development/check_showcase.wl` (Mathematica, about two minutes) runs every code
  block of the `.tex` in order in one fresh kernel, as a notebook reads it, and compares each output with what the page
  shows after the block (read by `Development/showcase_blocks.py`): the pictures of a sum (weights and TikZ code, band by
  band), a formula (as `ExpressionToLaTeX` writes it with the options of `writeFormula` in `make_figures.wl`, whose
  `"Display" -> f` renames symbols or indices for the display only), a value; a block after the line `% not run` is
  skipped, and part of an output shown (some orders) is listed, the text saying so.
- `Logo/` — the logo (SVG and PDF: the badge, which reads on light and dark pages; a white version for the dark title
  pages of the manual and the showcase; `_small.svg` for 16–32 px), drawn by `Development/make_logo.py`; the social preview
  `social_preview.png` is built by `Development/make_readme_figure.sh`.
- `Media/` — the demo video `demo.mp4` (1920×1080, 60 fps, H.264, under a minute), its looping preview
  `demo_preview.webp` (an animated WebP at 60 fps, 960×540, at the top of the README, a plain `<img>`) and a poster frame
  `demo_poster.png`, made by `python3 Development/make_demo_video.py` (about four minutes, most of them writing the WebP;
  pdflatex, pdftocairo, PIL, ffmpeg) from the figures of the showcase and the first example of the README: run it again
  when they change, and look at frames of the result (`--frames` writes some to `build/video/frames`). Its lengths in
  pixels are written for a frame 1280 px wide and scaled to the frame (`S`, `px`).
- Map of the functions (in the README): `Development/make_function_map.wl` (Mathematica) reads the group (block of
  `00_header.wl`), kind (function or inert data head), usage message and dependencies of every public name (from its
  held definitions, through the private helpers, stopping at public names) into `Documentation/function_map.json`;
  `Development/make_function_map.py` (numpy, matplotlib for the text metrics) lays out the names by their role in a
  calculation, in six stages (declare, write a functional, differentiate around `FunctionalDerivative`, classify and
  check, draw, evaluate; `STAGE_OVERRIDE` holds the few editorial placements, `PARTNER_NOTE` the captions of the names
  `FunctionalDerivative` itself uses, drawn beside it), with the names built on another indented
  under it and "uses" notes, and writes `Documentation/function_map.svg` (a white card legible on light and dark pages,
  the one the README shows) and the self-contained interactive `function_map.html` (the same layout, light and dark,
  every dependency drawn for the selected name, under a bar in the style of the site that links back to it).
- `wiki/` — the GitHub wiki (practical guides and a FAQ: what the README, the showcase and the manual do not already
  give, linking the manual for the details), as Markdown with GitHub's conventions: `Home.md`, `_Sidebar.md` (every page
  listed), `_Footer.md`, `Page-Name.md` (GitHub writes the title from the file name: no `#` title on a page but Home), links
  `[text](Page-Name)`, pictures in `wiki/images/`. The Action
  `.github/workflows/wiki.yml` mirrors the folder into the wiki repository (`FunctionalDiagrams.wiki.git`, branch master)
  on every push to main that touches it: edit the pages here, never on GitHub. `Development/check_wiki.wl` runs the code
  of every page (one fresh kernel per page, the pages at the same time): each ```` ```mathematica ```` block must give
  exactly the ```` ```text ```` block or the picture `![...](images/NAME.svg)` that follows it (a picture is the TikZ code
  of the block's last `DiagramPlot`, typeset on a white card; `--images` redraws the stale ones; `<!-- not run -->`
  before a block for installation lines and paths); `check_consistency.py` checks the version, the public names and the
  links of the pages.
- `Development/TODO.md` — things worth doing sooner or later; keep it up to date.
- `Development/benchmark.wl` — timings of about fifty representative calculations (`--list`; a case or group by name,
  `--runs 3` for the minimum of three runs), each output compared with its reference in `build/optim/reference/`
  (`--save`, from the code before a change), so that a faster package is checked to give exactly the same results.
- `index.html` — the landing page of GitHub Pages, a site of its own (hero with the buttons, the demo video, what the
  package does with the panels of the README and the map, the first example, Checks and Related software, installation,
  the documents, how to cite), **generated**: `python3 Development/make_site.py` writes it from the template
  `Development/site_template.html` (markup, CSS, the few lines of JavaScript: copy buttons, the light/dark toggle) and
  the sources of every fact on it: the README (tagline, opening paragraphs, install lines, the first example with its
  pictures and captions, the gallery, the scope, the text of the map, Checks, Related software, the BibTeX), `PacletInfo.wl`,
  `00_header.wl`, `CITATION.cff`, `Tests/RunTests.out`, `function_map.json`, the PDFs (pages), the video (length). Never
  edit it directly; no external scripts, fonts or trackers. A README section it needs that cannot be found stops the
  script with a message. `.nojekyll` (empty) — GitHub Pages serves the main branch as it is, so that the README can
  link the interactive map (`https://n4sd4q.github.io/FunctionalDiagrams/Documentation/function_map.html`) and the video
  (`.../Media/demo.mp4`).
- `PacletInfo.wl` — the repository is a paclet (``Needs["FunctionalDiagrams`"]`` after `PacletDirectoryLoad` or
  `PacletInstall`); `Development/build_paclet.wl` builds `build/FunctionalDiagrams-<version>.paclet` for a GitHub release.
  The version is the same in `PacletInfo.wl`, `CITATION.cff` and `$FunctionalDiagramsVersion` (`00_header.wl`).

## Workflow

1. Edit `Development/src/*.wl`, then `Development/build.sh`.
2. Run the tests: `wolfram -script Tests/RunTests.wl > Tests/RunTests.out`; every check must pass.
3. If results or pictures change: `cd Showcase && wolfram -script make_figures.wl && ./build_pdf.sh`, then look
   at the pages of the PDF (symmetric diagrams drawn symmetric, nothing overlapping, no overfull lines), and run
   `wolfram -script Development/check_showcase.wl` (also after editing the code of the showcase): every block must pass.
4. Every time something is implemented and works: commit it, then right away document it in the showcase, the manual
   (`cd Documentation && wolfram -script make_examples.wl && ./build_manual.sh`), `Examples.wl` (then regenerate
   `Examples.nb`), `README.md`, the pages of the wiki that treat it (`wiki/`, then `wolfram -script
   Development/check_wiki.wl`; every block must pass) and `CHANGELOG.md`, as clearly and linearly as possible and in the right place, so that the
   documents keep a natural, clean order (never appended at the end). Everything is generated by functional derivatives:
   the documents show the derivative that produces an object before any shortcut function.
5. `python3 Development/check_consistency.py` (also run by GitHub on every push) checks that the package, the notebook,
   the site and the documentation of the public names are up to date: after a change to the README, the tests, the PDFs
   or the version, `python3 Development/make_site.py` writes `index.html` again (look at it in a browser, light and
   dark, wide and at 375 px, when the template changes); `Development/make_readme_figure.sh` redraws the pictures of
   the README (`Development/readme_gallery.tex`) and the social preview from the figures of the showcase (the social
   preview keeps a panel of its own, `\socialpreviewpanel`: the PNG is uploaded by hand, so it must not change unasked);
   `wolfram -script Development/check_readme.wl` runs the first example of the README and checks that each `DiagramPlot`
   gives the bands of the panel after it, weights and pictures (run it when the example, its panels or the figures
   change). The README uses
   plain `<img>` elements, one file legible on light and dark pages for each picture: no `<picture>` (the GitHub mobile
   apps do not show it). When public names or their dependencies change, redraw the map of the
   functions: `wolfram -script Development/make_function_map.wl && python3 Development/make_function_map.py` (about
   fifteen seconds; look at the SVG), also after a version bump: `check_consistency.py` compares the map with `$publicNames` and
   the version.

`wolfram -script FILE` and `wolframscript -file FILE` do the same; on some installations only the first one works.
`check_consistency.py` on GitHub catches a commit in which a generated file is stale.

## Conventions

- `G0(x, y) = -<T psi(x) psibar(y)>`; free fermions `Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]]`;
  `psi` ↔ δ/δ`etabar`, `psibar` ↔ −δ/δ`eta`. An arrow from `y` to `x` is `G[x, y]`.
- `S_int = 1/2 ∫ V(x, y) psibar(x) psibar(y) psi(y) psi(x)`; the weight written with a picture is such
  that the weight times the values of the elements is the term: an interaction line stands for −V and a vertex for minus its
  coupling (the minus sign comes from expanding exp(−S_int)); so Fock +1, Hartree −1, the φ⁴ tadpole +½. Interaction lines
  are the heads of `DeclareInteractionLine` (also the Hubbard–Stratonovich propagators and W), couplings those of
  `DeclareCoupling` (λ, g: λⁿ → (−1)ⁿ), local vertices (`DeclareLocalVertex`) stand for minus their kernel; propagators
  (phonon, disorder correlator, photon, gluon) stand for themselves; `"Weights" -> "Coefficients"` shows the coefficients.
  Only the displayed weights follow this convention: the expressions are unchanged.
- Φ = −(2PI skeleton vacuum diagrams with G0 → G); Σ(x, y) = δΦ/δG(y, x); Γ⁰(1, 2; 3, 4) = δΣ(1, 2)/δG(4, 3), so that
  Γ⁰·Y puts Y in place of one line of Σ. Channels: ph (12|34), ph̄ (14|32), pp (13|24); Γ⁰ = F − R_ph = Γ_irr + R_ph̄ + R_pp,
  and Γ_irr (the fully irreducible vertex) is the bare vertex up to third order.
- 1PI action: Γ[φ̄] = S[φ̄] + ½Tr ln S''[φ̄] + Γ₁[φ̄]; fermions S[Ψ̄, Ψ] − ½Tr ln(−S''[Ψ̄, Ψ]) + Γ₁, S'' the Nambu matrix of the
  second derivatives in (Ψ, Ψ̄) (normal block D⁻¹ below, anomalous blocks δ²S/δΨ̄δΨ̄ = VΨΨ, δ²S/δΨδΨ = VΨ̄Ψ̄); superfield
  S[𝚿] − ½Tr ln(−S''[𝚿]) + Γ₁. Γ₁ = −(1PI vacuum diagrams with two or more loops, lines S''⁻¹, vertices of degree three or more),
  as Γ₂ is for the 2PI action; Γ₂ at G = S''⁻¹ is the 2PI part of Γ₁. S is written whole, its free and interacting parts named
  only where they are used; the one loop is never called Γ^[1] (Γ^[ℓ], ℓ ≥ 2, is the part of Γ₁ with ℓ loops).
  `EffectiveAction1PI` expands fermions and superfields in G0 (bare lines, the vertices of degree two kept).
- 2PI action with means (Cornwall, Jackiw and Tomboulis): Γ[φ̄, G] = S[φ̄] + ½Tr ln G⁻¹ + ½Tr(D⁻¹[φ̄]G − 1) + Γ₂, D⁻¹ = S'';
  fermions S[Ψ̄, Ψ] − Tr ln(−G⁻¹) − Tr(D⁻¹[Ψ̄, Ψ]G − 1) + Γ₂, D⁻¹(x, y) = δ²S/δΨ̄(x)δΨ(y) = G0⁻¹ − Σ_HF[−ΨΨ̄] (the first-order
  Σ_HF[G] with −Ψ(x)Ψ̄(y) in place of G(x, y): the Hartree and Fock potentials of the means); superfield −½Tr(S''[𝚿]𝒢 − 1). The
  Dyson equation is G⁻¹ = D⁻¹ − Σ[Γ₂]. Never write the mean-field part of the trace as ⟨S_int^(2)⟩_G.
- Vertex of the density: Λ(x1, x2; z) = −δG⁻¹(x1, x2)/δU(z), ⟨ψψ̄ρ⟩_c = L₀Λ, ⟨ρρ⟩_c = −L₀Λ; Λ = γ + Γ⁰L₀Λ = γ + FL₀γ,
  Σ = Σ_H − VGΛ. Hedin: W = V − V⟨ρρ⟩_cV, P = −(proper part of ⟨ρρ⟩_c) = L₀Λ̃, Σ = Σ_H − GWΛ̃ (L₀ a pair of propagators, never written GG). Hubbard–Stratonovich:
  ⟨φφ⟩_0 = V and the coupling −i∫φρ (φ⁴: ⟨σσ⟩_0 = λ/3 δ, coupling −(i/2)∫σφ²). Real bosons: Γ⁰ = −2δΣ/δG, and a pair of
  lines in a channel carries −½.
- Superfields (`"Statistics" -> "Superfield"`): 𝛙 = (ψ, ψ̄) (bold ψ in the documents: χ is the density response), one
  Grassmann field with one source; its propagator 𝒢 = −⟨T𝛙𝛙⟩ is the antisymmetric Nambu matrix with the blocks G,
  𝒢_ψψ = −⟨Tψψ⟩ and 𝒢_ψ̄ψ̄ = −⟨Tψ̄ψ̄⟩ (never called F: F is the full vertex). Γ = Φ − ½Tr ln(−𝒢⁻¹) − ½Tr((𝒢0⁻¹ − K)𝒢 − 1),
  the source entering as S + ½∫𝛙K𝛙; 𝚺 = −2δΦ/δ𝒢 (derivatives with respect to antisymmetric kernels are antisymmetrised),
  the gap functions Δ = 𝚺_ψ̄ψ̄ (= V𝒢_ψψ at first order) and Δ̄; Γ⁰ = 4δ²Φ/δ𝒢δ𝒢, whose channels (`"Channel"`) s, t, u are
  ph, pp, ph̄, with the ladders +½, −½, +½; `SuperfieldComponents` projects on ψ and ψ̄. Tests in `Tests/Superfields.wl`.
- Matsubara: G0(k, iω) = 1/(iω − ξ_k); the package's ⟨ρρ⟩_c is minus the Lindhard χ0, and its boson propagator ⟨φφ⟩ is
  minus the textbook D0.
- Dimensional regularization (`09_dimreg.wl`): Euclidean, a line is 1/(q² + m²), d = 4 − 2ε, every loop μ^(2ε) (e^γE/4π)^ε ∫d^dk/(2π)^d (the
  MS-bar measure: minimal subtraction is MS-bar, no γE or ln 4π in the results); φ⁴ with λ/4!, Γ⁽⁴⁾ = −(amputated 1PI part
  of the connected four-point function), β = μ dλ/dμ, γ_φ = ½ μ d ln Z_φ/dμ, γ_m2 = μ d ln m²/dμ.
- QED at one loop (`10_qed.wl`): Minkowski, Peskin–Schroeder except for the sign of the charge: metric (+,−,−,−), {γ^μ, γ^ν} = 2g^μν, Tr 1 = 4, no γ5
  (`GammaMatrix[5]` refused), abstract d-dimensional indices; `FeynmanDenominator[q, m]` = 1/(q² − m² + i0), electron line
  i(q̸ + m)/(q² − m²), photon −i[g − (1 − ξ)qq/(q² − λ²)]/(q² − λ²) (λ optional, an infrared regulator); e > 0 everywhere, the
  electron has charge −e: S_int = +e∫ψ̄γ^μψA_μ, vertex +ieγ^μ, α = e²/4π. The package expands exp(−X), so the interaction
  passed is X = −iS_int = e∫V ψ̄ψA with V = −iγ^μ δδ (`QEDVertex -> {"Vertex", -I}`), and the 1PI functions are −iΣ, iΠ^μν,
  +ieΓ^μ (Γ^μ = γ^μ + δΓ^μ); the MS-bar measure as above (P&S's 2/ε is
  our 1/ε); Feynman-parameter integrals with Δ − i0.
- In the documents φ⁴ theory and the electron gas are both shown at every step, complete up to third order, grouped by
  order; the third order is not presented as anything special.
- Names of propagators and fields must not be built-in symbols (`GreenFunction`, `Gamma`, `C`, `D`, `E`, `I`, `K`, `N`,
  `O`, …); this holds for the examples too (a symmetric kernel is `Q`, not `K`).
- Options: every public function that takes options has `Options[f]`, with every name it accepts (those it passes on to
  another function too), and resolves them where the call enters the package with `callOptions[f, {opts}]`
  (`01_notation_declarations.wl`): an unknown name gives `FunctionalDiagrams::optx` and is ignored, the defaults come
  from `Options[f]` (so `SetOptions` works). The private code reads the resolved list with `optionValue`; a public
  function calls the private code of another one with its own options, never the public function (no second check, and
  `SetOptions` on the inner function does not change the outer one).

## Drawing rules (`05_drawing.wl`)

- Diagrams in which every vertex lies on one fermion line (open or closed) use the unit layout (`unitLayoutWithExternals`,
  `unitLayoutVacuum`): a two-point function has its open line on a horizontal axis (`layoutAxisUnits`), a four-point
  function its two open lines as parallel rails (`layoutRailUnits`), with the legs towards the corners of a rectangle,
  counterclockwise from the top left (`x1` out, `x2` in, `x3` out, `x4` in); `"ExternalOrder" -> {"Uncrossed", {x4, x2,
  x3, x1}}` for particle–particle diagrams drawn as ladders; a rail that would join opposite corners is laid out with the
  labels of the right corners exchanged, and its two legs cross back (below).
- Four external points, in every layout (boson or fermion, blob or not): the labels in the order given to the function
  (or `"ExternalOrder" -> {x1, ...}`, a fixed order) at the corners counterclockwise from the top left. A diagram whose
  channel joins opposite corners is drawn crossed: its layout exchanges the labels of one side (`layoutRailUnits` and
  `layoutWithExternals` the right ones, when a node would hold two opposite corners and the picture as given is not
  clean, `cleanLayoutQ`; `treeLayoutWithExternals` turns a tree of two vertices upright; a blob picture has the corners
  of its ports), and `crossedLegPositions` puts the two legs back as an X, their ends at the heights of each other's
  vertex, crossing 0.3 outside everything else and at about a right angle (a bubble they leave towards becomes a lens);
  when both legs leave blobs or vertices of bubbles towards the bubbles (a chain of bubbles, a tree of two vertex
  functions), at 60 degrees, further out (`crowdedQ`, `$crowdedCrossingAngle`), so that the lenses are fuller and the X
  clear of the blobs; with straight legs a bubble they leave towards cannot stay round; if they would cross another line, a loop circle through their vertices is drawn straight and the other sides are
  tried (`crossedLegDefects`). `"ExternalOrder" -> "Uncrossed"` (or `{"Uncrossed", {x1, ...}}`) takes per diagram the
  first of the order, the order with the right labels exchanged and the order with the top labels exchanged whose
  layout has fewest straight crossings (`layoutCrossingCount`): the labels move with their legs. Two and three points
  keep their layouts (any order of three is planar), and so do the source points of Z[J] (no order is given).
  Rails keep the picture low: along vertical rails the nodes are 0.75 apart, between horizontal rails the lines to loops
  are 0.5 long and the bubbles 0.7 wide (above and below them the lines to loops keep their length); a rail of one node (bare vertex) has its legs open at 110 degrees, 1.25 legLength
  long (up to 2.2 towards the corners beside a longer rail), a wide V, never a straight line. Loops are circles hanging from the lines and from each other (`placeBlock`, `placeUnit`, `placeLoopOnLine`; a loop
  joined to points of a line further apart than it is wide has the joined vertices over its lowest 100 degrees, the lines
  leaning inwards, never tangent to it); sides of
  arcs and loops by counting conflicts (`axisSideChoice`, `railArcSides`); spacing widened until no loops overlap.
  A bubble joined to two points further apart than its width keeps its size and its lines are bent into one arc with
  it (`bubbleBends` in `frameItems`), unless the bent line would leave its point within 45 degrees of a leg or another
  line there (then the lines are straight).
  A bubble (two lines between two vertices) whose vertices carry another line or leg that would run into it (closer to
  its chord than a right angle: the bubble of a triangle, a vertex above a bubble on the axis) is a lens, flattened until
  the line stays outside (`lensLimit` in `frameItems`); so is a bubble with a line or leg at about a right angle to its
  chord on one side only (it would leave along the arc of a semicircle; with lines on both sides, a loop sitting on a
  line, the bubble stays round). Vacuum trees of loops: `layoutVacuumTree` (the central loop turned symmetric about
  the vertical axis). Three diameters of a loop of six: one horizontal, between two vertices 25 degrees above the horizontal, drawn outside
  over the top, the other two crossing at 70 degrees at the centre (vertices at 155, 235, 305, 25, 55, 125 degrees, the
  outside one found by `graphDiameterQ`); two bubbles joined to each other at both vertices (`layoutVacuumLoops`): the
  vertices on the facing sides, 50 degrees from the line of the centres, the bubble drawn as its circle
  (`bubbleCircleKind`); a diameter crossed by two or more chords is horizontal, the chords vertical (a "#"); four or more
  diameters: the vertices moved alternately apart (`layoutVacuumCycle`).
- Anomalous propagators: `"DivergingArrows"` for 𝒢_ψψ (arrows into both ends), `"ConvergingArrows"` for 𝒢_ψ̄ψ̄ (arrows
  towards the middle). A fermion line may run through anomalous segments: chains, loops and density points are found by
  the kind (ψ or ψ̄) of each line end, and the weight of a picture reads an anomalous line along its fermion line (a loop in
  either sense, an open line from its first external label).
- Three or more open fermion lines with their ends at external points or sources, joined in a chain (Z[eta, etabar] with
  six sources): horizontal rails one above the other, the lines between them vertical, their free ends spread to common
  ends so that the rails are aligned (`layoutStackedRails`). The unit
  layouts are upright by construction: only the general layout of a picture without external points is turned along
  its principal axes (`alignPrincipalAxes`).
- Density points: an external label alone at its point that, in every term, ends exactly two propagators (one in and
  one out, or two undirected) and nothing else, and is not an argument of a local vertex, is a density insertion: a
  crossed circle at the point, no leg (`densityLabelsFor`). So is a label alone at the third corner of a triangle blob,
  twice a port of one blob and nothing else (χ = γLγ), or sharing its point with exactly an outgoing and an incoming leg
  (a bare vertex); every density point carries its
  label (`$densityNodeLabels`), so the vertex of the density looks the same in every picture.
- Loops: of two crossing chords the one bulging up or down is drawn outside, over the top (a loop with two diameters has
  a vertex at the top), on a loop hanging from a line or a point the one farthest from where it hangs; an interaction
  line along the arc of a round loop is well inside it, three or more lines along it (a sunset) a melon about the chord,
  never a second circle; a bubble between two of
  three or more external points with no free vertex keeps the width of a bubble; vacuum diagrams whose vertices all lie
  on loops of undirected propagators (`scalarLoops`) use `layoutVacuumLoops`; a single pendant part
  (`placePendantVertices`) is turned, within its free sector, to the direction farthest from what is placed. A chord
  drawn outside a loop is 0.4 + 0.3 (chord/diameter) outside the circle at its middle, so a long one does not hug it
  and a diameter does not rise far above the loop. A source point at the end of a nearly vertical or horizontal line
  (general layout of Z[J]) is moved to make the line exactly vertical or horizontal (`straightenedSourceLines`).
- General layout with external points: a vertex on a leg with a tadpole or a pendant part hanging from it is pulled
  towards its external point (`tutteEmbedding` with `outward`) and the external points are 1.15 times farther out, so
  the hanging part is not wedged between two legs. A coil self-loop (gluon loop at the four-gluon vertex) sits on its
  vertex, its lines leaving almost sideways, and a unit of one node joined to several points is raised so that its
  lines are steep (a circle on a V, not an X).
- Trees of vertex functions with the external points at the leaves (the tree expansion of `ConnectedFunctionFromVertices`):
  `treeLayoutWithExternals`. So are trees whose leaves are free ends (the labels integrated over: the shapes of W^(n)),
  their inner nodes vertex functions or bare vertices of three or more propagators (`freeEndTreeLayout`, before the unit
  layouts of vacuum parts): the free ends where the external points would be, the centre on the axis, the branches of a
  node arranged as a palindrome of their shapes (`palindromicArrangement`), so that a symmetric tree is symmetric about
  the vertical axis, and the legs along a chain on its upper side (a comb).
  Everything else uses the general layout (`layoutWithExternals`, `layoutVacuumComponent`).
- TikZ: arrow heads are polygons (`tikzArrowHead`) and wavy lines (straight or arcs) and zigzag arcs explicit curves with a
  half-integer number of waves, never decorations on arcs; where two wavy lines cross, the waves are flattened
  (`waveCrossingParameters`, `waveEnvelope`) so that the crossing reads as an X
  (the markings decoration fails with "Dimension too large" on small arcs); the code needs only
  `decorations.pathmorphing`.
  A wavy arc has a half-integer number of waves (mirror symmetric about its middle) and, when short or low (the Fock
  arc), waves in proportion to its height, down to 0.95 mm, with an amplitude of a quarter of a wave (`tikzLine`).
  The arrow head of a double line (G) is 1.45 times larger, so that it stands out of the two strokes.
- Disconnected pieces side by side (`diagramGeometry`): those with external points, and open lines between sources or
  means, keep their axis at y = 0; every other piece, the first one too, is centred on it.
- Of two crossing interaction arcs one is drawn above and one below the axis; two arcs on one side that overlap over less
  than half of one of them are round, so that they cross high above the axis at a wide angle; an arc over another one
  stays above it. Closed fermion loops are circles.
- Blobs (`DeclareBlob`): circles, squares, equilateral triangles for three-point vertices (two fermion legs on the left;
  `"TriangleUp"` is the same triangle turned) and diamonds (hatched vertically, never along their edges). The fills follow
  one rule in every document (the code shown declares the fill drawn): a vertex function the more densely the more
  diagrams it contains, the fully irreducible vertex Γ_irr `"Dotted"` (staggered dots about 0.14 units apart, the grid
  fitted to a box or a diamond so that its outermost dots are 0.06 inside every side; `blobDotsOf`), the irreducible parts
  (Σ, Δ, P, Γ⁰, Γ_ph, Γ_ph̄, Γ_pp, Γ_xc, Λ̃) `"Hatched"`, the full vertex functions (F, Λ, Γ⁽⁴⁾) `"CrossHatched"`, and the
  correlation and response functions (W⁽⁴⁾, L, χ) `"White"`; `"Grey"` is left unused (a value of `"Fill"`, drawn
  nowhere by default). The blobs without a name follow the same rule: a factor of three or more labels that is not
  declared is a disc of radius 0.2 (`$blobRadius`, four hatch spacings across), hatched as the default of `DeclareBlob`,
  and the vertex functions Γ⁽ⁿ⁾ of the trees (`VertexFunction`, full vertex functions) cross-hatched (`unnamedBlobSpec`,
  drawn by `declaredBlobPrimitives` and `tikzDeclaredBlob` without a label or a clearing; a line to such a disc ends at its
  centre, its arrow in the middle of the part outside it, `visiblePart`, as on a line from a declared blob, which starts at
  its outline). A blob is as
  large as its label needs (`declaredBlobSize`, from `blobLabelExtent`, an estimate of the LaTeX box, with the label font
  taken as `$blobLabelEm` = 0.6 units): the label, in a white clearing on a hatched, cross-hatched or dotted blob, lies
  inside the blob with a band of the fill (0.1 units) all around it, at the centre or on the axis of a triangle near its incentre
  (`blobLabelPlacement`); a blob grows (0.6 up), its label never shrinks, except in a triangle, where it is written 0.8
  times smaller (`$triangleLabelScale`, a scaled TikZ node), so that Λ and Λ̃ keep about the size 0.6; in one picture, and in
  all the terms of a sum drawn together (`$sumBlobHeads`, set by `geometriesOf`), the blobs of one shape have one size
  (`pictureBlobSize`). The lines of a box (four ports) and of a triangle always leave from its corners, never from a
  point along a side. Two blobs joined by two lines have the lines parallel: a box joined by two lines to two corners
  of a triangle (Γ⁰ L₀ Λ, Γ_xc L₀ Λ̃) is as wide as the side between them, in that picture and in the whole sum drawn
  with it (`boxFloorOf`, `$pictureBoxFloor`, `$sumBoxFloor`); a circle with two ports joined to one blob has them
  facing it, on either side of the line of the centres, at that blob's spacing if the circle is wide enough (parallel
  lines), else 55 degrees off the line (`$circlePortAngle`), the lines spreading slightly to the corners of a box (the
  Thouless criterion, Δ joined to Γ_pp; in `blobLayoutWithExternals`). A box joined by single lines from three corners to
  one vertex, its fourth corner elsewhere (Γ⁽⁴⁾ in the Schwinger–Dyson equation of φ⁴), is turned by 45 degrees
  (`turnedBoxJoin`): a diamond as large as the square (larger when its upright label needs it), its ports in the same
  order round it, the fourth corner on the right (on the left when its label comes first), the vertex on the horizontal
  diagonal 0.8 outside the opposite corner (`$turnedBoxPointDistance`), the lines to the top and bottom corners arcs of
  the circle through the three points, mirror images (bends passed from the layout to `frameItems`). A point twice a port of one blob (χ = γLγ =
  L(z₁, z₁; z₂, z₂), an interaction line ending on L) stands outside the two corners of a side, joined to each by a line
  (`blobPortCorner[blob, point, k]`), as the end of two propagators at the corners of F (x◁F▷y): a density point if it
  is an external label and nothing else, a vertex (a dot) otherwise; two legs at a point of the blob layout are fanned
  90 degrees apart, counterclockwise from the top left. A line from a port of a blob that leaves it outwards, 30 degrees
  or more from its outline, is bent only to avoid crossing it (`leavesBlobClearQ` in `blobAvoidingKind`).
  Dressed lines follow the textbooks: in a skeleton expansion (Φ[G], Φ[G, W], Γ₂[G, D_σ] and their derivatives) every
  line is a dressed one and is drawn in the plain style of its kind, and the text says so; a double line marks the dressed
  propagator only where the bare one appears in the same picture or equation (a Dyson equation, Hedin's W = V + VPW next
  to V). A dressed interaction (W) declared `"DoubleWavy"` for those equations is drawn as an ordinary wavy line in a
  skeleton expansion with the per-picture option `"Interactions" -> {W}` (`skeletonW` in `make_figures.wl`).
  `"DoubleWavy"` is one flatter wavy line drawn
  `double, double distance=1.2\pgflinewidth`, so that its two strokes stay apart along the wave and with heavier lines. A source at the end of a single line (Z[J] at a non-zero source) is drawn at the point.
- A mean (source insertion on a stub) and a tadpole go to the largest free sector at their vertex that leads outside
  the loops and bubbles drawn (`outsideGapDirection`): a mean on a vertex of a bubble on the axis sticks out of it, a
  tadpole on a side of a triangle hangs outside it. With four means, the corners of the two rails between them are
  chosen so that the lines cross least (`meanUnitLayout`). A one-point function whose vertex is joined to one other
  vertex by a bundle of lines (a melon) has the bundle along its leg, on the axis.
- A ring through blobs (Tr ln(1 - G0 Sigma)): the blobs evenly around a circle wide enough for them, symmetric about the
  vertical axis (one at the bottom, two left and right, three with one at the bottom), the other vertices between them
  (`layoutVacuumCycle`). A fermion loop through a stretch of the axis (a loop closed on a density point) is a circle on
  the stretch as a diameter (`backboneSegmentOfLoop` in `roundFermionLoops`).
- Several means on a vertex with lines (the mass insertion λφ̄²/2) are spread evenly in its free sector closest to the
  outward direction (`outwardGap`): a V outside a loop, never one inside; a vacuum cycle through such an insertion is
  rotated so that the picture is symmetric (`symmetricCycleRotation`).
- Symmetric diagrams must be drawn symmetric: among layouts of equal quality the symmetric one wins.
- Colours of the Graphics (`DiagramPlot`, `DrawDiagram`): only black, white and greys, each set explicitly through the
  light/dark pairs of `05_drawing.wl` (`LightDarkSwitched` from 14.3, the light colours before), so that a picture reads
  on light and dark notebooks; fills that hide lines take the notebook's background. `"Appearance" -> "Light" | "Dark"`
  forces one look, on its own background. The TikZ code is for white pages and ignores it.
- To look at many pictures quickly, draw them into a LaTeX file and look at the PDF; never judge a layout from
  coordinates alone.

## Versions

Semantic versioning, MAJOR.MINOR.PATCH: the largest change since the last release decides the next number.

- MAJOR: code written for the previous version can stop working or give a different result without notice: a public
  name removed or renamed, the arguments or the default of an option changed, a convention of the expressions changed
  (signs, normalisations, order of arguments).
- MINOR: something new that existing code does not see: a public function, an option or a value of an option, an exact
  equation, a statistics, a kind of picture.
- PATCH: the package does better what its documents already say: a wrong result or a picture corrected, speed, messages.

Changes to the documents alone (README, showcase, manual text, wiki, video, logo) need no release: GitHub Pages serves main. A
tag and its paclet never move: a mistake in a release is corrected by the next patch release. A release: the version in
`00_header.wl`, `PacletInfo.wl`, `CITATION.cff` (with `date-released`), the check of `Tests/RunTests.wl`, the README
(install line), the manual (title, install line, latest release, the value of
`$FunctionalDiagramsVersion`, how to cite), the showcase (title, values), `Examples.wl`, `index.html`, the wiki (`Home.md`,
`Installation.md`, `_Footer.md`) and the map of the functions
(`git grep` the old number: only `CHANGELOG.md` keeps it; `check_consistency.py` reports any other x.y.z in the README,
`index.html`, the manual, the showcase, `Examples.wl` and the wiki, and the x.y of the showcase's title); `## Unreleased` of `CHANGELOG.md` becomes `## X.Y.Z (date)`;
everything rebuilt (the site too: `python3 Development/make_site.py`, which takes the version, the date, the install
line and the number of tests from their sources) and `check_consistency.py` passing, a commit
"FunctionalDiagrams X.Y.Z"; the paclet built from a clean checkout (`Development/build_paclet.wl`); the GitHub release
vX.Y.Z with the paclet and the two PDFs.

## Compatibility

The package needs Mathematica 12.1 or later (`"WolframVersion" -> "12.1+"` in `PacletInfo.wl`): a function introduced
in a later version needs that requirement raised, in `PacletInfo.wl`, the README and the manual, unless it is used only
when it exists, with a fallback (as `LightDarkSwitched`, from 14.3, for the pictures and the load banner).

## Rules

- Keep commits small and their messages clear.
- In the documents every command shown gives as output exactly what is shown right after it: a sum of diagrams as
  `DiagramPlot` draws the whole output (its orders in bands; the sizing options `"Scale"`, `"LineWidth"`, `"LabelFont"`
  are the only ones left out, as the introductions of the showcase and of the manual say), so that every other option (`"ExternalOrder"`,
  `"LegDirections"`, `"Interactions"`, `"Couplings"`, ...), declaration, channel split, grouping or selection is in the
  code shown or, for a selection of orders, said in the text. A rearrangement of an output comes after the plain output,
  with its own code. Pictures that follow text and no command (exact equations with blobs, left-hand sides) are free.
  The pages of the wiki follow the same rule, and change with the documents.
