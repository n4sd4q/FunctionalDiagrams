# Development

The package `../FunctionalDiagrams.wl` is assembled from the sources in `src/`, one file per part:

| file | |
|---|---|
| `00_header.wl` | package header, usage messages |
| `01_notation_declarations.wl` | notation (`IntegralOver`, `DeltaFunction`, Grassmann products), declarations |
| `02_terms_derivative_canonical.wl` | terms, functional derivatives, canonical form of diagrams |
| `03_wick_perturbation.wl` | Wick's theorem, perturbation theory |
| `04_classes_vertices.wl` | classes of diagrams, Luttinger–Ward functional, vertices, parquet decomposition |
| `05_drawing.wl` | layout and pictures (the general layout, the unit layout of fermion lines and loops, trees of vertex functions) |
| `06_tikz_footer.wl` | TikZ export, formulas in LaTeX |
| `07_matsubara.wl` | evaluation in momentum and Matsubara frequency: frequency sums, analytic continuation, cutting rules |
| `09_dimreg.wl` | Feynman integrals in dimensional regularization: loop integrals, Laurent expansion in ε, renormalization-group functions |
| `10_qed.wl` | QED at one loop: Dirac algebra in d dimensions, `QEDIntegrand`, Feynman-parameter integrals, form factors |
| `11_effective_actions.wl` | effective actions as objects: operator traces, the 1PI and 2PI actions, Potthoff's functionals, superfields, zero-dimensional values |
| `08_footer.wl` | `$publicNames`, load-time check of the public names, end of the package |

Edit the sources, then run `./build.sh`, which writes `../FunctionalDiagrams.wl`: it concatenates 00 to 07, then 09, 10
and 11, and 08 last.

The guided tour is written as a script, `../Examples.wl`; the notebook is generated from it:

```
python3 make_notebook.py ../Examples.wl ../Examples.nb
```

The logo (`../Logo/`) is drawn by `python3 make_logo.py ../Logo` (it needs `cairosvg` or `rsvg-convert` for the PDF files).

The pictures of `../README.md`, `../Documentation/readme_1.svg`, `readme_2.svg`, ... (`readme_gallery.tex`, one panel per
page, all of the same width, on white cards so that they read on light and dark pages), and the social preview of the
repository, `../Logo/social_preview.png` (`social_preview.tex`), are drawn from the figures of the showcase by
`./make_readme_figure.sh` (after `Showcase/make_figures.wl` and `make_logo.py`; it needs pdflatex, perl and pdftocairo).
The README uses plain `<img>` elements, one file per picture: the GitHub mobile apps do not show `<picture>`.

The demo video, `../Media/demo.mp4`, its looping preview at the top of the README, `../Media/demo_preview.gif`, and a
poster frame, `../Media/demo_poster.png`, are made by `python3 make_demo_video.py` (after `Showcase/make_figures.wl`;
it needs pdflatex, pdftocairo, PIL and ffmpeg): the code typed is the first example of the README, the diagrams and
formulas are the figures of the showcase, typeset one element per page and put together frame by frame.

The map of the functions in the README is drawn in two steps: `wolfram -script make_function_map.wl` reads the group,
kind, usage message and dependencies of every public name into `../Documentation/function_map.json`, and
`python3 make_function_map.py` (numpy, matplotlib) draws `function_map.svg` (the names by their role in a calculation,
in six stages around `FunctionalDerivative`) and the interactive `function_map.html` (the same layout, light and dark,
with every dependency). `latex_lines.py` breaks long formulas into lines, for the manual (`make_index.py`) and the
showcase (`assemble.py`).

`python3 check_consistency.py` checks, without Mathematica, that `../FunctionalDiagrams.wl` is the concatenation of the
sources, that `../Examples.nb` is generated from `../Examples.wl`, that every public function has a usage message
and an entry in the manual, that the version is the same everywhere, that the badge of the README matches the last run
of the tests, and that the map of the functions is drawn from the current public names and version. GitHub runs it on every push (`../.github/workflows/consistency.yml`).

`PacletInfo.wl` (at the root) makes the repository a paclet; `wolfram -script build_paclet.wl` builds
`../build/FunctionalDiagrams-<version>.paclet` (the package, the showcase, the manual, the guided tour, the license
with its `NOTICE`, the changelog), the
file attached to a GitHub release. The version is the same in `PacletInfo.wl`, `CITATION.cff` and `$FunctionalDiagramsVersion`
(`check_consistency.py` checks it).
