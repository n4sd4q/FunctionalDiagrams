# Contributing to FunctionalDiagrams

Reports of problems, questions and suggestions are welcome; so are corrections and new examples.

## Reporting a problem

Open an [issue](https://github.com/n4sd4q/FunctionalDiagrams/issues) with:

- the versions: `$FunctionalDiagramsVersion` and `$Version`;
- a minimal input that shows the problem, starting from ``Needs["FunctionalDiagrams`"]`` (or `Get`), with the declarations
  it needs;
- what you get (the output or the message) and what you expected. For a picture, a screenshot helps; for a wrong sign or
  weight, say which convention you compared with (the conventions are in the wiki's
  [Conventions](https://github.com/n4sd4q/FunctionalDiagrams/wiki/Conventions) page and in the manual).

## Working on the package

| Path | Contents |
|---|---|
| `FunctionalDiagrams.wl` | the package, a single file |
| `Examples.wl`, `Examples.nb` | the guided tour (a script, and the same as a notebook) |
| `Showcase/` | the showcase: `make_figures.wl` computes and draws everything in it, `build_pdf.sh` typesets it |
| `Documentation/` | the reference manual (`make_examples.wl` computes its examples, `build_manual.sh` typesets it) and the map of the functions |
| `Tests/` | the test suite `RunTests.wl`, its last output `RunTests.out` and its helpers |
| `wiki/` | the pages of the wiki, published to it by `.github/workflows/wiki.yml` |
| `Media/`, `Logo/` | the demo video and its preview; the logo, the exchange (Fock) vacuum diagram |
| `Development/` | the sources of the package, one file per part, and the scripts that assemble it, build the paclet and draw the pictures of the README |
| `PacletInfo.wl` | the paclet: the repository can be loaded as it is |
| `index.html` | the [website](https://n4sd4q.github.io/FunctionalDiagrams/), written by `Development/make_site.py` |

The repository has a few generated files: edit their sources, never the files themselves.

| Generated file | Edit | Then run |
|---|---|---|
| `FunctionalDiagrams.wl` | `Development/src/*.wl` | `Development/build.sh` |
| `Examples.nb` | `Examples.wl` | `python3 Development/make_notebook.py Examples.wl Examples.nb` |
| `Documentation/FunctionalDiagrams_Manual.pdf` | `Documentation/FunctionalDiagrams_Manual.tex` | `cd Documentation && wolfram -script make_examples.wl && ./build_manual.sh` |
| `Showcase/FunctionalDiagrams_Showcase.pdf` | `Showcase/FunctionalDiagrams_Showcase.tex`, `Showcase/make_figures.wl` | `cd Showcase && wolfram -script make_figures.wl && ./build_pdf.sh` |

`Development/README.md` describes the sources and every script.

Before proposing a change:

```sh
wolfram -script Tests/RunTests.wl > Tests/RunTests.out   # about a minute; every check must print PASS
python3 Development/check_consistency.py                 # generated files, usage messages, manual, version
```

(`wolframscript -file` does the same as `wolfram -script`.) GitHub runs `check_consistency.py` on every push; it cannot
run the tests, which need Mathematica. The code shown in the README, the wiki and the showcase is run by
`Development/check_readme.wl`, `check_wiki.wl` and `check_showcase.wl`.

Conventions to keep:

- The package needs Mathematica 12.1 or later: a function introduced in a later version needs that requirement raised
  (`PacletInfo.wl`, the README and the manual).
- Every public function has a usage message (`Development/src/00_header.wl`) and an entry in the manual, kept in sync.
- New results are shown as functional derivatives first, before any shortcut function; φ⁴ theory and the electron gas
  are the running examples.
- Names of propagators and fields in examples and tests must not be built-in symbols (`Gamma`, `C`, `D`, `E`, `I`, `K`,
  `N`, `O`, ...).
- Code, comments and documentation in English.

## License of contributions

FunctionalDiagrams is released under the GNU General Public License, version 3 or later (the text of the license is in
`LICENSE`), with the additional terms in `NOTICE`. By contributing you agree that your contribution is released under the
same terms. Modified versions must keep `NOTICE` and the attribution to the author, and must say that they are modified,
by whom and when; see terms (b) and (c) of `NOTICE`.
