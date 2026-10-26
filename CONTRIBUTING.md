# Contributing to FunctionalDiagrams

Reports of problems, questions and suggestions are welcome; so are corrections and new examples.

## Reporting a problem

Open an [issue](https://github.com/n4sd4q/FunctionalDiagrams/issues) with:

- the versions: `$FunctionalDiagramsVersion` and `$Version`;
- a minimal input that shows the problem, starting from ``Needs["FunctionalDiagrams`"]`` (or `Get`), with the declarations
  it needs;
- what you get (the output or the message) and what you expected. For a picture, a screenshot helps; for a wrong sign or
  weight, say which convention you compared with (the conventions are in the README, under *Details*, and in the manual).

## Working on the package

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
wolfram -script Tests/RunTests.wl > Tests/RunTests.out   # every check must print PASS
python3 Development/check_consistency.py                 # generated files, usage messages, manual, version
```

(`wolframscript -file Tests/RunTests.wl` does the same as `wolfram -script`.) GitHub runs `check_consistency.py` on every
push; it cannot run the tests, which need Mathematica.

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
by whom and when (terms (b) and (c) in `NOTICE`).
