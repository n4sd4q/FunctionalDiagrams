#!/bin/bash
# Typeset the showcase: assemble.py turns the output of make_figures.wl (folder figures/) into LaTeX, then pdflatex
# runs until the cross-references are settled: latexmk runs it again only when the .aux or .toc files have changed
# (once when no reference moved, not at all when nothing changed); without latexmk, twice.
# Run from anywhere: ./build_pdf.sh
set -e
cd "$(dirname "$0")"
python3 assemble.py > /dev/null
if command -v latexmk > /dev/null; then
  latexmk -pdf -interaction=nonstopmode -halt-on-error FunctionalDiagrams_Showcase.tex > /dev/null 2>&1
else
  pdflatex -interaction=nonstopmode -halt-on-error FunctionalDiagrams_Showcase.tex > /dev/null
  pdflatex -interaction=nonstopmode -halt-on-error FunctionalDiagrams_Showcase.tex > /dev/null
fi
grep -E "Overfull|undefined|Missing|Error" FunctionalDiagrams_Showcase.log || true
