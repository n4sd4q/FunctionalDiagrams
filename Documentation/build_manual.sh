#!/bin/bash
# Typeset the reference manual FunctionalDiagrams_Manual.pdf. The examples (examples/) are computed by make_examples.wl.
# latexmk runs pdflatex again only when the .aux or .toc files have changed: three times from scratch, once when the
# sources changed but no reference moved, not at all when nothing changed; without latexmk, three times.
set -e
cd "$(dirname "$0")"
python3 make_index.py
if command -v latexmk > /dev/null; then
  latexmk -pdf -interaction=nonstopmode FunctionalDiagrams_Manual.tex > /dev/null 2>&1 || true
else
  for run in 1 2 3; do
    pdflatex -interaction=nonstopmode FunctionalDiagrams_Manual.tex > /dev/null || true
  done
fi
grep -E "Overfull|undefined|Missing|Error" FunctionalDiagrams_Manual.log || echo "no warnings"
