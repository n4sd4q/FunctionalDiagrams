#!/bin/bash
# Assemble the single-file package ../FunctionalDiagrams.wl from the sources in src/ (run from any folder).
set -e
cd "$(dirname "$0")"
cat src/00_header.wl src/01_notation_declarations.wl src/02_terms_derivative_canonical.wl src/03_wick_perturbation.wl \
    src/04_classes_vertices.wl src/05_drawing.wl src/06_tikz_footer.wl src/07_matsubara.wl src/09_dimreg.wl \
    src/10_qed.wl src/11_effective_actions.wl src/08_footer.wl \
    > ../FunctionalDiagrams.wl
wc -l ../FunctionalDiagrams.wl
