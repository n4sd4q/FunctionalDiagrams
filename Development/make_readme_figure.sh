#!/bin/bash
# Draw the pictures of README.md from the figures of the showcase (run Showcase/make_figures.wl first): the panels
# Documentation/readme_1.svg, readme_2.svg, ... (readme_gallery.tex) and the social preview of the repository,
# Logo/social_preview.png (social_preview.tex; it needs the logo, python3 make_logo.py ../Logo). Needs pdflatex, perl
# and pdftocairo (poppler). Run from any folder.
set -e
cd "$(dirname "$0")"
build=$(mktemp -d)
trap 'rm -rf "$build"' EXIT
# The figures used, copied with one unit of length for all of them (the showcase draws them at 0.34 to 0.55 cm), the
# wavy lines and the lines scaled with it, and labels in \footnotesize: diagrams about twice the height of the text.
mkdir "$build/figures"
for name in $(grep -o '\\fig{[^}]*}' readme_gallery.tex | sed 's/\\fig{\(.*\)}/\1/' | sort -u); do
  perl -pe 'BEGIN { $unit = 0.58; $lines = 1.4; $f = 1 }
    if (/x=([0-9.]+)cm, y=([0-9.]+)cm/) { $f = $unit / $1; s/x=[0-9.]+cm, y=[0-9.]+cm/sprintf("x=%.3fcm, y=%.3fcm", $unit, $unit)/e }
    s/(amplitude|segment length)=([0-9.]+)mm/sprintf("%s=%.3fmm", $1, $2 * $f)/ge;
    s/line width=([0-9.]+)pt/sprintf("line width=%.3fpt", $1 * $lines)/ge;
    s/font=\\(tiny|scriptsize)\b/font=\\footnotesize/g' "../Showcase/figures/$name.tex" > "$build/figures/$name.tex"
done
pdflatex -interaction=nonstopmode -halt-on-error -output-directory "$build" -jobname readme_gallery \
  "\def\figdir{$build/figures}\input{readme_gallery.tex}" > /dev/null ||
  { tail -20 "$build/readme_gallery.log"; exit 1; }
# each panel as SVG, on a white card with rounded corners and a grey edge: legible on light and dark pages alike
pages=$(pdfinfo "$build/readme_gallery.pdf" | awk '/^Pages:/ {print $2}')
for n in $(seq 1 "$pages"); do
  svg="../Documentation/readme_$n.svg"
  pdftocairo -svg -f "$n" -l "$n" "$build/readme_gallery.pdf" "$svg"
  perl -0pi -e 's{(<svg [^>]*viewBox="0 0 ([0-9.]+) ([0-9.]+)"[^>]*>)}{sprintf(qq{%s\n<rect x="0.5" y="0.5" width="%.3f" height="%.3f" rx="5" fill="#ffffff" stroke="#d0d7de" stroke-width="0.75"/>}, $1, $2 - 1, $3 - 1)}e' "$svg"
  touch "$svg"
done
echo "Documentation/readme_1.svg ... readme_$pages.svg written"
pdflatex -interaction=nonstopmode -halt-on-error -output-directory "$build" -jobname social_preview \
  "\def\gallery{$build/readme_gallery.pdf}\input{social_preview.tex}" > /dev/null ||
  { tail -20 "$build/social_preview.log"; exit 1; }
pdftocairo -png -r 144 -singlefile "$build/social_preview.pdf" "$build/social_preview"
mv "$build/social_preview.png" ../Logo/social_preview.png
echo "Logo/social_preview.png written"
