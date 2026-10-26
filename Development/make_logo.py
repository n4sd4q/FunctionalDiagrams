"""Write the logo of FunctionalDiagrams: the exchange (Fock) vacuum diagram -- a closed fermion loop with an
interaction line across it -- the first diagram of the Luttinger-Ward functional, white and light blue on a blue
badge. The badge has a faint light edge, so that one file reads on light and on dark pages (the README uses it alone).
Output, in the folder given as argument (default Logo/):
  FunctionalDiagrams_logo.svg / .pdf         the badge
  FunctionalDiagrams_logo_white.svg / .pdf   the diagram alone, for dark backgrounds (title pages of the manual and
                                             of the showcase)
  FunctionalDiagrams_logo_small.svg          the badge drawn for 16-32 px (favicon): no arrows or dots, heavier lines
usage: python3 make_logo.py [OUTPUT_FOLDER]   (the PDF files need cairosvg or rsvg-convert)
"""
import math, os, shutil, subprocess, sys

out = sys.argv[1] if len(sys.argv) > 1 else 'Logo'
os.makedirs(out, exist_ok=True)
accent, light = '#1F4E79', '#9FC5E8'
C = 64.0  # centre of the 128 x 128 picture


def wavy(a, b, amplitude, halfwaves, n=240):
    """A wavy line from a to b: a sine of the given number of half waves across the straight line, its amplitude
    tapered at the ends, so that the line meets the vertices head on."""
    (ax, ay), (bx, by) = a, b
    length = math.hypot(bx - ax, by - ay)
    nx, ny = -(by - ay) / length, (bx - ax) / length
    points = []
    for t in (i / n for i in range(n + 1)):
        s = amplitude * min(1.0, 6 * t, 6 * (1 - t)) * math.sin(math.pi * halfwaves * t)
        points.append((ax + (bx - ax) * t + nx * s, ay + (by - ay) * t + ny * s))
    return 'M ' + ' L '.join('%.2f %.2f' % p for p in points)


def arrowhead(r, angle, size):
    """An isosceles arrowhead centred on the loop of radius r at the given angle (degrees; y points down), pointing
    counterclockwise on the screen."""
    a = math.radians(angle)
    px, py = C + r * math.cos(a), C + r * math.sin(a)
    tx, ty = math.sin(a), -math.cos(a)
    nx, ny = -ty, tx
    tip = (px + tx * size * 0.6, py + ty * size * 0.6)
    left = (px - tx * size * 0.6 + nx * size * 0.62, py - ty * size * 0.6 + ny * size * 0.62)
    right = (px - tx * size * 0.6 - nx * size * 0.62, py - ty * size * 0.6 - ny * size * 0.62)
    return 'M %.2f %.2f L %.2f %.2f L %.2f %.2f Z' % (tip + left + right)


def logo(badge, r=38, loop_width=8, wave_width=7, amplitude=7.5, halfwaves=6, dots=7.5, arrows=11):
    parts = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 128 128" width="128" height="128">',
             '<title>FunctionalDiagrams</title>']
    if badge:
        parts += ['<rect width="128" height="128" rx="28" fill="%s"/>' % accent,
                  '<rect x="1" y="1" width="126" height="126" rx="27" fill="none" stroke="#ffffff" stroke-opacity="0.16" '
                  'stroke-width="2"/>']
    parts += ['<path d="%s" fill="none" stroke="%s" stroke-width="%g" stroke-linecap="round" stroke-linejoin="round"/>'
              % (wavy((C - r, C), (C + r, C), amplitude, halfwaves), light, wave_width),
              '<circle cx="%g" cy="%g" r="%g" fill="none" stroke="white" stroke-width="%g"/>' % (C, C, r, loop_width)]
    if arrows:
        parts += ['<path d="%s" fill="white"/>' % arrowhead(r, angle, arrows) for angle in (-90, 90)]
    if dots:
        parts += ['<circle cx="%g" cy="%g" r="%g" fill="white"/>' % (x, C, dots) for x in (C - r, C + r)]
    parts.append('</svg>')
    return '\n'.join(parts) + '\n'


def write_pdf(svg, pdf):
    try:
        import cairosvg
        cairosvg.svg2pdf(url=svg, write_to=pdf)
    except ImportError:
        if not shutil.which('rsvg-convert'):
            print('neither cairosvg nor rsvg-convert found: no PDF')
            return
        # a fixed creation date, so that running the script again leaves the PDF unchanged
        subprocess.run(['rsvg-convert', '-f', 'pdf', '-o', pdf, svg], check=True,
                       env=dict(os.environ, SOURCE_DATE_EPOCH='1767225600'))
    print('wrote', pdf)


files = [('FunctionalDiagrams_logo', logo(True), True),
         ('FunctionalDiagrams_logo_white', logo(False), True),
         ('FunctionalDiagrams_logo_small', logo(True, r=42, loop_width=12, wave_width=10, amplitude=9, halfwaves=4,
                                                dots=0, arrows=0), False)]
for name, text, pdf in files:
    path = os.path.join(out, name + '.svg')
    open(path, 'w').write(text)
    print('wrote', path)
    if pdf:
        write_pdf(path, os.path.join(out, name + '.pdf'))
