"""Prepares the typesetting of the reference manual:
* index_rows.tex, the index of functions: three columns of the functions described in FunctionalDiagrams_Manual.tex (its
  \\begin{function}{Name} entries) with their pages;
* examples/NAME_lines.tex: the formulas of examples/NAME.tex that are too long for one line, broken before top-level
  + and - signs (Development/latex_lines.py); a NAME_lines.tex whose formula is no longer too long is removed."""
import os, re, glob, sys
os.chdir(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join('..', 'Development'))
from latex_lines import visible_length, broken

# the entries, outside comments
source = '\n'.join(line.split('%')[0] if not line.lstrip().startswith('%') else '' for line in open('FunctionalDiagrams_Manual.tex'))
names = sorted(set(re.findall(r'\\begin\{function\}\{([A-Za-z][A-Za-z0-9]*)\}', source)), key=str.lower)
# three columns, read from top to bottom
height = (len(names) + 2) // 3
rows = []
for i in range(height):
    cells = [(r'\fnref{%s} & \pageref{fn:%s}' % (names[i + k * height], names[i + k * height])) if i + k * height < len(names) else '&'
             for k in range(3)]
    rows.append(' & '.join(cells) + r'\\')
open('index_rows.tex', 'w').write('\n'.join(rows) + '\n')
print('%d functions in the index' % len(names))


for path in glob.glob('examples/*.tex'):
    if path.endswith('_lines.tex') or path.endswith('_pictures.tex'):
        continue
    tex = open(path).read().strip()
    lines = path[:-4] + '_lines.tex'
    b = broken(tex, 64) if visible_length(tex) > 64 else None
    if b:
        open(lines, 'w').write(b + '\n')
        print('%s: broken into lines' % path)
    elif os.path.exists(lines):
        # left by an earlier run, when the formula was longer: the manual would show it instead of the formula
        os.remove(lines)
        print('%s: stale lines removed' % lines)
