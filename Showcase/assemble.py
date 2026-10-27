"""Turn the output of make_figures.wl (folder figures/) into the LaTeX snippets that FunctionalDiagrams_Showcase.tex
includes. Run from the Showcase folder: python3 assemble.py (build_pdf.sh does it).

* figures/NAME.terms (one term per block: the weight, a line '%%PICTURE%%', the tikzpicture) -> figures/NAME.tex,
  the sum of the diagrams as a paragraph that LaTeX breaks into lines by itself. NAME_m.terms holds the terms of order
  m of a sum written order by order: its first term gets an explicit sign unless m is the lowest order of the sum;
  figures/NAME_m_alone.tex, without that sign, only for the orders that the pictures of the README show on their own
  (\\fig{NAME_m_alone} in Development/readme_*.tex).
* figures/line_styles.terms (the weight is the name of the style) -> figures/line_styles.tex, a table.
* figures/tikz_example.tex -> figures/tikz_example_first.tex, its first picture.
* figures/formula_NAME.tex (written by ExpressionToLaTeX) -> figures/formula_NAME_split.tex, with a line break, when it
  is long, and figures/formula_NAME_lines.tex, broken into lines of about 70 characters, when it is very long
  (Development/latex_lines.py; \\formulalines{NAME}).
* figures/values.txt (lines 'name :: value') -> figures/values.tex (\\setvalue{Key}{LaTeX}, the checks with a check mark
  when they vanish) and figures/parquet_table.tex.
"""
import glob, os, re, sys

os.chdir(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join('..', 'Development'))
from latex_lines import visible_length, split_formula, broken
channel_short = {'FullyIrreducible': r'\Gamma_{\rm irr}', 'ParticleHole': r'\mathrm{ph}', 'TransverseParticleHole': r'\overline{\mathrm{ph}}',
                 'ParticleParticle': r'\mathrm{pp}', 'SeveralChannels': r'\text{several}'}


# ---------------------------------------------------------------- sums of diagrams
def terms_of(path):
    text = open(path).read().strip()
    if text == '':
        return []
    out = []
    for block in text.split('\n%%TERM%%\n'):
        w, pic = block.split('\n%%PICTURE%%', 1)
        out.append((w.strip(), pic.strip()))
    return out


def term_latex(w, pic, first):
    negative = w.startswith('-')
    magnitude = w[1:] if negative else w
    coefficient = magnitude if (pic == '' or magnitude != '1') else ''
    sign = '-' if negative else ('' if first else '+')
    math = (sign + r'\,' if sign else '') + coefficient
    return r'\mbox{$\displaystyle %s$%s}' % (math, (r'\,' + pic) if pic else '')


def sum_latex(terms, first=True):
    return '\n\\diagramgap\n'.join(term_latex(w, pic, first and i == 0) for i, (w, pic) in enumerate(terms))


names = sorted(os.path.basename(p)[:-6] for p in glob.glob('figures/*.terms'))
ordered = {}      # base name -> {order: terms}
for name in names:
    terms = terms_of('figures/%s.terms' % name)
    m = re.match(r'(.*)_(\d+)$', name)
    if m:
        ordered.setdefault(m.group(1), {})[int(m.group(2))] = terms
        continue
    if name == 'line_styles':
        cells = [r'%s\enspace\texttt{"%s"}' % (pic, w) for w, pic in terms]
        rows = [' & '.join(cells[i:i + 3] + [''] * (3 - len(cells[i:i + 3]))) for i in range(0, len(cells), 3)]
        open('figures/line_styles.tex', 'w').write('\\begin{tabular}{@{}lll@{}}\n' + ' \\\\[8pt]\n'.join(rows) + '\n\\end{tabular}\n')
    else:
        open('figures/%s.tex' % name, 'w').write(sum_latex(terms) + '\n')
    print('figures/%s: %d terms' % (name, len(terms)))

# the orders that the pictures of the README (Development/readme_*.tex) show on their own, \fig{NAME_m_alone}
alone = set()
for path in glob.glob(os.path.join('..', 'Development', 'readme_*.tex')):
    alone.update(re.findall(r'\\fig\{(\w+)_alone\}', open(path).read()))
# a sum is written order by order when the showcase draws it with \orderrow; the other NAME_m are sums of their own
order_rows = set(re.findall(r'\\orderrow\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}\{(\w+)_\d+\}', open('FunctionalDiagrams_Showcase.tex').read()))
for base, rows in ordered.items():
    lowest = min(rows)
    for m, terms in rows.items():
        open('figures/%s_%d.tex' % (base, m), 'w').write(sum_latex(terms, m == lowest or base not in order_rows) + '\n')
        if '%s_%d' % (base, m) in alone:
            # the same order on its own (no sign before a positive first term), for a formula of its own
            open('figures/%s_%d_alone.tex' % (base, m), 'w').write(sum_latex(terms) + '\n')
        print('figures/%s_%d: %d terms' % (base, m, len(terms)))

# the first picture of the TikZ example, for the listing
if os.path.exists('figures/tikz_example.tex'):
    text = open('figures/tikz_example.tex').read()
    end = text.find(r'\end{tikzpicture}')
    if end >= 0:
        open('figures/tikz_example_first.tex', 'w').write(text[:end + len(r'\end{tikzpicture}')] + '\n')


# ---------------------------------------------------------------- formulas
# formulas written by ExpressionToLaTeX: the long ones also with a line break (figures/formula_NAME_split.tex)
for path in glob.glob('figures/formula_*.tex'):
    if path.endswith('_split.tex') or path.endswith('_lines.tex'):
        continue
    tex = open(path).read().strip()
    split = path[:-4] + '_split.tex'
    if visible_length(tex) > 70:
        open(split, 'w').write(split_formula(tex) + '\n')
        print('%s: split' % path)
    elif os.path.exists(split):
        # a split left by an earlier run, when the formula was longer
        os.remove(split)
        print('%s: stale split removed' % split)
    lines = path[:-4] + '_lines.tex'
    b = broken(tex, 70) if visible_length(tex) > 140 else None
    if b:
        open(lines, 'w').write(b + '\n')
        print('%s: broken into lines' % path)
    elif os.path.exists(lines):
        os.remove(lines)
        print('%s: stale lines removed' % lines)


# ---------------------------------------------------------------- values
def parse(text):
    """InputForm -> Python: numbers, strings, lists, associations (dicts)."""
    text = text.strip().replace('⇾', '->').replace('→', '->')
    pos = 0

    def skip():
        nonlocal pos
        while pos < len(text) and text[pos] in ' \n\t':
            pos += 1

    def value():
        nonlocal pos
        skip()
        if text.startswith('<|', pos):
            pos += 2
            d = {}
            skip()
            while not text.startswith('|>', pos):
                k = value(); skip()
                assert text.startswith('->', pos), text[pos:]
                pos += 2
                d[k] = value(); skip()
                if text[pos] == ',':
                    pos += 1
                skip()
            pos += 2
            return d
        if text[pos] == '{':
            pos += 1
            items = []
            skip()
            while text[pos] != '}':
                items.append(value()); skip()
                if text[pos] == ',':
                    pos += 1
                skip()
            pos += 1
            return items
        if text[pos] == '"':
            # a string of InputForm, whose backslashes are written twice
            j = text.index('"', pos + 1)
            s = re.sub(r'\\(.)', r'\1', text[pos + 1:j])
            pos = j + 1
            return s
        m = re.match(r'-?\d+(\.\d*)?(`[\d.]*)?(\*\^-?\d+)?', text[pos:])
        if m and (pos + m.end() == len(text) or text[pos + m.end()] in ',}| \n'):
            pos += m.end()
            s = m.group(0).split('`')[0]
            return float(s) if '.' in s else int(s)
        # anything else (a symbolic residual): up to the next top-level separator
        depth, j = 0, pos
        while j < len(text):
            c = text[j]
            if c in '[({':
                depth += 1
            elif c in '])}':
                if depth == 0:
                    break
                depth -= 1
            elif c == ',' and depth == 0:
                break
            j += 1
        s = text[pos:j].strip()
        pos = j
        return ('expression', s)

    return value()


def is_zero(v):
    if isinstance(v, dict):
        return all(is_zero(x) for x in v.values())
    if isinstance(v, list):
        return all(is_zero(x) for x in v)
    return v == 0


def latex_of(v):
    if isinstance(v, dict):
        return ', '.join('%s: %s' % (k, latex_of(x)) for k, x in v.items())
    if isinstance(v, list):
        return r'\{' + r',\,'.join(latex_of(x) for x in v) + r'\}'
    if isinstance(v, tuple):
        return r'\text{\color{red}(not zero)}'
    if isinstance(v, float):
        return ('%.1f' % v).rstrip('0').rstrip('.')
    return str(v)


def key_of(name):
    # 'check Gamma0 = F - R_ph' -> 'CheckGamma0FRPh'
    return ''.join(w[:1].upper() + w[1:] for w in re.split(r'[^A-Za-z0-9]+', name))


values = {}
if os.path.exists('figures/values.txt'):
    lines = []
    for line in open('figures/values.txt'):
        if ' :: ' not in line:
            continue
        name, text = line.rstrip('\n').split(' :: ', 1)
        v = values[name] = parse(text)
        key = key_of(name)
        if name == 'version':
            lines.append(r'\setvalue{%s}{%s}' % (key, r'FunctionalDiagrams %s, Mathematica %s' % (v[0], v[1].split(' ')[0])))
        elif name.startswith('check') or name.startswith('parquet residuals'):
            mark = r'\ok' if is_zero(v) else r'\textcolor{red}{$\times$}'
            if isinstance(v, dict):
                shown = ', '.join(r'$%s\colon %s$' % (channel_short.get(k, k), latex_of(x)) for k, x in v.items())
            else:
                shown = '$%s$' % latex_of(v)
            lines.append(r'\setvalue{%s}{%s\ %s}' % (key, shown, mark))
            # the entries of a list of checks, each with its own mark (KeyN, N from 1), for result boxes with one row per check
            if isinstance(v, list):
                for i, x in enumerate(v):
                    lines.append(r'\setvalue{%s%d}{$%s$\ %s}' % (key, i + 1, latex_of(x), r'\ok' if is_zero(x) else r'\textcolor{red}{$\times$}'))
        elif isinstance(v, dict):
            for k, x in v.items():
                lines.append(r'\setvalue{%s%s}{%s}' % (key, key_of(k), latex_of(x)))
                if isinstance(x, list):
                    for i, y in enumerate(x):
                        lines.append(r'\setvalue{%s%s%d}{%s}' % (key, key_of(k), i + 1, latex_of(y)))
            lines.append(r'\setvalue{%s}{$%s$}' % (key, r',\ '.join(r'%s\colon %s' % (channel_short.get(k, k), latex_of(x)) for k, x in v.items())))
        elif isinstance(v, list):
            # numbers of diagrams order by order (from order 0 for G; trees by number of points, the fermionic ones
            # for 4 and 6 points), and their total
            start, step = {'counts G': (0, 1), 'counts trees': (3, 1), 'counts fermion trees': (4, 2), 'counts electron numerator': (0, 1),
                           'counts phi2 correlation': (0, 1), 'counts phi4 R_s': (2, 1),
                           'counts Lambda': (0, 1), 'counts proper vertex': (0, 1), 'counts phi4 Lambda': (0, 1)}.get(name, (1, 1))
            lines.append(r'\setvalue{%s}{%s}' % (key, ', '.join(latex_of(x) for x in v)))
            for i, x in enumerate(v):
                lines.append(r'\setvalue{%s%d}{%s}' % (key, start + step * i, latex_of(x)))
            if all(isinstance(x, int) for x in v):
                lines.append(r'\setvalue{%sTotal}{%d}' % (key, sum(v)))
        elif (isinstance(v, (int, float)) and v < 0) or (isinstance(v, str) and re.fullmatch(r'-[0-9.]+', v)):
            # a negative number in math mode: a minus sign, not a hyphen
            lines.append(r'\setvalue{%s}{$%s$}' % (key, latex_of(v)))
        else:
            lines.append(r'\setvalue{%s}{%s}' % (key, latex_of(v)))
    open('figures/values.tex', 'w').write('\n'.join(lines) + '\n')
    print('figures/values.tex: %d values' % len(lines))

# ---------------------------------------------------------------- the parquet table
if 'parquet counts' in values:
    counts, residuals, full = values['parquet counts'], values['parquet residuals'], values['counts F']
    rows = []
    for k in range(len(full)):
        checks = ' '.join(r'\ok' if residuals[c][k] == 0 else r'\textcolor{red}{$\times$}'
                          for c in ['ParticleHole', 'TransverseParticleHole', 'ParticleParticle'])
        rows.append('$%s$ & %s & %s & %s & %s & %s & %s & %s \\\\' % (
            'V' if k == 0 else 'V^{%d}' % (k + 1), full[k], counts['FullyIrreducible'][k], counts['ParticleHole'][k],
            counts['TransverseParticleHole'][k], counts['ParticleParticle'][k], counts['SeveralChannels'][k], checks))
    table = (r'''\begin{center}
\small
\begin{tabular}{@{}c@{\hspace{1.4em}}c@{\hspace{1.4em}}ccccc@{\hspace{1.4em}}c@{}}
\toprule
order & diagrams of $F$ & $\Gamma_{\rm irr}$ & $R_{\ph}$ & $R_{\phbar}$ & $R_{\pp}$ & several channels & Bethe--Salpeter $\ph$, $\phbar$, $\pp$\\
\midrule
''' + '\n'.join(rows) + r'''
\bottomrule
\end{tabular}
\end{center}
''')
    open('figures/parquet_table.tex', 'w').write(table)
    print('figures/parquet_table.tex: %d orders' % len(full))
