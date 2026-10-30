#!/usr/bin/env python3
"""Draws the map of the public functions of the package from Documentation/function_map.json (written by
make_function_map.wl): Documentation/function_map.svg (a white card with a hairline edge and rounded corners, legible on
light and dark pages alike: the picture of the README) and Documentation/function_map.html (interactive, one
self-contained file, light and dark, with a bar in the style of the site linking back to it: the landing page of GitHub
Pages, the PDFs, the wiki and the repository, whose addresses come from PacletInfo.wl through make_site.py).

The map shows the idea of the package: you write a functional and its functional derivatives are the diagrams. A name
is placed by its role in a calculation, read from top to bottom as six stages: 1 declare the symbols, 2 write a
functional, 3 differentiate it, then 4 classify and check, 5 draw and 6 evaluate the diagrams. FunctionalDerivative is
the hub of stage 3, and the names built on it hang from it. A name goes to the stage of its group (STAGE_OF_CATEGORY),
or to stage 3 when it reaches FunctionalDerivative; a few placements are editorial (STAGE_OVERRIDE): the trace
vocabulary and the Legendre heads are written in the functional (stage 2), as are the two auxiliary-field transforms,
which rewrite it; ZeroDimensionalValue counts diagrams (stage 4); the package's own function, its update check, stands
with the declarations (stage 1). The names of stage 3 that FunctionalDerivative uses
itself stand beside it, each with what it is used for (PARTNER_NOTE). Colour: the group (a block of the usage messages of
00_header.wl); filled chips are functions, dashed chips inert heads used as data.

Dependencies, in the static picture: the transitive reduction of the graph of the JSON. A name indented under another
is built on it (the used name with the longest chain below it); the other public names it uses are written under it or
beside it ("uses ..."), in the colour of their group. Left out, as the key says: the uses of the names of stages 1 and 2
(notation, declarations, traces) and of the inert heads, which many functions read. Nothing crosses. The script checks
that each name is drawn exactly once and accounts for every dependency (drawn, implied by the others, or left out). The
interactive page has all the dependencies: a selected name shows its "uses" and "used by" lines, bundled through the
groups and stages of their ends.

The layout is searched, deterministically (the same files byte for byte on a rerun): the split of the first row, the
assignment of the subtrees of the hub to three columns, the split of Classify and Draw, the widths of the columns of
Evaluate. Text widths come from the outlines of DejaVu Sans (through matplotlib), wider than the fonts GitHub uses, so
that no name overflows its chip.

Python 3 with numpy and matplotlib. Run from any folder:  python3 Development/make_function_map.py"""

import base64
import html
import itertools
import json
import math
import os
import re
import sys
from collections import Counter, defaultdict

import numpy as np
from matplotlib.font_manager import FontProperties
from matplotlib.textpath import TextPath

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DOCUMENTATION = os.path.join(ROOT, 'Documentation')
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from make_site import paclet_info, site_url  # noqa: E402  (the addresses of the repository and of the site)
FONT_STACK = "-apple-system, BlinkMacSystemFont, 'Segoe UI', 'Noto Sans', Helvetica, Arial, sans-serif"
MONO_STACK = "ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, 'Liberation Mono', monospace"

# ---------------------------------------------------------------------------------------------------------------- data
MAP = json.load(open(os.path.join(DOCUMENTATION, 'function_map.json'), encoding='utf-8'))
CATEGORIES = MAP['categories']
NODES = {n['name']: n for n in MAP['nodes']}
NAMES = [n['name'] for n in MAP['nodes']]
ORDER = {a: i for i, a in enumerate(NAMES)}
CAT = {a: NODES[a]['category'] for a in NAMES}
KIND = {a: NODES[a]['kind'] for a in NAMES}
USES = defaultdict(set)
for _a, _b in MAP['edges']:
    USES[_a].add(_b)
EDGES = [(_a, _b) for _a, _b in MAP['edges']]


def closure(a):
    out, stack = set(), list(USES[a])
    while stack:
        x = stack.pop()
        if x not in out:
            out.add(x)
            stack.extend(USES[x])
    return out


REACH = {a: closure(a) for a in NAMES}
# the transitive reduction: a -> b is kept unless b is reached through another name that a uses
REDUCED = {a: {b for b in USES[a] if not any(b in REACH[c] for c in USES[a] if c != b)} for a in NAMES}

TITLES = {'notation': 'Notation', 'declarations': 'Declarations', 'calculus': 'Calculus',
          'perturbation theory': 'Perturbation theory', 'diagram classes': 'Diagram classes',
          'functionals and vertices': 'Functionals and vertices', 'channels and the parquet': 'Channels and the parquet',
          'tree expansion': 'Tree expansion', 'pictures': 'Pictures', 'evaluation: Matsubara sums': 'Matsubara sums',
          'evaluation: dimensional regularization': 'Dimensional regularization',
          'evaluation: QED at one loop': 'QED at one loop',
          'effective actions and operator traces': 'Effective actions and operator traces', 'the package': 'The package'}


def title(cat):
    return TITLES.get(cat, cat[:1].upper() + cat[1:])


# the stages of a calculation, and where each group goes; a name that reaches FunctionalDerivative goes to stage 3
HUB = 'FunctionalDerivative'
STAGE_OF_CATEGORY = {'declarations': 'declare', 'notation': 'write', 'calculus': 'differentiate',
                     'perturbation theory': 'differentiate', 'functionals and vertices': 'differentiate',
                     'tree expansion': 'differentiate', 'effective actions and operator traces': 'differentiate',
                     'diagram classes': 'classify', 'channels and the parquet': 'classify', 'pictures': 'draw',
                     'the package': 'declare'}
# editorial placements: the trace vocabulary and the Legendre heads are written in a functional, the auxiliary-field
# transforms rewrite one (stage 2); ZeroDimensionalValue counts the diagrams of a functional (stage 4)
STAGE_OVERRIDE = {'TraceLog': 'write', 'OperatorTrace': 'write', 'OperatorKernel': 'write',
                  'LegendreTransform': 'write', 'LegendrePropagator': 'write',
                  'HubbardStratonovich': 'write', 'IntegrateOutField': 'write', 'ZeroDimensionalValue': 'classify'}
TRANSFORMS = {'HubbardStratonovich', 'IntegrateOutField'}
# the names of stage 3 that FunctionalDerivative itself uses, drawn beside it, and what it uses them for
PARTNER_NOTE = {'SimplifyDiagrams': 'the canonical form', 'OrderPart': 'the orders kept'}


def stage_of(a):
    if a in STAGE_OVERRIDE:
        return STAGE_OVERRIDE[a]
    if HUB in REACH[a]:
        return 'differentiate'
    return STAGE_OF_CATEGORY.get(CAT[a], 'evaluate')


STAGE = {a: stage_of(a) for a in NAMES}
STAGES = [('declare', '1', 'Declare'), ('write', '2', 'Write a functional'), ('differentiate', '3', 'Differentiate'),
          ('classify', '4', 'Classify and check'), ('draw', '5', 'Draw'), ('evaluate', '6', 'Evaluate')]
STAGE_TITLE = {s: (n, t) for s, n, t in STAGES}
FUNCTIONS = [a for a in NAMES if KIND[a] == 'function']
NOTATION_USERS = (sum(1 for a in FUNCTIONS if any(CAT[b] == 'notation' for b in USES[a])), len(FUNCTIONS))
# the vocabulary, read by many functions: its uses are left out of the static picture
VOCAB = {a for a in NAMES if KIND[a] == 'data' or STAGE[a] in ('declare', 'write') and a not in TRANSFORMS}


def deps(a):
    return sorted((b for b in REDUCED[a] if b not in VOCAB), key=ORDER.get)


# ------------------------------------------------------------------------------------------------------------ colours
# One colour per group, in the order of 00_header.wl (light, dark). Blues for the notation and the declarations, greens
# for the calculus and perturbation theory, violets for vertices and channels, warm colours for the evaluation, grey for
# the effective actions, brown for the package itself (its update check, drawn with the declarations). Two pairs that share a panel and looked alike in a chip tint were moved apart (OKLab, also
# under simulated protanopia and deuteranopia): functionals and vertices / tree expansion (a blue-violet and a warm
# purple), Matsubara sums / QED (a crimson and a magenta). Every group also carries its name, so that colour is never
# the only cue.
PALETTE = [('#1d5b92', '#4681bc'), ('#00b5d1', '#46e0ff'), ('#027d6e', '#10a895'), ('#35ae63', '#65d88a'),
           ('#9fb911', '#c6e536'), ('#4366e8', '#7698fc'), ('#ad619f', '#d889c9'), ('#852fbc', '#b16ee5'),
           ('#ba6c00', '#ee8f1b'), ('#ce355d', '#fd6283'), ('#a82900', '#dd4c27'), ('#c04ad2', '#e08ced'),
           ('#7d8590', '#9198a1'), ('#8c6239', '#c4925f')]
THEMES = {
    'light': dict(card='#ffffff', panel='#f6f8fa', ink='#1f2328', ink2='#59636e', muted='#5f6872', hair='#d1d9e0',
                  code='#ffffff', headtext='#424a53', tree='#8c959f', spine='#1d5b92', on='#ffffff', hub='#027d6e',
                  chip=0.17, rim=0.55, dash=0.85, swatch=0.3, swrim=0.7),
    'dark': dict(card='#0d1117', panel='#151b23', ink='#f0f6fc', ink2='#9ea7b3', muted='#949ca7', hair='#3d444d',
                 code='#0d1117', headtext='#d1d7e0', tree='#656c76', spine='#2c6cb0', on='#ffffff', hub='#0b7a6c',
                 chip=0.26, rim=0.55, dash=0.6, swatch=0.5, swrim=0.85),
}


def hex_rgb(h):
    return np.array([int(h[i:i + 2], 16) for i in (1, 3, 5)], float)


def mix(h, background, t):
    """the opaque colour of h at opacity t over background"""
    c = t * hex_rgb(h) + (1 - t) * hex_rgb(background)
    return '#' + ''.join('%02x' % int(round(v)) for v in c)


def luminance(h):
    v = hex_rgb(h) / 255
    v = np.where(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055) ** 2.4)
    return 0.2126 * v[0] + 0.7152 * v[1] + 0.0722 * v[2]


def contrast(a, b):
    la, lb = sorted([luminance(a), luminance(b)])
    return (lb + 0.05) / (la + 0.05)


def text_colour(h, background, minimum):
    """h darkened (on a light background) or lightened (on a dark one) just enough for the contrast minimum"""
    towards = '#000000' if luminance(background) > 0.4 else '#ffffff'
    t = 0.0
    while contrast(mix(towards, h, t), background) < minimum and t < 1:
        t += 0.02
    return mix(towards, h, t)


def role_colour(role, theme):
    T = THEMES[theme]
    if role == 'spine-soft':
        return mix(T['spine'], T['card'], 0.55 if theme == 'light' else 0.7)
    if role == 'hub-text':
        return text_colour(T['hub'], T['panel'], 4.8)
    if role.startswith('g'):
        k, _, kind = role[1:].partition('-')
        base = PALETTE[int(k)][0 if theme == 'light' else 1]
        under = T['card'] if theme == 'light' else T['panel']
        if kind == '':
            return base
        if kind in ('chip', 'rim', 'dash', 'swatch', 'swrim'):
            return mix(base, under, T[kind])
        if kind == 'text':
            return text_colour(base, T['panel'], 4.8)
        if kind == 'note':
            return text_colour(base, T['panel'], 5.0)
        if kind == 'code':
            return text_colour(base, T['code'], 5.0)
    return T[role]


ROLES = (['card', 'panel', 'ink', 'ink2', 'muted', 'hair', 'code', 'headtext', 'tree', 'spine', 'spine-soft', 'on',
          'hub', 'hub-text'] + ['g%d%s' % (k, s) for k in range(len(CATEGORIES))
                                for s in ('', '-chip', '-rim', '-dash', '-text', '-note', '-code', '-swatch', '-swrim')])


def group(a_or_cat):
    cat = CAT.get(a_or_cat, a_or_cat)
    return 'g%d' % CATEGORIES.index(cat)


class Paint:
    """colours by role: hexadecimal values of the light theme for the static picture, CSS variables (defined for both
    themes) for the interactive page"""

    def __init__(self, css):
        self.css = css

    def __call__(self, style='', **paints):
        paints = {k.replace('_', '-'): v for k, v in paints.items() if v}
        if self.css:
            parts = ['%s:var(--%s)' % kv for kv in paints.items()] + ([style] if style else [])
            return 'style="%s"' % ';'.join(parts) if parts else ''
        out = ' '.join('%s="%s"' % (k, role_colour(v, 'light')) for k, v in paints.items())
        return out + (' style="%s"' % style if style else '')


def check_contrast():
    """the worst contrast of the text roles against the backgrounds they are written on, in both themes"""
    worst = []
    for theme in THEMES:
        T = THEMES[theme]
        pairs = [('ink2', 'panel'), ('muted', 'panel'), ('muted', 'card'), ('hub-text', 'panel'), ('ink', 'panel'),
                 ('on', 'spine'), ('on', 'hub')]
        pairs += [('g%d-%s' % (k, s), 'panel') for k in range(len(CATEGORIES)) for s in ('text', 'note')]
        pairs += [('ink', 'g%d-chip' % k) for k in range(len(CATEGORIES))]
        for a, b in pairs:
            worst.append((round(contrast(role_colour(a, theme), role_colour(b, theme)), 2), theme, a, b))
    return min(worst)


# ------------------------------------------------------------------------------------------------------- text metrics
FONT = {False: FontProperties(family='DejaVu Sans'), True: FontProperties(family='DejaVu Sans', weight='bold')}
_widths = {}


def tw(s, size, bold=False):
    """width of s, from the outlines of DejaVu Sans (wider than the fonts GitHub uses: an upper bound)"""
    if (s, bold) not in _widths:
        ext = TextPath((0, 0), s.replace(' ', ' '), size=100, prop=FONT[bold]).get_extents()
        _widths[(s, bold)] = ext.x1 + 3
    return _widths[(s, bold)] * size / 100


def mw(s, size):
    """width of s in a monospace font (0.602 em, DejaVu Sans Mono: an upper bound for the fonts of GitHub)"""
    return len(s) * 0.602 * size


def esc(s):
    return html.escape(s, quote=True)


def f(x):
    return ('%.1f' % x).rstrip('0').rstrip('.')


# ----------------------------------------------------------------------------------------------------------- geometry
W = 800
MARGIN = 18
GAP = 12                 # between panels
PAD = 12                 # inside a panel
NAME = 12.5              # font size of the names (13 px in the README column)
HEAD = 11.0              # font size of the inert heads
NOTE = 10.75             # font size of the "uses" notes
CHIP_H = 19
ROW = 22                 # vertical pitch of a row of names
INDENT = 18              # of a level of a tree
CHIP_PAD = 6
HEADER_H = 39
TITLE = 20.0             # font size of the titles of the stages
CAPTION = 12.0           # of the caption of the hub
SPACE = 3.4              # the gap of a space in a note (dx: renderers drop spaces at the ends of tspans)
PREFIXES = ('Declare',)


class Canvas:
    """the SVG items of the map; measure=True only measures. Records where each name is drawn, and which
    dependencies are drawn"""

    def __init__(self, paint, measure=False):
        self.items, self.paint, self.measure = [], paint, measure
        self.boxes, self.drawn, self.relations = {}, [], set()

    def add(self, s):
        if not self.measure:
            self.items.append(s)

    def relation(self, a, b):
        if not self.measure:
            self.relations.add((a, b))


def measure(fn, canvas, *args, **kw):
    return fn(Canvas(canvas.paint, True), *args, **kw)


def chip_width(a, size=None, count=None):
    data = KIND[a] == 'data'
    size = size or (HEAD if data else NAME)
    return tw(a, size) + 2 * (CHIP_PAD - data) + (tw(' %d' % count, 10) if count else 0)


def open_name(cv, a, x, y, w, h):
    """the group of a name: with its usage message as a tooltip (static) or as a selectable node (interactive)"""
    cv.boxes[a] = (x, y, w, h)
    if cv.paint.css:
        cv.add('<g class="node" data-i="%d" tabindex="0" role="button" aria-label="%s">' % (ORDER[a], esc(a)))
    else:
        cv.add('<g><title>%s</title>' % esc(a + ': ' + NODES[a]['usage']))


def chip(cv, x, y, a, size=None, h=CHIP_H, count=None):
    """a name, left edge x, vertical centre y; functions filled, inert heads dashed; returns the width"""
    p = cv.paint
    data = KIND[a] == 'data'
    size = size or (HEAD if data else NAME)
    w = chip_width(a, size, count)
    if cv.measure:
        return w
    cv.drawn.append(a)
    g = group(a)
    label = esc(a)
    for pre in PREFIXES:
        if a.startswith(pre) and len(a) > len(pre):
            label = '<tspan %s>%s</tspan>%s' % (p(fill='ink2'), pre, esc(a[len(pre):]))
    if count:
        label += '<tspan dx="%s" font-size="10" %s>%d</tspan>' % (f(3.2), p(fill='muted'), count)
    if data:
        top, hh = y - h / 2 + 1.5, h - 3
        open_name(cv, a, x, top, w, hh)
        cv.add('<rect class="box" x="%s" y="%s" width="%s" height="%s" rx="5" %s stroke-width="1" '
               'stroke-dasharray="3 2"/>' % (f(x), f(top), f(w), f(hh), p(fill='card', stroke=g + '-dash')))
        cv.add('<text x="%s" y="%s" font-size="%s" %s>%s</text></g>'
               % (f(x + CHIP_PAD - 1), f(y + 0.36 * size), f(size), p(fill='headtext'), label))
    else:
        open_name(cv, a, x, y - h / 2, w, h)
        cv.add('<rect class="box" x="%s" y="%s" width="%s" height="%s" rx="5" %s stroke-width="1"/>'
               % (f(x), f(y - h / 2), f(w), f(h), p(fill=g + '-chip', stroke=g + '-rim')))
        cv.add('<text x="%s" y="%s" font-size="%s" %s>%s</text></g>'
               % (f(x + CHIP_PAD), f(y + 0.36 * size), f(size), p(fill='ink'), label))
    return w


def note_pieces(names):
    """(text, name or None, gap before) of 'uses A, B, C'"""
    out = [('uses', None, 0)]
    for i, b in enumerate(names):
        out.append((b, b, SPACE))
        if i < len(names) - 1:
            out.append((',', None, 0))
    return out


def note_width(names):
    return sum(g + tw(t, NOTE) for t, _, g in note_pieces(names))


def note_lines(names, width):
    """'uses A, B, C' wrapped to width: a list of lines, each a list of pieces; a line breaks only before a name"""
    lines, cur, curw = [], [], 0
    for t, b, g in note_pieces(names):
        pw = g + tw(t, NOTE)
        if b is not None and cur and curw + pw > width and len(cur) > 1:
            lines.append(cur)
            cur, curw, g = [], 0, 0
            pw = tw(t, NOTE)
        cur.append((t, b, g))
        curw += pw
    lines.append(cur)
    return lines


def draw_note(cv, x, y, a, lines):
    """the 'uses' note of a, the baseline of its first line at y; returns the height"""
    p = cv.paint
    for k, parts in enumerate(lines):
        spans = []
        for t, b, g in parts:
            if b is not None:
                cv.relation(a, b)
            spans.append('<tspan%s %s>%s</tspan>' % (' dx="%s"' % f(g) if g else '',
                                                     p(fill='muted' if b is None else group(b) + '-note'), esc(t)))
        cv.add('<text x="%s" y="%s" font-size="%s" font-style="italic">%s</text>'
               % (f(x), f(y + k * (NOTE + 3)), f(NOTE), ''.join(spans)))
    return len(lines) * (NOTE + 3)


# ------------------------------------------------------------------------------------------------------------- trees
def forest(members_):
    """the names as a forest: the parent of a name is the member it uses with the longest chain below it"""
    members_ = sorted(members_, key=ORDER.get)
    mset = set(members_)
    depth = {}

    def dep(a):
        if a not in depth:
            depth[a] = 1 + max((dep(b) for b in deps(a) if b in mset), default=0)
        return depth[a]

    parent = {}
    for a in members_:
        inside = [b for b in deps(a) if b in mset]
        if inside:
            parent[a] = max(inside, key=lambda b: (dep(b), -ORDER[b]))
    children = defaultdict(list)
    for a, q in parent.items():
        children[q].append(a)
    roots = [a for a in members_ if a not in parent]

    def size(a):
        return 1 + sum(size(c) for c in children[a])

    roots.sort(key=lambda a: (-(size(a) > 1), ORDER[a]))
    for q in children:
        children[q].sort(key=ORDER.get)
    notes = {a: [b for b in deps(a) if b != parent.get(a)] for a in members_}
    return roots, children, notes


def heads_used_in_stage(users, stage):
    """the inert heads of a stage drawn under the first of their users of the same group"""
    out = {}
    for a in NAMES:
        if STAGE[a] == stage and KIND[a] == 'data':
            us = [b for b in users if a in USES[b] and CAT[b] == CAT[a]]
            if us:
                out.setdefault(us[0], []).append(a)
    return out


ATTACHED = {}            # name -> the inert heads drawn under it (stage 3)


def draw_tree(cv, x, y, a, F, width, depth=0, line='tree'):
    """the subtree of a: its chip at x + depth INDENT, its row from y; returns the height"""
    p = cv.paint
    roots, children, notes = F
    cx = x + depth * INDENT
    cy = y + ROW / 2
    w = chip(cv, cx, cy, a)
    h = ROW
    if notes[a]:
        if cx + w + 8 + note_width(notes[a]) <= x + width:
            draw_note(cv, cx + w + 7, cy + 0.36 * NOTE, a, note_lines(notes[a], 1e9))
        else:
            base = cy + CHIP_H / 2 + NOTE * 0.82
            hn = draw_note(cv, cx + 8, base, a, note_lines(notes[a], x + width - cx - 8))
            h = base - y + hn - NOTE + 1
    if ATTACHED.get(a):
        h += flow_chips(cv, cx + 12, y + h - 3, x + width - cx - 12, ATTACHED[a], gap=4, row=20) - 2
        for b in ATTACHED[a]:
            cv.relation(a, b)
    stem_x = cx + 4.5
    last = None
    for c in children[a]:
        top = y + h
        h += draw_tree(cv, x, top, c, F, width, depth + 1, line)
        cv.relation(c, a)
        cv.add('<path d="M%s %sH%s" %s stroke-width="1.3" fill="none"/>'
               % (f(stem_x), f(top + ROW / 2), f(cx + INDENT), p(stroke=line)))
        last = top + ROW / 2
    if last is not None:
        cv.add('<path d="M%s %sV%s" %s stroke-width="1.3" fill="none"/>'
               % (f(stem_x), f(cy + CHIP_H / 2), f(last), p(stroke=line)))
    return h


def draw_forest(cv, x, y, names_, width, F=None, keep_order=False):
    """the trees, then the names that are neither built on nor used by another one here, side by side; with
    keep_order, the roots in the order of the usage messages (the order of a calculation), consecutive single names
    side by side"""
    F = F or forest(names_)
    roots, children, notes = F
    leaves = [r for r in roots if not children[r] and not notes[r] and not ATTACHED.get(r)]
    yy = y
    if keep_order:
        run = []
        for r in sorted(roots, key=ORDER.get) + [None]:
            if r in leaves:
                run.append(r)
                continue
            if run:
                yy += flow_chips(cv, x, yy, width, run)
                run = []
            if r is not None:
                yy += draw_tree(cv, x, yy, r, F, width)
        return yy - y
    for r in roots:
        if r not in leaves:
            yy += draw_tree(cv, x, yy, r, F, width)
    if leaves:
        yy += flow_chips(cv, x, yy, width, leaves)
    return yy - y


def first_fit(names_, width, gap=5, counts=None):
    """the names in rows of at most width, each row filled with the first names that still fit"""
    counts = counts or {}
    left, out = list(names_), []
    while left:
        cx = 0
        for a in list(left):
            w = chip_width(a, count=counts.get(a))
            if cx == 0 or cx + w <= width:
                out.append(a)
                left.remove(a)
                cx += w + gap
    return out


def flow_chips(cv, x, y, width, names_, gap=5, row=None, pack=False, counts=None):
    """chips left to right, wrapped at width; returns the height"""
    row = row or ROW
    counts = counts or {}
    if pack:
        names_ = first_fit(names_, width, gap, counts)
    cx, cy = x, y + row / 2
    for a in names_:
        w = chip_width(a, count=counts.get(a))
        if cx > x and cx + w > x + width:
            cx, cy = x, cy + row
        chip(cv, cx, cy, a, count=counts.get(a))
        cx += w + gap
    return cy + row / 2 - y


# ------------------------------------------------------------------------------------------------------------ panels
def panel(cv, x, y, w, h, stage):
    p = cv.paint
    number, name = STAGE_TITLE[stage]
    if not cv.measure:
        cv.stage_boxes[stage] = (x, y, w, h)
    cv.add('<rect x="%s" y="%s" width="%s" height="%s" rx="10" %s stroke-width="1"/>'
           % (f(x), f(y), f(w), f(h), p(fill='panel', stroke='hair')))
    cv.add('<circle cx="%s" cy="%s" r="13.5" %s/>' % (f(x + PAD + 13), f(y + 21), p(fill='spine')))
    cv.add('<text x="%s" y="%s" font-size="15.5" font-weight="700" text-anchor="middle" %s>%s</text>'
           % (f(x + PAD + 13), f(y + 26.5), p(fill='on'), number))
    tx = x + PAD + 34
    cv.add('<text x="%s" y="%s" font-size="%s" font-weight="700" %s>%s</text>'
           % (f(tx), f(y + 28), f(TITLE), p(fill='ink'), esc(name)))
    return tx + tw(name, TITLE, True) + 10


def wrap(text, width, size):
    words, lines, cur = text.split(' '), [], ''
    for w in words:
        t = (cur + ' ' + w).strip()
        if cur and tw(t, size) > width:
            lines.append(cur)
            cur = w
        else:
            cur = t
    lines.append(cur)
    return lines


def rich_text(cv, x, y, runs, width, size, lead):
    """runs of (text, emphasised) wrapped to width, the first baseline at y; returns the height"""
    p = cv.paint
    words = [(w, e) for t, e in runs for w in t.split(' ')]
    lines, cur, curw = [], [], 0
    sp = tw('a a', size) - tw('aa', size)
    for w, e in words:
        ww = tw(w, size, e) - 3 * size / 100
        if cur and curw + sp + ww > width:
            lines.append(cur)
            cur, curw = [], 0
        curw += (sp if cur else 0) + ww
        cur.append((w, e))
    lines.append(cur)
    for k, line in enumerate(lines):
        spans, run, prev = [], [], None
        for w, e in line + [(None, None)]:
            if e != prev and run:
                attrs = 'font-weight="600" ' + p(fill='ink') if prev else p(fill='ink2')
                spans.append('<tspan%s %s>%s</tspan>' % (' dx="%s"' % f(0.28 * size) if spans else '', attrs,
                                                          esc(' '.join(run))))
                run = []
            if w is not None:
                run.append(w)
            prev = e
        cv.add('<text x="%s" y="%s" font-size="%s">%s</text>' % (f(x), f(y + k * lead), f(size), ''.join(spans)))
    return len(lines) * lead


CODE = 10.0
# the free generating functional of the README, its notation in the colour of the notation
EXAMPLE = [('Z0 = Exp[-', None), ('IntegralOver', 'notation'), ('[G0[x, y] etabar[x] ', None), ('**', 'notation'),
           (' eta[y], {x, y}]]', None)]
EXAMPLE_BREAK = 2         # the line breaks before this token when the example does not fit


def code_box(cv, x, y, width, tokens):
    """a line of code (two when it does not fit), top at y; returns the height"""
    p = cv.paint
    full = ''.join(t for t, _ in tokens)
    if mw(full, CODE) + 14 <= width:
        lines = [tokens]
    else:
        lines = [tokens[:EXAMPLE_BREAK] + [('[', None)],
                 [('    ' + tokens[EXAMPLE_BREAK][0].lstrip('['), None)] + tokens[EXAMPLE_BREAK + 1:]]
    h = 8 + len(lines) * (CODE + 4)
    w = max(mw(''.join(t for t, _ in l), CODE) for l in lines) + 14
    cv.add('<rect x="%s" y="%s" width="%s" height="%s" rx="5" %s/>' % (f(x), f(y), f(w), f(h),
                                                                       p(fill='code', stroke='hair')))
    for k, line in enumerate(lines):
        spans = ''.join('<tspan %s>%s</tspan>' % ('font-weight="700" ' + p(fill=group(c) + '-code') if c else
                                                  p(fill='ink'), esc(t)) for t, c in line)
        cv.add('<text x="%s" y="%s" font-family="%s" font-size="%s" xml:space="preserve" %s>%s</text>'
               % (f(x + 7), f(y + 4 + (k + 1) * (CODE + 4) - 4), esc(MONO_STACK), f(CODE),
                  p(style='white-space:pre'), spans))
    return h


def subtitle(cv, x, y, text, size=11.5, role='ink2'):
    cv.add('<text x="%s" y="%s" font-size="%s" %s>%s</text>' % (f(x), f(y), f(size), cv.paint(fill=role), esc(text)))


def group_label(cv, x, y, cat, text=None, extra=''):
    """a small heading in the colour of a group, baseline at y + 11; returns its height"""
    p = cv.paint
    cv.add('<text x="%s" y="%s" font-size="11" font-weight="700" letter-spacing="0.04em" %s>%s'
           '<tspan dx="6" font-weight="400" letter-spacing="0" %s>%s</tspan></text>'
           % (f(x), f(y + 11), p(fill=group(cat) + '-text'), esc((text or title(cat)).upper()), p(fill='ink2'),
              esc(extra)))
    return 17


def swatch_legend(cv, x, y, cats, width):
    """the colours of some groups, wrapped at width; returns the height"""
    p = cv.paint
    cx, cy = x, y
    for c in cats:
        label = title(c)
        w = 14 + tw(label, 11) + 14
        if cx > x and cx + w > x + width:
            cx, cy = x, cy + 16
        cv.add('<rect x="%s" y="%s" width="10" height="10" rx="2.5" %s/>'
               % (f(cx), f(cy + 1), p(fill=group(c) + '-swatch', stroke=group(c) + '-swrim')))
        cv.add('<text x="%s" y="%s" font-size="11" %s>%s</text>' % (f(cx + 14), f(cy + 10), p(fill='ink2'), esc(label)))
        cx += w
    return cy + 16 - y


def members(stage, cat=None):
    return sorted((a for a in NAMES if STAGE[a] == stage and (cat is None or CAT[a] == cat)), key=ORDER.get)


def sample_chip(cv, x, y, label, dashed=False):
    """a chip of the key, not a public name; left edge x, centre y; returns its width"""
    p = cv.paint
    w = tw(label, 11) + 10
    g = 'g%d' % CATEGORIES.index('effective actions and operator traces')
    if dashed:
        cv.add('<rect x="%s" y="%s" width="%s" height="15" rx="4" %s stroke-dasharray="3 2"/>'
               % (f(x), f(y - 7.5), f(w), p(fill='card', stroke=g + '-dash')))
    else:
        cv.add('<rect x="%s" y="%s" width="%s" height="17" rx="4" %s/>'
               % (f(x), f(y - 8.5), f(w), p(fill=g + '-chip', stroke=g + '-rim')))
    cv.add('<text x="%s" y="%s" font-size="11" %s>%s</text>' % (f(x + 5), f(y + 4), p(fill='ink'), esc(label)))
    return w


def draw_key(cv, x, y, width):
    """how to read the map, with samples; returns the height"""
    p = cv.paint
    cv.add('<text x="%s" y="%s" font-size="12.5" font-weight="700" %s>How to read the map</text>'
           % (f(x), f(y + 12), p(fill='ink')))
    yy = y + 31
    col = x + 78

    def say(text, yc):
        cv.add('<text x="%s" y="%s" font-size="11" %s>%s</text>' % (f(col), f(yc + 4), p(fill='ink2'), esc(text)))

    sample_chip(cv, x, yy, 'Function')
    say('a function, in its group’s colour', yy)
    yy += 21
    sample_chip(cv, x, yy, 'Head', dashed=True)
    say('an inert head, used as data', yy)
    yy += 21
    sample_chip(cv, x, yy, 'A')
    sample_chip(cv, x + 14, yy + 20, 'B')
    cv.add('<path d="M%s %sV%sH%s" %s stroke-width="1.3" fill="none"/>'
           % (f(x + 4.5), f(yy + 8.5), f(yy + 20), f(x + 14), p(stroke='tree')))
    cv.add('<text x="%s" y="%s" font-size="%s" font-style="italic" %s>uses<tspan dx="%s" %s>C</tspan></text>'
           % (f(x + 36), f(yy + 24), f(NOTE), p(fill='muted'), f(SPACE), p(fill='ink')))
    say('B is built on A', yy + 1)
    say('and also uses C', yy + 17)
    yy += 37
    for line in wrap('“uses” leaves out the names of 1 and 2 and the inert heads, read by many functions; '
                     'the interactive map has every dependency.', width, 10.5):
        cv.add('<text x="%s" y="%s" font-size="10.5" %s>%s</text>' % (f(x), f(yy + 4), p(fill='muted'), esc(line)))
        yy += 14
    return yy - y


def arrow_down(cv, x, y0, y1, width=2.4):
    p = cv.paint
    cv.add('<path d="M%s %sV%s" %s stroke-width="%s" fill="none"/>' % (f(x), f(y0), f(y1 - 5), p(stroke='spine-soft'),
                                                                       f(width)))
    cv.add('<path d="M%s %sl-5 -7h10z" %s/>' % (f(x), f(y1), p(fill='spine-soft')))


# ----------------------------------------------------------------------------------------------------------- drawing
def build(cv):
    """draws the map on the canvas; returns its height"""
    p = cv.paint
    cv.stage_boxes = {}
    inner = W - 2 * MARGIN
    y = MARGIN

    # the title, the counts, the idea
    cv.add('<text x="%s" y="%s" font-size="23" font-weight="700" %s>FunctionalDiagrams'
           '<tspan dx="7" font-weight="400" %s>·</tspan><tspan dx="7" font-weight="400" %s>map of the '
           'public functions</tspan></text>' % (f(MARGIN), f(y + 21), p(fill='ink'), p(fill='ink2'), p(fill='ink2')))
    cv.add('<text x="%s" y="%s" font-size="11.5" text-anchor="end" %s>%d public names · %d dependencies</text>'
           % (f(W - MARGIN), f(y + 20), p(fill='muted'), len(NAMES), len(EDGES)))
    y += 33
    lead = [('You write a functional (Z, W = log Z, an effective action, Φ): its functional derivatives are the '
             'diagrams.', True),
            ('The others declare the symbols and classify, draw and evaluate the diagrams: all %d public names, '
             'by their role.' % len(NAMES), False)]
    y += rich_text(cv, MARGIN, y + 11, lead, W - 2 * MARGIN, 13.2, 17.5) + 8

    # ---- row A: declare | write
    decl = members('declare', 'declarations')
    package = members('declare', 'the package')
    notation = members('write', 'notation')
    traces = [a for a in members('write') if CAT[a] == 'effective actions and operator traces']
    transforms = [a for a in members('write') if a in TRANSFORMS]
    counts = {a: len(NODES[a]['usedBy']) for a in notation}

    def declare_block(s, x0, y0, width):
        lines = wrap('the symbols: fields, sources, propagators, couplings', width, 11.5)
        if len(lines) > 1:
            lines = wrap('fields, sources, propagators, couplings', width, 11.5)
        for k, l in enumerate(lines):
            subtitle(s, x0, y0 + 9 + 15 * k, l)
        yy = y0 + 16 + 15 * (len(lines) - 1)
        yy += flow_chips(s, x0, yy, width, decl, pack=True) + 3
        yy += group_label(s, x0, yy, 'the package', 'the package', '· its update check')
        yy += flow_chips(s, x0, yy, width, package)
        return yy - y0

    def write_block(s, x0, y0, width):
        yy = y0
        subtitle(s, x0, yy + 9, 'Z₀ and the interaction, in the notation of the package')
        yy += 16
        yy += code_box(s, x0, yy, width, EXAMPLE) + 4
        subtitle(s, x0, yy + 9, 'then W = log Z, Γ (1PI, 2PI), Φ: see stage 3', 10.75, 'muted')
        yy += 17
        yy += group_label(s, x0, yy, 'notation', 'notation', '· read by %d of the %d functions' % NOTATION_USERS)
        yy += flow_chips(s, x0, yy, width, notation, counts=counts) + 3
        yy += group_label(s, x0, yy, 'effective actions and operator traces', 'traces, Legendre transforms')
        yy += flow_chips(s, x0, yy, width, traces) + 3
        yy += group_label(s, x0, yy, 'channels and the parquet', 'auxiliary fields')
        yy += draw_forest(s, x0, yy, transforms, width)
        return yy - y0

    best = None
    for wa in range(300, 520, 4):
        wb = inner - wa - GAP
        ha = measure(declare_block, cv, 0, 0, wa - 2 * PAD)
        hb = measure(write_block, cv, 0, 0, wb - 2 * PAD)
        if best is None or max(ha, hb) < best[0] - 0.5:
            best = (max(ha, hb), wa)
    hcontent, wa = best
    wb = inner - wa - GAP
    xa, xb = MARGIN, MARGIN + wa + GAP
    hrow = HEADER_H + hcontent + PAD
    panel(cv, xa, y, wa, hrow, 'declare')
    declare_block(cv, xa + PAD, y + HEADER_H, wa - 2 * PAD)
    panel(cv, xb, y, wb, hrow, 'write')
    write_block(cv, xb + PAD, y + HEADER_H, wb - 2 * PAD)
    y_flow = y + hrow
    y += hrow + 22

    # ---- row B: differentiate
    diff = members('differentiate')
    partners = [b for b in deps(HUB) if STAGE[b] == 'differentiate']
    below = [a for a in diff if HUB in REACH[a]]
    F = forest(below + [HUB])
    tops = F[1][HUB]
    attached = {h for v in ATTACHED.values() for h in v}
    tools = [a for a in diff if a != HUB and a not in below and a not in partners and a not in attached]
    ncol = 3
    colgap = 14
    TRUNK = 14            # from the trunk of a column to its chips
    colw = (inner - 2 * PAD - (ncol - 1) * colgap) / ncol
    heights = [measure(draw_tree, cv, TRUNK, 0, a, F, colw - TRUNK) for a in tops]
    best = None
    n = len(tops)
    for assign in itertools.product(range(ncol), repeat=n):
        hs = [0.0] * ncol
        for t, k in enumerate(assign):
            hs[k] += heights[t]
        if min(hs) == 0:
            continue
        key = (round(max(hs)), sum(1 for t in range(n - 1) if assign[t + 1] < assign[t]), assign)
        if best is None or key < best:
            best = key
    hcols = best[0]
    cols = [[t for t in range(n) if best[2][t] == k] for k in range(ncol)]
    hub_h = 48
    legend_cats = sorted({CAT[a] for a in diff}, key=CATEGORIES.index)
    title_end = MARGIN + PAD + 34 + tw('Differentiate', TITLE, True) + 18
    hleg = measure(swatch_legend, cv, title_end, 0, legend_cats, MARGIN + inner - PAD - title_end)
    hub_top = HEADER_H + max(0, hleg - 22) + 8
    tree_top = hub_top + hub_h + 30
    tool_top = tree_top + hcols + 8
    htools = 18 + measure(flow_chips, cv, 0, 0, inner - 2 * PAD, tools)
    hpanel = tool_top + htools + PAD
    panel(cv, MARGIN, y, inner, hpanel, 'differentiate')
    swatch_legend(cv, title_end, y + 14, legend_cats, MARGIN + inner - PAD - title_end)
    # the hub
    hub_w = tw('δ', 20, True) + 10 + tw(HUB, 20, True) + 44
    hx = MARGIN + inner / 2 - hub_w / 2
    hy = y + hub_top
    if not cv.measure:
        cv.drawn.append(HUB)
    open_name(cv, HUB, hx, hy, hub_w, hub_h)
    cv.add('<rect class="box" x="%s" y="%s" width="%s" height="%s" rx="13" %s/>'
           % (f(hx), f(hy), f(hub_w), f(hub_h), p(fill='hub')))
    cv.add('<text x="%s" y="%s" font-size="20" font-weight="700" %s>δ<tspan dx="10">%s</tspan></text></g>'
           % (f(hx + 22), f(hy + hub_h / 2 + 7), p(fill='on'), esc(HUB)))
    # what the derivatives give, to its left
    caption = [('of Z and W', ': correlation functions'), ('of Γ', ': vertex functions'), ('of Φ', ': Σ and Γ⁰')]
    room = hx - 14 - MARGIN - PAD
    need = max(tw(o, CAPTION, True) + tw(t, CAPTION) for o, t in caption)
    assert need <= room, 'the caption of the hub does not fit: %.0f > %.0f' % (need, room)
    for k, (op, text) in enumerate(caption):
        cv.add('<text x="%s" y="%s" font-size="%s" text-anchor="end" %s><tspan font-weight="700" %s>%s</tspan>%s'
               '</text>' % (f(hx - 14), f(hy + hub_h / 2 + 4.5 + 16 * (k - (len(caption) - 1) / 2)), f(CAPTION),
                            p(fill='ink2'), p(fill='hub-text'), esc(op), esc(text)))
    # what the hub itself uses, to its right (the canonical form, the orders kept): stacked, each with its caption
    # under it, joined to the hub by a fork that ends at the chips (a line across a chip would strike its name out)
    missing_note = [b for b in partners if b not in PARTNER_NOTE]
    assert not missing_note, 'no caption in PARTNER_NOTE for %s, used by the hub' % missing_note
    xr, mid = hx + hub_w, hy + hub_h / 2
    px, fork = xr + 22, xr + 11
    pitch = CHIP_H + NOTE + 5
    centres = [mid + pitch * (k - (len(partners) - 1) / 2) for k in range(len(partners))]
    right = MARGIN + inner - PAD
    if partners:
        assert centres[0] - CHIP_H / 2 >= hy - 8 and centres[-1] + CHIP_H / 2 + NOTE + 2.5 <= y + tree_top - 10, \
            'the names used by the hub do not fit beside it: %s' % partners
        cv.add('<path d="M%s %sH%sM%s %sV%s%s" %s stroke-width="1.5" fill="none"/>'
               % (f(xr), f(mid), f(fork), f(fork), f(centres[0]), f(centres[-1]),
                  ''.join('M%s %sH%s' % (f(fork), f(c), f(px)) for c in centres), p(stroke='hub')))
    for b, c in zip(partners, centres):
        cv.relation(HUB, b)
        w = chip(cv, px, c, b)
        assert px + max(w, 6 + tw(PARTNER_NOTE[b], NOTE)) <= right, '%s does not fit beside the hub' % b
        cv.add('<text x="%s" y="%s" font-size="%s" font-style="italic" %s>%s</text>'
               % (f(px + 6), f(c + CHIP_H / 2 + NOTE - 0.5), f(NOTE), p(fill='muted'), esc(PARTNER_NOTE[b])))
    # the flow from the two panels above into the hub
    for sx in (xa + wa / 2, xb + wb / 2):
        arrow_down(cv, sx, y_flow, y)
    # the trunk and the columns, in the colour of the hub: what the derivative generates
    bus_y = y + tree_top - 14
    col_x = [MARGIN + PAD + k * (colw + colgap) for k in range(ncol)]
    stroke = '%s stroke-width="2.4" fill="none" stroke-linecap="round"' % p(stroke='hub')
    cv.add('<path d="M%s %sV%sM%s %sH%s" %s/>'
           % (f(hx + hub_w / 2), f(hy + hub_h), f(bus_y), f(col_x[0] + 4), f(bus_y), f(col_x[-1] + 4), stroke))
    for k, idx in enumerate(cols):
        yy = y + tree_top
        rows = []
        for t in idx:
            draw_tree(cv, col_x[k] + TRUNK, yy, tops[t], F, colw - TRUNK, line='hub')
            cv.relation(tops[t], HUB)
            rows.append(yy + ROW / 2)
            yy += heights[t]
        cv.add('<path d="M%s %sV%s %s" %s/>'
               % (f(col_x[k] + 4), f(bus_y), f(rows[-1]), ' '.join('M%s %sH%s' % (f(col_x[k] + 4), f(r),
                                                                                  f(col_x[k] + TRUNK)) for r in rows),
                  stroke))
    cv.add('<text x="%s" y="%s" font-size="%s" font-style="italic" %s>also in this stage</text>'
           % (f(MARGIN + PAD), f(y + tool_top + 11), f(NOTE), p(fill='muted')))
    flow_chips(cv, MARGIN + PAD, y + tool_top + 18, inner - 2 * PAD, tools)
    y_after_diff = y + hpanel
    y += hpanel + 22

    # ---- row C: classify | draw
    clf = members('classify')
    left = [a for a in clf if CAT[a] != 'channels and the parquet']
    right = [a for a in clf if a not in left]
    draw = members('draw')

    def classify_column(s, x0, y0, names_, width, label_cat):
        hh = group_label(s, x0, y0, label_cat)
        return hh + draw_forest(s, x0, y0 + hh, names_, width)

    def row_c(wcl):
        colw3 = inner - wcl - GAP
        sub = (wcl - 2 * PAD - 16) / 2
        hl = measure(classify_column, cv, 0, 0, left, sub, 'diagram classes')
        hr = measure(classify_column, cv, 0, 0, right, sub, 'channels and the parquet')
        hc = HEADER_H + max(hl, hr) + PAD
        hd = HEADER_H + 17 + measure(draw_forest, cv, 0, 0, draw, colw3 - 2 * PAD) + PAD
        return max(hc, hd + 14 + measure(draw_key, cv, 0, 0, colw3 - 12)), hd

    widest = max(chip_width(a) for a in clf) + 2 * INDENT
    best = None
    for wcl in range(int(2 * (widest + PAD) + 16), int(inner * 0.75), 4):
        h, _ = row_c(wcl)
        if best is None or h < best[0] - 0.5:
            best = (h, wcl)
    hrow, wcl = best
    hd = row_c(wcl)[1]
    colw3 = inner - wcl - GAP
    sub = (wcl - 2 * PAD - 16) / 2
    panel(cv, MARGIN, y, wcl, hrow, 'classify')
    classify_column(cv, MARGIN + PAD, y + HEADER_H, left, sub, 'diagram classes')
    classify_column(cv, MARGIN + PAD + sub + 16, y + HEADER_H, right, sub, 'channels and the parquet')
    xd = MARGIN + wcl + GAP
    panel(cv, xd, y, colw3, hd, 'draw')
    group_label(cv, xd + PAD, y + HEADER_H, 'pictures')
    draw_forest(cv, xd + PAD, y + HEADER_H + 17, draw, colw3 - 2 * PAD)
    for sx in (MARGIN + wcl / 2, xd + colw3 / 2):
        arrow_down(cv, sx, y_after_diff, y)
    x_gutter = MARGIN + wcl + GAP / 2
    draw_key(cv, xd + 10, y + hd + 14, colw3 - 12)
    y += hrow + GAP

    # ---- row D: evaluate, one column per group, each in the order of a calculation
    ev = members('evaluate')
    cats = sorted({CAT[a] for a in ev}, key=CATEGORIES.index)
    workflow = {'evaluation: Matsubara sums': 'DiagramIntegrand → MatsubaraSum → AnalyticContinuation'}

    def eval_column(s, x0, y0, cat, width):
        names_ = [a for a in ev if CAT[a] == cat]
        funcs = [a for a in names_ if KIND[a] != 'data']
        heads = [a for a in names_ if KIND[a] == 'data']
        yy = y0 + group_label(s, x0, y0, cat)
        if cat in workflow:
            for line in wrap(workflow[cat], width, 10.5):
                subtitle(s, x0, yy + 8, line, 10.5, 'muted')
                yy += 13.5
            yy += 3
        yy += draw_forest(s, x0, yy, funcs, width, keep_order=True) + 4
        yy += flow_chips(s, x0, yy, width, heads, gap=4, row=20, pack=True)
        return yy - y0

    total = inner - 2 * PAD - 2 * 16

    def min_width(cat):
        """the widest row of the trees of a column (a name at its depth)"""
        roots, children, notes = forest([a for a in ev if CAT[a] == cat and KIND[a] != 'data'])
        out = []

        def walk(a, d):
            out.append(d * INDENT + chip_width(a))
            for c in children[a]:
                walk(c, d + 1)
        for r in roots:
            walk(r, 0)
        return max(out + [max(chip_width(a) for a in ev if CAT[a] == cat)]) + 4

    lows = [min_width(c) for c in cats]
    best = None
    for w1 in range(int(lows[0]), int(total) - int(lows[1]) - int(lows[2]), 4):
        for w2 in range(int(lows[1]), int(total) - w1 - int(lows[2]) + 1, 4):
            ws = [w1, w2, total - w1 - w2]
            hh = max(measure(eval_column, cv, 0, 0, c, w) for c, w in zip(cats, ws))
            key = (round(hh), abs(ws[0] - ws[1]) + abs(ws[1] - ws[2]))
            if best is None or key < best[0]:
                best = (key, ws)
    widths = best[1]
    he = HEADER_H + best[0][0] + PAD
    panel(cv, MARGIN, y, inner, he, 'evaluate')
    arrow_down(cv, x_gutter, y_after_diff, y)
    xx = MARGIN + PAD
    for c, w in zip(cats, widths):
        eval_column(cv, xx, y + HEADER_H, c, w)
        xx += w + 16
    y += he + MARGIN
    return math.ceil(y)


def prepare():
    """the inert heads of stage 3 hang under their first user of the same group"""
    diff = members('differentiate')
    below = [a for a in diff if HUB in REACH[a]]
    ATTACHED.update(heads_used_in_stage(below, 'differentiate'))


def description():
    return ('The %d public names of the package in %d groups, and the %d dependencies between them, by their role in '
            'a calculation: 1 declare the symbols, 2 write a functional, 3 differentiate it (FunctionalDerivative, the '
            'hub, with the names built on it), then 4 classify and check, 5 draw and 6 evaluate the diagrams. A name '
            'indented under another is built on it; "uses" lists the other names it uses (version %s).'
            % (len(NAMES), len(CATEGORIES), len(EDGES), MAP['version']))


def accounting(cv):
    """every dependency: drawn (a tree edge, a note, an inert head under its user), implied by the drawn ones through
    other names (not in the transitive reduction), or left out (a use of the vocabulary of stages 1 and 2 or of an
    inert head)"""
    reduced = {(a, b) for a in NAMES for b in REDUCED[a]}
    drawn = cv.relations & reduced
    extra = cv.relations - reduced
    left_out = {(a, b) for a, b in reduced - drawn if b in VOCAB or a in VOCAB}
    missing = reduced - drawn - left_out
    implied = len(EDGES) - len(reduced)
    assert not extra, 'drawn but not dependencies: %s' % sorted(extra)
    assert not missing, 'dependencies neither drawn nor left out: %s' % sorted(missing)
    assert len(drawn) + len(left_out) + implied == len(EDGES)
    return len(drawn), len(left_out), implied


# ------------------------------------------------------------------------------------------------------ static picture
def render_svg():
    cv = Canvas(Paint(False))
    H = build(cv)
    head = ('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" font-family="%s" '
            'role="img" aria-labelledby="map-title map-desc">' % (W, H, W, H, esc(FONT_STACK)))
    meta = ('<title id="map-title">FunctionalDiagrams: map of the public functions</title><desc id="map-desc">%s</desc>'
            % esc(description()))
    bg = ('<rect x="0.5" y="0.5" width="%d" height="%d" rx="14" fill="%s" stroke="%s"/>'
          % (W - 1, H - 1, role_colour('card', 'light'), role_colour('hair', 'light')))
    return '\n'.join([head, meta, bg] + cv.items + ['</svg>']) + '\n', cv, H


# ---------------------------------------------------------------------------------------------- interactive page
BETA = 0.85              # the straightening of a bundled line (Holten): 1 follows the hierarchy, 0 is straight


def centre(box):
    x, y, w, h = box
    return np.array([x + w / 2, y + h / 2])


def union(boxes):
    x0 = min(b[0] for b in boxes)
    y0 = min(b[1] for b in boxes)
    x1 = max(b[0] + b[2] for b in boxes)
    y1 = max(b[1] + b[3] for b in boxes)
    return (x0, y0, x1 - x0, y1 - y0)


def leave_box(box, towards):
    """where the segment from the centre of box to towards leaves the box"""
    c = centre(box)
    d = np.asarray(towards, float) - c
    if not d.any():
        return c
    x, y, w, h = box
    t = min(abs((w / 2) / d[0]) if d[0] else 1e9, abs((h / 2) / d[1]) if d[1] else 1e9, 1.0)
    return c + t * d


def bspline_path(points):
    """a uniform cubic B-spline through the control points, the ends tripled so that it starts and ends at them,
    as SVG cubic Bezier segments"""
    q = [points[0]] * 2 + list(points) + [points[-1]] * 2
    d = 'M%s %s' % (f(q[0][0]), f(q[0][1]))
    for i in range(len(q) - 3):
        p0, p1, p2, p3 = q[i:i + 4]
        b1 = (2 * p1 + p2) / 3
        b2 = (p1 + 2 * p2) / 3
        b3 = (p1 + 4 * p2 + p3) / 6
        d += 'C%s %s %s %s %s %s' % (f(b1[0]), f(b1[1]), f(b2[0]), f(b2[1]), f(b3[0]), f(b3[1]))
    return d


def edge_paths(cv):
    """the line of every dependency a -> b, bundled through the hierarchy name - its group in its stage - its stage"""
    region = {}
    for (stage, cat), names_ in itertools.groupby(sorted(cv.boxes, key=lambda a: (STAGE[a], CAT[a])),
                                                  key=lambda a: (STAGE[a], CAT[a])):
        region[(stage, cat)] = centre(union([cv.boxes[a] for a in names_]))
    stage_centre = {s: centre(b) for s, b in cv.stage_boxes.items()}
    out = []
    for a, b in EDGES:
        ra, rb = region[(STAGE[a], CAT[a])], region[(STAGE[b], CAT[b])]
        if (STAGE[a], CAT[a]) == (STAGE[b], CAT[b]):
            via = [ra]
        elif STAGE[a] == STAGE[b]:
            via = [ra, stage_centre[STAGE[a]], rb]
        else:
            via = [ra, stage_centre[STAGE[a]], stage_centre[STAGE[b]], rb]
        pa = leave_box(cv.boxes[a], via[0])
        pb = leave_box(cv.boxes[b], via[-1])
        pts = [pa] + via + [pb]
        n = len(pts) - 1
        pts = [BETA * pt + (1 - BETA) * (pa + (k / n) * (pb - pa)) for k, pt in enumerate(pts)]
        out.append(bspline_path(pts))
    return out


def render_html():
    cv = Canvas(Paint(True))
    H = build(cv)
    paths = edge_paths(cv)
    edges = ''.join('<path class="edge" data-s="%d" data-t="%d" d="%s"/>' % (ORDER[a], ORDER[b], d)
                    for (a, b), d in zip(EDGES, paths))
    svg = ('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" role="group" aria-labelledby="map-desc">'
           '<desc id="map-desc">%s</desc><defs id="grads"></defs>'
           '<rect class="card" x="0.5" y="0.5" width="%d" height="%d" rx="14" style="fill:var(--card);'
           'stroke:var(--hair)"/>%s<g id="edges" fill="none">%s</g></svg>'
           % (W, H, esc(description()), W - 1, H - 1, ''.join(cv.items), edges))

    def block(theme):
        return ' '.join('--%s:%s;' % (r, role_colour(r, theme)) for r in ROLES)

    nodes = [dict(name=a, group=CATEGORIES.index(CAT[a]), stage=[s for s, _, _ in STAGES].index(STAGE[a]),
                  kind=KIND[a], usage=NODES[a]['usage'], file=NODES[a]['file'],
                  uses=sorted(ORDER[b] for b in USES[a]), usedBy=sorted(ORDER[b] for b in NODES[a]['usedBy']))
             for a in NAMES]
    data = dict(version=MAP['version'], groups=[title(c) for c in CATEGORIES],
                stages=['%s %s' % (n, t) for _, n, t in STAGES], notation=CATEGORIES.index('notation'), nodes=nodes)
    repo = paclet_info()['url']
    with open(os.path.join(ROOT, 'Logo', 'FunctionalDiagrams_logo_small.svg'), encoding='utf-8') as f:
        favicon = 'data:image/svg+xml;base64,' + base64.b64encode(f.read().strip().encode('utf-8')).decode('ascii')
    with open(os.path.join(ROOT, 'Logo', 'FunctionalDiagrams_logo_white.svg'), encoding='utf-8') as f:
        mark = re.sub(r'<title>.*?</title>\s*', '', f.read().strip(), flags=re.S)
    mark = mark.replace('<svg ', '<svg aria-hidden="true" focusable="false" ', 1)
    page = HTML_PAGE
    for key, value in [('{{REPO}}', esc(repo)), ('{{SITE}}', esc(site_url(repo))), ('{{FAVICON}}', esc(favicon)),
                       ('{{MARK}}', mark), ('{{DESCRIPTION}}', esc(description())),
                       ('{{LIGHT}}', block('light')), ('{{DARK}}', block('dark')), ('{{SVG}}', svg),
                       ('{{DATA}}', json.dumps(data, ensure_ascii=False, separators=(',', ':')).replace('</', '<\\/')),
                       ('{{COUNTS}}', '%d public names, %d dependencies' % (len(NAMES), len(EDGES))),
                       ('{{VERSION}}', esc(MAP['version'])), ('{{FONTS}}', FONT_STACK)]:
        page = page.replace(key, value)
    return page, cv


HTML_PAGE = r'''<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>FunctionalDiagrams function map</title>
<meta name="description" content="{{DESCRIPTION}}">
<link rel="canonical" href="{{SITE}}Documentation/function_map.html">
<link rel="icon" href="{{FAVICON}}" type="image/svg+xml">
<meta property="og:type" content="website">
<meta property="og:site_name" content="FunctionalDiagrams">
<meta property="og:title" content="FunctionalDiagrams: map of the public functions">
<meta property="og:description" content="{{DESCRIPTION}}">
<meta property="og:url" content="{{SITE}}Documentation/function_map.html">
<meta property="og:image" content="{{SITE}}Logo/social_preview.png">
<meta name="twitter:card" content="summary_large_image">
<script>
  try { var t = localStorage.getItem('FunctionalDiagrams.theme');
        if (t === 'light' || t === 'dark') document.documentElement.setAttribute('data-theme', t); } catch (e) {}
</script>
<style>
:root { color-scheme: light; {{LIGHT}} --page: #ffffff; --sheet: #f6f8fa; --button: #f6f8fa; --ring: #0969da; --bar: #1f4e79; }
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) { color-scheme: dark; {{DARK}} --page: #0d1117; --sheet: #151b23; --button: #212830; --ring: #4493f8; --bar: #163b5e; }
}
:root[data-theme="dark"] { color-scheme: dark; {{DARK}} --page: #0d1117; --sheet: #151b23; --button: #212830; --ring: #4493f8; --bar: #163b5e; }
* { box-sizing: border-box; }
html { overflow-x: hidden; }  /* on html only: on body it would make body a scroll container, and the side panel would not stick */
body { margin: 0; background: var(--page); color: var(--ink); font-family: {{FONTS}}; font-size: 14px; line-height: 1.45; }
.site { background: var(--bar); color: #ffffff; }
.site .in { display: flex; flex-wrap: wrap; align-items: center; gap: 2px 18px; padding: 6px 20px; min-height: 52px; }
.site a { display: inline-flex; align-items: center; min-height: 36px; color: #d6e5f2; text-decoration: none; font-size: 14px; }
.site a:hover { color: #ffffff; text-decoration: underline; text-underline-offset: 3px; }
.site a:focus-visible { outline: 2px solid #ffffff; outline-offset: 2px; border-radius: 4px; }
.site .home { gap: 9px; margin-right: auto; color: #ffffff; font-weight: 650; font-size: 15px; }
.site .home svg { width: 26px; height: 26px; flex: none; }
.site .home span { color: #b9cfe3; font-weight: 400; }
.site nav { display: flex; flex-wrap: wrap; gap: 0 16px; }
header { padding: 16px 20px 8px; display: flex; flex-wrap: wrap; gap: 8px 24px; align-items: flex-end; justify-content: space-between; }
h1 { margin: 0; font-size: 20px; font-weight: 700; }
h1 span { font-weight: 400; color: var(--ink2); }
header p { margin: 4px 0 0; color: var(--ink2); font-size: 13px; max-width: 760px; }
.tools { display: flex; flex-wrap: wrap; gap: 8px 12px; align-items: center; }
input[type=search] { font: inherit; font-size: 13px; padding: 6px 10px; width: 230px; max-width: 100%; border: 1px solid var(--hair); border-radius: 6px; background: var(--page); color: var(--ink); }
label.toggle { font-size: 13px; color: var(--ink2); display: inline-flex; align-items: center; gap: 5px; cursor: pointer; }
button { font: inherit; font-size: 13px; padding: 6px 10px; border: 1px solid var(--hair); border-radius: 6px; background: var(--button); color: var(--ink); cursor: pointer; }
main { display: grid; grid-template-columns: minmax(0, 1fr) 340px; gap: 16px; padding: 8px 20px 20px; align-items: start; }
#map { min-width: 0; }
#map svg { width: 100%; max-width: 1150px; height: auto; display: block; margin: 0 auto; font-family: {{FONTS}}; }
aside { position: sticky; top: 12px; background: var(--sheet); border: 1px solid var(--hair); border-radius: 10px; padding: 14px 16px; max-height: calc(100vh - 24px); overflow: auto; }
aside h2 { margin: 0 0 4px; font-size: 15px; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; overflow-wrap: anywhere; }
aside .meta { color: var(--ink2); font-size: 12.5px; margin: 2px 0; display: flex; align-items: flex-start; gap: 6px; }
aside .dot { width: 10px; height: 10px; border-radius: 50%; display: inline-block; flex: none; margin-top: 4px; }
aside .file { color: var(--muted); font-size: 12px; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; overflow-wrap: anywhere; }
aside .usage { font-size: 13px; margin: 8px 0 12px; white-space: pre-wrap; overflow-wrap: anywhere; }
aside h3 { font-size: 12px; text-transform: uppercase; letter-spacing: .04em; color: var(--ink2); margin: 12px 0 6px; font-weight: 600; display: flex; align-items: center; gap: 8px; }
aside h3 svg { flex: none; }
.chips { display: flex; flex-wrap: wrap; gap: 4px; }
.chip { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 11.5px; padding: 2px 7px 2px 6px; border-radius: 10px; background: var(--card); color: var(--ink); border: 1px solid var(--hair); cursor: pointer; display: inline-flex; align-items: center; gap: 5px; }
.chip i { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }
.hint { color: var(--ink2); font-size: 13px; margin: 0; }
.none { color: var(--muted); font-size: 12.5px; }
.close { float: right; padding: 2px 8px; margin: -4px -6px 0 8px; }
#map .node { cursor: pointer; outline: none; }
#map .node .box { transition: opacity .12s, stroke-width .12s; }
#map .node:focus-visible .box { stroke: var(--ring) !important; stroke-width: 2.5; }
#map svg .node, #map svg text, #map svg path, #map svg rect, #map svg circle { transition: opacity .12s; }
#edges { pointer-events: none; }
#edges .edge { display: none; stroke-width: 1.6; }
#edges .edge.uses, #edges .edge.usedby { display: inline; }
#edges .edge.usedby { stroke: var(--ink2); stroke-dasharray: 4 3; stroke-opacity: .8; }
/* the exclusions in :where(), of no specificity, so that the rules for the names kept (.on, .near, .match) win */
#map svg.focus > :not(:where(#edges, .card, defs, desc)) { opacity: .22; }
#map svg.focus > .node.on, #map svg.focus > .node.near { opacity: 1; }
#map svg.focus .node.on .box { stroke: var(--ring) !important; stroke-width: 2.6; stroke-dasharray: none; }
#map svg.focus .node.near .box { stroke: var(--ink) !important; stroke-width: 1.8; }
#map svg.search > :not(:where(#edges, .card, defs, desc)) { opacity: .25; }
#map svg.search > .node.match { opacity: 1; }
#map svg.search .node.match .box { stroke: var(--ring) !important; stroke-width: 2.4; }
@media (max-width: 900px) {
  main { grid-template-columns: minmax(0, 1fr); padding: 8px 8px 45vh; }
  header { padding: 12px 12px 6px; }
  .site .in { padding: 4px 12px; gap: 0 14px; }
  .site .home span { display: none; }
  #map { overflow-x: auto; -webkit-overflow-scrolling: touch; }
  #map svg { min-width: 640px; }
  aside { position: fixed; left: 8px; right: 8px; bottom: 8px; top: auto; max-height: 40vh; box-shadow: 0 -2px 14px rgba(0,0,0,.2); z-index: 2; }
  aside.idle { max-height: none; padding: 8px 12px; }
  input[type=search] { width: 170px; }
  header p { font-size: 12.5px; }
  #map .node { scroll-margin-bottom: 40vh; }
}
</style>
</head>
<body>
<div class="site">
  <div class="in">
    <a class="home" href="../">{{MARK}}FunctionalDiagrams <span>{{VERSION}}</span></a>
    <nav aria-label="FunctionalDiagrams">
      <a href="../">Home</a>
      <a href="../Showcase/FunctionalDiagrams_Showcase.pdf">Showcase</a>
      <a href="FunctionalDiagrams_Manual.pdf">Manual</a>
      <a href="{{REPO}}/wiki">Wiki</a>
      <a href="{{REPO}}">GitHub</a>
    </nav>
  </div>
</div>
<header>
  <div>
    <h1>Map of the public functions</h1>
    <p>{{COUNTS}} (version {{VERSION}}). Point at a name, or tab to it, to see what it uses (lines in the colour of the name used, darker towards it) and what uses it (dashed); click, tap or press Enter to keep the selection, Escape or a click on the background to release it.</p>
  </div>
  <div class="tools">
    <input id="search" type="search" placeholder="Find a name…" aria-label="Find a name" autocomplete="off">
    <label class="toggle"><input id="notation" type="checkbox"> lines to the notation</label>
    <button id="theme" type="button" title="Switch between light and dark">Theme</button>
  </div>
</header>
<main>
  <div id="map">{{SVG}}</div>
  <aside id="panel" class="idle" aria-live="polite"><p class="hint">Select a name to read its usage message, its stage and group, the names it uses and the names that use it.</p></aside>
</main>
<script id="map-data" type="application/json">{{DATA}}</script>
<script>
(function () {
  var data = JSON.parse(document.getElementById('map-data').textContent);
  var svg = document.querySelector('#map svg');
  var defs = document.getElementById('grads');
  var panel = document.getElementById('panel');
  var hint = panel.innerHTML;
  var nodes = [];
  Array.prototype.forEach.call(svg.querySelectorAll('.node'), function (g) { nodes[+g.getAttribute('data-i')] = g; });
  var edges = Array.prototype.slice.call(svg.querySelectorAll('.edge'));
  var showNotation = document.getElementById('notation');
  var pinned = null, current = null;
  var NS = 'http://www.w3.org/2000/svg';

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function chip(i) {
    var b = el('button', 'chip');
    b.type = 'button';
    var dot = el('i');
    dot.style.background = 'var(--g' + data.nodes[i].group + ')';
    b.appendChild(dot);
    b.appendChild(document.createTextNode(data.nodes[i].name));
    b.addEventListener('click', function (ev) { ev.stopPropagation(); select(i, true); reveal(i); });
    return b;
  }
  function sample(dashed) {
    var s = document.createElementNS(NS, 'svg');
    s.setAttribute('width', '26'); s.setAttribute('height', '8');
    var p = document.createElementNS(NS, 'path');
    p.setAttribute('d', 'M1 4H25'); p.setAttribute('stroke-width', '2');
    p.style.stroke = dashed ? 'var(--ink2)' : 'var(--ink)';
    if (dashed) p.setAttribute('stroke-dasharray', '4 3');
    s.appendChild(p);
    return s;
  }
  function list(title, items, dashed) {
    var frag = document.createDocumentFragment();
    var h = el('h3');
    h.appendChild(sample(dashed));
    h.appendChild(document.createTextNode(title + ' (' + items.length + ')'));
    frag.appendChild(h);
    if (!items.length) { frag.appendChild(el('div', 'none', 'none')); return frag; }
    var box = el('div', 'chips');
    items.slice().sort(function (a, b) { return data.nodes[a].name < data.nodes[b].name ? -1 : 1; })
      .forEach(function (j) { box.appendChild(chip(j)); });
    frag.appendChild(box);
    return frag;
  }
  function show(i) {
    var n = data.nodes[i];
    panel.innerHTML = '';
    panel.classList.remove('idle');
    if (pinned !== null) {
      var close = el('button', 'close', '×');
      close.type = 'button';
      close.setAttribute('aria-label', 'Release the selection');
      close.addEventListener('click', function (ev) { ev.stopPropagation(); release(); });
      panel.appendChild(close);
    }
    panel.appendChild(el('h2', '', n.name));
    var meta = el('div', 'meta');
    var dot = el('span', 'dot');
    dot.style.background = 'var(--g' + n.group + ')';
    meta.appendChild(dot);
    meta.appendChild(el('span', '', data.groups[n.group] + ' · stage ' + data.stages[n.stage] + ' · ' +
      (n.kind === 'function' ? 'function' : 'inert head, used as data')));
    panel.appendChild(meta);
    panel.appendChild(el('div', 'file', 'Development/src/' + n.file));
    panel.appendChild(el('div', 'usage', n.usage));
    panel.appendChild(list('Uses', n.uses, false));
    panel.appendChild(list('Used by', n.usedBy, true));
  }
  function gradient(k, p, group) {
    var len = p.getTotalLength(), a = p.getPointAtLength(0), b = p.getPointAtLength(len);
    var g = document.createElementNS(NS, 'linearGradient');
    g.setAttribute('id', 'eg' + k);
    g.setAttribute('gradientUnits', 'userSpaceOnUse');
    g.setAttribute('x1', a.x); g.setAttribute('y1', a.y); g.setAttribute('x2', b.x); g.setAttribute('y2', b.y);
    [[0, .25], [1, 1]].forEach(function (s) {
      var stop = document.createElementNS(NS, 'stop');
      stop.setAttribute('offset', s[0]);
      stop.setAttribute('style', 'stop-color:var(--g' + group + ');stop-opacity:' + s[1]);
      g.appendChild(stop);
    });
    defs.appendChild(g);
    return 'url(#eg' + k + ')';
  }
  function clear() {
    svg.classList.remove('focus');
    nodes.forEach(function (g) { g.classList.remove('on', 'near'); });
    edges.forEach(function (p) { p.classList.remove('uses', 'usedby'); p.style.stroke = ''; });
    defs.innerHTML = '';
  }
  function highlight(i) {
    clear();
    current = i;
    var n = data.nodes[i], selfNotation = n.group === data.notation;
    svg.classList.add('focus');
    nodes[i].classList.add('on');
    edges.forEach(function (p, k) {
      var s = +p.getAttribute('data-s'), t = +p.getAttribute('data-t');
      if (s !== i && t !== i) return;
      var other = s === i ? t : s;
      if (!selfNotation && !showNotation.checked && data.nodes[other].group === data.notation) return;
      nodes[other].classList.add('near');
      if (s === i) { p.classList.add('uses'); p.style.stroke = gradient(k, p, data.nodes[t].group); }
      else p.classList.add('usedby');
    });
    show(i);
  }
  function release() {
    pinned = null;
    current = null;
    clear();
    panel.innerHTML = hint;
    panel.classList.add('idle');
  }
  function select(i, pin) {
    if (pin) pinned = i;
    highlight(i);
  }
  function reveal(i) {
    if (nodes[i].scrollIntoView) nodes[i].scrollIntoView({block: 'center', inline: 'center', behavior: 'auto'});
    nodes[i].focus({preventScroll: true});
  }
  nodes.forEach(function (g, i) {
    g.addEventListener('mouseenter', function () { if (pinned === null) highlight(i); });
    g.addEventListener('mouseleave', function () { if (pinned === null) release(); });
    g.addEventListener('click', function (ev) { ev.stopPropagation(); if (pinned === i) release(); else select(i, true); });
    g.addEventListener('keydown', function (ev) {
      if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); if (pinned === i) release(); else select(i, true); }
    });
    g.addEventListener('focus', function () { if (pinned === null) highlight(i); });
    g.addEventListener('blur', function () { if (pinned === null) release(); });
  });
  showNotation.addEventListener('change', function () { if (current !== null) highlight(current); });
  document.addEventListener('click', function (ev) {
    if (pinned !== null && !ev.target.closest('aside, .tools, #map .node')) release();
  });
  document.addEventListener('keydown', function (ev) { if (ev.key === 'Escape') { release(); if (document.activeElement && document.activeElement.blur) document.activeElement.blur(); } });

  var search = document.getElementById('search');
  function matches(q) {
    var out = [];
    data.nodes.forEach(function (n, i) {  // the best match first: the name itself, then a name starting with it, then the shortest
      var name = n.name.toLowerCase(), at = name.indexOf(q);
      if (q !== '' && at >= 0) out.push([name === q ? 0 : at === 0 ? 1 : 2, name.length, i]);
    });
    out.sort(function (a, b) { return a[0] - b[0] || a[1] - b[1]; });
    return out.map(function (r) { return r[2]; });
  }
  search.addEventListener('input', function () {
    var q = search.value.trim().toLowerCase();
    if (q !== '') release();
    var hits = matches(q);
    nodes.forEach(function (g, i) { g.classList.toggle('match', hits.indexOf(i) >= 0); });
    svg.classList.toggle('search', q !== '');
  });
  search.addEventListener('keydown', function (ev) {
    if (ev.key !== 'Enter') return;
    var hits = matches(search.value.trim().toLowerCase());
    if (!hits.length) return;
    svg.classList.remove('search');
    nodes.forEach(function (g) { g.classList.remove('match'); });
    select(hits[0], true);
    reveal(hits[0]);
  });

  document.getElementById('theme').addEventListener('click', function () {
    var root = document.documentElement;
    var dark = root.getAttribute('data-theme') ? root.getAttribute('data-theme') === 'dark'
      : window.matchMedia('(prefers-color-scheme: dark)').matches;
    root.setAttribute('data-theme', dark ? 'light' : 'dark');
    try { localStorage.setItem('FunctionalDiagrams.theme', dark ? 'light' : 'dark'); } catch (e) {}
  });
})();
</script>
</body>
</html>
'''


def main():
    prepare()
    svg, cv, H = render_svg()
    dup = sorted(a for a, k in Counter(cv.drawn).items() if k > 1)
    missing = [a for a in NAMES if a not in cv.drawn]
    assert not dup and not missing, 'names drawn twice %s, not drawn %s' % (dup, missing)
    drawn, left_out, implied = accounting(cv)
    with open(os.path.join(DOCUMENTATION, 'function_map.svg'), 'w', encoding='utf-8') as out:
        out.write(svg)
    page, cv2 = render_html()
    assert cv2.boxes == cv.boxes, 'the interactive page and the picture have different layouts'
    with open(os.path.join(DOCUMENTATION, 'function_map.html'), 'w', encoding='utf-8') as out:
        out.write(page)
    print('function map: %d names (each drawn once), %d groups; %d dependencies = %d drawn + %d left out (uses of the '
          'vocabulary of stages 1-2 and of inert heads) + %d implied through other names; picture %d x %d'
          % (len(NAMES), len(CATEGORIES), len(EDGES), drawn, left_out, implied, W, H))
    print('worst text contrast: %.2f (%s, %s on %s)' % check_contrast())
    print('stages:', ', '.join('%s %d' % (s, sum(1 for a in NAMES if STAGE[a] == s)) for s, _, _ in STAGES))


if __name__ == '__main__':
    main()
