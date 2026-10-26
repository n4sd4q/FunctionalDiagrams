"""Build Examples.nb from Examples.wl (deterministic: the same .wl always gives the same .nb).

Text cells are written in the .wl file as a marker line followed by a one-line comment,
    (* ::Section:: *)
    (*text*)
with the styles Title, Subtitle, Chapter, Section, Subsection, Subsubsection, Text, Item, Subitem, DisplayFormula.
Code blocks are separated by blank lines; every code block becomes one Input cell, comments included
(the comment at its end gives the expected result).

In Text and Item cells, names of functions (CamelCase words with at least two capitals, with the brackets that follow
them) and options ("Name" -> value) are set in a code font; ψ̄ and η̄ (a letter followed by a combining macron) are
written with a bar; other non-ASCII characters are written as \\:XXXX escapes, so that the notebook file is plain ASCII.
usage: python3 make_notebook.py Examples.wl Examples.nb
"""
import re
import sys

TEXT_STYLES = {'Title', 'Subtitle', 'Chapter', 'Section', 'Subsection', 'Subsubsection', 'Text', 'Item', 'Subitem',
               'DisplayFormula'}
FORMATTED_STYLES = {'Text', 'Item', 'Subitem'}
CODE_FONT = 'FontFamily -> "Source Code Pro", FontWeight -> "Bold", FontColor -> RGBColor[0.15, 0.2, 0.45]'


NAMED = {'\u27e8': '\\[LeftAngleBracket]', '\u27e9': '\\[RightAngleBracket]'}  # the brackets of the Mathematica fonts


def esc(t):
    """A Wolfram Language string literal (without the quotes): ASCII only."""
    out = []
    for ch in t.replace('\\', '\\\\').replace('"', '\\"'):
        out.append(ch if ord(ch) < 128 else NAMED.get(ch, '\\:%04x' % ord(ch)))
    return ''.join(out)


def balanced_end(t, i):
    """End of the bracket group that starts at t[i] == '[' (or i itself if there is none)."""
    if i >= len(t) or t[i] != '[':
        return i
    depth = 0
    for j in range(i, len(t)):
        depth += {'[': 1, ']': -1}.get(t[j], 0)
        if depth == 0:
            return j + 1
    return i


NAME = re.compile(r'\??\$?[A-Z][a-z0-9]+[A-Z][A-Za-z0-9]*')
NOT_NAMES = {'LaTeX', 'TikZ'}
CODE = [re.compile(r'"[A-Za-z]+" -> (?:"[A-Za-z]+"|\{[^{}]*\}|[A-Za-z0-9.]+)'),  # an option
        re.compile(r'"[A-Za-z]+"'),  # a string
        re.compile(r'(?:[A-Za-z]+/)*[A-Za-z_]+\.(?:pdf|wl|nb|tex)\b'),  # a file
        re.compile(r'wolfram -script [\w.]*\w'),  # a command
        re.compile(r'\?[A-Z][A-Za-z]*\b')]  # ?Name
BARRED = re.compile('(ph|.)\u0304')  # a bar over a letter (over ph: the transverse channel)


def text_pieces(t):
    """Split a text into ('plain' | 'code' | 'bar', string) pieces."""
    pieces, i, plain = [], 0, ''
    while i < len(t):
        end = None
        if i == 0 or not (t[i - 1].isalnum() or t[i - 1] in '$`_/'):
            for pattern in CODE:
                m = pattern.match(t, i)
                if m:
                    end = m.end()
                    break
            n = NAME.match(t, i)
            if end is None and n and n.group().lstrip('?$') not in NOT_NAMES:
                end = balanced_end(t, n.end())
        if end is None:
            b = BARRED.match(t, i)
            if b:
                pieces.append(('plain', plain))
                pieces.append(('bar', b.group(1)))
                plain, i = '', b.end()
            else:
                plain += t[i]
                i += 1
            continue
        pieces.append(('plain', plain))
        pieces.append(('code', t[i:end]))
        plain, i = '', end
    pieces.append(('plain', plain))
    return [p for p in pieces if p[1]]


def split_symbols(s):
    """Split a string into runs of ASCII text and single words that contain a non-ASCII character: the front end trims
    the spaces around a string that contains such a character (it is laid out as a formula), so these words are
    strings of their own, and the spaces stay in the ASCII strings."""
    out, cur = [], ''
    for m in re.finditer(r'\S+|\s+', s):
        if not m.group().isspace() and any(ord(ch) >= 128 for ch in m.group()):
            out.extend([cur, m.group()] if cur else [m.group()])
            cur = ''
        else:
            cur += m.group()
    return out + ([cur] if cur else [])


def text_cell(t, style):
    pieces = text_pieces(t) if style in FORMATTED_STYLES else [('plain', t)]
    options = ', TextAlignment -> Center' if style == 'DisplayFormula' else ''
    style = 'Text' if style == 'DisplayFormula' else style  # a centred line of text
    if all(kind == 'plain' for kind, _ in pieces) and len(split_symbols(t)) == 1:
        return 'Cell["%s", "%s"%s]' % (esc(t), style, options)
    items = []
    for kind, s in pieces:
        if kind == 'plain':
            items.extend('"%s"' % esc(w) for w in split_symbols(s))
        elif kind == 'code':
            items.append('StyleBox["%s", %s]' % (esc(s), CODE_FONT))
        else:
            items.append('Cell[BoxData[OverscriptBox["%s", "_"]]]' % esc(s))
    return 'Cell[TextData[{%s}], "%s"%s]' % (', '.join(items), style, options)


src = open(sys.argv[1], encoding='utf-8').read().split('\n')
cells = []
block = []


def flush():
    global block
    while block and block[-1].strip() == '':
        block.pop()
    while block and block[0].strip() == '':
        block.pop(0)
    if block:
        code = '\n'.join(block)
        assert all(ord(ch) < 128 for ch in code), 'non-ASCII character in a code block: ' + code[:60]
        cells.append(('Input', code))
    block = []


i = 0
while i < len(src):
    line = src[i]
    m = re.match(r'\(\* ::(\w+):: \*\)$', line.strip())
    if m:
        flush()
        kind = m.group(1)
        if kind == 'Package':
            i += 1
            continue
        assert kind in TEXT_STYLES, 'unknown cell style ' + kind
        txt = src[i + 1].strip()
        assert txt.startswith('(*') and txt.endswith('*)'), txt
        cells.append((kind, txt[2:-2]))
        i += 2
        continue
    if line.strip() == '':
        flush()
    else:
        block.append(line)
    i += 1
flush()

# the front end groups the cells under the sections by itself (CellGrouping -> Automatic)
items = []
for kind, t in cells:
    if kind == 'Input':
        items.append('Cell["%s", "Input"]' % esc(t))
    else:
        items.append(text_cell(t, kind))
title = next((t for kind, t in cells if kind == 'Title'), 'Examples')
out = ['(* Content-type: application/vnd.wolfram.mathematica *)', '', 'Notebook[{', ',\n'.join(items),
       '}, WindowSize -> {1000, 850}, WindowTitle -> "%s", StyleDefinitions -> "Default.nb"]' % esc(title + ': a guided tour')]
open(sys.argv[2], 'w', encoding='utf-8').write('\n'.join(out) + '\n')
print(len(cells), 'cells')
