"""The code blocks of the showcase and what the page shows after each of them, for Development/check_showcase.wl.

    python3 Development/showcase_blocks.py [Showcase/FunctionalDiagrams_Showcase.tex]

prints a JSON list (showcase_blocks(tex_file) gives it as a Python list, for Development/check_consistency.py), one entry
per \\begin{code} ... \\end{code} block, in the order of the document: {"line": first line of the code, "code": the code,
"run": false when the line before \\begin{code} is the comment "% not run" (or "% not run: why"), "items": what follows
the block up to the next one, "boxes": the LaTeX of the output boxes (\\begin{outputbox} ... \\end{outputbox}) that
follow it}. The items, in the order of the page, LaTeX comments left out:
  {"kind": "sum", "rows": [{"label": ..., "name": ...}, ...], "line": n}   \\begin{ordersum}{lhs} \\orderrow{label}{name}
                                                                          ... \\end{ordersum}, one row per band
  {"kind": "sum", "rows": [{"label": null, "name": name}], "line": n}     \\figuresum[...]{lhs}{name}
  {"kind": "figure", "name": name, "line": n}                            \\inputfigure{name} elsewhere (not in a
                                                                          left-hand side)
  {"kind": "formula", "name": name, "line": n}                           \\formula, \\formulasplit, \\formulalines
  {"kind": "value", "key": key, "line": n}                               \\val{key}
Standard library only."""
import json, os, re, sys


def without_comment(line):
    # a LaTeX comment starts at a % that is not escaped
    m = re.search(r'(?<!\\)%', line)
    return line[:m.start()] if m else line


def argument(text, i):
    """The braced argument that starts at text[i] == '{': (its content, the index after it)."""
    assert text[i] == '{', text[i:i + 30]
    depth = 0
    for j in range(i, len(text)):
        if text[j] == '{':
            depth += 1
        elif text[j] == '}':
            depth -= 1
            if depth == 0:
                return text[i + 1:j], j + 1
    raise ValueError('unbalanced braces: ' + text[i:i + 60])


def skip_optional(text, i):
    """Skip spaces and an optional argument [...] at text[i]."""
    while i < len(text) and text[i] in ' \t\n':
        i += 1
    if i < len(text) and text[i] == '[':
        depth = 0
        for j in range(i, len(text)):
            if text[j] == '[':
                depth += 1
            elif text[j] == ']':
                depth -= 1
                if depth == 0:
                    return j + 1
    return i


def next_brace(text, i):
    while text[i] in ' \t\n':
        i += 1
    return i


def items_of(text, first_line):
    """The items of a stretch of LaTeX (comments removed) that starts at the line first_line."""
    items, i = [], 0
    line_at = lambda k: first_line + text.count('\n', 0, k)
    pattern = re.compile(r'\\(begin\{ordersum\}|figuresum|orderrow|formulasplit|formulalines|formula|val|inputfigure)(?![A-Za-z])')
    current = None  # the ordersum being read
    while True:
        m = pattern.search(text, i)
        if not m:
            break
        command, i = m.group(1), m.end()
        if command == 'begin{ordersum}':
            _, i = argument(text, next_brace(text, i))  # the left-hand side: its pictures are not output
            current = {'kind': 'sum', 'rows': [], 'line': line_at(m.start())}
            end = text.find(r'\end{ordersum}', i)
            body, after = text[i:end], end + len(r'\end{ordersum}')
            for r in re.finditer(r'\\orderrow(?![A-Za-z])', body):
                label, k = argument(body, next_brace(body, r.end()))
                name, _ = argument(body, next_brace(body, k))
                current['rows'].append({'label': label.strip(), 'name': name.strip()})
            items.append(current)
            i = after
        elif command == 'figuresum':
            i = skip_optional(text, i)
            _, i = argument(text, next_brace(text, i))
            name, i = argument(text, next_brace(text, i))
            items.append({'kind': 'sum', 'rows': [{'label': None, 'name': name.strip()}], 'line': line_at(m.start())})
        elif command == 'orderrow':
            raise ValueError('\\orderrow outside an ordersum at line %d' % line_at(m.start()))
        elif command in ('formula', 'formulasplit', 'formulalines'):
            name, i = argument(text, next_brace(text, i))
            items.append({'kind': 'formula', 'name': name.strip(), 'line': line_at(m.start())})
        elif command == 'val':
            key, i = argument(text, next_brace(text, i))
            items.append({'kind': 'value', 'key': key.strip(), 'line': line_at(m.start())})
        elif command == 'inputfigure':
            name, i = argument(text, next_brace(text, i))
            items.append({'kind': 'figure', 'name': name.strip(), 'line': line_at(m.start())})
    return items


def showcase_blocks(tex_file):
    """The blocks of a .tex file, as described above; a ValueError when its LaTeX cannot be read."""
    lines = open(tex_file, encoding='utf-8').read().split('\n')
    blocks, i = [], 0
    while i < len(lines):
        if lines[i].strip().startswith(r'\begin{code}'):
            start = i
            previous = lines[i - 1].strip() if i > 0 else ''
            run = re.fullmatch(r'%\s*not run(:.*)?', previous) is None
            j = i + 1
            while j < len(lines) and not lines[j].strip().startswith(r'\end{code}'):
                j += 1
            if j == len(lines):
                raise ValueError('\\begin{code} at line %d is not closed' % (start + 1))
            blocks.append({'line': start + 2, 'code': '\n'.join(lines[start + 1:j]), 'run': run, 'end': j})
            i = j + 1
        else:
            i += 1
    for k, b in enumerate(blocks):
        stop = blocks[k + 1]['line'] - 2 if k + 1 < len(blocks) else len(lines)
        stretch = '\n'.join(without_comment(line) for line in lines[b['end'] + 1:stop])
        b['items'] = items_of(stretch, b['end'] + 2)
        b['boxes'] = re.findall(r'\\begin\{outputbox\}(.*?)\\end\{outputbox\}', stretch, re.S)
        del b['end']
    return blocks


if __name__ == '__main__':
    tex_file = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'Showcase',
                                                                    'FunctionalDiagrams_Showcase.tex')
    json.dump(showcase_blocks(tex_file), sys.stdout, ensure_ascii=False, indent=0)
