"""Long formulas (LaTeX written by ExpressionToLaTeX) broken into lines, for the documents: imported by
Showcase/assemble.py (one break, figures/formula_NAME_split.tex) and Documentation/make_index.py (several lines,
examples/NAME_lines.tex)."""
import re


def visible_length(tex):
    """the number of characters that are printed, about: commands, braces, ^, _ and thin spaces not counted"""
    return len(re.sub(r'\\[a-zA-Z]+|[{}^_]|\\,', '', tex))


def sign_positions(tex):
    """the positions of the binary + and - signs (' + ', ' - ') at the top level of the formula, and of those directly
    inside a pair of big parentheses \\left ... \\right at the top level. A \\left or \\right is read with its delimiter,
    ( or \\{ or . or another, so that \\left. ... \\right) is a pair; \\leftarrow and \\rightarrow are other commands"""
    stack, top, inner, i = [], [], [], 0
    while i < len(tex):
        for command in ('\\left', '\\right'):
            end = i + len(command)
            if tex.startswith(command, i) and not (end < len(tex) and tex[end].isalpha()):
                # the command and its delimiter
                i = end + (2 if tex.startswith('\\', end) else 1)
                if command == '\\left':
                    stack.append('left')
                elif stack:
                    stack.pop()
                break
        else:
            c = tex[i]
            if c in '{(':
                stack.append(c)
            elif c in '})':
                if stack:
                    stack.pop()
            elif tex[i:i + 3] in (' + ', ' - ') and i > 0:
                if not stack:
                    top.append(i)
                elif stack == ['left']:
                    inner.append(i)
            i += 1
    return top, inner


def split_formula(tex):
    """the formula with a line break (\\\\) before the top-level + or - nearest to its middle; without one, before a + or -
    directly inside a pair of big parentheses \\left( ... \\right) at the top level, which are closed and opened again"""
    top, inner = sign_positions(tex)
    middle = len(tex) / 2
    if top:
        best = min(top, key=lambda i: abs(i - middle))
        return tex[:best] + r' \\ \qquad' + tex[best:]
    if inner:
        best = min(inner, key=lambda i: abs(i - middle))
        return tex[:best] + r'\right. \\ \qquad\left.' + tex[best:]
    return tex


def broken(tex, width):
    """the formula broken into lines before top-level + and - signs, each line at most about width characters; None if
    it stays on one line"""
    ops = sign_positions(tex)[0]
    if not ops:
        return None
    # the terms, each with its sign; a line is closed before a term that would make it too long
    bounds = [0] + ops + [len(tex)]
    terms = [tex[bounds[k]:bounds[k + 1]] for k in range(len(bounds) - 1)]
    lines, current = [], ''
    for t in terms:
        if current and visible_length(current + t) > width:
            lines.append(current)
            current = t
        else:
            current += t
    lines.append(current)
    if len(lines) == 1:
        return None
    return (r' \\' + '\n' + r'\qquad').join(lines)
