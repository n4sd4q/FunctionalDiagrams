#!/usr/bin/env python3
"""The demo video of the package: Media/demo.mp4 (1920 x 1080, 60 fps, H.264), its looping preview for the README,
Media/demo_preview.webp (960 x 540, 60 fps; its first frame, the one shown where animated images do not play, is a card
in full view), and a poster frame, Media/demo_poster.png.

Nothing in it is drawn by hand: the diagrams and formulas are the figures of the showcase (Showcase/figures, computed
by Showcase/make_figures.wl; run it first), redrawn with one unit of length as in make_readme_figure.sh, and the code
typed in the notebook is the first example of README.md. The pictures go through pdflatex and pdftocairo (one PNG per
element, transparent), the frames are put together with PIL (at 1920 x 1080; the preview is reduced from them) and
encoded by ffmpeg. The logo of the first and the last card is drawn again in every frame from the geometry of
Development/make_logo.py, its wavy line rippling, its loop breathing and turning once (the arrow heads travel around it,
with a soft start, a glide and a slight overshoot before they settle on their rest positions). Needs pdflatex (TeX Live:
mathpazo, Source Code Pro, varwidth), pdftocairo (poppler), python3 with PIL, ffmpeg with libx264. Run from any folder:
    python3 Development/make_demo_video.py            (all three files, about four minutes: most of it the WebP)
    python3 Development/make_demo_video.py --frames   (also a few frames as PNG in build/video/frames, to look at;
                                                      with --only-frames, nothing else)
"""
import concurrent.futures
import math
import multiprocessing
import os
import re
import shutil
import subprocess
import sys

from PIL import Image, ImageChops, ImageDraw, ImageFont

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from latex_lines import sign_positions      # noqa: E402  (Development/latex_lines.py, shared with the documents)

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FIGURES = os.path.join(ROOT, "Showcase", "figures")
BUILD = os.path.join(ROOT, "build", "video")
MEDIA = os.path.join(ROOT, "Media")
W, H, FPS = 1920, 1080, 60
# Every length in pixels below (fonts, margins, gaps, line widths, arrows, brackets) is written for a frame 1280 px wide
# and scaled by S, so that a frame of another size is the same picture, only sharper
S = W / 1280


def px(v):
    """A length in pixels of a frame 1280 px wide, in pixels of this frame."""
    return v * S


def ipx(v):
    """The same, in whole pixels."""
    return int(math.floor(v * S + 0.5))


# The preview: an animated WebP, which can run at 60 fps (a GIF cannot: its delays are in hundredths of a second, so
# 50 fps at most, and browsers slow down delays below 0.02 s), in full colour; GitHub shows it in the README as a plain
# <img>, and it plays wherever WebP does (every current browser, the GitHub apps)
PREVIEW_W, PREVIEW_H, PREVIEW_FPS, PREVIEW_QUALITY = 960, 540, 60, 90
# the LaTeX pages are drawn for a frame 16 cm wide: 80 px to the centimetre
DPI = W / (16 / 2.54)

ACCENT = (31, 78, 121)
MUTED = (89, 99, 110)
CODE_BG = (245, 247, 250)
CODE_FRAME = (201, 211, 223)
WHITE = (255, 255, 255)
# Mathematica's colours in an input cell: comments and strings grey, undefined symbols blue, the rest black
CODE_COLORS = {"comment": (140, 140, 140), "string": (110, 110, 110), "symbol": (36, 74, 170), "plain": (0, 0, 0)}
LABEL = (70, 96, 140)        # In[n]:= and Out[n]=
BRACKET = (150, 172, 200)    # the cell brackets on the right


# ---------------------------------------------------------------------------------------------------------------------
# the pictures: LaTeX elements, one page each

BLOB_EM = 0.6   # the font of a label in a blob, in units of length ($blobLabelEm of the package)
# an arrow head: \fill (tip) -- (corner) -- (notch) -- (corner) -- cycle;
ARROW_HEAD = re.compile(r"\\fill \((-?[0-9.]+),(-?[0-9.]+)\) -- \((-?[0-9.]+),(-?[0-9.]+)\) -- \((-?[0-9.]+),(-?[0-9.]+)\) -- "
                        r"\((-?[0-9.]+),(-?[0-9.]+)\) -- cycle;")
# the length of an arrow head in the plain diagrams of the showcase (drawn with the unit 0.34 cm), in units
PLAIN_ARROW = 0.238
PARQUET_UNIT = 0.51


def longer_arrow_head(m, length):
    """An arrow head made at least `length` long (tip to the middle of its base), about its middle."""
    p = [float(v) for v in m.groups()]
    tip, base = (p[0], p[1]), ((p[2] + p[6]) / 2, (p[3] + p[7]) / 2)
    now = math.dist(tip, base)
    if now <= 0 or now >= length:
        return m.group(0)
    s, cx, cy = length / now, (tip[0] + base[0]) / 2, (tip[1] + base[1]) / 2
    q = ["(%.3f,%.3f)" % (cx + s * (p[i] - cx), cy + s * (p[i + 1] - cy)) for i in range(0, 8, 2)]
    return r"\fill " + " -- ".join(q) + " -- cycle;"


def scaled_figure(name, unit, line=0.62, font=r"\scriptsize", arrow=None):
    """Copy Showcase/figures/name.tex to build/video/figures with the unit of length `unit` (cm), the base line width
    `line` (pt; the other widths and the wavy lines scaled with it) and the labels in `font`; returns the \\input.
    The showcase draws its arrow heads of one size in centimetres, whatever the unit of a picture, so that those of the
    pictures with blobs (drawn larger) are shorter in units; with `arrow` (a length in units) they are made at least
    that long: PLAIN_ARROW gives every picture of a card the arrow heads of its plain diagrams."""
    path = os.path.join(FIGURES, name + ".tex")
    if not os.path.exists(path):
        sys.exit("missing %s: run Showcase/make_figures.wl first" % path)
    out, f, g = [], 1.0, 1.0
    for text in open(path, encoding="utf-8").read().splitlines():
        if arrow:
            text = ARROW_HEAD.sub(lambda m: longer_arrow_head(m, arrow), text)
        m = re.search(r"x=([0-9.]+)cm, y=([0-9.]+)cm, line width=([0-9.]+)pt", text)
        if m:
            f, g = unit / float(m.group(1)), line / float(m.group(3))
            text = text.replace(m.group(0), "x=%.3fcm, y=%.3fcm, line width=%.3fpt" % (unit, unit, line))
        else:
            text = re.sub(r"line width=([0-9.]+)pt", lambda m: "line width=%.3fpt" % (float(m.group(1)) * g), text)
        text = re.sub(r"(amplitude|segment length)=([0-9.]+)mm", lambda m: "%s=%.3fmm" % (m.group(1), float(m.group(2)) * f), text)
        if r"\node" in text and "inner sep=0pt" not in text:
            # the name written inside a blob: scaled with the blob, as large in it as the package sizes blobs for
            # (0.6 units of length, a little larger than in the showcase, whose sizes are those of an 11pt document), so
            # that it fits with a band of hatching around it
            text = re.sub(r"font=\\(tiny|scriptsize|footnotesize|small)\b", lambda m: "font=\\fontsize{%.2f}{%.2f}\\selectfont" % (
                (BLOB_EM * unit * 72.27 / 2.54,) * 2), text)
            text = re.sub(r"(inner sep|rounded corners)=([0-9.]+)pt", lambda m: "%s=%.2fpt" % (m.group(1), float(m.group(2)) * f), text)
        text = re.sub(r"font=\\(tiny|scriptsize|footnotesize|small)\b", lambda m: "font=" + font, text)
        out.append(text)
    tag = "%s_%d" % (name, round(unit * 1000))
    with open(os.path.join(BUILD, "figures", tag + ".tex"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(out) + "\n")
    return r"\input{figures/%s.tex}" % tag


def formula(name):
    return open(os.path.join(FIGURES, "formula_" + name + ".tex"), encoding="utf-8").read().strip()


PREAMBLE = r"""\documentclass[12pt,multi=elem,border=3pt]{standalone}
\usepackage[T1]{fontenc}
\usepackage[sc]{mathpazo}
\usepackage[scale=0.9]{sourcecodepro}
\usepackage{amsmath,slashed,tikz,xcolor,graphicx,varwidth}
\usetikzlibrary{decorations.pathmorphing}
\definecolor{accent}{HTML}{1F4E79}\definecolor{muted}{HTML}{59636E}\definecolor{comment}{HTML}{6A7B8C}
\definecolor{codebg}{HTML}{F5F7FA}\definecolor{codeframe}{HTML}{C9D3DF}
\definecolor{bandA}{HTML}{EEF3F9}\definecolor{bandB}{HTML}{F7F9FC}
\newcommand{\diagramgap}{\hskip 0.9em\relax}
\newcommand{\channelmark}[1]{\par\mbox{\footnotesize\color{comment}(#1)}\diagramgap}
\newcommand{\figlines}{\lineskiplimit=9pt\lineskip=9pt\relax}
\newcommand{\rest}{\unskip\nobreak\diagramgap\mbox{${}+\cdots$}}
% a row "lhs = diagrams": the left-hand side in a box of fixed width, the diagrams as a ragged paragraph
% (a wider left-hand side keeps its width: make_demo_video.py then warns that the rows are not aligned)
\newlength{\lhsnatural}
\newcommand{\eqrow}[4]{\settowidth{\lhsnatural}{#2}\ifdim\lhsnatural>#1\relax\typeout{WIDE-LHS}#2\else\makebox[#1][r]{#2}\fi
  \begin{varwidth}[t]{#3}\raggedright\figlines #4\end{varwidth}}
% a band of a sum written order by order, as in the showcase
\newlength{\orderlabel}\setlength{\orderlabel}{1.0cm}
\newcommand{\band}[4]{\colorbox{#1}{\parbox{#2}{\raggedright\figlines\hangindent=\orderlabel\hangafter=1\noindent
  \makebox[\orderlabel][l]{\footnotesize\color{accent}$#3$}#4\par}}}
% a name of the package, as in a notebook
\newcommand{\chip}[1]{\tikz[baseline=(c.base)]\node[fill=codebg, draw=codeframe, line width=0.5pt, rounded corners=2pt,
  inner xsep=5pt, inner ysep=3pt, font=\ttfamily\small] (c) {#1};}
\begin{document}
"""


def elements():
    """name -> LaTeX of every picture of the video."""
    nb = dict(unit=0.31, line=0.5, font=r"\tiny")      # the outputs in the notebook
    # the cards: their arrow heads as long as in the plain diagrams, so that the fermion lines read at a glance
    cd = dict(unit=0.52, line=0.70, font=r"\footnotesize", arrow=PLAIN_ARROW)
    fig = scaled_figure
    big = r"\fontsize{15}{19}\selectfont"
    e = {}
    # title and end card
    e["logo"] = r"\includegraphics[height=2.6cm]{logo.pdf}"
    e["name"] = r"{\color{accent}\fontsize{30}{36}\selectfont\bfseries FunctionalDiagrams}"
    e["tagline"] = r"{\fontsize{16}{20}\selectfont Feynman diagrams from functional derivatives}"
    e["about"] = r"{\color{muted}\fontsize{12}{15}\selectfont a Mathematica package for perturbation theory, effective actions and many-body physics}"
    e["url"] = r"{\color{accent}\fontsize{15}{19}\selectfont\ttfamily github.com/n4sd4q/FunctionalDiagrams}"
    # the outputs of the first example of README.md
    w = "13.4cm"
    e["nb_green"] = (r"\begin{tabular}{@{}l@{}}" + r"\band{bandA}{%s}{V^0}{%s}\\[-1pt]" % (w, fig("green_function_0", **nb)) +
                     r"\band{bandB}{%s}{V}{%s}\\[-1pt]" % (w, fig("green_function_1", **nb)) +
                     r"\band{bandA}{%s}{V^2}{%s}" % (w, fig("green_function_2", **nb)) + r"\end{tabular}")
    lhs, rhs = "2.2cm", "11.4cm"
    small = r"\fontsize{9}{11}\selectfont\renewcommand{\channelmark}[1]{}\def\break{\diagramgap}"
    # the last cell has two outputs, Sigma and Gamma0 to third order (Out[4] and Out[5]): shown, like the README, up to
    # second order, the rest as +...
    e["nb_sigma"] = r"{%s\eqrow{%s}{$\Sigma=\dfrac{\delta\Phi}{\delta G}={}$}{%s}{%s\diagramgap%s\rest}}" % (
        small, lhs, rhs, fig("self_energy_1", **nb), fig("self_energy_2", **nb))
    e["nb_gamma"] = r"{%s\eqrow{%s}{$\Gamma^0=\dfrac{\delta\Sigma}{\delta G}={}$}{%s}{%s\diagramgap%s\rest}}" % (
        small, lhs, rhs, fig("irreducible_vertex_1", **nb), fig("irreducible_vertex_2", **nb))
    # the same, larger, for the preview
    lhs, rhs, cd, big = "2.7cm", "12.5cm", dict(cd, unit=0.40), r"\fontsize{13.5}{17}\selectfont"
    e["card_phi"] = r"{%s\eqrow{%s}{$\Phi[G]={}$}{%s}{%s\diagramgap%s\rest}}" % (big, lhs, rhs, fig("luttinger_ward_1", **cd), fig("luttinger_ward_2", **cd))
    e["card_sigma"] = r"{%s\eqrow{%s}{$\Sigma=\dfrac{\delta\Phi}{\delta G}={}$}{%s}{%s\diagramgap%s\rest}}" % (
        big, lhs, rhs, fig("self_energy_1", **cd), fig("self_energy_2", **cd))
    e["card_gamma"] = r"{%s\eqrow{%s}{$\Gamma^0=\dfrac{\delta\Sigma}{\delta G}={}$}{%s}{%s\rest}}" % (
        big, lhs, rhs, fig("irreducible_vertex_1", **cd))
    # exact equations
    big = r"\fontsize{15}{19}\selectfont"
    lhs, rhs, ex = "2.7cm", "12.2cm", dict(cd, unit=0.40)
    eq = lambda left, right: r"{%s\eqrow{%s}{%s\,${}={}$}{%s}{%s}}" % (big, lhs, fig(left, **ex), rhs, fig(right, **ex))
    # two particles, for the electron gas: the exact equations (blobs, fermion legs) and their first diagrams
    bs = dict(cd, unit=0.47, line=0.8)
    # the exact equation, with blobs, a little larger: the names in the blobs as large as the parquet card's
    bsq, bslhs = dict(bs, unit=0.50), "2.9cm"
    e["eq_bethe_salpeter"] = r"{%s\eqrow{%s}{%s\,${}={}$}{%s}{%s}}" % (big, bslhs, fig("exact_vertex_lhs", **bsq), rhs, fig("exact_bethe_salpeter", **bsq))
    row = lambda left, name, more=True: r"{%s\eqrow{%s}{$%s={}$}{%s}{%s%s}}" % (
        big, bslhs, left, rhs, fig(name, **bs), r"\rest" if more else "")
    e["bs_rpa"] = row(r"F_{\rm chain}", "rpa_chain")
    e["bs_ladder"] = row(r"T^{(2)}", "pp_ladder", more=False)
    # the parquet equation alone, drawn larger, so that the names in its blobs read (12 px and more): its four terms
    # on two lines
    pq = dict(cd, unit=PARQUET_UNIT)
    e["eq_parquet"] = r"{%s\eqrow{%s}{%s\,${}={}$}{%s}{%s}}" % (big, "3.3cm", fig("exact_vertex_lhs", **pq), rhs, fig("exact_parquet", **pq))
    # Hedin: four rows, the last one (the vertex, an equilateral triangle with its legs as long as those of the bare
    # vertex) tall: drawn a little smaller, so that its x2 keeps clear of the bottom band
    lhs, ex = "2.7cm", dict(cd, unit=0.43)
    e["hedin_sigma"] = r"{%s\eqrow{%s}{$\Sigma={}$}{%s}{%s}}" % (big, lhs, rhs, fig("hedin_sigma", **ex))
    for name in ("w", "p", "lambda"):
        e["hedin_" + name] = eq("hedin_%s_lhs" % name, "hedin_" + name)
    # Matsubara
    e["lindhard_diagram"] = r"{%s$\langle\rho\rho\rangle_c(q,i\nu_m)={}$\,%s}" % (big, fig("density_correlation_0", unit=0.8, line=0.8, font=r"\footnotesize"))
    e["lindhard_formula"] = r"{%s$\displaystyle {}={}%s$}" % (big, formula("lindhard"))
    # QED
    qd = dict(unit=0.45, line=0.75, font=r"\footnotesize", arrow=0.2)
    qbig = r"\fontsize{12}{15}\selectfont"
    e["qed_sigma"] = r"{%s$-i\Sigma={}$\,%s}" % (qbig, fig("qed_self_energy", **qd))
    e["qed_pi"] = r"{%s$i\Pi^{\mu\nu}={}$\,%s}" % (qbig, fig("qed_vacuum_polarization", **qd))
    e["qed_vertex"] = r"{%s$ie\,\delta\Gamma^{\mu}={}$\,%s}" % (qbig, fig("qed_vertex", **qd))
    # what the package computes from each of them (QEDIntegrand, IntegrateOneLoop, VertexFormFactors; the showcase's
    # section on QED, Tests/QEDOneLoop.wl): the pole of Sigma, the transverse Pi with its logarithm, F2(0)
    # (without the thin spaces of the package's TeX between factors, so that the long ones fit beside the diagrams)
    res = r"\fontsize{10}{13}\selectfont"
    compact = lambda name: formula(name).replace(r"\,", "")
    note = lambda text: r"\\[1pt]{\color{muted}\fontsize{8.5}{10}\selectfont %s}" % text
    result = lambda body: r"{%s\begin{tabular}{@{}l@{}}%s\end{tabular}}" % (res, body)
    e["qed_sigma_result"] = result(r"$\displaystyle\Sigma=%s+O(\varepsilon^0)$" % compact("qed_sigma_pole") +
                                   note(r"its pole, in any covariant gauge $\xi$"))
    pi = compact("qed_pi")
    cut = sign_positions(pi)[0][0]      # the pole, then the logarithm on a second line
    e["qed_pi_result"] = result(r"$\displaystyle\Pi^{\mu\nu}=(q^2g^{\mu\nu}-q^\mu q^\nu)\,\Pi_2(q^2),\quad\Pi_2=%s$\\"
                                r"$\displaystyle\qquad%s$" % (pi[:cut], pi[cut:]) + note(r"transverse, and the same in every gauge"))
    e["qed_vertex_result"] = result(r"$\displaystyle F_2(0)=%s,\qquad g=2+\frac{\alpha}{\pi}$" % compact("qed_F2_zero") +
                                    note("the anomalous magnetic moment of the electron"))
    # TikZ
    e["tikz_picture"] = r"{%s$\Sigma^{(1)}(x_1,x_2)=%s$}" % (big, fig("tikz_example", unit=0.62, line=0.75, font=r"\footnotesize"))
    # headings of the cards, their second lines and the names of the package they show
    for key, text in HEADINGS.items():
        e["h_" + key] = r"{\color{accent}\fontsize{19}{23}\selectfont\bfseries %s}" % text[0]
        if text[1]:
            e["s_" + key] = r"{\color{muted}\fontsize{13}{16}\selectfont %s}" % text[1]
        e["c_" + key] = r"\hbox{" + r"\hskip 6pt".join(r"\chip{%s}" % c for c in text[2]) + "}"
    return e


HEADINGS = {
    "derivatives": ("Diagrams from functional derivatives",
                    "", ["LuttingerWardFunctional", "FunctionalDerivative"]),
    "twoparticle": ("Two particles: the Bethe--Salpeter equation",
                    "the kernel $\\Gamma^0=\\delta\\Sigma/\\delta G$; the bubble chain of the RPA, the $T$-matrix",
                    ["ExactEquation", "BetheSalpeterLadder"]),
    "parquet": ("The parquet equations",
                "$F=\\Gamma_{\\rm irr}+R_{\\rm ph}+R_{\\overline{\\rm ph}}+R_{\\rm pp}$, checked order by order",
                ["ParquetDecomposition", "CheckParquetEquations"]),
    "hedin": ("Hedin's equations", "self-energy, screened interaction, polarization and vertex",
              ["ExactEquation[\"Hedin\", \\dots]"]),
    "matsubara": ("Momentum and Matsubara frequency", "the free density correlation: minus the Lindhard function",
                  ["EvaluateMatsubara"]),
    # no second line: the three diagrams below say it, and their results need the room
    "qed": ("QED at one loop", "",
            ["QEDIntegrand", "IntegrateOneLoop", "VertexFormFactors"]),
    "tikz": ("TikZ export, ready for a paper", "", ["DiagramToTikZ"]),
}


def render_elements():
    """Typeset every element and return name -> RGBA image (transparent, 80 px to the cm)."""
    os.makedirs(os.path.join(BUILD, "figures"), exist_ok=True)
    shutil.copy(os.path.join(ROOT, "Logo", "FunctionalDiagrams_logo.pdf"), os.path.join(BUILD, "logo.pdf"))
    els = elements()
    names = list(els)
    with open(os.path.join(BUILD, "elements.tex"), "w", encoding="utf-8") as fh:
        fh.write(PREAMBLE)
        for name in names:
            fh.write("%% %s\n\\begin{elem}%s\\end{elem}\n" % (name, els[name]))
        fh.write("\\end{document}\n")
    run = subprocess.run(["pdflatex", "-interaction=nonstopmode", "-halt-on-error", "elements.tex"], cwd=BUILD,
                         capture_output=True, text=True)
    if run.returncode:
        sys.exit("pdflatex failed:\n" + run.stdout[-3000:])
    pngs = os.path.join(BUILD, "png")
    os.makedirs(pngs, exist_ok=True)
    images = {}

    def page(i, name):
        subprocess.run(["pdftocairo", "-png", "-transp", "-r", "%.4f" % DPI, "-f", str(i), "-l", str(i), "-singlefile",
                        os.path.join(BUILD, "elements.pdf"), os.path.join(pngs, name)], check=True)

    # one pdftocairo per page, several at a time
    with concurrent.futures.ThreadPoolExecutor(max_workers=os.cpu_count() or 1) as pool:
        list(pool.map(page, range(1, len(names) + 1), names))
    for name in names:
        target = os.path.join(pngs, name)
        images[name] = Image.open(target + ".png").convert("RGBA")
        box = images[name].getchannel("A").getbbox()
        if box and (box[0] == 0 or box[1] == 0 or box[2] == images[name].width or box[3] == images[name].height):
            print("warning: element %s is cut at the edge of its page" % name)
    if "WIDE-LHS" in run.stdout:
        print("warning: a left-hand side is wider than its box (rows not aligned)")
    return images


# ---------------------------------------------------------------------------------------------------------------------
# code, typed as in a notebook

def font(name, size):
    path = subprocess.run(["kpsewhich", name], capture_output=True, text=True).stdout.strip()
    if not path:
        sys.exit("font %s not found (TeX Live package sourcecodepro)" % name)
    return ImageFont.truetype(path, size)


def readme_cells():
    """The first example of README.md, one cell per paragraph."""
    text = open(os.path.join(ROOT, "README.md"), encoding="utf-8").read()
    block = re.search(r"```mathematica\n(.*?)```", text, re.S).group(1)
    return [cell.strip("\n") for cell in re.split(r"\n\s*\n", block) if cell.strip()]


TOKEN = re.compile(r"\(\*.*?\*\)|\"(?:[^\"\\]|\\.)*\"|[A-Za-z$][A-Za-z0-9$`]*|\d+|.", re.S)


def tokens(code):
    """(text, colour) of the code: comments, strings, functions (an upper-case name before '['), other symbols."""
    out = []
    for m in TOKEN.finditer(code):
        t = m.group(0)
        if t.startswith("(*"):
            kind = "comment"
        elif t.startswith('"'):
            kind = "string"
        elif re.match(r"[A-Za-z$]", t):
            after = code[m.end():m.end() + 1]
            kind = "plain" if (t[0].isupper() and after == "[" and len(t) > 2) else "symbol"
        else:
            kind = "plain"
        out.append((t, CODE_COLORS[kind]))
    return out


class CodeCell:
    def __init__(self, code, fnt, x, y, line_height):
        self.code, self.font, self.x, self.y, self.lh = code, fnt, x, y, line_height
        self.toks = tokens(code)
        self.advance = fnt.getlength("M")
        self.lines = code.count("\n") + 1
        self.height = self.lines * line_height

    def draw(self, draw, n, dy, cursor=False):
        """Draw the first n characters at a vertical offset dy; with the cursor after them."""
        col, row, left = 0, 0, n
        for text, colour in self.toks:
            if left <= 0:
                break
            piece = text[:left]
            left -= len(piece)
            parts = piece.split("\n")
            for k, part in enumerate(parts):
                if k:
                    row, col = row + 1, 0
                y = self.y + row * self.lh - dy
                if part.strip() and -self.lh < y < H:
                    draw.text((self.x + col * self.advance, y), part, font=self.font, fill=colour)
                col += len(part)
        if cursor:
            cx, cy = self.x + col * self.advance + px(1), self.y + row * self.lh - dy
            draw.rectangle([cx, cy - px(1), cx + ipx(2) - 1, cy + self.lh - px(7)], fill=ACCENT)


# ---------------------------------------------------------------------------------------------------------------------
# frames

def ease(u):
    u = min(max(u, 0.0), 1.0)
    return u * u * (3 - 2 * u)


def ease_out(u):
    """Quick at first, then settling: the fade-in of the elements."""
    u = min(max(u, 0.0), 1.0)
    return 1 - (1 - u) ** 3


def ease_out_back(u, overshoot=1.6):
    """Like ease_out, a little beyond 1 before settling: the logo popping in."""
    u = min(max(u, 0.0), 1.0) - 1
    return 1 + (overshoot + 1) * u ** 3 + overshoot * u ** 2


def with_alpha(img, a):
    if a >= 1:
        return img
    out = img.copy()
    out.putalpha(img.getchannel("A").point(lambda v: int(v * a)))
    return out


class Item:
    """An image on the frame from time t0, fading in (and rising a few pixels), until t1 (or the end of its scene)."""

    def __init__(self, img, x, y, t0=0.0, fade=0.28, t1=None, rise=px(10)):
        self.img, self.x, self.y, self.t0, self.fade, self.t1, self.rise = img, x, y, t0, fade, t1, rise

    def draw(self, frame, t, dy=0):
        if t < self.t0 or (self.t1 is not None and t >= self.t1):
            return
        a = ease_out((t - self.t0) / self.fade) if self.fade else 1
        y = int(round(self.y - dy + (1 - a) * self.rise))
        if y > H or y + self.img.height < 0:
            return
        paste_clipped(frame, with_alpha(self.img, a), int(round(self.x)), y)


LOGO_LIGHT = (159, 197, 232)   # the light blue of the wavy line (make_logo.py)


def smootherstep(u):
    """0 to 1 with zero slope and zero curvature at both ends."""
    u = min(max(u, 0.0), 1.0)
    return u * u * u * (u * (6 * u - 15) + 10)


class Turn:
    """The fermion loop of the logo turning: its arrow heads travel around it, in the direction they point, by `angle`
    degrees (a whole turn, so that they land on their rest positions), starting at the time `start` (s).
    The motion is that of a heavy wheel pulled by a soft spring: the pull runs with a velocity that rises from rest
    (smootherstep, so with no jerk), glides and comes back to rest; the heads follow it through a damped spring (natural
    frequency `omega`, damping ratio `zeta`), so that they start a little late and softly, glide, and land just beyond
    their rest positions before settling on them. The motion is integrated once into a table; the last fraction of a
    degree of the settling is faded out, so that the heads end exactly at rest."""

    def __init__(self, start, angle=360.0, ramp_up=0.5, glide=0.45, ramp_down=0.5, omega=10.0, zeta=0.5, settle=0.55):
        self.start, self.angle = start, angle
        self.end = ramp_up + glide + ramp_down          # the end of the pull, from the start
        self.length = self.end + settle                 # at rest from then on
        self.dt = 1 / 2400
        top = angle / (ramp_up / 2 + glide + ramp_down / 2)

        def pull(t):
            if t < ramp_up:
                return top * smootherstep(t / ramp_up)
            if t < ramp_up + glide:
                return top
            return top * (1 - smootherstep((t - ramp_up - glide) / ramp_down))

        x = th = w = 0.0
        self.theta, self.omega = [0.0], [0.0]
        for k in range(int(self.length / self.dt) + 2):
            t = k * self.dt
            # semi-implicit Euler: the pull x, the heads th (angular velocity w)
            x += pull(t) * self.dt
            w += (omega ** 2 * (x - th) - 2 * zeta * omega * w) * self.dt
            th += w * self.dt
            fade = smootherstep((t - self.end) / settle)       # the residual of the settling, faded out
            self.theta.append(angle + (th - angle) * (1 - fade))
            self.omega.append(w * (1 - fade))
        self.peak = max(abs(v) for v in self.omega)

    def _look(self, table, t):
        u = (t - self.start) / self.dt
        if u <= 0:
            return table[0]
        k = int(u)
        if k >= len(table) - 1:
            return table[-1]
        return table[k] + (u - k) * (table[k + 1] - table[k])

    def __call__(self, t):
        """The angle (degrees) the heads have travelled at the time t."""
        return self._look(self.theta, t)

    def speed(self, t):
        """The angular speed at the time t, as a fraction of its largest value."""
        return abs(self._look(self.omega, t)) / self.peak

    def active(self, t):
        """1 while the loop turns, 0 at rest, smoothly in between: the breathing of the loop pauses meanwhile."""
        return smootherstep((t - self.start + 0.25) / 0.4) * (1 - smootherstep((t - self.start - self.end) / 0.7))


# the geometry of the badge of Development/make_logo.py, in units of its 128 x 128 picture
LOGO_R, LOGO_LOOP, LOGO_WAVE, LOGO_DOT, LOGO_AMPLITUDE = 38.0, 6.5, 6.0, 7.0, 7.5
LOGO_HEAD, LOGO_HEAD_LENGTH, LOGO_HEAD_WIDTH = 13.0, 0.58, 0.72     # arrowhead(r, angle, size, length, width)


def logo_image(size, tau, turn=None):
    """The badge of Development/make_logo.py (a 128 x 128 picture: a fermion loop of radius 38, slim lines, large arrow
    heads, an interaction line across it, six half waves) drawn at `size` px at the time tau (s), from its geometry: the phase of the wavy line runs, so that
    it ripples between its two fixed ends, and the loop breathes. With a Turn, the arrow heads travel around the loop in
    the direction they point, tangent to it: the ripple is driven by the turn (its phase advances with the angle and its
    waves rise with the speed), the breathing pauses and gives way to a slight swell of the loop at speed, and behind each
    head a faint wake shows where it has just been. At rest the logo is the static one. Drawn four times larger and
    reduced, for smooth edges."""
    k = 4
    big = size * k
    f = big / 128.0
    img = Image.new("RGBA", (big, big), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    d.rounded_rectangle([0, 0, big - 1, big - 1], radius=int(28 * f), fill=ACCENT + (255,))
    edge = tuple(int(v + 0.16 * (255 - v)) for v in ACCENT)    # the faint light edge of the badge
    d.rounded_rectangle([f, f, big - 1 - f, big - 1 - f], radius=int(27 * f), outline=edge + (255,), width=int(2 * f))
    c = 64.0
    theta = turn(tau) if turn else 0.0
    speed = turn.speed(tau) if turn else 0.0
    rest = 1 - turn.active(tau) if turn else 1.0
    r = LOGO_R * (1 + 0.035 * rest * math.sin(2 * math.pi * tau / 2.2) + 0.03 * speed ** 2)
    phase = 2 * math.pi * tau / 1.2 + math.radians(theta)
    amplitude = LOGO_AMPLITUDE * (1 + 0.15 * rest * math.sin(2 * math.pi * tau / 1.7 + 1.0) + 0.22 * speed ** 2)
    n = 700
    wave = []
    for i in range(n + 1):
        u = i / n
        s = amplitude * min(1.0, 6 * u, 6 * (1 - u)) * math.sin(math.pi * 6 * u - phase)
        wave.append(((c - r + 2 * r * u) * f, (c + s) * f))
    half = LOGO_WAVE / 2 * f    # the wavy line, with round ends: discs along it (PIL's wide polylines have gaps)
    for x, y in wave:
        d.ellipse([x - half, y - half, x + half, y + half], fill=LOGO_LIGHT + (255,))
    # the loop, centred on the radius r (PIL draws the outline inside the box)
    hw = LOGO_LOOP / 2
    d.ellipse([(c - r - hw) * f, (c - r - hw) * f, (c + r + hw) * f, (c + r + hw) * f], outline=(255, 255, 255, 255),
              width=int(round(LOGO_LOOP * f)))

    def head(angle):
        """The arrow head centred on the loop at `angle` (degrees, y down), pointing towards smaller angles."""
        a, ln, wd = math.radians(angle), LOGO_HEAD * LOGO_HEAD_LENGTH, LOGO_HEAD * LOGO_HEAD_WIDTH
        px, py = c + r * math.cos(a), c + r * math.sin(a)
        tx, ty = math.sin(a), -math.cos(a)
        nx, ny = -ty, tx
        pts = ((px + tx * ln, py + ty * ln),
               (px - tx * ln + nx * wd, py - ty * ln + ny * wd),
               (px - tx * ln - nx * wd, py - ty * ln - ny * wd))
        return [(x * f, y * f) for x, y in pts]

    if turn and speed > 0.05:
        # the wake: behind each head, along the arc it has covered in the last 0.12 s, a band as wide as the base of
        # the head tapering into the loop, the more transparent the older (layers of growing length, added up), and
        # the fainter the slower the turn
        travelled = math.radians(theta - turn(tau - 0.12))
        base = LOGO_HEAD * LOGO_HEAD_LENGTH / r                                 # from the middle of a head to its base, in radians
        mask, layers = Image.new("L", (big, big), 0), 6
        for j in range(1, layers + 1):
            layer = Image.new("L", (big, big), 0)
            ld = ImageDraw.Draw(layer)
            for rest_angle in (-90, 90):
                start = math.radians(rest_angle - theta) + base
                outer, inner = [], []
                for i in range(25):
                    u = i / 24 * j / layers
                    a, w = start + u * travelled, LOGO_LOOP / 2 + 0.9 * (LOGO_HEAD * LOGO_HEAD_WIDTH - LOGO_LOOP / 2) * (1 - u) ** 1.4
                    outer.append(((c + (r + w) * math.cos(a)) * f, (c + (r + w) * math.sin(a)) * f))
                    inner.append(((c + (r - w) * math.cos(a)) * f, (c + (r - w) * math.sin(a)) * f))
                ld.polygon(outer + inner[::-1], fill=int(255 * 0.55 * speed ** 2 / layers))
            mask = ImageChops.add(mask, layer)
        img.paste(Image.new("RGBA", (big, big), (255, 255, 255, 255)), (0, 0), mask)
    for rest_angle in (-90, 90):
        d.polygon(head(rest_angle - theta), fill=(255, 255, 255, 255))
    for x in (c - r, c + r):
        d.ellipse([(x - LOGO_DOT) * f, (c - LOGO_DOT) * f, (x + LOGO_DOT) * f, (c + LOGO_DOT) * f], fill=(255, 255, 255, 255))
    return img.resize((size, size), Image.LANCZOS)


class LogoItem:
    """The logo, alive: it pops in (scaled up from 80%, a little beyond its size and back) at t0, then ripples; with
    `turn` (a time after t0), its fermion loop turns once (Turn)."""

    def __init__(self, size, x, y, t0=0.0, fade=0.45, turn=None):
        self.size, self.x, self.y, self.t0, self.fade = size, x, y, t0, fade
        self.turn = Turn(turn) if turn is not None else None

    def draw(self, frame, t, dy=0):
        if t < self.t0:
            return
        u = (t - self.t0) / self.fade
        scale = 0.8 + 0.2 * ease_out_back(u)
        size = max(ipx(8), int(round(self.size * scale)))
        img = with_alpha(logo_image(size, t, self.turn), ease_out(1.6 * u))
        paste_clipped(frame, img, int(round(self.x + (self.size - size) / 2)), int(round(self.y - dy + (self.size - size) / 2)))


def paste_clipped(frame, img, x, y):
    top, bottom = max(0, -y), min(img.height, H - y)
    if bottom > top:
        frame.alpha_composite(img.crop((0, top, img.width, bottom)), (x, y + top))


def blank():
    frame = Image.new("RGBA", (W, H), WHITE + (255,))
    ImageDraw.Draw(frame).rectangle([0, H - ipx(6), W, H], fill=ACCENT)   # the band of the social preview
    return frame


def fade_to_white(frame, a):
    """a = 1: the frame; a = 0: white (the accent band stays)."""
    if a >= 1:
        return frame
    out = Image.blend(blank(), frame, a)
    return out


class Scene:
    def __init__(self, duration, items=(), draw=None, fade_out=0.22):
        self.duration, self.items, self.custom, self.fade_out = duration, list(items), draw, fade_out

    def frame(self, t):
        frame = blank()
        if self.custom:
            self.custom(frame, t)
        for item in self.items:
            item.draw(frame, t)
        if self.fade_out and t > self.duration - self.fade_out:
            a = (self.duration - t) / self.fade_out
            shift = int(round(px(36) * (1 - a) ** 2))
            if shift:
                moved = blank()
                moved.paste(frame.crop((shift, 0, W, H - ipx(6))), (0, 0))
                frame = moved
            frame = fade_to_white(frame, a)
        return frame


def centred(img, y):
    return (W - img.width) / 2, y


def title_scene(el, duration, url, fade_out=None, turn=None):
    """The logo, the name and the tagline (and the address of the repository), stacked in the middle; the title card
    fades out, the end card only with `fade_out` (the preview, which loops). With `turn` (a time), the fermion loop of
    the logo turns once from then on (Turn: about two seconds to come to rest)."""
    parts = ["logo", "name", "tagline"] + (["url"] if url else [])
    gaps = {"name": px(26), "tagline": px(12), "url": px(34)}
    height = sum(el[p].height for p in parts) + sum(gaps[p] for p in parts[1:])
    y, items = (H - height) / 2 - px(8), []
    times = {"logo": 0.0, "name": 0.25, "tagline": 0.45, "url": 0.7}
    for p in parts:
        y += gaps.get(p, 0) if p != "logo" else 0
        if p == "logo":
            size = round(2.6 / 2.54 * DPI)     # the height of the logo in elements()
            items.append(LogoItem(size, (W - size) / 2, y + (el[p].height - size) / 2, times[p], turn=turn))
        else:
            x, _ = centred(el[p], y)
            items.append(Item(el[p], x, y, times[p], fade=0.35))
        y += el[p].height
    return Scene(duration, items, fade_out=fade_out if fade_out is not None else (0.3 if not url else 0))


def card_scene(el, key, body, duration, horizontal=False, gap=26, body_start=0.28, stagger=0.3, extra=()):
    """A heading (top left), its second line, the names of the functions (top right) and a body of elements, stacked
    and left-aligned (or side by side), centred in the rest of the frame, appearing one after the other (`gap`: in pixels
    of a frame 1280 px wide, like every length here)."""
    items, x0, y0, gap = [], px(64), px(44), px(gap)
    h = el["h_" + key]
    items.append(Item(h, x0, y0, 0.0, fade=0.22))
    y = y0 + h.height
    if "s_" + key in el:
        items.append(Item(el["s_" + key], x0, y + px(2), 0.07, fade=0.22))
        y += el["s_" + key].height + px(2)
    chips = el["c_" + key]
    items.append(Item(chips, x0 - px(3), y + px(10), 0.14, fade=0.22))
    top = y + px(10) + chips.height + px(20)
    imgs = [el[b] if isinstance(b, str) else b for b in body]
    if horizontal:
        total = sum(i.width for i in imgs) + gap * (len(imgs) - 1)
        x, mid = (W - total) / 2, (top + H - px(30)) / 2
        for k, img in enumerate(imgs):
            items.append(Item(img, x, mid - img.height / 2, body_start + stagger * k))
            x += img.width + gap
    else:
        total = sum(i.height for i in imgs) + gap * (len(imgs) - 1)
        widest = max((i.width for i in imgs), default=0)
        x, y = (W - widest) / 2, top + max(0, (H - px(30) - top - total) / 2)
        for k, img in enumerate(imgs):
            items.append(Item(img, x, y, body_start + stagger * k))
            y += img.height + gap
    items.extend(extra)
    return Scene(duration, items)


def tikz_code_panel():
    """The first lines of the TikZ code of the showcase's example (DiagramToTikZ), on a code background."""
    lines = open(os.path.join(FIGURES, "tikz_example_first.tex"), encoding="utf-8").read().splitlines()
    shown = [l if len(l) <= 36 else l[:34] + " \u2026" for l in lines[:6]] + ["  \u2026", lines[-1]]
    fnt = font("SourceCodePro-Regular.otf", px(16))
    lh, pad = px(25), ipx(16)
    width = int(max(fnt.getlength(l) for l in shown)) + 2 * pad
    img = Image.new("RGBA", (width, int(round(lh * len(shown) + 2 * pad - px(4)))), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    d.rounded_rectangle([0, 0, img.width - 1, img.height - 1], radius=ipx(6), fill=CODE_BG + (255,), outline=CODE_FRAME + (255,),
                        width=ipx(1))
    for k, l in enumerate(shown):
        colour = ACCENT if re.match(r"\s*\\(begin|end)", l) or l.startswith("-") else (40, 40, 40)
        d.text((pad, pad + k * lh - px(2)), l, font=fnt, fill=colour)
    return img


def arrow_image(width=60):
    """An arrow to the right, `width` px long (in a frame 1280 px wide)."""
    w, y = ipx(width), ipx(15)
    img = Image.new("RGBA", (w, ipx(30)), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    d.line([ipx(4), y, w - ipx(8), y], fill=MUTED + (255,), width=ipx(2))
    d.polygon([(w - ipx(4), y), (w - ipx(14), y - ipx(6)), (w - ipx(14), y + ipx(6))], fill=MUTED + (255,))
    return img


class Reveal(Item):
    """An image uncovered from left to right (an arrow drawing itself), from t0 in `fade` seconds."""

    def draw(self, frame, t, dy=0):
        if t < self.t0:
            return
        u = ease((t - self.t0) / self.fade)
        w = int(round(self.img.width * u))
        if w > 0:
            paste_clipped(frame, self.img.crop((0, 0, w, self.img.height)), int(round(self.x)), int(round(self.y - dy)))


def result_arrow(width=78, colour=ACCENT):
    """A slim arrow to the right, `width` px long (in a frame 1280 px wide), drawn four times larger and reduced, for
    smooth edges."""
    k, w, h = 4, ipx(width), ipx(26)
    f = k * S       # one pixel of a frame 1280 px wide, in the large drawing
    right, mid = w * k, h * k / 2
    img = Image.new("RGBA", (w * k, h * k), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    d.line([3 * f, h * k // 2, right - 12 * f, h * k // 2], fill=colour + (255,), width=int(2.4 * f))
    d.polygon([(right - 2 * f, mid), (right - 17 * f, mid - 7 * f), (right - 13 * f, mid), (right - 17 * f, mid + 7 * f)],
              fill=colour + (255,))
    return img.resize((w, h), Image.LANCZOS)


def results_scene(el, key, rows, duration, top=None, gap=22, start=0.28, step=0.55):
    """A card whose body is a column of diagrams, each with an arrow to what the package computes from it: row k (a
    diagram and its result, each centred on the row) appears at start + k step, its arrow drawing itself, then the
    result."""
    scene = card_scene(el, key, [], duration)
    head_bottom = max(i.y + i.img.height for i in scene.items) + px(20)
    left = [el[a] for a, _ in rows]
    right = [el[b] for _, b in rows]
    arrow = result_arrow()
    lw, rw, aw, pad, gap = max(i.width for i in left), max(i.width for i in right), arrow.width, px(18), px(gap)
    heights = [max(a.height, b.height) for a, b in zip(left, right)]
    total = sum(heights) + gap * (len(rows) - 1)
    y = head_bottom + max(0, (H - px(30) - head_bottom - total) / 2)
    x0 = (W - (lw + 2 * pad + aw + rw)) / 2
    for k, (a, b, h) in enumerate(zip(left, right, heights)):
        t = start + step * k
        mid = y + h / 2
        scene.items.append(Item(a, x0 + lw - a.width, mid - a.height / 2, t))
        scene.items.append(Reveal(arrow, x0 + lw + pad, mid - arrow.height / 2, t + 0.3, fade=0.3))
        scene.items.append(Item(b, x0 + lw + 2 * pad + aw, mid - b.height / 2, t + 0.5))
        y += h + gap
    return scene


class Notebook:
    """The cells of the first example typed one after the other, each evaluated and followed by its outputs, as in a
    notebook: In[n]:= on the left of an input cell, Out[n]= on the left of each output cell (a cell of several lines
    without a semicolon has one output for each, numbered on), and on the right a bracket for each cell and one around
    the group of an input with its outputs. The page scrolls to keep the newest line in view."""
    X_CODE, LH, CELL_GAP, OUT_GAP = px(122), px(30), px(26), px(14)
    X_BRACKET, X_GROUP = W - ipx(36), W - ipx(27)     # the cell brackets, and those of the groups

    def __init__(self, el, start=0.25):
        self.codefont = font("SourceCodePro-Medium.otf", px(19))
        self.labelfont = font("SourceCodePro-Regular.otf", px(14))
        cells = readme_cells()
        # the outputs, and the height of their Out[n]= (from their top): the first band of the Green function, the
        # middle of the left-hand side of an equation (Out[n]= level with its = sign)
        outputs = {2: [(el["nb_green"], px(6))], 3: [(el[k], self.lhs_middle(el[k]) - px(9)) for k in ("nb_sigma", "nb_gamma")]}
        speeds = [70, 260, 170, 240]    # characters per second
        self.cells, t, y, line = [], start, px(34), 1
        for i, code in enumerate(cells):
            cell = CodeCell(code, self.codefont, self.X_CODE, y, self.LH)
            cps = speeds[min(i, len(speeds) - 1)]
            typing = len(code) / cps
            c = dict(cell=cell, n=line, t0=t, cps=cps, t_eval=t + typing + 0.15, top=y - px(8), outs=[])
            y += cell.height
            c["input_bottom"] = y + px(2)
            t = c["t_eval"] + 0.15
            outs = outputs.get(i, [])
            for k, (img, at) in enumerate(outs):
                y += self.OUT_GAP
                c["outs"].append((Item(img, self.X_CODE - px(12), y, t + 0.32 * k + 0.1, fade=0.3), line + k, at))
                y += img.height
            line += max(1, len(outs))
            c["bottom"] = y + px(4)
            hold = {0: 0.1, 1: 0.2, 2: 1.5, 3: 2.6}.get(i, 1.0)
            t += 0.32 * max(0, len(outs) - 1) + (0.3 if outs else 0) + hold
            y += self.CELL_GAP
            self.cells.append(c)
        self.duration = t
        self.scroll_track = self._scroll_track()

    def _bottom(self, t):
        """The lowest point on the page that should be in view at time t."""
        bottom = 0
        for c in self.cells:
            cell = c["cell"]
            if t < c["t0"]:
                break
            n = int((t - c["t0"]) * c["cps"])
            rows = c["cell"].code[:n].count("\n") + 1
            bottom = cell.y + rows * cell.lh
            for item, _, _ in c["outs"]:
                if t >= item.t0 - 0.25:
                    bottom = item.y + item.img.height
        return bottom

    def _scroll_track(self):
        s, track, dt = 0.0, [], 1 / FPS
        for k in range(int(self.duration * FPS) + 2):
            t = k * dt
            target = max(0.0, self._bottom(t) + px(34) - H)
            s += (max(s, target) - s) * (1 - math.exp(-dt / 0.12))
            track.append(s)
        return track

    def draw(self, frame, t):
        # the scroll at this time, interpolated (the frames of the preview are more frequent than the track)
        u = min(t * FPS, len(self.scroll_track) - 1.001)
        k = int(u)
        dy = self.scroll_track[k] + (u - k) * (self.scroll_track[k + 1] - self.scroll_track[k])
        d = ImageDraw.Draw(frame)
        for c in self.cells:
            if t < c["t0"]:
                break
            cell = c["cell"]
            n = min(len(cell.code), int((t - c["t0"]) * c["cps"]))
            typing = t < c["t_eval"]
            blink = n < len(cell.code) or int(t * 2.2) % 2 == 0
            cell.draw(d, n, dy, cursor=typing and blink)
            if t >= c["t_eval"]:
                self.label(d, "In[%d]:=" % c["n"], cell.y + px(4) - dy, LABEL)
            self.bracket(d, self.X_BRACKET, c["top"] - dy, c["input_bottom"] - dy)
            bottom = None
            for item, number, at in c["outs"]:
                if t < item.t0:
                    continue
                item.draw(frame, t, dy)
                a = ease((t - item.t0) / 0.3)
                colour = tuple(int(255 - (255 - v) * a) for v in LABEL)
                self.label(d, "Out[%d]=" % number, item.y + at - dy, colour)
                bottom = item.y + item.img.height + px(4)
                self.bracket(d, self.X_BRACKET, item.y - px(6) - dy, bottom - dy, colour=tuple(
                    int(255 - (255 - v) * a) for v in BRACKET))
            if bottom is not None:
                self.bracket(d, self.X_GROUP, c["top"] - dy, bottom - dy)

    @staticmethod
    def lhs_middle(img, width=2.2 / 2.54 * DPI):
        """The middle of the left-hand side of a row of \\eqrow (its first 2.2 cm), from the top of the image."""
        top, bottom = img.getchannel("A").crop((0, 0, int(width), img.height)).getbbox()[1::2]
        return (top + bottom) / 2

    def label(self, d, text, y, colour):
        """In[n]:= or Out[n]=, right-aligned before the cells."""
        d.text((self.X_CODE - px(12) - self.labelfont.getlength(text), y), text, font=self.labelfont, fill=colour)

    @staticmethod
    def bracket(d, x, top, bottom, colour=BRACKET):
        d.line([x, top, x, bottom], fill=colour, width=ipx(1))
        d.line([x - ipx(5), top, x, top], fill=colour, width=ipx(1))
        d.line([x - ipx(5), bottom, x, bottom], fill=colour, width=ipx(1))


# ---------------------------------------------------------------------------------------------------------------------

def timeline(el, preview=False):
    d = (lambda video, short: short if preview else video)     # the preview is shorter
    twoparticle = card_scene(el, "twoparticle", ["eq_bethe_salpeter", "bs_rpa", "bs_ladder"], d(3.9, 3.4), gap=8)
    parquet = card_scene(el, "parquet", ["eq_parquet"], d(3.9, 3.4))
    # four rows, the last one tall: close together, to fit the frame
    hedin = card_scene(el, "hedin", ["hedin_sigma", "hedin_w", "hedin_p", "hedin_lambda"], d(3.4, 3.0), gap=0, stagger=0.22)
    # the end card of the preview fades out like the others, and the first card builds up again (a fade redraws the
    # whole card in every frame of the preview: kept as short as those of the other cards); the preview starts inside
    # that first card, in full view (PREVIEW_START)
    end = title_scene(el, d(3.4, 2.8), url=True, fade_out=d(0, 0.22), turn=d(0.55, 0.45))
    if preview:
        derivatives = card_scene(el, "derivatives", ["card_phi", "card_sigma", "card_gamma"], 3.4, gap=26, stagger=0.35)
        return [derivatives, twoparticle, parquet, hedin, end]
    notebook = Notebook(el)
    # the last element of each card in full view for at least about 1.6 s
    matsubara = card_scene(el, "matsubara", ["lindhard_diagram", "lindhard_formula"], 3.0, gap=30, stagger=0.3)
    # the three diagrams, each with an arrow to its result: the last one in full view for about 2.7 s
    qed = results_scene(el, "qed", [("qed_sigma", "qed_sigma_result"), ("qed_pi", "qed_pi_result"),
                                    ("qed_vertex", "qed_vertex_result")], 4.5, gap=12)
    tikz = card_scene(el, "tikz", [tikz_code_panel(), arrow_image(), "tikz_picture"], 3.0, horizontal=True, gap=36, stagger=0.24)
    return [title_scene(el, 2.4, url=False, turn=0.3), Scene(notebook.duration, draw=notebook.draw, fade_out=0.25),
            twoparticle, parquet, hedin, matsubara, qed, tikz, end]


# The preview loops from this time (s) of its first card, when all of it is in view: the first frame of the preview,
# the one seen by visitors who do not play animated images and in link previews, is that card (not the white frame before
# it builds up); its build-up, after the end card, closes the loop on that frame.
PREVIEW_START = 1.5


_JOB = None    # (scenes, size) of the frames being drawn, read by the worker processes (inherited by fork)


def _frame_bytes(job):
    i, t = job
    scenes, size = _JOB
    f = scenes[i].frame(t).convert("RGB")
    return (f if size is None else f.resize(size, Image.LANCZOS)).tobytes()


def frames(scenes, fps, size=None, start=0.0):
    """The frames of the scenes one after the other, as raw RGB bytes; with `start` (s, in the first scene), a loop shown
    from there: the frames from that time to the end, then those before it. Each frame depends only on its time: they
    are drawn by several processes (fork) at once, in order, where fork is available."""
    global _JOB
    times = [(i, k / fps) for i, scene in enumerate(scenes) for k in range(int(round(scene.duration * fps)))]
    k0 = int(round(start * fps))
    times = times[k0:] + times[:k0]
    _JOB = (scenes, size)
    if "fork" not in multiprocessing.get_all_start_methods():
        yield from map(_frame_bytes, times)
        return
    with multiprocessing.get_context("fork").Pool(os.cpu_count() or 1) as pool:
        yield from pool.imap(_frame_bytes, times, chunksize=4)


def encode(scenes, fps, args, size=None, start=0.0):
    w, h = size or (W, H)
    cmd = ["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", "%dx%d" % (w, h),
           "-r", str(fps), "-i", "-"] + args
    proc = subprocess.Popen(cmd, stdin=subprocess.PIPE)
    for f in frames(scenes, fps, size, start):
        proc.stdin.write(f)
    proc.stdin.close()
    if proc.wait():
        sys.exit("ffmpeg failed")


def write_preview(scenes, path):
    """The looping preview, from PREVIEW_START, as an animated WebP at PREVIEW_FPS (written by PIL): a run of identical
    frames (a card in full view) is one frame, held as long; each frame is stored lossy or lossless, whichever is smaller
    (allow_mixed), so that the fades stay small and the lines of the cards sharp."""
    size, kept, counts = (PREVIEW_W, PREVIEW_H), [], []
    for f in frames(scenes, PREVIEW_FPS, size, PREVIEW_START):
        if kept and f == kept[-1]:
            counts[-1] += 1
        else:
            kept.append(f)
            counts.append(1)
    # the delays in milliseconds, rounded so that they add up to the time of the frames (16 or 17 ms at 60 fps)
    durations, k = [], 0
    for c in counts:
        durations.append(round((k + c) * 1000 / PREVIEW_FPS) - round(k * 1000 / PREVIEW_FPS))
        k += c
    images = [Image.frombytes("RGB", size, f) for f in kept]
    images[0].save(path, save_all=True, append_images=images[1:], duration=durations, loop=0, minimize_size=True,
                   allow_mixed=True, quality=PREVIEW_QUALITY, method=6)


def main():
    os.makedirs(BUILD, exist_ok=True)
    os.makedirs(MEDIA, exist_ok=True)
    el = render_elements()
    video = timeline(el)
    total = sum(s.duration for s in video)
    print("video: %.1f s in %d scenes" % (total, len(video)))
    if "--frames" in sys.argv:
        out = os.path.join(BUILD, "frames")
        os.makedirs(out, exist_ok=True)
        t0 = 0.0
        for i, s in enumerate(video):
            for u in (0.55, 0.9):
                s.frame(u * s.duration).convert("RGB").save(os.path.join(out, "scene%d_%02d.png" % (i, round(u * 100))))
            t0 += s.duration
        for i, s in enumerate(timeline(el, preview=True)):
            s.frame(0.9 * s.duration).convert("RGB").resize((PREVIEW_W, PREVIEW_H), Image.LANCZOS).save(os.path.join(out, "preview%d.png" % i))
            if i == 0:
                s.frame(PREVIEW_START).convert("RGB").resize((PREVIEW_W, PREVIEW_H), Image.LANCZOS).save(os.path.join(out, "preview_first.png"))
        print("frames in", out)
        if "--only-frames" in sys.argv:
            return
    mp4 = os.path.join(MEDIA, "demo.mp4")
    encode(video, FPS, ["-c:v", "libx264", "-preset", "veryslow", "-crf", "20", "-tune", "animation", "-pix_fmt", "yuv420p",
                        "-movflags", "+faststart", mp4])
    # the poster: the first example, all of its output in view
    nb = video[1]
    nb.frame(nb.duration - 0.6).convert("RGB").quantize(colors=64, method=Image.MEDIANCUT).save(
        os.path.join(MEDIA, "demo_poster.png"), optimize=True)
    preview = timeline(el, preview=True)
    first = preview[0]
    if not max(i.t0 + i.fade for i in first.items) <= PREVIEW_START < first.duration - first.fade_out:
        print("warning: the first frame of the preview is not its first card in full view (PREVIEW_START)")
    webp = os.path.join(MEDIA, "demo_preview.webp")
    write_preview(preview, webp)
    for path, length in ((mp4, total), (webp, sum(s.duration for s in preview)), (os.path.join(MEDIA, "demo_poster.png"), 0)):
        print("%s: %.0f kB%s" % (os.path.relpath(path, ROOT), os.path.getsize(path) / 1024, ", %.1f s" % length if length else ""))


if __name__ == "__main__":
    main()
