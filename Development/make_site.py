#!/usr/bin/env python3
"""Writes index.html, the landing page of the GitHub Pages site (https://n4sd4q.github.io/FunctionalDiagrams/), from the
template Development/site_template.html and the sources of every fact on it, so that nothing is stated twice:

- README.md: the tagline, the opening paragraphs, the install lines, the first example (its code blocks, pictures and
  captions, in order), the panels of the gallery with their captions, the scope (What it does), the text of the map,
  Checks, Related software, and the BibTeX entry of Citing;
- PacletInfo.wl: the name, the repository (hence the address of the site), the Mathematica version needed, the license
  and the name of the guided tour among the assets; Development/src/00_header.wl: the version; CITATION.cff: the date of
  the release, the author and the affiliation;
- Tests/RunTests.out: the number of tests of the last run; Documentation/function_map.json: the numbers of public names
  and dependencies; the PDFs: their numbers of pages; Media/demo.mp4: its length; Logo/social_preview.png: its size.

Deterministic (the same sources give the same page) and standard library only: Development/check_consistency.py
generates the page again and compares. A README section the page needs that cannot be found stops the script with a
message. Run from any folder: python3 Development/make_site.py [OUTPUT]   (default: index.html at the root)"""
import html
import json
import math
import os
import re
import struct
import sys
import zlib

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def read(*path):
    with open(os.path.join(ROOT, *path), encoding='utf-8') as f:
        return f.read()


def fail(message):
    sys.exit('make_site.py: ' + message)


def esc(text):
    return html.escape(text, quote=True)


# ------------------------------------------------------------------------------------------------------------ sources

def paclet_info():
    text = read('PacletInfo.wl')

    def field(key):
        m = re.search(r'"%s" -> "([^"]*)"' % key, text)
        if not m:
            fail('PacletInfo.wl has no "%s"' % key)
        return m.group(1)

    assets = dict(re.findall(r'\{"([^"]+)", "([^"]+)"\}', text))
    return dict(name=field('Name'), version=field('Version'), wolfram=field('WolframVersion'), license=field('License'),
                url=field('URL'), creator=field('Creator'), assets=assets)


def package_version():
    m = re.search(r'^\$FunctionalDiagramsVersion = "([^"]+)";', read('Development', 'src', '00_header.wl'), re.M)
    if not m:
        fail('no $FunctionalDiagramsVersion in Development/src/00_header.wl')
    return m.group(1)


def site_url(repo):
    """the address of GitHub Pages for a repository https://github.com/OWNER/NAME"""
    m = re.fullmatch(r'https://github\.com/([^/]+)/([^/]+)', repo)
    if not m:
        fail('the URL of PacletInfo.wl is not a GitHub repository: ' + repo)
    return 'https://%s.github.io/%s/' % (m.group(1).lower(), m.group(2))


def citation():
    text = read('CITATION.cff')
    date = re.search(r'^date-released: "?(\d{4})-(\d{2})-(\d{2})"?', text, re.M)
    family = re.search(r'family-names: (.+)', text)
    given = re.search(r'given-names: (.+)', text)
    affiliation = re.search(r'affiliation: "?([^"\n]+)"?', text)
    if not (date and family and given and affiliation):
        fail('CITATION.cff lacks date-released, the author or the affiliation')
    months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October',
              'November', 'December']
    return dict(date='%d %s %s' % (int(date.group(3)), months[int(date.group(2)) - 1], date.group(1)),
                author='%s %s' % (given.group(1).strip(), family.group(1).strip()), affiliation=affiliation.group(1).strip())


def tests_passed():
    m = re.search(r'^(\d+) passed, 0 failed', read('Tests', 'RunTests.out'), re.M)
    if not m:
        fail('Tests/RunTests.out does not end with "N passed, 0 failed"')
    return int(m.group(1))


def package_names():
    """the public names, and the names with a usage message ($-variables included): the names set in colour in the code"""
    package = read('FunctionalDiagrams.wl')
    public = re.findall(r'"([A-Za-z][A-Za-z0-9]*)"', re.search(r'\$publicNames = \{(.*?)\};', package, re.S).group(1))
    usages = re.findall(r'^(\$?[A-Za-z][A-Za-z0-9]*)::usage', package, re.M)
    return set(public) | set(usages)


def pdf_pages(*path):
    """the /Count of the root /Pages object, also inside compressed object streams"""
    with open(os.path.join(ROOT, *path), 'rb') as f:
        data = f.read()
    texts = [data]
    for m in re.finditer(rb'stream\r?\n', data):
        end = data.find(b'endstream', m.end())
        try:
            texts.append(zlib.decompressobj().decompress(data[m.end():end]))
        except zlib.error:
            pass
    counts = set()
    for text in texts:
        for d in re.finditer(rb'<<((?:(?!<<|>>).|<<(?:(?!>>).)*>>)*?)>>', text, re.S):
            body = d.group(1)
            if re.search(rb'/Type\s*/Pages\b', body) and not re.search(rb'/Parent\b', body):
                c = re.search(rb'/Count\s+(\d+)', body)
                if c:
                    counts.add(int(c.group(1)))
    if len(counts) != 1:
        fail('cannot read the number of pages of ' + os.path.join(*path))
    return counts.pop()


def mp4_seconds(*path):
    """the duration of the movie header (mvhd) of an MP4 file, rounded to seconds"""
    with open(os.path.join(ROOT, *path), 'rb') as f:
        data = f.read()

    def boxes(start, end):
        i = start
        while i + 8 <= end:
            size, kind = struct.unpack('>I4s', data[i:i + 8])
            head = 8
            if size == 1:
                size, head = struct.unpack('>Q', data[i + 8:i + 16])[0], 16
            elif size == 0:
                size = end - i
            if size < head:
                break
            yield kind, i + head, i + size
            i += size

    for kind, start, end in boxes(0, len(data)):
        if kind == b'moov':
            for kind2, s, _ in boxes(start, end):
                if kind2 == b'mvhd':
                    if data[s] == 1:
                        scale, duration = struct.unpack('>IQ', data[s + 20:s + 32])
                    else:
                        scale, duration = struct.unpack('>II', data[s + 12:s + 20])
                    return int(duration / scale + 0.5)
    fail('no movie header in ' + os.path.join(*path))


def png_size(*path):
    with open(os.path.join(ROOT, *path), 'rb') as f:
        head = f.read(24)
    if head[:8] != b'\x89PNG\r\n\x1a\n':
        fail(os.path.join(*path) + ' is not a PNG file')
    return struct.unpack('>II', head[16:24])


def svg_size(*path):
    """the width and height of the viewBox of an SVG file, rounded"""
    m = re.search(r'viewBox="([-\d.]+) ([-\d.]+) ([\d.]+) ([\d.]+)"', read(*path))
    if not m:
        fail(os.path.join(*path) + ' has no viewBox')
    return round(float(m.group(3))), round(float(m.group(4)))


# ------------------------------------------------------------------------------------------------- the README, parsed

class Readme:
    def __init__(self, text):
        self.text = text

    def head(self):
        """the part before the first section"""
        return re.split(r'^## ', self.text, maxsplit=1, flags=re.M)[0]

    def section(self, title):
        m = re.search(r'^## ' + re.escape(title) + r'[ \t]*\n(.*?)(?=^## |\Z)', self.text, re.S | re.M)
        if not m:
            fail('README.md has no section "## %s"' % title)
        return m.group(1)


def blocks(text):
    """the blocks of a piece of Markdown, in order: ('code', language, text), ('html', text) or ('para', text)"""
    out, lines, i = [], text.split('\n'), 0
    while i < len(lines):
        line = lines[i]
        if not line.strip():
            i += 1
            continue
        fence = re.match(r'^(```+)\s*(\S*)\s*$', line)
        if fence:
            j = i + 1
            while j < len(lines) and not lines[j].startswith(fence.group(1)):
                j += 1
            out.append(('code', fence.group(2), '\n'.join(lines[i + 1:j])))
            i = j + 1
            continue
        j = i
        while j < len(lines) and lines[j].strip() and not re.match(r'^```', lines[j]):
            j += 1
        chunk = '\n'.join(lines[i:j])
        if not chunk.lstrip().startswith('<'):
            out.append(('para', chunk))
        elif re.fullmatch(r'(\s*<p\b.*?</p>)+\s*', chunk, re.S):
            out.extend(('html', p.strip()) for p in re.findall(r'<p\b.*?</p>', chunk, re.S))  # a picture, its caption
        else:
            out.append(('html', chunk))
        i = j
    return out


def one_line(text):
    return re.sub(r'\s+', ' ', text).strip()


class Links:
    """where a link of the README leads on the site: files that a browser shows (PDFs, pages, pictures, the video) and the
    notebook (a download) are served by GitHub Pages next to the page; sources and Markdown open on GitHub; anchors of
    the README lead to the sections of the page that have them"""
    ANCHORS = {'installation': 'install', 'a-first-example': 'example', 'checks': 'checks', 'citing': 'cite',
               'related-software': 'related', 'documentation': 'docs', 'what-it-does': 'what'}
    SERVED = ('.pdf', '.html', '.svg', '.png', '.mp4', '.webp', '.nb')

    def __init__(self, repo, site):
        self.repo, self.site = repo, site

    def __call__(self, target):
        if target.startswith(self.site):
            return target[len(self.site):] or './'
        if target.startswith('#'):
            anchor = self.ANCHORS.get(target[1:])
            return '#' + anchor if anchor else self.repo + target
        if re.match(r'[a-z]+:', target):
            return target
        path, _, anchor = target.partition('#')
        if path.lower().endswith(self.SERVED):
            return target
        return '%s/blob/main/%s' % (self.repo, target)


def inline(md, link):
    """Markdown of one paragraph (with inline HTML) as HTML: code spans, links, bold, italics"""
    keep = []

    def hold(s):
        keep.append(s)
        return '\x00%d\x00' % (len(keep) - 1)

    def code(m):
        body = m.group(2)
        if body.startswith(' ') and body.endswith(' ') and body.strip():
            body = body[1:-1]
        return hold('<code>%s</code>' % esc(body))

    text = re.sub(r'(`+)(.+?)(?<!`)\1(?!`)', code, one_line(md))
    text = re.sub(r'</?[A-Za-z][^>]*>', lambda m: hold(m.group(0)), text)
    text = re.sub(r'&(?![A-Za-z]+;|#\d+;)', '&amp;', text).replace('<', '&lt;').replace('>', '&gt;')
    text = re.sub(r'\[([^\]]+)\]\(([^)\s]+)\)', lambda m: hold('<a href="%s">' % esc(link(m.group(2)))) + m.group(1)
                  + hold('</a>'), text)
    text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
    text = re.sub(r'(?<![\w*])\*(?!\s)(.+?)(?<!\s)\*(?![\w*])', r'<em>\1</em>', text)
    while '\x00' in text:
        text = re.sub(r'\x00(\d+)\x00', lambda m: keep[int(m.group(1))], text)
    return text


def paragraphs(text, link):
    found = [inline(b[1], link) for b in blocks(text) if b[0] == 'para']
    return '\n'.join('<p>%s</p>' % p for p in found)


def image_of(chunk):
    """the src and the alt text of the <img> of an HTML block"""
    src = re.search(r'<img\b[^>]*?\ssrc="([^"]+)"', chunk, re.S)
    alt = re.search(r'<img\b[^>]*?\salt="([^"]*)"', chunk, re.S)
    if not src:
        return None
    return src.group(1), one_line(html.unescape(alt.group(1))) if alt else ''


def inner_paragraph(chunk):
    """the content of an HTML block <p ...>...</p> without an image"""
    m = re.fullmatch(r'\s*<p\b[^>]*>(.*)</p>\s*', chunk, re.S)
    return one_line(m.group(1)) if m and '<img' not in chunk else None


# ---------------------------------------------------------------------------------------------------- pieces of HTML

ICONS = {
    'GITHUB': '<svg viewBox="0 0 16 16" aria-hidden="true" focusable="false"><path fill="currentColor" d="M8 0C3.58 0 0 3.58 '
              '0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23'
              '-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1'
              '.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32'
              '-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07'
              '-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4'
              '.42-3.58-8-8-8z"/></svg>',
    'MOON': '<svg class="moon" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M20.5 14.2A8.5 8.5 0 0 1 9.8 '
            '3.5a8.5 8.5 0 1 0 10.7 10.7z" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linejoin="round"/>'
            '</svg>',
    'SUN': '<svg class="sun" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><g fill="none" stroke="currentColor" '
           'stroke-width="1.8" stroke-linecap="round"><circle cx="12" cy="12" r="4.2"/><path d="M12 2.5v2.2M12 19.3v2.2M2.5 '
           '12h2.2M19.3 12h2.2M5.3 5.3l1.6 1.6M17.1 17.1l1.6 1.6M5.3 18.7l1.6-1.6M17.1 6.9l1.6-1.6"/></g></svg>',
    'ARROW': '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M5 12h13M12.5 6l6 6-6 6" fill="none" '
             'stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"/></svg>',
    'COPY': '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><g fill="none" stroke="currentColor" '
            'stroke-width="1.8" stroke-linejoin="round"><rect x="8.5" y="8.5" width="12" height="12" rx="2.2"/><path d="M15.5 '
            '8.5V5.7a2.2 2.2 0 0 0-2.2-2.2H5.7a2.2 2.2 0 0 0-2.2 2.2v7.6a2.2 2.2 0 0 0 2.2 2.2h2.8"/></g></svg>',
}


def wave_path(width=150, height=18, waves=4.5, margin=3):
    """the wavy line of the hero, a photon line with a half-integer number of waves as in the logo"""
    pts = []
    for k in range(121):
        x = margin + (width - 2 * margin) * k / 120
        y = height / 2 - (height / 2 - margin) * math.sin(2 * math.pi * waves * k / 120)
        pts.append('%.2f %.2f' % (x, y))
    return 'M ' + ' L '.join(pts)


def highlight(code, names):
    """Wolfram Language code as HTML: comments, strings and the names of the package marked"""
    out, i, n = [], 0, len(code)
    word = re.compile(r'\$?[A-Za-z][A-Za-z0-9]*')
    while i < n:
        if code.startswith('(*', i):
            depth, j = 1, i + 2
            while j < n and depth:
                if code.startswith('(*', j):
                    depth, j = depth + 1, j + 2
                elif code.startswith('*)', j):
                    depth, j = depth - 1, j + 2
                else:
                    j += 1
            out.append('<span class="tok-c">%s</span>' % esc(code[i:j]))
            i = j
        elif code[i] == '"':
            j = i + 1
            while j < n and code[j] != '"':
                j += 2 if code[j] == '\\' else 1
            out.append('<span class="tok-s">%s</span>' % esc(code[i:j + 1]))
            i = j + 1
        else:
            m = word.match(code, i)
            if m:
                w = m.group(0)
                out.append('<span class="tok-f">%s</span>' % w if w in names else w)
                i = m.end()
            else:
                out.append(esc(code[i]))
                i += 1
    return ''.join(out)


class Code:
    """code blocks with a copy button; every block gets its own id"""
    def __init__(self, names):
        self.names, self.count = names, 0

    def _id(self):
        self.count += 1
        return 'code-%d' % self.count

    def button(self, ident, what):
        return ('<button class="copy" type="button" data-copy="%s" aria-label="Copy %s">%s<span class="label">Copy</span>'
                '</button>' % (ident, esc(what), ICONS['COPY']))

    def block(self, code, language, what):
        ident = self._id()
        body = highlight(code, self.names) if language == 'mathematica' else esc(code)
        label = {'mathematica': 'Mathematica', 'bibtex': 'BibTeX'}.get(language, language)
        return ('<div class="codecard"><div class="codehead"><span>%s</span>%s</div><pre><code id="%s">%s</code></pre></div>'
                % (label, self.button(ident, what), ident, body))

    def line(self, code, what):
        ident = self._id()
        return ('<div class="oneliner"><pre><code id="%s">%s</code></pre>%s</div>'
                % (ident, highlight(code, self.names), self.button(ident, what)))


def picture(src, alt, cls='paper'):
    """a picture of the README, a link to itself: the SVG opens at full size in the browser"""
    w, h = svg_size(*src.split('/'))
    return ('<a class="zoom" href="%s" title="Open the picture at full size"><img class="%s" src="%s" width="%d" '
            'height="%d" loading="lazy" decoding="async" alt="%s"></a>' % (esc(src), cls, esc(src), 2 * w, 2 * h, esc(alt)))


# ------------------------------------------------------------------------------------------------------------- the page

def build():
    info = paclet_info()
    version = package_version()
    if version != info['version']:
        fail('the version of Development/src/00_header.wl (%s) is not that of PacletInfo.wl (%s)' % (version, info['version']))
    repo, cite = info['url'], citation()
    site = site_url(repo)
    link = Links(repo, site)
    names = package_names()
    code = Code(names)
    readme = Readme(read('README.md'))
    values = {}

    # the header of the README: logo, tagline, opening paragraphs, the demo
    head = readme.head()
    tag = re.search(r'<b>(.*?)</b>\s*<br>\s*(.*?)\s*</p>', head, re.S)
    if not tag:
        fail('README.md has no tagline (<b>...</b><br> ...)')
    tagline, subline = one_line(tag.group(1)), one_line(tag.group(2))
    logo = image_of(head)
    demo = [image_of(b[1]) for b in blocks(head) if b[0] == 'html' and 'demo_preview' in b[1]]
    intro = [b[1] for b in blocks(head) if b[0] == 'para']
    if not logo or not demo or not intro:
        fail('README.md lacks the logo, the demo preview or the opening paragraphs')
    video_label = re.sub(r'^Preview of the demo video', 'Demo video', demo[0][1])
    values.update(NAME=esc(info['name']), TAGLINE=esc(tagline), SUBLINE=esc(subline), LOGO_ALT=esc(logo[1]),
                  DESCRIPTION=esc('%s: %s.' % (tagline, subline)), VIDEO_LABEL=esc(video_label),
                  INTRO_LEAD='<p class="lead">%s</p>' % inline(intro[0], link),
                  INTRO_REST='\n'.join('<p>%s</p>' % inline(p, link) for p in intro[1:]))

    # Installation: the two lines, and the sentence on the update check
    install = readme.section('Installation')
    lines = [b[2].strip() for b in blocks(install) if b[0] == 'code' and b[1] == 'mathematica']
    paclet = [c for c in lines if c.startswith('PacletInstall[')]
    needs = [c for c in lines if c.startswith('Needs[')]
    if not paclet or not needs:
        fail('the Installation of README.md has no PacletInstall line or no Needs line')
    prose = one_line(' '.join(re.sub(r'^\s*[-*]\s+', '', b[1], flags=re.M) for b in blocks(install) if b[0] == 'para'))
    update = [s for s in re.split(r'(?<=\.)\s+(?=[A-Z`])', prose) if '$FunctionalDiagramsUpdateCheck' in s]
    if not update:
        fail('the Installation of README.md does not say how to turn off the update check')
    tour = [name for name, path in info['assets'].items() if path.endswith('.nb')]
    if not tour:
        fail('PacletInfo.wl has no notebook among its assets')
    # the line that opens the tour: the README's, if it gives one, else that of the wiki's Installation page for the PDFs
    opens = [c for c in lines if '"AssetLocation", "%s"' % tour[0] in c]
    opens = opens[0] if opens else 'SystemOpen[PacletObject["%s"]["AssetLocation", "%s"]]' % (info['name'], tour[0])
    values.update(INSTALL_LINE=code.line(paclet[0], 'the install line'), NEEDS_LINE=code.line(needs[0], 'the Needs line'),
                  UPDATE_NOTE=inline(update[0], link), TOUR_LINE=code.line(opens, 'the line that opens the guided tour'))

    # A first example: its code blocks, each with the pictures and captions that follow it, then its paragraphs
    steps, after = [], []
    for b in blocks(readme.section('A first example')):
        if b[0] == 'code' and b[1] == 'mathematica':
            steps.append(dict(code=b[2], figures=[]))
        elif b[0] == 'html' and image_of(b[1]) and steps:
            steps[-1]['figures'].append(dict(image=image_of(b[1]), caption=None))
        elif b[0] == 'html' and inner_paragraph(b[1]) and steps and steps[-1]['figures']:
            steps[-1]['figures'][-1]['caption'] = inner_paragraph(b[1])
        elif b[0] == 'para':
            after.append(b[1])
    if not steps:
        fail('the first example of README.md has no code')
    example = []
    for k, s in enumerate(steps):
        figures = ''.join('<figure class="output">%s%s</figure>'
                          % (picture(f['image'][0], f['image'][1]),
                             '<figcaption>%s</figcaption>' % f['caption'] if f['caption'] else '')
                          for f in s['figures'])
        example.append('<div class="step">%s%s</div>' % (code.block(s['code'], 'mathematica', 'the code of the example'
                                                                     + (' (part %d)' % (k + 1) if len(steps) > 1 else '')),
                                                         '<div>%s</div>' % figures if figures else ''))
    values.update(EXAMPLE='\n'.join(example), EXAMPLE_AFTER='\n'.join('<p>%s</p>' % inline(p, link) for p in after))

    # the gallery: its opening sentence and its panels, each a picture with a caption that starts with a bold title
    gallery, opening, pending = [], [], None
    for b in blocks(readme.section('Gallery')):
        if b[0] == 'para':
            opening.append(inline(b[1], link))
        elif b[0] == 'html' and image_of(b[1]):
            pending = image_of(b[1])
        elif b[0] == 'html' and inner_paragraph(b[1]) and pending:
            m = re.fullmatch(r'<b>(.*?)\.?</b>\s*(.*)', inner_paragraph(b[1]), re.S)
            if not m:
                fail('a caption of the gallery of README.md does not start with a bold title')
            gallery.append('<figure class="panel">%s<figcaption><h3>%s</h3><p>%s</p></figcaption></figure>'
                           % (picture(*pending), m.group(1), m.group(2)))
            pending = None
    if not gallery:
        fail('the gallery of README.md has no panels')
    values.update(GALLERY='\n'.join(gallery), GALLERY_NOTE=' '.join(opening))

    # the map of the functions
    fmap = json.loads(read('Documentation', 'function_map.json'))
    map_text = [b[1] for b in blocks(readme.section('Map of the functions')) if b[0] == 'para']
    if not map_text:
        fail('the Map of the functions of README.md has no text')
    first = re.match(r'(.+?\.)(\s|$)', one_line(map_text[0]))
    w, h = svg_size('Documentation', 'function_map.svg')
    values.update(MAP_TEXT=inline(first.group(1), link), MAP_WIDTH=str(w), MAP_HEIGHT=str(h),
                  MAP_COUNTS='%d public names · %d dependencies · version %s'
                             % (len(fmap['nodes']), len(fmap['edges']), esc(fmap['version'])))

    # the scope of the package, the last paragraph of What it does if the README states it
    scope = [b[1] for b in blocks(readme.section('What it does')) if b[0] == 'para' and b[1].startswith('**Scope.**')]
    values.update(SCOPE='<p class="scope">%s</p>' % inline(scope[0], link) if scope else '')

    # Checks, Related software, Citing
    values.update(CHECKS=paragraphs(readme.section('Checks'), link),
                  RELATED=paragraphs(readme.section('Related software'), link))
    citing = blocks(readme.section('Citing'))
    bibtex = [b[2] for b in citing if b[0] == 'code' and b[1] == 'bibtex']
    if not bibtex:
        fail('the Citing of README.md has no BibTeX entry')
    cite_text = [b[1] for b in citing if b[0] == 'para'][:1]
    values.update(BIBTEX=code.block(bibtex[0], 'bibtex', 'the BibTeX entry'),
                  CITE_TEXT='\n'.join('<p>%s</p>' % inline(p, link) for p in cite_text))

    # the facts
    og_w, og_h = png_size('Logo', 'social_preview.png')
    wolfram = re.fullmatch(r'(\d+(?:\.\d+)*)\+', info['wolfram'])
    values.update(VERSION=esc(version), RELEASE_DATE=cite['date'], AUTHOR=esc(cite['author']),
                  AFFILIATION=esc(cite['affiliation']), LICENSE=esc(info['license']), REPO=esc(repo), SITE_URL=esc(site),
                  REQUIREMENT='Mathematica %s or later' % wolfram.group(1) if wolfram else 'Mathematica ' + esc(info['wolfram']),
                  TESTS=str(tests_passed()), TESTS_OUT=esc(repo + '/blob/main/Tests/RunTests.out'),
                  SHOWCASE='Showcase/FunctionalDiagrams_Showcase.pdf', MANUAL='Documentation/FunctionalDiagrams_Manual.pdf',
                  SHOWCASE_PAGES=str(pdf_pages('Showcase', 'FunctionalDiagrams_Showcase.pdf')),
                  MANUAL_PAGES=str(pdf_pages('Documentation', 'FunctionalDiagrams_Manual.pdf')),
                  VIDEO_SECONDS=str(mp4_seconds('Media', 'demo.mp4')), OG_WIDTH=str(og_w), OG_HEIGHT=str(og_h),
                  WAVE_PATH=wave_path())
    values.update({'ICON_' + k: v for k, v in ICONS.items()})

    # the template: the dark colours written once, every placeholder filled, every file it links present
    page = read('Development', 'site_template.html')
    dark = re.search(r':root\[data-theme="dark"\] (\{.*?\})', page, re.S)
    if not dark:
        fail('the template has no block :root[data-theme="dark"]')
    values['DARK_TOKENS'] = dark.group(1).replace('\n', '\n  ')
    page = re.sub(r'\{\{([A-Z_0-9]+)\}\}', lambda m: values[m.group(1)] if m.group(1) in values
                  else fail('the template uses {{%s}}, which make_site.py does not fill' % m.group(1)), page)
    for target in sorted(set(re.findall(r'\s(?:src|href|poster)="([^"#:]+)"', page))):
        if not os.path.exists(os.path.join(ROOT, *target.split('/'))):
            fail('the page links %s, which is not a file of the repository' % target)
    return page


def main():
    out = sys.argv[1] if len(sys.argv) > 1 else os.path.join(ROOT, 'index.html')
    page = build()
    with open(out, 'w', encoding='utf-8', newline='\n') as f:
        f.write(page)
    print('wrote %s (%d bytes)' % (out, len(page.encode('utf-8'))))


if __name__ == '__main__':
    main()
