#!/usr/bin/env python3
"""Checks that the generated files are up to date and that the public functions are documented (no Mathematica needed):
FunctionalDiagrams.wl is the concatenation of Development/src, Examples.nb is generated from Examples.wl, and every public
name has a usage message and an entry in the manual; one version is stated in the package, PacletInfo.wl, CITATION.cff,
the install line of the README, the title of the manual and the check of the tests; the map of the functions
(Documentation/function_map.*) is drawn from the current public names and version; the pages of the wiki (wiki/) state
that version, use only names of the package that exist, and link pages, images and files that exist (their code is run
by Development/check_wiki.wl, which needs Mathematica). Run from any folder:
python3 Development/check_consistency.py"""
import json, os, re, subprocess, sys, tempfile, unicodedata

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
problems = []

# the package and its sources (the list of build.sh)
build = open(os.path.join(root, 'Development', 'build.sh')).read()
sources = re.findall(r'src/\S+\.wl', build)
assembled = ''.join(open(os.path.join(root, 'Development', s), encoding='utf-8').read() for s in sources)
if assembled != open(os.path.join(root, 'FunctionalDiagrams.wl'), encoding='utf-8').read():
    problems.append('FunctionalDiagrams.wl is not the concatenation of Development/src: run Development/build.sh')

# the notebook of the guided tour
with tempfile.TemporaryDirectory() as tmp:
    nb = os.path.join(tmp, 'Examples.nb')
    subprocess.run([sys.executable, os.path.join(root, 'Development', 'make_notebook.py'), os.path.join(root, 'Examples.wl'), nb],
                   check=True, stdout=subprocess.DEVNULL)
    if open(nb, encoding='utf-8').read() != open(os.path.join(root, 'Examples.nb'), encoding='utf-8').read():
        problems.append('Examples.nb is not generated from Examples.wl: run Development/make_notebook.py')

# public names, usage messages, manual
package = open(os.path.join(root, 'FunctionalDiagrams.wl'), encoding='utf-8').read()
public = re.findall(r'"([A-Za-z][A-Za-z0-9]*)"', re.search(r'\$publicNames = \{(.*?)\};', package, re.S).group(1))
usages = set(re.findall(r'^([A-Za-z][A-Za-z0-9]*)::usage', package, re.M))
manual = open(os.path.join(root, 'Documentation', 'FunctionalDiagrams_Manual.tex'), encoding='utf-8').read()
documented = set(re.findall(r'\\begin\{function\}\{([A-Za-z][A-Za-z0-9]*)\}', manual))
for name in public:
    if name not in usages:
        problems.append(name + ' has no usage message')
    if name not in documented and not name.startswith('$'):
        problems.append(name + ' has no entry in the manual')
for name in sorted(usages - set(public) - {'FunctionalDiagrams'}):
    problems.append(name + ' has a usage message but is not in $publicNames')

# one version everywhere: the package, the paclet, the citation, and the places easy to forget at a release (the
# install line of the README, the manual, the check of the tests); '?' where the version is not found
def stated(name, pattern, flags=0):
    m = re.search(pattern, open(os.path.join(root, name), encoding='utf-8').read(), flags)
    return m.group(1) if m else '?'

versions = {
    'FunctionalDiagrams.wl': re.search(r'\$FunctionalDiagramsVersion = "([^"]+)"', package).group(1),
    'PacletInfo.wl': stated('PacletInfo.wl', r'"Version" -> "([^"]+)"'),
    'CITATION.cff': stated('CITATION.cff', r'^version: (\S+)', re.M),
    'README.md (release of PacletInstall)': stated('README.md', r'PacletInstall\["https://\S+/releases/download/v([^/"]+)/'),
    'README.md (paclet of PacletInstall)': stated('README.md', r'PacletInstall\["https://\S+/FunctionalDiagrams-([^"]+)\.paclet"\]'),
    'the title of the manual': stated(os.path.join('Documentation', 'FunctionalDiagrams_Manual.tex'),
                                      r'SISSA \\textperiodcentered\{\} version (\S+) \\textperiodcentered'),
    'Tests/RunTests.wl': stated(os.path.join('Tests', 'RunTests.wl'),
                                r'check\["package loaded again", \$FunctionalDiagramsVersion, "([^"]+)"\]'),
}
if len(set(versions.values())) > 1:
    problems.append('different versions: ' + ', '.join('%s %s' % kv for kv in versions.items()))

# the badge of the README: the number of checks of the last run of the tests
readme = open(os.path.join(root, 'README.md'), encoding='utf-8').read()
badge = re.search(r'tests-(\d+)%20passed', readme)
ran = re.search(r'^(\d+) passed, 0 failed', open(os.path.join(root, 'Tests', 'RunTests.out'), encoding='utf-8').read(), re.M)
if not badge or not ran or badge.group(1) != ran.group(1):
    problems.append('the badge of README.md (%s passed) does not match Tests/RunTests.out (%s passed, 0 failed)'
                    % (badge.group(1) if badge else '?', ran.group(1) if ran else '?'))

# the map of the functions (README): read from the current public names, drawn from the current data
fmap = json.load(open(os.path.join(root, 'Documentation', 'function_map.json'), encoding='utf-8'))
if [n['name'] for n in fmap['nodes']] != public or fmap['version'] != versions['FunctionalDiagrams.wl']:
    problems.append('Documentation/function_map.json is out of date: run Development/make_function_map.wl '
                    'and Development/make_function_map.py')
index = {n['name']: i for i, n in enumerate(fmap['nodes'])}
edges = sorted((index[a], index[b]) for a, b in fmap['edges'])
for name in ('function_map.svg', 'function_map.html'):
    text = open(os.path.join(root, 'Documentation', name), encoding='utf-8').read()
    m = re.search(r'The (\d+) public names of the package in \d+ groups, and the (\d+) dependencies', text)
    stale = not m or (int(m.group(1)), int(m.group(2))) != (len(fmap['nodes']), len(edges))
    if name.endswith('.html'):
        drawn = sorted((int(a), int(b)) for a, b in re.findall(r'class="edge[^"]*" data-s="(\d+)" data-t="(\d+)"', text))
        stale = stale or drawn != edges or '(version %s)' % fmap['version'] not in text
    if stale:
        problems.append('Documentation/%s is not drawn from function_map.json: run Development/make_function_map.py' % name)

# the pages of the wiki (wiki/, published by .github/workflows/wiki.yml; Development/check_wiki.wl runs their code): the
# version wherever it is written, the names of the package in their code, the links to pages, images and files
def code_tokens(text):
    """the tokens of Wolfram Language code (symbols and single characters), outside strings and comments"""
    out, i, n = [], 0, len(text)
    while i < n:
        if text.startswith('(*', i):
            depth, i = 1, i + 2
            while i < n and depth:
                if text.startswith('(*', i):
                    depth, i = depth + 1, i + 2
                elif text.startswith('*)', i):
                    depth, i = depth - 1, i + 2
                else:
                    i += 1
        elif text[i] == '"':
            i += 1
            while i < n and text[i] != '"':
                i += 2 if text[i] == '\\' else 1
            i += 1
        else:
            m = re.compile(r'\$?[A-Za-z][A-Za-z0-9]*|.', re.S).match(text, i)
            out.append(m.group(0))
            i = m.end()
    return out

def code_symbols(text):
    """the symbols of Wolfram Language code, outside strings and comments"""
    return [s for s in code_tokens(text) if re.match(r'\$?[A-Za-z]', s)]

def slug(heading):
    """the anchor GitHub gives a heading: lower case, the letters, marks, decimal digits, hyphens, underscores and spaces
    kept (a superscript ⁴ is dropped), the spaces made hyphens"""
    kept = ''.join(c for c in heading.strip().lower()
                   if c in '-_ ' or unicodedata.category(c)[0] in 'LM' or unicodedata.category(c) == 'Nd')
    return kept.replace(' ', '-')

wiki = os.path.join(root, 'wiki')
if os.path.isdir(wiki):
    version = versions['FunctionalDiagrams.wl']
    pages = {f[:-3]: open(os.path.join(wiki, f), encoding='utf-8').read() for f in sorted(os.listdir(wiki)) if f.endswith('.md')}
    images = set(os.listdir(os.path.join(wiki, 'images'))) if os.path.isdir(os.path.join(wiki, 'images')) else set()
    anchors = {name: {slug(h) for h in re.findall(r'^#+ (.*)$', re.sub(r'```.*?```', '', text, flags=re.S), re.M)}
               for name, text in pages.items()}
    # the symbols of the package's code (outside its strings and comments) are the built-in ones it uses; these few
    # are used only on the pages (the last ones as names not to give to a propagator)
    known = set(public) | set(code_symbols(package)) | {
        'FunctionalDiagrams', 'Needs', 'Get', 'Put', 'Export', 'Print', 'ToString', 'AbsoluteTiming', 'SetOptions',
        'Options', 'SystemOpen', 'NotebookOpen', 'FindFile', 'PacletInstall', 'PacletFind', 'PacletObject', 'PacletUninstall',
        'PacletDirectoryLoad', '$UserBaseDirectory', '$Version', 'Times', 'LightDarkSwitched', 'Remove',
        'C', 'D', 'E', 'I', 'K', 'N', 'O', 'Beta', 'Gamma', 'GreenFunction'}
    code = {name: re.findall(r'```mathematica\n(.*?)```', text, re.S) for name, text in pages.items()}
    # inline code, except paths and file names
    inline = {name: [m[1] for m in re.findall(r'(`+)(.+?)(?<!`)\1(?!`)', re.sub(r'```.*?```', '', text, flags=re.S))
                     if not re.search(r'/|\.[a-z]+$', m[1])]
              for name, text in pages.items()}
    # the symbols that the pages declare or define: the user's own (propagators, fields, results)
    defined = set()
    for snippet in [b for blocks in code.values() for b in blocks] + [s for spans in inline.values() for s in spans]:
        for args in re.findall(r'\bDeclare[A-Za-z]*\[([^\]]*)\]', snippet):
            defined |= set(code_symbols(args))
        defined |= set(re.findall(r'(?m)(?:^|;)\s*([A-Za-z][A-Za-z0-9]*)(?:\[[^\]]*\])?\s*:?=(?!=)', snippet))
    def checked_symbols(snippet):
        """the heads and $-symbols of a snippet of code, and the snippet itself if it is a name of four or more
        characters (shorter ones, G0, Dph, W, are the user's propagators and kernels)"""
        tokens = [s for s in code_tokens(snippet) if not s.isspace()]
        found = {s for s, after in zip(tokens, tokens[1:] + ['']) if after == '[' and re.match(r'\$?[A-Za-z]', s)}
        found |= {s for s in code_symbols(snippet) if s.startswith('$')}
        if re.fullmatch(r'\$?[A-Za-z][A-Za-z0-9]{3,}', snippet.strip()):
            found.add(snippet.strip())
        return found
    for name, text in pages.items():
        where = 'wiki/%s.md' % name
        for v in sorted(set(re.findall(r'(?<![\d.])v?(\d+\.\d+\.\d+)(?![\d.])', text)) - {version}):
            problems.append('%s states the version %s, not %s' % (where, v, version))
        symbols = set()
        for snippet in code[name] + inline[name]:
            symbols |= checked_symbols(snippet)
        for s in sorted(symbols):
            if (s[0].isupper() or s[0] == '$') and s not in known and s not in defined:
                problems.append('%s: %s is neither a public name of the package nor a name the pages declare '
                                '(a built-in symbol used only in the wiki goes in the list of check_consistency.py)'
                                % (where, s))
        for image, target in re.findall(r'(!?)\[[^\]]*\]\(([^)\s]+)\)', re.sub(r'```.*?```', '', text, flags=re.S)):
            if image:
                if not (target.startswith('images/') and target[len('images/'):] in images):
                    problems.append('%s shows %s, which is not in wiki/images' % (where, target))
                continue
            repo_file = re.match(r'https://(?:github\.com/n4sd4q/FunctionalDiagrams/(?:blob|tree)/main|'
                                 r'n4sd4q\.github\.io/FunctionalDiagrams)/([^#?]+)', target)
            if repo_file:
                if not os.path.exists(os.path.join(root, repo_file.group(1))):
                    problems.append('%s links %s, which is not a file of the repository' % (where, target))
            elif not re.match(r'[a-z]+:', target):
                page, _, anchor = target.partition('#')
                page = page or name
                if page not in pages:
                    problems.append('%s links the page %s, which is not in wiki/' % (where, page))
                elif anchor and anchor not in anchors[page]:
                    problems.append('%s links %s, which is not a heading of wiki/%s.md' % (where, target, page))
    used = set(re.findall(r'\]\(images/([^)\s]+)\)', '\n'.join(pages.values())))
    for image in sorted(images - used):
        problems.append('wiki/images/%s is not shown by any page' % image)
    listed = set(re.findall(r'\]\(([A-Z][A-Za-z-]*)\)', pages.get('_Sidebar', '')))
    for name in sorted(set(pages) - listed - {'_Sidebar', '_Footer'}):
        problems.append('wiki/%s.md is not in wiki/_Sidebar.md' % name)

print('\n'.join(problems) if problems else 'consistent: package, notebook, %d public names documented, wiki' % len(public))
sys.exit(1 if problems else 0)
