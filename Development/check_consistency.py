#!/usr/bin/env python3
"""Checks that the generated files are up to date and that the public functions are documented (no Mathematica needed):
FunctionalDiagrams.wl is the concatenation of Development/src, Examples.nb is generated from Examples.wl, and every public
name has a usage message and an entry in the manual; one version is stated in the package, PacletInfo.wl, CITATION.cff,
the install line of the README, the title of the manual and the check of the tests; the map of the functions
(Documentation/function_map.*) is drawn from the current public names and version. Run from any folder:
python3 Development/check_consistency.py"""
import json, os, re, subprocess, sys, tempfile

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

print('\n'.join(problems) if problems else 'consistent: package, notebook, %d public names documented' % len(public))
sys.exit(1 if problems else 0)
