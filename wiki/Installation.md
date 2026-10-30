The package needs Mathematica 12.1 or later (for the paclet manager that loads it). It is written in the Wolfram
Language alone: nothing to compile, no other package needed.

## From a release (recommended)

Install the paclet of the latest release, once:

<!-- not run: it downloads the paclet -->
```mathematica
PacletInstall["https://github.com/n4sd4q/FunctionalDiagrams/releases/download/v2.6.0/FunctionalDiagrams-2.6.0.paclet"];
```

Without a connection from Mathematica (a proxy, a cluster node), download `FunctionalDiagrams-2.6.0.paclet` from the
[release page](https://github.com/n4sd4q/FunctionalDiagrams/releases/latest) with a browser and give its path:

<!-- not run: a path of your machine -->
```mathematica
PacletInstall["/path/to/FunctionalDiagrams-2.6.0.paclet"];
```

Then, in every session:

```mathematica
Needs["FunctionalDiagrams`"]
```

In a notebook this prints a banner of two lines (the logo, the version, the license, how to cite, links to the
repository and the manual) and, once a day, looks for a newer release (see [Updating and
removing](#updating-and-removing)); in a script nothing is printed. `` ?FunctionalDiagrams`* `` lists the functions and
`?Name` explains each one.

The paclet also carries the showcase, the manual and the guided tour. To open the manual of the version installed:

<!-- not run: it opens a PDF viewer -->
```mathematica
SystemOpen[PacletObject["FunctionalDiagrams"]["AssetLocation", "Manual"]]
```

(`"Showcase"` opens the showcase.) The guided tour is a notebook, which opens in the running session:

<!-- not run: it opens a notebook -->
```mathematica
NotebookOpen[PacletObject["FunctionalDiagrams"]["AssetLocation", "Examples"]]
```

## From a clone of the repository

A clone is a paclet too, and is loaded as it is:

```sh
git clone https://github.com/n4sd4q/FunctionalDiagrams.git
```

<!-- not run: a path of your machine -->
```mathematica
PacletDirectoryLoad["/path/to/FunctionalDiagrams"];
Needs["FunctionalDiagrams`"]
```

`PacletDirectoryLoad` lasts for the session: put the line in your `init.m` (`FileNameJoin[{$UserBaseDirectory,
"Kernel", "init.m"}]`) to have it in every session. Loading the single file works as well, with or without the paclet
manager:

<!-- not run: a path of your machine -->
```mathematica
Get["/path/to/FunctionalDiagrams/FunctionalDiagrams.wl"]
```

The branch `main` may hold changes made after the latest release; they are listed under *Unreleased* in the
[changelog](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/CHANGELOG.md).

## Which version is loaded

```mathematica
$FunctionalDiagramsVersion
```

```text
2.6.0
```

When several copies are around (an installed paclet and a clone, two versions), these show which ones Mathematica knows
and which file `Needs` loads; their outputs depend on your machine:

<!-- not run: the output depends on your machine -->
```mathematica
#["Location"] & /@ PacletFind["FunctionalDiagrams"]
FindFile["FunctionalDiagrams`"]
```

`FindFile` gives the file that `Needs` loads.

## Updating and removing

In a notebook, loading the package checks once a day whether a newer release is out: it asks `api.github.com` in the
background, so that loading does not wait for the answer, and if there is one, the banner gets a third line with its
version, a link to what is new and the `PacletInstall` line of its paclet, ready to copy. The date of the last check is
kept in `LastUpdateCheck.txt`, in the folder `ApplicationData/FunctionalDiagrams` of `$UserBaseDirectory`. A script
never checks, and a failure (no connection, a proxy) is silent. To load the package without the check:

```mathematica
$FunctionalDiagramsUpdateCheck = False;
Needs["FunctionalDiagrams`"]
```

(put the first line in your `init.m`, see above, to turn the check off for good). To check by hand, at any time:

<!-- not run: it asks GitHub, and its output depends on the latest release -->
```mathematica
FunctionalDiagramsUpdateCheck[]
```

It gives the version loaded, the latest release and the line that installs it, and prints that line when the release
is newer (when GitHub cannot be reached, `$Failed` and a message).

The number of a release says what may change for you: a new major version (the first number) may make code written
for the previous one stop working or give another result, a minor one (the second) only adds, and a patch (the third)
fixes and speeds up, with the same results otherwise; the head of the
[changelog](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/CHANGELOG.md) states the rule, and the changelog
lists every change. To update, install the paclet of the new release as above. `Needs` loads the newest version installed, in a new
session; in the running one, `Needs` does nothing once the package is loaded, and

<!-- not run: it loads the package again -->
```mathematica
Get["FunctionalDiagrams`"]
```

loads it again (from the newest version found). A package loaded again starts afresh: its definitions and the record of your declarations are cleared
(the attributes that declarations gave to your own symbols, such as `Orderless` for a symmetric function, remain), so
declare your theory again after it.

To remove every installed version (a clone is not deleted):

<!-- not run: it uninstalls the paclet -->
```mathematica
PacletUninstall["FunctionalDiagrams"]
```

## Typical problems

- **`Needs::nocont` or `Get::noopen`: cannot open ``FunctionalDiagrams` ``.** The paclet is not installed, or the clone
  was not loaded in this session with `PacletDirectoryLoad` (check the path: it is the folder that contains
  `PacletInfo.wl`).
- **The package does not load in an old Mathematica.** It needs 12.1 or later; `$Version` tells yours.
- **A file of the repository downloaded from GitHub does not load.** Saving the page of a file in a browser saves HTML;
  use *Download raw file* on GitHub, or the release.
- **`FunctionalDiagrams::clash`** when the package is loaded: names of the package are symbols of the context
  ``System` ``, built-in functions of a Mathematica newer than the package, and those are used in place of the
  package's own. Load FunctionalDiagrams in a fresh kernel; if the message remains, please report it in an
  [issue](https://github.com/n4sd4q/FunctionalDiagrams/issues).
- **`DiagramPlot::shdw`** (or another name of the package) when the package is loaded: the name was typed in the
  session before `Needs`, which made a symbol of its own in ``Global` ``, or another package loaded before defines it.
  The package's function is the one used from then on (the context loaded last comes first on the context path);
  ``Remove["Global`DiagramPlot"]`` removes the symbol of ``Global` ``, and loading the package first avoids the
  message.
- **The pictures are black on a dark notebook.** Before Mathematica 14.3 the pictures have the colours of a light
  notebook: see [Light and dark notebooks](Light-and-Dark-Notebooks).
- **Results show `DummyVariable[1, 1]`** in a script: see [Working in scripts](Working-in-Scripts).
- **A function is missing**, or behaves as described in a newer document: compare `$FunctionalDiagramsVersion` with the
  version of the document (the manual says which version it describes on its title page).
