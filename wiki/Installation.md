The package needs Mathematica 12.1 or later. It is written in the Wolfram Language alone: nothing to compile, no other
package needed.

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

In a notebook this prints a short banner and, once a day, looks for a newer release (see [Updating and
removing](#updating-and-removing)); a script prints nothing. `` ?FunctionalDiagrams`* `` lists the functions and `?Name`
explains each one.

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

## Updating and removing

In a notebook, loading the package looks for a newer release once a day, in the background; if there is one, the banner
shows its `PacletInstall` line, ready to copy. A script never checks, and a failure is silent;
[`SECURITY.md`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/SECURITY.md) says what is sent and where the date
of the last check is kept. To load the package without the check:

```mathematica
$FunctionalDiagramsUpdateCheck = False;
Needs["FunctionalDiagrams`"]
```

(put the first line in your `init.m`, see above, to turn the check off for good). To check by hand, at any time:

<!-- not run: it asks GitHub, and its output depends on the latest release -->
```mathematica
FunctionalDiagramsUpdateCheck[]
```

It gives the version loaded, the latest release and the line that installs it.

Releases follow semantic versioning: a new major version may change results or break code, a minor one only adds, a
patch fixes; the [changelog](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/CHANGELOG.md) lists every change. To
update, install the paclet of the new release as above. `Needs` loads the newest version installed, in a new session; in
the running one it does nothing once the package is loaded, and

<!-- not run: it loads the package again -->
```mathematica
Get["FunctionalDiagrams`"]
```

loads it again: declare your theory again after it.

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
  The package's function wins; ``Remove["Global`DiagramPlot"]`` clears the stray symbol, and loading the package first
  avoids the message.
- **The pictures are black on a dark notebook.** Before Mathematica 14.3 the pictures have the colours of a light
  notebook: see the [FAQ](FAQ#why-are-my-pictures-black-on-a-dark-notebook).
- **Results show `DummyVariable[1, 1]`** in a script: see [Working in scripts](Working-in-Scripts).
- **A function is missing**, or behaves as described in a newer document: compare `$FunctionalDiagramsVersion` with the
  version of the document (the manual says which version it describes on its title page).
