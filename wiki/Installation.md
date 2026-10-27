The package needs Mathematica 12.1 or later (for the paclet manager that loads it). It is written in the Wolfram
Language alone: nothing to compile, no other package needed.

## From a release (recommended)

Install the paclet of the latest release, once:

<!-- not run: it downloads the paclet -->
```mathematica
PacletInstall["https://github.com/n4sd4q/FunctionalDiagrams/releases/download/v2.4.0/FunctionalDiagrams-2.4.0.paclet"];
```

Without a connection from Mathematica (a proxy, a cluster node), download `FunctionalDiagrams-2.4.0.paclet` from the
[release page](https://github.com/n4sd4q/FunctionalDiagrams/releases/latest) with a browser and give its path:

<!-- not run: a path of your machine -->
```mathematica
PacletInstall["/path/to/FunctionalDiagrams-2.4.0.paclet"];
```

Then, in every session:

```mathematica
Needs["FunctionalDiagrams`"]
```

In a notebook this prints a banner of two lines (the logo, the version, the license, how to cite, links to the
repository and the manual); in a script nothing is printed. `` ?FunctionalDiagrams`* `` lists the functions and `?Name`
explains each one.

The paclet also carries the showcase, the manual and the guided tour. To open the manual of the version installed:

<!-- not run: it opens a PDF viewer -->
```mathematica
SystemOpen[PacletObject["FunctionalDiagrams"]["AssetLocation", "Manual"]]
```

(`"Showcase"` and `"Examples"` open the other two.)

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
2.4.0
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

To update, install the paclet of the new release as above. `Needs` loads the newest version installed, in a new
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
- **`FunctionalDiagrams::clash`** when the package is loaded: names of the package are already defined in another
  context (a function of a newer Mathematica, for example), and the package's own versions are not used. Load
  FunctionalDiagrams in a fresh kernel; if the message remains, please report it in an
  [issue](https://github.com/n4sd4q/FunctionalDiagrams/issues).
- **`DiagramPlot::shdw`** (or another name of the package) when the package is loaded: the name was typed in the
  session before `Needs`, which made a symbol of its own in ``Global` ``. The package's function is the one used from
  then on; ``Remove["Global`DiagramPlot"]`` removes the other, and loading the package first avoids the message.
- **The pictures are black on a dark notebook.** Before Mathematica 14.3 the pictures have the colours of a light
  notebook: see [Light and dark notebooks](Light-and-Dark-Notebooks).
- **Results show `DummyVariable[1, 1]`** in a script: see [Working in scripts](Working-in-Scripts).
- **A function is missing**, or behaves as described in a newer document: compare `$FunctionalDiagramsVersion` with the
  version of the document (the manual says which version it describes on its title page).
