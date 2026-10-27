`DiagramPlot` and `DrawDiagram` draw with black, white and greys only, and follow the appearance of the notebook:

- **Mathematica 14.3 or later**: lines and text are dark on a light notebook and light on a dark one (through
  `LightDarkSwitched`); what hides the lines under it (the inside of a blob, of an external point, the clearing behind
  the label of a hatched blob) takes the colour of the background, and the grey fills are darker on a dark notebook.
- **Before 14.3**: the pictures have the colours of a light notebook, also on a dark one.

The option `"Appearance"` forces one look, on a background of its own (white, or the dark grey of a dark notebook):

```mathematica
Needs["FunctionalDiagrams`"]
Options[DiagramPlot, "Appearance"]
```

```text
{Appearance -> Automatic}
```

`"Appearance" -> "Light"` or `"Dark"` is meant for a picture exported for a page of the other kind, for example a
picture for slides with a dark background:

```mathematica
DeclareSource[J]; DeclareField[phi, J]; DeclareSymmetric[D0]; DeclareCoupling[lambda];
Zb = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
phi4 = lambda/24 IntegralOver[phi[z]^4, {z}];
propagator = PerturbativeCorrelationFunction[Zb, {phi[a], phi[b]}, phi4, 1];
Export["propagator_dark.png", DiagramPlot[propagator, {a, b}, "Appearance" -> "Dark"]]
```

```text
propagator_dark.png
```

For a whole session, `SetOptions` sets the default of both functions:

```mathematica
SetOptions[{DiagramPlot, DrawDiagram}, "Appearance" -> "Dark"];
```

## TikZ and the web

`DiagramToTikZ` ignores `"Appearance"`: its code is for white pages, black lines on white. A picture made from it for
a page that may be light or dark, a README or a wiki, reads on both when it is placed on a white card: the pictures of
the README and of this wiki are SVG files with a white rounded rectangle under the drawing
([`Development/make_readme_figure.sh`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Development/make_readme_figure.sh)
and [`Development/check_wiki.wl`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Development/check_wiki.wl)
show how). See [Pictures in LaTeX](Pictures-in-LaTeX).
