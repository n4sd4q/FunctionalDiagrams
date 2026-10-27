# Working in scripts

Long computations (high orders, many checks) are easier to run as scripts than in a notebook. A script is a file of
Wolfram Language code, `run.wl`, run from a shell:

```sh
wolfram -script run.wl         # or: wolframscript -file run.wl
```

The two commands do the same; on some installations only one of them is available. A script prints only what you
`Print`: results of statements are not shown, and the banner of the package is not printed.

## Printing results

In a script `Print` writes the input form, in which the integration variables are `DummyVariable[1, 1]`,
`DummyVariable[1, 2]`, ...; `ToString` writes them as in a notebook, `z1`, `z2`, ...:

```mathematica
Needs["FunctionalDiagrams`"]
DeclareSymmetric[V];
fock = SimplifyDiagrams[IntegralOver[G[x, u] V[u, v] G[u, v] G[v, y], {u, v}]];
Print[ToString[fock]]
```

```text
IntegralOver[G[x, z2] G[z1, y] G[z2, z1] V[z1, z2], {z1, z2}]
```

For a value that you want to read back later, write the input form to a file and `Get` it:
`Put[result, "result.m"]`, then `result = Get["result.m"]`.

## Lines and statements

A statement ends at the end of a line where it is complete:

<!-- not run: it shows a mistake -->
```mathematica
total = OrderPart[green, V, 1]
  + OrderPart[green, V, 2]
```

is two statements, the second a lone `+ OrderPart[...]`. Break long lines after an operator, or inside brackets:

<!-- not run: green is not defined on this page -->
```mathematica
total = OrderPart[green, V, 1] +
  OrderPart[green, V, 2]
```

## Long computations

- Compute only the orders you need (`{k}` in `PerturbativeCorrelationFunction`, `"CouplingOrder"`, `"MaximumOrder"`:
  see [Orders and simplification](Orders-and-Simplification)).
- `AbsoluteTiming[expr]` gives the time and the result; the showcase writes the times of its main steps in
  `Showcase/figures/values.txt`.
- Pictures can be written to files from a script: `Export["green.pdf", DiagramPlot[green, {x, y}]]` (the front end is
  started in the background for it), or `DiagramToTikZ` to a `.tex` file, see [Pictures in LaTeX](Pictures-in-LaTeX).

## Examples in the repository

[`Examples.wl`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Examples.wl) (the guided tour),
[`Showcase/make_figures.wl`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Showcase/make_figures.wl) (everything
in the showcase) and [`Tests/RunTests.wl`](https://github.com/n4sd4q/FunctionalDiagrams/blob/main/Tests/RunTests.wl)
(the test suite) are scripts; each one loads the package from the repository with `Get`.
