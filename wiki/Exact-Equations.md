# Exact equations

`ExactEquation` writes the exact equations of many-body theory (Dyson, Bethe–Salpeter, parquet, Schwinger–Dyson, the
vertex of the density, Hedin's equations, the response) between full objects. The same equation can be **drawn**, with
the objects as blobs, or **checked**, with the objects as perturbative series, order by order.

## Drawing an equation

The objects are heads: blobs declared with `DeclareBlob`, and lines. The Dyson equation G = G₀ + G₀ΣG, with the full
propagator as a double line next to the bare one:

```mathematica
Needs["FunctionalDiagrams`"]
DeclareDirectedPropagator[G0]; DeclareLineStyle[G, "DoubleArrow"];
DeclareBlob[SigmaBlob, "\\Sigma"];
dyson = ExactEquation["Dyson", {"G" -> G, "G0" -> G0, "Sigma" -> SigmaBlob}, {x, y}];
DiagramPlot[Last[dyson], {x, y}]
```

![The right-hand side of the Dyson equation: the bare line, and the bare line, the self-energy blob and the full line](images/exact_dyson.svg)

`ExactEquation` gives `lhs == rhs`; `Last` is the right-hand side. The names of the equations and of their objects are
in the usage message, `?ExactEquation`, and in the manual, section *Exact equations*.

## Checking an equation

For a check, the objects are functions of their labels: perturbative series wrapped in `LabelFunction`. The Dyson
equation of the electron gas to second order, with the Green function and the self-energy computed independently:

```mathematica
DeclareGrassmannSource[eta, etabar];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Z0 = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareSymmetric[V]; DeclareInteractionLine[V];
interaction =
  1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
green = -PerturbativeCorrelationFunction[Z0, {psi[x], psibar[y]}, interaction, 2];
sigma = AmputateExternalLegs[ParticleIrreducibleDiagrams[green, 1, {x, y}, {G0}],
  {x, y}, {G0}];
check = ExactEquation["Dyson", {"G" -> LabelFunction[green, {x, y}],
    "G0" -> G0, "Sigma" -> LabelFunction[sigma, {x, y}]}, {x, y},
   "MaximumOrder" -> {V, 2}];
CheckEquation[check, V, 2]
```

```text
{0, 0, 0}
```

`CheckEquation[lhs == rhs, V, n]` gives the residuals of the orders 0 to n: all zero, the equation holds to second
order. `"MaximumOrder" -> {V, 2}` keeps the products of two series from growing beyond the order checked.

## Notes

- A wrong sign or a missing diagram shows up as a non-zero residual at the order where it enters; `DiagramPlot` of the
  residual draws what is missing.
- Four-point equations use the convention of the vertex: fermions leave at x₁, x₃ and enter at x₂, x₄
  ([Conventions](Conventions#self-energy-and-vertices)); `"Channel"` chooses the channel of `"BetheSalpeter"`.
- `"Statistics" -> "Boson"` gives the equations of a real boson (φ⁴ theory).
- The showcase checks the Dyson, Bethe–Salpeter, parquet, Schwinger–Dyson and vertex equations for the electron gas and
  φ⁴ theory, and Hedin's equations for the electron gas, to third order, and draws them.
