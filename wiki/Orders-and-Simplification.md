Every result of the package is a sum of terms in canonical form: one term per diagram, with its coefficient. The
electron gas of the README:

```mathematica
Needs["FunctionalDiagrams`"]
DeclareGrassmannSource[eta, etabar];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Z0 = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareSymmetric[V]; DeclareInteractionLine[V]; DeclareDirectedPropagator[G0, G];
interaction =
  1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
green = -PerturbativeCorrelationFunction[Z0, {psi[x], psibar[y]}, interaction, 2];
```

## Selecting and counting orders

`OrderPart[expr, V, k]` keeps the terms with k factors of the head `V` (for a coupling constant, the terms
proportional to its k-th power); `DiagramTerms` lists the terms:

```mathematica
Table[Length[DiagramTerms[OrderPart[green, V, k]]], {k, 0, 2}]
```

```text
{1, 2, 10}
```

`OrderPart[expr, V, {1, 2}]` keeps several orders. It is often faster not to compute the orders you do not need:

- `PerturbativeCorrelationFunction[Z0, fields, interaction, n]` takes `n`, `{k}` (order k only) or `{kmin, kmax}`;
- `ConnectedCorrelationFunction` and `ConnectedVacuumDiagrams` take `"CouplingOrder" -> {g, k}` (the same as
  `OrderPart[..., g, k]`, faster);
- `FunctionalDerivative[F, s1, ..., "MaximumOrder" -> {V, n}]` drops the terms above order n after each derivative;
- `ExactEquation`, `ExpandTrace`, `BetheSalpeterLadder` and `DysonSeries` take `"MaximumOrder"` as well.

## Comparing results

Two results are equal when their difference simplifies to zero:

```mathematica
Phi = LuttingerWardFunctional[Z0, interaction, 2, G0 -> G];
SimplifyDiagrams[SelfEnergy[Phi, G, {x, y}] - FunctionalDerivative[Phi, G[y, x]]]
```

```text
0
```

The self-energy Σ[G] of the Luttinger–Ward functional is the skeleton part of the perturbative self-energy, with
G₀ → G:

```mathematica
sigmaPerturbative = AmputateExternalLegs[
   ParticleIrreducibleDiagrams[green, 1, {x, y}, {G0}], {x, y}, {G0}];
SimplifyDiagrams[(FunctionalDerivative[Phi, G[y, x]] /. G -> G0) -
  SkeletonDiagrams[sigmaPerturbative, {x, y}, {G0}]]
```

```text
0
```

Compare with `SimplifyDiagrams[a - b] === 0`, never by the look of the expressions: the integration variables are
renamed `z1, z2, ...` in the order of a canonical labelling, not in the order in which you wrote them.

## When to call SimplifyDiagrams

The functions of the package return their results in canonical form. An expression you write or change yourself (a
substitution, a product, a sum of results) is not: two integrals that differ only by the names of their variables stay
apart,

```mathematica
a = IntegralOver[G[x, u] V[u, v] G[v, y], {u, v}];
b = IntegralOver[G[x, s] V[t, s] G[t, y], {s, t}];
a + b
```

```text
IntegralOver[G[t, y] G[x, s] V[s, t], {s, t}] + IntegralOver[G[v, y] G[x, u] V[u, v], {u, v}]
```

until `SimplifyDiagrams` brings them to canonical form and adds them up:

```mathematica
SimplifyDiagrams[a + b]
```

```text
2 IntegralOver[G[x, z2] G[z1, y] V[z1, z2], {z1, z2}]
```

A substitution needs the new head declared like the old one: with `G0` symmetric and `G` not, the terms of `G` with
their labels exchanged are not recognized as equal. The rule `G0 -> G` of `LuttingerWardFunctional` and
`PerturbativeFullVertex` gives the message `FunctionalDiagrams::rulesymmetry` then; a substitution of your own,
`expr /. G0 -> G`, is not checked.

## Classes of diagrams

`ConnectedDiagrams`, `DropVacuumBubbles`, `ParticleIrreducibleDiagrams`, `SkeletonDiagrams`, `ReducibleDiagrams`,
`IrreducibleDiagrams`, `DropClosedLoops` and `AmputateExternalLegs` select diagrams by cutting lines; each takes the
external labels and the heads of the lines that may be cut. The manual, section *Classes of diagrams*, has them all.
