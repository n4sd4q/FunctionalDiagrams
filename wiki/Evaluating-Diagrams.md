The diagrams of the package are functions of labels. Two families of evaluations turn them into formulas: in momentum
and Matsubara frequency at finite temperature, and loop integrals in dimensional regularization at zero temperature.
Both route the momenta through the lines themselves; the evaluation part is deliberately small, and covers the
integrals that come up in the examples, not general loop calculations.

## Matsubara sums

The electron gas, and its self-energy at first order:

```mathematica
Needs["FunctionalDiagrams`"]
DeclareGrassmannSource[eta, etabar];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Z0 = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareSymmetric[V]; DeclareInteractionLine[V]; DeclareDirectedPropagator[G0];
interaction =
  1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
green = -PerturbativeCorrelationFunction[Z0, {psi[x], psibar[y]}, interaction, {1}];
sigma = AmputateExternalLegs[green, {x, y}, {G0}];
```

The rules say what each line is: `G0` a fermion line with the dispersion ξ, G₀(k, iω) = 1/(iω − ξ_k), and `V` an
instantaneous interaction v(q). The external momentum and frequency enter at `y` and leave at `x`:

```mathematica
rules = {G0 -> {"Fermion", xi}, V -> {"Instantaneous", v}};
DeclareMatsubaraFrequency[w, "Fermion"];
EvaluateMatsubara[sigma, {y -> {k, I w}, x -> {-k, -I w}}, rules,
  "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]
```

```text
-MomentumSum[FermiFunction[xi[p]] v[k - p], {p}] + 2 MomentumSum[FermiFunction[xi[p]], {p}] v[0]
```

The Hartree term, with the factor 2 of the spin of its closed loop, and the Fock term: the frequency sums are done (by
residues), the momentum integrals stay as `MomentumSum`. The density correlation function at zero order is the bubble:

```mathematica
DeclareMatsubaraFrequency[nu, "Boson"];
bubble = ConnectedCorrelationFunction[Z0, {psibar[a], psi[a], psibar[b], psi[b]},
   interaction, {0}];
EvaluateMatsubara[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules,
  "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]
```

```text
               FermiFunction[xi[p]] - FermiFunction[xi[p + q]]
-2 MomentumSum[-----------------------------------------------, {p}]
                          I nu + xi[p] - xi[p + q]
```

This is **minus** the Lindhard function χ₀ of the textbooks: the package's ⟨ρρ⟩_c has the sign of a correlation
function ([Conventions](Conventions#matsubara-frequencies)). `AnalyticContinuation`, `RetardedImaginaryPart` and
`ZeroTemperatureLimit` continue the result to real frequency and take its limits; `CheckCuttingRules` compares the
imaginary part with the cutting rules at finite temperature. The manual, section *Evaluation: Matsubara sums*, has the
second-order self-energy, phonons, and the grand potential of the 2PI effective action.

## Dimensional regularization

φ⁴ theory with a mass m, and its self-energy at first order, the tadpole:

```mathematica
DeclareSource[J]; DeclareField[phi, J]; DeclareSymmetric[D0];
Zb = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
phi4 = lambda/24 IntegralOver[phi[z]^4, {z}];
propagator = PerturbativeCorrelationFunction[Zb, {phi[a], phi[b]}, phi4, {1}];
tadpole = AmputateExternalLegs[propagator, {a, b}, {D0}]
```

```text
  1
-(-) (lambda D0[a, a] DeltaFunction[a, b])
  2
```

`DiagramLoopIntegral` writes it in momentum space, with the line `D0` a scalar propagator 1/(q² + m²), and
`IntegrateLoops` does the loop integral in d = 4 − 2ε:

```mathematica
loop = DiagramLoopIntegral[tadpole, {a -> p, b -> -p}, {D0 -> {"Scalar", m}},
  "LoopMomenta" -> {k}]
```

```text
  1
-(-) (lambda LoopIntegral[ScalarPropagator[k, m], {k}])
  2
```

```mathematica
EpsilonExpand[IntegrateLoops[loop, eps], eps, 0]
```

```text
                                                 2
                                 2              m
                         lambda m  Log[---------------------]
        2           2                                      2
lambda m    lambda m                   RenormalizationScale
--------- + ---------- - ------------------------------------
      2              2                       2
 32 Pi      32 eps Pi                   32 Pi
```

Every loop carries the MS-bar measure μ^(2ε) (e^γE/4π)^ε ∫d^dk/(2π)^d (`RenormalizationScale` is μ), so no γE or ln 4π
appears, and the pole is the MS-bar counterterm (`PolePart`). The manual, section *Evaluation: dimensional
regularization*, goes on to two loops, the infrared rearrangement and the β function of φ⁴ theory; section *Evaluation:
QED at one loop* does the Dirac algebra, the one-loop integrals with Feynman parameters, and the self-energy, vacuum
polarization and vertex of QED.
