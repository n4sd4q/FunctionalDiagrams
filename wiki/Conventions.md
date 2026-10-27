# Conventions

The conventions of the package in one place, as a user meets them. The manual gives each one with its derivation:
the sections are named in *italics* below.

## Expressions

- `IntegralOver[f, {x, y, ...}]` is the integral, or sum, of `f` over the labels `x, y, ...`; a label stands for
  everything a field depends on (position, imaginary time, spin, any index) and must be a symbol.
  `DeltaFunction[x, y]` is the delta function, integrated automatically.
- Grassmann-odd factors are multiplied with `**`, in their order; `Times` would lose the order and the sign.
- `FunctionalDerivative[F, s1, s2, ..., sn]` is the left derivative δ/δs1 δ/δs2 … δ/δsn F: the **rightmost acts first**.
- Results are in canonical form: the integration variables are renamed `z1, z2, ...` and equal diagrams are added up,
  so `SimplifyDiagrams[a - b]` is `0` exactly when `a` and `b` are the same sum of diagrams (manual: *Canonical form*).

## Fermions

The free propagator is G₀(x, y) = −⟨T ψ(x) ψ̄(y)⟩₀, with the generating functional and the fields

```mathematica
Needs["FunctionalDiagrams`"]
DeclareGrassmannSource[eta, etabar];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Z0 = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
GaussianExpectationValue[Z0, {psi[x], psibar[y]}]
```

```text
-G0[x, y]
```

so ψ ↔ δ/δη̄ and ψ̄ ↔ −δ/δη (the `-1` of `DeclareField`), and the Green function is
`-PerturbativeCorrelationFunction[Z0, {psi[x], psibar[y]}, interaction, n]`. `G[x, y]` is drawn as an arrow **from `y`
to `x`**. The two-body interaction is S_int = ½ ∫ V(x, y) ψ̄(x) ψ̄(y) ψ(y) ψ(x), and the package expands exp(−S_int):

```mathematica
DeclareSymmetric[V]; DeclareInteractionLine[V]; DeclareDirectedPropagator[G0, G];
interaction =
  1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
```

Closed fermion loops and exchange terms carry their signs by themselves (manual: *Conventions for many-body problems*).

## Weights of the pictures

The number written in front of a picture, times the values of its elements, is the term. A propagator stands for
itself; an interaction line (`DeclareInteractionLine`) for −V; a vertex for minus its coupling (`DeclareCoupling`); a
local vertex (`DeclareLocalVertex`) for minus its kernel. The minus signs come from expanding exp(−S_int). So the Fock
self-energy has the weight +1, the Hartree self-energy −1 (its closed loop), the tadpole of φ⁴ theory +½; a diagram of G
or Σ with n_L closed fermion loops has the weight (−1)^n_L. Only the weights shown with the pictures follow this rule:
the expressions keep their coefficients, and `"Weights" -> "Coefficients"` shows those. See
[Reading the weights](Reading-the-Weights) and the manual, *The weights*.

## Self-energy and vertices

- Φ[G] (`LuttingerWardFunctional`) is minus the sum of the two-particle-irreducible vacuum diagrams, with G₀ → G.
- Σ(x, y) = δΦ/δG(y, x): `FunctionalDerivative[Phi, G[y, x]]` (the shortcut `SelfEnergy[Phi, G, {x, y}]`).
- Γ⁰(x₁, x₂; x₃, x₄) = δΣ(x₁, x₂)/δG(x₄, x₃) = δ²Φ/δG(x₄, x₃)δG(x₂, x₁):
  `FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]]` (the shortcut `IrreducibleVertex`). Fermions **leave at x₁, x₃
  and enter at x₂, x₄**; the pictures put the legs counterclockwise from the top left: x₁ (out), x₂ (in), x₃ (out),
  x₄ (in).
- The full vertex F: ⟨ψ(1′) ψ̄(2′) ψ̄(4′) ψ(3′)⟩_c = G(1′, 1) G(2, 2′) F(1, 2; 3, 4) G(4, 4′) G(3′, 3).
- A pair of propagators between two vertices is written L₀: in the particle–hole channel
  L₀(x₁, x₂; x₃, x₄) = G(x₁, x₄) G(x₃, x₂), so that F = Γ⁰ + Γ⁰L₀F.
- Real bosons (`"Statistics" -> "Boson"`, G = ⟨φφ⟩ declared symmetric): Σ = −2 δΦ/δG(x, y), Γ⁰ = −2 δΣ/δG = 4 δ²Φ/δGδG, and
  a pair of lines in a channel carries −½. Complex bosons: `"Statistics" -> "ComplexBoson"`.

Manual: *Self-energy, Luttinger–Ward functional and vertices*.

## Channels

For fermions, with the legs above:

| channel | name in the package | pairs of legs |
|---|---|---|
| particle–hole (ph) | `"ParticleHole"` | (x₁ x₂ \| x₃ x₄) |
| transverse particle–hole (ph̄) | `"TransverseParticleHole"` | (x₁ x₄ \| x₃ x₂) |
| particle–particle (pp) | `"ParticleParticle"` | (x₁ x₃ \| x₂ x₄) |

Γ⁰ is irreducible in ph: Γ⁰ = F − R_ph = Γ_irr + R_ph̄ + R_pp, with Γ_irr the fully irreducible vertex (the bare vertex up
to third order). For real bosons and superfields the channels are `"s"` (a b | c d), `"t"` (a c | b d) and `"u"`
(a d | b c). The particle–particle kernel of fermions is a derivative with respect to the anomalous propagators, so
it needs the superfield below. Manual: *Two-particle channels, Bethe–Salpeter ladders and the parquet*;
[Particle–particle diagrams](Particle-Particle-Diagrams).

## The vertex of the density and Hedin's equations

- Λ(x₁, x₂; z) = −δG⁻¹(x₁, x₂)/δU(z) for a potential U coupled to ρ = ψ̄ψ (`PerturbativeDensityVertex`):
  ⟨ψ(x₁) ψ̄(x₂) ρ(z)⟩_c = L₀Λ, and Λ = γ + Γ⁰L₀Λ = γ + FL₀γ with γ = δ(x₁, z) δ(x₂, z); Σ = Σ_H − VGΛ.
- Hedin: W = V − V⟨ρρ⟩_cV, P = −(proper part of ⟨ρρ⟩_c) = L₀Λ̃, Σ = Σ_H − GWΛ̃, with Λ̃ the proper vertex.
- Hubbard–Stratonovich (`HubbardStratonovich`): ⟨φφ⟩₀ = V and the coupling −i∫φρ.

Manual: *Exact equations*.

## Superfields

`"Statistics" -> "Superfield"`: the Nambu field 𝛙 = (ψ, ψ̄) is one Grassmann field with one source. Its propagator
𝒢 = −⟨T𝛙𝛙⟩ is antisymmetric (`DeclareAntisymmetric`), with the blocks G, 𝒢_ψψ = −⟨Tψψ⟩ and 𝒢_ψ̄ψ̄ = −⟨Tψ̄ψ̄⟩; derivatives
with respect to it are antisymmetrised. 𝚺 = −2 δΦ/δ𝒢, with the gap functions Δ = 𝚺_ψ̄ψ̄ and Δ̄ = 𝚺_ψψ; Γ⁰ = 4 δ²Φ/δ𝒢δ𝒢,
whose channels s, t, u are ph, pp and ph̄. `SuperfieldComponents` projects on ψ and ψ̄. In the pictures 𝒢_ψψ has two
arrow heads pointing to its ends (`"DivergingArrows"`) and 𝒢_ψ̄ψ̄ two pointing to its middle (`"ConvergingArrows"`).
Manual: *Superfields*.

## Matsubara frequencies

G₀(k, iω) = 1/(iω − ξ_k). The density correlation function ⟨ρρ⟩_c of the package is **minus** the Lindhard function χ₀,
and its boson propagator ⟨φφ⟩₀ is minus the 𝒟₀ = −⟨Tφφ⟩₀ of the textbooks. Each closed fermion loop gets the factor
`"SpinDegeneracy"` on evaluation. Manual: *Evaluation: Matsubara sums*, and [Evaluating diagrams](Evaluating-Diagrams).

## Dimensional regularization

Euclidean, a line is 1/(q² + m²) (`ScalarPropagator`), d = 4 − 2ε, every loop μ^(2ε) (e^γE/4π)^ε ∫d^dk/(2π)^d: minimal
subtraction is then the MS-bar scheme, with no γE or ln 4π in the results. φ⁴ theory with λ/4!;
Γ⁽⁴⁾ = −(amputated 1PI part of the connected four-point function); β = μ dλ/dμ, γ_φ = ½ μ d ln Z_φ/dμ,
γ_m² = μ d ln m²/dμ. Manual: *Evaluation: dimensional regularization*.

## QED at one loop

Minkowski, metric (+, −, −, −), Peskin and Schroeder except for the sign of the charge: e > 0 everywhere and the
electron has the charge −e, so S_int = +e∫ψ̄γ^μψA_μ and the vertex is +ieγ^μ; α = e²/4π. No γ5 (`GammaMatrix[5]` is
refused). The package expands exp(−X), so the interaction passed is X = −iS_int, and the 1PI functions are −iΣ, iΠ^μν
and +ieΓ^μ. With the MS-bar measure above, the 2/ε of Peskin and Schroeder is our 1/ε. Manual: *Evaluation: QED at
one loop*.

## Names

Names of propagators, fields and sources must be symbols that are not built into the Wolfram Language: `C`, `D`, `E`,
`I`, `K`, `N`, `O`, `Gamma`, `Beta`, `GreenFunction` are (the package warns with `FunctionalDiagrams::builtin`). See the
[FAQ](FAQ#why-does-a-name-like-c-or-d-fail).
