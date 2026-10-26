(* ::Package:: *)

(* ::Title:: *)
(*FunctionalDiagrams*)

(* ::Subtitle:: *)
(*A guided tour: Feynman diagrams from functional derivatives*)

(* ::Section:: *)
(*0. Welcome*)

(* ::Text:: *)
(*In field theory every correlation function is a derivative of a generating functional with respect to its sources. FunctionalDiagrams carries out these functional derivatives symbolically, with the signs of fermions, and writes the result as a sum of Feynman diagrams, each with its weight (symmetry factor and sign). The diagrams can then be drawn, sorted (connected, irreducible, skeleton, by channel), checked against the exact equations of the theory (Dyson, Bethe–Salpeter, parquet, Ward identity) and exported to LaTeX for a paper.*)

(* ::Text:: *)
(*How to use this notebook: evaluate the cells in order (Shift+Enter, or Evaluation > Evaluate Notebook); the whole tour takes less than a minute. Every result is a sum of diagrams in a canonical form, and DiagramPlot draws it. The comment at the end of each input cell says what you should get. The same file runs as a script: wolfram -script Examples.wl.*)

(* ::Subsection:: *)
(*Loading the package*)

(* load FunctionalDiagrams.wl from the folder of this notebook (or script); when it is
   not there (the notebook opened on its own), Needs["FunctionalDiagrams`"] loads the
   installed paclet (PacletInstall["/path/to/FunctionalDiagrams-<version>.paclet"]) *)
With[{folder = Quiet[If[$InputFileName =!= "", DirectoryName[$InputFileName], NotebookDirectory[]]]},
  If[StringQ[folder] && FileExistsQ[FileNameJoin[{folder, "FunctionalDiagrams.wl"}]],
    Get[FileNameJoin[{folder, "FunctionalDiagrams.wl"}]],
    Needs["FunctionalDiagrams`"]]];

$FunctionalDiagramsVersion
(* the version of the package, e.g. "2.3.0" *)

(* ::Subsection:: *)
(*The road map*)

(* ::Text:: *)
(*The tour follows the path of field theory. Each step is a new functional, obtained from the previous one by a logarithm or a Legendre transform, that keeps fewer diagrams:*)

(* ::Item:: *)
(*Z[J], the generating functional: all diagrams, the moments ⟨φφ⋯⟩ (sections 2 and 3).*)

(* ::Item:: *)
(*W[J] = log Z[J]: the connected diagrams, the cumulants; the propagator G, the self-energy Σ and the Dyson equation (sections 4 and 5).*)

(* ::Item:: *)
(*Γ[φ̄], the Legendre transform of W in the source, a functional of the mean field φ̄ = ⟨φ⟩: the one-particle-irreducible (1PI) vertices; every connected function is a tree of propagators and 1PI vertices (section 6).*)

(* ::Item:: *)
(*Γ[φ̄, G], the Legendre transform of W[J, K] in the source J and in a two-point source K coupled to φφ: the two-particle-irreducible (2PI) skeleton diagrams, the Luttinger–Ward functional Φ[G], the self-energy Σ[G] = δΦ/δG and the irreducible vertex Γ⁰ = δΣ/δG (section 7).*)

(* ::Item:: *)
(*Two particles: the Bethe–Salpeter equation F = Γ⁰ + Γ⁰L₀F (L₀ a pair of propagators) and the parquet equations, which sort the four-point vertex F by channel; the superfield (ψ, ψ̄), whose anomalous propagators give the particle–particle kernel and the BCS gap equation (sections 8 and 9).*)

(* ::Item:: *)
(*Exact equations: the vertex of the density, Hedin's equations, and the 2PI functional of G and of the screened interaction W given by a Hubbard–Stratonovich field, whose first term is the GW approximation (section 10).*)

(* ::Item:: *)
(*The same tools elsewhere: linear response and Matsubara sums, phonons and disorder, dimensional regularization, QED and QCD (section 11); pictures and formulas for a paper, and new objects (sections 12 and 13).*)

(* ::Text:: *)
(*Every step is shown on two examples: φ^4 theory, a real scalar field with a local quartic interaction (the textbook example of quantum field theory), and the electron gas, fermions with a two-body interaction V (the textbook example of many-body theory). φ^4 theory is worked out to third order in λ; the electron gas, which has many more diagrams, to second order in V, so that the tour stays fast (the showcase, Showcase/FunctionalDiagrams_Showcase.pdf, has it to third order). Every object is a functional derivative of a functional that you write, Z, W = log Z, Γ[φ̄], Γ[φ̄, G] or Φ[G]; functions such as SelfEnergy or PerturbativeFullVertex are shortcuts for these derivatives, and the tour uses them as checks.*)

(* ::Section:: *)
(*1. The engine in five minutes*)

(* ::Text:: *)
(*Everything is built on a symbolic calculus of functional derivatives. IntegralOver[f, {x, y}] is the integral (or the sum) of f over the labels x and y; a label stands for whatever a field depends on: position, imaginary time, spin, any index. DeltaFunction[x, y] is the delta function. A source J is declared once with DeclareSource, and FunctionalDerivative[F, J[x]] is the derivative δF/δJ(x). Labels are plain symbols (x, y, a, b, x1, …).*)

DeclareSource[J];
FunctionalDerivative[J[x], J[y]]
(* DeltaFunction[x, y] *)

FunctionalDerivative[IntegralOver[f[x] J[x], {x}], J[y]]
(* f[y]: the delta function has been integrated *)

FunctionalDerivative[IntegralOver[J[x]^2 f[x], {x}], J[y]]
(* 2 f[y] J[y] *)

(* ::Text:: *)
(*FunctionalDerivative[F, J[a], J[b], …] takes several derivatives, the rightmost first. The product rule and the chain rule (logarithms, exponentials, powers, any function, nested integrals) are built in:*)

FunctionalDerivative[Log[1 + IntegralOver[g[x] J[x], {x}]], J[y]]
(* g[y]/(1 + IntegralOver[g[z1] J[z1], {z1}]) *)

(* ::Text:: *)
(*The canonical form. Integration variables are dummy: two terms that differ only by the names of their integration variables are the same diagram. SimplifyDiagrams writes every term in a canonical form, with the integration variables renamed z1, z2, …, and adds up equal terms; every function of the package returns its results in this form, so that SimplifyDiagrams[a - b] is 0 exactly when a and b are equal. Symmetries are declared once: DeclareSymmetric[Q] says Q[x, y] = Q[y, x].*)

SimplifyDiagrams[IntegralOver[f[x] g[x, y], {x}] + IntegralOver[f[u] g[u, y], {u}]]
(* 2 IntegralOver[f[z1] g[z1, y], {z1}] *)

DeclareSymmetric[Q];
SimplifyDiagrams[IntegralOver[f[u] Q[u, v] g[v], {u, v}]
    - IntegralOver[g[s] Q[t, s] f[t], {s, t}]]
(* 0: the same term, with other names and the arguments of Q exchanged *)

(* ::Text:: *)
(*Fermions need Grassmann (anticommuting) sources, declared with DeclareGrassmannSource. Products of Grassmann quantities are written in their order with ** (NonCommutativeMultiply), and the derivatives anticommute too, so that the two orders of two derivatives differ by a sign:*)

DeclareGrassmannSource[eta, etabar];
bilinear = IntegralOver[M[x, y] etabar[x] ** eta[y], {x, y}];
{FunctionalDerivative[bilinear, etabar[a], eta[b]],
    FunctionalDerivative[bilinear, eta[b], etabar[a]]}
(* {-M[a, b], M[a, b]} *)

(* ::Text:: *)
(*A term that changes sign under a symmetry of its own vanishes: here Q is symmetric and etabar(u) etabar(v) antisymmetric.*)

SimplifyDiagrams[IntegralOver[Q[u, v] etabar[u] ** etabar[v], {u, v}]]
(* 0 *)

(* ::Section:: *)
(*2. Generating functionals and Wick's theorem*)

(* ::Text:: *)
(*The generating functional of a field theory is the path integral Z[J] = ∫Dφ exp(−S[φ] + J·φ), with J·φ = ∫ J(x) φ(x). A derivative with respect to the source brings down a field, δ exp(J·φ)/δJ(a) = φ(a) exp(J·φ): the derivatives of Z at J = 0, divided by Z[0], are the expectation values ⟨φ(a) φ(b) ⋯⟩, the moments of the distribution exp(−S). As for any probability distribution, the derivatives of W = log Z are the cumulants, the connected correlation functions.*)

(* ::Text:: *)
(*DeclareField[phi, J] says that the field phi is the derivative with respect to J, and FieldExpectationValue differentiates any generating functional. Here Z = exp(W) is written in terms of its first three cumulants W1, W2, W3: the third moment is the third cumulant plus the products of lower ones, and the derivatives of log Z give the cumulant back.*)

DeclareField[phi, J]; DeclareSymmetric[W2, W3];
Z = Exp[IntegralOver[W1[x] J[x], {x}] + 1/2 IntegralOver[W2[x, y] J[x] J[y], {x, y}]
    + 1/6 IntegralOver[W3[x, y, z] J[x] J[y] J[z], {x, y, z}]];
FieldExpectationValue[Z, {phi[a], phi[b], phi[c]}]
(* W3[a, b, c] + W1[a] W2[b, c] + W1[b] W2[a, c] + W1[c] W2[a, b] + W1[a] W1[b] W1[c] *)

SetSourcesToZero[FunctionalDerivative[Log[Z], J[a], J[b], J[c]]]
(* W3[a, b, c] *)

(* ::Subsection:: *)
(*The interaction as a differential operator*)

(* ::Text:: *)
(*Split the action into a quadratic part and an interaction, S = ½ φ·D0⁻¹·φ + S_int[φ]. Inside the integral a field is a derivative of the source term, φ(x) exp(J·φ) = δ exp(J·φ)/δJ(x), so the interaction can be taken out of the integral as a differential operator, and what is left is a Gaussian integral, done once and for all:*)

(* ::DisplayFormula:: *)
(*Z[J] = N exp(−S_int[δ/δJ]) exp(½ J·D0·J)*)

(* ::Text:: *)
(*From here on everything is differentiation. Each derivative acting on the Gaussian either brings down a factor D0·J or joins two of the fields brought down before into a propagator D0: this is Wick's theorem, and at J = 0 only the complete pairings survive. Expanding exp(−S_int[δ/δJ]) generates every Feynman diagram with its weight (section 3). SetSourcesToZero[FunctionalDerivative[...]] uses this: it takes the derivatives one at a time and drops at once the terms with more factors of J than derivatives left.*)

DeclareSymmetric[FreePropagator]; DeclareDisplayName[FreePropagator, "D0"];
FreeBosonGeneratingFunctional = Exp[1/2 IntegralOver[J[x] FreePropagator[x, y] J[y],
    {x, y}]];
fourPointPairings = SetSourcesToZero[FunctionalDerivative[FreeBosonGeneratingFunctional,
    J[a], J[b], J[c], J[d]]]
(* D0[a, b] D0[c, d] + D0[a, c] D0[b, d] + D0[a, d] D0[b, c]: the three pairings of four
   points *)

DiagramPlot[fourPointPairings, {a, b, c, d}]
(* every pairing is a diagram: a line is a free propagator D0, an open circle an
   external point *)

(* ::Text:: *)
(*For a Gaussian generating functional GaussianExpectationValue sums over the complete pairings directly, which is much faster than differentiating; the two methods agree. Six fields have 5 × 3 = 15 pairings:*)

sixPoint = GaussianExpectationValue[FreeBosonGeneratingFunctional,
    {phi[a], phi[b], phi[c], phi[d], phi[e], phi[f]}];
{Length[DiagramTerms[sixPoint]], SimplifyDiagrams[sixPoint - FieldExpectationValue[
    FreeBosonGeneratingFunctional, {phi[a], phi[b], phi[c], phi[d], phi[e], phi[f]}]]}
(* {15, 0} *)

(* ::Subsection:: *)
(*Fermions*)

(* ::Text:: *)
(*Fermions are Grassmann fields ψ, ψ̄ with Grassmann sources η̄, η. With the many-body convention G0(x, y) = −⟨T ψ(x) ψ̄(y)⟩, the free generating functional is Z0 = exp(−η̄·G0·η), and the fields are derivatives: ψ is δ/δη̄ and ψ̄ is −δ/δη (DeclareField with the factor −1). An arrow from y to x is G0(x, y).*)

DeclareDisplayName[BareGreenFunction, "G0"]; DeclareDirectedPropagator[BareGreenFunction];
FreeFermionGeneratingFunctional =
    Exp[-IntegralOver[BareGreenFunction[x, y] etabar[x] ** eta[y], {x, y}]];
DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
FieldExpectationValue[FreeFermionGeneratingFunctional, {psi[a], psibar[b]}]
(* -G0[a, b] *)

fermionFourPoint = GaussianExpectationValue[FreeFermionGeneratingFunctional,
    {psi[a], psi[b], psibar[c], psibar[d]}]
(* G0[a, d] G0[b, c] - G0[a, c] G0[b, d]: the exchange sign is not put in by hand, it
   comes out of the Grassmann algebra *)

DiagramPlot[fermionFourPoint, {a, b, c, d}]

fermionSixPoint = GaussianExpectationValue[FreeFermionGeneratingFunctional,
    {psi[a], psi[b], psi[c], psibar[d], psibar[e], psibar[f]}];
SimplifyDiagrams[fermionSixPoint
    - Det[Table[BareGreenFunction[i, j], {i, {a, b, c}}, {j, {d, e, f}}]]]
(* 0: a 3 x 3 determinant *)

(* ::Section:: *)
(*3. Perturbation theory and vacuum bubbles*)

(* ::Text:: *)
(*φ^4 theory has the interaction S_int = λ/4! ∫ φ(z)^4, so that Z = exp(−λ/4! ∫ (δ/δJ(z))^4) Z0[J]. The package applies this operator literally, one order at a time; here to second order, since the third takes about a minute (the functions used from the next subsection on go to third order in a fraction of a second). At J = 0 the series is Z/Z0 = ⟨exp(−S_int)⟩0, the sum of all the vacuum diagrams, disconnected ones included, each with its symmetry factor. In the pictures each vertex stands for −λ (DeclareCoupling[lambda]) and an interaction line for minus its kernel: the minus sign comes from expanding exp(−S_int). The number written in front of a picture, times the values of its elements, is the term.*)

DeclareCoupling[lambda];
applyInteraction[F_] := SimplifyDiagrams[
    -lambda/24 IntegralOver[FunctionalDerivative[F, J[z], J[z], J[z], J[z]], {z}]];
firstOrder = applyInteraction[FreeBosonGeneratingFunctional];
secondOrder = applyInteraction[firstOrder];
vacuumOverFree = SetSourcesToZero[FreeBosonGeneratingFunctional + firstOrder
    + secondOrder/2];
DiagramPlot[vacuumOverFree, {}]
(* 1, 1/8 (the figure eight, -lambda/8 with the vertex -lambda); 1/128 (two eights),
   1/48 (the melon) and 1/16 (the cactus) *)

(* ::Subsection:: *)
(*Where the symmetry factors come from*)

(* ::Text:: *)
(*At first order Z/Z0 − 1 = −λ/4! ∫ ⟨φ(z)^4⟩0. Wick's theorem pairs the four fields φ(z) in 3 ways, and all three give the figure eight D0(z, z)^2: its coefficient is −3λ/4! = −λ/8, drawn as the weight 1/8 since the vertex stands for −λ. The symmetry factor 1/8 is the number of pairings that give the diagram divided by the 4! of the vertex; equivalently, 8 = 2 × 2 × 2 is the number of symmetries of the eight (each loop can be reversed, and the two loops exchanged).*)

GaussianExpectationValue[FreeBosonGeneratingFunctional, {phi[z], phi[z], phi[z], phi[z]}]
(* 3 D0[z, z]^2 *)

(* ::Text:: *)
(*The same count for the propagator: ⟨φ(a) φ(b) φ(z)^4⟩0 has 15 pairings. In 12 of them a and b are both joined to z (4 choices for a, then 3 for b, and the last two fields paired together): this is the tadpole, with the coefficient −12λ/4! = −λ/2 (the weight 1/2). In the other 3, a is joined to b and the four fields at z to each other: the free propagator times a figure eight, a vacuum bubble.*)

GaussianExpectationValue[FreeBosonGeneratingFunctional,
    {phi[a], phi[b], phi[z], phi[z], phi[z], phi[z]}]
(* 12 D0[a, z] D0[b, z] D0[z, z] + 3 D0[a, b] D0[z, z]^2 *)

(* ::Subsection:: *)
(*Vacuum bubbles cancel*)

(* ::Text:: *)
(*A correlation function is a ratio, ⟨φ(a) φ(b)⟩ = ⟨φ(a) φ(b) exp(−S_int)⟩0 / ⟨exp(−S_int)⟩0. Its numerator, the second derivative of the series above, contains the vacuum bubbles as factors; to first order, exactly the two terms just counted:*)

numerator = SetSourcesToZero[
    FunctionalDerivative[FreeBosonGeneratingFunctional + firstOrder, J[a], J[b]]];
DiagramPlot[numerator, {a, b}]
(* D0; the tadpole (1/2) and D0 beside a figure eight (1/8) *)

(* ::Text:: *)
(*The numerator is a product: its diagrams without bubbles times Z/Z0. A diagram of the numerator is a part joined to a and b times vacuum pieces, and the vertices can be given to the two in all possible ways, with the counting explained below for Z itself:*)

SimplifyDiagrams[OrderPart[numerator - DropVacuumBubbles[numerator, {a, b}] vacuumOverFree,
    lambda, {0, 1}]]
(* 0 *)

(* ::Text:: *)
(*So dividing by Z/Z0 removes the bubbles: the normalised correlation function is the sum of the diagrams without vacuum bubbles. PerturbativeCorrelationFunction[Z0, fields, interaction, n] computes it directly to order n, and never generates the bubbles. OrderPart[expr, lambda, k] keeps the terms of order k.*)

Phi4Vertex = lambda/24 IntegralOver[phi[z]^4, {z}];
propagatorPhi4 = PerturbativeCorrelationFunction[FreeBosonGeneratingFunctional,
    {phi[a], phi[b]}, Phi4Vertex, 3];
Table[Length[DiagramTerms[OrderPart[propagatorPhi4, lambda, m]]], {m, 0, 3}]
(* {1, 1, 3, 10} *)

DiagramPlot[propagatorPhi4, {a, b}, "Columns" -> 5]
(* D0; the tadpole (1/2); at lambda^2 the sunset (1/6), the tadpole on a tadpole
   and two tadpoles in a row (1/4 each); ten diagrams at lambda^3 *)

SimplifyDiagrams[DropVacuumBubbles[numerator, {a, b}]
    - OrderPart[propagatorPhi4, lambda, {0, 1}]]
(* 0 *)

(* ::Text:: *)
(*Applied to Z itself, the same counting is the linked-cluster theorem. A disconnected diagram is the product of its connected pieces. At order n the exponential brings 1/n!, and there are n!/(n1! n2! ...) ways of giving the vertices to pieces with n1, n2, ... vertices: the n! cancels and every piece keeps its own weight, except that k identical pieces can be exchanged without making a new term, and carry 1/k!. That is how the exponential series counts: Z/Z0 = exp(sum of the connected vacuum diagrams), and log(Z/Z0) is that sum. The two eights, (−λ/8)^2/2 = λ^2/128, are the square of the eight in the exponential. Checked on the series above, computed by derivatives (ConnectedDiagrams only picks out the connected diagrams):*)

vacuum1 = OrderPart[vacuumOverFree, lambda, 1]; vacuum2 = OrderPart[vacuumOverFree, lambda, 2];
SimplifyDiagrams[vacuum1 + vacuum2 - vacuum1^2/2 - ConnectedDiagrams[vacuum1 + vacuum2, {}]]
(* 0 *)

(* ::Text:: *)
(*ConnectedVacuumDiagrams computes the connected vacuum diagrams directly:*)

vacuumPhi4 = ConnectedVacuumDiagrams[FreeBosonGeneratingFunctional, Phi4Vertex, 3];
DiagramPlot[vacuumPhi4, {}]
(* 1/8 (eight); 1/48 (melon) and 1/16 (cactus); four diagrams at
   lambda^3 *)

(* ::Section:: *)
(*4. Connected functions*)

(* ::Text:: *)
(*W = log Z generates the connected correlation functions. Each derivative of Z = exp(W) either brings down a new derivative of W (a new block) or differentiates one already there (the point joins that block), so a moment is the sum over the partitions of its points into cumulants: the counting of the vacuum diagrams again, with points in place of vertices. In φ^4 theory at J = 0 the odd cumulants vanish, and ⟨φ(a) φ(b) φ(c) φ(d)⟩ is ⟨φ(a) φ(b) φ(c) φ(d)⟩c plus the products ⟨φ(a) φ(b)⟩⟨φ(c) φ(d)⟩ of the three pairings. In φ^4 theory the full four-point function has 3, 7, 28 and 120 diagrams of orders 0 to 3; to first order, drawn below, the three pairings of propagators, the same with a tadpole on one line (all disconnected), and one connected diagram, the cross.*)

fourPointPhi4 = PerturbativeCorrelationFunction[FreeBosonGeneratingFunctional,
    {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3];
Table[Length[DiagramTerms[OrderPart[fourPointPhi4, lambda, m]]], {m, 0, 3}]
(* {3, 7, 28, 120} *)

DiagramPlot[OrderPart[fourPointPhi4, lambda, {0, 1}], {a, b, c, d}, "Columns" -> 5]
(* the three pairings; the six with a tadpole (1/2 each); the cross (1) *)

(* ::Subsection:: *)
(*W = log Z, differentiated*)

(* ::Text:: *)
(*The connected functions are the derivatives of W = log Z. Differentiating Log[Z] as it stands is slow: every derivative of the logarithm brings a power of 1/Z, with all the orders of λ it contains. Expanded in λ first it is fast: Z/Z0 − 1, built in section 3, is a polynomial in J (dividing by Z0 cancels the Gaussian factor), and W = ½ J·D0·J + log(1 + Z/Z0 − 1) is expanded with Series before it is differentiated. Its fourth derivative keeps only the connected diagrams:*)

zOverZ0 = SimplifyDiagrams[(firstOrder + secondOrder/2)/FreeBosonGeneratingFunctional];
logZ = 1/2 IntegralOver[J[x] FreePropagator[x, y] J[y], {x, y}] +
    SimplifyDiagrams[Normal[Series[Log[1 + zOverZ0], {lambda, 0, 2}]]];
connectedFourPointW = SimplifyDiagrams[SetSourcesToZero[
    FunctionalDerivative[logZ, J[a], J[b], J[c], J[d]]]];
DiagramPlot[connectedFourPointW, {a, b, c, d}, "Columns" -> 4]
(* the cross (1); at lambda^2 a bubble in each of the channels s, t, u and a
   tadpole on each of the four legs (1/2 each) *)

(* ::Text:: *)
(*The option "MaximumOrder" -> {lambda, 2} of FunctionalDerivative does this expansion itself: the Gaussian factor of Z = Z0 + Z1 + Z2/2 is taken out of the logarithm, log(Z0 p) = ½ J·D0·J + log p, log p is replaced by its series to λ², and the terms of higher order are dropped after each derivative. The result is the same:*)

SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[Log[FreeBosonGeneratingFunctional
    + firstOrder + secondOrder/2], J[a], J[b], J[c], J[d], "MaximumOrder" -> {lambda, 2}]]
    - connectedFourPointW]
(* 0 *)

(* ::Text:: *)
(*ConnectedCorrelationFunction computes these derivatives of W directly, to any order: 1, 7 and 42 connected diagrams of orders 1 to 3. The moment is the cumulant plus the three pairings of the propagator:*)

connectedFourPointPhi4 = ConnectedCorrelationFunction[FreeBosonGeneratingFunctional,
    {phi[a], phi[b], phi[c], phi[d]}, Phi4Vertex, 3];
{Table[Length[DiagramTerms[OrderPart[connectedFourPointPhi4, lambda, m]]], {m, 3}],
 SimplifyDiagrams[OrderPart[connectedFourPointPhi4, lambda, {1, 2}] - connectedFourPointW]}
(* {{1, 7, 42}, 0} *)

pair[p_, q_] := propagatorPhi4 /. {a -> p, b -> q};
SimplifyDiagrams[OrderPart[fourPointPhi4 - connectedFourPointPhi4 - pair[a, b] pair[c, d]
    - pair[a, c] pair[b, d] - pair[a, d] pair[b, c], lambda, {0, 1, 2, 3}]]
(* 0 *)

(* ::Section:: *)
(*5. The self-energy and the Dyson equation*)

(* ::Text:: *)
(*The connected two-point function is the full propagator G. A diagram is one-particle irreducible (1PI) if it stays connected when any one propagator line is cut; the 1PI diagrams of G with their two external legs removed are the self-energy Σ, and the Dyson equation G = G0 + G0 Σ G, i.e. G = G0 + G0ΣG0 + G0ΣG0ΣG0 + ⋯, rebuilds all the diagrams of G from those of Σ. For φ^4 theory:*)

selfEnergyPhi4 = AmputateExternalLegs[ParticleIrreducibleDiagrams[propagatorPhi4, 1, {a, b},
    {FreePropagator}], {a, b}, {FreePropagator}];
DiagramPlot[selfEnergyPhi4, {a, b}, "Columns" -> 4]
(* the tadpole (1/2); the sunset (1/6) and the tadpole on a tadpole (1/4); five diagrams
   at lambda^3 *)

SimplifyDiagrams[DysonSeries[selfEnergyPhi4 /. {a -> s1, b -> s2}, {s1, s2}, FreePropagator,
    {a, b}, 3, "MaximumOrder" -> {lambda, 3}] - propagatorPhi4]
(* 0: the chains G0 + G0 Sigma G0 + ..., up to lambda^3, are the propagator of section 3
   *)

(* ::Subsection:: *)
(*The electron gas*)

(* ::Text:: *)
(*Electrons with a two-body interaction V(x, y) between densities (the Coulomb interaction or any other) have S_int = ½ ∫ V(x, y) ψ̄(x) ψ̄(y) ψ(y) ψ(x). V is drawn as a wavy line (DeclareInteractionLine). The Green function G = −⟨T ψ ψ̄⟩ to second order has 1, 2 and 10 diagrams; each is drawn with the weight ±1, the sign (−1)^L of a diagram with L closed fermion loops, since the wavy line stands for −V (its coefficient is (−1)^(n+L) at order n); the package obtains the signs from the Grassmann algebra, not from this rule.*)

DeclareSymmetric[InteractionPotential]; DeclareDisplayName[InteractionPotential, "V"];
DeclareInteractionLine[InteractionPotential];
CoulombVertex = 1/2 IntegralOver[
    InteractionPotential[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x], {x, y}];
greenFunction = -PerturbativeCorrelationFunction[FreeFermionGeneratingFunctional,
    {psi[x], psibar[y]}, CoulombVertex, 2];
Table[Length[DiagramTerms[OrderPart[greenFunction, InteractionPotential, m]]], {m, 0, 2}]
(* {1, 2, 10} *)

DiagramPlot[greenFunction, {x, y}, "Columns" -> 4]
(* G0; Hartree (-1) and Fock (+1); the ten diagrams of second order *)

(* ::Text:: *)
(*The self-energy: the fermion enters at y and leaves at x, and the Hartree term, proportional to δ(x, y), sits on the single point x = y.*)

selfEnergyPerturbative = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction, 1,
    {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}];
DiagramPlot[selfEnergyPerturbative, {x, y}, "Columns" -> 4]
(* Hartree and Fock; six diagrams of second order *)

SimplifyDiagrams[DysonSeries[selfEnergyPerturbative /. {x -> s1, y -> s2}, {s1, s2},
    BareGreenFunction, {x, y}, 2, "MaximumOrder" -> {InteractionPotential, 2}]
    - greenFunction]
(* 0 *)

(* ::Text:: *)
(*Skeleton diagrams have no self-energy insertions on their lines. With the bare lines replaced by the full G they give Σ[G], the self-energy of self-consistent approximations, which section 7 derives from the Luttinger–Ward functional.*)

skeletonSelfEnergy = SkeletonDiagrams[selfEnergyPerturbative, {x, y}, {BareGreenFunction}];
DiagramPlot[skeletonSelfEnergy, {x, y}]
(* Hartree and Fock; the second-order exchange and the polarization bubble *)

(* ::Section:: *)
(*6. The 1PI effective action and the tree expansion*)

(* ::Text:: *)
(*The Legendre transform of W, Γ[φ̄] = −W[J] + J·φ̄ with the mean field φ̄ = δW/δJ = ⟨φ⟩, is the 1PI effective action. Its second derivative is the inverse full propagator, Γ^(2) = G⁻¹ = G0⁻¹ − Σ (the Dyson equation again), and its higher derivatives Γ^(n) are the 1PI vertices. Conversely, every connected function is a tree of full propagators G and 1PI vertices Γ^(n). The trees are generated by differentiating with respect to the source: a line G becomes two lines joined by a Γ^(3), and a vertex Γ^(n) gains a leg, Γ^(n+1). ConnectedFunctionFromVertices builds them this way (FullPropagator is G, VertexFunction is Γ^(n)):*)

connectedFourPoint = ConnectedFunctionFromVertices[{a, b, c, d}, "Boson"];
DiagramPlot[connectedFourPoint, {a, b, c, d}]
(* -Gamma^(4) with four lines G, and the three channels: two Gamma^(3) joined by a line
   (+1 each) *)

(* ::Text:: *)
(*Every tree is generated exactly once, with the factor −1 for each vertex. Integrating over the external labels identifies the trees of the same shape, and the canonical form counts them: W^(5) has 1 + 10 + 15 = 26 trees.*)

SimplifyDiagrams[IntegralOver[ConnectedFunctionFromVertices[{a, b, c, d, e}, "Boson"],
    {a, b, c, d, e}]]
(* three shapes, with the weights -1 (Gamma^(5)), 10 (Gamma^(4) Gamma^(3)) and -15
   (three Gamma^(3)) *)

(* ::Text:: *)
(*For fermions the trees are made of directed lines G = −⟨ψψ̄⟩ and of vertices with as many outgoing as incoming legs. The connected six-point function ⟨ψ(a) ψ̄(b) ψ(c) ψ̄(d) ψ(e) ψ̄(f)⟩c has ten trees:*)

DiagramPlot[ConnectedFunctionFromVertices[{a, b, c, d, e, f}, "FermionLines"],
    {a, b, c, d, e, f}]
(* -Gamma^(6), and nine trees of two Gamma^(4) joined by a fermion line, each with the
   sign of the permutation of its legs *)

(* ::Subsection:: *)
(*The four-point vertex of φ^4 theory*)

(* ::Text:: *)
(*In φ^4 theory Γ^(3) = 0 by symmetry, and Γ^(4) starts with λ, a point. EffectiveAction1PI writes Γ[φ̄] itself (here with bare lines D0, to λ^3), and its fourth derivative at φ̄ = 0 is Γ^(4), with the self-energy insertions on its lines (1, 3 and 12 diagrams); the second derivative is D0⁻¹ − Σ.*)

effectiveAction1PI = EffectiveAction1PI[FreeBosonGeneratingFunctional, Phi4Vertex, 3,
    phi -> phiMean, FreePropagator -> FreePropagator];
gamma4 = SetSourcesToZero[FunctionalDerivative[effectiveAction1PI,
    phiMean[a], phiMean[b], phiMean[c], phiMean[d]]];
Table[Length[DiagramTerms[OrderPart[gamma4, lambda, m]]], {m, 3}]
(* {1, 3, 12} *)

(* ::Text:: *)
(*Its skeleton diagrams, written with the full propagator G, are the vertex of the skeleton expansion. Since it starts at order λ, two loops means λ^3. PerturbativeFullVertex computes the same directly from the connected four-point function with its legs removed (for real bosons Γ^(4) = −W^(4) amputated), keeping the skeleton diagrams:*)

DeclareSymmetric[DressedPropagator]; DeclareDisplayName[DressedPropagator, "G"];
vertexPhi4 = SkeletonDiagrams[gamma4, {a, b, c, d}, {FreePropagator}] /.
    FreePropagator -> DressedPropagator;
{Table[Length[DiagramTerms[OrderPart[vertexPhi4, lambda, m]]], {m, 3}],
 SimplifyDiagrams[PerturbativeFullVertex[FreeBosonGeneratingFunctional, Phi4Vertex, 3,
    {phi}, {a, b, c, d}, FreePropagator -> DressedPropagator] - vertexPhi4]}
(* {{1, 3, 9}, 0} *)

DiagramPlot[vertexPhi4, {a, b, c, d}, "Columns" -> 5]
(* -1 (the bare vertex, standing for -lambda); one bubble in each of the channels s, t, u
   (-1/2 each); nine diagrams of two loops *)

(* ::Section:: *)
(*7. The 2PI effective action: the Luttinger–Ward functional*)

(* ::Text:: *)
(*A second Legendre transform, in a two-point source K coupled to φ(x) φ(y), makes the full propagator G itself a variable. What remains beyond one loop is the sum of the two-particle-irreducible (2PI) vacuum diagrams, those that stay connected when any two lines are cut, with full propagators G: the skeleton diagrams. For fermions it is the Luttinger–Ward functional Φ[G]. Its derivative, which opens one line in all possible ways, is the self-energy Σ[G]; the derivative of Σ is the irreducible vertex Γ⁰, the kernel of the Bethe–Salpeter equation. Truncating Φ gives conserving approximations (Baym–Kadanoff).*)

DeclareDisplayName[DressedGreenFunction, "G"];
DeclareDirectedPropagator[DressedGreenFunction];
luttingerWardFunctional = LuttingerWardFunctional[FreeFermionGeneratingFunctional,
    CoulombVertex, 2, BareGreenFunction -> DressedGreenFunction];
DiagramPlot[luttingerWardFunctional, {}]
(* first order: Fock (+1/2) and Hartree (-1/2); second order: the exchange (+1/4) and
   the ring (-1/4) *)

(* ::Text:: *)
(*The self-energy is the derivative Σ(x, y) = δΦ/δG(y, x), taken by FunctionalDerivative with G treated as a source (nothing to declare): it is exactly the skeleton part of the perturbative Σ of section 5, and with the first order of Φ alone it is the Hartree–Fock approximation. SelfEnergy[Φ, G, {x, y}] is a shortcut for the same derivative.*)

selfEnergy = FunctionalDerivative[luttingerWardFunctional, DressedGreenFunction[y, x]];
SimplifyDiagrams[(selfEnergy /. DressedGreenFunction -> BareGreenFunction)
    - skeletonSelfEnergy]
(* 0 *)

firstOrderPhi = OrderPart[luttingerWardFunctional, InteractionPotential, 1];
hartreeFockSelfEnergy = FunctionalDerivative[firstOrderPhi, DressedGreenFunction[y, x]]
(* DeltaFunction[x, y] IntegralOver[G[z1, z1] V[x, z1], {z1}] - G[x, y] V[x, y] *)

SimplifyDiagrams[SelfEnergy[luttingerWardFunctional, DressedGreenFunction, {x, y}] - selfEnergy]
(* 0 *)

(* ::Subsection:: *)
(*The irreducible vertex and the Ward identity*)

(* ::Text:: *)
(*Γ⁰(x1, x2; x3, x4) = δΣ(x1, x2)/δG(x4, x3) = δ²Φ/δG(x4, x3) δG(x2, x1) is the vertex irreducible in the particle–hole channel (FunctionalDerivative[F, s1, s2] takes the rightmost derivative first; IrreducibleVertex[Φ, G, {x1, x2, x3, x4}] is the shortcut). The fermions leave at x1, x3 and enter at x2, x4; the legs are drawn counterclockwise from the top left.*)

irreducibleVertex = FunctionalDerivative[luttingerWardFunctional,
    DressedGreenFunction[x4, x3], DressedGreenFunction[x2, x1]];
DiagramPlot[irreducibleVertex, {x1, x2, x3, x4}, "Columns" -> 4]
(* first order: the direct (-1) and the exchange (+1) interaction; second order: six
   diagrams *)

(* ::Text:: *)
(*Ward identity of charge conservation: under a local phase rotation G(x, y) → exp(iθ(x)) G(x, y) exp(−iθ(y)) every diagram of Φ is invariant, so Σ[G] rotates like G, and Γ⁰ · δG = δΣ. The identity holds order by order, hence for every truncation of Φ: this is why Φ-derivable approximations are conserving.*)

rotationOfG = (theta[x4] - theta[x3]) DressedGreenFunction[x4, x3];
SimplifyDiagrams[IntegralOver[irreducibleVertex rotationOfG, {x3, x4}]
    - (theta[x1] - theta[x2]) (selfEnergy /. {x -> x1, y -> x2})]
(* 0 *)

(* ::Subsection:: *)
(*φ^4 theory*)

(* ::Text:: *)
(*For real bosons the same function gives Γ2[G], the 2PI part of the effective action (the figure eight, the melon and one diagram at λ^3), and Σ(a, b) = −2 δΓ2/δG(a, b), the derivative with respect to the symmetric G being symmetrized (the shortcut SelfEnergy takes the option "Statistics" -> "Boson"). Again Σ[G] is the skeleton part of the perturbative Σ:*)

gamma2 = LuttingerWardFunctional[FreeBosonGeneratingFunctional, Phi4Vertex, 3,
    FreePropagator -> DressedPropagator];
DiagramPlot[gamma2, {}]
(* -1/8 (eight), -1/48 (melon) and -1/48 (three points joined in pairs by two lines) *)

selfEnergyPhi4G = SimplifyDiagrams[-2 FunctionalDerivative[gamma2, DressedPropagator[a, b]]]
(* -lambda/2 DeltaFunction[a, b] G[a, a] + lambda^2/6 G[a, b]^3 - lambda^3/4 G[a, b]
   IntegralOver[G[a, z1]^2 G[b, z1]^2, {z1}] *)

skeletonSelfEnergyPhi4 = SkeletonDiagrams[selfEnergyPhi4, {a, b}, {FreePropagator}];
SimplifyDiagrams[selfEnergyPhi4G
    - (skeletonSelfEnergyPhi4 /. FreePropagator -> DressedPropagator)]
(* 0 *)

(* ::Subsection:: *)
(*The effective action as an object*)

(* ::Text:: *)
(*The 2PI effective action itself is Γ[G] = Φ[G] − Tr ln(−G⁻¹) − Tr(G0⁻¹G − 1) for fermions: the Luttinger–Ward (or Baym–Kadanoff) functional of the grand potential, the Γ[φ̄, G] of Cornwall, Jackiw and Tomboulis at zero mean field (EffectiveAction2PI[..., "MeanFields" -> ...] gives the field dependence, EffectiveAction1PI the 1PI action Γ[φ̄]). The traces are inert objects, TraceLog and OperatorTrace, and FunctionalDerivative differentiates with respect to any two-index object G[x, y]: Γ is stationary where G⁻¹ = G0⁻¹ − Σ[G], the Dyson equation.*)

effectiveAction2PI = EffectiveAction2PI[luttingerWardFunctional,
    BareGreenFunction -> DressedGreenFunction];
SimplifyDiagrams[FunctionalDerivative[effectiveAction2PI, DressedGreenFunction[y, x]]
    - (selfEnergy + OperatorKernel[DressedGreenFunction^-1][x, y]
       - OperatorKernel[BareGreenFunction^-1][x, y])]
(* 0: dGamma/dG(y, x) = Sigma(x, y) + G^-1(x, y) - G0^-1(x, y) *)

(* ::Text:: *)
(*Potthoff's self-energy functional Ω[Σ] = 𝓕[Σ] − Tr ln(−(G0⁻¹ − Σ)) uses the Legendre transform 𝓕[Σ] of Φ from G to Σ, which does not contain G0: the same for every system with the same interaction. Its derivative vanishes where G[Σ], the propagator with self-energy Σ, is (G0⁻¹ − Σ)⁻¹:*)

DeclareDirectedPropagator[TrialSelfEnergy]; DeclareDisplayName[TrialSelfEnergy, "\[CapitalSigma]"];
selfEnergyFunctional = SelfEnergyFunctional[luttingerWardFunctional,
    BareGreenFunction -> TrialSelfEnergy, DressedGreenFunction];
FunctionalDerivative[selfEnergyFunctional, TrialSelfEnergy[x, y]]
(* -G[Sigma](y, x) + ((G0^-1 - Sigma)^-1)(y, x) *)

(* ::Text:: *)
(*In zero dimensions (one level with spin 1/2, G0 = 1/a, Z = a² − u) every diagram is a number. The skeleton series of Σ[g] has the Catalan numbers as coefficients and converges, for 4ug² < 1, to (−1 + √(1 + 4ug²))/(2g), one of the two branches of the exact Σ[g]. At strong repulsion (a² < u) the physical self-energy u/a lies on the other branch, and the series converges to the wrong one. The branches meet where 1 + 4ug² = 0 (u < 0): there the map G0 → G cannot be inverted and the irreducible vertex δΣ/δG diverges.*)

ZeroDimensionalValue[FunctionalDerivative[LuttingerWardFunctional[
    FreeFermionGeneratingFunctional, CoulombVertex, 4,
    BareGreenFunction -> DressedGreenFunction], DressedGreenFunction[y, x]],
  {DressedGreenFunction -> g, InteractionPotential -> u}, "LoopFactor" -> 2]
(* g u - g^3 u^2 + 2 g^5 u^3 - 5 g^7 u^4 *)

(* ::Section:: *)
(*8. Two particles: the Bethe–Salpeter equation*)

(* ::Text:: *)
(*The two-particle Green function ⟨T ψ(x1) ψ̄(x2) ψ̄(x4) ψ(x3)⟩ is made of uncorrelated pairs of propagators and of a connected part; the connected part with its four legs removed is the full vertex F, the fourth derivative of the 1PI action Γ[Ψ̄, Ψ] of the means Ψ = ⟨ψ⟩ and Ψ̄ = ⟨ψ̄⟩ at zero means (the rightmost derivative first). Its skeleton diagrams, with full lines G:*)

effectiveActionPsi = EffectiveAction1PI[FreeFermionGeneratingFunctional, CoulombVertex, 2,
    {psi -> MeanPsi, psibar -> MeanPsibar}, BareGreenFunction -> BareGreenFunction];
fullVertex = SkeletonDiagrams[SetSourcesToZero[FunctionalDerivative[effectiveActionPsi,
    MeanPsibar[x1], MeanPsi[x2], MeanPsibar[x3], MeanPsi[x4]]], {x1, x2, x3, x4},
    {BareGreenFunction}] /. BareGreenFunction -> DressedGreenFunction;
DiagramPlot[fullVertex, {x1, x2, x3, x4}, "Columns" -> 4]
(* first order: the bare vertex, direct and exchange; second order: ten diagrams *)

(* ::Text:: *)
(*PerturbativeFullVertex computes the same from the connected four-point function, with its legs removed:*)

SimplifyDiagrams[PerturbativeFullVertex[FreeFermionGeneratingFunctional, CoulombVertex, 2,
    {psi, psibar}, {x1, x2, x3, x4}, BareGreenFunction -> DressedGreenFunction] - fullVertex]
(* 0 *)

(* ::Text:: *)
(*The Bethe–Salpeter equation F = Γ⁰ + Γ⁰L₀F resums the particle–hole channel: the diagrams of F that fall apart when a particle–hole pair of lines is cut are a Γ⁰, the pair of lines L₀ and F again. BetheSalpeterLadder[A, B, {x1, x2, x3, x4}, channel, G] joins two vertices by a pair of lines in a channel; the equation holds order by order:*)

SimplifyDiagrams[fullVertex - irreducibleVertex
    - BetheSalpeterLadder[irreducibleVertex, fullVertex, {x1, x2, x3, x4}, "ParticleHole",
    DressedGreenFunction, "MaximumOrder" -> {InteractionPotential, 2}]]
(* 0 *)

(* ::Subsection:: *)
(*The RPA chain*)

(* ::Text:: *)
(*Iterating the direct interaction alone gives the bubble chain of the random-phase approximation, the screened interaction W = V + VΠV + VΠVΠV + ⋯, with the bubble Π(x, y) = G(x, y) G(y, x):*)

directKernel = DeltaFunction[x1, x2] DeltaFunction[x3, x4] InteractionPotential[x1, x3];
oneBubble = BetheSalpeterLadder[directKernel, directKernel, {x1, x2, x3, x4},
    "ParticleHole", DressedGreenFunction];
twoBubbles = BetheSalpeterLadder[directKernel, oneBubble, {x1, x2, x3, x4}, "ParticleHole",
    DressedGreenFunction];
DiagramPlot[directKernel + oneBubble + twoBubbles, {x1, x2, x3, x4},
    "LegDirections" -> "Alternating"]
(* V, V Pi V and V Pi V Pi V, with the weights -1, +1, -1 (each wavy line stands for -V) *)

(* ::Subsection:: *)
(*The particle–particle ladder*)

(* ::Text:: *)
(*Two particles scattering repeatedly: the bare vertex iterated in the particle–particle channel (the T-matrix). The option "ExternalOrder" -> {x4, x2, x3, x1} puts the incoming particles on the left, so that the rungs of the ladder are vertical.*)

bareVertex = OrderPart[fullVertex, InteractionPotential, 1];
particleParticleLadder = BetheSalpeterLadder[bareVertex, bareVertex, {x1, x2, x3, x4},
    "ParticleParticle", DressedGreenFunction];
DiagramPlot[particleParticleLadder, {x1, x2, x3, x4}, "ExternalOrder" -> {x4, x2, x3, x1}]
(* the ladder with two rungs (+1) and the same with the outgoing particles exchanged
   (-1) *)

(* ::Section:: *)
(*9. The parquet equations*)

(* ::Text:: *)
(*The four legs of a vertex can be split into two pairs in three ways, the channels particle–hole ph (x1 x2|x3 x4), transverse particle–hole ph̄ (x1 x4|x3 x2) and particle–particle pp (x1 x3|x2 x4). A diagram is reducible in a channel if cutting two lines separates its legs into the two pairs of that channel, and no diagram is reducible in two channels, so that F = Γ_irr + R_ph + R_ph̄ + R_pp, with Γ_irr fully irreducible. In each channel the vertex irreducible in it, Γ_r = F − R_r, generates R_r by a Bethe–Salpeter equation. Given Γ_irr these equations, with the Dyson equation, determine G and F together; the parquet approximation takes Γ_irr = V, exact to third order.*)

(* ::Text:: *)
(*The vertices are derivatives. Γ_ph is the Γ⁰ = δ²Φ/δG δG of section 7, and Γ_irr is the fourth derivative of the 2PI effective action Γ[Ψ̄, Ψ, G] (section 7) with respect to the means Ψ = ⟨ψ⟩ and Ψ̄ = ⟨ψ̄⟩, at zero means and fixed G, the rightmost derivative first. Below V^4 it is the bare vertex, the first order of F:*)

gammaMeans = EffectiveAction2PI[FreeFermionGeneratingFunctional, CoulombVertex, 2,
    BareGreenFunction -> DressedGreenFunction,
    "MeanFields" -> {psi -> MeanPsi, psibar -> MeanPsibar}];
fullyIrreducibleVertex = SetSourcesToZero[FunctionalDerivative[gammaMeans,
    MeanPsibar[x1], MeanPsi[x2], MeanPsibar[x3], MeanPsi[x4]]];
SimplifyDiagrams[fullyIrreducibleVertex - OrderPart[fullVertex, InteractionPotential, 1]]
(* 0 *)

(* ::Text:: *)
(*ParquetDecomposition sorts the diagrams of F by cutting their lines, and checks the derivatives: Γ_irr is its fully irreducible part, and F − R_ph is Γ⁰.*)

parquetDecomposition = ParquetDecomposition[fullVertex, {x1, x2, x3, x4},
    {DressedGreenFunction}];
Map[Length[DiagramTerms[#]] &, parquetDecomposition]
(* <|FullyIrreducible -> 2, ParticleHole -> 4, TransverseParticleHole -> 4,
   ParticleParticle -> 2, SeveralChannels -> 0|> *)

DiagramPlot[parquetDecomposition["ParticleParticle"], {x1, x2, x3, x4},
    "ExternalOrder" -> {x4, x2, x3, x1}]
(* the two particle-particle diagrams of second order *)

{SimplifyDiagrams[fullyIrreducibleVertex - parquetDecomposition["FullyIrreducible"]],
 SimplifyDiagrams[fullVertex - parquetDecomposition["ParticleHole"] - irreducibleVertex]}
(* {0, 0} *)

(* ::Text:: *)
(*CheckParquetEquations checks the Bethe–Salpeter equation of every channel, order by order:*)

CheckParquetEquations[fullVertex, {x1, x2, x3, x4}, DressedGreenFunction,
    InteractionPotential, 2]["Residuals"]
(* <|ParticleHole -> {0, 0}, TransverseParticleHole -> {0, 0}, ParticleParticle -> {0,
   0}|> *)

(* ::Text:: *)
(*For bosons the channels are s, t and u ("Statistics" -> "Boson"); the four-point vertex of φ^4 theory of section 6, to λ^3:*)

Map[Length[DiagramTerms[#]] &, ParquetDecomposition[vertexPhi4, {a, b, c, d},
    {DressedPropagator}, "Statistics" -> "Boson"]]
(* <|FullyIrreducible -> 1, s -> 4, t -> 4, u -> 4, SeveralChannels -> 0|> *)

CheckParquetEquations[vertexPhi4, {a, b, c, d}, DressedPropagator, lambda, 3,
    "Statistics" -> "Boson"]["Residuals"]
(* <|s -> {0, 0, 0}, t -> {0, 0, 0}, u -> {0, 0, 0}|> *)

(* ::Subsection:: *)
(*Pairing: the superfield (ψ, ψ̄)*)

(* ::Text:: *)
(*Γ⁰ = δΣ/δG gives the kernels of ph and ph̄, but not that of pp: the two lines cut in a particle–particle diagram run the same way, and the variables they call for are the anomalous propagators −⟨Tψψ⟩ and −⟨Tψ̄ψ̄⟩, the order parameter of a superconductor. Nambu and Gor'kov make them variables by taking ψ and ψ̄ as the two components of one Grassmann field 𝛙 = (ψ, ψ̄), the superfield, whose propagator 𝒢 = −⟨T𝛙𝛙⟩ is an antisymmetric matrix with the normal block G and the two anomalous ones. For the package it is written as φ^4 theory: one source, one antisymmetric propagator and one totally antisymmetric vertex 𝒱, with the factors 1/2 and 1/4!. Its Φ has one diagram at each order:*)

DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex];
DeclareLocalVertex[NambuVertex];
nambuZ0 = Exp[1/2 IntegralOver[BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b],
    {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d]
    NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
nambuPhi = LuttingerWardFunctional[nambuZ0, nambuInteraction, 2,
    BareNambuPropagator -> NambuPropagator];
DiagramPlot[nambuPhi, {}]
(* -1/8 (eight) and -1/48 (melon), the pictures of phi^4 theory *)

(* ::Text:: *)
(*SuperfieldComponents writes a superfield expression in the components: the rules give the components of the field, the blocks of the propagator and, for the vertex, the interaction of the electron gas, whose derivatives are its components. To first order Φ has the Fock and Hartree diagrams of Φ[G] and a pairing term. An anomalous line has two arrowheads, which point to the ψ ends of −⟨Tψψ⟩ and away from the ψ̄ ends of −⟨Tψ̄ψ̄⟩:*)

DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar];
DeclareLineStyle[AnomalousGreenFunction, "DivergingArrows"];
DeclareLineStyle[AnomalousGreenFunctionBar, "ConvergingArrows"];
nambuRules = {NambuField -> {psi, psibar}, NambuPropagator -> {DressedGreenFunction,
    AnomalousGreenFunction, AnomalousGreenFunctionBar}, NambuVertex -> CoulombVertex};
DiagramPlot[SuperfieldComponents[OrderPart[nambuPhi, NambuVertex, 1], {}, nambuRules], {}]
(* the pairing term (-1/2), Fock (+1/2) and Hartree (-1/2) *)

(* ::Text:: *)
(*The self-energy of the superfield, Σ = −2 δΦ/δ𝒢, has the normal block Σ and two anomalous ones, the gap functions; Δ(x1, x2) = Σ_ψ̄ψ̄(x1, x2) is, to first order, Δ = V𝒢_ψψ = −V⟨Tψψ⟩, the gap equation of BCS theory. With the Gor'kov equations, the Dyson equation of the superfield in components, these are the Hartree–Fock–Bogoliubov equations:*)

nambuSelfEnergy = -2 FunctionalDerivative[nambuPhi, NambuPropagator[a, b]];
(* the same: SelfEnergy[nambuPhi, NambuPropagator, {a, b}, "Statistics" -> "Superfield"] *)
SuperfieldComponents[OrderPart[nambuSelfEnergy, NambuVertex, 1],
  {a -> psibar[x1], b -> psibar[x2]}, nambuRules]
(* AnomalousGreenFunction[x1, x2] V[x1, x2] *)

(* ::Text:: *)
(*With one kind of line the kernels of the three channels come from the same functional, as for φ^4: Γ⁰(a, b, c, d) = Γ_s = 4 δ²Φ/δ𝒢(a, b)δ𝒢(c, d) is irreducible in s, and the kernels of t and u are the same function with its labels exchanged, with the sign of the permutation, Γ_t(a, b, c, d) = −Γ⁰(a, c, b, d) and Γ_u(a, b, c, d) = Γ⁰(a, d, b, c). With the legs (x1 ψ̄, x2 ψ, x3 ψ̄, x4 ψ), s is ph, u is ph̄ and t is pp: at zero anomalous propagators the kernel of t is the particle–particle kernel of the electron gas, F − R_pp:*)

kernelT = -4 FunctionalDerivative[nambuPhi, NambuPropagator[b, d], NambuPropagator[a, c]];
(* the same: IrreducibleVertex[nambuPhi, NambuPropagator, {a, b, c, d},
   "Statistics" -> "Superfield", "Channel" -> All]["t"] *)
particleParticleKernel = SuperfieldComponents[kernelT,
    {a -> psibar[x1], b -> psi[x2], c -> psibar[x3], d -> psi[x4]},
    {NambuField -> {psi, psibar}, NambuPropagator -> {DressedGreenFunction, 0, 0},
     NambuVertex -> CoulombVertex}];
SimplifyDiagrams[particleParticleKernel
    - (fullVertex - parquetDecomposition["ParticleParticle"])]
(* 0 *)

(* ::Text:: *)
(*In the components it is the derivative with respect to the two anomalous propagators, Γ_pp(x1, x2; x3, x4) = −4 δ²Φ/δ𝒢_ψ̄ψ̄(x1, x3)δ𝒢_ψψ(x2, x4) at zero anomalous propagators:*)

nambuPhiComponents = SuperfieldComponents[nambuPhi, {}, nambuRules];
SimplifyDiagrams[(-4 FunctionalDerivative[nambuPhiComponents,
       AnomalousGreenFunction[x2, x4], AnomalousGreenFunctionBar[x1, x3]]
     /. {AnomalousGreenFunction[__] -> 0, AnomalousGreenFunctionBar[__] -> 0})
    - particleParticleKernel]
(* 0 *)

(* ::Text:: *)
(*EffectiveAction2PI[..., "Statistics" -> "Superfield"] is the 2PI functional of the superfield, with the pairing sources and the means; it is exact at its stationary point also at finite means. The showcase (section 13) has it to third order, with the Thouless criterion and an exact check on one site with pairing.*)

(* ::Section:: *)
(*10. Vertices, Hedin's equations and GW*)

(* ::Text:: *)
(*An external field U coupled to a one-body operator (the density, a current, a spin density) changes the propagator, and the change defines a three-point vertex Λ = −δG^-1/δU, so that δG = G Λ G. For the density ρ = ψ̄ψ the potential enters the bare propagator, G0⁻¹ → G0⁻¹ − U, so that δG0(p, q)/δU(z) = G0(p, z) G0(z, q), a rule declared once. With G⁻¹ = G0⁻¹ − U − Σ, the vertex is Λ = γ + δΣ/δU, γ = δ(x1, z) δ(x2, z): FunctionalDerivative inserts the potential into every line of the perturbative self-energy of section 5, and the skeleton diagrams, with dressed lines, are Λ[G]; the density sits at z:*)

DeclareSource[LocalPotential];
DeclareFunctionalDerivative[BareGreenFunction[p_, q_], LocalPotential[r_],
    BareGreenFunction[p, r] BareGreenFunction[r, q]];
densityVertex = DeltaFunction[x1, z] DeltaFunction[x2, z] + (SkeletonDiagrams[
    FunctionalDerivative[selfEnergyPerturbative /. {x -> x1, y -> x2}, LocalPotential[z]],
    {x1, x2, z}, {BareGreenFunction}] /. BareGreenFunction -> DressedGreenFunction);
DiagramPlot[OrderPart[densityVertex, InteractionPotential, 1], {x1, x2, z}]
(* first order: an interaction line between the two legs, and the bubble that screens
   the field *)

(* ::Text:: *)
(*PerturbativeDensityVertex computes the same from ⟨ψ ψ̄ ρ⟩_c, with the two fermion legs removed:*)

SimplifyDiagrams[PerturbativeDensityVertex[FreeFermionGeneratingFunctional, CoulombVertex, 2,
    {psi, psibar}, {x1, x2, z}, BareGreenFunction -> DressedGreenFunction] - densityVertex]
(* 0 *)

(* ::Text:: *)
(*The full objects obey exact equations. ExactEquation writes them with the indices contracted and the signs of the conventions of the package. The objects are heads, drawn as blobs (DeclareBlob), or functions of their labels (LabelFunction of a perturbative series); CheckEquation then checks the equation order by order. The Dyson equation, drawn:*)

DeclareBlob[SigmaBlob, "\\Sigma"]; DeclareLineStyle[FullG, "DoubleArrow"];
DiagramPlot[Last[ExactEquation["Dyson", {"G" -> FullG, "G0" -> BareGreenFunction,
    "Sigma" -> SigmaBlob}, {x, y}]], {x, y}]
(* G0 + G0 Sigma G, with G a double line *)

(* ::Text:: *)
(*The vertex obeys a Bethe–Salpeter equation, Λ = γ + Γ⁰L₀Λ (γ is the bare vertex, L₀ the pair of lines), whose solution is Λ = γ + FL₀γ; the equation of motion of ψ gives the self-energy from the vertex, Σ = Σ_Hartree − V G Λ. With the perturbative objects of sections 7 and 8, all three hold to second order:*)

objects = {"G" -> DressedGreenFunction, "V" -> InteractionPotential,
    "Sigma" -> LabelFunction[selfEnergy, {x, y}],
    "Lambda" -> LabelFunction[densityVertex, {x1, x2, z}],
    "Gamma0" -> LabelFunction[irreducibleVertex, {x1, x2, x3, x4}],
    "F" -> LabelFunction[fullVertex, {x1, x2, x3, x4}]};
{CheckEquation[ExactEquation["Vertex", objects, {x1, x2, z}], InteractionPotential, 2],
 CheckEquation[ExactEquation["VertexFromF", objects, {x1, x2, z}], InteractionPotential, 2],
 CheckEquation[ExactEquation["SelfEnergyFromVertex", objects, {x, y}], InteractionPotential, 2]}
(* {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}} *)

(* ::Text:: *)
(*Hedin's equations separate from the vertex the chains of bubbles that screen the interaction: with the proper vertex Λ̃, the polarization P = L₀Λ̃ (the pair of lines closed on Λ̃) and the screened interaction W = V + V P W, the self-energy is Σ = Σ_Hartree − G W Λ̃ (ExactEquation["Hedin", ...]; the showcase checks the four equations to third order). Λ̃ and P are Hedin's derivatives with respect to the total potential U_tot = U + V⟨ρ⟩: Λ̃ = −δG⁻¹/δU_tot and P = δ⟨ρ⟩/δU_tot, the vertex Λ and the density response without the chains joined by one interaction line, which IrreducibleDiagrams removes by cutting interaction lines (section 11). Replacing Λ̃ by the bare vertex gives the GW approximation. W is drawn as a double wavy line:*)

DeclareSymmetric[ScreenedW]; DeclareInteractionLine[ScreenedW, "DoubleWavy"];
gw = ExactEquation["Hedin", {"G" -> DressedGreenFunction, "V" -> InteractionPotential,
    "W" -> ScreenedW, "Sigma" -> SigmaBlob, "P" -> PolarizationBlob, "GammaXC" -> XCBlob,
    "Lambda" -> LabelFunction[DeltaFunction[x1, z] DeltaFunction[x2, z], {x1, x2, z}]},
    {x, y, z}];
DiagramPlot[Last[First[gw]], {x, y}]
(* the Hartree term (-1), and -G W: the GW self-energy (+1, the line W standing for -W) *)

(* ::Text:: *)
(*Hubbard–Stratonovich. The interaction between densities is the exchange of a boson φ with ⟨φφ⟩_0 = V, coupled by −i∫φ ψ̄ψ: integrating out φ gives back exp(−½∫ρVρ). HubbardStratonovich does this for any interaction ½∫K O O, given the operator O (here the density); HSPropagator is the propagator of φ, standing for V. For the package the result is a theory of fermions and bosons like any other, and it gives the same Green function:*)

hs = HubbardStratonovich[{FreeFermionGeneratingFunctional, CoulombVertex},
    psibar[#] ** psi[#] &, {hsField, Jhs}, HSPropagator];
FreeHSGeneratingFunctional = hs["FreeGeneratingFunctional"];
HSVertex = hs["Interaction"]
(* -I IntegralOver[hsField[z1] psibar[z1] ** psi[z1], {z1}], up to the order of the fields *)
hsGreenFunction = -PerturbativeCorrelationFunction[FreeHSGeneratingFunctional,
    {psi[x], psibar[y]}, HSVertex, 4];
SimplifyDiagrams[hsGreenFunction - (greenFunction /. InteractionPotential -> HSPropagator)]
(* 0: the same diagrams, with two boson-fermion vertices for each V *)

SimplifyDiagrams[(IntegrateOutField[{FreeHSGeneratingFunctional, HSVertex}, hsField, Jhs][
    "Interaction"] /. HSPropagator -> InteractionPotential) - CoulombVertex]
(* 0: IntegrateOutField integrates the boson out, and the interaction comes back *)

(* ::Text:: *)
(*The full boson propagator is the screened interaction W. A Legendre transform in both propagators gives the 2PI functional Φ[G, W]: LuttingerWardFunctional with two replacement rules. Its first diagram gives the GW self-energy, and the next one the first vertex correction:*)

phiGW = LuttingerWardFunctional[FreeHSGeneratingFunctional, HSVertex, 4,
    {BareGreenFunction -> DressedGreenFunction, HSPropagator -> ScreenedW}];
DiagramPlot[phiGW]
(* -1/2 G W G (drawn with the weight 1/2), and the diagram with two crossed W lines *)

FunctionalDerivative[OrderPart[phiGW, ScreenedW, 1], DressedGreenFunction[y, x]]
(* -DressedGreenFunction[x, y] ScreenedW[x, y]: Sigma = dPhi/dG = -G W *)

(* ::Section:: *)
(*11. More physics with the same tools*)

(* ::Subsection:: *)
(*Linear response*)

(* ::Text:: *)
(*The density response is a derivative: with the potential U of section 10, χ(a, b) = δ⟨ρ(a)⟩/δU(b) = δG(a, a)/δU(b), and ⟨ρ(a) ρ(b)⟩c = −χ. To first order: the bubble, the self-energy insertions, the vertex correction and the first RPA term. The density ρ(x) = ψ̄(x) ψ(x) is a composite operator, and ConnectedCorrelationFunction computes the same function as the connected four-point function with the fields taken pairwise at the same point. IrreducibleDiagrams, cutting interaction lines instead of propagators, keeps the proper polarization.*)

densityCorrelation = -FunctionalDerivative[OrderPart[greenFunction, InteractionPotential,
    {0, 1}] /. {x -> a, y -> a}, LocalPotential[b]];
DiagramPlot[densityCorrelation, {a, b}]
(* the bubble; Hartree and Fock insertions on each line, the vertex correction and two
   bubbles joined by V *)

SimplifyDiagrams[ConnectedCorrelationFunction[FreeFermionGeneratingFunctional,
    {psibar[a], psi[a], psibar[b], psi[b]}, CoulombVertex, 1] - densityCorrelation]
(* 0 *)

SimplifyDiagrams[densityCorrelation
    - IrreducibleDiagrams[densityCorrelation, {{a}, {b}}, 1, {InteractionPotential}]]
(* the first RPA term: two bubbles joined by an interaction line *)

(* ::Subsection:: *)
(*From diagrams to numbers: Matsubara sums*)

(* ::Text:: *)
(*For a translation-invariant system the diagrams become integrals over momenta and sums over Matsubara frequencies. EvaluateMatsubara routes momenta and frequencies through the lines (a fermion line is 1/(iω − ξ_p), V an instantaneous v(q)), sums the frequency of each loop by residues, so that Fermi and Bose functions appear, and leaves the momentum integrals as MomentumSum. The bubble of the density correlation is minus the Lindhard function:*)

DeclareMatsubaraFrequency[w, "Fermion"]; DeclareMatsubaraFrequency[nu, "Boson"];
gasRules = {BareGreenFunction -> {"Fermion", xi}, DressedGreenFunction -> {"Fermion", xi},
    InteractionPotential -> {"Instantaneous", v}};
EvaluateMatsubara[OrderPart[densityCorrelation, InteractionPotential, 0],
    {b -> {q, I nu}, a -> {-q, -I nu}}, gasRules, "SpinDegeneracy" -> 2]
(* -2 MomentumSum[(n_F(xi[p]) - n_F(xi[p + q]))/(I nu + xi[p] - xi[p + q]), {p}], with p = LoopMomentum[1] *)

(* ::Text:: *)
(*Continued to real frequency, the imaginary part of the second-order self-energy is the rate of the decay into a particle and a particle–hole pair. The package obtains it from the residues and, independently, from the cutting rules at finite temperature (the optical theorem); CheckCuttingRules compares the two:*)

selfEnergy2 = OrderPart[selfEnergy, InteractionPotential, 2];
CheckCuttingRules[selfEnergy2, {y -> {k, I w}, x -> {-k, -I w}}, gasRules, w -> omega,
    "SpinDegeneracy" -> 2]
(* 0 *)

(* ::Subsection:: *)
(*Electrons and phonons*)

(* ::Text:: *)
(*Electrons coupled to a phonon field, g ∫ φ(z) ψ̄(z) ψ(z): the free generating functional is the product of the fermionic and the bosonic one, and phonon lines are dashed. The self-energy is a derivative, as in section 7: the 2PI functional Φ[G, D] of the full electron and phonon propagators is LuttingerWardFunctional with two replacement rules (as Φ[G, W] in section 10), and its first diagram gives Migdal's self-energy Σ = δΦ/δG = g² G D (the phonon line is ⟨φφ⟩, minus the D of the textbooks):*)

DeclareSource[Jphonon]; DeclareField[phonon, Jphonon];
DeclareSymmetric[PhononPropagator, DressedPhononPropagator];
DeclareDisplayName[PhononPropagator, "D0"]; DeclareDisplayName[DressedPhononPropagator, "D"];
DeclareLineStyle[PhononPropagator, "Dashed"]; DeclareLineStyle[DressedPhononPropagator, "Dashed"];
FreeElectronPhononGeneratingFunctional = FreeFermionGeneratingFunctional Exp[
    1/2 IntegralOver[Jphonon[x] PhononPropagator[x, y] Jphonon[y], {x, y}]];
ElectronPhononVertex = g IntegralOver[phonon[z] psibar[z] ** psi[z], {z}];
phiElectronPhonon = LuttingerWardFunctional[FreeElectronPhononGeneratingFunctional,
    ElectronPhononVertex, 2, {BareGreenFunction -> DressedGreenFunction,
    PhononPropagator -> DressedPhononPropagator}];
migdalSelfEnergy = FunctionalDerivative[phiElectronPhonon, DressedGreenFunction[y, x]]
(* g^2 G[x, y] D[x, y]: Migdal's self-energy *)

(* ::Text:: *)
(*The check by perturbation theory. Exchanging a phonon is an effective retarded interaction: the electron Green function to order g^2 is that of section 5 to first order with V → −g^2 D0. Its self-energy is Migdal's, with bare lines, plus the phonon tadpole, the mean field of the phonon, which is not part of Φ[G, D] (it belongs to the mean field φ̄ of the 2PI action):*)

electronGreenFunction = -PerturbativeCorrelationFunction[
    FreeElectronPhononGeneratingFunctional, {psi[x], psibar[y]}, ElectronPhononVertex, 2];
DiagramPlot[electronGreenFunction, {x, y}, "Couplings" -> {g}]
(* G0, the phonon tadpole (Hartree) and the phonon exchange (Fock); with "Couplings" -> {g}
   each vertex stands for -g (g is the coupling of QCD below too, whose local vertices stand
   for minus their kernels: it is not declared with DeclareCoupling) *)

phononAsInteraction = InteractionPotential -> (-g^2 PhononPropagator[#1, #2] &);
phononTadpole = -g^2 DeltaFunction[x, y] IntegralOver[PhononPropagator[x, u]
    BareGreenFunction[u, u], {u}];
{SimplifyDiagrams[OrderPart[electronGreenFunction, g, 2]
    - (OrderPart[greenFunction, InteractionPotential, 1] /. phononAsInteraction)],
 SimplifyDiagrams[AmputateExternalLegs[ParticleIrreducibleDiagrams[OrderPart[
    electronGreenFunction, g, 2], 1, {x, y}, {BareGreenFunction}], {x, y}, {BareGreenFunction}]
    - phononTadpole - (migdalSelfEnergy /. {DressedGreenFunction -> BareGreenFunction,
    DressedPhononPropagator -> PhononPropagator})]}
(* {0, 0} *)

(* ::Subsection:: *)
(*Disorder*)

(* ::Text:: *)
(*A static Gaussian random potential u(x), with ⟨u(x) u(y)⟩ = Δ_imp(x, y), is one more Gaussian field: the disorder average is Wick's theorem again. For the quenched average the diagrams with closed fermion loops are dropped (they vanish in the replica limit). Impurity lines are dashed, with a cross. The self-energy of the averaged propagator is again a derivative of a functional of G, 2PI in the fermion and in the impurity lines (the impurity line is not dressed: its self-energy would be a closed loop); its first diagram gives the self-consistent Born approximation, its second the crossed diagram:*)

DeclareSource[Jimpurity]; DeclareField[impurityPotential, Jimpurity];
DeclareSymmetric[DisorderCorrelator, FullDisorderCorrelator];
DeclareDisplayName[DisorderCorrelator, "\[CapitalDelta]imp"];
DeclareLineStyle[DisorderCorrelator, "Impurity"];
FreeDisorderGeneratingFunctional = FreeFermionGeneratingFunctional Exp[
    1/2 IntegralOver[Jimpurity[x] DisorderCorrelator[x, y] Jimpurity[y], {x, y}]];
ImpurityVertex = IntegralOver[impurityPotential[z] psibar[z] ** psi[z], {z}];
phiDisorder = LuttingerWardFunctional[FreeDisorderGeneratingFunctional, ImpurityVertex, 4,
    {BareGreenFunction -> DressedGreenFunction, DisorderCorrelator -> FullDisorderCorrelator}] /.
    FullDisorderCorrelator -> DisorderCorrelator;
disorderSelfEnergyG = DropClosedLoops[SimplifyDiagrams[
    FunctionalDerivative[phiDisorder, DressedGreenFunction[y, x]]], {DressedGreenFunction}];
DiagramPlot[disorderSelfEnergyG, {x, y}]
(* the self-consistent Born diagram; at order Delta_imp^2 the crossed diagram *)

(* ::Text:: *)
(*The check by perturbation theory: the perturbative self-energy has the Born diagram and, at order Δ_imp², the rainbow and the crossed diagram; the rainbow is the Born diagram of Σ[G] with G expanded to first order, G = G0 + G0 Σ_Born G0:*)

disorderAveragedGreenFunction = DropClosedLoops[-PerturbativeCorrelationFunction[
    FreeDisorderGeneratingFunctional, {psi[x], psibar[y]}, ImpurityVertex, 4],
    {BareGreenFunction}];
disorderSelfEnergy = AmputateExternalLegs[ParticleIrreducibleDiagrams[
    disorderAveragedGreenFunction, 1, {x, y}, {BareGreenFunction}], {x, y},
    {BareGreenFunction}];
DiagramPlot[disorderSelfEnergy, {x, y}]
(* the Born diagram; at order Delta_imp^2 the rainbow and the crossed diagram *)

bornG[p_, q_] := BareGreenFunction[p, q] + IntegralOver[BareGreenFunction[p, u]
    DisorderCorrelator[u, v] BareGreenFunction[u, v] BareGreenFunction[v, q], {u, v}];
SimplifyDiagrams[disorderSelfEnergy - OrderPart[OrderPart[disorderSelfEnergyG, DisorderCorrelator, 1]
    /. DressedGreenFunction[p_, q_] :> bornG[p, q], DisorderCorrelator, {1, 2}]
    - (OrderPart[disorderSelfEnergyG, DisorderCorrelator, 2] /. DressedGreenFunction -> BareGreenFunction)]
(* 0 *)

(* ::Subsection:: *)
(*Dimensional regularization: φ^4 theory at one loop*)

(* ::Text:: *)
(*At zero temperature the same routing gives Euclidean loop integrals, which IntegrateLoops evaluates in d = 4 − 2ε dimensions, every loop with the MS-bar measure μ^(2ε) (e^γ/4π)^ε ∫d^dk/(2π)^d (μ is RenormalizationScale), and EpsilonExpand expands in ε. The massless bubble is a G-function; in units of 1/(16π²):*)

bubble = IntegrateLoops[LoopIntegral[ScalarPropagator[k, 0] ScalarPropagator[k + p, 0],
    {k}], eps];
EpsilonExpand[16 Pi^2 bubble, eps, 0]
(* 2 + 1/eps - Log[p^2/RenormalizationScale^2] *)

(* ::Text:: *)
(*The 1PI four-point vertex Γ⁽⁴⁾, minus the amputated 1PI part of the connected four-point function, has one bubble in each channel at order λ². InfraredRearrangedIntegral evaluates it with one momentum p through two of its legs, the massless lines given by the rule FreePropagator -> {"Scalar", 0}. Its pole is cancelled by λ → Z_λ λ, with Z_λ = 1 − (pole part)/λ at one loop, and BetaFunctionFromZ gives the beta function μ dλ/dμ:*)

vertexOneLoop = -AmputateExternalLegs[ParticleIrreducibleDiagrams[
    OrderPart[connectedFourPointPhi4, lambda, {1, 2}], 1, {a, b, c, d}, {FreePropagator}],
    {a, b, c, d}, {FreePropagator}];
vertexPole = PolePart[InfraredRearrangedIntegral[vertexOneLoop, {a, b, c, d}, p,
    {FreePropagator -> {"Scalar", 0}}, eps], eps]
(* -3 lambda^2/(32 Pi^2 eps): the three bubbles, -lambda^2/2 each *)
BetaFunctionFromZ[1 - vertexPole/lambda, lambda, eps, 2]
(* 3 lambda^2/(16 Pi^2): lambda grows at short distances; at two loops the showcase finds
   -17/3 lambda^3/(16 Pi^2)^2, and the Wilson-Fisher exponents in d = 4 - e *)

(* ::Subsection:: *)
(*QED at one loop*)

(* ::Text:: *)
(*In QED a label stands for a point with its spinor or Lorentz index, and the vertex is a local kernel, −iγ^μ times delta functions. The electron has the charge −e, with e > 0, so that S_int = e ∫ ψ̄ γ^μ ψ A_μ. The package expands exp(−S); for the exp(iS) of Minkowski space the interaction passed is −iS_int, so that each vertex is +ieγ^μ (Peskin and Schroeder, with e → −e). QEDIntegrand writes the diagrams in momentum space, an open fermion line as a product of Dirac matrices and a closed loop as a trace, with the photon in a covariant gauge ξ; IntegrateOneLoop does the loop integral by Feynman parameters, exactly in d = 4 − 2ε, and ExpandParameterIntegrals expands in ε. The 1PI functions are the derivatives of Γ[Ψ̄, Ψ, A]. EffectiveAction1PI builds the action of one real boson or of one fermion field; with several fields the package takes the other side of the Legendre transform (section 6): the connected functions, the derivatives of W = log Z, are trees of the vertex functions, so that at one loop the 1PI functions are the 1PI parts of the connected functions, with their legs removed (qedOnePI). The pole of the electron self-energy:*)

DeclareSource[Jphoton]; DeclareField[photon, Jphoton]; DeclareField[electron, etabar];
DeclareField[electronbar, eta, -1]; DeclareSymmetric[PhotonLine]; DeclareLocalVertex[QEDVertex];
FreeQED = Exp[1/2 IntegralOver[Jphoton[x] PhotonLine[x, y] Jphoton[y], {x, y}] +
    IntegralOver[ElectronLine[x, y] etabar[x] ** eta[y], {x, y}]];
qedVertex = e IntegralOver[QEDVertex[x, y, z] electronbar[x] ** electron[y] photon[z],
    {x, y, z}];
qedOnePI[fields_, labels_, n_] := ParticleIrreducibleDiagrams[AmputateExternalLegs[
    ConnectedCorrelationFunction[FreeQED, fields, qedVertex, n, "CouplingOrder" -> {e, n}],
    labels, {PhotonLine, ElectronLine}], 1, labels, {PhotonLine, ElectronLine}];
qedRules = {ElectronLine -> {"Fermion", m}, PhotonLine -> {"Photon", xi},
    QEDVertex -> {"Vertex", -I}};
electronSelfEnergy = DiracSimplify[I IntegrateOneLoop[QEDIntegrand[qedOnePI[
    {electron[x], electronbar[y]}, {x, y}, 2], {y -> p, x -> -p}, qedRules], eps]];
ExpandParameterIntegrals[electronSelfEnergy, eps, -1]
(* (e^2/(16 Pi^2 eps)) (m (3 + xi) - xi pslash): Z_m = 1 - 3 alpha/(4 Pi eps), the same in
   every gauge *)

(* ::Text:: *)
(*The vertex correction between on-shell spinors, split by VertexFormFactors into F1, F2 and F3; at zero momentum transfer F2 is the anomalous magnetic moment of the electron, Schwinger's α/2π, in every gauge:*)

vertexCorrection = -I/e IntegrateOneLoop[QEDIntegrand[qedOnePI[{electron[x1],
    electronbar[x2], photon[x3]}, {x1, x2, x3}, 3], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2},
    qedRules, "Indices" -> {x3 -> mu}], eps];
formFactors = VertexFormFactors[DiracSimplify[SpinorChain[{p2, m}, vertexCorrection,
    {p1, m}]] /. ScalarProduct[p1, p2] -> m^2 - q2/2, mu, {p1, p2, m}];
ExpandParameterIntegrals[formFactors[[2]] /. q2 -> 0, eps, 0]
(* e^2/(8 Pi^2) = alpha/(2 Pi); Tests/QEDOneLoop.wl goes on to the vacuum polarization,
   beta(e) = e^3/(12 Pi^2), and the Ward identity Z1 = Z2 *)

(* ::Subsection:: *)
(*QCD*)

(* ::Text:: *)
(*In condensed notation a label stands for a point, a Lorentz index and a colour index (and a spinor index for quarks); the interaction of QCD is the sum of four vertices, whose kernels contain the colour factors, the Dirac matrices and the derivatives of the Feynman rules. The ghosts are Grassmann fields with a propagator of their own. DeclareLocalVertex draws the kernels as points; gluons are coils ("Curly"), ghosts dotted lines with an arrow ("DottedArrow").*)

DeclareSource[Jgluon]; DeclareGrassmannSource[chi, chibar, sigma, sigmabar];
DeclareField[gluon, Jgluon]; DeclareField[quark, chibar]; DeclareField[quarkbar, chi, -1];
DeclareField[ghost, sigmabar]; DeclareField[ghostbar, sigma, -1];
DeclareSymmetric[GluonPropagator, ThreeGluonVertex, FourGluonVertex];
DeclareLocalVertex[QuarkGluonVertex, ThreeGluonVertex, FourGluonVertex, GhostGluonVertex];
DeclareLineStyle[GluonPropagator, "Curly"];
DeclareLineStyle[GhostPropagator, "DottedArrow"];
DeclareDirectedPropagator[QuarkPropagator];
FreeQCDGeneratingFunctional = Exp[
    1/2 IntegralOver[Jgluon[x] GluonPropagator[x, y] Jgluon[y], {x, y}]
    + IntegralOver[QuarkPropagator[x, y] chibar[x] ** chi[y], {x, y}]
    + IntegralOver[GhostPropagator[x, y] sigmabar[x] ** sigma[y], {x, y}]];
QCDVertices =
   g IntegralOver[QuarkGluonVertex[x, y, z] quarkbar[x] ** quark[y] gluon[z], {x, y, z}] +
   g/6 IntegralOver[ThreeGluonVertex[x, y, z] gluon[x] gluon[y] gluon[z], {x, y, z}] +
   g^2/24 IntegralOver[FourGluonVertex[w, x, y, z] gluon[w] gluon[x] gluon[y] gluon[z],
     {w, x, y, z}] +
   g IntegralOver[GhostGluonVertex[x, y, z] ghostbar[x] ** ghost[y] gluon[z], {x, y, z}];

(* ::Text:: *)
(*The gluon self-energy is a derivative of the 1PI action, Γ^(2) = D⁻¹ − Π; as for QED, at one loop Π is the 1PI part of the connected two-point function at order g^2, with its legs removed. The quark and the ghost loops carry the −1 of a closed Grassmann loop, the gluon loop and the tadpole the symmetry factor ½.*)

qcdLines = {GluonPropagator, QuarkPropagator, GhostPropagator};
gluonTwoPoint = OrderPart[ConnectedCorrelationFunction[FreeQCDGeneratingFunctional,
    {gluon[x], gluon[y]}, QCDVertices, 2], g, 2];
gluonSelfEnergy = ParticleIrreducibleDiagrams[
    AmputateExternalLegs[gluonTwoPoint, {x, y}, qcdLines], 1, {x, y}, qcdLines];
DiagramPlot[gluonSelfEnergy, {x, y}]
(* the tadpole (g^2/2: one vertex, standing for minus its kernel), the ghost loop (-g^2),
   the quark loop (-g^2) and the gluon loop (g^2/2) *)

(* ::Section:: *)
(*12. Pictures and formulas for a paper*)

(* ::Text:: *)
(*DiagramPlot, DrawDiagram (one term) and DiagramToTikZ take the same options, among them "VertexLabels" (Automatic, All, None), "LegDirections", "ExternalOrder", "Columns" and "Weights" ("Coefficients": the coefficients of the terms, instead of the weights of the pictures, in which an interaction line stands for −V and a vertex for minus its coupling); SetOptions changes their defaults. Line styles are declared once with DeclareLineStyle: "Plain", "Arrow", "DoubleArrow", "DottedArrow", "Double", "Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag", "Impurity", "DivergingArrows", "ConvergingArrows". A dressed propagator is usually drawn as a double line:*)

DeclareLineStyle[DressedGreenFunction, "DoubleArrow"];
DiagramPlot[selfEnergy, {x, y}, "VertexLabels" -> All]
(* the skeleton self-energy, with the internal points z1, z2, ... labelled *)

(* ::Text:: *)
(*DiagramToTikZ writes the same pictures as TikZ code, with the weights in LaTeX, ready to be pasted into an equation (it needs \usetikzlibrary{decorations.pathmorphing}). "Scale" (cm per unit), "LineWidth" (pt) and "LabelFont" make the pictures smaller:*)

SetOptions[DiagramToTikZ, "Scale" -> 0.5, "LineWidth" -> 0.5,
    "LabelFont" -> "\\scriptsize"];
tikzCode = DiagramToTikZ[selfEnergy, {x, y}];
StringTake[tikzCode, 300]
(* the beginning of the code; Export["self_energy.tex", tikzCode, "Text"] writes it to a
   file *)

(* ::Text:: *)
(*ExpressionToLaTeX writes any result as a formula, with the display names of DeclareDisplayName (or the option "Names"):*)

ExpressionToLaTeX[hartreeFockSelfEnergy]
(* \delta(x,y)\,\int_{z_{1}}G(z_{1},z_{1})\,V(x,z_{1}) - G(x,y)\,V(x,y) *)

(* ::Text:: *)
(*ExactEquation draws its equations with blobs (section 10), and so does any expression written by hand: DeclareBlob draws a factor as a blob with its name inside. The right-hand side of the Dyson equation G = G0 + G0 Σ G, with SigmaBlob and the double line FullG of section 10:*)

dysonEquation = BareGreenFunction[x, y] +
    IntegralOver[BareGreenFunction[x, u] SigmaBlob[u, v] FullG[v, y], {u, v}];
DiagramPlot[dysonEquation, {x, y}]

(* ::Section:: *)
(*13. Your own objects*)

(* ::Text:: *)
(*Any symbol can be a function, a propagator or a vertex: declare its symmetries (DeclareSymmetric), how it is drawn (DeclareLineStyle, DeclareInteractionLine, DeclareLocalVertex, DeclareBlob) and how it is printed (DeclareDisplayName: the input keeps the explicit name, the output shows a short one). Objects that depend on the sources implicitly are differentiated with rules declared once by DeclareFunctionalDerivative. A propagator in a local source, G = (G0⁻¹ − J)⁻¹, obeys δG(a, b)/δJ(c) = G(a, c) G(c, b), and its second derivative follows:*)

DeclareSymmetric[PropagatorOfJ]; DeclareDisplayName[PropagatorOfJ, "G"];
DeclareFunctionalDerivative[PropagatorOfJ[p_, q_], J[r_],
    PropagatorOfJ[p, r] PropagatorOfJ[r, q]];
FunctionalDerivative[PropagatorOfJ[x, y], J[u], J[v]]
(* G[x, u] G[u, v] G[v, y] + G[x, v] G[v, u] G[u, y] *)

(* ::Text:: *)
(*The rules of the tree expansion of section 6 are rules of this kind: ConnectedFunctionFromVertices declares them for FullPropagator and VertexFunction and differentiates.*)

(* ::Subsection:: *)
(*Where to go next*)

(* ::Item:: *)
(*The showcase, Showcase/FunctionalDiagrams_Showcase.pdf: the same path to third order, with the derivations, the checks and all the pictures.*)

(* ::Item:: *)
(*The manual, Documentation/FunctionalDiagrams_Manual.pdf: every function, option and convention, with examples.*)

(* ::Item:: *)
(*In Mathematica, ?Name shows the usage of a function, for example ?PerturbativeCorrelationFunction.*)
