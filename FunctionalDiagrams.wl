(* ::Package:: *)

(* ========================================================================= *)
(*  FunctionalDiagrams.wl                                                     *)
(*                                                                            *)
(*  Functional derivatives and Feynman diagrams.                              *)
(*                                                                            *)
(*  Functional derivatives with respect to bosonic and Grassmann sources,     *)
(*  Wick contractions of Gaussian generating functionals, perturbation       *)
(*  theory, classes of diagrams (connected, n-particle irreducible,           *)
(*  skeleton, amputated), the Luttinger-Ward functional with its self-energy  *)
(*  and irreducible vertex, two-particle channels, Bethe-Salpeter ladders and *)
(*  the parquet decomposition, the tree expansion of connected functions      *)
(*  in vertex functions, the 2PI and 1PI effective actions and Potthoff's     *)
(*  functionals, pictures of every result (Mathematica graphics or TikZ) and  *)
(*  formulas in LaTeX.                                                        *)
(*                                                                            *)
(*  Author:  Nazzareno Africani (SISSA)                                       *)
(*  License: GNU GPL v3 or later (LICENSE), with additional terms (NOTICE)    *)
(*                                                                            *)
(*  Guiding principle: weights are never put in by hand. Diagrams are         *)
(*  produced by differentiating generating functionals (or, equivalently, by  *)
(*  Wick's theorem), and the package only recognizes when two terms are the   *)
(*  same diagram, so that their weights add up. Symmetry factors and          *)
(*  fermionic signs come out by themselves.                                   *)
(*                                                                            *)
(*  Representation: every expression is a sum of terms                       *)
(*      c * IntegralOver[ f * GrassmannProduct[odd factors], {dummies} ]      *)
(*  where f commutes. Integration variables are renamed canonically           *)
(*  (displayed z1, z2, ...), so that equal diagrams are recognized.           *)
(* ========================================================================= *)

BeginPackage["FunctionalDiagrams`"];

(* a fresh start when the package is loaded again in the same session. $FunctionalDiagramsUpdateCheck keeps its
   setting (section 21): that of the package's symbol when the package is loaded again, or that of the symbol made by
   typing the name before the package was first loaded (a symbol of Global`), which is removed before the package's own
   symbol is made, so that neither shadows the other (::shdw). The symbols are named by strings, since a name in this
   code would make the package's symbol at once *)
With[{FunctionalDiagrams`Private`others = Select[Names["*`$FunctionalDiagramsUpdateCheck"],
     StringContainsQ[#, "`"] && !StringStartsQ[#, "FunctionalDiagrams`"] &]},
  With[{FunctionalDiagrams`Private`setting = Join @@ (If[ToExpression[#, InputForm, ValueQ], {ToExpression[#]}, {}] &) /@
       Join[Names["FunctionalDiagrams`$FunctionalDiagramsUpdateCheck"], FunctionalDiagrams`Private`others]},
   Scan[Quiet[Remove[#]] &, FunctionalDiagrams`Private`others];
   Unprotect["FunctionalDiagrams`*"];
   ClearAll["FunctionalDiagrams`*", "FunctionalDiagrams`Private`*"];
   If[FunctionalDiagrams`Private`setting =!= {},
    Set @@ {Symbol["FunctionalDiagrams`$FunctionalDiagramsUpdateCheck"], Last[FunctionalDiagrams`Private`setting]}]]];

$FunctionalDiagramsVersion::usage = "$FunctionalDiagramsVersion is the version of the FunctionalDiagrams package.";
FunctionalDiagrams::usage = "FunctionalDiagrams derives Feynman diagrams by differentiating generating functionals; ?FunctionalDiagrams`* lists its functions. Its messages are FunctionalDiagrams::name.";

(* ---------------------------------------------------------------- notation *)
IntegralOver::usage = "IntegralOver[f, {x, y, ...}] is the integral (or sum) of f over the integration variables x, y, ... (space, time, spin and any other label they stand for). Integration variables must be symbols; IntegralOver[f, x] is IntegralOver[f, {x}].";
DeltaFunction::usage = "DeltaFunction[x, y] is the delta function \[Delta](x - y) of two labels. It is symmetric and is integrated automatically against IntegralOver variables.";
GrassmannProduct::usage = "GrassmannProduct[a, b, ...] is the ordered product of Grassmann-odd factors. Products entered with ** (NonCommutativeMultiply) are converted to it.";
DummyVariable::usage = "DummyVariable[depth, k] is the k-th canonical integration variable at nesting depth depth; it is displayed as z_k (w_k, u_k, ... inside nested integrals).";

(* ------------------------------------------------------------ declarations *)
DeclareSource::usage = "DeclareSource[J1, J2, ...] declares commuting (bosonic) source heads: J[x] is then differentiated by FunctionalDerivative.";
DeclareGrassmannSource::usage = "DeclareGrassmannSource[eta, etabar, ...] declares anticommuting (Grassmann) source heads.";
DeclareGrassmannOdd::usage = "DeclareGrassmannOdd[psi, ...] declares heads of Grassmann-odd quantities that do not depend on the sources (for instance fermion fields inside an interaction).";
DeclareGrassmannGraded::usage = "DeclareGrassmannGraded[h, ...] declares heads whose Grassmann parity is the number of their arguments mod 2 and which depend on the sources (odd ones vanish at zero sources). Used by ConnectedFunctionFromVertices for fermions (superfield rules).";
DeclareSymmetric::usage = "DeclareSymmetric[h, ...] makes h totally symmetric in its arguments (for example a bosonic propagator or a density-density interaction).";
DeclareAntisymmetric::usage = "DeclareAntisymmetric[h, ...] makes h totally antisymmetric in its arguments (h[b, a] evaluates to -h[a, b], h[a, a] to 0: in a pattern or a rule write h[__], never h[_, _]). The derivative with respect to an antisymmetric kernel, FunctionalDerivative[F, h[x, y]], is antisymmetrised, and the kernel of the inverse of an antisymmetric operator, OperatorKernel[h^-1], is antisymmetric: a superfield propagator, the Nambu matrix of (psi, psibar) with the normal block G and the anomalous blocks -<T psi psi> and -<T psibar psibar>, is such a kernel.";
DeclareFunctionalDerivative::usage = "DeclareFunctionalDerivative[object[a_, ...], source[c_], rhs] declares the functional derivative \[Delta]object/\[Delta]source = rhs of an object that depends on the sources implicitly. Integration variables in rhs are renamed automatically.";
DeclareDiscreteIndexValues::usage = "DeclareDiscreteIndexValues[{up, down, ...}] declares discrete index values: DeltaFunction[up, up] = 1 and DeltaFunction[up, down] = 0.";
DeclareField::usage = "DeclareField[phi, J] associates the field phi with the source J (phi[x] stands for \[Delta]/\[Delta]J[x]); DeclareField[psibar, eta, -1] associates psibar[x] with -\[Delta]/\[Delta]eta[x]. Fields attached to Grassmann sources are Grassmann odd.";
DeclareLineStyle::usage = "DeclareLineStyle[h, style] sets how a two-label factor h[x, y] is drawn: \"Plain\", \"Arrow\" (a propagator running from y to x), \"DoubleArrow\" (a dressed propagator), \"DottedArrow\" (a ghost), \"Double\", \"Wavy\", \"DoubleWavy\" (a dressed interaction beside the bare one, e.g. the screened W next to V), \"Curly\" (a gluon), \"Dashed\", \"Dotted\", \"Zigzag\", \"Impurity\" (dashed, with a cross), \"DivergingArrows\" (an anomalous propagator -<T psi(x) psi(y)>: two arrowheads pointing to its ends) or \"ConvergingArrows\" (-<T psibar(x) psibar(y)>: two arrowheads pointing to its middle). The seven from \"Wavy\" to \"Impurity\" are drawn as interaction lines; the arrows run on through every vertex and turn only inside an anomalous line.";
DeclareDirectedPropagator::usage = "DeclareDirectedPropagator[G, ...] is DeclareLineStyle[G, \"Arrow\"]: G[x, y] is drawn with an arrow from y to x (a fermion propagating from y to x).";
DeclareInteractionLine::usage = "DeclareInteractionLine[V, ...] declares interaction lines, kernels of the interaction such as V in S_int = 1/2 \[Integral] V(x, y) psibar(x) psibar(y) psi(y) psi(x): V[x, y] is drawn as a wavy line, and in the weight written with a picture it stands for -V (the minus sign comes from expanding exp(-S_int)). DeclareInteractionLine[V, ..., style] draws them in another style, e.g. \"DoubleWavy\" for a screened interaction W drawn beside V (in a skeleton expansion, where every line is a dressed one, the plain styles are used: the option \"Interactions\" -> {W} draws W as a wavy line in one picture). A line declared only with DeclareLineStyle is a propagator and stands for itself.";
DeclareCoupling::usage = "DeclareCoupling[lambda, ...] declares couplings of local interactions, such as lambda in S_int = lambda/4! \[Integral] phi^4 or g in g \[Integral] phi psibar psi. In the weight written with a picture they are absorbed into the vertices, each of which stands for minus its coupling: lambda^n in the coefficient of a term becomes (-1)^n, so that the tadpole -lambda/2 G(a, z) G(z, z) G(z, b) has the weight 1/2. The expressions are not changed.";
DeclareLocalVertex::usage = "DeclareLocalVertex[h, ...] declares heads of local vertices: kernels such as the quark-gluon vertex h[x, y, z] or the three-gluon vertex, whose labels carry different indices (Lorentz, colour, spinor) at one point of space-time. The pictures draw h[x, y, ...] as one point, at which its lines meet, instead of a blob; in the weight written with a picture the vertex stands for minus its kernel, -h (the sign of exp(-S_int)).";
DeclareDisplayName::usage = "DeclareDisplayName[h, name] displays h[x, y, ...] as name with the subscript x y ..., for example DeclareDisplayName[BareGreenFunction, \"G0\"]: input keeps the explicit name, output is compact.";
DeclareBlob::usage = "DeclareBlob[h, label] draws a factor h[x1, ..., xn] (n >= 2, a self-energy h[x, y] included) as a blob with label (a LaTeX string, e.g. \"\\\\Sigma\") written inside; its legs leave it counterclockwise from the top left, at the corners of a square for n = 4, and its lines leave from these corners, as from those of a triangle (a point that is twice an argument, h[z, z, w, w], stands outside two corners, joined to each); a box joined by single lines from three of its corners to one vertex (the four-point vertex in the Schwinger-Dyson equation of phi^4 theory) is turned by 45 degrees, the vertex on the diagonal through the fourth corner, the lines to the two other corners arcs symmetric about it, the label upright. Options: \"Shape\" -> \"Circle\" (default), \"Box\" or \"Triangle\" (equilateral, three legs at its corners, two on the left and one on the right: the vertex of a one-body operator), or \"TriangleUp\" (the same triangle with two legs at the bottom and one at the top), \"Diamond\" (a square turned by 45 degrees, legs at the left, bottom, right and top corners); \"AxialPorts\" -> {k1, ...}: the lines and legs of these ports leave horizontally, so that a self-energy can be drawn with its fermion line on an axis (the default for \"TriangleUp\" is {1, 2}); \"Fill\" -> \"Hatched\" (default), \"CrossHatched\", \"Dotted\" (dots on a staggered grid, sparser than the hatching), \"Grey\" or \"White\"; the documents fill a vertex function the more densely the more diagrams it contains (the fully irreducible vertex dotted, the irreducible parts hatched, the full vertex functions cross-hatched) and leave the correlation and response functions white. A blob is as large as its label needs: the label (on a hatched, cross-hatched or dotted blob in a white clearing, with a band of the fill left around it) sits at the centre, or near the incentre of a triangle, and a label with an accent, a subscript or a superscript makes the blob a little larger, never the label smaller; in one picture, and in all the terms of a sum drawn together, the blobs of one shape have one size. Blobs joined by lines are drawn side by side, as in the exact equations of many-body theory (Dyson, Schwinger-Dyson, Bethe-Salpeter, parquet).";
ResetDeclarations::usage = "ResetDeclarations[] removes all declarations (sources, fields, symmetries, derivative rules, line styles, interaction lines, couplings, local vertices, blobs, Matsubara frequencies): symmetric heads lose the attribute Orderless and antisymmetric ones their sign rules; display formats remain.";

(* ---------------------------------------------------------------- calculus *)
FunctionalDerivative::usage = "FunctionalDerivative[F, s1, s2, ..., sn] is the functional (left) derivative \[Delta]/\[Delta]s1 \[Delta]/\[Delta]s2 ... \[Delta]/\[Delta]sn F, the rightmost acting first. The si are sources, e.g. J[x] or eta[x], and must be declared. The result is in canonical form. FunctionalDerivative[F, {s1, ..., sn}] is the same. FunctionalDerivative[F, G[x, y]], with G a propagator or any other two-index kernel that is not a source (a self-energy, an interaction V, a background propagator), treats every G[a, b] as an independent variable (the derivative symmetrised for a symmetric G, DeclareSymmetric, and antisymmetrised, (\[Delta]F/\[Delta]G(x, y) - \[Delta]F/\[Delta]G(y, x))/2, for an antisymmetric one, DeclareAntisymmetric), as SelfEnergy does for the Luttinger-Ward functional: \[CapitalSigma](x, y) = \[Delta]Phi/\[Delta]G(y, x) and the irreducible vertex \[CapitalGamma]0(x1, x2; x3, x4) = \[Delta]^2 Phi/\[Delta]G(x4, x3) \[Delta]G(x2, x1) (IrreducibleVertex) (fermions) are such derivatives. F may contain TraceLog, OperatorTrace, OperatorKernel and LegendreTransform (\[Delta] Tr ln A = Tr(A^-1 \[Delta]A), \[Delta]G^-1 = -G^-1 \[Delta]G G^-1), and kernels whose dependence on a source is declared with DeclareFunctionalDerivative; an expression free of G gives 0 when G is a known kernel (a declared line, a symmetric head), the message nosource otherwise. A variation that is not known (a non-integer power of an operator) gives the message opvariation. FunctionalDerivative[F, s1, ..., sn, \"MaximumOrder\" -> {x, n}] is OrderPart[FunctionalDerivative[F, s1, ..., sn], x, Range[0, n]] for a coupling x or the head x of a line, with the terms of order above n dropped after each derivative; a logarithm of a sum polynomial in a coupling, such as Log[Z0 + Z1 + Z2/2], is first replaced by its series up to x^n (W = log Z order by order). When x is a source or a head with a declared derivative (the derivatives lower its order), the whole derivative is taken, then cut.";
SimplifyDiagrams::usage = "SimplifyDiagrams[expr] brings expr to canonical form: delta functions are integrated, integration variables are renamed canonically and equal diagrams are combined.";
DiagramTerms::usage = "DiagramTerms[expr] gives the list of the canonical terms (diagrams) of expr, each with its weight.";
SetSourcesToZero::usage = "SetSourcesToZero[expr] sets all declared sources to zero. It holds its argument: in SetSourcesToZero[FunctionalDerivative[F, s1, ..., sn]] the derivatives are taken one at a time, and after each the terms with more sources than derivatives left, which vanish at zero sources, are dropped; the result is the same as setting the sources to zero in the whole derivative.";
FieldDerivative::usage = "FieldDerivative[Z, {phi[x], psibar[y], ...}] applies to Z the source derivatives corresponding to the fields (see DeclareField), the rightmost first.";
FieldExpectationValue::usage = "FieldExpectationValue[Z, {fields...}] is the expectation value of the product of fields in the generating functional Z: the field derivatives of Z at zero sources divided by Z at zero sources. It is computed by literal differentiation.";
GaussianExpectationValue::usage = "GaussianExpectationValue[Z0, {fields...}] is the expectation value of a product of fields in a Gaussian generating functional Z0 (Wick's theorem). Option \"Method\" -> Automatic | \"WickPairings\" (sum over complete pairings, fast) | \"Differentiation\" (literal differentiation of Z0, which works for any Z0).";

(* ------------------------------------------------------ perturbation theory *)
PerturbativeCorrelationFunction::usage = "PerturbativeCorrelationFunction[Z0, {fields...}, interaction, n] is the expectation value <fields> = <fields exp(-interaction)>_0/<exp(-interaction)>_0 to order n in the interaction: all diagrams without vacuum bubbles. The interaction is written with the fields, e.g. lambda/24 IntegralOver[phi[z]^4, {z}]. With n = {k}, only order k; with {kmin, kmax}, orders kmin to kmax. Option \"Method\" -> Automatic | \"WickPairings\" | \"Differentiation\", as in GaussianExpectationValue; option \"CouplingOrder\" as in ConnectedCorrelationFunction.";
ConnectedCorrelationFunction::usage = "ConnectedCorrelationFunction[Z0, {fields...}, interaction, n] is the connected correlation function (cumulant) of the fields to order n: the diagrams in which all external points are connected, the derivatives of W = log Z with respect to the sources at zero source, computed order by order. Fields at the same point form a composite operator: the density-density function is ConnectedCorrelationFunction[Z0, {psibar[a], psi[a], psibar[b], psi[b]}, V, n], and psibar[a] ** psi[a] may stand for the two entries psibar[a], psi[a]; it is also -\[Delta]G(a, a)/\[Delta]U(b) for a potential U coupled to the density, whose derivative of the bare line is declared, DeclareFunctionalDerivative[G0[p_, q_], U[r_], G0[p, r] G0[r, q]]. With the option \"CouplingOrder\" -> {g, k} only the terms of order g^k are computed (the same as OrderPart[..., g, k], faster). Option \"Method\" as in PerturbativeCorrelationFunction.";
ConnectedVacuumDiagrams::usage = "ConnectedVacuumDiagrams[Z0, interaction, n] is log(Z/Z0) to order n in the interaction: the connected vacuum diagrams with their weights (linked-cluster theorem): the connected diagrams of Z/Z0 computed by derivatives (ConnectedDiagrams), directly. Options \"Method\" as in PerturbativeCorrelationFunction and \"CouplingOrder\" as in ConnectedCorrelationFunction.";
OrderPart::usage = "OrderPart[expr, x, k] keeps the terms of expr of order k: if x is a symbol (a coupling constant) the terms proportional to x^k; if x is the head of an interaction line, e.g. V, the terms with k factors V[...]. OrderPart[expr, x, {k1, k2, ...}] keeps the terms of the orders k1, k2, ....";

(* ---------------------------------------------------------- diagram classes *)
ConnectedDiagrams::usage = "ConnectedDiagrams[expr, {x1, ..., xn}] keeps the terms whose diagram is connected (the external labels x1, ..., xn and the integration variables are the points of the diagram).";
DropVacuumBubbles::usage = "DropVacuumBubbles[expr, {x1, ..., xn}] drops the terms containing vacuum bubbles, i.e. components without external points.";
ParticleIrreducibleQ::usage = "ParticleIrreducibleQ[term, n, {x1, ...}, {G, ...}] is True if the diagram of term stays connected after cutting any n or fewer lines with heads G, ... Interaction lines and vertices are never cut. External labels that end a single propagator are external legs (amputated before cutting); other external labels are vertices of the diagram.";
ParticleIrreducibleDiagrams::usage = "ParticleIrreducibleDiagrams[expr, n, {x1, ...}, {G, ...}] keeps the n-particle-irreducible terms of expr (see ParticleIrreducibleQ). With n = 1: one-particle irreducible (1PI) diagrams; with n = 2 and no external labels, ParticleIrreducibleDiagrams[vacuum, 2, {}, {G}]: the 2PI vacuum diagrams.";
SkeletonDiagrams::usage = "SkeletonDiagrams[expr, {x1, ..., xn}, {G, ...}] keeps the skeleton diagrams: one-particle irreducible and without self-energy insertions on the lines G. For a two-point function with external legs, these are the diagrams that close into 2PI vacuum diagrams; for amputated vertex functions, the diagrams in which no two lines G cut off a part without external labels.";
ClosedLoopCount::usage = "ClosedLoopCount[term, {G, ...}] is the number of closed loops made of the directed lines G (fermion loops) in the diagram of term.";
DropClosedLoops::usage = "DropClosedLoops[expr, {G, ...}] drops the terms with closed loops of the lines G, e.g. the fermion loops that cancel in a quenched average over disorder (replica limit).";
AmputateExternalLegs::usage = "AmputateExternalLegs[expr, {x1, ..., xn}, {G, ...}] removes the external legs: in every term, the propagator G that joins the external point xi to an internal vertex z is deleted and z is renamed xi (two external points ending on the same vertex give a DeltaFunction). Terms in which an external point is not joined to an internal vertex by a single propagator are dropped.";

(* ------------------------------------------------- functionals and vertices *)
LuttingerWardFunctional::usage = "LuttingerWardFunctional[Z0, interaction, n, G0 -> G] is the Luttinger-Ward functional to order n: minus the sum of the 2PI connected vacuum diagrams, with the bare propagator G0 replaced by the dressed one G. For bosons it is the 2PI part \[CapitalGamma]2 of the effective action. LuttingerWardFunctional[Z0, interaction, n, {G0 -> G, D0 -> W, ...}] makes several propagators full at once: the diagrams that are 2PI with respect to all of them (e.g. the functional of G and of the screened interaction W of a Hubbard-Stratonovich field). Option \"Method\" as in PerturbativeCorrelationFunction.";
SelfEnergy::usage = "SelfEnergy[Phi, G, {x, y}] is the self-energy \[CapitalSigma](x, y) obtained from the Luttinger-Ward functional Phi: \[Delta]Phi/\[Delta]G(y, x) for fermions (option \"Statistics\" -> \"Fermion\", the default, with G = -<psi psibar>), -2 \[Delta]Phi/\[Delta]G(x, y) for real bosons (\"Statistics\" -> \"Boson\", with G = <phi phi> symmetric), -\[Delta]Phi/\[Delta]G(y, x) for complex bosons (\"Statistics\" -> \"ComplexBoson\", with G = <phi phibar>) and -2 \[Delta]Phi/\[Delta]G(x, y), the derivative antisymmetrised, for a superfield (\"Statistics\" -> \"Superfield\": a Grassmann field with an antisymmetric propagator G(a, b) = -<T NambuField_a NambuField_b>, e.g. the Nambu field NambuField = (psi, psibar), whose self-energy has the normal block \[CapitalSigma] and the anomalous ones, the gap functions). In all cases G = G0 + G0 \[CapitalSigma] G. It is a shortcut for the derivative: for fermions SelfEnergy[Phi, G, {x, y}] = FunctionalDerivative[Phi, G[y, x]], for real bosons -2 FunctionalDerivative[Phi, G[x, y]] (G symmetric: the derivative is symmetrised). On a symmetric or antisymmetric G without \"Statistics\" the fermionic default gives the message statisticsdefault.";
IrreducibleVertex::usage = "IrreducibleVertex[Phi, G, {x1, x2, x3, x4}] is the vertex irreducible in the particle-hole channel (x1 x2|x3 x4), the kernel of the Bethe-Salpeter equation: \[CapitalGamma]0(x1, x2; x3, x4) = \[Delta]\[CapitalSigma](x1, x2)/\[Delta]G(x4, x3) = \[Delta]^2 Phi/\[Delta]G(x4, x3) \[Delta]G(x2, x1) for fermions (fermions leave at x1, x3 and enter at x2, x4), -2 \[Delta]\[CapitalSigma](x1, x2)/\[Delta]G(x3, x4) for real bosons, \[Delta]\[CapitalSigma](x1, x2)/\[Delta]G(x4, x3) for complex bosons (\"Statistics\" -> \"ComplexBoson\"), and -2 \[Delta]\[CapitalSigma](a, b)/\[Delta]G(c, d) = 4 \[Delta]^2 Phi/\[Delta]G(a, b) \[Delta]G(c, d), antisymmetrised, for a superfield (\"Statistics\" -> \"Superfield\"), the kernel of the channel s (a b|c d). Option \"Channel\": the kernel irreducible in another channel, from the same functional: for real bosons and superfields \"s\" (the default), \"t\" (a c|b d), \"u\" (a d|b c) or All (an association), \[CapitalGamma]_t(a, b, c, d) = \[PlusMinus]\[CapitalGamma]_s(a, c, b, d) and \[CapitalGamma]_u(a, b, c, d) = \[CapitalGamma]_s(a, d, b, c), with the sign + for real bosons and - for superfields (the sign of the permutation); each is F - R_r (ParquetDecomposition). For fermions \"ParticleHole\" (the default) and \"TransverseParticleHole\", -\[CapitalGamma]0(x3, x2; x1, x4); the particle-particle kernel is a derivative with respect to the anomalous propagators -<T psi psi>, -<T psibar psibar>: it is the channel t of the superfield (psi, psibar) projected with SuperfieldComponents, and \"ParticleParticle\" gives the message ppkernel. As a derivative of the components of the superfield Phi[G, Gpsipsi, Gpsibarpsibar] (SuperfieldComponents), \[CapitalGamma]_pp(x1, x2; x3, x4) = -4 \[Delta]^2 Phi/\[Delta]Gpsipsi(x2, x4) \[Delta]Gpsibarpsibar(x1, x3) at zero anomalous propagators, i.e. -4 FunctionalDerivative[Phi, Gpsipsi[x2, x4], Gpsibarpsibar[x1, x3]] with Gpsipsi, Gpsibarpsibar then set to 0. IrreducibleVertex is a shortcut for the derivatives: for fermions IrreducibleVertex[Phi, G, {x1, x2, x3, x4}] = FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]], for real bosons 4 FunctionalDerivative[Phi, G[x3, x4], G[x1, x2]].";
PerturbativeFullVertex::usage = "PerturbativeFullVertex[Z0, interaction, n, fields, {x1, x2, x3, x4}, G0 -> G] is the full (amputated, one-particle irreducible) four-point vertex F to order n, as a skeleton expansion in the dressed propagator G. For fermions, fields = {psi, psibar} and <psi(x1') psibar(x2') psibar(x4') psi(x3')>_c = G(x1', x1) G(x2, x2') F(x1, x2; x3, x4) G(x4, x4') G(x3', x3); for real bosons, fields = {phi} and F is the 1PI vertex \[CapitalGamma]^(4) = -(amputated connected four-point function), and so for a superfield (one Grassmann field with an antisymmetric propagator, fields = {NambuField}). The order n can be n, {k} or {kmin, kmax}; option \"Method\" as in PerturbativeCorrelationFunction. Several bare propagators made full at once are a list of rules, {G0up -> Gup, G0down -> Gdown}. It is ConnectedCorrelationFunction[Z0, {psi[x1], psibar[x2], psibar[x4], psi[x3]}, interaction, n] amputated (AmputateExternalLegs) with its skeleton diagrams kept (SkeletonDiagrams) and G0 -> G; with spin, fields that carry the index of their sources (psi[x1, up], ...) are written in this way, the fields psi[x] without it giving the message fieldarity. With bare lines, F is ParticleIrreducibleDiagrams[AmputateExternalLegs[...], 1, ...] of the same function. The vertex is the fourth derivative of the 1PI action at zero means (EffectiveAction1PI with bare lines): the result is SkeletonDiagrams[SetSourcesToZero[FunctionalDerivative[Gamma, phiMean[x1], phiMean[x2], phiMean[x3], phiMean[x4]]], {x1, x2, x3, x4}, {G0}] /. G0 -> G, for fermions with the derivatives Psibar[x1], Psi[x2], Psibar[x3], Psi[x4]; this function computes the same from the connected function.";
SuperfieldComponents::usage = "SuperfieldComponents[expr, externals, rules] gives the components of an expression of a superfield, a Grassmann field NambuField_a whose label a = (x, s) stands for a point and a component (the Nambu field NambuField = (psi, psibar), with the antisymmetric propagator \[ScriptCapitalG](a, b) = -<T NambuField_a NambuField_b>). externals: {label -> component[new label], ...} for every free label ({} for none). rules: field -> {psi, psibar} (required: the superfield and its components, in the order of the component index; e.g. {psiUp, psiDn, psibarUp, psibarDn} with spin); mean -> {Psi, Psibar}; a propagator h -> {G, Gpsipsi, Gpsibarpsibar}, h = [[Gpsipsi, G], [-G^T, Gpsibarpsibar]] with G = -<T psi psibar> and the anomalous propagators Gpsipsi = -<T psi psi>, Gpsibarpsibar = -<T psibar psibar> (heads or 0); any two-label kernel as a matrix h -> {{h11, h12}, {h21, h22}}, h(x s, y t) = h_st(x, y), with entries heads, 0 or functions of two labels (the self-energy {{GapFunctionBar, -Sigma[#2, #1] &}, {Sigma, GapFunction}}: \[CapitalSigma](x psibar, y psi) = \[CapitalSigma](x, y), the gap function \[CapitalDelta](x, y) = \[CapitalSigma](x psibar, y psibar)); a vertex h -> S, its components the derivatives h(a1, ..., ak) = \[Delta]/\[Delta]chi_ak ... \[Delta]/\[Delta]chi_a1 S of the component interaction S. Traces and kernels of operators made of heads without anomalous blocks are written with the normal blocks (Tr ln(-\[ScriptCapitalG]^-1) = 2 Tr ln(-G^-1), Tr(\[ScriptCapitalG]0^-1 . \[ScriptCapitalG] - 1) = 2 Tr(G0^-1 . G - 1)); with anomalous blocks a trace of products is written as rings (ExpandTrace), while Tr ln of an operator with anomalous blocks has no component form: it is kept as it is, with the message trace, beside the components of the other terms (it does not depend on the means; a derivative of it with respect to a component, G or an anomalous block, gives the message supertrace: take the derivative of the superfield functional with respect to the Nambu propagator first, then its components at zero anomalous blocks). The rule of a mean is mean -> {Psi, Psibar}, e.g. {NambuField -> {psi, psibar}, X -> {Psi, Psibar}, NambuPropagator -> {G, Gpsipsi, Gpsibarpsibar}, BareNambuPropagator -> {G0, 0, 0}, NambuVertex -> S}. The order of the labels of a derivative is the order of the legs: with legs a -> psibar[x1], b -> psi[x2], c -> psibar[x3], d -> psi[x4], FunctionalDerivative[Gamma, X[a], X[b], X[c], X[d]] projects onto FunctionalDerivative[Gamma, Psibar[x1], Psi[x2], Psibar[x3], Psi[x4]], and an odd permutation of the four Grassmann derivatives changes the sign. The result is in canonical form.";

(* ------------------------------------------------- channels and the parquet *)
TwoParticleReducibleQ::usage = "TwoParticleReducibleQ[term, {{x1, x2}, {x3, x4}}, {G, ...}] is True if cutting two or fewer lines G separates the external labels x1, x2 from x3, x4.";
ReducibleDiagrams::usage = "ReducibleDiagrams[expr, {{x1, ...}, {y1, ...}}, n, {G, ...}] keeps the terms in which cutting n or fewer lines G separates the external labels x1, ... from y1, ... For instance ReducibleDiagrams[chi, {{a}, {b}}, 1, {V}] keeps the diagrams of a response function that fall apart when a single interaction line is cut (the RPA-reducible ones).";
IrreducibleDiagrams::usage = "IrreducibleDiagrams[expr, {{x1, ...}, {y1, ...}}, n, {G, ...}] keeps the terms in which no cut of n or fewer lines G separates the external labels x1, ... from y1, ..., e.g. the proper polarisation IrreducibleDiagrams[chi, {{a}, {b}}, 1, {V}].";
ParquetDecomposition::usage = "ParquetDecomposition[F, {x1, x2, x3, x4}, {G, ...}] classifies the diagrams of a four-point vertex by two-particle reducibility and returns an association with the fully irreducible part \"FullyIrreducible\" and the reducible parts of the three channels: for fermions (\"Statistics\" -> \"Fermion\", legs x1, x3 outgoing and x2, x4 incoming) \"ParticleHole\" (x1 x2|x3 x4), \"TransverseParticleHole\" (x1 x4|x3 x2) and \"ParticleParticle\" (x1 x3|x2 x4); for real bosons and superfields (\"Statistics\" -> \"Boson\" or \"Superfield\") \"s\" (x1 x2|x3 x4), \"t\" (x1 x3|x2 x4) and \"u\" (x1 x4|x2 x3). Diagrams reducible in more than one channel, which the parquet theorem excludes, are collected under \"SeveralChannels\". The parts found by cutting lines are derivatives, which the classification checks: \"FullyIrreducible\" is the fourth derivative of the 2PI action with respect to the means at fixed G (EffectiveAction2PI with \"MeanFields\"), and F minus the part reducible in a channel is the kernel of that channel, a second derivative of Phi (IrreducibleVertex).";
BetheSalpeterLadder::usage = "BetheSalpeterLadder[L, R, {x1, x2, x3, x4}, channel, G] is the contraction L \[SmallCircle] R of two four-point vertices with a pair of propagators G in the given channel, integrated over the internal points 5, 6, 7, 8. Fermions (legs x1, x3 outgoing, x2, x4 incoming; G(a, b) runs from b to a): \"ParticleHole\": L(x1, x2; 5, 6) G(6, 7) G(8, 5) R(7, 8; x3, x4); \"TransverseParticleHole\": -L(x1, 6; 5, x4) G(6, 7) G(8, 5) R(7, x2; x3, 8); \"ParticleParticle\": -1/2 L(x1, 5; x3, 6) G(5, 7) G(6, 8) R(7, x2; 8, x4). Real bosons (\"Statistics\" -> \"Boson\"): \"s\", \"t\", \"u\", e.g. -1/2 L(x1, x2, 5, 6) G(5, 7) G(6, 8) R(7, 8, x3, x4); superfields (\"Statistics\" -> \"Superfield\", G antisymmetric): the same with the signs +1/2, -1/2, +1/2 in s, t, u. With these conventions every channel obeys R_r = \[CapitalGamma]_r \[SmallCircle] F (Bethe-Salpeter) and F = \[CapitalGamma]irr + Sum_r R_r (parquet, \[CapitalGamma]irr the fully irreducible vertex). Option \"MaximumOrder\" -> {V, n} keeps only the products of order n or less.";
CheckParquetEquations::usage = "CheckParquetEquations[F, {x1, x2, x3, x4}, G, coupling, n] checks, order by order up to n, that every diagram of the vertex F is reducible in at most one channel and that the reducible part of each channel equals the Bethe-Salpeter ladder \[CapitalGamma]_r \[SmallCircle] F, with \[CapitalGamma]_r = F - R_r. It returns an association with the residuals (all 0 if the equations hold) and the number of diagrams per order and channel. Option \"Statistics\": \"Fermion\" (the default), \"Boson\" or \"Superfield\", for the channels and the ladders of BetheSalpeterLadder.";
PerturbativeDensityVertex::usage = "PerturbativeDensityVertex[Z0, V, n, {psi, psibar}, {x1, x2, z}, G0 -> G] is the vertex of the density at z, \[CapitalLambda](x1, x2; z) = -\[Delta]G^-1(x1, x2)/\[Delta]U(z) for a potential U coupled to \[Rho] = psibar psi: <psi(x1) psibar(x2) \[Rho](z)>_c = L0 \[CapitalLambda], with L0 the pair of propagators G(x1, a) G(b, x2) (integrated over a, b). It is \[Delta](x1, z) \[Delta](x2, z) plus the skeleton diagrams to order n, with the bare propagator G0 replaced by G. For a real boson field, PerturbativeDensityVertex[Z0, V, n, {phi}, {x1, x2, z}, D0 -> G] is the vertex of \[Rho] = phi^2/2 (a local mass term). It is the derivative \[CapitalLambda] = \[Delta](x1, z) \[Delta](x2, z) + \[Delta]\[CapitalSigma](x1, x2)/\[Delta]U(z) of the perturbative self-energy through its bare lines (DeclareFunctionalDerivative[G0[p_, q_], U[r_], G0[p, r] G0[r, q]]; for a real boson \[Delta](x1, z) \[Delta](x2, z) - \[Delta]\[CapitalSigma]/\[Delta]U with \[Delta]D0(p, q)/\[Delta]U(r) = -D0(p, r) D0(r, q)), its skeleton diagrams with G0 -> G; this function computes the same from <psi psibar \[Rho]>_c. Option \"Method\" as in PerturbativeCorrelationFunction.";
LabelFunction::usage = "LabelFunction[e, {x1, ..., xn}] is the function of n labels that gives e with x1, ..., xn replaced by its arguments: a perturbative series used as an object of ExactEquation.";
ExactEquation::usage = "ExactEquation[name, {\"G\" -> G, \"Sigma\" -> S, ...}, labels] is an exact equation between full objects, lhs == rhs. The objects are heads (e.g. blobs declared by DeclareBlob, to draw the equation) or functions of their labels (LabelFunction of perturbative series, to check it with CheckEquation). Fermions leave a four-point vertex at x1, x3 and enter at x2, x4. Names and objects: \"Dyson\" (G, G0, Sigma; labels {x, y}): G = G0 + G0 \[CapitalSigma] G. \"BetheSalpeter\" (F, Gamma0, G; {x1, x2, x3, x4}; option \"Channel\"): F = \[CapitalGamma]0 + \[CapitalGamma]0 o F. \"Parquet\" (F, Lambda, GammaPh, GammaPhbar, GammaPp, G; for bosons Gammas, Gammat, Gammau): F = \[CapitalGamma]irr + Sum_r \[CapitalGamma]_r o F, with the fully irreducible vertex \[CapitalGamma]irr (the object Lambda). \"SchwingerDyson\" (Sigma, G, F, V; {x, y}): \[CapitalSigma] = Hartree + Fock - V G L0 F for a density-density interaction V; for real bosons (Sigma, G, F, Coupling) \[CapitalSigma] = -\[Lambda]/2 G(x, x) + \[Lambda]/6 G L0 F in \[Phi]^4 theory. \"Vertex\" (Lambda, Gamma0, G; {x1, x2, z}): \[CapitalLambda] = \[Gamma] + \[CapitalGamma]0 L0 \[CapitalLambda] for the vertex of the density (L0 the pair of propagators of the channel), \[Gamma] = \[Delta](x1, z) \[Delta](x2, z). \"VertexFromF\" (Lambda, F, G): \[CapitalLambda] = \[Gamma] + F L0 \[Gamma]. \"SelfEnergyFromVertex\" (Sigma, G, V, Lambda; {x, y}): \[CapitalSigma] = Hartree - V G \[CapitalLambda]. \"Hedin\" (Sigma, G, W, P, Lambda, GammaXC, V; {x, y, z}): Hedin's equations \[CapitalSigma] = Hartree - G W \[CapitalLambda], P = L0 \[CapitalLambda], W = V + V P W, \[CapitalLambda] = \[Gamma] + \[CapitalGamma]xc L0 \[CapitalLambda], with the proper vertex \[CapitalLambda] and \[CapitalGamma]xc = \[Delta]\[CapitalSigma]xc/\[Delta]G. Option \"Statistics\" -> \"Boson\" for real bosons (BetheSalpeter, Parquet, SchwingerDyson, Vertex, VertexFromF). Option \"MaximumOrder\" -> {x, n}: both sides only up to order n in x, enough for CheckEquation to order n (the products of two series are not brought into canonical form beyond it). \"Response\" (L, Gamma0, G; {x1, x2, x3, x4}): L = L0 + L0 \[CapitalGamma]0 L, the Bethe-Salpeter equation of the response of G to a one-body source, with L0 the pair of propagators; fermions (the legs of a four-point vertex: leaving at x1, x3, entering at x2, x4) L(x1, x2; x3, x4) = \[Delta]G(x1, x2)/\[Delta]U(x4, x3) = -<psi(x1) psibar(x2) psi(x3) psibar(x4)>_c + G(x1, x4) G(x3, x2), L = G(x1, x4) G(x3, x2) + G(x1, a) G(b, x2) \[CapitalGamma]0(a, b; c, d) L(d, c; x3, x4); real bosons L = -2 \[Delta]G/\[Delta]U = <phi phi phi phi>_c + G13 G24 + G14 G23, L(x1, x2; x3, x4) = G(x1, x3) G(x2, x4) + G(x1, x4) G(x2, x3) - 1/2 G(x1, a) G(x2, b) \[CapitalGamma]0(a, b; c, d) L(c, d; x3, x4). \"Susceptibility\" (Chi, L, optionally BareVertex; {z1, z2}): \[Chi] = \[Gamma] L \[Gamma], the response \[Delta]<O(z1)>/\[Delta]u(z2) = \[Delta]^2 \[CapitalOmega]/\[Delta]u \[Delta]u = -<O O>_c of O(z) = \[Integral] psibar(a) \[Gamma](a, b; z) psi(b) (1/2 phi \[Gamma] phi for real bosons, with a factor -1/4), the density when no \"BareVertex\" -> gamma (a function of three labels: current, spin, ...) is given. \"Vertex\" and \"VertexFromF\" also take \"BareVertex\" -> gamma, for the vertex of a general one-body operator.";
CheckEquation::usage = "CheckEquation[lhs == rhs, x, n] is the list of the residuals SimplifyDiagrams[lhs - rhs] of orders 0, ..., n in x: all 0 if the equation holds to order n. CheckEquation[{eq1, eq2, ...}, x, n] checks several equations.";
HubbardStratonovich::usage = "HubbardStratonovich[{Z0, interaction}, O, {phi, J}, D] decouples an interaction 1/2 Integral K(x, y) O(x) O(y), with O a local operator given as a function of one label (psibar[#] ** psi[#] &, or phi[#]^2 &), by a real auxiliary field phi with source J: <phi phi>_0 = K and the coupling -i Integral phi O, since <exp(i Integral phi O)> = exp(-1/2 Integral O K O). The kernel K is read off the interaction (two-label terms, or local terms like lambda/4! phi^4). D is the head of the propagator of phi, standing for K; with D = Automatic and K = c h[x, y], the head h itself is the propagator and Sqrt[c] goes into the coupling. The result is an association: \"FreeGeneratingFunctional\" (Z0 times the Gaussian of phi), \"Interaction\", \"Propagator\" and \"Kernel\" (K as a function of two labels, to substitute D back). The field and the source are declared, and the propagator of the field declared symmetric and, if it has no line style yet, an interaction line.";
IntegrateOutField::usage = "IntegrateOutField[{Z0, interaction}, phi, J] integrates out a real Gaussian field phi with source J, <phi phi>_0 = D (read off Z0), that enters the interaction linearly, Integral phi(z) A(z): since <exp(-Integral phi A)> = exp(1/2 Integral A D A), the interaction becomes the other terms - 1/2 Integral A(x) D(x, y) A(y). The result is an association with the \"FreeGeneratingFunctional\" without phi and the \"Interaction\". The converse of HubbardStratonovich; for phonons it gives the effective retarded interaction of the electrons.";
DysonSeries::usage = "DysonSeries[sigma, {s1, s2}, G0, {x, y}, m] is G0(x, y) + G0 \[CapitalSigma] G0 + G0 \[CapitalSigma] G0 \[CapitalSigma] G0 + ... with up to m self-energy insertions, where sigma is an expression for \[CapitalSigma](s1, s2). With the option \"MaximumOrder\" -> {V, n}, only the chains of total order n or less in V (a coupling symbol or the head of the interaction lines) are built.";

(* --------------------------------------------------------- tree expansion *)
ConnectedFunctionFromVertices::usage = "ConnectedFunctionFromVertices[{x1, ..., xn}, \"Boson\" | \"Fermion\"] gives the connected n-point function at zero source as a sum of trees of full propagators FullPropagator and one-particle-irreducible vertices VertexFunction (the tree expansion), obtained by differentiating the full propagator with respect to the source ExternalSource with the rules that follow from the Legendre transform. ConnectedFunctionFromVertices[n, type] uses the labels x1, ..., xn; the type is \"Boson\" by default. \"Fermion\" gives the trees of a superfield (psi, psibar), with antisymmetric propagator and vertices; \"FermionLines\" gives them with fermion lines: <psi(x1) psibar(x2) psi(x3) psibar(x4) ...>_c as trees of directed propagators FullPropagator[x, y] = G(x, y) = -<psi(x) psibar(y)>_c and vertices VertexFunction[outs..., ins...], antisymmetric within the outgoing and within the incoming legs; ConnectedFunctionFromVertices[labels, \"FermionLines\", {\"Out\" | \"In\", ...}] chooses psi (\"Out\") or psibar (\"In\") for each label.";
FullPropagator::usage = "FullPropagator[x, y] is the full (dressed) propagator G, the connected two-point function at the current source, used by ConnectedFunctionFromVertices.";
VertexFunction::usage = "VertexFunction[x1, ..., xn] is the one-particle-irreducible vertex \[CapitalGamma]^(n), used by ConnectedFunctionFromVertices; the pictures draw it as a cross-hatched disc.";
MeanField::usage = "MeanField[x] is the mean field <phi(x)> at the current source, displayed as phi with a bar (the expectation value of the field, the exact one-point function, not a mean-field approximation), used by ConnectedFunctionFromVertices.";
ExternalSource::usage = "ExternalSource[x] is the source J(x) with respect to which ConnectedFunctionFromVertices differentiates.";

(* ----------------------------------------------------------------- pictures *)
DrawDiagram::usage = "DrawDiagram[term, {x1, ..., xn}, options] draws one term as a Feynman diagram. Propagators are lines, interaction lines are wavy (see DeclareLineStyle), factors with three or more labels are blobs (one not declared with DeclareBlob is a small disc without a name, hatched as the default of DeclareBlob, and a vertex function VertexFunction a cross-hatched one, as the full vertex functions of the documents), factors with one label are source insertions. External points of correlation functions are open circles; in amputated vertex functions (self-energies, vertices) the external labels sit on vertices and are drawn as external legs; labels joined by a DeltaFunction are the same point. Options as for DiagramPlot.";
DiagramPlot::usage = "DiagramPlot[expr, {x1, ..., xn}, options] draws every canonical term of expr with its weight: the number written in front of a picture, times the values of its elements, is the term. A propagator stands for itself, an interaction line (DeclareInteractionLine) for minus its kernel, a local vertex (DeclareLocalVertex) for minus its kernel, and a vertex for minus its coupling (DeclareCoupling): the minus signs come from expanding exp(-S_int). An anomalous line (\"DivergingArrows\", \"ConvergingArrows\") or an antisymmetric blob of two labels shows no orientation: the weight takes it read along its fermion line, around a loop in either sense, along an open line from its end at the first external label. A kernel OperatorKernel[D^-1] is drawn as a line: in a term with lines D (a derivative of a trace before ExpandTrace) it gives the message inversekernel. Options: \"VertexLabels\" -> Automatic (external labels) | All (also z1, z2, ...) | None; \"LegDirections\" -> Automatic | \"Alternating\" | {\"Out\", \"In\", ...} (arrows on the external legs of fermionic vertex functions); \"AmputatedLegs\" -> Automatic | True | False; \"InteractionStyle\" -> Automatic | \"Wavy\" | \"DoubleWavy\" | \"Curly\" | \"Dashed\" | \"Dotted\" | \"Zigzag\" (the style of all the interaction lines); \"Directed\" -> {G, ...} and \"Interactions\" -> {V, ...} (heads drawn as arrows or as interaction lines in this picture only); \"Couplings\" -> {g, ...} (couplings absorbed into the vertices in this picture only, as with DeclareCoupling); \"ExternalOrder\" -> Automatic | {x1, ...} | \"Uncrossed\" | {\"Uncrossed\", {x1, ...}} (four external labels are at the corners counterclockwise from the top left, in the order given to the function or in the order of the list, and a diagram whose channel joins two opposite corners is drawn with two legs crossing as an X; with \"Uncrossed\" each diagram has instead the legs of one side exchanged where that keeps its lines from crossing, the labels moving with their legs); \"Columns\" -> n; \"Weights\" -> True | False | \"Coefficients\" (the coefficients of the terms, every line and vertex standing for its kernel); \"LineWidth\" -> width of the lines in points; \"Output\" -> \"Grid\" (a grid of weights and pictures); ImageSize -> width of the picture in pixels (Automatic: 46 pixels per diagram unit); \"Appearance\" -> Automatic (dark lines and text on a light notebook, light ones on a dark notebook, through LightDarkSwitched of Mathematica 14.3 or later; before 14.3 the light colours) | \"Light\" | \"Dark\" (forced, on a background of the paper colour: white, or the dark grey of a dark notebook; SetOptions[{DiagramPlot, DrawDiagram}, \"Appearance\" -> \"Dark\"] for a session). DiagramToTikZ ignores it: the TikZ code is for white pages.";
DiagramToTikZ::usage = "DiagramToTikZ[expr, {x1, ..., xn}, options] gives LaTeX/TikZ code for the diagrams of expr: a sum of tikzpictures with their weights, ready for a paper (it needs the TikZ library decorations.pathmorphing). Same options as DiagramPlot, plus \"Scale\" -> length of one diagram unit in cm (0.7), \"LineWidth\" -> width of the lines in pt (0.7; arrow heads, dashes and double lines follow), \"LabelFont\" -> LaTeX font command of the labels (\"\\\\small\"), \"Labels\" -> rules from labels to LaTeX strings, and \"Output\" -> \"List\" for a list of {weight, picture} strings.";
ExpressionToLaTeX::usage = "ExpressionToLaTeX[expr, options] gives the LaTeX code of a formula: integrals with their variables as a subscript, functions with their arguments in parentheses, delta functions, Grassmann products in their order. Options: \"Names\" -> {h -> \"G_0\", ...}, the LaTeX of heads and symbols (by default the name given with DeclareDisplayName, else the symbol name, with greek letters, trailing digits and a final bar recognised) and of subexpressions (FermiFunction[xi[p1]] -> \"n_1\"); \"Labels\" -> {x -> \"x_1\", ...}, the LaTeX of labels; \"Subscripts\" -> {xi, v, ...}, heads whose arguments are written as a subscript, xi[p] as \\xi_{p} (dispersions and potentials in momentum space).";

(* ------------------------------------------------ evaluation: Matsubara sums *)
FermiFunction::usage = "FermiFunction[e] is the Fermi function n_F(e) = 1/(e^(e/T) + 1) (inert; n_F(0) = 1/2). Its derivatives Derivative[k][FermiFunction][e] come from poles of higher order (self-energy insertions on a line).";
BoseFunction::usage = "BoseFunction[e] is the Bose function n_B(e) = 1/(e^(e/T) - 1) (inert).";
EnergyDelta::usage = "EnergyDelta[x] is the inert delta function \[Delta](x) of an energy; Derivative[k][EnergyDelta][x] are its derivatives.";
DeclareMatsubaraFrequency::usage = "DeclareMatsubaraFrequency[w, \"Fermion\"] or DeclareMatsubaraFrequency[{w1, w2, ...}, \"Boson\"] declares that the symbols are Matsubara frequencies of that statistics (\[Omega]_n = (2n + 1) \[Pi] T, \[Nu]_n = 2n \[Pi] T), written I w in expressions. Statistical factors at energies shifted by them are simplified by periodicity: n_F(e + I \[Nu]) = n_F(e), n_F(e + I \[Omega]) = -n_B(e), n_B(e + I \[Omega]) = -n_F(e).";
MatsubaraSum::usage = "MatsubaraSum[f, z, \"Fermion\"] is T Sum_n f(z = I \[Omega]_n) over the fermionic Matsubara frequencies, MatsubaraSum[f, z, \"Boson\"] the sum over the bosonic ones, for f rational in z with a denominator that factors into linear and quadratic factors (propagators), computed by residues: the sum over the poles of f of Res[f n_F] (fermions) or -Res[f n_B] (bosons); poles of order k give the derivatives of order k - 1 of n_F, n_B. Option \"ConvergenceFactor\" -> 1 (a factor e^(I \[Omega]_n 0+), as for a density), -1 (e^(-I \[Omega]_n 0+)) or 0 (symmetric sum, no factor): it matters only when f decays like 1/z, and with the default Automatic a message is given in that case and the symmetric sum is taken. For f = 1/(z - a) the three give n_F(a), n_F(a) - 1 and n_F(a) - 1/2 (fermions), -n_B(a), -n_B(a) - 1 and -n_B(a) - 1/2 (bosons). A summand that does not decay (the sum diverges) or that is not split into poles gives a message and $Failed. A summand may also contain logarithms, c Log[r] with r rational in z (the sums of Tr ln): T Sum_n e^(I \[Eta] \[Omega]_n 0+) ln(a - I \[Omega]_n) = -Temperature Log[1 - FermiFunction[a]] + (1 - \[Eta])/2 a for fermions, -Temperature Log[1 + BoseFunction[a]] + (1 - \[Eta])/2 a for bosons (\[Eta] = 1, -1, 0 the three convergence factors; -T ln(1 - n_F(a)) = T ln(1 + e^(-a/T))), each zero of r counting +1 and each pole -1, a constant factor nothing (the trace of the identity is zero); the factors of a real boson, ln(W^2 - z^2), give W - 2 Temperature Log[1 + BoseFunction[W]] with every ordering, the zero-point energy included.";
LoopMomentum::usage = "LoopMomentum[i] is the momentum of the i-th loop of a diagram evaluated by EvaluateMatsubara or DiagramLoopIntegral (unless the option \"LoopMomenta\" -> {p1, p2, ...} gives names); ExpressionToLaTeX writes it p_i, and \[ScriptL]_i in QED (in a MinkowskiLoopIntegral, and in Dirac and Lorentz objects outside one).";
LoopFrequency::usage = "LoopFrequency[i, statistics] is the Matsubara frequency of the i-th loop of a diagram (DiagramIntegrand), written I LoopFrequency[i, statistics] in the integrand.";
MomentumSum::usage = "MomentumSum[f, {p1, p2, ...}] is the inert integral over the loop momenta, \[Integral] d^dp1/(2\[Pi])^d ... f (or (1/V) Sum_p f).";
DiagramIntegrand::usage = "DiagramIntegrand[diagrams, {x1 -> {k1, I w1}, ...}, rules, options] routes momenta and Matsubara frequencies through each term of diagrams as EvaluateMatsubara does and gives, for each term, {\"Weight\" -> ..., \"Integrand\" -> ..., \"LoopMomenta\" -> ..., \"LoopFrequencies\" -> ..., \"EqualTimeFrequency\" -> ...}: the integrand before the frequency sums, a function of I LoopFrequency[i, statistics]; \"EqualTimeFrequency\" is the sum of the frequencies of the fermion lines at equal times, the exponent \[CapitalOmega] of their convergence factor e^(I \[CapitalOmega] 0+) for the default \"ConvergenceFactor\" -> 1 of EvaluateMatsubara (e^(-I \[CapitalOmega] 0+) for -1, none for 0) (0 when there is none). Options as in EvaluateMatsubara: \"SpinDegeneracy\" (in the weight) and \"LoopMomenta\".";
EvaluateMatsubara::usage = "EvaluateMatsubara[diagrams, {x1 -> {k1, I w1}, x2 -> {k2, I w2}, ...}, rules] evaluates diagrams of the package (functions of labels: position, imaginary time, spin) in momentum and Matsubara frequency, for a translation-invariant system: ki and I wi enter the diagram at the external label xi (they add up to zero; the statistics of wi is declared by DeclareMatsubaraFrequency). The rules give the lines: head -> {\"Fermion\", xi} (G0(p, I \[Omega]) = 1/(I \[Omega] - xi[p]) for a line head[x, y] from y to x), head -> {\"Instantaneous\", v} (an even interaction v[q], instantaneous) or head -> {\"Boson\", Function[{q, z}, d]} (an even boson propagator, a rational function of z = I \[Nu], e.g. 2 \[CapitalOmega][q]/(\[CapitalOmega][q]^2 - z^2) for the <\[Phi] \[Phi]> of the package). Each loop gives T Sum_n \[Integral] d^dp/(2\[Pi])^d: the frequency sums are done by MatsubaraSum, with the convergence factor e^(I \[Omega] 0+) of the lines at equal times (option \"ConvergenceFactor\"), and the momentum integrals are left as MomentumSum. A function of one external frequency is written in partial fractions of it (the denominators of the textbooks), separately in the parts of a term between the lines that carry that frequency alone (the external legs of a function that is not amputated, the lines between its irreducible parts), which stay as factors. Options: \"SpinDegeneracy\" -> 1 (a factor (2s + 1) for each closed fermion loop), \"LoopMomenta\" -> Automatic (LoopMomentum[i]) or a list of names, \"ConvergenceFactor\" -> 1 (the ordering of a fermion line at equal times, G[x, y] with x and y at the same time: 1 for e^(I \[Omega] 0+), G(\[Tau] = 0-) = <\[Psi]^\[Dagger] \[Psi]>, n_F(xi) for a line closed on itself; -1 for e^(-I \[Omega] 0+), G(0+) = -<\[Psi] \[Psi]^\[Dagger]>, n_F(xi) - 1; 0 for no factor, the symmetric sum, the mean (1/2) <[\[Psi]^\[Dagger], \[Psi]]>, n_F(xi) - 1/2, natural on a lattice with a particle-hole symmetric interaction, where the difference is a shift of the chemical potential, but divergent in the continuum). Invalid arguments, or a term that cannot be evaluated, give a message and $Failed. Traces of operators are evaluated too (with no external point): TraceLog[A] and OperatorTrace[A], with A built from kernels that have line rules (\"Fermion\": 1/(z - xi[k]); \"Instantaneous\": v[k], e.g. a static self-energy; \"Boson\": f[k, z]), give (2s + 1) MomentumSum[T Sum_n e^(I \[Eta] \[Omega]_n 0+) ln A(k, I \[Omega]_n)] and the same with A (its constant part dropped: Tr 1 = 0), per unit of \[Beta] times the volume, as the vacuum diagrams: with EffectiveAction2PI this is the grand potential \[CapitalOmega]/V, e.g. -Tr ln(-G0^-1) = 2 Temperature MomentumSum[Log[1 - FermiFunction[xi[p]]], {p}] for spin 1/2. Option \"Limit\" -> \"Static\" sets all external momenta and frequencies to zero (the limit q -> 0 at zero frequency, the thermodynamic response: at q = 0 the double poles give derivatives of n_F), \"Dynamic\" sets the external momenta to zero and takes the external frequency to zero at the end (zero for the density of a conserving approximation; a message if it diverges).";
AnalyticContinuation::usage = "AnalyticContinuation[expr, w -> omega] continues I w -> omega + I 0+ after the Matsubara sums (the retarded function): the result is a function of omega, understood as omega + I 0+. The statistical factors must be free of w (declare its statistics with DeclareMatsubaraFrequency before the sums).";
RetardedImaginaryPart::usage = "RetardedImaginaryPart[expr, omega] is the imaginary part of a retarded function of omega + I 0+ (the result of AnalyticContinuation) with real coefficients: 1/(omega - E + I 0+) -> -\[Pi] EnergyDelta[omega - E], and poles of order k give derivatives of EnergyDelta. It works inside MomentumSum.";
CuttingRuleImaginaryPart::usage = "CuttingRuleImaginaryPart[diagrams, externals, rules, w -> omega] is the imaginary part of a retarded two-point function from the cutting rules at finite temperature, computed in imaginary time without Matsubara sums: the cut separates the time vertex where I w enters from the one where it leaves, every cut line is on shell (EnergyDelta) with its factor of emission (1 - n_F, 1 + n_B) or absorption (n_F, n_B), and the factor 1 + e^(-omega/T) (fermions) or 1 - e^(-omega/T) (bosons) adds the inverse process. It is implemented for diagrams whose propagating lines form chains from one external time vertex to the other (bubbles, sunsets, with static insertions such as Hartree and Fock on the lines); other diagrams give CuttingRuleImaginaryPart::class and $Failed, and so does a frequency w that does not enter the diagram at one external point and leave it at another (CuttingRuleImaginaryPart::frequency). Arguments and options as in EvaluateMatsubara.";
CheckCuttingRules::usage = "CheckCuttingRules[diagrams, externals, rules, w -> omega] compares, term by term and for each EnergyDelta, the imaginary part of the retarded function from the Matsubara sums (EvaluateMatsubara, AnalyticContinuation, RetardedImaginaryPart) with the cutting rules (CuttingRuleImaginaryPart); statistical factors are compared through e^(e/T). It gives 0 if they agree, or the list of the differences ($Failed for a term to which the cutting rules do not apply), and $Failed for a frequency w that does not cross the diagram. Options as in EvaluateMatsubara.";
ZeroTemperatureLimit::usage = "ZeroTemperatureLimit[expr] is the limit T -> 0 of the statistical factors: n_F(e) -> UnitStep[-e], n_B(e) -> -UnitStep[-e], and their derivatives give derivatives of EnergyDelta (n_F'(e) -> -EnergyDelta[e]). n_F(0) = 1/2 at every temperature, while UnitStep[0] = 1: substitute an energy that may vanish before taking the limit. The terms of the sums of logarithms go as Temperature Log[1 - FermiFunction[e]] -> e UnitStep[-e] and Temperature Log[1 + BoseFunction[e]] -> 0.";
Temperature::usage = "Temperature is the temperature T of the statistical factors, n_F(e) = 1/(e^(e/Temperature) + 1) (FermiFunction) and n_B(e) = 1/(e^(e/Temperature) - 1) (BoseFunction). It appears explicitly only in the sums of logarithms of MatsubaraSum and in the traces Tr ln of EvaluateMatsubara: T Sum_n e^(I \[Omega]_n 0+) ln(a - I \[Omega]_n) = -Temperature Log[1 - FermiFunction[a]] = T ln(1 + e^(-a/T)), minus the grand potential of one level (\[CapitalOmega] = -Tr ln(-G0^-1) per level).";

(* ------------------------------------------------ evaluation: dimensional regularization *)
ScalarPropagator::usage = "ScalarPropagator[q, m] is the Euclidean scalar propagator 1/(q^2 + m^2) of a line with momentum q (a linear combination of momenta) and mass m (0 for a massless line); ScalarPropagator[q, m]^a is 1/(q^2 + m^2)^a, with a any power (massless lines get powers a + b - 2 + \[Epsilon] from the one-loop integrals inside them).";
MomentumSquared::usage = "MomentumSquared[q] is the square q^2 of a momentum q, a linear combination of momenta (c p gives c^2 p^2 for a single momentum p and a number c).";
RenormalizationScale::usage = "RenormalizationScale is the scale \[Mu] of dimensional regularization: every loop is \[Mu]^(2\[Epsilon]) (e^\[Gamma]E/(4\[Pi]))^\[Epsilon] \[Integral] d^dk/(2\[Pi])^d, d = 4 - 2\[Epsilon] (the MS-bar measure: minimal subtraction of the poles in \[Epsilon] is then the MS-bar scheme, and \[Mu] is the MS-bar scale).";
LoopIntegral::usage = "LoopIntegral[f, {k1, k2, ...}] is the inert Euclidean loop integral of f over the loop momenta k1, k2, ..., each with the MS-bar measure \[Mu]^(2\[Epsilon]) (e^\[Gamma]E/(4\[Pi]))^\[Epsilon] \[Integral] d^dk/(2\[Pi])^d in d = 4 - 2\[Epsilon] (\[Mu] = RenormalizationScale); f is a product of ScalarPropagator factors, possibly with numerators that are squares of momenta (MomentumSquared[q], or k^2 for a loop momentum k). It depends on \[Epsilon] only through d: EpsilonExpand refuses it (evaluate it first with IntegrateLoops).";
DiagramLoopIntegral::usage = "DiagramLoopIntegral[diagrams, {x1 -> p1, x2 -> p2, ...}, {G0 -> {\"Scalar\", m}, ...}] is the momentum-space form of the diagrams of the package (functions of labels, local vertices), for a translation-invariant theory at zero temperature in Euclidean space: the momentum pi enters at the external label xi (they add up to zero), the momenta are routed through the lines as in EvaluateMatsubara, and each term is its weight times LoopIntegral[product of the lines, loop momenta]. A rule G0 -> {\"Scalar\", m} makes the line G0[x, y] the propagator ScalarPropagator[q, m] = 1/(q^2 + m^2) (m = 0: massless); G0 -> {\"Kernel\", f} gives it the value f[q] (an insertion such as a counterterm). The momentum q of a line is written with the sign that makes its first term positive, so f must be even, f[-q] = f[q] (a function of q^2). Option \"LoopMomenta\" -> {k1, ...} names the loop momenta (default LoopMomentum[i]).";
IntegrateLoops::usage = "IntegrateLoops[expr, \[Epsilon]] evaluates the LoopIntegral objects of expr in d = 4 - 2\[Epsilon] when the loops can be done one after the other as one-loop integrals: at each step a loop momentum that appears in at most two different propagators is integrated, giving a tadpole (TadpoleIntegral), a massless one-loop integral (GFunction times (P^2)^(2 - \[Epsilon] - a - b), a new massless line of non-integer power) or a massive bubble (BubbleIntegral). This covers the massless propagator-type integrals that reduce to nested one-loop ones (the sunset, bubble insertions, the two-loop four-point diagrams with the external momentum through two legs) and products of massive one-loop factors. Numerators q^2 are written 1/ScalarPropagator[q, m] - m^2 with the mass of a line of the same momentum (1/ScalarPropagator[q, 0] otherwise); a numerator in a bubble with a massive line is reduced to tadpoles. Scaleless integrals are 0, as in dimensional regularization; when one of them is logarithmically divergent, its ultraviolet pole (cancelled by an infrared one) is lost, and a message says so. A message is also given for an infrared divergence (a massless line of power 2 or more at \[Epsilon] = 0, or of zero momentum): the poles of the result are then not only ultraviolet ones. For ultraviolet counterterms give the lines a mass, or use InfraredRearrangedIntegral. A loop integral that is not reduced gives a message and stays unevaluated.";
GFunction::usage = "GFunction[a, b, \[Epsilon]] is the coefficient G(a, b) = \[CapitalGamma](d/2 - a) \[CapitalGamma](d/2 - b) \[CapitalGamma](a + b - d/2)/(\[CapitalGamma](a) \[CapitalGamma](b) \[CapitalGamma](d - a - b)), d = 4 - 2\[Epsilon], of the massless one-loop integral \[Integral] d^dk/(2\[Pi])^d 1/((k^2)^a ((k + p)^2)^b) = (4\[Pi])^(-d/2) G(a, b) (p^2)^(d/2 - a - b); a and b can be any powers.";
TadpoleIntegral::usage = "TadpoleIntegral[a, m, \[Epsilon]] is the one-loop tadpole \[Integral]_k 1/(k^2 + m^2)^a with the MS-bar measure of LoopIntegral: e^(\[Epsilon] \[Gamma]E) \[Mu]^(2\[Epsilon]) \[CapitalGamma](a - d/2)/(16 \[Pi]^2 \[CapitalGamma](a)) (m^2)^(d/2 - a), exact in d = 4 - 2\[Epsilon] (0 for m = 0, and for a power a that is an integer a <= 0).";
BubbleIntegral::usage = "BubbleIntegral[{a, b}, p2, {m1, m2}, \[Epsilon]] is the one-loop bubble \[Integral]_k 1/((k^2 + m1^2)^a ((k + p)^2 + m2^2)^b) at p^2 = p2, with the MS-bar measure of LoopIntegral. It evaluates to closed forms in d when there is one: both lines massless (GFunction); a power that is an integer <= 0 (a numerator, reduced to tadpoles); p2 = 0 with equal masses or a massless line (tadpoles and Gamma functions) and p2 = 0 with unequal masses for a = b = 1 (partial fractions). Otherwise it stays as the Feynman-parameter integral e^(\[Epsilon] \[Gamma]E) \[Mu]^(2\[Epsilon]) \[CapitalGamma](a + b - d/2)/(16 \[Pi]^2 \[CapitalGamma](a) \[CapitalGamma](b)) \[Integral]_0^1 dx x^(a - 1) (1 - x)^(b - 1) \[CapitalDelta]^(d/2 - a - b), \[CapitalDelta] = x (1 - x) p2 + x m1^2 + (1 - x) m2^2, which EpsilonExpand expands when the powers are positive, independent of \[Epsilon], and less than 2 on a massless line: in closed form (logarithms; the square root of the K\[ADoubleDot]ll\[EAcute]n function, and ArcTanh for equal masses) for the finite part of a = b = 1 and for a + b = 3, as FeynmanParameterIntegral beyond.";
FeynmanParameterIntegral::usage = "FeynmanParameterIntegral[f, FeynmanParameter] is the inert integral \[Integral]_0^1 dx f of the Feynman parameter x = FeynmanParameter, left by EpsilonExpand for the coefficients of a massive bubble that it does not give in closed form; N evaluates it by NIntegrate (N[expr, n] to n digits).";
FeynmanParameter::usage = "FeynmanParameter is the Feynman parameter x, integrated from 0 to 1, in FeynmanParameterIntegral[f, FeynmanParameter]. FeynmanParameter[i] are the Feynman parameters of IntegrateOneLoop (in ParameterIntegral and SimplexIntegral).";
EpsilonExpand::usage = "EpsilonExpand[expr, \[Epsilon], n] is the Laurent expansion of expr in \[Epsilon] up to \[Epsilon]^n included, computed without Series: Gamma functions of arguments n0 + c \[Epsilon] through PolyGamma (and their recurrence at the poles), powers X^(c \[Epsilon]) through logarithms (ln(X/\[Mu]^2) when the powers of \[Mu] = RenormalizationScale balance them), exponentials and rational functions of \[Epsilon] term by term, and massive bubbles (BubbleIntegral in this \[Epsilon]) through their Feynman parameters. With the MS-bar measure of LoopIntegral no Euler constant and no ln(4\[Pi]) appears. An unevaluated LoopIntegral or MinkowskiLoopIntegral, whose dependence on \[Epsilon] is not explicit, gives a message and $Failed.";
PolePart::usage = "PolePart[expr, \[Epsilon]] is the part of the Laurent expansion of expr with negative powers of \[Epsilon] (EpsilonExpand[expr, \[Epsilon], -1]): the MS-bar counterterm is minus it.";
InfraredRearrangedIntegral::usage = "InfraredRearrangedIntegral[diagrams, {x1, ..., xn}, p, rules, \[Epsilon]] evaluates each term of diagrams with one momentum p entering at one external label and leaving at another (the other external momenta zero), trying the pairs of labels in order until the term reduces to nested one-loop integrals without infrared divergences (no massless line of power 2 or more at \[Epsilon] = 0) and without logarithmically divergent scaleless integrals: the infrared rearrangement, legitimate for the ultraviolet poles of logarithmically divergent functions once their subdivergences are subtracted, since those are polynomial in the external momenta and the masses. A part of a term with a scaleless loop that is only power divergent in the ultraviolet (a massless tadpole) is 0: in a massless theory such a loop has no ultraviolet counterterm. A term without a valid routing gives a message and $Failed. Only the counterterms left after the subtraction of the subdivergences are physical: the finite parts and the pole parts of the single rearranged diagrams (with their logarithms of p^2/\[Mu]^2) depend on the routing. With the option \"Routing\" -> True the result is the list of {term, {label in, label out}, value}.";
MassInsertion::usage = "MassInsertion[diagrams, G0, z] is -\[PartialD]/\[PartialD]m^2 of the diagrams through their lines G0 = 1/(q^2 + m^2): the sum, over the lines G0[x, y], of the diagram with that line replaced by G0[x, z] G0[z, y], z a new external label, a symbol that does not appear in the diagrams (the insertion of the operator \[Phi]^2/2 at z). At m = 0 the momentum entering at z makes the infrared rearrangement of \[PartialD]\[CapitalSigma]/\[PartialD]m^2 possible (InfraredRearrangedIntegral).";
BetaFunctionFromZ::usage = "BetaFunctionFromZ[Z, g, \[Epsilon], n] is the beta function \[Beta](g) = \[Mu] dg/d\[Mu] (at \[Epsilon] = 0, to order g^n) of a coupling renormalized as g_B = \[Mu]^(2\[Epsilon]) Z g (MS-bar, d = 4 - 2\[Epsilon]): \[Beta] = -2 \[Epsilon] g/(1 + g \[PartialD]_g ln Z) + 2 \[Epsilon] g, expanded in g; the limit \[Epsilon] -> 0 must exist (a check of the poles of Z), otherwise a message is given. Option \"ScalePower\" -> s for g_B = \[Mu]^(s \[Epsilon]) Z g (s = 2 for \[Lambda] \[Phi]^4, s = 1 for a gauge or Yukawa coupling in d = 4 - 2\[Epsilon]).";
AnomalousDimensionFromZ::usage = "AnomalousDimensionFromZ[Z, g, \[Epsilon], betaEpsilon, n] is \[Mu] d ln Z/d\[Mu] = betaEpsilon \[PartialD]_g ln Z at \[Epsilon] = 0, to order g^n, with betaEpsilon = -s \[Epsilon] g + \[Beta](g) the beta function in d = 4 - 2\[Epsilon] (s = 2 for g_B = \[Mu]^(2\[Epsilon]) Z_g g); the limit \[Epsilon] -> 0 must exist, otherwise a message is given. For a field \[Phi]_B = Z_\[Phi]^(1/2) \[Phi], \[Gamma]_\[Phi] = 1/2 \[Mu] d ln Z_\[Phi]/d\[Mu].";

(* ------------------------------------------------ evaluation: QED at one loop *)
GammaMatrix::usage = "GammaMatrix[\[Mu]] is the Dirac matrix \[Gamma]^\[Mu] in d dimensions (Minkowski metric (+,-,-,-), {\[Gamma]^\[Mu], \[Gamma]^\[Nu]} = 2 g^\[Mu]\[Nu], Tr 1 = 4). The Lorentz indices are abstract: an index that appears twice in a term is contracted, more than twice is an error, and explicit components (GammaMatrix[0], ...) are not supported. There is no \[Gamma]5: GammaMatrix[5] is refused by DiracSimplify, DiracOrder and IntegrateOneLoop (message GammaMatrix::gamma5; $Failed, or the integral left unevaluated).";
SlashedMomentum::usage = "SlashedMomentum[p] is p-slash = \[Gamma]^\[Mu] p_\[Mu], linear in p: a linear combination of momenta (symbols, or objects such as p[1] and LoopMomentum[i]) with coefficients that are numbers or polynomials in FeynmanParameter[i]. A symbolic coefficient (x p, m p, p/m) is ambiguous: the object is left as it is, with the message DiracSimplify::momentum.";
DiracProduct::usage = "DiracProduct[a, b, ...] is the product of Dirac matrices a b ... (GammaMatrix, SlashedMomentum, sums of them and scalars); DiracProduct[] is the unit matrix. DiracSimplify expands it, takes the scalars out and contracts its indices. A product of matrices written with Times or Power is refused (DiracSimplify::product).";
DiracTrace::usage = "DiracTrace[X] is the trace of the Dirac matrix X (Tr 1 = 4, no \[Gamma]5); DiracSimplify evaluates it.";
SpinorChain::usage = "SpinorChain[{p2, m2}, X, {p1, m1}] is ubar(p2) X u(p1), with on-shell spinors: p2^2 = m2^2, p1^2 = m1^2, ubar(p2) p2-slash = m2 ubar(p2), p1-slash u(p1) = m1 u(p1). DiracSimplify moves p1-slash to the right and p2-slash to the left and uses the Dirac equation; the other momenta should be written in terms of p1 and p2 (q -> p2 - p1).";
LorentzMetric::usage = "LorentzMetric[\[Mu], \[Nu]] is the metric g^\[Mu]\[Nu] (symmetric; g^\[Mu]_\[Mu] = d = SpacetimeDimension; abstract indices, no components).";
FourVector::usage = "FourVector[p, \[Mu]] is the component p^\[Mu] of the momentum p (\[Mu] an abstract index).";
ScalarProduct::usage = "ScalarProduct[p, q] is the Minkowski scalar product p\[CenterDot]q (symmetric and bilinear; ScalarProduct[p, p] = p^2).";
SpacetimeDimension::usage = "SpacetimeDimension is the dimension d of the Dirac algebra (g^\[Mu]_\[Mu] = d); IntegrateOneLoop and ExpandParameterIntegrals set d = 4 - 2\[Epsilon].";
DiracSimplify::usage = "DiracSimplify[expr] expands the Dirac matrices of expr (DiracProduct, DiracTrace, SpinorChain), makes slashed momenta, four-vectors and scalar products linear, contracts the repeated Lorentz indices (\[Gamma]^\[Mu] \[Gamma]_\[Mu] = d, \[Gamma]^\[Mu] a \[Gamma]_\[Mu] = (2 - d) a, \[Gamma]^\[Mu] a b \[Gamma]_\[Mu] = 4 a\[CenterDot]b - (4 - d) a b, ..., in general \[Gamma]^\[Mu] X a \[Gamma]_\[Mu] = 2 a X - (\[Gamma]^\[Mu] X \[Gamma]_\[Mu]) a), p-slash p-slash = p^2, evaluates the traces and applies the Dirac equation in the spinor chains. It gives a message and $Failed for GammaMatrix[5], explicit component indices, a commutative product of matrices and an index that appears more than twice in a term (also in a term of the integrand of a loop integral); a momentum that is not a linear combination of momenta with coefficients that are numbers or polynomials in the Feynman parameters is left as it is (message). An unevaluated MinkowskiLoopIntegral is simplified inside (one in a SpinorChain or a DiracTrace is first taken out of it: the spinors do not depend on the loop momentum, which is renamed if a spinor has a momentum of the same name, and the trace is linear; an integrand that is 0 gives 0): its integrand is expanded and its indices are counted term by term, and in the term outside it counts with its free indices.";
DiracOrder::usage = "DiracOrder[expr] is DiracSimplify[expr] with the factors of every product put in a canonical order by {a, b} = 2 a\[CenterDot]b: two expressions are equal when DiracOrder of their difference is 0.";
QEDIntegrand::usage = "QEDIntegrand[diagrams, {x1 -> p1, ...}, {S -> {\"Fermion\", m}, Ph -> {\"Photon\", \[Xi]}, V -> {\"Vertex\", c}}] is the momentum-space form of one-loop diagrams of the package with spinor and Lorentz indices (condensed notation): S[x, y] = <T \[Psi](x) \[Psi]bar(y)> is the fermion line, i (q-slash + m)/(q^2 - m^2) with q flowing from y to x; Ph[z, w] the photon, -i [g^\[Mu]\[Nu] - (1 - \[Xi]) q^\[Mu] q^\[Nu]/(q^2 - \[Lambda]^2)]/(q^2 - \[Lambda]^2) (Ph -> {\"Photon\", {\[Xi], \[Lambda]}} for a photon mass \[Lambda], an infrared regulator); V[x, y, z] the local vertex c \[Gamma]^\[Mu] (spinor indices of x and y, Lorentz index of z). The momentum pi enters at the external label xi (they add up to zero). The spinor indices are followed along the fermion lines: an open line is a DiracProduct (from its \[Psi] end to its \[Psi]bar end), a closed loop a DiracTrace (its minus sign is in the weight of the package); at most one open line, one loop. Each term is its weight times MinkowskiLoopIntegral[numerator FeynmanDenominator[q, m]..., {k}]. Options \"LoopMomentum\" -> k (default LoopMomentum[1], written \[ScriptL]_1 by ExpressionToLaTeX) and \"Indices\" -> {x -> \[Mu], ...} (the Lorentz index of a label; by default the label itself).";
FeynmanDenominator::usage = "FeynmanDenominator[q, m] is the Minkowski propagator denominator 1/(q^2 - m^2 + i0).";
MinkowskiLoopIntegral::usage = "MinkowskiLoopIntegral[f, {k}] is the inert one-loop integral of f over the loop momentum k with the measure \[Mu]^(2\[Epsilon]) (e^\[Gamma]E/(4\[Pi]))^\[Epsilon] \[Integral] d^dk/(2\[Pi])^d in Minkowski space, d = 4 - 2\[Epsilon] (\[Mu] = RenormalizationScale; MS-bar); f is a product of FeynmanDenominator factors (integer powers; a negative power is a numerator) and of a numerator polynomial in k (slashed momenta, four-vectors, scalar products, Dirac products and traces). DiracSimplify simplifies its integrand, IntegrateOneLoop evaluates it.";
IntegrateOneLoop::usage = "IntegrateOneLoop[expr, \[Epsilon]] evaluates the MinkowskiLoopIntegral objects of expr in d = 4 - 2\[Epsilon]: Feynman parameters FeynmanParameter[i] for any number of denominators and any integer powers, the shift k = l - \[CapitalSigma] xi ri, the tensor reduction l^\[Mu]1 ... l^\[Mu]2n -> (l^2)^n (sum of the products of metrics)/(d (d+2) ... (d+2n-2)) (odd ranks vanish), and \[Integral]_l (l^2)^J/(l^2 - \[CapitalDelta])^A = i (-1)^(A+J) e^(\[Epsilon] \[Gamma]E) \[Mu]^(2\[Epsilon]) \[CapitalGamma](J + d/2) \[CapitalGamma](A - J - d/2)/(16 \[Pi]^2 \[CapitalGamma](d/2) \[CapitalGamma](A)) \[CapitalDelta]^(d/2 + J - A), with \[CapitalDelta] - i0 (Feynman). The result is a sum of Dirac and tensor structures times exact Feynman-parameter integrals: ParameterIntegral (two denominators) or SimplexIntegral (three or more). An integral with more than one loop momentum, a non-integer power of a denominator or a numerator that is not polynomial in k is left unevaluated with a message; without a denominator depending on k it is 0 (scaleless).";
SimplexIntegral::usage = "SimplexIntegral[f, {x1, ..., xn}] is the inert Feynman-parameter integral \[Integral]_0^1 dx1 ... dxn \[Delta](1 - x1 - ... - xn) f. DifferentiateUnderIntegral differentiates under the integral sign, ReduceParameterIntegrals does it in closed form where possible, N evaluates it by NIntegrate over the unit cube, with \[CapitalDelta] - i0: the logarithms and non-integer powers of a base that is negative somewhere (above a threshold) are Log|b| - i\[Pi] and |b|^a e^(-i\[Pi] a) there; N does not evaluate a power <= -1 of a base that changes sign in the region (with \[CapitalDelta] - i0 the integral is finite, but NIntegrate cannot integrate the pole): SimplexIntegral::threshold and Indeterminate.";
ParameterIntegral::usage = "ParameterIntegral[f, x] is the inert integral \[Integral]_0^1 dx f (exact in d: f may contain \[Epsilon]). DifferentiateUnderIntegral differentiates under the integral sign, ReduceParameterIntegrals does it in closed form where possible, N evaluates it by NIntegrate (N[expr, n] to n digits), with \[CapitalDelta] - i0: the logarithms and non-integer powers of a base that is negative somewhere (above a threshold) are Log|b| - i\[Pi] and |b|^a e^(-i\[Pi] a) there, the interval split at the real roots of the bases; N does not evaluate a power <= -1 of a base with a root inside the interval (with \[CapitalDelta] - i0 the integral is finite, but NIntegrate cannot integrate the pole): ParameterIntegral::threshold and Indeterminate.";
ReduceParameterIntegrals::usage = "ReduceParameterIntegrals[expr] does the Feynman-parameter integrals of expr where possible, exactly in d: a SimplexIntegral whose non-polynomial factors depend on one parameter only (after the delta function is used, e.g. the vertex at q^2 = 0), and polynomially on the others, is integrated over the others; a ParameterIntegral of powers of monomials c x^a (1 - x)^b, with exponents that do not depend on x (e.g. on shell), is a product of Gamma functions (infrared divergences then appear as poles in \[Epsilon], dimensional regularization of the infrared). The powers c^a of the coefficients are taken at c - i0 when c is a negative number; for a symbolic c, c^a is the principal power (valid for c > 0, below thresholds). The others stay.";
CombineParameterIntegrals::usage = "CombineParameterIntegrals[expr] writes the Feynman-parameter integrals of expr that have the same variables and the same factor in front as one integral of the sum of their integrands. The factor in front is the Dirac or tensor structure and the factors that do not appear in the integrand, such as the coupling; the numbers and the other factors go into the integrand. ReduceParameterIntegrals does it at the end.";
ExpandParameterIntegrals::usage = "ExpandParameterIntegrals[expr, \[Epsilon], n] is the Laurent expansion of expr in \[Epsilon] up to \[Epsilon]^n: ReduceParameterIntegrals, then EpsilonExpand, with the integrands of the remaining Feynman-parameter integrals expanded under the integral sign to the order that the factor in front of each needs (polynomial coefficients integrated exactly, the others inert ParameterIntegral and SimplexIntegral, evaluated by N). The expansion under the integral sign needs coefficients integrable at \[Epsilon] = 0: an integrand that is not (an infrared divergence at a face of the region, where a base vanishes) has its singular part subtracted, exactly in d (Beta functions times integrals over the face, which carry the infrared poles), when there is one singular face (in one dimension, each endpoint) and every factor of the singular terms that depends on the parameters is a power of a polynomial (as in the results of IntegrateOneLoop), the exponents of those that vanish there linear in \[Epsilon] and regulated by it; otherwise ExpandParameterIntegrals::ir and $Failed. Other factors (logarithms, Gamma functions of the parameters) are assumed regular and are not checked. A list is expanded element by element.";
DifferentiateUnderIntegral::usage = "DifferentiateUnderIntegral[expr, v] is the derivative of expr with respect to v, with the integrands of its ParameterIntegral and SimplexIntegral objects differentiated under the integral sign (e.g. d/dp^2 of a self-energy, for the wave-function renormalization).";
VertexFormFactors::usage = "VertexFormFactors[\[CapitalGamma], \[Mu], {p1, p2, m}] gives {F1, F2, F3} for a vertex ubar(p2) \[CapitalGamma]^\[Mu] u(p1) = ubar(p2) [\[Gamma]^\[Mu] F1 + i \[Sigma]^\[Mu]\[Nu] q_\[Nu] F2/(2m) + q^\[Mu] F3] u(p1), q = p2 - p1, between spinors of mass m (\[CapitalGamma] a sum of SpinorChain[{p2, m}, ..., {p1, m}] terms; it is reduced by DiracSimplify to \[Gamma]^\[Mu] A + p1^\[Mu] B1 + p2^\[Mu] B2 and decomposed by the Gordon identity: F1 = A + m (B1 + B2), F2 = -m (B1 + B2), F3 = (B2 - B1)/2). A part of another form gives VertexFormFactors::rest and $Failed.";

(* ------------------------------------------------ effective actions and operator traces *)
EffectiveAction2PI::usage = "EffectiveAction2PI[Phi, G0 -> G] is the two-particle-irreducible (2PI) effective action \[CapitalGamma][G] at zero mean field built on the Luttinger-Ward functional Phi (minus the 2PI vacuum diagrams, LuttingerWardFunctional): for fermions (\"Statistics\" -> \"Fermion\", the default, G = -<psi psibar>) \[CapitalGamma][G] = Phi[G] - Tr ln(-G^-1) - Tr(G0^-1 G - 1); for real bosons (\"Boson\", G = <phi phi> symmetric) \[CapitalGamma]2[G] + 1/2 Tr ln G^-1 + 1/2 Tr(D0^-1 G - 1); for complex bosons (\"ComplexBoson\", G = <phi phibar>) Phi + Tr ln G^-1 + Tr(G0^-1 G - 1); for a superfield (\"Superfield\", a Grassmann field with the antisymmetric propagator G = -<T NambuField NambuField>, e.g. the Nambu field NambuField = (psi, psibar), whose propagator has the normal block and the anomalous ones -<T psi psi>, -<T psibar psibar>) Phi[G] - 1/2 Tr ln(-G^-1) - 1/2 Tr(G0^-1 G - 1), the Nambu-Gor'kov functional (the factor 1/2 of a Pfaffian; Hartree-Fock-Bogoliubov and the BCS gap equation are its stationarity at first order, and its components are given by SuperfieldComponents). It is the Legendre transform of -log Z (W = log Z) with respect to the two-point source K (and, with \"MeanFields\", the one-point source J): stationary (FunctionalDerivative with respect to G) where G^-1 = G0^-1 - \[CapitalSigma][G] (Dyson, \[CapitalSigma] = SelfEnergy[Phi, G]), and equal to -log Z (the grand potential over T) there. It is also called the Luttinger-Ward or Baym-Kadanoff functional \[CapitalOmega][G], and \[CapitalGamma][<phi>, G] of Cornwall, Jackiw and Tomboulis at zero mean field; some authors (Almbladh, von Barth and van Leeuwen) call this form the Klein functional and keep the name Luttinger-Ward for the form with \[CapitalSigma][G] inside the logarithm. A truncated Phi gives a \[CapitalPhi]-derivable (conserving) approximation. EffectiveAction2PI[Z0, interaction, n, G0 -> G] takes Phi = LuttingerWardFunctional[Z0, interaction, n, G0 -> G]. The option \"MeanFields\" -> {phi -> phiMean} (one real boson) or {psi -> Psi, psibar -> Psibar} (fermions: Grassmann-odd means, zero on shell at zero sources; complex bosons {phi -> phiMean, phibar -> phibarMean}, the unbarred field being the one whose source is in the first slot of G0 in Z0, or the one not declared with the sign -1) or {NambuField -> X} (a superfield: one Grassmann-odd mean, the free part -1/2 X G0^-1 X, the trace term -1/2 Tr(S''[X] G - 1) with the whole second derivative of S, its anomalous vertices included) makes the expectation values of the fields variables too, declared as sources so that FunctionalDerivative differentiates with respect to them. EffectiveAction2PI[Z0, interaction, n, D0 -> G, \"Statistics\" -> \"Boson\", \"MeanFields\" -> {phi -> phiMean}] is then the 2PI effective action \[CapitalGamma][phiMean, G] of Cornwall, Jackiw and Tomboulis, S[phiMean] + 1/2 Tr ln G^-1 + 1/2 Tr(D^-1[phiMean] G - 1) + \[CapitalGamma]2[phiMean, G]: S is the classical action, D^-1[phiMean] = S''[phiMean] = D0^-1 + S_int''[phiMean] the bare inverse propagator in the background (phi^4: the mass lambda phiMean^2/2, which 1/2 Tr(S_int''[phiMean] G) closes with one line G), and \[CapitalGamma]2 is minus the 2PI vacuum diagrams with lines G and the vertices of S_int[phiMean + phi] of degree three or more in phi (phi^4: lambda phiMean phi^3/6 and lambda phi^4/24); the mean field enters \[CapitalGamma]2 only through these vertices, and at phiMean = 0 it is \[CapitalGamma][G]. Its stationarity conditions are the field equation \[Delta]\[CapitalGamma]/\[Delta]phiMean = S'[phiMean] + 1/2 S'''[phiMean] G + \[Delta]\[CapitalGamma]2/\[Delta]phiMean = 0 and the Dyson equation G^-1 = D^-1[phiMean] - \[CapitalSigma][phiMean, G] with \[CapitalSigma] = SelfEnergy of \[CapitalGamma]2: through D^-1 the self-energy gains its mean-field part -S_int''[phiMean], zero at phiMean = 0 (phi^4: the tadpole -lambda phiMean^2/2, which with -lambda G(x, x)/2 from \[CapitalGamma]2 makes the tadpole of <phi phi> = G + phiMean phiMean). At phiMean != 0 a diagram of \[CapitalSigma] need not be 2PI as an open graph (the bubble with two vertices lambda phiMean, as in phi^3 theory): the perturbative \[CapitalSigma] in the background is selected with SkeletonDiagrams, not with ParticleIrreducibleDiagrams[\[CapitalSigma], 2, ...]. For fermions \[CapitalGamma][Psibar, Psi, G] = S[Psibar, Psi] - Tr ln(-G^-1) - Tr(D^-1[Psibar, Psi] G - 1) + \[CapitalGamma]2[Psibar, Psi, G], with S[Psibar, Psi] = -Psibar G0^-1 Psi + S_int[Psibar, Psi] and D^-1[Psibar, Psi](x, y) = FunctionalDerivative[S, Psibar[x], Psi[y]] = G0^-1(x, y) - Sigma_HF[-Psi Psibar](x, y), the Hartree and Fock potentials of the means (the first-order self-energy Sigma_HF[G] with -Psi(x) Psibar(y) in place of G(x, y)), closed by G in -Tr(D^-1 G - 1) = -Tr(G0^-1 G - 1) + Tr(Sigma_HF[-Psi Psibar] G) (D^-1 is the normal block of the Nambu matrix S''[Psibar, Psi] of the second derivatives of S, whose one loop -1/2 Tr ln(-S'') is that of EffectiveAction1PI, zero means giving G0^-1; the Dyson equation G^-1 = D^-1[Psibar, Psi] - \[CapitalSigma][Psibar, Psi, G] has at first order Sigma_HF[G - Psi Psibar], the Hartree and Fock potentials of the whole -<T psi psibar> = G - Psi Psibar), and \[CapitalGamma]2 minus the 2PI vacuum diagrams with lines G and the vertices of degree three or more, those with an odd number of fluctuating fields carrying one mean: the Nambu-Gor'kov functional of the superfield (psi, psibar) at zero anomalous propagator, exact at zero means in the normal state (no pairing source and no spontaneous pairing; in a paired state the anomalous propagator is finite at zero means too); at finite means the anomalous propagator is needed (Gpsipsi ~ V Psi Psi), and the superfield functional (\"Statistics\" -> \"Superfield\", \"MeanFields\" -> {NambuField -> X}) at its stationary G is the 1PI action \[CapitalGamma][X], the anomalous diagrams included. For an interaction even in the fields (phi^4, the two-body interaction of fermions), where \[Delta]^2\[CapitalGamma]/\[Delta]phiMean \[Delta]G = 0 at zero mean field, the derivatives with respect to the means at zero mean field and fixed G are skeleton vertex functions: the second is D0^-1 - \[CapitalSigma][G] (for fermions FunctionalDerivative[Gamma, Psibar[a], Psi[b]] = G0^-1(a, b) - \[CapitalSigma][G](a, b), the rightmost derivative acting first), the fourth the fully irreducible vertex \[CapitalGamma]irr (\"FullyIrreducible\" of ParquetDecomposition: the bare vertex, then the envelope from the fourth order; for fermions FunctionalDerivative[Gamma, Psibar[x1], Psi[x2], Psibar[x3], Psi[x4]] = \[CapitalGamma]irr(x1, x2; x3, x4) in the convention F(x1, x2; x3, x4) of PerturbativeFullVertex, irreducible in all three channels, and FunctionalDerivative[Gamma, G[x4, x3], Psibar[x1], Psi[x2]] = -\[CapitalGamma]0(x1, x2; x3, x4); for a superfield FunctionalDerivative[Gamma, X[a], X[b], X[c], X[d]] = \[CapitalGamma]irr(a, b, c, d), and FunctionalDerivative[Gamma, G[a, b]] = -1/2 (\[CapitalSigma] + G^-1 - G0^-1)(a, b)); \"MeanFieldDegree\" -> {k, ...} computes only the terms of these degrees in the means. In general (phi^3) \[Delta]^2\[CapitalGamma]/\[Delta]phiMean^2 at fixed G differs from G^-1 (for phi^3 it is D0^-1: the bubble of \[CapitalSigma][G] comes through the mixed derivative \[Delta]^2\[CapitalGamma]/\[Delta]phiMean \[Delta]G), and the 1PI \[CapitalGamma]^(2) is the Schur complement \[CapitalGamma]_phiMeanphiMean - \[CapitalGamma]_phiMeanG \[CapitalGamma]_GG^-1 \[CapitalGamma]_GphiMean. Its 1PI counterpart is EffectiveAction1PI. The option \"Source\" -> U adds a one-body source to the bare inverse propagator, so that the action gains \[Integral] (barred field) U (field): G0^-1 -> G0^-1 - U for fermions (\[Integral] psibar U psi), G0^-1 + U for complex bosons (\[Integral] phibar U phi), D0^-1 + U for real bosons (1/2 \[Integral] phi U phi), G0^-1 - K for a superfield (1/2 \[Integral] NambuField K NambuField: the one-body source and the pairing sources 1/2 \[Integral] psi Kpsipsi psi, 1/2 \[Integral] psibar Kpsibarpsibar psibar). \[CapitalGamma] depends on U only through the trace term, so that by stationarity the derivative of the grand potential is the explicit one, \[Delta]\[CapitalOmega]/\[Delta]U(x, y) = G(y, x) = <psibar(x) psi(y)> (<phibar(x) phi(y)> for complex bosons, 1/2 G(x, y) for real bosons): an expectation value is a variation, N = -\[PartialD]\[CapitalOmega]/\[PartialD]\[Mu] for U = -\[Mu] \[Delta]. U must be a kernel: a single symbol is taken as one, so that a chemical potential is \"Source\" -> LabelFunction[-mu DeltaFunction[a, b], {a, b}] (or mu is put into G0). Its second derivative is the response of a \[CapitalPhi]-derivable approximation, \[Chi] = \[Gamma] L \[Gamma] with L = L0 + L0 \[CapitalGamma]0 L and \[CapitalGamma]0 = \[Delta]\[CapitalSigma]/\[Delta]G (ExactEquation \"Response\" and \"Susceptibility\"), conserving: its density response vanishes at zero momentum and finite frequency, and its static limit is the second derivative of \[CapitalOmega] (EvaluateMatsubara, option \"Limit\"). EvaluateMatsubara evaluates \[CapitalGamma] in momentum and Matsubara frequency, its traces included. Option \"Method\" as in PerturbativeCorrelationFunction, for the diagrams of Phi when it is computed from Z0.";
EffectiveAction1PI::usage = "EffectiveAction1PI[Z0, interaction, n, phi -> phiMean, D0 -> Dm] is the one-particle-irreducible (1PI) effective action \[CapitalGamma][phiMean] of a real boson to order n, the Legendre transform of -log Z with respect to the source of phi (the quantum effective action of Jona-Lasinio, the generating functional of the 1PI vertex functions; at constant phiMean the volume times the effective potential, Coleman-Weinberg at one loop): \[CapitalGamma] = S[phiMean] + 1/2 Tr ln S''[phiMean] + \[CapitalGamma]1[phiMean], with S the classical action, S''[phiMean] = D^-1[phiMean] = D0^-1 + S_int''[phiMean] its second derivative, the fluctuation operator in the background, and \[CapitalGamma]1, defined by this equation as \[CapitalGamma]2 is by the 2PI action, minus the connected 1PI vacuum diagrams with two or more loops, with lines Dm = D[phiMean] = S''[phiMean]^-1, the propagator in the background, and the vertices of S_int[phiMean + phi] of degree three or more in phi. The derivative of Dm with respect to phiMean is declared (DeclareFunctionalDerivative: dDm(x, y)/dphiMean(a) = -Integral Dm(x, u) S_int'''[phiMean](u, v, a) Dm(v, y)). FunctionalDerivative with respect to phiMean gives the exact equation for the mean field and, at phiMean = 0 (Dm = D0 when the interaction has no quadratic part), the vertex functions VertexFunction of ConnectedFunctionFromVertices: \[CapitalGamma]^(2) = D0^-1 - \[CapitalSigma] and \[CapitalGamma]^(n) = -(amputated 1PI n-point function), whose skeletons are PerturbativeFullVertex: SetSourcesToZero[FunctionalDerivative[Gamma, phiMean[a], phiMean[b]]] /. Dm -> D0, then ExpandTrace (which contracts Dm . Dm^-1). With the lines Dm a derivative brings a coupling (dDm/dphiMean does), so that the derivatives of the action of order n hold terms of higher orders, not all of them complete: keep the orders up to n (OrderPart). It is the 2PI effective action EffectiveAction2PI[Z0, interaction, n, D0 -> G, \"Statistics\" -> \"Boson\", \"MeanFields\" -> {phi -> phiMean}] at its stationary G[phiMean]; at G = Dm its \[CapitalGamma]2[phiMean, Dm] is the part of \[CapitalGamma]1 2PI in the lines Dm. With bare lines, D0 -> D0, the vertices of degree two in the fluctuation stay vertices: S[phiMean] + 1/2 Tr ln D0^-1 - (connected 1PI vacuum diagrams with lines D0 and the vertices of S_int[phiMean + phi] of degree two or more), the same series written in D0: its diagrams with one loop are the rings of 1/2 Tr ln S''[phiMean] - 1/2 Tr ln D0^-1, those with two or more \[CapitalGamma]1 with its lines Dm expanded in D0. For fermions, EffectiveAction1PI[Z0, interaction, n, {psi -> Psi, psibar -> Psibar}, G0 -> G0] is \[CapitalGamma][Psibar, Psi] = S[Psibar, Psi] - 1/2 Tr ln(-S''[Psibar, Psi]) + \[CapitalGamma]1[Psibar, Psi], the same form with the signs of a Grassmann integral: S'' is the Nambu matrix of the second derivatives of S with respect to (Psi, Psibar), the trace runs over its components, its normal block is D^-1[Psibar, Psi](x, y) = FunctionalDerivative[S, Psibar[x], Psi[y]] (G0^-1 minus the Hartree and Fock potentials of the means) and its anomalous blocks are FunctionalDerivative[S, Psibar[x], Psibar[y]] = V(x, y) Psi(x) Psi(y) and FunctionalDerivative[S, Psi[x], Psi[y]] = V(x, y) Psibar(x) Psibar(y), from the vertices V Psi Psi psibar psibar and V Psibar Psibar psi psi; \[CapitalGamma]1 has the lines S''[Psibar, Psi]^-1, the Nambu propagator in the background, with anomalous blocks. Such a line is not one directed line, and the action is computed with bare lines, its only form: S[Psibar, Psi] - Tr ln(-G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of S_int[Psibar + psibar, Psi + psi] of degree two or more, the anomalous ones included), whose diagrams with one loop are the rings of -1/2 Tr ln(-S''[Psibar, Psi]) + Tr ln(-G0^-1) and those with two or more \[CapitalGamma]1 with its lines expanded in G0. The means are declared as Grassmann sources; at zero means FunctionalDerivative[Gamma, Psibar[a], Psi[b]] (the rightmost derivative acting first) is G0^-1(a, b) - \[CapitalSigma](a, b) (the perturbative self-energy), and FunctionalDerivative[Gamma, Psibar[x1], Psi[x2], Psibar[x3], Psi[x4]] the full vertex F(x1, x2; x3, x4) with bare lines, whose skeletons are PerturbativeFullVertex. For a superfield, EffectiveAction1PI[Z0, interaction, n, NambuField -> X, G0 -> G0, \"Statistics\" -> \"Superfield\"] is \[CapitalGamma][X] = S[X] - 1/2 Tr ln(-S''[X]) + \[CapitalGamma]1[X] (S[X] = -1/2 X G0^-1 X + S_int[X], \[CapitalGamma]1 with the lines S''[X]^-1) with bare lines, S[X] - 1/2 Tr ln(-G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of degree two or more); its components (SuperfieldComponents) are \[CapitalGamma][Psibar, Psi], and it is the superfield EffectiveAction2PI at its stationary propagator. Other lines for fermions and superfields, a single Grassmann field or complex bosons give the message onepi and $Failed. EffectiveAction2PI with \"MeanFields\" -> {psi -> Psi, psibar -> Psibar} at its stationary G is the Nambu-Gor'kov functional at zero anomalous propagator: to third order it differs from \[CapitalGamma][Psibar, Psi] by the diagrams with an anomalous vertex, which the superfield functional recovers.";
TraceLog::usage = "TraceLog[A] is Tr ln A, for an operator A: kernels (symbols h standing for h[x, y], or LabelFunction objects), numbers (times the identity), sums, coefficients times operators, products A . B (Dot), integer powers and inverses A^-1 (Inverse[A] is read as A^-1). Write A^(-1) . B: Mathematica reads A^-1 . B as A^(-(1 . B)), which is taken as A^(-1) . B with the message opdotpower. A source enters an operator through a kernel: a LabelFunction (the one-loop action 1/2 TraceLog[D0^-1 + LabelFunction[lambda/2 phibar[u]^2 DeltaFunction[u, v], {u, v}]] is differentiated with respect to phibar) or a head with a declared derivative; a source outside any kernel gives the message opsource. A single symbol is a kernel; in a product with Times the factor that contains a declared kernel (DeclareDirectedPropagator, DeclareSymmetric, a source, the variable of the derivative) is the operator and the others are coefficients (lambda S with S declared); when this cannot be told (lambda S with neither declared) a message is given and the result is $Failed. The kernel of A . B is Integral A(x, u) B(u, y) and Tr(A . B) = Integral A(x, y) B(y, x). FunctionalDerivative differentiates it (\[Delta] Tr ln A = Tr(A^-1 \[Delta]A)); ExpandTrace expands it around a bare propagator; ZeroDimensionalValue evaluates it in zero dimensions.";
OperatorTrace::usage = "OperatorTrace[A] is the trace Tr A of an operator A (see TraceLog); OperatorTrace[1] is the trace of the identity. ExpandTrace writes the trace of a product of kernels as a ring of diagrams.";
OperatorKernel::usage = "OperatorKernel[A][x, y] is the kernel A(x, y) of an operator A that is not a single kernel, e.g. OperatorKernel[G^-1][x, y] = G^-1(x, y) (symmetric when G is). ExpandTrace writes it as diagrams and contracts Integral G^-1(a, u) G(u, b) to \[Delta](a, b).";
ExpandTrace::usage = "ExpandTrace[expr] writes the traces (OperatorTrace), kernels (OperatorKernel) and universal functionals (LegendreTransform, as Phi[G] + \[Kappa] Tr(S . G)) in expr as diagrams, with the products of inverse kernels contracted (Integral G^-1(a, u) G(u, b) du = \[Delta](a, b)), and brings the result to canonical form. ExpandTrace[expr, G0, n] also expands around the bare propagator G0, to n insertions: Tr ln(c (G0^-1 - X)) = Tr ln(c G0^-1) - Sum_k Tr((G0 X)^k)/k (a series of rings) and (c (G0^-1 - X))^-1 = (1/c) Sum_k (G0 X)^k G0 (the Dyson series); a propagator replaced by an operator, e.g. G -> (G0^-1 - S)^-1 with S = LabelFunction[sigma, {a, b}], is expanded in the diagrams too. The option \"MaximumOrder\" -> {V, m} keeps the terms of order m or less in V (a coupling symbol or the head of the interaction lines), truncating the products as they are formed.";
LegendreTransform::usage = "LegendreTransform[Phi, G -> S] is Potthoff's universal functional \[ScriptCapitalF][S] = Phi[G[S]] - Tr(S G[S]) (fermions; + 1/2 Tr(S G) for real bosons, \"Statistics\" -> \"Boson\", + Tr(S G) for complex bosons), the Legendre transform of the Luttinger-Ward functional Phi from G to the self-energy S: G[S] is the propagator at which SelfEnergy[Phi, G] equals S. It is inert: FunctionalDerivative with respect to S[x, y] gives -G[S](y, x) (fermions; 1/2 G[S], G[S] for bosons), G[S] being LegendrePropagator; with respect to a kernel of Phi other than G, e.g. an interaction V[x, y], it gives \[Delta]Phi/\[Delta]V at G = G[S] (\[ScriptCapitalF] is stationary in G); with respect to G it gives 0. ExpandTrace and ZeroDimensionalValue write it out as Phi[G] - Tr(S . G) (fermions; + 1/2 Tr(S . G) for real bosons, + Tr(S . G) for complex bosons), and G[S] as G. It depends on the interaction only, not on G0: the diagrams of Phi contain only G and the interaction.";
LegendrePropagator::usage = "LegendrePropagator[G -> S, {V, ...}][x, y] is G[S](x, y), the propagator at which the self-energy of a Luttinger-Ward functional Phi (of G and of the kernels V, ...) equals S: the stationary point in G of Potthoff's universal functional \[ScriptCapitalF][S] = LegendreTransform[Phi, G -> S], whose derivative with respect to S is written with it. It is shown as G[S](x, y), and ExpandTrace and ZeroDimensionalValue write it as G(x, y) (G -> g(S) among the rules of ZeroDimensionalValue). Its derivatives are not available (\[Delta]G[S]/\[Delta]S is the inverse of the irreducible vertex, (\[Delta]\[CapitalSigma]/\[Delta]G)^-1, \[Delta]G[S]/\[Delta]V needs it too): FunctionalDerivative gives a message and the terms that need one are $Failed, as are the second derivatives of \[ScriptCapitalF] and of SelfEnergyFunctional with respect to S.";
SelfEnergyFunctional::usage = "SelfEnergyFunctional[Phi, G0 -> S, G] is Potthoff's self-energy functional \[CapitalOmega]_G0[S] = \[ScriptCapitalF][S] - Tr ln(-(G0^-1 - S)) for fermions (\[ScriptCapitalF][S] + 1/2 Tr ln(D0^-1 - S) for real bosons, \[ScriptCapitalF][S] + Tr ln(G0^-1 - S) for complex bosons; option \"Statistics\"), with \[ScriptCapitalF] = LegendreTransform[Phi, G -> S] the universal functional. G0 enters only through the trace-log (Potthoff writes \[CapitalOmega]_t[S]: his t, the one-body part, is what G0^-1 contains besides the frequencies), so that \[ScriptCapitalF] is the same for every system with the same interaction and can be computed on a simpler reference system (the idea of the variational cluster approximation and of DMFT): \[CapitalOmega]_G0[S] is stationary at the physical self-energy, FunctionalDerivative with respect to S[x, y] being (G0^-1 - S)^-1(y, x) - G[S](y, x) (Dyson), and equal to -log Z there. G[S] is (G0^-1 - S)^-1 only at the stationary point: \[CapitalOmega]_G0[S] is a Legendre transform, not the substitution G -> (G0^-1 - S)^-1 in \[CapitalGamma][G] (EffectiveAction2PI), which is stationary at the same point but has G0 also inside Phi, and is therefore not universal.";
ZeroDimensionalValue::usage = "ZeroDimensionalValue[expr, {G -> g, V -> u, ...}] is the value of expr in zero dimensions (one point, possibly with internal states): integrals dropped, DeltaFunction -> 1, every kernel h[x, y] replaced by its value, and a factor n (option \"LoopFactor\" -> n, e.g. 2s + 1; default 1) for every closed loop of kernels (ClosedLoopCount; kernels of operators, OperatorKernel[A][x, y], included, so that Integral A(x, y) B(y, x) counts as the trace Tr(A . B)) and every trace: OperatorTrace[A] -> n a and TraceLog[A] -> Log[a^n] (n Log[a] for a symbolic n), a the value of the operator A (products of numbers). LegendreTransform[Phi, G -> S] is written out as Phi[G] - Tr(S . G) for fermions (+ 1/2 Tr(S . G) for real bosons, + Tr(S . G) for complex bosons), with G -> g(S) given among the rules. The option \"Loops\" -> {G, ...} gives the heads of the lines whose loops are counted (default Automatic: every two-label kernel of expr that is neither symmetric nor an interaction line, i.e. propagators, self-energies and one-body sources, and the kernels of operators made of them). It is the exact check of the weights of the diagrams on a zero-dimensional model whose integral can be done. When a value is a list, an array of rank the number of labels of its kernel (e.g. the 4 x 4 Nambu matrix of one site with spin 1/2, NambuField = (psiUp, psiDn, psibarUp, psibarDn)), the labels run over the internal states: DeltaFunction is the identity, integrals are sums, operators are matrices (numbers times the identity), Tr ln A = Log[Det[A]], Tr A = Tr[A], OperatorKernel[A][x, y] the entry, and \"LoopFactor\" is not used; \"Indices\" -> {x -> i, ...} gives the internal states of the free labels.";

(* ------------------------------------------------------------- the package *)
FunctionalDiagramsUpdateCheck::usage = "FunctionalDiagramsUpdateCheck[] asks GitHub (api.github.com) for the latest release of FunctionalDiagrams and gives <|\"InstalledVersion\" -> ..., \"LatestVersion\" -> ..., \"InstallLine\" -> ...|>, the version loaded, the latest release and the PacletInstall line of its paclet; when the latest release is newer, it also prints that line. It gives $Failed, with a message, when the release cannot be read. Loading the package in a notebook does the same check by itself, at most once a day (see $FunctionalDiagramsUpdateCheck).";
$FunctionalDiagramsUpdateCheck::usage = "$FunctionalDiagramsUpdateCheck is True (the default) when loading the package in a notebook checks for a newer release: at most once a day, in the background, it asks GitHub (api.github.com) for the latest release and, if it is newer than $FunctionalDiagramsVersion, adds to the banner the PacletInstall line of that release; any failure is silent, and scripts never check. Set $FunctionalDiagramsUpdateCheck = False before Needs[\"FunctionalDiagrams`\"] (in init.m to turn it off for good) to load without it. FunctionalDiagramsUpdateCheck[] checks by hand.";

Begin["`Private`"];

$FunctionalDiagramsVersion = "2.4.0";

(* ========================================================================= *)
(*  1. Notation and declarations                                             *)
(* ========================================================================= *)

SetAttributes[DeltaFunction, Orderless];

(* displayed forms, in StandardForm and TraditionalForm: Interpretation keeps them copyable as input. They are not
   given for OutputForm (a terminal session), where Interpretation is not a typesetting construct and its second
   argument would be formatted again, endlessly: there the expressions print as input *)
SetAttributes[defineDisplayForm, HoldAll];
defineDisplayForm[lhs_, rhs_] := (Format[lhs, StandardForm] := rhs; Format[lhs, TraditionalForm] := rhs;);
defineDisplayForm[DeltaFunction[x_, y_], Interpretation[Subscript["\[Delta]", Row[{x, y}]], DeltaFunction[x, y]]];
defineDisplayForm[GrassmannProduct[a__], Interpretation[Row[{a}, "\[CenterDot]"], GrassmannProduct[a]]];
defineDisplayForm[IntegralOver[f_, v_List], Interpretation[Row[{Subscript["\[Integral]", Row[v, ","]], " ", f}], IntegralOver[f, v]]];
defineDisplayForm[DummyVariable[depth_Integer, k_Integer],
  Interpretation[Subscript[{"z", "w", "u", "v", "t"}[[Min[depth, 5]]], k], DummyVariable[depth, k]]];
Format[DummyVariable[depth_Integer, k_Integer], OutputForm] := {"z", "w", "u", "v", "t"}[[Min[depth, 5]]] <> ToString[k];

(* a single integration variable may be given without braces, as in Integrate *)
IntegralOver[f_, v_Symbol] := IntegralOver[f, {v}];

(* ---- the declaration registry ---- *)
$bosonicSources = {}; $grassmannSources = {}; $grassmannOddHeads = {}; $gradedHeads = {};
$symmetricHeads = {}; $antisymmetricHeads = {}; $derivativeRules = {}; $derivativePairs = {};
$discreteIndexValues = {}; $fieldSourceMap = {}; $lineStyles = {}; $displayNames = {}; $localVertexHeads = {};
$blobDeclarations = {}; $interactionLineHeads = {}; $couplingSymbols = {};

(* the symmetries are undone too: the attribute Orderless of symmetric heads and the sign rules of antisymmetric ones *)
ResetDeclarations[] := (
   (* the rules of the tree expansion (ConnectedFunctionFromVertices: FullPropagator, VertexFunction), section 13 *)
   clearTreeRules[];
   Scan[ClearAttributes[#, Orderless] &, $symmetricHeads];
   Scan[(DownValues[#] = Select[DownValues[#], FreeQ[#, hasDuplicatesQ | permutationSign] &]) &, $antisymmetricHeads];
   $bosonicSources = {}; $grassmannSources = {}; $grassmannOddHeads = {}; $gradedHeads = {};
   $symmetricHeads = {}; $antisymmetricHeads = {}; $derivativeRules = {}; $derivativePairs = {};
   $discreteIndexValues = {}; $fieldSourceMap = {}; $lineStyles = {}; $displayNames = {}; $localVertexHeads = {}; $blobDeclarations = {};
   $interactionLineHeads = {}; $couplingSymbols = {}; $contractionCache = {}; $matsubaraFrequencies = {};
   $superfieldTraceComponents = {};
   defineTreeDisplayForms[];);

(* a warning for heads that are built-in symbols (for instance GreenFunction in Mathematica 13 and later): their own
   definitions and messages interfere with the package *)
FunctionalDiagrams::builtin = "`1` is a built-in symbol of the Wolfram Language, whose own definitions interfere with the package: use another name.";
warnIfBuiltin[h_Symbol] := If[Context[h] === "System`", Message[FunctionalDiagrams::builtin, h]];
warnIfBuiltin[_] := Null;

DeclareSource[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $bosonicSources = Union[$bosonicSources, {heads}];);
(* fields declared before their Grassmann source become odd now: the order of the declarations does not matter *)
DeclareGrassmannSource[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $grassmannSources = Union[$grassmannSources, {heads}];
   Scan[If[MemberQ[{heads}, #[[2]]], DeclareGrassmannOdd[#[[1]]]] &, $fieldSourceMap]; $contractionCache = {};);
DeclareGrassmannOdd[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $grassmannOddHeads = Union[$grassmannOddHeads, {heads}];);
DeclareGrassmannGraded[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $gradedHeads = Union[$gradedHeads, {heads}];);

DeclareSymmetric[heads__Symbol] := Scan[(warnIfBuiltin[#]; SetAttributes[#, Orderless];
     $symmetricHeads = Union[$symmetricHeads, {#}]) &, {heads}];

DeclareAntisymmetric[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; Scan[defineAntisymmetric, {heads}]);
defineAntisymmetric[head_Symbol] := (
   head[args___] /; hasDuplicatesQ[{args}] := 0;
   head[args___] /; Sort[{args}] =!= {args} := permutationSign[{args}] (head @@ Sort[{args}]);
   $antisymmetricHeads = Union[$antisymmetricHeads, {head}];);

(* discrete index values: the test uses SameQ, so that a pattern such as x_ is never taken for an index value *)
discreteIndexValueQ[x_] := AnyTrue[$discreteIndexValues, # === x &];
DeclareDiscreteIndexValues[values_List] := (
   $discreteIndexValues = Union[$discreteIndexValues, values];
   DeltaFunction[x_, y_] /; discreteIndexValueQ[x] && discreteIndexValueQ[y] := If[x === y, 1, 0];);

DeclareField[field_Symbol, source_Symbol, sign_ : 1] := (
   warnIfBuiltin[field];
   $fieldSourceMap = Append[DeleteCases[$fieldSourceMap, {field, _, _}], {field, source, sign}];
   If[MemberQ[$grassmannSources, source], DeclareGrassmannOdd[field]];
   $contractionCache = {};);

SetAttributes[DeclareFunctionalDerivative, HoldAll];
DeclareFunctionalDerivative[object_, source_, rhs_] := Module[{held = Hold[rhs], variables, new},
   variables = Union[Flatten[Cases[held, IntegralOver[_, v_List] :> v, Infinity]]];
   new = Table[Unique["FunctionalDiagrams`Private`d"], {Length[variables]}];
   held = held /. Thread[variables -> new];
   $derivativeRules = Append[$derivativeRules, RuleDelayed @@ Join[Hold[{object, source}], held]];
   $derivativePairs = Union[$derivativePairs, {{Head[object], Head[source]}}];];

$allowedLineStyles = {"Plain", "Arrow", "DoubleArrow", "DottedArrow", "Double", "Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted",
   "Zigzag", "Impurity", "DivergingArrows", "ConvergingArrows"};
DeclareLineStyle::style = "Unknown line style `1`; use one of `2`.";
DeclareLineStyle[head_Symbol, style_String] := If[! MemberQ[$allowedLineStyles, style],
   Message[DeclareLineStyle::style, style, $allowedLineStyles],
   warnIfBuiltin[head]; $lineStyles = Append[DeleteCases[$lineStyles, {head, _}], {head, style}];];
DeclareDirectedPropagator[heads__Symbol] := Scan[DeclareLineStyle[#, "Arrow"] &, {heads}];
(* interaction lines: kernels of S_int, drawn wavy (or in another style); in the weight written with a picture each one
   stands for minus its kernel *)
DeclareInteractionLine[heads__Symbol] := DeclareInteractionLine[heads, "Wavy"];
DeclareInteractionLine[heads__Symbol, style_String] := Scan[(DeclareLineStyle[#, style];
     If[MemberQ[$allowedLineStyles, style], $interactionLineHeads = Union[$interactionLineHeads, {#}]]) &, {heads}];
(* couplings: symbols such as lambda in S_int = lambda/4! Integral phi^4, absorbed into the vertices of the pictures *)
DeclareCoupling[couplings__Symbol] := (Scan[warnIfBuiltin, {couplings}]; $couplingSymbols = Union[$couplingSymbols, {couplings}];);

DeclareDisplayName[head_Symbol, name_] := If[Context[head] === "System`", warnIfBuiltin[head],
   $displayNames = Append[DeleteCases[$displayNames, {head, _}], {head, name}];
   defineDisplayForm[head[args___], Interpretation[Subscript[name, Row[{args}]], head[args]]];];

lineStyleOf[head_] := With[{m = Select[$lineStyles, #[[1]] === head &]}, If[m === {}, "Plain", m[[1, 2]]]];
directedStyleQ[style_] := MemberQ[{"Arrow", "DoubleArrow", "DottedArrow"}, style];
(* anomalous propagators: -<T psi(x) psi(y)> with two arrowheads pointing to its ends ("DivergingArrows": an arrow into
   each psi end), -<T psibar(x) psibar(y)> with two pointing to its middle ("ConvergingArrows"); with them the arrows run
   on through every vertex and turn only inside an anomalous line *)
anomalousStyleQ[style_] := MemberQ[{"DivergingArrows", "ConvergingArrows"}, style];
fermionStyleQ[style_] := directedStyleQ[style] || anomalousStyleQ[style];
interactionStyleQ[style_] := MemberQ[{"Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag", "Impurity"}, style];

(* local vertices: kernels such as the three-gluon vertex V3(x, y, z), whose labels carry different indices (Lorentz,
   colour) at one point of space-time; a picture draws them as one point, not as a blob *)
DeclareLocalVertex[heads__Symbol] := (Scan[warnIfBuiltin, {heads}]; $localVertexHeads = Union[$localVertexHeads, {heads}];);
localVertexQ[f_] := ! AtomQ[f] && MemberQ[$localVertexHeads, Head[f]];

(* blobs: factors h[x1, ..., xn] drawn as a circle or a square with a label inside (a LaTeX string), even for n = 2, e.g.
   a self-energy; {head, label, shape, fill, axial ports} *)
$blobShapes = {"Circle", "Box", "Triangle", "TriangleUp", "Diamond"}; $blobFills = {"Hatched", "CrossHatched", "Dotted", "Grey", "White"};
DeclareBlob::opt = "Unknown value `1` of the option `2` of DeclareBlob; use one of `3`.";
DeclareBlob[head_Symbol, label_String, opts___Rule] := Module[
   {shape = optionValue[{opts}, {"Shape" -> "Circle"}, "Shape"], fill = optionValue[{opts}, {"Fill" -> "Hatched"}, "Fill"],
    axial = optionValue[{opts}, {"AxialPorts" -> Automatic}, "AxialPorts"]},
   Which[
    ! MemberQ[$blobShapes, shape], Message[DeclareBlob::opt, shape, "Shape", $blobShapes],
    ! MemberQ[$blobFills, fill], Message[DeclareBlob::opt, fill, "Fill", $blobFills],
    True, warnIfBuiltin[head];
    $blobDeclarations = Append[DeleteCases[$blobDeclarations, {head, ___}],
      {head, label, shape, fill, Replace[axial, Automatic :> Switch[shape, "TriangleUp", {1, 2}, "Diamond", {1, 3}, _, {}]]}]];];
declaredBlobHeadQ[h_] := MemberQ[Cases[$blobDeclarations, {g_, ___} :> g], h];
declaredBlobQ[f_] := ! AtomQ[f] && declaredBlobHeadQ[Head[f]];

(* ========================================================================= *)
(*  2. Small utilities                                                       *)
(* ========================================================================= *)

hasDuplicatesQ[list_List] := Length[Union[list]] < Length[list];

(* sign of the permutation performed by Sort: Ordering gives the positions in sorted order, ties by position *)
permutationSign[list_List] := Signature[Ordering[list]];

(* sort a list of Grassmann-odd atoms: {sign, sorted list}; sign 0 if an atom repeats (nilpotency) *)
sortGrassmannAtoms[list_List] := If[hasDuplicatesQ[list], {0, list}, {permutationSign[list], Sort[list]}];

factorList[c_] := If[Head[c] === Times, List @@ c, {c}];

(* the components merged with every set that meets them, one set after the other (the classes of the equivalence that
   the sets generate, when the components start as singletons); Complement sorts, the merged component comes last *)
mergeOverlapping[components_List, sets_List] := Fold[Function[{cs, s}, With[{hit = Select[cs, Intersection[#, s] =!= {} &]},
     Append[Complement[cs, hit], Union[Flatten[{hit, s}]]]]], components, sets];
numericFactor[e_] := Which[NumberQ[e], e, Head[e] === Times, Times @@ Select[List @@ e, NumberQ], True, 1];

sourceHeads[] := Join[$bosonicSources, $grassmannSources];
sourceAtomQ[a_] := ! AtomQ[a] && MemberQ[sourceHeads[], Head[a]];
grassmannSourceQ[s_] := ! AtomQ[s] && MemberQ[$grassmannSources, Head[s]];

(* Grassmann-odd atoms: Grassmann sources, declared odd quantities, graded objects with an odd number of arguments *)
grassmannOddAtomQ[a_] := ! AtomQ[a] && (MemberQ[$grassmannSources, Head[a]] || MemberQ[$grassmannOddHeads, Head[a]] ||
      (MemberQ[$gradedHeads, Head[a]] && OddQ[Length[a]]));

(* atoms that vanish at zero sources when they are odd *)
sourceDependentOddAtomQ[a_] := MemberQ[$grassmannSources, Head[a]] || MemberQ[$gradedHeads, Head[a]];

headsDependingOn[source_] := Union[{Head[source]}, Cases[$derivativePairs, {h_, Head[source]} :> h]];
dependsOnSourceQ[f_, source_] := Or @@ (! FreeQ[f, #[___]] & /@ headsDependingOn[source]);
declaredDerivativeAtomQ[f_] := ! AtomQ[f] && MemberQ[$derivativePairs[[All, 1]], Head[f]];

freshSymbols[n_Integer] := Table[Unique["FunctionalDiagrams`Private`x"], {n}];

(* option lookup that accepts strings and symbols as option names *)
optionKey[k_String] := k;
optionKey[k_Symbol] := SymbolName[k];
optionKey[k_] := ToString[k];
(* the first rule whose key is name (a string, a symbol of that name, or anything whose ToString is name), a string key
   tested first *)
optionValue[options_List, defaults_List, name_String] := With[{r = FirstCase[Flatten[options],
      rule : (Rule | RuleDelayed)[k_, _] /; (k === name || (! StringQ[k] && optionKey[k] === name)) :> rule, None, {1}]},
   If[r === None, name /. defaults, r[[2]]]];

(* ========================================================================= *)
(*  3. Terms:  diagramTerm[c, odds, dummies] = IntegralOver[c * GrassmannProduct @@ odds, dummies] *)
(* ========================================================================= *)

unitTermList = {diagramTerm[1, {}, {}]};

multiplyTwoTerms[diagramTerm[c1_, o1_, d1_], diagramTerm[c2_, o2_, d2_]] := diagramTerm[c1 c2, Join[o1, o2], Join[d1, d2]];
multiplyTermLists[l1_List, l2_List] := Flatten[Table[multiplyTwoTerms[a, b], {a, l1}, {b, l2}], 1];
multiplyTermListsInOrder[lists_List] := Fold[multiplyTermLists, unitTermList, lists];

toTermList[0] := {};
(* a Grassmann product with a zero factor vanishes *)
toTermList[(NonCommutativeMultiply | GrassmannProduct)[___, 0, ___]] := {};
toTermList[e_Plus] := Join @@ (toTermList /@ (List @@ e));
(* Grassmann-odd factors multiplied with Times have lost their order, and with it the sign of the result: the
   evaluation is aborted, rather than giving a result with a sign that may be wrong *)
(* a product of plain factors (plainFactorQ, below) is one term *)
toTermList[e_Times] := If[plainFactorsQ[List @@ e], {diagramTerm[e, {}, {}]},
   Module[{parts = toTermList /@ (List @@ e)},
    If[Count[parts, l_ /; AnyTrue[l, OddQ[Length[#[[2]]]] &]] > 1, Message[FunctionalDerivative::times, userForm[e]]; Abort[]];
    multiplyTermListsInOrder[parts]]];
toTermList[NonCommutativeMultiply[a___]] := multiplyTermListsInOrder[toTermList /@ {a}];
toTermList[GrassmannProduct[a___]] := multiplyTermListsInOrder[toTermList /@ {a}];
(* the integration variables are renamed apart; a nested integral over a variable of the same name (an expression in
   canonical form substituted into another) keeps its own *)
scopedReplace[f_, {}] := f;
scopedReplace[f_, rules_List] := f /. Join[{IntegralOver[g_, w_List] :> IntegralOver[scopedReplace[g, Select[rules, ! MemberQ[w, #[[1]]] &]], w]},
    rules];
toTermList[IntegralOver[f_, v_List]] := Module[{new = freshSymbols[Length[v]]},
   (diagramTerm[#[[1]], #[[2]], Join[new, #[[3]]]] &) /@
    toTermList[If[FreeQ[f, IntegralOver], f /. Thread[v -> new], scopedReplace[f, Thread[v -> new]]]]];
(* a Grassmann-odd atom is nilpotent (Times folds eta[x] eta[x] into a power) *)
toTermList[Power[b_?grassmannOddAtomQ, n_Integer]] /; n > 1 := {};
toTermList[Power[b_, n_Integer?Positive]] /; (Head[b] === Plus || Head[b] === IntegralOver || Head[b] === NonCommutativeMultiply ||
     Head[b] === GrassmannProduct) :=
  multiplyTermListsInOrder[Table[toTermList[b], {n}]];
toTermList[a_?grassmannOddAtomQ] := {diagramTerm[1, {a}, {}]};
toTermList[e_] := {diagramTerm[e, {}, {}]};
(* a plain factor f is one with toTermList[f] === {diagramTerm[f, {}, {}]} (the last rule above): a product of plain
   factors is one term, without a toTermList of every factor *)
$compositeHeads = {Plus, Times, NonCommutativeMultiply, GrassmannProduct, IntegralOver};
plainFactorQ[f_] := AtomQ[f] || ! (MemberQ[$compositeHeads, Head[f]] || grassmannOddAtomQ[f] ||
     (Head[f] === Power && (grassmannOddAtomQ[f[[1]]] || MemberQ[$compositeHeads, Head[f[[1]]]])));
(* the same for a list of factors at once (graded heads count as not plain, whatever the number of their arguments: the
   general route is then taken, with the same result) *)
plainFactorsQ[factors_List] := With[{special = Join[$compositeHeads, $grassmannSources, $grassmannOddHeads, $gradedHeads]},
   Intersection[Head /@ factors, special] === {} &&
    Intersection[Cases[factors, Power[b_?(! AtomQ[#] &), _] :> Head[b]], special] === {}];

FunctionalDerivative::times = "Grassmann-odd factors are multiplied with Times in `1`: Times sorts its factors, so that their order and the sign are lost. Write the product with ** ; the evaluation is aborted.";
(* an expression for messages: the copies of the user's kernels that derivatives are taken with respect to by their
   user's names (G -> g$123 in derivativeWithRespectToPropagator, $kernelCopies), the integration variables renamed by
   the package without their context *)
userForm[e_] := (e /. $kernelCopies) /. s_Symbol /; Context[s] === "FunctionalDiagrams`Private`" :> SymbolName[s];

grassmannProductOrOne[{}] := 1;
grassmannProductOrOne[o_List] := GrassmannProduct @@ o;

termToExpression[diagramTerm[c_, o_, {}]] := c grassmannProductOrOne[o];
termToExpression[diagramTerm[c_, o_, d_]] := Module[{factors = factorList[c], outside, inside},
   outside = Select[factors, FreeQ[#, Alternatives @@ d] &];
   inside = Select[factors, ! FreeQ[#, Alternatives @@ d] &];
   (Times @@ outside) IntegralOver[(Times @@ inside) grassmannProductOrOne[o], d]];
termListToExpression[l_List] := Plus @@ (termToExpression /@ l);

(* ========================================================================= *)
(*  4. Integration of delta functions                                        *)
(* ========================================================================= *)

findIntegrableDelta[c_, d_] := Module[{factors = factorList[c]}, Catch[
    Do[With[{f = factors[[i]]}, With[{g = If[Head[f] === Power && Head[f[[1]]] === DeltaFunction, f[[1]], f]},
       If[Head[g] === DeltaFunction && Length[g] == 2 && g[[1]] =!= g[[2]],
        If[MemberQ[d, g[[1]]], Throw[{i, g[[1]], g[[2]]}]];
        If[MemberQ[d, g[[2]]], Throw[{i, g[[2]], g[[1]]}]]]]], {i, Length[factors]}];
    None]];

(* a term without delta functions is left as it is. The deltas are integrated one at a time, on the product of the
   deltas alone: their relative order is the same as in the whole product (Times sorts its factors by one total order,
   and a delta combines with nothing but a delta), so the substitutions x -> y are those of integrating them in the whole
   term, composed into one list of rules applied once to the other factors and the odd atoms. A term that vanishes is
   integrated as before, one substitution at a time in the whole term, which stops where it vanishes *)
deltaFactorQ[f_] := Head[f] === DeltaFunction || (Head[f] === Power && Head[f[[1]]] === DeltaFunction);
integrateDeltaFunctions[t : diagramTerm[c0_, o0_, d0_]] := If[FreeQ[c0, DeltaFunction], t,
   Module[{factors = factorList[c0], deltaMask, cd, d = d0, found, fs, f, i, x, y, rules = {}, c},
    deltaMask = deltaFactorQ /@ factors;
    cd = Times @@ Pick[factors, deltaMask];
    While[(found = findIntegrableDelta[cd, d]) =!= None,
     {i, x, y} = found; fs = factorList[cd]; f = fs[[i]];
     cd = (Times @@ Delete[fs, i]) If[Head[f] === Power, f[[1]]^(f[[2]] - 1), 1];
     cd = cd /. x -> y; rules = Append[rules /. x -> y, x -> y]; d = DeleteCases[d, x]];
    If[rules === {}, normalizeExternalDeltas[t],
     c = If[cd === 0, 0, cd ((Times @@ Pick[factors, deltaMask, False]) /. rules)];
     If[c === 0, integrateDeltaFunctionsInTerm[t], normalizeExternalDeltas[diagramTerm[c, o0 /. rules, d]]]]]];
integrateDeltaFunctionsInTerm[diagramTerm[c0_, o0_, d0_]] := Module[{c = c0, o = o0, d = d0, found, factors, f, i, x, y},
   While[(found = findIntegrableDelta[c, d]) =!= None,
    {i, x, y} = found; factors = factorList[c]; f = factors[[i]];
    c = (Times @@ Delete[factors, i]) If[Head[f] === Power, f[[1]]^(f[[2]] - 1), 1];
    c = c /. x -> y; o = o /. x -> y; d = DeleteCases[d, x]];
   normalizeExternalDeltas[diagramTerm[c, o, d]]];

(* Delta functions between labels that are not integrated over: the labels of a class joined by deltas are replaced,
   everywhere else in the term, by the first of them in canonical order; delta(a, b) f(a, b) = delta(a, b) f(a, a). *)
normalizeExternalDeltas[t : diagramTerm[c_, o_, d_]] := If[FreeQ[c, DeltaFunction], t, Module[{factors = expandIntegerPowers[factorList[c]], positions, classes, rules},
   positions = Select[Range[Length[factors]], Head[factors[[#]]] === DeltaFunction && Length[factors[[#]]] == 2 &&
       factors[[#, 1]] =!= factors[[#, 2]] && FreeQ[factors[[#]], Alternatives @@ d] &];
   If[positions === {}, t,
    classes = Sort /@ mergeOverlapping[{}, Table[List @@ factors[[p]], {p, positions}]];
    rules = Flatten[Table[(# -> First[cl] &) /@ Rest[cl], {cl, classes}]];
    diagramTerm[((Times @@ Delete[factors, List /@ positions]) /. rules) Times @@ Flatten[Table[DeltaFunction[First[cl], m], {cl, classes}, {m, Rest[cl]}]],
     o /. rules, d]]]];

(* ========================================================================= *)
(*  5. The functional derivative                                             *)
(* ========================================================================= *)

differentiateTermList[terms_List, source_] := Join @@ (differentiateTerm[#, source] & /@ terms);

differentiateTerm[diagramTerm[c_, o_, d_], source_] := Module[{result, sign, derivatives},
   (* derivative of the commuting part, placed to the left of the odd factors *)
   result = multiplyTermLists[differentiateCommutingPart[c, source], {diagramTerm[1, o, d]}];
   (* derivative of the odd atoms, with the Grassmann sign of a left derivative *)
   Do[derivatives = differentiateAtom[o[[k]], source];
    sign = If[grassmannSourceQ[source], (-1)^(k - 1), 1];
    result = Join[result, (diagramTerm[sign c #[[1]], Join[Take[o, k - 1], #[[2]], Drop[o, k]], Join[d, #[[3]]]] &) /@ derivatives],
    {k, Length[o]}];
   integrateDeltaFunctions /@ result];

differentiateCommutingPart[c_, source_] := Module[{factors = factorList[c], out = {}, plain},
   plain = plainFactorsQ[factors];
   Do[If[dependsOnSourceQ[factors[[i]], source],
     out = Join[out, multiplyTermLists[differentiateFactor[factors[[i]], source],
        If[plain, {diagramTerm[Times @@ Delete[factors, i], {}, {}]}, toTermList[Times @@ Delete[factors, i]]]]]],
    {i, Length[factors]}];
   out];

differentiateAtom[a_, source_] := Which[
   ! dependsOnSourceQ[a, source], {},
   sourceAtomQ[a] && Head[a] === Head[source] && Length[a] == Length[source],
   {diagramTerm[Times @@ MapThread[DeltaFunction, {List @@ a, List @@ source}], {}, {}]},
   sourceAtomQ[a], {},
   True, With[{r = Replace[{a, source}, $derivativeRules]}, If[r === {a, source}, {}, toTermList[r]]]];

differentiateFactor[f_, source_] := Which[
   ! dependsOnSourceQ[f, source], {},
   sourceAtomQ[f] || declaredDerivativeAtomQ[f], differentiateAtom[f, source],
   Head[f] === Power, differentiatePower[f[[1]], f[[2]], source],
   Head[f] === Log, With[{split = sharedExponential[f[[1]]]}, If[split === None,
     multiplyTermLists[toTermList[1/f[[1]]], differentiateTermList[toTermList[f[[1]]], source]],
     (* Log[E^X p] = X + Log[p]: the Gaussian factor of Z = Z0 + Z1 + ... is not carried through the chain rule *)
     Join[differentiateTermList[toTermList[split[[1]]], source],
      multiplyTermLists[toTermList[1/split[[2]]], differentiateTermList[toTermList[split[[2]]], source]]]]],
   MemberQ[{Plus, Times, IntegralOver, GrassmannProduct, NonCommutativeMultiply}, Head[f]], differentiateTermList[toTermList[f], source],
   True, (* any other function: chain rule *)
   Join @@ Table[If[dependsOnSourceQ[f[[i]], source],
      multiplyTermLists[toTermList[(Derivative @@ UnitVector[Length[f], i])[Head[f]] @@ (List @@ f)],
       differentiateTermList[toTermList[f[[i]]], source]], {}], {i, Length[f]}]];

(* a sum whose every term has one exponential factor E^X, the same X up to the names of its integration variables (as
   Z0 + Z1 + Z2/2 of a Gaussian Z0): {X, the sum with E^X divided out}; None otherwise *)
sharedExponential[s_Plus] := Module[{terms = With[{x = Expand[s]}, If[Head[x] === Plus, List @@ x, {x}]], xs, canon},
   xs = (Cases[If[Head[#] === Times, List @@ #, {#}], Power[E, x_] :> x] &) /@ terms;
   If[! AllTrue[xs, Length[#] == 1 &], Return[None]];
   xs = xs[[All, 1]];
   canon = SimplifyDiagrams /@ xs;
   If[Length[Union[canon]] != 1, None, {First[canon], Total[MapThread[#1/Power[E, #2] &, {terms, xs}]]}]];
sharedExponential[_] := None;

differentiatePower[b_, n_, source_] := Which[
   ! dependsOnSourceQ[n, source], multiplyTermLists[toTermList[n b^(n - 1)], differentiateTermList[toTermList[b], source]],
   ! dependsOnSourceQ[b, source], multiplyTermLists[{diagramTerm[b^n Log[b], {}, {}]}, differentiateTermList[toTermList[n], source]],
   True, differentiateFactor[Exp[n Log[b]], source]];

(* ========================================================================= *)
(*  6. Canonical form                                                        *)
(*  A colour refinement of the integration variables (each is characterised  *)
(*  by the factors it appears in, iteratively), followed by individualisation *)
(*  of the remaining ties, as in graph-isomorphism algorithms. Transpositions *)
(*  that leave a term invariant are pruned; a term that is odd under one of   *)
(*  them vanishes.                                                           *)
(* ========================================================================= *)

inertForm[e_?AtomQ] := e;
inertForm[e_] := inert[inertForm[Head[e]], inertForm /@ (List @@ e)];
(* during a canonical form (canonicalTermList) the answers are remembered in $symmetricMemo: the declarations and the
   attributes cannot change meanwhile *)
$symmetricMemo = None;
symmetricHeadQ[h_Symbol] := If[AssociationQ[$symmetricMemo], Lookup[$symmetricMemo, h, $symmetricMemo[h] = symmetricHeadQAt[h]],
   symmetricHeadQAt[h]];
symmetricHeadQ[_] := False;
symmetricHeadQAt[h_Symbol] := MemberQ[Join[{Times, Plus, DeltaFunction}, $symmetricHeads, $antisymmetricHeads], h] ||
   MemberQ[Attributes[h], Orderless];
normalizedInertForm[inert[h_, args_List]] := With[{a = normalizedInertForm /@ args}, inert[h, If[symmetricHeadQ[h], Sort[a], a]]];
normalizedInertForm[e_] := e;

(* the rank of each descriptor among the distinct ones, looked up in an association; descriptors with inexact numbers
   (machine reals that Union takes as equal may be different keys) by rules: they contain no patterns (heads and atoms
   of the factors are wrapped in inert), so that they can be the left-hand sides of rules *)
rankOfEach[descriptors_List] := With[{u = Union[descriptors]}, If[Precision[descriptors] === Infinity,
    Lookup[AssociationThread[u -> Range[Length[u]]], descriptors], Replace[descriptors, Thread[u -> Range[Length[u]]], {1}]]];

(* The data of the refinement, computed once per term: for each inert factor the variables it contains and their
   positions in d; for each variable the factors that contain it. A flat factor (an atomic head with atomic arguments,
   which most factors are) is also stored as its head, whether the head is symmetric, and a template: the indices of
   its arguments in the list of values Join[colours of d, constants]. *)
refinementData[inerts_, d_] := Module[{n = Length[d], marked, positions, flat, constants, rules},
   marked = inerts /. Thread[d -> Table[refinementVariable[j], {j, n}]];
   positions = Table[Union[Cases[f, refinementVariable[j_] :> j, {0, Infinity}]], {f, marked}];
   flat = Table[MatchQ[f, inert[_?AtomQ, {___?AtomQ}]], {f, inerts}];
   constants = Union[Select[Flatten[Table[If[flat[[k]], marked[[k, 2]], {}], {k, Length[inerts]}]], AtomQ]];
   rules = Join[{refinementVariable[j_] :> j}, Thread[constants -> n + Range[Length[constants]]]];
   {inerts, d, positions, Table[First /@ Position[positions, i, {2}], {i, n}], flat,
    Table[If[flat[[k]], {inerts[[k, 1]], symmetricHeadQ[inerts[[k, 1]]], Replace[marked[[k, 2]], rules, {1}]}, None], {k, Length[inerts]}],
    constants}];

(* The descriptor of the variable i: its colour and the factors that contain it, with i marked SELF and the other variables
   replaced by their colours (normalizedInertForm[f /. {d[[i]] -> SELF, d[[j]] -> COLOUR[colours[[j]]]}], computed directly
   for the flat factors from values = Join[COLOUR /@ colours, constants], made once per round). The descriptors are ordered
   by the colour first, so that a variable alone in its colour keeps its place whatever its factors: they are not computed
   for it. Once every variable has a colour of its own, a further round gives the same colours: the refinement stops
   without it *)
colourRefinement[{inerts_, d_, positions_, incidence_, flat_, flatData_, constants_}, colours0_] := Module[{colours = colours0, new,
    again = True, values, mine, shared},
   While[again,
    values = Join[COLOUR /@ colours, constants];
    shared = Cases[Tally[colours], {c_, m_ /; m > 1} :> c];
    new = rankOfEach[Table[If[! MemberQ[shared, colours[[i]]], {colours[[i]], {}},
        mine = values; mine[[i]] = SELF;
        {colours[[i]], Sort[Table[If[flat[[k]], With[{a = mine[[flatData[[k, 3]]]]}, inert[flatData[[k, 1]], If[flatData[[k, 2]], Sort[a], a]]],
            normalizedInertForm[inerts[[k]] /. Thread[d[[positions[[k]]]] -> mine[[positions[[k]]]]]]], {k, incidence[[i]]}]]}],
       {i, Length[colours]}]];
    again = Length[Union[new]] > Length[Union[colours]] && Length[Union[new]] < Length[new];
    colours = new];
   colours];

(* evaluated key of a term under a renaming rule: {key, coefficient} *)
termKey[c_, o_, rule_] := Module[{cRenamed = recanonicalizeNestedIntegrals[c /. rule, rule[[All, 2]]], oRenamed = o /. rule, numbers, sign, sorted},
   numbers = numericFactor /@ oRenamed;
   If[cRenamed === 0 || MemberQ[numbers, 0], {None, 0},
    {sign, sorted} = sortGrassmannAtoms[oRenamed/numbers];
    If[sign === 0, {None, 0}, {{cRenamed/numericFactor[cRenamed], sorted}, numericFactor[cRenamed] (Times @@ numbers) sign}]]];

(* the transposition of x and y is an automorphism (+1), an odd automorphism (-1), or neither (0); k1 is the key of the
   identity, termKey[c, o, {}] *)
transpositionSymmetrySign[c_, o_, x_, y_, k1_] := Module[{k2 = termKey[c, o, {x -> y, y -> x}]},
   Which[k1[[1]] =!= k2[[1]], 0, k1[[2]] === k2[[2]], 1, k1[[2]] === -k2[[2]], -1, True, 0]];

(* The search tree of the individualisation: the leaves are the orders of the variables (names: their canonical names, so
   that a leaf has a key). At a node whose target cell {m1, ..., mk} is not fully symmetric the subtree of m1 is searched;
   for each other member m only the leftmost leaf of its subtree (leftmostOnly) is computed. Refinement and
   individualisation keep the order of the cells, so the permutation between the two leftmost leaves fixes the prefix and
   maps m1 to m: if their keys and coefficients are equal it is an automorphism and the subtree of m (its image) gives the
   same keys, so it is skipped; opposite coefficients, an odd automorphism: the term vanishes. In integrated trees a leaf
   can only be exchanged together with its vertex (a product of transpositions), which the transpositions do not see.
   keys holds the keys of the term, each computed once: keys["Identity"], keys["Transposition", {x, y}] (the sign of the
   transposition of x and y), keys["Leaf", order] *)
individualizationSearch[keys_, data_, d_, colours0_, prefix_List, leftmostOnly_ : False] := Module[{colours = colourRefinement[data, colours0],
    target, members, signs, k, branch, first, firstKey, out, key, leaf, done},
   If[Length[Union[colours]] == Length[d],
    {Transpose[Sort[Transpose[{colours, d}]]][[2]]},
    target = First[Select[Union[colours], Count[colours, #] > 1 &]];
    members = Select[Range[Length[d]], colours[[#]] == target &];
    signs = Table[keys["Transposition", Sort[{d[[members[[1]]]], d[[m]]}]], {m, Rest[members]}];
    k = Length[members];
    (* the colours after individualising the member m *)
    branch = Function[m, Table[If[j == m, 2 colours[[j]] - 1, 2 colours[[j]]], {j, Length[d]}]];
    Which[
     MemberQ[signs, -1], Throw[$vanishingTerm, canonical],
     (* all transpositions with the first member are automorphisms: they generate the symmetric group of the class, so
        that the class is fully symmetric and its members are individualised at once, in one branch *)
     FreeQ[signs, 0],
     individualizationSearch[keys, data, d, Table[With[{p = Position[members, j, {1}]},
        If[p === {}, (k + 1) colours[[j]], (k + 1) colours[[j]] - (k + 1) + p[[1, 1]]]], {j, Length[d]}], Join[prefix, members], leftmostOnly],
     leftmostOnly, individualizationSearch[keys, data, d, branch[members[[1]]], Append[prefix, members[[1]]], True],
     True,
     first = individualizationSearch[keys, data, d, branch[members[[1]]], Append[prefix, members[[1]]]];
     firstKey = keys["Leaf", First[first]];
     out = first; done = {members[[1]]};
     Do[
      (* a member in the orbit of one done, under the automorphisms found so far that fix the prefix: its subtree is the
         image of one searched, with the same keys *)
      If[! MemberQ[automorphismOrbit[done, Select[keys["Automorphisms"], Function[pi, AllTrue[prefix, pi[[#]] == # &]]]], m],
       leaf = First[individualizationSearch[keys, data, d, branch[m], Append[prefix, m], True]];
       key = keys["Leaf", leaf];
       Which[
        firstKey[[2]] =!= 0 && key[[1]] === firstKey[[1]] && key[[2]] === firstKey[[2]],
        (* the permutation taking this leaf to the first one is an automorphism of the term *)
        keys["Automorphisms"] = Append[keys["Automorphisms"], leafPermutation[d, leaf, First[first]]],
        firstKey[[2]] =!= 0 && key[[1]] === firstKey[[1]] && key[[2]] === -firstKey[[2]], Throw[$vanishingTerm, canonical],
        True, out = Join[out, individualizationSearch[keys, data, d, branch[m], Append[prefix, m]]]];
       AppendTo[done, m]],
      {m, Rest[members]}];
     out]]];

(* the permutation of the positions in d that takes the leaf l1 to the leaf l2; the orbit of a set of positions under a
   list of permutations *)
leafPermutation[d_, l1_, l2_] := Module[{pi = Range[Length[d]], index},
   index = Function[v, First[First[Position[d, v, {1}, Heads -> False]]]];
   Do[pi[[index[l1[[j]]]]] = index[l2[[j]]], {j, Length[d]}];
   pi];
automorphismOrbit[start_List, perms_List] := Module[{orbit = start, new = start, images},
   While[new =!= {},
    images = Complement[Union[Flatten[Table[pi[[new]], {pi, perms}]]], orbit];
    orbit = Union[orbit, images]; new = images];
   orbit];

canonicalizeNestedIntegrals[c_, depth_] := c /. i_IntegralOver :> nestedCanonicalExpression[i, depth + 1];
(* the canonical form of a nested integral depends on the names of the outer variables it contains: it is computed
   again once they have their canonical names, so that the key of a term does not depend on the names it came with.
   Only the nested integrals that contain one of the renamed variables vars can change: the others are already in
   canonical form (canonicalizeNestedIntegrals) *)
recanonicalizeNestedIntegrals[c_, vars_List] := If[FreeQ[c, IntegralOver] || vars === {}, c,
   c /. i : IntegralOver[_, {DummyVariable[depth_, _], ___}] /; ! FreeQ[i, Alternatives @@ vars] :> nestedCanonicalExpression[i, depth]];
canonicalExpression[e_, depth_] := termListToExpression[canonicalTermList[toTermList[e], depth]];
(* the canonical form of a nested integral, computed once for each integral and depth during a canonical form at depth 1
   (the same integrals, such as the exponent of Z0 or the integrals of a sum z in Log[1 + z], occur in many terms); the
   memo nestedMemo lives as long as that canonical form, so that the declarations cannot change meanwhile *)
nestedCanonicalExpression[i_, depth_] := If[! TrueQ[$nestedMemoActive], canonicalExpression[i, depth],
   With[{m = nestedMemo[i, depth]}, If[Head[m] === nestedMemo, nestedMemo[i, depth] = canonicalExpression[i, depth], m]]];

(* returns {coefficient, diagramTerm[rest, odds, dummies]}, or {0, None} *)
canonicalTerm[diagramTerm[c0_, o0_, d_], depth_] := Module[{c, o, result},
   {c, o} = List @@ normalizeExternalDeltas[diagramTerm[c0, o0, d]][[{1, 2}]];
   c = canonicalizeNestedIntegrals[c, depth];
   result = If[d === {}, canonicalTermWithoutDummies[c, o], Catch[canonicalTermWithDummies[c, o, d, depth], canonical]];
   If[result === $vanishingTerm, {0, None}, result]];

canonicalTermWithoutDummies[c_, o_] := With[{k = termKey[c, o, {}]},
   If[k[[2]] === 0, {0, None}, {k[[2]], diagramTerm[k[[1, 1]], k[[1, 2]], {}]}]];

canonicalTermWithDummies[c0_, o0_, d0_, depth_] := Module[{canonicalNames = Table[DummyVariable[depth, k], {k, Length[d0]}],
    d = freshSymbols[Length[d0]], c, o, inerts, orders, candidates, best, coefficients, keys},
   (* work with atomic symbols, whatever the incoming names (e.g. canonical DummyVariable[..]) *)
   c = c0 /. Thread[d0 -> d]; o = o0 /. Thread[d0 -> d];
   (* the variables of nested integrals are masked: their numbering depends on the outer names *)
   inerts = inertForm /@ (Select[Join[factorList[c], o], ! FreeQ[#, Alternatives @@ d] &] /. DummyVariable[k_ /; k > depth, _] -> nestedVariable);
   (* the keys of the term, each computed once *)
   keys["Identity"] := keys["Identity"] = termKey[c, o, {}];
   keys["Transposition", {x_, y_}] := keys["Transposition", {x, y}] = transpositionSymmetrySign[c, o, x, y, keys["Identity"]];
   keys["Leaf", p_List] := keys["Leaf", p] = termKey[c, o, Thread[p -> canonicalNames]];
   keys["Automorphisms"] = {};
   orders = individualizationSearch[keys, refinementData[inerts, d], d, Table[1, {Length[d]}], {}];
   candidates = Select[keys["Leaf", #] & /@ orders, #[[2]] =!= 0 &];
   If[candidates === {}, {0, None},
    best = First[Sort[candidates[[All, 1]]]];
    coefficients = Union[Select[candidates, #[[1]] === best &][[All, 2]]];
    If[Length[coefficients] > 1, {0, None}, {First[coefficients], diagramTerm[best[[1]], best[[2]], canonicalNames]}]]];

(* Copies: many raw terms are copies of one another (the Wick sums of real fields, SuperfieldComponents, the derivatives
   of exp(1/2 J G J), whose copies have fresh names), and the canonical form is computed once for the first term of each
   key, scaled for the others. The key of a term: {its product without the exact numeric factor and its odd atoms, the
   variables renamed by their position in d inside Hold (nothing evaluated), that factor}; two terms with one key are
   n2/n1 times each other renamed, and the canonical form is invariant under the renaming and scales with an exact
   factor. A term that is a number, or whose numeric factor is inexact, has a key of its own (its position i). *)
copyKey[diagramTerm[c_, o_, d_], i_] := If[Head[c] === Times && NumberQ[First[c]],
   If[ExactNumberQ[First[c]], {{Length[d], Delete[Hold[c, o], {1, 1}] /. Thread[d -> Array[copySlot, Length[d]]]}, First[c]},
    {{"single", i}, 1}],
   If[NumberQ[c], {{"single", i}, 1}, {{Length[d], Hold[c, o] /. Thread[d -> Array[copySlot, Length[d]]]}, 1}]];
copyKey[_, i_] := {{"single", i}, 1};
(* the canonical forms of the terms, in their order: computed for the first term of each key (in the order of the
   terms), the copies' from it *)
canonicalTermsOfCopies[terms_List, keys_List, depth_] := Module[{factors = keys[[All, 2]], firstOf, representatives, canonicalOf},
   firstOf = Lookup[AssociationThread[Reverse[keys[[All, 1]]] -> Reverse[Range[Length[terms]]]], keys[[All, 1]]];
   representatives = DeleteDuplicates[firstOf];
   canonicalOf = AssociationThread[representatives -> (canonicalTerm[#, depth] & /@ terms[[representatives]])];
   MapThread[Function[{k, f, nk, nf}, If[k === f, canonicalOf[f],
       With[{r = canonicalOf[f]}, If[r[[1]] === 0, r, {r[[1]] nk/nf, r[[2]]}]]]], {Range[Length[terms]], firstOf, factors, factors[[firstOf]]}]];

canonicalTermList[terms_List, depth_ : 1] := If[! AssociationQ[$symmetricMemo],
   Block[{$symmetricMemo = <||>}, canonicalTermList[terms, depth]], canonicalTermListMemo[terms, depth]];
canonicalTermListMemo[terms_List, depth_] := If[depth === 1 && ! TrueQ[$nestedMemoActive] && ! FreeQ[terms, IntegralOver],
   Block[{$nestedMemoActive = True, nestedMemo}, canonicalTermListAt[terms, depth]], canonicalTermListAt[terms, depth]];
canonicalTermListAt[terms_List, depth_] := Module[{keys = MapIndexed[copyKey[#1, First[#2]] &, terms], canonical, groups},
   canonical = Select[If[DuplicateFreeQ[keys[[All, 1]]], canonicalTerm[#, depth] & /@ terms, canonicalTermsOfCopies[terms, keys, depth]],
     #[[1]] =!= 0 &];
   groups = GatherBy[canonical, Last];
   Select[Table[With[{w = Together[Total[g[[All, 1]]]]}, diagramTerm[w First[g][[2, 1]], First[g][[2, 2]], First[g][[2, 3]]]],
     {g, groups}], #[[1]] =!= 0 &]];

(* ========================================================================= *)
(*  7. The user-level calculus                                               *)
(* ========================================================================= *)

(* "MaximumOrder" -> {x, n}: only the orders 0 to n in x of the result (OrderPart[result, x, Range[0, n]]), the terms of
   higher order dropped after each derivative. This needs derivatives that never lower the order of a term: x a coupling,
   or the head of a line that is neither a source nor a head with a declared derivative (DeclareFunctionalDerivative);
   otherwise the whole derivative is taken, then cut *)
FunctionalDerivative[e_, s__, "MaximumOrder" -> {x_, n_Integer}] := With[{chain = derivativeChain[Hold[FunctionalDerivative[e, s]]]},
   If[chain === $Failed || ! orderKeepingQ[x], OrderPart[FunctionalDerivative[e, s], x, Range[0, n]],
    With[{order = {x, ! FreeQ[e, x[___]], n}},
     chain[[3]] termListToExpression[differentiateUpToOrder[toTermList[seriesLogs[ReleaseHold[chain[[1]]], order]], chain[[2]], order, False]]]]];
orderKeepingQ[x_] := ! MemberQ[sourceHeads[], x] && ! MemberQ[$derivativePairs[[All, 1]], x];
(* with an order in a coupling x: Log[s] of a sum polynomial in x is replaced by its power series in x up to x^n (after
   the shared exponential is split off, Log[E^X p] = X + Log[p]): Log[p0 + d] = Log[p0] + Sum_k (-1)^(k+1) (d/p0)^k/k,
   d = p - p0 of order >= 1, so that the terms of higher order are dropped by the order, as for a polynomial *)
seriesLogs[e_, {x_, headQ_, n_}] := If[headQ || FreeQ[e, Log], e, e /. Log[s_] /; ! FreeQ[s, x] :> logSeries[s, x, n]];
logSeries[s_, x_, n_] := With[{split = sharedExponential[s]}, If[split === None, logPowerSeries[s, x, n], split[[1]] + logPowerSeries[split[[2]], x, n]]];
(* d = p - p0 by order, d_j x^j; only the products of total order n or less of the powers of d are written: d^k as the
   products d_j1 ... d_jk over the compositions j1 + ... + jk <= n (2^n - 1 of them for all k together) *)
logPowerSeries[p_, x_, n_] := With[{q = Expand[p]}, With[{p0 = q /. x -> 0}, If[p0 === 0 || ! PolynomialQ[q, x], Log[p],
    With[{ds = Table[Coefficient[q, x, j] x^j/p0, {j, n}]},
     Log[p0] + Sum[(-1)^(k + 1)/k Total[(Times @@ ds[[#]] &) /@ compositionsUpTo[k, n]], {k, n}]]]]];
(* the k-tuples of positive integers with total m or less, in lexicographic order *)
compositionsUpTo[0, _] := {{}};
compositionsUpTo[k_Integer, m_Integer] := If[m < k, {}, Join @@ Table[(Prepend[#, j] &) /@ compositionsUpTo[k - 1, m - j], {j, m - k + 1}]];
FunctionalDerivative[e_, l_List] := FunctionalDerivative[e, Sequence @@ l];
FunctionalDerivative::nosource = "`1` is not a declared source (DeclareSource, DeclareGrassmannSource): the derivative with respect to it is 0.";
(* a kernel of an antisymmetric head written with its labels out of canonical order, h[y, x] (as the propagator
   G_psibarpsibar of a superfield), has already evaluated to -h[x, y]: the derivative with respect to it is minus the
   derivative with respect to h[x, y] *)
FunctionalDerivative[e_, Times[-1, k : (g_Symbol[__])]] /; Context[g] =!= "System`" := -FunctionalDerivative[e, k];
FunctionalDerivative[e_, source_] := If[! sourceAtomQ[source], Message[FunctionalDerivative::nosource, source]; 0,
   termListToExpression[canonicalTermList[differentiateTermList[toTermList[e], source]]]];
FunctionalDerivative[e_, sources__, last_] := FunctionalDerivative[FunctionalDerivative[e, last], sources];

(* the raw terms of an expression, with their delta functions integrated, not yet in canonical form; the canonical terms *)
rawTermsOf[e_] := integrateDeltaFunctions /@ toTermList[e];
canonicalTermsOf[e_] := canonicalTermList[rawTermsOf[e]];
SimplifyDiagrams[e_] := termListToExpression[canonicalTermsOf[e]];
DiagramTerms[e_] := termToExpression /@ canonicalTermsOf[e];

setSourcesToZeroInTerm[diagramTerm[c_, o_, d_]] := If[AnyTrue[o, sourceDependentOddAtomQ], {},
   With[{cc = c //. {a_ /; sourceAtomQ[a] :> 0} //. {(GrassmannProduct | NonCommutativeMultiply)[___, 0, ___] -> 0, IntegralOver[0, _] -> 0}},
    If[cc === 0, {}, {diagramTerm[cc, o, d]}]]];
(* SetSourcesToZero holds its argument: the derivatives of a FunctionalDerivative written in it (directly, or nested) are
   taken one at a time with the terms that cannot survive at zero sources dropped after each of them (the number of their
   factors that vanish at zero sources, sourceDegree, is larger than the number of derivatives left: a derivative removes
   at most one of them), as GaussianExpectationValue does; the result is the same *)
SetAttributes[SetSourcesToZero, HoldFirst];
SetSourcesToZero[d : FunctionalDerivative[_, __]] := With[{chain = derivativeChain[Hold[d]]},
   If[chain === $Failed, setSourcesToZero[d],
    chain[[3]] termListToExpression[canonicalTermList[If[chain[[4]] === None,
       differentiateAndSetSourcesToZero[toTermList[ReleaseHold[chain[[1]]]], chain[[2]]],
       differentiateUpToOrder[toTermList[seriesLogs[ReleaseHold[chain[[1]]], chain[[4]]]], chain[[2]], chain[[4]], True]]]]]];
(* the derivatives (the last source first) with a canonical form after each. A term is dropped when its order in x can
   be read (x, or x[..] when headQ, only in factors x^k) and is above n: the derivatives do not lower it. A term whose
   order cannot be read (x in an exponential, a logarithm, a non-integer power or a nested integral) is kept; atZero, the
   terms that cannot survive at zero sources are dropped too, and the sources are then set to zero. At the end the orders
   0 to n are kept, read as OrderPart reads them *)
differentiateUpToOrder[terms0_List, sources_List, {x_, headQ_, n_}, atZero_] := Module[{terms = terms0, k},
   terms = Select[terms, ! orderAboveQ[#, x, headQ, n] &];
   Do[terms = Select[canonicalTermList[differentiateTermList[terms, sources[[k]]]], ! orderAboveQ[#, x, headQ, n] && (! atZero || sourceDegree[#] <= k - 1) &],
    {k, Length[sources], 1, -1}];
   If[atZero, terms = Join @@ (setSourcesToZeroInTerm /@ terms)];
   Select[terms, MemberQ[Range[0, n], termOrder[#, x, headQ]] &]];
orderAboveQ[t : diagramTerm[c_, _, _], x_, headQ_, n_] := AllTrue[factorList[c], If[headQ,
     FreeQ[#, x[___]] || Head[#] === x || (Head[#] === Power && Head[#[[1]]] === x && IntegerQ[#[[2]]] && #[[2]] > 0),
     FreeQ[#, x] || # === x || (Head[#] === Power && #[[1]] === x && IntegerQ[#[[2]]] && #[[2]] > 0)] &] &&
   termOrder[t, x, headQ] > n;
SetSourcesToZero[e_] := setSourcesToZero[e];
setSourcesToZero[e_] := termListToExpression[canonicalTermList[Join @@ (setSourcesToZeroInTerm /@ toTermList[e])]];
(* {held expression, sources (the last one acts first), sign, order} of nested FunctionalDerivative[...], with the order
   {x, headQ, n} of "MaximumOrder" -> {x, n} or None; $Failed when a source is not a declared source atom (the derivative
   then gives its message) or the order cannot be used while differentiating *)
SetAttributes[derivativeChain, HoldAll];
derivativeChain[Hold[FunctionalDerivative[e_, s__]]] := Module[{inner = derivativeChain[Hold[e]], sources = Flatten[{s}], signs, order},
   order = Cases[sources, ("MaximumOrder" -> {x_, n_Integer}) :> {x, n}];
   sources = DeleteCases[sources, "MaximumOrder" -> _];
   signs = Replace[sources, {Times[-1, k : (g_Symbol[__])] /; Context[g] =!= "System`" :> -1, _ -> 1}, {1}];
   sources = Replace[sources, Times[-1, k : (g_Symbol[__])] /; Context[g] =!= "System`" :> k, {1}];
   Which[
    (* an order given only to the outermost derivative: the inner ones may then be cut at it too, not the converse *)
    inner === $Failed || ! AllTrue[sources, sourceAtomQ] || Length[order] > 1 || inner[[4]] =!= None, $Failed,
    order === {}, {inner[[1]], Join[sources, inner[[2]]], inner[[3]] Times @@ signs, None},
    ! orderKeepingQ[order[[1, 1]]], $Failed,
    True, With[{x = order[[1, 1]]}, {inner[[1]], Join[sources, inner[[2]]], inner[[3]] Times @@ signs,
      {x, ! FreeQ[ReleaseHold[inner[[1]]], x[___]], order[[1, 2]]}}]]];
derivativeChain[Hold[e_]] := {Hold[e], {}, 1, None};

fieldOperator[f_] := Module[{m = Select[$fieldSourceMap, #[[1]] === Head[f] &]},
   If[m === {}, Message[FieldDerivative::nofield, f]; {1, f}, {m[[1, 3]], m[[1, 2]] @@ (List @@ f)}]];
FieldDerivative::nofield = "No source declared for `1` (use DeclareField).";

(* a composite operator may be written as a product of fields: psibar[a] ** psi[a] stands for the two entries psibar[a],
   psi[a] of the list, and a power or a Times of commuting fields for its factors. Grassmann fields multiplied with Times
   have lost their order: $Failed, with a message *)
FieldDerivative::times = "The Grassmann fields in `1` are multiplied with Times, which loses their order: write them as separate entries of the list of fields (or with **).";
expandOperator[(GrassmannProduct | NonCommutativeMultiply)[a___]] := Join @@ (expandOperator /@ {a});
expandOperator[Power[a_, n_Integer?Positive]] := Join @@ Table[expandOperator[a], {n}];
expandOperator[e_Times] := Join @@ (expandOperator /@ (List @@ e));
expandOperator[e_] := {e};
normaliseOperators[operators_List] := With[{bad = Select[operators, Head[#] === Times && Count[List @@ #, _?grassmannOddAtomQ] > 1 &]},
   If[bad =!= {}, Message[FieldDerivative::times, First[bad]]; $Failed, Join @@ (expandOperator /@ operators)]];

FieldDerivative[e_, operators_List] := With[{ops = normaliseOperators[operators]}, If[ops === $Failed, $Failed,
    Fold[With[{p = fieldOperator[#2]}, p[[1]] FunctionalDerivative[#1, p[[2]]]] &, e, Reverse[ops]]]];

FieldExpectationValue[z_, operators_List] := With[{ops = normaliseOperators[operators]}, If[ops === $Failed, $Failed,
    checkGeneratingFunctional[z]; checkFieldArity[z, ops]; SimplifyDiagrams[SetSourcesToZero[FieldDerivative[z, ops]]/SetSourcesToZero[z]]]];

(* a generating functional without any declared source: the sources were not declared, or a propagator named with a
   built-in symbol (D, ...) evaluated to 0, so that every expectation value would vanish *)
FunctionalDiagrams::nosources = "The generating functional `1` contains no declared source: declare its sources with DeclareSource or DeclareGrassmannSource (and do not name a propagator with a built-in symbol such as D).";
checkGeneratingFunctional[z_] := If[FreeQ[z, _?sourceAtomQ], Message[FunctionalDiagrams::nosources, Short[z]]];
(* a field with another number of arguments than its source in Z (psi[x] while Z holds eta[x, s], with a spin index):
   every expectation value of it vanishes *)
FunctionalDiagrams::fieldarity = "The field `1` has `2` argument(s), but its source `3` occurs in the generating functional only with `4`: the expectation values of `1` vanish. Give the field the arguments of its source (a spin index: psi[x, up]).";
checkFieldArity[z_, operators_List] := Scan[Function[f, Module[{m = Select[$fieldSourceMap, #[[1]] === Head[f] &], arities},
     If[m =!= {},
      arities = Union[Cases[{z}, m[[1, 2]][a___] :> Length[{a}], {0, Infinity}]];
      If[arities =!= {} && ! MemberQ[arities, Length[f]],
       Message[FunctionalDiagrams::fieldarity, f, Length[f], m[[1, 2]], If[Length[arities] == 1, First[arities], arities]]]]]],
   DeleteDuplicatesBy[operators, {Head[#], Length[#]} &]];

(* ========================================================================= *)
(*  8. Gaussian expectation values: Wick's theorem                           *)
(*  Two equivalent methods:                                                  *)
(*   - "Differentiation": the source derivatives are applied to Z0 one at a  *)
(*     time (terms that cannot survive at zero sources are dropped early);   *)
(*   - "WickPairings": the sum over complete pairings of the fields, each    *)
(*     pair replaced by its contraction <f1 f2>_0 (itself obtained from Z0   *)
(*     by differentiation), with the sign of the permutation for Grassmann   *)
(*     fields. Interchangeable interaction vertices and identical fields are *)
(*     enumerated once, with their multiplicity.                             *)
(* ========================================================================= *)

(* number of source atoms in the polynomial prefactor of a term *)
sourceDegree[diagramTerm[c_, o_, d_]] := Count[o, a_ /; sourceDependentOddAtomQ[a]] +
   Total[Map[Which[sourceAtomQ[#], 1, Head[#] === Power && sourceAtomQ[#[[1]]] && IntegerQ[#[[2]]], #[[2]], True, 0] &,
     factorList[c]]];

(* apply the derivatives (rightmost first) and set the sources to zero, dropping terms that cannot survive *)
differentiateAndSetSourcesToZero[terms0_List, sources_List] := Module[{terms = terms0, n = Length[sources]},
   Do[terms = canonicalTermList[differentiateTermList[terms, sources[[k]]]];
    terms = Select[terms, sourceDegree[#] <= k - 1 &], {k, n, 1, -1}];
   Join @@ (setSourcesToZeroInTerm /@ terms)];

expectationByDifferentiation[z0_, operators_List] := Module[{ops = fieldOperator /@ operators},
   (Times @@ ops[[All, 1]]) termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[toTermList[z0], ops[[All, 2]]]]]];

(* Z0 = Exp[quadratic form in the sources] *)
gaussianFunctionalQ[z0_] := Head[z0] === Power && z0[[1]] === E && With[{q = toTermList[z0[[2]]]},
    q =!= {} && AllTrue[q, sourceDegree[#] == 2 &]];

(* contraction <f1 f2>_0, computed once per pair of field heads and arities *)
$contractionCache = {};
contractionValue[z0_, f1_, f2_] := Module[{key = {z0, Head[f1], Length[f1], Head[f2], Length[f2]}, entry, g1, g2, value},
   entry = Select[$contractionCache, #[[1]] === key &, 1];
   If[entry === {},
    g1 = freshSymbols[Length[f1]]; g2 = freshSymbols[Length[f2]];
    value = expectationByDifferentiation[z0, {Head[f1] @@ g1, Head[f2] @@ g2}];
    entry = {key, {g1, g2, value}}; AppendTo[$contractionCache, entry],
    entry = First[entry]];
   entry[[2, 3]] /. Thread[Join[entry[[2, 1]], entry[[2, 2]]] -> Join[List @@ f1, List @@ f2]]];

fieldParityOdd[f_] := grassmannOddAtomQ[f];

(* the automorphisms of a vertex as permutations of its slots (image of each slot): permutations of the integration
   variables that leave the coefficient unchanged and permute the odd fields evenly, combined with the permutations of
   identical commuting fields; $Failed if there are more than 5040 of them *)
vertexSlotGroup[{coefficient_, fields_List, dummies_List}] := Module[{n = Length[fields], oddPositions, classes, within = {Range[Length[fields]]},
    group = {}, image, used, base, ok},
   oddPositions = Select[Range[n], fieldParityOdd[fields[[#]]] &];
   classes = Select[GatherBy[Complement[Range[n], oddPositions], fields[[#]] &], Length[#] > 1 &];
   If[Times @@ (Factorial[Length[#]] & /@ classes) > 5040, Return[$Failed]];
   (* the permutations within the classes of identical commuting fields *)
   Do[within = Join @@ Table[With[{t = ReplacePart[w, Thread[c -> q]]}, t], {w, within}, {q, Permutations[c]}], {c, classes}];
   Do[With[{rule = Thread[dummies -> p]},
      If[(coefficient /. rule) === coefficient,
       image = fields /. rule;
       If[Sort[image] === Sort[fields],
        used = {}; ok = True;
        base = Table[With[{t = Select[Flatten[Position[fields, image[[i]], {1}]], ! MemberQ[used, #] &, 1]},
           If[t === {}, ok = False; i, AppendTo[used, t[[1]]]; t[[1]]]], {i, n}];
        If[ok && permutationSign[base[[oddPositions]]] === 1,
         group = Join[group, Table[w[[base]], {w, within}]]]]]],
    {p, If[1 < Length[dummies] <= 5, Permutations[dummies], {dummies}]}];
   If[Length[group] > 5040, $Failed, Union[group]]];

(* the orbits of the slots of a vertex under its automorphisms (group: vertexSlotGroup): {orbit id, orbit size} for every
   slot. When the group is $Failed (too large), the classes of identical commuting fields: the orbits of a subgroup, so that
   the multiplicities stay exact (the enumeration is less pruned) *)
vertexSlotOrbits[fields_List, group_] := Table[With[{o = Which[group =!= $Failed, Union[group[[All, i]]],
       fieldParityOdd[fields[[i]]], {i}, True, Flatten[Position[fields, fields[[i]], {1}]]]}, {Min[o], Length[o]}],
   {i, Length[fields]}];

(* the orbits of the free slots under the automorphisms that fix the slots fixed (local slot numbers) *)
stabiliserOrbits[group_List, fixed_List] := With[{h = Select[group, Function[pi, AllTrue[fixed, pi[[#]] == # &]]]},
   DeleteDuplicates[Table[Union[h[[All, i]]], {i, Complement[Range[Length[First[group]]], fixed]}]]];

findRoot[parent_List, g_Integer] := Module[{r = g}, While[parent[[r]] =!= r, r = parent[[r]]]; r];

(* the labels of a field that are points (not discrete index values such as a spin projection) *)
pointLabelsOf[field_] := Select[List @@ field, ! discreteIndexValueQ[#] &];

(* The sum over complete pairings. groups: one per external field, then one per interaction vertex.
   mode: "All", "NoVacuumBubbles" (components without external fields are discarded) or "Connected". *)

wickPairingSum[z0_, externalFields_List, vertices_List, mode_String] := Module[{slots = {}, slotGroup = {}, groupType = {},
    groupSlots, orbitId = {}, orbitSize = {}, nGroups, external, odd, oddCount, contraction, results, enumerate, initialParent,
    slotGroups = {}, contractionTable, slotClasses, slotClass, classCompatible, compatible, stuckQ, orbitMemo,
    orbitMultiplicities, matchableQ, groupsOfType},
   (* slots *)
   Do[AppendTo[slots, externalFields[[i]]]; AppendTo[slotGroup, i]; AppendTo[groupType, -i];
    AppendTo[orbitId, 0]; AppendTo[orbitSize, 1]; AppendTo[slotGroups, $Failed], {i, Length[externalFields]}];
   Do[With[{v = vertices[[j]], g = Length[externalFields] + j},
     AppendTo[groupType, v[[1]]];
     AppendTo[slotGroups, With[{hit = Select[Take[vertices, j - 1], #[[1]] === v[[1]] &, 1]},
       If[hit =!= {}, slotGroups[[Length[externalFields] + First[First[Position[vertices[[All, 1]], v[[1]], {1}]]]]],
        vertexSlotGroup[v[[2 ;; 4]]]]]];
     With[{orbits = vertexSlotOrbits[v[[3]], Last[slotGroups]]},
      Do[AppendTo[slots, v[[3, i]]]; AppendTo[slotGroup, g]; AppendTo[orbitId, orbits[[i, 1]]]; AppendTo[orbitSize, orbits[[i, 2]]],
       {i, Length[v[[3]]]}]]], {j, Length[vertices]}];
   nGroups = Length[groupType];
   groupSlots = Table[Select[Range[Length[slots]], slotGroup[[#]] == g &], {g, nGroups}];
   external = Table[g <= Length[externalFields], {g, nGroups}];
   odd = fieldParityOdd /@ slots; oddCount = Boole[odd];
   (* the groups of each vertex type *)
   groupsOfType = Association[(# -> Select[Range[nGroups], Function[g, groupType[[g]] === #]] &) /@ DeleteDuplicates[groupType]];
   (* the orbits of the free slots of an opened vertex of type t under its automorphisms that fix its contracted slots
      (local slot numbers): only the first slot of each orbit is contracted, with the size of the orbit as multiplicity;
      orbitMultiplicities: that multiplicity for every local slot (0 for the others) *)
   orbitMemo[t_, fixedLocal_] := orbitMemo[t, fixedLocal] =
     stabiliserOrbits[slotGroups[[First[Select[Range[nGroups], groupType[[#]] === t &, 1]]]], fixedLocal];
   orbitMultiplicities[t_, fixedLocal_, size_] := orbitMultiplicities[t, fixedLocal, size] = Module[{m = ConstantArray[0, size]},
      Scan[(m[[First[#]]] = Length[#]) &, Reverse[orbitMemo[t, fixedLocal]]]; m];
   contractionTable = Table[Null, {Length[slots]}, {Length[slots]}];
   contraction[i_, j_] := With[{known = contractionTable[[i, j]]}, If[known =!= Null, known,
      contractionTable[[i, j]] = If[odd[[i]] =!= odd[[j]], 0, contractionValue[z0, slots[[i]], slots[[j]]]]]];
   (* classes of slots (head, number of labels, parity): two slots can be contracted only if their classes can *)
   slotClasses = Union[Table[{Head[s], Length[s], fieldParityOdd[s]}, {s, slots}]];
   slotClass = Table[First[First[Position[slotClasses, {Head[s], Length[s], fieldParityOdd[s]}, {1}]]], {s, slots}];
   classCompatible = Table[c1[[3]] === c2[[3]] &&
      contractionValue[z0, c1[[1]] @@ freshSymbols[c1[[2]]], c2[[1]] @@ freshSymbols[c2[[2]]]] =!= 0, {c1, slotClasses}, {c2, slotClasses}];
   compatible = Boole[classCompatible];
   (* a component with free slots none of which can be contracted with a free slot outside it can only close: it is
      discarded if it is not the whole diagram ("Connected") or if it has no external field ("NoVacuumBubbles") *)
   (* (only the component just joined is checked: comp = the component of every group, root = its label, members = its
      groups) *)
   stuckQ[free_List, comp_List, root_, members_List] := With[{labels = comp[[slotGroup[[free]]]]},
     With[{inside = slotClass[[Pick[free, labels, root]]], outside = slotClass[[Pick[free, Unitize[labels - root], 1]]]},
      inside =!= {} && (outside === {} || Max[compatible[[Union[inside], Union[outside]]]] == 0) &&
       If[mode === "Connected", Length[members] < nGroups, ! AnyTrue[members, external[[#]] &]]]];
   (* a complete pairing exists only if the numbers of slots of each class can be matched (e.g. an even number of
      gluons, as many quarks as antiquarks): checked once, on the numbers of slots of each class *)
   matchableQ[counts_List] := matchableQ[counts] = With[{nonzero = Select[Range[Length[counts]], counts[[#]] > 0 &]},
     If[nonzero === {}, True,
      With[{c = First[nonzero]}, AnyTrue[nonzero, Function[d, classCompatible[[c, d]] && (d =!= c || counts[[c]] >= 2) &&
          matchableQ[ReplacePart[counts, If[d === c, {c -> counts[[c]] - 2}, {c -> counts[[c]] - 1, d -> counts[[d]] - 1}]]]]]]]];
   (* comp: the component of every group, an integer label (on a merge the label of the first slot's component is kept);
      freeCount: the number of free slots of every group *)
   enumerate[free_List, weight_, factors_List, opened_List, comp_List, freeCount_List] := Module[{a, rest, ga, firstUnopened = {},
      usedOrbits = {}, usedIdentical = {}, unopenedCount, gb, multiplicity, value, sign, oddBefore, newFree, newComp, newFreeCount,
      root, cb, members, newOpened},
     If[free === {}, Sow[{weight, factors}],
      a = First[free]; rest = Rest[free]; ga = slotGroup[[a]];
      newOpened = ReplacePart[opened, ga -> True];
      (* the number of odd slots of rest before each one *)
      oddBefore = If[odd[[a]] && rest =!= {}, Prepend[Most[Accumulate[oddCount[[rest]]]], 0], {}];
      Do[With[{b = rest[[j]]},
       gb = slotGroup[[b]];
       value = contraction[a, b];
       If[value =!= 0,
        multiplicity = 0;
        If[! newOpened[[gb]],
         (* an unopened vertex: only the first unopened vertex of its type, and one slot per orbit *)
         With[{t = groupType[[gb]]},
          With[{f = Select[firstUnopened, #[[1]] === t &]},
           If[f === {}, AppendTo[firstUnopened, {t, gb}]];
           If[f === {} || f[[1, 2]] === gb,
            If[! MemberQ[usedOrbits, {gb, orbitId[[b]]}],
             AppendTo[usedOrbits, {gb, orbitId[[b]]}];
             unopenedCount = Count[newOpened[[groupsOfType[t]]], False];
             multiplicity = unopenedCount orbitSize[[b]]]]]],
         (* an opened vertex: slots related by the automorphisms that fix its contracted slots are interchangeable *)
         If[slotGroups[[gb]] =!= $Failed,
          With[{offset = First[groupSlots[[gb]]] - 1},
           multiplicity = orbitMultiplicities[groupType[[gb]], Complement[groupSlots[[gb]], rest] - offset,
              Length[groupSlots[[gb]]]][[b - offset]]],
         (* an external field: identical commuting fields are interchangeable *)
         If[odd[[b]], multiplicity = 1,
          If[! MemberQ[usedIdentical, {gb, slots[[b]]}],
           AppendTo[usedIdentical, {gb, slots[[b]]}];
           multiplicity = Count[rest, s_ /; slotGroup[[s]] == gb && slots[[s]] === slots[[b]]]]]]];
        If[multiplicity > 0,
         sign = If[odd[[a]], (-1)^oddBefore[[j]], 1];
         newFree = Delete[rest, j];
         root = comp[[ga]]; cb = comp[[gb]];
         newComp = If[root === cb, comp, comp + (root - cb) (1 - Unitize[comp - cb])];
         members = Pick[Range[nGroups], newComp, root];
         newFreeCount = freeCount; newFreeCount[[ga]]--; newFreeCount[[gb]]--;
         If[Which[
           Total[newFreeCount[[members]]] > 0, True,                  (* the component is still open *)
           mode === "NoVacuumBubbles", AnyTrue[members, external[[#]] &],
           mode === "Connected", newFree === {},
           True, True] && (mode === "All" || newFree === {} || ! stuckQ[newFree, newComp, root, members]),
          enumerate[newFree, weight multiplicity sign, Append[factors, value], ReplacePart[newOpened, gb -> True], newComp,
           newFreeCount]]]]],
       {j, Length[rest]}]]];
   (* external fields at the same point (e.g. the two fields of a density psibar(a) psi(a)) are connected from the start *)
   initialParent = Range[nGroups];
   Do[If[Intersection[pointLabelsOf[externalFields[[i]]], pointLabelsOf[externalFields[[j]]]] =!= {},
      With[{ri = findRoot[initialParent, i], rj = findRoot[initialParent, j]}, If[ri =!= rj, initialParent[[rj]] = ri]]],
    {i, Length[externalFields]}, {j, i + 1, Length[externalFields]}];
   If[! matchableQ[Table[Count[slotClass, k], {k, Length[slotClasses]}]], Return[{}]];
   results = Reap[enumerate[Range[Length[slots]], 1, {}, Table[False, {nGroups}], Table[findRoot[initialParent, g], {g, nGroups}],
       Length /@ groupSlots]][[2]];
   If[results === {}, {}, First[results]]];

(* a vertex of the interaction: {coefficient (without the fields), fields in order, integration variables} *)
splitInteractionTerm[diagramTerm[c_, o_, d_]] := Module[{factors = factorList[c], bosons = {}, rest = 1},
   Do[Which[
     ! AtomQ[f] && MemberQ[$fieldSourceMap[[All, 1]], Head[f]], AppendTo[bosons, f],
     Head[f] === Power && ! AtomQ[f[[1]]] && MemberQ[$fieldSourceMap[[All, 1]], Head[f[[1]]]] && IntegerQ[f[[2]]],
     bosons = Join[bosons, Table[f[[1]], {f[[2]]}]],
     True, rest = rest f], {f, factors}];
   {rest, Join[bosons, o], d}];

(* the vertices of all the multisets of k interaction terms, with the weight (-1)^k / (m1! m2! ...) *)
vertexMultisets[interactionTerms_List, k_Integer] := Module[{types = Range[Length[interactionTerms]], multisets},
   multisets = Select[Tuples[types, k], OrderedQ];
   Table[With[{counts = Table[Count[m, t], {t, types}]},
     {(-1)^k/(Times @@ (Factorial /@ counts)),
      Table[With[{v = splitInteractionTerm[interactionTerms[[m[[i]]]]]},
        With[{new = freshSymbols[Length[v[[3]]]]}, {m[[i]], v[[1]] /. Thread[v[[3]] -> new], v[[2]] /. Thread[v[[3]] -> new], new}]],
       {i, Length[m]}]}], {m, multisets}]];

(* a pairing whose factors are all plain is one term directly (the vertex variables are fresh symbols already); the
   pairings share a few distinct factors, each tested once *)
pairingTermsToTermList[pairs_List, vertices_List] := Module[{coefficient = Times @@ vertices[[All, 2]],
    dummies = Join @@ vertices[[All, 4]], plain = <||>, plainQ},
   plainQ[f_] := Lookup[plain, Key[f], plain[f] = plainFactorQ[f]];
   Join @@ Table[With[{c = p[[1]] coefficient Times @@ p[[2]]},
      If[AllTrue[factorList[c], plainQ], If[c === 0, {}, {diagramTerm[c, {}, dummies]}],
       toTermList[If[dummies === {}, c, IntegralOver[c, dummies]]]]], {p, pairs}]];

GaussianExpectationValue::method = "Method `1` is not available; using \"Differentiation\".";
Options[GaussianExpectationValue] = {"Method" -> Automatic};
GaussianExpectationValue[z0_, operatorList_List, opts___Rule] := Module[{operators = normaliseOperators[operatorList]},
   If[operators === $Failed, Return[$Failed]];
   checkGeneratingFunctional[z0];
   If[perturbationMethod[z0, {opts}] === "WickPairings",
    termListToExpression[canonicalTermList[pairingTermsToTermList[wickPairingSum[z0, operators, {}, "All"], {}]]],
    expectationByDifferentiation[z0, operators]]];

(* ========================================================================= *)
(*  9. Perturbation theory                                                   *)
(* ========================================================================= *)

PerturbativeCorrelationFunction::order = "The order `1` must be a non-negative integer n, {k} or {kmin, kmax}.";
orderList[n_Integer?NonNegative] := Range[0, n];
orderList[{k_Integer?NonNegative}] := {k};
orderList[{kmin_Integer?NonNegative, kmax_Integer}] := Range[kmin, kmax];
orderList[n_] := (Message[PerturbativeCorrelationFunction::order, n]; {});

(* the sum over orders of <operators exp(-interaction)>_0 with the given pruning of the pairings *)
(* coupling: None, or {x, m} to keep only the terms of order x^m (as OrderPart[result, x, m]): the multisets of vertices
   of another order are skipped when every interaction term is a monomial in x (and Z0 does not contain x), and the
   terms of the pairings are selected before canonicalisation *)
(* test: None, or a property of the diagrams (the particle irreducibility of LuttingerWardFunctional) by which the terms
   are selected before the canonical form, with their delta functions integrated (as rawTermsOf does) *)
perturbativePairingSum[z0_, operators_List, interaction_, orders_List, mode_String, coupling_ : None, test_ : None] := Module[
   {interactionTerms = toTermList[interaction], terms = {}, x, m, headQ, exponents = None},
   If[coupling =!= None,
    {x, m} = coupling; headQ = ! FreeQ[{z0, interaction}, x[___]];
    If[! headQ && FreeQ[z0, x] && AllTrue[interactionTerms, FreeQ[#[[1]]/x^Exponent[#[[1]], x], x] && FreeQ[#[[2]], x] &],
     exponents = Exponent[#[[1]], x] & /@ interactionTerms]];
   Do[Do[If[exponents === None || Total[exponents[[ms[[2, All, 1]]]]] == m,
       terms = Join[terms, (diagramTerm[ms[[1]] #[[1]], #[[2]], #[[3]]] &) /@
          pairingTermsToTermList[wickPairingSum[z0, operators, ms[[2]], mode], ms[[2]]]]],
     {ms, vertexMultisets[interactionTerms, k]}], {k, orders}];
   termListToExpression[Which[
     test =!= None, selectCanonicalTermList[integrateDeltaFunctions /@ terms, If[coupling === None, test, termOrder[#, x, headQ] == m && test[#] &]],
     coupling === None, canonicalTermList[terms],
     True, selectCanonicalTermList[terms, termOrder[#, x, headQ] == m &]]]];

FunctionalDiagrams::coupling = "The value `1` of \"CouplingOrder\" is not None or {coupling, k} with an integer k; it is ignored.";
couplingOption[opts_List] := With[{c = optionValue[opts, {"CouplingOrder" -> None}, "CouplingOrder"]},
   Which[c === None, None, MatchQ[c, {_, _Integer}], c, True, Message[FunctionalDiagrams::coupling, c]; None]];
(* the order x^m of the result of the method "Differentiation" *)
couplingOrderPart[e_, None] := e;
couplingOrderPart[e_, {x_, m_}] := OrderPart[e, x, m];

(* the same sum by literal differentiation of Z0 (any Z0) *)
perturbativeDifferentiationSum[z0_, operators_List, interaction_, orders_List] := Module[{total = 0,
    z00 = expectationByDifferentiation[z0, {}]},
   Do[Module[{accumulated = 0},
      Do[Module[{vs = splitInteractionTerm /@ tuple, coefficient, allOperators, dummies},
        coefficient = Times @@ vs[[All, 1]];
        allOperators = Join[operators, Join @@ vs[[All, 2]]];
        dummies = Join @@ vs[[All, 3]];
        accumulated = accumulated + IntegralOver[coefficient expectationByDifferentiation[z0, allOperators]/z00, dummies]],
       {tuple, Tuples[Table[toTermList[interaction], {i, k}]]}];
      total = total + (-1)^k/k! accumulated], {k, orders}];
   SimplifyDiagrams[total]];

FunctionalDiagrams::method = "The method `1` is not Automatic, \"WickPairings\" or \"Differentiation\"; Automatic is used.";
perturbationMethod[z0_, opts_List] := With[{m = Replace[optionValue[opts, {"Method" -> Automatic}, "Method"],
     mm_ /; ! MemberQ[{Automatic, "WickPairings", "Differentiation"}, mm] :> (Message[FunctionalDiagrams::method, mm]; Automatic)]},
   Which[m === Automatic, If[gaussianFunctionalQ[z0], "WickPairings", "Differentiation"],
    m === "WickPairings" && ! gaussianFunctionalQ[z0], Message[GaussianExpectationValue::method, m]; "Differentiation",
    True, m]];

labelArguments[operators_List] := Union[Select[Flatten[List @@@ operators], ! discreteIndexValueQ[#] &]];

(* the correlation functions: mode is the pruning of the pairings, filter the same selection of the terms for the
   method "Differentiation" (a pure function, so that it reads as a call, not as the name of a caller) *)
correlationFunction[z0_, operatorList_List, interaction_, n_, opts_List, mode_String, filter_] :=
  With[{operators = normaliseOperators[operatorList]}, If[operators === $Failed, $Failed, checkGeneratingFunctional[z0]; checkFieldArity[z0, operators];
    checkLabels[0, labelArguments[operators]];
    If[perturbationMethod[z0, opts] === "WickPairings",
     perturbativePairingSum[z0, operators, interaction, orderList[n], mode, couplingOption[opts]],
     couplingOrderPart[filter[perturbativeDifferentiationSum[z0, operators, interaction, orderList[n]], labelArguments[operators]],
      couplingOption[opts]]]]];

Options[PerturbativeCorrelationFunction] = {"Method" -> Automatic, "CouplingOrder" -> None};
PerturbativeCorrelationFunction[z0_, operatorList_List, interaction_, n_, opts___Rule] :=
  correlationFunction[z0, operatorList, interaction, n, {opts}, "NoVacuumBubbles", DropVacuumBubbles[#1, #2] &];

Options[ConnectedCorrelationFunction] = {"Method" -> Automatic, "CouplingOrder" -> None};
ConnectedCorrelationFunction[z0_, operatorList_List, interaction_, n_, opts___Rule] :=
  correlationFunction[z0, operatorList, interaction, n, {opts}, "Connected", ConnectedDiagrams[#1, #2] &];

(* linked-cluster theorem: the connected terms of Z/Z0 are log(Z/Z0) *)
Options[ConnectedVacuumDiagrams] = {"Method" -> Automatic, "CouplingOrder" -> None};
ConnectedVacuumDiagrams[z0_, interaction_, n_, opts___Rule] := connectedVacuumDiagrams[z0, interaction, n, {opts}, None];
(* the connected vacuum diagrams with a property (test, or None for all) *)
connectedVacuumDiagrams[z0_, interaction_, n_, opts_List, test_] := With[{orders = DeleteCases[orderList[n], 0]},
   checkGeneratingFunctional[z0];
   If[perturbationMethod[z0, opts] === "WickPairings",
    perturbativePairingSum[z0, {}, interaction, orders, "Connected", couplingOption[opts], test],
    With[{all = couplingOrderPart[ConnectedDiagrams[perturbativeDifferentiationSum[z0, {}, interaction, orders], {}], couplingOption[opts]]},
     If[test === None, all, selectTerms[all, test]]]]];

(* order of a term: power of a coupling symbol, or number of factors with a given head *)
termOrder[diagramTerm[c_, o_, d_], x_, headQ_] := If[headQ,
   Total[Map[Which[Head[#] === x, 1, Head[#] === Power && Head[#[[1]]] === x && IntegerQ[#[[2]]], #[[2]], True, 0] &, factorList[c]]],
   Exponent[c, x]];
OrderPart[e_, x_, k_Integer] := With[{headQ = ! FreeQ[e, x[___]]},
   termListToExpression[selectCanonicalTerms[e, termOrder[#, x, headQ] == k &]]];
OrderPart[e_, x_, ks_List] := With[{headQ = ! FreeQ[e, x[___]]},
   termListToExpression[selectCanonicalTerms[e, MemberQ[ks, termOrder[#, x, headQ]] &]]];

(* ========================================================================= *)
(*  10. Classes of diagrams                                                  *)
(* ========================================================================= *)

(* connectivity of the diagram of a term: components of the points (external labels and integration variables) *)
connectedComponentsOfTerm[diagramTerm[c_, o_, d_], externals_List] := Module[{nodes = Join[externals, d], sets},
   sets = Select[Table[Select[nodes, ! FreeQ[f, #] &], {f, Join[factorList[c], o]}], # =!= {} &];
   mergeOverlapping[List /@ Select[nodes, ! FreeQ[{c, o}, #] &], sets]];

(* the canonical terms of e that pass test, a property of the diagram (unchanged by renaming the integration variables
   and by combining equal diagrams): the terms are selected before canonicalisation, so that only the selected ones are
   canonicalised. Terms with nested integrals (whose canonicalisation may change them) are tested again afterwards. *)
selectCanonicalTermList[terms_List, test_] := With[{nestedQ = (! FreeQ[#[[1]], IntegralOver] &)},
   Select[canonicalTermList[Select[terms, nestedQ[#] || test[#] &]], ! nestedQ[#] || test[#] &]];
selectCanonicalTerms[e_, test_] := selectCanonicalTermList[rawTermsOf[e], test];

ConnectedDiagrams[e_, externals_List] := termListToExpression[selectCanonicalTerms[e,
    Length[connectedComponentsOfTerm[#, externals]] == 1 &]];
DropVacuumBubbles[e_, externals_List] := termListToExpression[selectCanonicalTerms[e,
    AllTrue[connectedComponentsOfTerm[#, externals], Intersection[#, externals] =!= {} &] &]];
(* the canonical terms with a property of the diagram *)
selectTerms[e_, test_] := termListToExpression[selectCanonicalTerms[e, test]];

expandIntegerPowers[factors_List] := Flatten[If[Head[#] === Power && IntegerQ[#[[2]]] && #[[2]] > 0, Table[#[[1]], {#[[2]]}], {#}] & /@ factors];

(* ---- connectivity of graphs with nodes 1..n and edges {a, b} (a packed integer matrix), compiled (the virtual
   machine of Compile: no C compiler needed); a union-find over the edges, the roots found by their parent pointers ---- *)
$cutCompileOptions = {CompilationTarget -> "WVM", CompilationOptions -> {"InlineCompiledFunctions" -> True, "InlineExternalDefinitions" -> True}};
(* the classes of the points 1..n joined by the pairs: the root (smallest point) of the class of each point *)
cGlueRoots = Compile[{{n, _Integer}, {pairs, _Integer, 2}},
   Module[{parent = Range[n], ra = 0, rb = 0},
    Do[ra = pairs[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
     rb = pairs[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
     If[ra != rb, If[ra < rb, parent[[rb]] = ra, parent[[ra]] = rb]], {e, Length[pairs]}];
    Table[ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; ra, {v, n}]], CompilationTarget -> "WVM"];
(* the graph without the edges numbered s1 and s2 (0: none) is connected *)
cConnectedWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], components = n, ra = 0, rb = 0},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra; components--]], {e, Length[edges]}];
    components <= 1], CompilationTarget -> "WVM"];
(* every component of the graph without the edges s1, s2 holds a node marked in external (0 or 1 for each node) *)
cEveryComponentExternalWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {external, _Integer, 1}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], hasExternal = Table[0, {n}], ra = 0, rb = 0, ok = True},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra]], {e, Length[edges]}];
    Do[ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[external[[v]] == 1, hasExternal[[ra]] = 1], {v, n}];
    Do[If[parent[[v]] == v && hasExternal[[v]] == 0, ok = False], {v, n}];
    ok], CompilationTarget -> "WVM"];
(* without the edges s1, s2 the nodes of left (not empty) are in one component and those of right in another one *)
cSeparatesWithout = Compile[{{n, _Integer}, {edges, _Integer, 2}, {left, _Integer, 1}, {right, _Integer, 1}, {s1, _Integer}, {s2, _Integer}},
   Module[{parent = Range[n], ra = 0, rb = 0, rl = 0, rr = 0, ok = True},
    Do[If[e != s1 && e != s2,
      ra = edges[[e, 1]]; While[parent[[ra]] != ra, ra = parent[[ra]]];
      rb = edges[[e, 2]]; While[parent[[rb]] != rb, rb = parent[[rb]]];
      If[ra != rb, parent[[rb]] = ra]], {e, Length[edges]}];
    rl = left[[1]]; While[parent[[rl]] != rl, rl = parent[[rl]]];
    rr = right[[1]]; While[parent[[rr]] != rr, rr = parent[[rr]]];
    If[rl == rr, ok = False];
    Do[If[ok, ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[ra != rl, ok = False]], {v, left}];
    Do[If[ok, ra = v; While[parent[[ra]] != ra, ra = parent[[ra]]]; If[ra != rr, ok = False]], {v, right}];
    ok], CompilationTarget -> "WVM"];
(* the graph stays connected when any k <= 2 of the edges listed in cut are removed *)
cConnectedAfterCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {k, _Integer}},
   Module[{ok = True, m = Length[cut]},
    ok = cConnectedWithout[n, edges, 0, 0];
    If[ok && k >= 1, Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], 0]], {i, m}]];
    If[ok && k >= 2, Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], cut[[j]]]], {i, m}, {j, i + 1, m}]];
    ok], Evaluate[Sequence @@ $cutCompileOptions]];
(* the skeleton test of a connected graph: no single edge of cut disconnects it, and no pair of them leaves a component
   without an external node (hasExternal 1) or disconnects it (hasExternal 0) *)
cSkeletonCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {external, _Integer, 1}, {hasExternal, _Integer}},
   Module[{ok = True, m = Length[cut]},
    Do[If[ok, ok = cConnectedWithout[n, edges, cut[[i]], 0]], {i, m}];
    Do[If[ok, ok = If[hasExternal == 0, cConnectedWithout[n, edges, cut[[i]], cut[[j]]],
        cEveryComponentExternalWithout[n, edges, external, cut[[i]], cut[[j]]]]], {i, m}, {j, i + 1, m}];
    ok], Evaluate[Sequence @@ $cutCompileOptions]];
(* some k <= 2 of the edges listed in cut separate left from right *)
cSeparatedByCuts = Compile[{{n, _Integer}, {edges, _Integer, 2}, {cut, _Integer, 1}, {left, _Integer, 1}, {right, _Integer, 1}, {k, _Integer}},
   Module[{found = False, m = Length[cut]},
    If[k >= 1, Do[If[! found, found = cSeparatesWithout[n, edges, left, right, cut[[i]], 0]], {i, m}]];
    If[k >= 2, Do[If[! found, found = cSeparatesWithout[n, edges, left, right, cut[[i]], cut[[j]]]], {i, m}, {j, i + 1, m}]];
    found], Evaluate[Sequence @@ $cutCompileOptions]];
(* the edges as the compiled functions take them; a self-loop, which joins nothing, stands for no edge (n >= 1) *)
packedEdges[edges_List] := Developer`ToPackedArray[If[edges === {}, {{1, 1}}, edges]];

(* The graph used for cuts. Points are the integration variables and the external labels; factors that are not lines
   (vertices, interaction lines, delta functions) glue their points into one node. An external label that ends a single
   line coming from an integration variable is an external leg: the leg is not a line of the graph, and the node it
   ends on is marked as external. Any other external label is a point of the graph (amputated vertex functions).
   Result: {number of nodes n, edges {i, j} between the nodes 1..n (in the order of the lines), the external nodes,
   rules external label -> node}. The points are numbered by an association, the arguments of all the factors looked up
   at once, the points glued by a compiled union-find; the nodes are numbered in the order of their first point. *)
FunctionalDiagrams::lineargs = "Lines are functions of two labels, but `1` has `2` arguments, and it is treated as a vertex: put internal indices such as spin into the head (Gup, Gdown) or sum over them explicitly.";
graphForCuts[diagramTerm[c_, o_, d_], externals_List, lineHeads_List] := Module[{factors = Join[factorList[c], o], compound,
    lengths, lineHead, occurrences, legEnds = {}, legFactors = {}, vertexExternals, points, np, index, kept, keptLengths, args,
    lineQ, glue, roots, seen, present, presentRoots, renumber, node, rules},
   If[! FreeQ[factors, Power], factors = expandIntegerPowers[factors]];
   compound = Pick[factors, AtomQ /@ factors, False];
   lengths = Length /@ compound;
   lineHead = (MemberQ[lineHeads, #] &) /@ (Head /@ compound);
   Do[If[lengths[[i]] =!= 2, Message[FunctionalDiagrams::lineargs, userForm[compound[[i]]], lengths[[i]]]], {i, Pick[Range[Length[compound]], lineHead]}];
   (* external legs *)
   Do[occurrences = DeleteCases[Union[Position[factors, e, {1, Infinity}][[All, 1]]], 0];
    If[Length[occurrences] == 1, With[{f = factors[[occurrences[[1]]]]},
      If[! AtomQ[f] && MemberQ[lineHeads, Head[f]] && Length[f] == 2 && Count[List @@ f, e] == 1 &&
        MemberQ[d, First[DeleteCases[List @@ f, e]]],
       AppendTo[legEnds, {e, First[DeleteCases[List @@ f, e]]}]; AppendTo[legFactors, occurrences[[1]]]]]], {e, externals}];
   vertexExternals = Complement[externals, legEnds[[All, 1]]];
   points = Join[vertexExternals, d]; np = Length[points];
   index = AssociationThread[points -> Range[np]];
   (* the factors of the graph: not atoms, not legs *)
   If[legFactors === {}, kept = compound; keptLengths = lengths,
    With[{mask = (! MemberQ[legFactors, #] &) /@ Pick[Range[Length[factors]], AtomQ /@ factors, False]},
     kept = Pick[compound, mask]; keptLengths = Pick[lengths, mask]; lineHead = Pick[lineHead, mask]]];
   (* the point of every argument of every factor (0: not a point); a line: a line head with two points *)
   args = TakeList[Lookup[index, Join @@ (List @@@ kept), 0], keptLengths];
   lineQ = MapThread[#1 && #2 == 2 && FreeQ[#3, 0] &, {lineHead, keptLengths, args}];
   glue = Join @@ (With[{l = DeleteCases[#, 0]}, If[Length[l] < 2, {}, Thread[{First[l], Rest[l]}]]] &) /@ Pick[args, lineQ, False];
   roots = If[glue === {}, Range[np], cGlueRoots[np, glue]];
   (* the nodes: the classes of the points that occur *)
   seen = DeleteCases[Union[Flatten[args]], 0];
   present = If[Length[seen] == np, seen, Union[seen, Select[Complement[Range[np], seen], ! FreeQ[{c, o}, points[[#]]] &]]];
   presentRoots = DeleteDuplicates[roots[[present]]];
   renumber = ConstantArray[0, np]; If[presentRoots =!= {}, renumber[[presentRoots]] = Range[Length[presentRoots]]];
   node = renumber[[roots[[index[#]]]]] &;
   rules = Join[(# -> node[#] &) /@ Select[vertexExternals, ! FreeQ[{c, o}, #] &], (#[[1]] -> node[#[[2]]] &) /@ legEnds];
   {Length[presentRoots], Partition[renumber[[roots[[Flatten[Pick[args, lineQ]]]]]], 2], Union[rules[[All, 2]]], rules}];

(* connected components of a graph with nodes 1..n *)
graphComponents[n_Integer, edges_List] := mergeOverlapping[List /@ Range[n], edges];

(* removable lines: self-loops never disconnect anything *)
cuttableLines[edges_List] := If[edges === {}, {}, Pick[Range[Length[edges]], Unitize[edges[[All, 1]] - edges[[All, 2]]], 1]];

particleIrreducibleTermQ[t_diagramTerm, n_Integer, externals_List, lineHeads_List] := Module[{nodes, edges, ext, rules, cut},
   {nodes, edges, ext, rules} = graphForCuts[t, externals, lineHeads];
   If[nodes == 0, Return[False]];
   cut = cuttableLines[edges];
   If[n <= 2, cConnectedAfterCuts[nodes, packedEdges[edges], cut, n],
    (* more than two lines: every subset, each tested by the compiled connectivity *)
    cConnectedWithout[nodes, packedEdges[edges], 0, 0] && AllTrue[Join @@ Table[Subsets[cut, {k}], {k, 1, Min[n, Length[cut]]}],
      cConnectedWithout[nodes, packedEdges[Delete[edges, List /@ #]], 0, 0] &]]];

(* the external labels must occur in the expression as arguments of its functions: a label that does not is a misprint,
   or the arguments are in the order of version 1 (line heads before labels) *)
FunctionalDiagrams::label = "The label `1` does not occur in the expression: check the labels and the order of the arguments (labels before line heads).";
FunctionalDiagrams::labelsymbol = "The label `1` is not a symbol: labels must be symbols (numbers are confused with the indices of the integration variables).";
checkLabels[e_, labels_List] := (Scan[If[Head[#] =!= Symbol, Message[FunctionalDiagrams::labelsymbol, #]] &, Flatten[labels]];
   If[e =!= 0, With[{leaves = Union[Level[e, {-1}]]},
     Scan[If[! MemberQ[leaves, #], Message[FunctionalDiagrams::label, #]] &, Flatten[labels]]]]);

ParticleIrreducibleQ[t_, n_Integer, externals_List, lineHeads_List] := With[{ts = canonicalTermsOf[t]},
   checkLabels[t, externals]; Length[ts] == 1 && particleIrreducibleTermQ[First[ts], n, externals, lineHeads]];
ParticleIrreducibleDiagrams[e_, n_Integer, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[selectCanonicalTerms[e, particleIrreducibleTermQ[#, n, externals, lineHeads] &]]);

(* Skeleton: one-particle irreducible, and no two lines cut off a part without external points (a self-energy
   insertion). For a two-point function with external legs this is the lemma "closing the legs gives a 2PI vacuum
   graph"; for amputated vertex functions it is the usual definition. *)
skeletonTermQ[t_diagramTerm, externals_List, lineHeads_List] := Module[{n, edges, ext, rules, marks},
   {n, edges, ext, rules} = graphForCuts[t, externals, lineHeads];
   If[n == 0 || ! cConnectedWithout[n, packedEdges[edges], 0, 0], Return[False]];
   marks = ConstantArray[0, n]; If[ext =!= {}, marks[[ext]] = 1];
   (* without external points (vacuum diagrams) a skeleton is two-particle irreducible *)
   cSkeletonCuts[n, packedEdges[edges], cuttableLines[edges], marks, If[ext === {}, 0, 1]]];

SkeletonDiagrams[e_, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[selectCanonicalTerms[e, skeletonTermQ[#, externals, lineHeads] &]]);

(* ---- closed loops of lines (fermion loops) ---- *)
(* closed components of directed lines {from, to, head}: every point has one incoming and one outgoing line *)
closedComponents[lines_List] := Module[{points = Union[Flatten[lines[[All, {1, 2}]]]], components, inDegree, outDegree},
   If[lines === {}, Return[{}]];
   components = graphComponents[Length[points], Map[First[First[Position[points, #, {1}]]] &, lines[[All, {1, 2}]], {2}]];
   inDegree[x_] := Count[lines, {_, x, _}]; outDegree[x_] := Count[lines, {x, _, _}];
   Select[points[[#]] & /@ components, AllTrue[#, inDegree[#] == 1 && outDegree[#] == 1 &] &]];
(* the loops of each head, also where several lines meet at a local vertex (Hubbard), and the loops made of lines of
   different heads *)
closedLoopCountOfTerm[diagramTerm[c_, o_, d_], lineHeads_List] := Module[{lines},
   lines = Cases[expandIntegerPowers[factorList[c]], f_ /; ! AtomQ[f] && MemberQ[lineHeads, Head[f]] && Length[f] == 2 :>
      {f[[2]], f[[1]], Head[f]}];
   Total[Table[Length[closedComponents[Select[lines, #[[3]] === h &]]], {h, lineHeads}]] +
    Count[closedComponents[lines], cmp_ /; Length[Union[Select[lines, MemberQ[cmp, #[[1]]] &][[All, 3]]]] > 1]];
ClosedLoopCount[t_, lineHeads_List] := With[{ts = canonicalTermsOf[t]}, If[Length[ts] == 1, closedLoopCountOfTerm[First[ts], lineHeads], $Failed]];
DropClosedLoops[e_, lineHeads_List] := termListToExpression[selectCanonicalTerms[e, closedLoopCountOfTerm[#, lineHeads] == 0 &]];

(* ---- amputation of the external legs ---- *)
amputateTerm[diagramTerm[c_, o_, d_], externals_List, lineHeads_List] := Module[{factors = expandIntegerPowers[factorList[c]],
    legs = {}, renamed = {}, deltas = 1, ok = True, occurrences, f, z, previous},
   Do[If[ok,
     occurrences = Select[Range[Length[factors]], ! FreeQ[factors[[#]], e] &];
     If[Length[occurrences] != 1 || ! FreeQ[o, e], ok = False,
      f = factors[[occurrences[[1]]]];
      If[AtomQ[f] || ! MemberQ[lineHeads, Head[f]] || Length[f] != 2 || Count[List @@ f, e] != 1, ok = False,
       z = First[DeleteCases[List @@ f, e]];
       If[! MemberQ[d, z] || MemberQ[legs, occurrences[[1]]], ok = False,
        AppendTo[legs, occurrences[[1]]];
        previous = Select[renamed, #[[1]] === z &];
        If[previous === {}, AppendTo[renamed, {z, e}], deltas = deltas DeltaFunction[e, previous[[1, 2]]]]]]]], {e, externals}];
   If[! ok, {},
    With[{rules = (#[[1]] -> #[[2]] &) /@ renamed},
     {diagramTerm[((Times @@ Delete[factors, List /@ legs]) /. rules) deltas, o /. rules, Complement[d, renamed[[All, 1]]]]}]]];

AmputateExternalLegs[e_, externals_List, lineHeads_List] := (checkLabels[e, externals];
   termListToExpression[amputatedTerms[e, externals, lineHeads]]);
(* the canonical terms of the amputated diagrams: the raw terms are amputated (amputation commutes with the renaming of
   the integration variables) and brought into canonical form once *)
amputatedTerms[e_, externals_List, lineHeads_List] := canonicalTermList[Join @@ (amputateTerm[#, externals, lineHeads] &) /@ rawTermsOf[e]];

(* ========================================================================= *)
(*  11. The Luttinger-Ward functional, the self-energy and the vertices      *)
(* ========================================================================= *)

Options[LuttingerWardFunctional] = {"Method" -> Automatic};
LuttingerWardFunctional[z0_, interaction_, n_, rule_Rule, opts___Rule] := LuttingerWardFunctional[z0, interaction, n, {rule}, opts];
(* a full propagator without the symmetry of the bare one (G0 symmetric, G undeclared): G(x, y) and G(y, x) would be
   different terms *)
FunctionalDiagrams::rulesymmetry = "`1` is `2` and `3` is not: declare it (`4`[`3`]), or its terms with the labels exchanged are not recognized as equal.";
checkRuleSymmetry[rules_List] := Scan[Function[r, If[MatchQ[r, _Symbol -> _Symbol],
     Which[orderlessQ[r[[1]]] && ! orderlessQ[r[[2]]], Message[FunctionalDiagrams::rulesymmetry, r[[1]], "symmetric", r[[2]], DeclareSymmetric],
      MemberQ[$antisymmetricHeads, r[[1]]] && ! MemberQ[$antisymmetricHeads, r[[2]]],
      Message[FunctionalDiagrams::rulesymmetry, r[[1]], "antisymmetric", r[[2]], DeclareAntisymmetric]]]], rules];
(* several propagators made full at once (e.g. G and the screened interaction W of a Hubbard-Stratonovich field): the
   diagrams that stay connected when any two lines, of any of these kinds, are cut *)
LuttingerWardFunctional[z0_, interaction_, n_, rules : {__Rule}, opts___Rule] := (Scan[warnIfBuiltin, rules[[All, 2]]]; checkRuleSymmetry[rules];
  -connectedVacuumDiagrams[z0, interaction, n, {opts}, particleIrreducibleTermQ[#, 2, {}, rules[[All, 1]]] &] /. rules);

FunctionalDiagrams::statistics = "The option \"Statistics\" -> `1` is not one of `2`; \"Fermion\" is used.";
statisticsOption[opts_List, allowed_List : {"Fermion", "Boson"}] := With[{s = optionValue[opts, {"Statistics" -> "Fermion"}, "Statistics"]},
   If[MemberQ[allowed, s], s, Message[FunctionalDiagrams::statistics, s, allowed]; "Fermion"]];
(* "Boson" is for a real field, with a symmetric propagator *)
FunctionalDiagrams::realboson = "\"Statistics\" -> \"Boson\" is for real fields, with a symmetric propagator, and `1` is not symmetric: for complex bosons use \"ComplexBoson\".";
checkRealBoson[g_Symbol] := If[! MemberQ[Attributes[g], Orderless], Message[FunctionalDiagrams::realboson, g]];
(* "Superfield" is for a Grassmann field with an antisymmetric propagator: the Nambu field (psi, psibar), whose propagator
   calG(a, b) = -<T NambuField_a NambuField_b> has the normal block G and the anomalous blocks -<T psi psi>, -<T psibar psibar> *)
FunctionalDiagrams::superfield = "\"Statistics\" -> \"Superfield\" is for a Grassmann field with an antisymmetric propagator (DeclareAntisymmetric), and `1` is not antisymmetric.";
checkSuperfield[g_Symbol] := If[! MemberQ[$antisymmetricHeads, g], Message[FunctionalDiagrams::superfield, g]];

(* the default "Statistics" -> "Fermion" on a symmetric or antisymmetric G (the propagator of a real boson or of a
   superfield): a message *)
FunctionalDiagrams::statisticsdefault = "`1` is `2`, and \"Statistics\" is not given: `3` uses the fermionic convention (\[CapitalSigma](x, y) = \[Delta]\[CapitalPhi]/\[Delta]`1`(y, x), \[CapitalGamma]0(x1, x2; x3, x4) = \[Delta]\[CapitalSigma](x1, x2)/\[Delta]`1`(x4, x3)). For `4` give \"Statistics\" -> \"`5`\".";
checkStatisticsGiven[f_, g_, opts_List] := If[! MemberQ[optionKey /@ opts[[All, 1]], "Statistics"],
   Which[orderlessQ[g], Message[FunctionalDiagrams::statisticsdefault, g, "symmetric", f, "a real boson", "Boson"],
    MemberQ[$antisymmetricHeads, g], Message[FunctionalDiagrams::statisticsdefault, g, "antisymmetric", f, "a superfield", "Superfield"]]];
FunctionalDiagrams::vertexlabels = "`1` takes the list of its `2` labels, `3`; `4` was given.";

(* derivative with respect to G[x, y], G being treated as a source; for a symmetric G (bosons) the derivative is
   taken with respect to an unsymmetrised copy and then symmetrised; for an antisymmetric G (a superfield propagator,
   mode "Antisymmetric") it is antisymmetrised, (d - d(x <-> y))/2, the counterpart of dG(u, v)/dG(x, y) =
   (delta(u, x) delta(v, y) - delta(u, y) delta(v, x))/2 *)
$kernelCopies = {};
derivativeWithRespectToPropagator[e_, g_Symbol, {x_, y_}, mode_] := Module[{copy = Unique["FunctionalDiagrams`Private`g"], d},
   Block[{$bosonicSources = Union[$bosonicSources, {copy}], $kernelCopies = Append[$kernelCopies, copy -> g]},
    d = FunctionalDerivative[e /. g -> copy, copy[x, y]];
    Switch[mode,
     True, d = SimplifyDiagrams[(d + (d /. {x -> y, y -> x}))/2],
     "Antisymmetric", d = SimplifyDiagrams[(d - (d /. {x -> y, y -> x}))/2]]];
   d /. copy -> g];
(* the mode of the derivative with respect to a kernel g *)
propagatorDerivativeMode[g_Symbol] := Which[orderlessQ[g], True, MemberQ[$antisymmetricHeads, g], "Antisymmetric", True, False];

(* G = G0 + G0 Sigma G in all cases: fermions (G = -<psi psibar>) Sigma(x, y) = dPhi/dG(y, x); real bosons (G = <phi phi>,
   symmetric) Sigma = -2 dPhi/dG(x, y); complex bosons (G = <phi phibar>) Sigma(x, y) = -dPhi/dG(y, x); superfields
   (calG = -<NambuField NambuField>, antisymmetric) Sigma = -2 dPhi/dcalG(x, y), the derivative antisymmetrised *)
Options[SelfEnergy] = {"Statistics" -> "Fermion"};
SelfEnergy[phi_, g_Symbol, labels_List, ___Rule] /; Length[labels] != 2 :=
  (Message[FunctionalDiagrams::vertexlabels, SelfEnergy, 2, "{x, y}", labels]; $Failed);
SelfEnergy[phi_, g_Symbol, {x_, y_}, opts___Rule] := (checkStatisticsGiven[SelfEnergy, g, {opts}]; selfEnergyOf[phi, g, {x, y}, opts]);
selfEnergyOf[phi_, g_Symbol, {x_, y_}, opts___Rule] := Switch[statisticsOption[{opts}, {"Fermion", "Boson", "ComplexBoson", "Superfield"}],
   "Boson", checkRealBoson[g]; SimplifyDiagrams[-2 derivativeWithRespectToPropagator[phi, g, {x, y}, True]],
   "Superfield", checkSuperfield[g]; SimplifyDiagrams[-2 derivativeWithRespectToPropagator[phi, g, {x, y}, "Antisymmetric"]],
   "ComplexBoson", SimplifyDiagrams[-derivativeWithRespectToPropagator[phi, g, {y, x}, False]],
   _, derivativeWithRespectToPropagator[phi, g, {y, x}, False]];

(* Gamma0(x1, x2; x3, x4) = dSigma(x1, x2)/dG(x4, x3) for fermions and complex bosons; -2 dSigma(x1, x2)/dG(x3, x4) for
   real bosons and superfields (antisymmetrised for these) *)
Options[IrreducibleVertex] = {"Statistics" -> "Fermion", "Channel" -> Automatic};
FunctionalDiagrams::channel = "\"Channel\" -> `1` is not one of `2` for \"Statistics\" -> `3`.";
FunctionalDiagrams::ppkernel = "The kernel of the particle-particle channel is a derivative with respect to the anomalous propagators -<T psi psi> and -<T psibar psibar>, not with respect to G: use the superfield (psi, psibar) (\"Statistics\" -> \"Superfield\", \"Channel\" -> \"t\") and SuperfieldComponents.";
IrreducibleVertex[phi_, g_Symbol, labels_List, ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, IrreducibleVertex, 4, "{x1, x2, x3, x4}", labels]; $Failed);
IrreducibleVertex[phi_, g_Symbol, labels : {x1_, x2_, x3_, x4_}, opts___Rule] := Module[
   {statistics = statisticsOption[{opts}, {"Fermion", "Boson", "ComplexBoson", "Superfield"}],
    channel = optionValue[{opts}, Options[IrreducibleVertex], "Channel"], rest, allowed, first},
   checkStatisticsGiven[IrreducibleVertex, g, {opts}];
   rest = Append[Select[{opts}, ! MemberQ[{"Channel", "Statistics"}, optionKey[#[[1]]]] &], "Statistics" -> statistics];
   allowed = Switch[statistics, "Boson" | "Superfield", {"s", "t", "u", All}, "Fermion", {"ParticleHole", "TransverseParticleHole"}, _, {}];
   Which[
    channel === Automatic || channel === First[Append[allowed, None]], irreducibleVertexFirstChannel[phi, g, labels, statistics, rest],
    statistics === "Fermion" && MemberQ[{"ParticleParticle", All}, channel], Message[FunctionalDiagrams::ppkernel]; $Failed,
    ! MemberQ[allowed, channel], Message[FunctionalDiagrams::channel, channel, Prepend[allowed, Automatic], statistics]; $Failed,
    True,
    first = irreducibleVertexFirstChannel[phi, g, labels, statistics, rest];
    If[statistics === "Fermion",
     (* the transverse particle-hole channel (x1 x4|x3 x2): -Gamma0(x3, x2; x1, x4) *)
     SimplifyDiagrams[-(first /. {x1 -> x3, x3 -> x1})],
     (* Gamma_r(a, b, c, d) = sgn(pi_r) Gamma0(pi_r(a, b, c, d)): pi_t = (a c b d), odd, pi_u = (a d b c), even; the sign is +1
        for real bosons *)
     With[{sign = If[statistics === "Superfield", -1, 1]},
      With[{rules = {"s" -> first, "t" -> SimplifyDiagrams[sign (first /. {x2 -> x3, x3 -> x2})],
          "u" -> SimplifyDiagrams[first /. {x2 -> x4, x3 -> x2, x4 -> x3}]}},
       If[channel === All, Association[rules], channel /. rules]]]]]];
irreducibleVertexFirstChannel[phi_, g_, {x1_, x2_, x3_, x4_}, statistics_, opts_List] := Switch[statistics,
   "Boson", SimplifyDiagrams[-2 derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x3, x4}, True]],
   "Superfield", SimplifyDiagrams[-2 derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x3, x4}, "Antisymmetric"]],
   _, derivativeWithRespectToPropagator[SelfEnergy[phi, g, {x1, x2}, Sequence @@ opts], g, {x4, x3}, False]];

Options[PerturbativeFullVertex] = {"Method" -> Automatic};
(* the orders of the vertex: n means 1 to n (the vertex has no zeroth order), {k} only k, {kmin, kmax} kmin to kmax *)
vertexOrders[n_Integer] := {1, n};
vertexOrders[{k_Integer}] := {k};
vertexOrders[{kmin_Integer, kmax_Integer}] := {Max[kmin, 1], kmax};
vertexOrders[n_] := n;
PerturbativeFullVertex[z0_, interaction_, n_, fields_List, labels_List, rules : (_Rule | {__Rule}), ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, PerturbativeFullVertex, 4, "{x1, x2, x3, x4}", labels]; $Failed);
(* several bare propagators made full at once (one per spin, or G and the screened interaction): a list of rules *)
PerturbativeFullVertex[z0_, interaction_, n_, fields_List, labels : {x1_, x2_, x3_, x4_}, rule : (_Rule | {__Rule}), opts___Rule] := Module[{correlation, sign, vertexTerms,
    rules = Flatten[{rule}]},
   Scan[warnIfBuiltin, rules[[All, 2]]]; checkRuleSymmetry[rules];
   If[Length[fields] == 2,
    (* fermions: <psi(1) psibar(2) psibar(4) psi(3)>_c = G G F G G *)
    correlation = ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[2]][x2], fields[[2]][x4], fields[[1]][x3]},
      interaction, vertexOrders[n], opts]; sign = 1,
    (* real bosons: F = Gamma^(4) = -(amputated connected four-point function) *)
    correlation = ConnectedCorrelationFunction[z0, fields[[1]] /@ labels, interaction, vertexOrders[n], opts]; sign = -1];
   (* SkeletonDiagrams[sign AmputateExternalLegs[correlation, ...], ...], with one canonical form instead of three *)
   checkLabels[correlation, labels];
   vertexTerms = amputatedTerms[sign correlation, labels, rules[[All, 1]]];
   checkLabels[If[vertexTerms === {}, 0, vertexTerms], labels];
   termListToExpression[Select[vertexTerms, skeletonTermQ[#, labels, rules[[All, 1]]] &]] /. rules];

Options[DysonSeries] = {"MaximumOrder" -> None};
dysonChain[pieces_List, {s1_, s2_}, g0_, {x_, y_}] := Module[{k = Length[pieces], as, bs},
   as = freshSymbols[k]; bs = freshSymbols[k];
   IntegralOver[g0[x, as[[1]]] Times @@ Table[(pieces[[i]] /. {s1 -> as[[i]], s2 -> bs[[i]]}) If[i < k, g0[bs[[i]], as[[i + 1]]], g0[bs[[i]], y]],
       {i, k}], Join[as, bs]]];
DysonSeries::zeroth = "The self-energy has a part of order 0 in `1`, which the chains of \"MaximumOrder\" leave out.";
DysonSeries[sigma_, {s1_, s2_}, g0_, {x_, y_}, m_Integer, opts___Rule] := Module[{maximum = optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"],
    total = g0[x, y], parts},
   If[maximum === None,
    Do[total = total + dysonChain[Table[sigma, {k}], {s1, s2}, g0, {x, y}], {k, m}],
    (* only the chains of total order <= n in the coupling x *)
    parts = orderParts[sigma, maximum[[1]], maximum[[2]]];
    If[First[parts] =!= 0, Message[DysonSeries::zeroth, maximum[[1]]]];
    parts = Rest[parts];
    Do[Do[If[Total[c] <= maximum[[2]] && ! MemberQ[parts[[c]], 0], total = total + dysonChain[parts[[c]], {s1, s2}, g0, {x, y}]],
       {c, Tuples[Range[maximum[[2]]], k]}], {k, Min[m, maximum[[2]]]}]];
   SimplifyDiagrams[total]];

(* ========================================================================= *)
(*  12. Two-particle channels, Bethe-Salpeter ladders and the parquet         *)
(* ========================================================================= *)

channelPairings["Fermion", {x1_, x2_, x3_, x4_}] := {"ParticleHole" -> {{x1, x2}, {x3, x4}},
   "TransverseParticleHole" -> {{x1, x4}, {x3, x2}}, "ParticleParticle" -> {{x1, x3}, {x2, x4}}};
channelPairings["Boson", {x1_, x2_, x3_, x4_}] := {"s" -> {{x1, x2}, {x3, x4}}, "t" -> {{x1, x3}, {x2, x4}}, "u" -> {{x1, x4}, {x2, x3}}};
(* a superfield: the channels of real bosons *)
channelPairings["Superfield", labels_List] := channelPairings["Boson", labels];

twoParticleReducibleTermQ[t_diagramTerm, pairs : {_List, _List}, lineHeads_List] := cutReducibleTermQ[t, pairs, 2, lineHeads];

TwoParticleReducibleQ[t_, pairs : {_List, _List}, lineHeads_List] := With[{ts = canonicalTermsOf[t]},
   checkLabels[t, pairs]; Length[ts] == 1 && twoParticleReducibleTermQ[First[ts], pairs, lineHeads]];

(* cutting n or fewer lines separates the external labels of the first group from those of the second *)
cutReducibleTermQ[t_diagramTerm, {left_List, right_List}, n_Integer, lineHeads_List] :=
  cutReducibleGraphQ[graphForCuts[t, Join[left, right], lineHeads], {left, right}, n];
(* the graph does not depend on the order of the external labels: it is computed once for all the channels *)
cutReducibleGraphQ[{m_Integer, edges_, ext_, rules_}, {left_List, right_List}, n_Integer] := Module[{cut, l, r},
   l = Union[left /. rules]; r = Union[right /. rules];
   If[Intersection[l, r] =!= {} || ! AllTrue[Join[l, r], IntegerQ], Return[False]];
   If[edges === {} || l === {} || r === {}, Return[False]];
   cut = cuttableLines[edges];
   If[n <= 2, cSeparatedByCuts[m, packedEdges[edges], cut, l, r, n],
    (* more than two lines: every subset, each tested by the compiled separation *)
    AnyTrue[Join @@ Table[Subsets[cut, {k}], {k, 1, Min[n, Length[cut]]}],
     cSeparatesWithout[m, packedEdges[Delete[edges, List /@ #]], l, r, 0, 0] &]]];
ReducibleDiagrams[e_, groups : {_List, _List}, n_Integer, lineHeads_List] := (checkLabels[e, groups];
   selectTerms[e, cutReducibleTermQ[#, groups, n, lineHeads] &]);
IrreducibleDiagrams[e_, groups : {_List, _List}, n_Integer, lineHeads_List] := (checkLabels[e, groups];
   selectTerms[e, ! cutReducibleTermQ[#, groups, n, lineHeads] &]);

Options[ParquetDecomposition] = {"Statistics" -> "Fermion"};
$parquetStatistics = {"Fermion", "Boson", "Superfield"};
(* the canonical terms of f by class: {"FullyIrreducible" -> terms, channel -> terms, ..., "SeveralChannels" -> terms} *)
parquetClassTerms[terms_List, channels_List, lineHeads_List, labels_List] := Module[{classes},
   classes = Table[With[{graph = graphForCuts[t, labels, lineHeads]},
      Select[channels, cutReducibleGraphQ[graph, #[[2]], 2] &][[All, 1]]], {t, terms}];
   Join[{"FullyIrreducible" -> Pick[terms, (# === {} &) /@ classes]},
    Table[ch[[1]] -> Pick[terms, (# === {ch[[1]]} &) /@ classes], {ch, channels}],
    {"SeveralChannels" -> Pick[terms, (Length[#] > 1 &) /@ classes]}]];

(* misuse: four labels, the heads of the lines in a list (one head alone is accepted), else a message *)
ParquetDecomposition::args = "ParquetDecomposition takes the vertex F, the list of its four external labels {x1, x2, x3, x4} and the list of the heads of its lines {G, ...}, then options; it was given `1` arguments with the heads `2`.";
ParquetDecomposition[f_, labels_List, lineHeads_List, ___Rule] /; Length[labels] != 4 :=
  (Message[FunctionalDiagrams::vertexlabels, ParquetDecomposition, 4, "{x1, x2, x3, x4}", labels]; $Failed);
ParquetDecomposition[f_, labels_List, g_Symbol, opts___Rule] := ParquetDecomposition[f, labels, {g}, opts];
ParquetDecomposition[args___] /; ! MatchQ[{args}, {_, _List, _List | _Symbol, ___Rule}] :=
  (Message[ParquetDecomposition::args, Length[{args}], Head /@ {args}]; $Failed);
ParquetDecomposition[f_, labels_List, lineHeads_List, opts___Rule] := Module[{channels = channelPairings[statisticsOption[{opts}, $parquetStatistics], labels],
    terms = canonicalTermsOf[f]},
   checkLabels[f, labels];
   Association[(#[[1]] -> termListToExpression[#[[2]]] &) /@ parquetClassTerms[terms, channels, lineHeads, labels]]];

(* L(a, b, c, d): the vertex expression with its labels x1..x4 replaced simultaneously *)
relabelVertex[v_, labels_List, new_List] := v /. Thread[labels -> new];

Options[BetheSalpeterLadder] = {"Statistics" -> "Fermion", "MaximumOrder" -> None};
BetheSalpeterLadder[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol, opts___Rule] /;
   optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"] =!= None :=
  Module[{maximum = optionValue[{opts}, {"MaximumOrder" -> None}, "MaximumOrder"], rest = Select[{opts}, optionKey[#[[1]]] =!= "MaximumOrder" &]},
   (* only the products of total order <= n *)
   ladderUpToOrder[orderParts[left, maximum[[1]], maximum[[2]]], orderParts[right, maximum[[1]], maximum[[2]]], labels, channel, g,
    statisticsOption[rest, $parquetStatistics]]];
BetheSalpeterLadder[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol, opts___Rule] :=
  SimplifyDiagrams[ladderIntegral[left, right, labels, channel, g, statisticsOption[{opts}, $parquetStatistics]]];

(* the parts of order 0, 1, ..., n of the canonical terms (as OrderPart, whose order of a term is termOrder) *)
orderPartsOfTerms[terms_List, x_, headQ_, n_Integer] := Table[Select[terms, termOrder[#, x, headQ] == k &], {k, 0, n}];
(* OrderPart[e, x, k] for k = 0, ..., n, with one canonical form of e *)
orderParts[e_, x_, n_Integer] := termListToExpression /@ orderPartsOfTerms[canonicalTermsOf[e], x, ! FreeQ[e, x[___]], n];
(* the ladder of the parts of order k1 of the left vertex and k2 of the right one, k1 + k2 <= n (lists of the parts of
   order 0..n), brought into canonical form once *)
ladderUpToOrder[leftParts_List, rightParts_List, labels_List, channel_String, g_Symbol, statistics_ : "Fermion"] := With[{n = Length[leftParts] - 1},
   SimplifyDiagrams[Total[Flatten[Table[If[leftParts[[k1 + 1]] === 0 || rightParts[[k2 + 1]] === 0, 0,
        ladderIntegral[leftParts[[k1 + 1]], rightParts[[k2 + 1]], labels, channel, g, statistics]], {k1, 0, n}, {k2, 0, n - k1}]]]]];

(* superfields: the ladders of real bosons times -1, +1, -1 in the channels s, t, u (the Grassmann signs of the legs
   relabelled, checked by CheckParquetEquations) *)
ladderIntegral[left_, right_, labels_List, channel : ("s" | "t" | "u"), g_Symbol, "Superfield"] :=
  If[channel === "t", 1, -1] ladderIntegral[left, right, labels, channel, g];
ladderIntegral[left_, right_, labels_List, channel_String, g_Symbol, _String] := ladderIntegral[left, right, labels, channel, g];
ladderIntegral[left_, right_, labels : {x1_, x2_, x3_, x4_}, channel_String, g_Symbol] := Module[{p5, p6, p7, p8, l, r},
   {p5, p6, p7, p8} = freshSymbols[4];
   l[a_, b_, c_, d_] := relabelVertex[left, labels, {a, b, c, d}];
   r[a_, b_, c_, d_] := relabelVertex[right, labels, {a, b, c, d}];
   Switch[channel,
     (* fermions leave the vertices at positions 1 and 3 and enter at 2 and 4; G[a, b] runs from b to a. The signs of
        the transverse and particle-particle channels are the Grassmann signs of the relabelled legs (checked by
        CheckParquetEquations against the diagrams order by order). *)
     "ParticleHole", IntegralOver[l[x1, x2, p5, p6] g[p6, p7] g[p8, p5] r[p7, p8, x3, x4], {p5, p6, p7, p8}],
     "TransverseParticleHole", -IntegralOver[l[x1, p6, p5, x4] g[p6, p7] g[p8, p5] r[p7, x2, x3, p8], {p5, p6, p7, p8}],
     "ParticleParticle", -1/2 IntegralOver[l[x1, p5, x3, p6] g[p5, p7] g[p6, p8] r[p7, x2, p8, x4], {p5, p6, p7, p8}],
     (* real bosons *)
     "s", -1/2 IntegralOver[l[x1, x2, p5, p6] g[p5, p7] g[p6, p8] r[p7, p8, x3, x4], {p5, p6, p7, p8}],
     "t", -1/2 IntegralOver[l[x1, p5, x3, p6] g[p5, p7] g[p6, p8] r[p7, x2, p8, x4], {p5, p6, p7, p8}],
     "u", -1/2 IntegralOver[l[x1, p5, p6, x4] g[p5, p7] g[p6, p8] r[p7, x2, x3, p8], {p5, p6, p7, p8}]]];

Options[CheckParquetEquations] = {"Statistics" -> "Fermion"};
(* the canonical terms of f are computed once and split by class and by order: the canonical form of an expression made
   of canonical terms gives the same terms, and is not computed again *)
CheckParquetEquations[f_, labels_List, g_Symbol, x_, n_Integer, opts___Rule] := Module[{pairings, channels, terms, classTerms, part,
    rightParts, residuals, counts},
   pairings = channelPairings[statisticsOption[{opts}, $parquetStatistics], labels];
   channels = pairings[[All, 1]];
   terms = canonicalTermsOf[f];
   checkLabels[f, labels];
   classTerms = parquetClassTerms[terms, pairings, {g}, labels];
   part[key_] := key /. classTerms;
   rightParts = termListToExpression /@ orderPartsOfTerms[terms, x, ! FreeQ[f, x[___]], n];
   residuals = With[{rules = Table[ch -> Module[{left = Complement[terms, part[ch]], ladder, difference},
          ladder = ladderUpToOrder[termListToExpression /@ orderPartsOfTerms[left, x, ! FreeQ[left, x[___]], n], rightParts, labels, ch, g,
            statisticsOption[{opts}, $parquetStatistics]];
          difference = canonicalTermsOf[termListToExpression[part[ch]] - ladder];
          termListToExpression /@ Rest[orderPartsOfTerms[difference, x, ! FreeQ[difference, x[___]], n]]], {ch, channels}]},
     Association[rules]];
   counts = With[{rules = Table[key -> Length /@ Rest[orderPartsOfTerms[part[key], x, ! FreeQ[part[key], x[___]], n]],
        {key, Join[{"FullyIrreducible"}, channels, {"SeveralChannels"}]}]}, Association[rules]];
   With[{rules = {"Residuals" -> residuals, "DiagramCounts" -> counts,
       "SeveralChannels" -> termListToExpression[part["SeveralChannels"]], "FullyIrreducible" -> termListToExpression[part["FullyIrreducible"]]}},
    Association[rules]]];

(* ========================================================================= *)
(*  12a. Vertices of one-body operators and exact equations                  *)
(* ========================================================================= *)

(* The vertex of the density at z. Fermions: a potential U coupled to rho = psibar psi gives G^-1 = G0^-1 - U - Sigma, and
   Lambda(x1, x2; z) = -dG^-1(x1, x2)/dU(z), so that <psi(x1) psibar(x2) rho(z)>_c = dG(x1, x2)/dU(z) = G G Lambda. Real
   bosons: rho = phi^2/2, G^-1 = G0^-1 + U - Sigma and Lambda = dG^-1/dU, so that <phi(x1) phi(x2) rho(z)>_c = G G Lambda.
   In both cases Lambda = delta(x1, z) delta(x2, z) + corrections. The legs x1, x2 are removed and the skeleton diagrams
   kept; the bare term is added by hand (AmputateExternalLegs removes only legs that end at internal points). *)
Options[PerturbativeDensityVertex] = {"Method" -> Automatic};
PerturbativeDensityVertex[z0_, interaction_, n_, fields_List, {x1_, x2_, z_}, rule_Rule, opts___Rule] := Module[{correlation},
   warnIfBuiltin[rule[[2]]];
   correlation = If[Length[fields] == 2,
     ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[2]][x2], fields[[2]][z], fields[[1]][z]}, interaction, vertexOrders[n], opts],
     1/2 ConnectedCorrelationFunction[z0, {fields[[1]][x1], fields[[1]][x2], fields[[1]][z], fields[[1]][z]}, interaction,
       vertexOrders[n], opts]];
   SimplifyDiagrams[DeltaFunction[x1, z] DeltaFunction[x2, z] +
     (SkeletonDiagrams[AmputateExternalLegs[correlation, {x1, x2}, {rule[[1]]}], {x1, x2, z}, {rule[[1]]}] /. rule)]];

(* a function of labels from an expression: LabelFunction[e, {x, y}][a, b] is e with x -> a, y -> b. The integration
   variables of e (DummyVariable[...] in canonical form) are renamed to fresh symbols first, so that an argument with the
   same name (a variable of an expression in canonical form) is not captured. The fresh symbols are made in the user's
   context, x$nn as those of Module, so that a printed result does not show the private context of the package *)
labelFunctionSymbols[n_Integer] := Table[Unique["x$"], {n}];
LabelFunction[e_, labels_List] := With[{vars = labelFunctionSymbols[Length[labels]], dummies = Union[Cases[e, _DummyVariable, Infinity]]},
   Function @@ {vars, e /. Thread[dummies -> labelFunctionSymbols[Length[dummies]]] /. Thread[labels -> vars]}];

(* The exact equations of many-body theory between full objects, given as heads (e.g. declared blobs, for the pictures)
   or as functions of their labels (LabelFunction of perturbative series, for the checks). Fermions leave at x1, x3 and
   enter at x2, x4; G(a, b) runs from b to a. The vertex equations take an optional "BareVertex" -> gamma, a function of
   three labels (the density, gamma(a, b; z) = delta(a, z) delta(b, z), by default; a current, a spin density). *)
FunctionalDiagrams::exactobject = "ExactEquation[`1`, ...] needs the object `2` (a rule \"`2`\" -> h).";
FunctionalDiagrams::exactname = "Unknown equation `1`; the equations are `2`.";
$exactEquations = {"Dyson", "BetheSalpeter", "Parquet", "SchwingerDyson", "Vertex", "VertexFromF", "SelfEnergyFromVertex", "Hedin",
   "Response", "Susceptibility"};
Options[ExactEquation] = {"Statistics" -> "Fermion", "Channel" -> Automatic, "MaximumOrder" -> None};
ExactEquation[name_String, objects_List, labels_List, opts___Rule] := Module[{statistics = statisticsOption[{opts}], ob, missing,
    a, b, c, d, u, v, w, g, gbar, gam, given, maximum = optionValue[{opts}, Options[ExactEquation], "MaximumOrder"], side},
   If[! MemberQ[$exactEquations, name], Message[FunctionalDiagrams::exactname, name, $exactEquations]; Return[$Failed]];
   missing = Select[Switch[name,
      "Dyson", {"G", "G0", "Sigma"}, "BetheSalpeter", {"F", "Gamma0", "G"},
      "Parquet", If[statistics === "Boson", {"F", "Lambda", "Gammas", "Gammat", "Gammau", "G"}, {"F", "Lambda", "GammaPh", "GammaPhbar", "GammaPp", "G"}],
      "SchwingerDyson", If[statistics === "Boson", {"Sigma", "G", "F", "Coupling"}, {"Sigma", "G", "F", "V"}],
      "Vertex", {"Lambda", "Gamma0", "G"}, "VertexFromF", {"Lambda", "F", "G"}, "SelfEnergyFromVertex", {"Sigma", "G", "V", "Lambda"},
      "Hedin", {"Sigma", "G", "W", "P", "Lambda", "GammaXC", "V"},
      "Response", {"L", "Gamma0", "G"}, "Susceptibility", {"Chi", "L"}], ! MemberQ[optionKey /@ objects[[All, 1]], #] &];
   If[missing =!= {}, Message[FunctionalDiagrams::exactobject, name, First[missing]]; Return[$Failed]];
   ob[k_String] := First[Cases[objects, (key_ -> h_) /; optionKey[key] === k :> h]];
   given[k_String] := MemberQ[optionKey /@ objects[[All, 1]], k];
   {a, b, c, d, u, v, w} = freshSymbols[7];
   g = If[given["G"], ob["G"], None];
   gam = If[given["BareVertex"], ob["BareVertex"], Function[{p, q, r}, DeltaFunction[p, r] DeltaFunction[q, r]]];
   (* both sides in canonical form (an equation whose sides are the same diagrams becomes True); with "MaximumOrder" ->
      {x, n} only their parts of order <= n in x *)
   side[e_] := If[maximum === None, SimplifyDiagrams[e], OrderPart[e, maximum[[1]], Range[0, maximum[[2]]]]];
   Replace[#, HoldPattern[Equal[l_, r_]] :> side[l] == side[r], {0, 1}] &@ Switch[name,
    (* G = G0 + G0 Sigma G *)
    "Dyson", g @@ labels == ob["G0"] @@ labels + IntegralOver[ob["G0"][labels[[1]], u] ob["Sigma"][u, v] g[v, labels[[2]]], {u, v}],
    (* F = Gamma0 + Gamma0 o F in one channel (BetheSalpeterLadder); the particle-hole channel, or s for real bosons *)
    "BetheSalpeter", With[{channel = Replace[optionValue[{opts}, Options[ExactEquation], "Channel"],
        Automatic -> If[statistics === "Boson", "s", "ParticleHole"]]},
     ob["F"] @@ labels == ob["Gamma0"] @@ labels + BetheSalpeterLadder[ob["Gamma0"] @@ labels, ob["F"] @@ labels, labels, channel, g,
       "Statistics" -> statistics, "MaximumOrder" -> maximum]],
    (* F = Lambda + Sum_r Gamma_r o F *)
    "Parquet", ob["F"] @@ labels == ob["Lambda"] @@ labels + Total[Table[
       BetheSalpeterLadder[ob[ch[[1]]] @@ labels, ob["F"] @@ labels, labels, ch[[2]], g, "Statistics" -> statistics, "MaximumOrder" -> maximum],
       {ch, If[statistics === "Boson", {{"Gammas", "s"}, {"Gammat", "t"}, {"Gammau", "u"}},
         {{"GammaPh", "ParticleHole"}, {"GammaPhbar", "TransverseParticleHole"}, {"GammaPp", "ParticleParticle"}}]}]],
    (* fermions with a density-density interaction V: Sigma = Hartree + Fock - V G G G F; real bosons with the interaction
       lambda/4! phi^4: Sigma = -lambda/2 G(x, x) + lambda/6 G G G F *)
    "SchwingerDyson", If[statistics === "Boson",
     ob["Sigma"] @@ labels == -ob["Coupling"]/2 DeltaFunction @@ labels g[labels[[1]], labels[[1]]] +
       ob["Coupling"]/6 IntegralOver[g[labels[[1]], a] g[labels[[1]], b] g[labels[[1]], c] ob["F"][a, b, c, labels[[2]]], {a, b, c}],
     ob["Sigma"] @@ labels == DeltaFunction @@ labels IntegralOver[ob["V"][labels[[1]], u] g[u, u], {u}] - ob["V"] @@ labels g @@ labels -
       IntegralOver[ob["V"][labels[[1]], u] g[labels[[1]], a] g[d, u] g[u, c] ob["F"][a, labels[[2]], c, d], {u, a, c, d}]],
    (* the vertex of the density at z: Lambda = gamma + Gamma0 G G Lambda (fermions), gamma - 1/2 Gamma0 G G Lambda (real
       bosons), gamma(x1, x2; z) = delta(x1, z) delta(x2, z), or the bare vertex given *)
    "Vertex", With[{x1 = labels[[1]], x2 = labels[[2]], z = labels[[3]]}, ob["Lambda"][x1, x2, z] ==
      gam[x1, x2, z] + If[statistics === "Boson",
       -1/2 IntegralOver[ob["Gamma0"][x1, x2, a, b] g[a, c] g[b, d] ob["Lambda"][c, d, z], {a, b, c, d}],
       IntegralOver[ob["Gamma0"][x1, x2, a, b] g[b, c] ob["Lambda"][c, d, z] g[d, a], {a, b, c, d}]]],
    (* Lambda = gamma + F G gamma G *)
    "VertexFromF", With[{x1 = labels[[1]], x2 = labels[[2]], z = labels[[3]]}, ob["Lambda"][x1, x2, z] ==
      gam[x1, x2, z] + Which[
       ! given["BareVertex"] && statistics === "Boson", -1/2 IntegralOver[ob["F"][x1, x2, a, b] g[a, z] g[b, z], {a, b}],
       ! given["BareVertex"], IntegralOver[ob["F"][x1, x2, a, b] g[b, z] g[z, a], {a, b}],
       statistics === "Boson", -1/2 IntegralOver[ob["F"][x1, x2, a, b] g[a, c] g[b, d] gam[c, d, z], {a, b, c, d}],
       True, IntegralOver[ob["F"][x1, x2, a, b] g[b, c] gam[c, d, z] g[d, a], {a, b, c, d}]]],
    (* the equation of motion: Sigma = Hartree - V G Lambda, with the vertex of the density *)
    "SelfEnergyFromVertex", ob["Sigma"] @@ labels == DeltaFunction @@ labels IntegralOver[ob["V"][labels[[1]], u] g[u, u], {u}] -
      IntegralOver[ob["V"][labels[[1]], u] g[labels[[1]], a] ob["Lambda"][a, labels[[2]], u], {u, a}],
    (* Hedin's equations, with the proper vertex Lambda (irreducible with respect to an interaction line), the polarization P,
       the screened interaction W and the kernel GammaXC = dSigma_xc/dG = Gamma0 without its Hartree part *)
    "Hedin", With[{x = labels[[1]], y = labels[[2]], z = labels[[3]]}, {
      ob["Sigma"][x, y] == DeltaFunction[x, y] IntegralOver[ob["V"][x, u] g[u, u], {u}] - IntegralOver[ob["W"][x, u] g[x, a] ob["Lambda"][a, y, u], {u, a}],
      ob["P"][x, y] == IntegralOver[g[x, a] g[b, x] ob["Lambda"][a, b, y], {a, b}],
      ob["W"][x, y] == ob["V"][x, y] + IntegralOver[ob["V"][x, u] ob["P"][u, v] ob["W"][v, y], {u, v}],
      ob["Lambda"][x, y, z] == DeltaFunction[x, z] DeltaFunction[y, z] +
        IntegralOver[ob["GammaXC"][x, y, a, b] g[b, c] ob["Lambda"][c, d, z] g[d, a], {a, b, c, d}]}],
    (* L = L0 + L0 Gamma0 L, the Bethe-Salpeter equation of the response of G to a one-body source U, with L0 the pair of
       propagators. Fermions, with the legs of a four-point vertex (leaving at x1, x3, entering at x2, x4):
       L(x1, x2; x3, x4) = dG(x1, x2)/dU(x4, x3) = -<psi(x1) psibar(x2) psi(x3) psibar(x4)>_c + G(x1, x4) G(x3, x2), for
       the source Integral psibar U psi. Real bosons: L = -2 dG/dU for 1/2 Integral phi U phi, <phi1 phi2 phi3 phi4>_c +
       G13 G24 + G14 G23, and the pair of lines in the channel carries -1/2 *)
    "Response", With[{x1 = labels[[1]], x2 = labels[[2]], x3 = labels[[3]], x4 = labels[[4]], l = ob["L"], gamma0 = ob["Gamma0"]},
     l[x1, x2, x3, x4] == If[statistics === "Boson",
       g[x1, x3] g[x2, x4] + g[x1, x4] g[x2, x3] - 1/2 IntegralOver[g[x1, a] g[x2, b] gamma0[a, b, c, d] l[c, d, x3, x4], {a, b, c, d}],
       g[x1, x4] g[x3, x2] + IntegralOver[g[x1, a] g[b, x2] gamma0[a, b, c, d] l[d, c, x3, x4], {a, b, c, d}]]],
    (* Chi = gamma L gamma: the response of the one-body operator O(z) = Integral psibar(a) gamma(a, b; z) psi(b) (1/2 phi
       gamma phi for real bosons) to its source u(z), Chi(z1, z2) = d<O(z1)>/du(z2) = d^2 Omega/du(z1) du(z2) =
       -<O(z1) O(z2)>_c (the sign of the Lindhard function) *)
    "Susceptibility", ob["Chi"] @@ labels == If[statistics === "Boson",
      -1/4 IntegralOver[gam[a, b, labels[[1]]] ob["L"][a, b, c, d] gam[c, d, labels[[2]]], {a, b, c, d}],
      IntegralOver[gam[a, b, labels[[1]]] ob["L"][b, a, d, c] gam[c, d, labels[[2]]], {a, b, c, d}]]]];

(* the residuals of an exact equation (or of a list of them), order by order in x from 0 to n *)
CheckEquation[equations_List, x_, n_Integer] := CheckEquation[#, x, n] & /@ equations;
CheckEquation[True, x_, n_Integer] := Table[0, {n + 1}];
(* only the terms of order <= n are brought into canonical form (the order of a term does not change under renaming and
   delta integration; terms with nested integrals are tested again after it), and split by order in one pass *)
CheckEquation[HoldPattern[Equal[lhs_, rhs_]], x_, n_Integer] := With[{headQ = ! FreeQ[{lhs, rhs}, x[___]]},
   With[{terms = selectCanonicalTermList[rawTermsOf[lhs - rhs], termOrder[#, x, headQ] <= n &]},
    termListToExpression /@ orderPartsOfTerms[terms, x, ! FreeQ[termListToExpression[terms], x[___]], n]]];

(* ---- auxiliary fields: Hubbard-Stratonovich, and the converse ---- *)
(* An interaction 1/2 Integral K(x, y) O(x) O(y), with O a local operator of even Grassmann parity (a density psibar psi, or
   phi^2), is the exchange of a real boson phi with <phi phi>_0 = K coupled by -i Integral phi O, because
   <exp(i Integral phi O)>_phi = exp(-1/2 Integral O K O). The kernel K is read off the terms of the interaction: a term
   with two labels gives K = kernel(x, y) + kernel(y, x), a local term (one label) K = 2 kernel(x) delta(x, y); the sign of
   the reordering of the fields is found by comparing the terms with 1/2 O K O. *)
FunctionalDiagrams::hsform = "The term `1` of the interaction is not of the form 1/2 Integral K(x, y) O(x) O(y) for the operator O given; it is kept as it is.";
FunctionalDiagrams::hsnone = "No term of the interaction is of the form 1/2 Integral K(x, y) O(x) O(y) for the operator O given.";
FunctionalDiagrams::hshead = "The kernel `1` is not a number times a propagator h[x, y]: give the head of the propagator of the auxiliary field.";
fieldHeadsQ[f_] := MemberQ[$fieldSourceMap[[All, 1]], If[Head[f] === Power, Head[f[[1]]], Head[f]]];
hsKernelOfTerm[diagramTerm[c_, o_, d_], operator_] := Module[{factors = expandIntegerPowers[factorList[c]], kern, x, y, k, candidate, term},
   term = termToExpression[diagramTerm[c, o, d]];
   kern = Times @@ Select[factors, ! fieldHeadsQ[#] &];
   {x, y} = freshSymbols[2];
   k = Switch[Length[d],
     2, (kern /. Thread[d -> {x, y}]) + (kern /. Thread[d -> {y, x}]),
     1, 2 (kern /. d[[1]] -> x) DeltaFunction[x, y],
     _, Return[None]];
   candidate[sign_] := SimplifyDiagrams[term - sign/2 IntegralOver[k operator[x] ** operator[y], {x, y}]];
   Which[candidate[1] === 0, {x, y, k}, candidate[-1] === 0, {x, y, -k}, True, None]];
HubbardStratonovich[{z0_, interaction_}, operator_, {field_Symbol, source_Symbol}, propagator_] := Module[
   {terms = canonicalTermsOf[interaction], kernels, matched, kept, x, y, kernel, d, coupling, kernelFunction, a, b, z},
   kernels = hsKernelOfTerm[#, operator] & /@ terms;
   matched = Pick[terms, (# =!= None &) /@ kernels]; kept = Pick[terms, (# === None &) /@ kernels];
   Scan[Message[FunctionalDiagrams::hsform, userForm[termToExpression[#]]] &, kept];
   If[matched === {}, Message[FunctionalDiagrams::hsnone]; Return[$Failed]];
   {x, y} = freshSymbols[2];
   kernel = SimplifyDiagrams[Total[Table[k[[3]] /. {k[[1]] -> x, k[[2]] -> y}, {k, DeleteCases[kernels, None]}]]];
   kernelFunction = LabelFunction[kernel, {x, y}];
   (* the propagator of the auxiliary field: the head of the kernel (Automatic), with the number in front in the coupling;
      or a new head standing for the whole kernel *)
   {d, coupling} = If[propagator === Automatic,
     With[{heads = Cases[{kernel}, h_Symbol[x, y] | h_Symbol[y, x] :> h, Infinity]},
      If[Length[DeleteDuplicates[heads]] == 1 && FreeQ[Simplify[kernel/heads[[1]][x, y]], x | y | IntegralOver],
       {heads[[1]], Sqrt[Simplify[kernel/heads[[1]][x, y]]]},
       Message[FunctionalDiagrams::hshead, userForm[kernel]]; Return[$Failed]]],
     {propagator, 1}];
   DeclareSource[source]; DeclareField[field, source]; DeclareSymmetric[d];
   If[lineStyleOf[d] === "Plain", DeclareInteractionLine[d]];
   {a, b, z} = freshSymbols[3];
   With[{rules = {"FreeGeneratingFunctional" -> z0 Exp[1/2 IntegralOver[source[a] d[a, b] source[b], {a, b}]],
       "Interaction" -> SimplifyDiagrams[termListToExpression[kept] - I coupling IntegralOver[field[z] operator[z], {z}]],
       "Propagator" -> d, "Kernel" -> kernelFunction}}, Association[rules]]];

(* The converse: a real Gaussian field with <phi phi>_0 = D, coupled linearly, Integral phi(z) A(z), integrated out:
   <exp(-Integral phi A)>_phi = exp(1/2 Integral A D A), so that the interaction becomes -1/2 Integral A(x) D(x, y) A(y) *)
FunctionalDiagrams::outsource = "`1` has no Gaussian factor exp(1/2 Integral `2`[x] D(x, y) `2`[y]).";
FunctionalDiagrams::outlinear = "The field `1` does not enter the interaction linearly: it cannot be integrated out exactly.";
IntegrateOutField[{z0_, interaction_}, field_Symbol, source_Symbol] := Module[{exponents, gaussian, rest, a, b, w, x, y, propagatorFunction,
    terms = canonicalTermsOf[interaction], linear, others, coupledTo, coupled},
   (* the exponent with the source, and the propagator D(a, b) = its second derivative *)
   exponents = Cases[{z0}, Exp[e_] /; ! FreeQ[e, source] :> e, Infinity];
   If[Length[exponents] != 1, Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   {a, b, w, x, y} = freshSymbols[5];
   gaussian = SetSourcesToZero[FunctionalDerivative[exponents[[1]], source[a], source[b]]];
   If[! FreeQ[gaussian, source] || gaussian === 0, Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   propagatorFunction = LabelFunction[gaussian, {a, b}];
   rest = SimplifyDiagrams[exponents[[1]] - 1/2 IntegralOver[source[x] propagatorFunction[x, y] source[y], {x, y}]];
   If[! FreeQ[rest, source], Message[FunctionalDiagrams::outsource, userForm[z0], source]; Return[$Failed]];
   (* the terms with the field, each linear in it: phi(u) times A, with u renamed w *)
   coupledTo[t : diagramTerm[c_, o_, d_]] := With[{f = Cases[expandIntegerPowers[factorList[c]], field[u_] :> u]},
     If[Length[f] != 1 || ! FreeQ[c /. field[f[[1]]] -> 1, field], $Failed,
      termToExpression[diagramTerm[(c /. field[f[[1]]] -> 1) /. f[[1]] -> w, o /. f[[1]] -> w, DeleteCases[d, f[[1]]]]]]];
   linear = Select[terms, ! FreeQ[#, field] &]; others = Select[terms, FreeQ[#, field] &];
   coupled = coupledTo /@ linear;
   If[MemberQ[coupled, $Failed] || linear === {}, Message[FunctionalDiagrams::outlinear, field]; Return[$Failed]];
   With[{af = LabelFunction[Total[coupled], {w}]},
    With[{rules = {"FreeGeneratingFunctional" -> (z0 /. Exp[exponents[[1]]] -> Exp[rest]),
        "Interaction" -> SimplifyDiagrams[termListToExpression[others] -
          1/2 IntegralOver[propagatorFunction[x, y] af[x] ** af[y], {x, y}]]}}, Association[rules]]]];

(* ========================================================================= *)
(*  13. Connected functions from 1PI vertices (the tree expansion)           *)
(* ========================================================================= *)

defineTreeDisplayForms[] := (
   defineDisplayForm[FullPropagator[a_, b_], Interpretation[Subscript["G", Row[{a, b}]], FullPropagator[a, b]]];
   defineDisplayForm[VertexFunction[a__], Interpretation[Subsuperscript["\[CapitalGamma]", Row[{a}], Row[{"(", Length[{a}], ")"}]],
     VertexFunction[a]]];);
defineTreeDisplayForms[];
defineDisplayForm[MeanField[a_], Interpretation[Subscript[OverBar["\[Phi]"], a], MeanField[a]]];

(* the rules and declarations of the three modes removed, so that the state after a switch does not depend on the
   previous mode *)
clearTreeRules[] := (
   Clear[FullPropagator, VertexFunction]; ClearAttributes[{FullPropagator, VertexFunction}, Orderless];
   $lineStyles = DeleteCases[$lineStyles, {FullPropagator, _}];
   $derivativeRules = Select[$derivativeRules, FreeQ[#, ExternalSource] &];
   $derivativePairs = Select[$derivativePairs, FreeQ[#, ExternalSource] &];
   $grassmannSources = DeleteCases[$grassmannSources, ExternalSource];
   $bosonicSources = DeleteCases[$bosonicSources, ExternalSource];
   $gradedHeads = Complement[$gradedHeads, {FullPropagator, VertexFunction, MeanField}];
   $antisymmetricHeads = Complement[$antisymmetricHeads, {FullPropagator, VertexFunction}];
   $symmetricHeads = Complement[$symmetricHeads, {FullPropagator, VertexFunction}];);

setupTreeRules["Boson"] := (
   clearTreeRules[];
   DeclareSource[ExternalSource]; DeclareSymmetric[FullPropagator, VertexFunction];
   (* dphibar_a/dJ_b = G_ab,  dG_ab/dJ_c = -G_ax G_by G_cz Gamma_xyz,  dGamma_a../dJ_b = Gamma_a..x G_xb *)
   DeclareFunctionalDerivative[MeanField[a_], ExternalSource[b_], FullPropagator[a, b]];
   DeclareFunctionalDerivative[FullPropagator[a_, b_], ExternalSource[c_],
    -IntegralOver[FullPropagator[a, x] FullPropagator[b, y] FullPropagator[c, z] VertexFunction[x, y, z], {x, y, z}]];
   DeclareFunctionalDerivative[VertexFunction[as__], ExternalSource[b_], IntegralOver[VertexFunction[as, x] FullPropagator[x, b], {x}]];
   defineTreeDisplayForms[];);

setupTreeRules["Fermion"] := (
   clearTreeRules[];
   DeclareGrassmannSource[ExternalSource]; DeclareGrassmannGraded[FullPropagator, VertexFunction, MeanField];
   DeclareAntisymmetric[FullPropagator, VertexFunction];
   (* superfield rules: d_c phibar_a = G_ca,  d_c G_ab = G_ae G_cd Gamma_def G_fb,  d_c Gamma_a.. = G_cd Gamma_da.. *)
   DeclareFunctionalDerivative[MeanField[a_], ExternalSource[c_], FullPropagator[c, a]];
   DeclareFunctionalDerivative[FullPropagator[a_, b_], ExternalSource[c_],
    IntegralOver[FullPropagator[a, e] FullPropagator[c, dd] VertexFunction[dd, e, f] FullPropagator[f, b], {dd, e, f}]];
   DeclareFunctionalDerivative[VertexFunction[as__], ExternalSource[c_], IntegralOver[FullPropagator[c, dd] VertexFunction[dd, as], {dd}]];
   defineTreeDisplayForms[];);

(* ---- fermion lines: the superfield trees projected on the components psi, psibar ----
   A superfield label a = (x, psi) or (x, psibar); at zero source only the components that conserve the number of
   fermions survive. A line G_ab is <psi(x) psibar(y)>_c = -G(x, y) for a = (x, psi), b = (y, psibar), with the
   many-body propagator G = -<psi psibar> (a line from y to x), and G(y, x) for the reverse; a vertex Gamma_a1...an becomes VertexFunction[outs..., ins...], the legs where the
   fermion lines leave the vertex (its psibar components) and then those where they enter it (its psi components), with
   the sign of that permutation. VertexFunction is then antisymmetric within the outs and within the ins. *)
halfAntisymmetricForm[h_, args_List] := With[{m = Length[args]/2}, With[{o = Take[args, m], i = Drop[args, m]},
    If[hasDuplicatesQ[o] || hasDuplicatesQ[i], 0, permutationSign[o] permutationSign[i] (h @@ Join[Sort[o], Sort[i]])]]];
halfUnsortedQ[args_List] := EvenQ[Length[args]] && With[{m = Length[args]/2},
    Take[args, m] =!= Sort[Take[args, m]] || Drop[args, m] =!= Sort[Drop[args, m]] || hasDuplicatesQ[Take[args, m]] ||
     hasDuplicatesQ[Drop[args, m]]];
(* components: 1 for psi, 2 for psibar *)
fermionLineFactor[f_, comp_] := f /. {
    FullPropagator[p_, q_] :> Which[{comp[p], comp[q]} === {1, 2}, -lineG[p, q], {comp[p], comp[q]} === {2, 1}, lineG[q, p], True, 0],
    VertexFunction[ps__] :> With[{cs = comp /@ {ps}}, If[Count[cs, 1] =!= Count[cs, 2], 0,
       With[{order = Join[Select[Range[Length[cs]], cs[[#]] === 2 &], Select[Range[Length[cs]], cs[[#]] === 1 &]]},
        permutationSign[order] (lineVertex @@ ({ps}[[order]]))]]]};
(* the superfield trees e, with the components of the external labels (rules label -> 1 | 2), summed over the
   components of the integration variables *)
lineVertex[args___] /; halfUnsortedQ[{args}] := halfAntisymmetricForm[lineVertex, {args}];
fermionLinesFromSuperfield[e_, external_List] := SimplifyDiagrams[Total[
     (# /. IntegralOver[f_, d_] :> IntegralOver[Total[Table[fermionLineFactor[f, Function[x, x /. Join[external, Thread[d -> tuple]]]],
           {tuple, Tuples[{1, 2}, Length[d]]}]], d]) & /@ DiagramTerms[e]]];

(* the results with fermion lines: FullPropagator directed, VertexFunction antisymmetric within its outs and its ins *)
setupTreeRules["FermionLines"] := (
   clearTreeRules[];
   VertexFunction[args___] /; halfUnsortedQ[{args}] := halfAntisymmetricForm[VertexFunction, {args}];
   $lineStyles = Append[$lineStyles, {FullPropagator, "Arrow"}];
   defineTreeDisplayForms[];);

ConnectedFunctionFromVertices::fermionlines = "The directions `1` must be \"Out\" or \"In\", as many of each, one for every label.";
fermionLineTrees[labels_List, directions_List] := Module[{trees},
   setupTreeRules["Fermion"];
   trees = fermionLinesFromSuperfield[termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[
        toTermList[FullPropagator[labels[[-2]], labels[[-1]]]], ExternalSource /@ Drop[labels, -2]]]],
     Thread[labels -> (directions /. {"Out" -> 1, "In" -> 2})]];
   setupTreeRules["FermionLines"];
   SimplifyDiagrams[trees /. {lineG -> FullPropagator, lineVertex -> VertexFunction}]];

ConnectedFunctionFromVertices[n_Integer, type_String : "Boson"] :=
  ConnectedFunctionFromVertices[Table[Symbol["x" <> ToString[i]], {i, n}], type];
ConnectedFunctionFromVertices::labels = "At least two labels are needed; got `1`.";
ConnectedFunctionFromVertices[labels_List, type_String : "Boson"] /; Length[labels] < 2 :=
  (Message[ConnectedFunctionFromVertices::labels, labels]; $Failed);
ConnectedFunctionFromVertices[labels_List, "FermionLines", directions_ : Automatic] /; Length[labels] >= 2 :=
  With[{dirs = If[directions === Automatic, Table[If[OddQ[k], "Out", "In"], {k, Length[labels]}], directions]},
   If[! ListQ[dirs] || Length[dirs] =!= Length[labels] || ! SubsetQ[{"Out", "In"}, dirs] || Count[dirs, "Out"] =!= Count[dirs, "In"],
    Message[ConnectedFunctionFromVertices::fermionlines, directions]; $Failed, fermionLineTrees[labels, dirs]]];
ConnectedFunctionFromVertices[labels_List, type_String : "Boson"] /; Length[labels] >= 2 && type =!= "FermionLines" := Module[{n = Length[labels]},
   setupTreeRules[type];
   (* W^(n) = d_1 ... d_(n-2) G_(n-1, n); terms that cannot survive at zero source are dropped early *)
   termListToExpression[canonicalTermList[differentiateAndSetSourcesToZero[toTermList[FullPropagator[labels[[n - 1]], labels[[n]]]],
      ExternalSource /@ Take[labels, n - 2]]]]];

(* ========================================================================= *)
(*  13a. Superfields: the components                                        *)
(*                                                                           *)
(*  A superfield NambuField_a, a = (x, s), is an ordinary Grassmann field of *)
(*  the package with plain labels; its components (psi, psibar for the Nambu *)
(*  field) live only here. SuperfieldComponents assigns a component to every *)
(*  label of every term (the vertices first, zero entries dropped at once)   *)
(*  and replaces the superfield objects by their components: a propagator    *)
(*  calG = [[G_psipsi, G], [-G^T, G_psibarpsibar]], any two-label kernel as  *)
(*  a matrix, a vertex by the derivatives of the component interaction, the  *)
(*  field and its mean by their components; traces and kernels of operators  *)
(*  of heads without anomalous blocks by the normal block.                   *)
(* ========================================================================= *)

SuperfieldComponents::field = "The rules `1` give no superfield: a rule field -> {component, ...} for a declared field (the Nambu field NambuField -> {psi, psibar}) is required.";
SuperfieldComponents::head = "`1` has no rule (a superfield head, or a factor that carries a label of the superfield): give its components, a propagator h -> {G, Gpsipsi, Gpsibarpsibar}, a matrix of heads h -> {{h11, h12}, {h21, h22}}, a mean m -> {m1, m2}, or for a vertex the component interaction h -> S.";
SuperfieldComponents::labels = "The free label `1` is not among the external labels `2`: give label -> component[new label], the component one of `3`.";
SuperfieldComponents::trace = "`1` has no component form: the trace of an operator that mixes the components through anomalous blocks (Tr ln of a Nambu matrix with anomalous blocks) is not a sum of traces of the blocks, and an operator without a rule has no known blocks. It is kept as it is, beside the components of the other terms, and its derivatives with respect to the components are not taken (they vanish for the means); with the anomalous blocks set to 0 (h -> {G, 0, 0}) it has components.";
SuperfieldComponents::rule = "The rule `1` is not understood: the right side must be a list of the components of a field or a mean, a list {G, Gpsipsi, Gpsibarpsibar} (two components), a matrix of entries (heads, 0 or functions of two labels) or the component interaction of a vertex.";

SuperfieldComponents[e_, externals_List, rules_List] := Catch[superfieldComponents[e, externals, rules], superfieldFailure];
SetAttributes[superfieldFailure, HoldFirst];
superfieldFailure[message_, args___] := (Message[message, args]; Throw[$Failed, superfieldFailure]);

superfieldComponents[e_, externals_List, rules_List] := Module[{fieldRules, field, components, n, meanRules, matrixRules = {}, vertexRules = {},
    oneLabel, proxies, e1, kernelRules = {}, types, normal, terms, externalComponents, renaming, cache, result},
   fieldRules = Select[rules, MatchQ[#, (_Symbol -> {__Symbol})] && MemberQ[$fieldSourceMap[[All, 1]], #[[1]]] &];
   If[fieldRules === {}, superfieldFailure[SuperfieldComponents::field, rules]];
   {field, components} = List @@ First[fieldRules];
   n = Length[components];
   meanRules = Select[rules, MatchQ[#, (_Symbol -> {__Symbol})] && Length[#[[2]]] == n && # =!= First[fieldRules] &];
   Scan[Function[r, Which[
       MemberQ[meanRules, r] || r === First[fieldRules], Null,
       MatchQ[r, _Symbol -> {_, _, _}] && n == 2, AppendTo[matrixRules, r[[1]] -> propagatorMatrix[r[[2]]]],
       MatchQ[r, _Symbol -> _List] && Dimensions[r[[2]], 2] === {n, n}, AppendTo[matrixRules, r[[1]] -> r[[2]]],
       MatchQ[r, _Symbol -> _] && ! ListQ[r[[2]]], AppendTo[vertexRules, r],
       True, superfieldFailure[SuperfieldComponents::rule, r]]], rules];
   (* the means of a Grassmann-odd superfield are Grassmann-odd: their components too *)
   Scan[Function[r, If[grassmannOddAtomQ[r[[1]][Unique["FunctionalDiagrams`Private`x"]]],
       Scan[If[! MemberQ[$grassmannSources, #] && ! MemberQ[$grassmannOddHeads, #], DeclareGrassmannSource[#]] &, r[[2]]]]], meanRules];
   oneLabel = Join[{First[fieldRules]}, meanRules];
   (* the external labels: label -> component index, label -> new label *)
   externalComponents = Function[r, If[! MatchQ[r, _ -> _[_]] || ! MemberQ[components, Head[r[[2]]]],
        superfieldFailure[SuperfieldComponents::labels, r, externals, components], r[[1]] -> First[Flatten[Position[components, Head[r[[2]]]]]]]] /@ externals;
   renaming = (#[[1]] -> First[#[[2]]] &) /@ externals;
   (* the operators: heads without anomalous blocks are "row" ([[0, p], [-p^T, 0]], a propagator) or "column" ([[0, -q^T],
      [q, 0]], an inverse propagator or a one-body source) operators, represented by their normal block *)
   types = Select[(#[[1]] -> normalBlockType[#[[2]]] &) /@ matrixRules, #[[2]] =!= None &];
   normal = (#[[1]] -> #[[2, 2]] &) /@ types;
   (* the traces first (a trace of products that has no component form as a whole is written as rings, whose kernels the
      second replacement projects; a trace kept as it is, keptTrace, is restored at the end) *)
   e1 = e /. {
      TraceLog[a_] /; superfieldOperatorQ[a, matrixRules] :> superfieldTraceLog[a, types, normal],
      OperatorTrace[a_] /; superfieldOperatorQ[a, matrixRules] :> superfieldTrace[a, types, normal]};
   e1 = e1 /. {
      OperatorKernel[a_][x_, y_] /; superfieldOperatorQ[a, matrixRules] :> With[{k = Unique["FunctionalDiagrams`Private`kernel"]},
        AppendTo[kernelRules, k -> superfieldKernelMatrix[a, types, normal]]; k[x, y]]};
   (* the traces kept as they are: their superfield heads and the components of these, for the message supertrace of a
      later derivative with respect to a component *)
   Scan[Function[k, Scan[Function[h, $superfieldTraceComponents = Union[$superfieldTraceComponents,
          {h -> superfieldComponentHeads[h /. matrixRules]}]], Select[matrixRules[[All, 1]], ! FreeQ[k[[2]], #] &]]],
     Cases[e1, _keptTrace, {0, Infinity}]];
   matrixRules = Join[matrixRules, kernelRules];
   proxies = Table[Unique["FunctionalDiagrams`Private`component"], {n}];
   Block[{$grassmannSources = Union[$grassmannSources, proxies]},
    (* the components of a vertex h(a1, ..., ak) = d_ak ... d_a1 S of the component interaction S, computed once per
       tuple of components with fresh labels *)
    cache[h_, comps_List] := cache[h, comps] = Module[{ys = freshSymbols[Length[comps]]},
       {ys, SimplifyDiagrams[FunctionalDerivative[(h /. vertexRules) /. Thread[components -> proxies],
          Sequence @@ Reverse[MapThread[proxies[[#1]][#2] &, {comps, ys}]]]]}];
    terms = canonicalTermsOf[e1];
    result = Total[projectSuperfieldTerm[#, n, externalComponents, matrixRules, vertexRules, oneLabel, components, cache] & /@ terms]];
   SimplifyDiagrams[result /. renaming /. keptTrace[h_, a_] :> h[a]]];

(* the heads of the entries of a matrix of components (symbols, 0, or functions of two labels such as -G[#2, #1] &) *)
$superfieldTraceComponents = {};
superfieldComponentHeads[m_] := Union[Cases[Flatten[{m}], s_Symbol /; s =!= 0], Cases[m, s_Symbol[_, _] :> s, {0, Infinity}]];

(* {G, Gpsipsi, Gpsibarpsibar} as the matrix [[Gpsipsi, G], [-G^T, Gpsibarpsibar]] *)
propagatorMatrix[{g_, gpp_, gbb_}] := {{gpp, g}, {If[g === 0, 0, Function[{x, y}, -g[y, x]]], gbb}};
normalBlockType[m_List] := Which[Dimensions[m] =!= {2, 2} || m[[1, 1]] =!= 0 || m[[2, 2]] =!= 0, None,
   Head[m[[1, 2]]] === Symbol && m[[1, 2]] =!= 0, {"row", m[[1, 2]]},
   Head[m[[2, 1]]] === Symbol && m[[2, 1]] =!= 0, {"col", m[[2, 1]]},
   True, None];
superfieldOperatorQ[a_, matrixRules_List] := ! FreeQ[a, Alternatives @@ (matrixRules[[All, 1]])] && matrixRules =!= {};
swapOperatorType["row"] = "col"; swapOperatorType["col"] = "row"; swapOperatorType[_] = None;
superfieldAtomType[h_Symbol, types_List] := Replace[h, Append[(#[[1]] -> #[[2, 1]] &) /@ types, _ -> None]];
superfieldAtomType[Power[b_, -1], types_List] := swapOperatorType[If[Head[b] === Symbol, superfieldAtomType[b, types], superfieldOperatorType[b, types]]];
superfieldAtomType[_, _] := None;
(* the type of an operator all of whose monomials are single atoms of one type *)
superfieldOperatorType[a_, types_List] := With[{m = Catch[collectMonomials[opMonomials[a]], operatorFailure]},
   If[! ListQ[m] || m === {}, None,
    With[{t = (If[Length[#[[2]]] == 1, superfieldAtomType[#[[2, 1]], types], None] &) /@ m}, If[Length[Union[t]] == 1, First[t], None]]]];
(* det(c P) = det(c p)^2 for P = [[0, p], [-p^T, 0]], and so for the column operators: Tr ln -> 2 Tr ln of the normal block *)
superfieldTraceLog[a_, types_List, normal_List] := If[superfieldOperatorType[a, types] === None,
   Message[SuperfieldComponents::trace, TraceLog[a]]; keptTrace[TraceLog, a], 2 TraceLog[a /. normal]];
(* Tr(Q . P) = Tr(P . Q) = 2 Tr(q p), Tr 1 doubles: every monomial an identity or a product of a column and a row operator;
   otherwise the identity monomials double and the others are written as rings (ExpandTrace), projected term by term *)
superfieldTrace[a_, types_List, normal_List] := With[{m = Catch[collectMonomials[opMonomials[a]], operatorFailure]},
   Which[
    ! ListQ[m], Message[SuperfieldComponents::trace, OperatorTrace[a]]; keptTrace[OperatorTrace, a],
    AllTrue[m, Function[mono, mono[[2]] === {} || (Length[mono[[2]]] == 2 && Sort[superfieldAtomType[#, types] & /@ mono[[2]]] === {"col", "row"})]],
    2 OperatorTrace[a /. normal],
    True, With[{identity = Total[Select[m, #[[2]] === {} &][[All, 1]]], rings = ExpandTrace[OperatorTrace[opFromMonomials[Select[m, #[[2]] =!= {} &]]]]},
     If[! FreeQ[rings, OperatorTrace | $Failed], Message[SuperfieldComponents::trace, OperatorTrace[a]]; keptTrace[OperatorTrace, a],
      2 identity OperatorTrace[1] + rings]]]];
(* the kernel of a row operator [[0, p], [-p^T, 0]] or of a column operator [[0, -q^T], [q, 0]] *)
superfieldKernelMatrix[a_, types_List, normal_List] := With[{t = superfieldOperatorType[a, types], b = a /. normal},
   With[{k = If[Head[b] === Symbol, b, OperatorKernel[b]]},
    Switch[t,
     "row", {{0, k}, {Function[{x, y}, -k[y, x]], 0}},
     "col", {{0, Function[{x, y}, -k[y, x]]}, {k, 0}},
     _, superfieldFailure[SuperfieldComponents::trace, OperatorKernel[a]]]]];

(* the value of a factor for an assignment of components (a list of rules label -> component) *)
superfieldEntry[0, _, _] := 0;
superfieldEntry[entry_, x_, y_] := entry[x, y];
superfieldFactorValue[f_, asg_List, n_, matrixRules_, vertexRules_, oneLabel_, components_, cache_] := With[{h = Head[f], comp = Replace[#, asg] &},
   Which[
    h === DeltaFunction, If[comp[f[[1]]] === comp[f[[2]]], f, 0],
    MemberQ[matrixRules[[All, 1]], h], superfieldEntry[(h /. matrixRules)[[comp[f[[1]]], comp[f[[2]]]]], f[[1]], f[[2]]],
    MemberQ[vertexRules[[All, 1]], h], With[{c = cache[h, comp /@ (List @@ f)]}, c[[2]] /. Thread[c[[1]] -> (List @@ f)]],
    MemberQ[oneLabel[[All, 1]], h], (h /. oneLabel)[[comp[f[[1]]]]] @@ f,
    True, f]];
superfieldFactorQ[f_, matrixRules_, vertexRules_, oneLabel_] := ! AtomQ[f] && (Head[f] === DeltaFunction ||
    (MemberQ[matrixRules[[All, 1]], Head[f]] && Length[f] == 2) || MemberQ[vertexRules[[All, 1]], Head[f]] ||
    (MemberQ[oneLabel[[All, 1]], Head[f]] && Length[f] == 1));

projectSuperfieldTerm[diagramTerm[c_, o_List, d_List], n_, externalComponents_, matrixRules_, vertexRules_, oneLabel_, components_, cache_] :=
  Module[{fresh = freshSymbols[Length[d]], factors, odd, projectable, others, labels, free, start, asgs, value, auts, groups},
   factors = expandIntegerPowers[factorList[c /. Thread[d -> fresh]]];
   odd = o /. Thread[d -> fresh];
   projectable = Select[Join[factors, odd], superfieldFactorQ[#, matrixRules, vertexRules, oneLabel] &];
   others = Select[Join[factors, odd], ! superfieldFactorQ[#, matrixRules, vertexRules, oneLabel] &];
   labels = Union[Flatten[(List @@ #) & /@ projectable]];
   Scan[If[! AtomQ[#] && (MemberQ[$antisymmetricHeads, Head[#]] || ! FreeQ[#, Alternatives @@ Append[labels, Unique[]]]),
      superfieldFailure[SuperfieldComponents::head, #]] &, others];
   free = Complement[labels, fresh];
   Scan[If[! MemberQ[externalComponents[[All, 1]], #], superfieldFailure[SuperfieldComponents::labels, #, externalComponents[[All, 1]], components]] &, free];
   start = Select[externalComponents, MemberQ[free, #[[1]]] &];
   value = superfieldFactorValue[#1, #2, n, matrixRules, vertexRules, oneLabel, components, cache] &;
   (* the assignments, factor by factor (the factors with most labels first), the zero ones dropped as they appear *)
   asgs = Fold[Function[{current, f}, Join @@ Map[Function[asg, With[{new = Select[DeleteDuplicates[List @@ f], ! MemberQ[First /@ asg, #] &]},
          Select[(Join[asg, Thread[new -> #]] &) /@ Tuples[Range[n], Length[new]], value[f, #] =!= 0 &]]], current]],
     {start}, SortBy[projectable, -Length[DeleteDuplicates[List @@ #]] &]];
   (* the assignments related by an automorphism of the term give the same diagram: one of each orbit, times its size *)
   auts = termAutomorphisms[Times @@ factors, odd, fresh];
   groups = If[Length[auts] < 2, List /@ asgs, GatherBy[asgs, Function[asg, With[{v = fresh /. asg}, First[Sort[(v[[#]] &) /@ auts]]]]]];
   Total[Function[g, Length[g] termToExpression[diagramTerm[(Times @@ (value[#, First[g]] & /@ factors)), value[#, First[g]] & /@ odd, fresh]]] /@ groups]];

(* The automorphisms of a term c o with the integration variables d (atomic symbols): the permutations p of d (d[[i]]
   -> d[[p[[i]]]]) under which the term is unchanged, its sign included. Backtracking within the classes of the colour
   refinement (an invariant colouring), a factor checked as soon as its variables are mapped (its image must be a factor
   of the term, up to the order of the labels of a symmetric head), every complete candidate checked with termKey.
   Only the identity when the term has nested integrals or the search would be long (no reduction then). *)
$automorphismSearchLimit = 20000;
termAutomorphisms[c_, o_List, d_List] := Module[{factors, inerts, data, incidence, colours, present, heads, k1, order, nodes = 0, auts = {}, search, n = Length[d]},
   If[n < 2 || ! FreeQ[c, IntegralOver], Return[{Range[n]}]];
   factors = Select[Join[factorList[c], o], ! FreeQ[#, Alternatives @@ d] &];
   inerts = inertForm /@ factors;
   data = refinementData[inerts, d];
   incidence = data[[4]];
   colours = colourRefinement[data, Table[1, {n}]];
   If[Length[Union[colours]] == n, Return[{Range[n]}]];
   present = Union[normalizedInertForm /@ inerts];
   heads = ({#[[1]], Length[#[[2]]]} &) /@ inerts;
   k1 = termKey[c, o, {}];
   (* the variables in the order of the search: each next one sharing most factors with those before it (so that factors
      are checked early), then of the smallest class *)
   order = {First[SortBy[Range[n], {Count[colours, colours[[#]]], #} &]]};
   Do[With[{rest = Complement[Range[n], order]},
     AppendTo[order, First[SortBy[rest, {-Length[Intersection[incidence[[#]], Join @@ incidence[[order]]]], Count[colours, colours[[#]]], #} &]]]],
    {n - 1}];
   search[img_List, pos_Integer] := If[pos > n,
     If[termKey[c, o, Thread[d -> d[[img]]]] === k1, AppendTo[auts, img]],
     With[{i = order[[pos]]},
      Do[If[colours[[j]] == colours[[i]] && ! MemberQ[img, j],
        If[++nodes > $automorphismSearchLimit, Throw[{Range[n]}, automorphismSearch]];
        With[{img2 = ReplacePart[img, i -> j]},
         If[AllTrue[incidence[[i]], Function[k, With[{vars = data[[3, k]]}, With[{images = Select[img2[[vars]], # > 0 &]},
              If[Length[images] == Length[vars],
               MemberQ[present, normalizedInertForm[inerts[[k]] /. Thread[d[[vars]] -> d[[img2[[vars]]]]]]],
               (* a factor mapped in part: the images of its variables mapped so far lie in one factor with its head *)
               AnyTrue[incidence[[First[images]]], Function[k2, heads[[k2]] === heads[[k]] && Complement[images, data[[3, k2]]] === {}]]]]]]],
          search[img2, pos + 1]]]], {j, n}]]];
   Catch[search[Table[0, {n}], 1]; If[auts === {}, {Range[n]}, auts], automorphismSearch]];

(* ========================================================================= *)
(*  14. Pictures                                                             *)
(*  Only basic graphics primitives (Line, Disk, Polygon, Text) and a         *)
(*  deterministic layout, so that the same geometry can be exported to TikZ. *)
(*                                                                           *)
(*  A term is drawn as follows. Labels joined by a DeltaFunction are one     *)
(*  point. Two-label factors are lines (propagators or interaction lines),   *)
(*  factors with three or more labels are blobs (hatched; a vertex function  *)
(*  VertexFunction cross-hatched), one-label factors are source insertions.  *)
(*  An external label that ends a single propagator is an external point of  *)
(*  a correlation function (open circle); any other external label sits on a *)
(*  vertex of an amputated function (self-energy, vertex) and gets an        *)
(*  external leg, with an arrow for fermions.                                *)
(* ========================================================================= *)

$drawingDefaults = {"VertexLabels" -> Automatic, "Interactions" -> {}, "Directed" -> {}, "InteractionStyle" -> Automatic,
   "Columns" -> Automatic, "ImageSize" -> Automatic, "Output" -> "Graphics", "Weights" -> True, "ExternalOrder" -> Automatic,
   "LegDirections" -> Automatic, "AmputatedLegs" -> Automatic, "Scale" -> 0.7, "Labels" -> {}, "LineWidth" -> Automatic,
   "LabelFont" -> "\\small", "Couplings" -> {}, "Appearance" -> Automatic};
drawingOption[opts_List, name_String] := optionValue[opts, $drawingDefaults, name];

(* the colours of the pictures of Mathematica (the TikZ code is for white pages and has none), as {light, dark}: the
   lines and the text, the paper (what hides the lines under it: the inside of a blob, of an external point or of a
   source, the clearing behind the label of a hatched or dotted blob), the fill "Grey" of a blob and the frames of the cells
   of DiagramPlot. "Appearance" -> Automatic follows the notebook (dark lines on a light notebook,
   light ones on a dark notebook) through LightDarkSwitched, of Mathematica 14.3 or later, the paper being then the
   background of the notebook itself; before 14.3 the light colours. "Light" and "Dark" force one of them, on a
   background of the paper colour (for a picture exported or shown on a page of the other kind) *)
$diagramPalette = <|"Foreground" -> {GrayLevel[0], GrayLevel[0.9]}, "Paper" -> {GrayLevel[1], GrayLevel[0.1]},
    "GreyFill" -> {GrayLevel[0.8], GrayLevel[0.42]}, "CellFrame" -> {GrayLevel[0.85], GrayLevel[0.3]}|>;
(* a colour for light notebooks and one for dark notebooks (LightDarkSwitched, Mathematica 14.3 or later; dark ->
   Automatic: the dark variant chosen by the front end) *)
lightDarkColor[light_, dark_] := If[Names["System`LightDarkSwitched"] =!= {}, Symbol["LightDarkSwitched"][light, dark],
   light];
FunctionalDiagrams::appearance = "\"Appearance\" -> `1` is not Automatic, \"Light\" or \"Dark\": Automatic is used.";
diagramAppearance[opts_List] := With[{a = drawingOption[opts, "Appearance"]},
   If[MemberQ[{Automatic, "Light", "Dark"}, a], a, Message[FunctionalDiagrams::appearance, a]; Automatic]];
diagramColor[appearance_, key_String] := With[{c = $diagramPalette[key]}, Switch[appearance,
    "Light", c[[1]], "Dark", c[[2]],
    _, If[key === "Paper", lightDarkColor[c[[1]], Automatic], lightDarkColor @@ c]]];

(* the options of a call: the explicit ones first, then those of the function (which SetOptions changes); unknown
   names give a message *)
Options[DiagramPlot] = $drawingDefaults; Options[DrawDiagram] = $drawingDefaults; Options[DiagramToTikZ] = $drawingDefaults;
FunctionalDiagrams::optx = "Unknown option `1` of `2`; the options are `3`.";
drawingOptions[f_Symbol, opts_List] := Module[{given = Select[Flatten[opts], MatchQ[#, _Rule | _RuleDelayed] &]},
   Scan[If[! MemberQ[$drawingDefaults[[All, 1]], optionKey[#[[1]]]], Message[FunctionalDiagrams::optx, #[[1]], f,
       $drawingDefaults[[All, 1]]]] &, given];
   Join[given, Options[f]]];

(* sizes, in layout units *)
$pixelsPerUnit = 46.; $dotRadius = 0.075; $externalPointRadius = 0.08; $blobRadius = 0.2; $loopRadius = 0.28; $bosonLoopRadius = 0.36;
$arrowSize = 0.2; $waveAmplitude = 0.055; $waveLength = 0.17; $stubLength = 0.4; $legLength = 0.6; $fontPixels = 10.;

(* ---------------------------------------------------------------- plane geometry *)
vectorNorm[v_] := Sqrt[v . v];
unitVectorOf[v_] := With[{n = vectorNorm[v]}, If[n < 10.^-9, {1., 0.}, v/n]];
rotate90[{x_, y_}] := {-y, x};
angleOf[{x_, y_}] := If[Abs[x] + Abs[y] < 10.^-12, 0., ArcTan[x, y]];
directionAt[t_] := {Cos[t], Sin[t]};
signedAngle[p_, q_] := ArcTan[p . q, p[[1]] q[[2]] - p[[2]] q[[1]]];

(* A path is {"segment", u, v}, {"arc", centre, radius, a0, da} (from the angle a0 to a0 + da) or a balloon (below).
   Points along it are computed with listable arithmetic, which is much faster than point-by-point evaluation. *)
(* circular arc from u to v whose midpoint is displaced by s along the left normal of v - u *)
arcThrough[u_, v_, s_] := Module[{c = vectorNorm[v - u], m = (u + v)/2, normal, t, centre},
   normal = rotate90[unitVectorOf[v - u]];
   If[c < 10.^-9 || Abs[s] < 10.^-3 c, {"segment", N[u], N[v]},
    t = (s^2 - c^2/4)/(2 s); centre = m + t normal;
    {"arc", centre, Abs[s - t], angleOf[u - centre], 2 signedAngle[u - centre, m + s normal - centre]}]];
loopPath[p_, th_] := {"arc", p + $loopRadius directionAt[th], $loopRadius, th + Pi, 2. Pi};
(* A self-loop of coils or waves is a balloon, {"balloon", p, centre, radius, 2 Pi}: a larger circle and the two lines
   from p tangent to it, which leave p at an angle, so that the coils do not touch the line the loop hangs from. It is
   traversed counterclockwise, from p along the right tangent. *)
selfLoopPath[p_, th_, style_, others_List : {}] := Which[
   (* coils need a larger circle to be read as a loop *)
   style === "Curly", balloonAt[p, th, 1.4 $bosonLoopRadius, 1.9/1.4, others],
   MemberQ[{"Wavy", "DoubleWavy", "Zigzag"}, style], balloonAt[p, th, $bosonLoopRadius, 1.4, others],
   True, loopPath[p, th]];
(* the balloon of radius r in the direction th, its centre at q r from p. At a vertex with two more lines (directions
   others, e.g. a gluon loop at the four-gluon vertex) the tangents must not continue those lines, or the picture reads as
   an X: if they would (within 30 degrees), the neck is made narrower than the angle of the continued lines (or wider,
   when that angle is small) *)
balloonAt[p_, th_, r_, q_, others_List] := With[{a0 = ArcSin[1./q]},
   If[Length[others] =!= 2, {"balloon", p, p + q r directionAt[th], r, 2. Pi},
    With[{g = Mean[Table[Abs[angleDifference[o + Pi, th]], {o, others}]]},
     With[{a = If[Abs[a0 - g] >= 30. Degree, a0, If[g > 45. Degree, Max[20. Degree, g - 35. Degree], Min[75. Degree, g + 35. Degree]]]},
      {"balloon", p, p + (r/Sin[a]) directionAt[th], r, 2. Pi}]]]];
balloonParts[{"balloon", p_, c_, r_, _}] := With[{dd = vectorNorm[c - p], phi = angleOf[p - c]}, With[{beta = ArcCos[r/dd]},
    (* {length of a tangent, length of the arc, start angle of the arc, first tangent point, second tangent point} *)
    {Sqrt[dd^2 - r^2], r (2. Pi - 2 beta), phi + beta, c + r directionAt[phi + beta], c + r directionAt[phi - beta]}]];
reversedPath[{"segment", u_, v_}] := {"segment", v, u};
reversedPath[{"arc", c_, r_, a0_, da_}] := {"arc", c, r, a0 + da, -da};
pathLength[{"segment", u_, v_}] := vectorNorm[v - u];
pathLength[{"arc", c_, r_, a0_, da_}] := r Abs[da];
pathLength[b : {"balloon", __}] := With[{q = balloonParts[b]}, 2 q[[1]] + q[[2]]];

(* points at the parameters ts (in [0, 1]), displaced by ds along the left normal *)
pathPoints[{"segment", u_, v_}, ts_, ds_] := With[{n = rotate90[unitVectorOf[v - u]]},
   Transpose[{u[[1]] + (v[[1]] - u[[1]]) ts + n[[1]] ds, u[[2]] + (v[[2]] - u[[2]]) ts + n[[2]] ds}]];
pathPoints[{"arc", c_, r_, a0_, da_}, ts_, ds_] := With[{an = a0 + da ts, rr = r - Sign[da] ds},
   Transpose[{c[[1]] + rr Cos[an], c[[2]] + rr Sin[an]}]];
pathPoints[b : {"balloon", p_, c_, r_, _}, ts_, ds_] := Module[{q = balloonParts[b], len, u1, u2, dl = If[ListQ[ds], ds, Table[ds, {Length[ts]}]]},
   len = 2 q[[1]] + q[[2]]; u1 = unitVectorOf[q[[4]] - p]; u2 = unitVectorOf[p - q[[5]]];
   Table[With[{x = len ts[[i]], d = dl[[i]]}, Which[
      x <= q[[1]], p + x u1 + d rotate90[u1],
      x <= q[[1]] + q[[2]], c + (r - d) directionAt[q[[3]] + (x - q[[1]])/r],
      True, q[[5]] + (x - q[[1]] - q[[2]]) u2 + d rotate90[u2]]], {i, Length[ts]}]];
pathPolyline[path_, ds_ : 0.] := With[{k = If[path[[1]] === "segment", 1, Max[6, Ceiling[Abs[path[[5]]]/0.12]]]},
   pathPoints[path, N[Range[0, k]]/k, ds]];
pathWavy[path_, ds_ : 0.] := With[{nw = Max[1, Round[pathLength[path]/$waveLength]]}, With[{ts = N[Range[0, 8 nw]]/(8 nw)},
    pathPoints[path, ts, ds + $waveAmplitude Sin[2. Pi nw ts]]]];
pathZigzag[path_] := With[{nz = Max[2, 2 Round[pathLength[path]/$waveLength]]}, With[{ts = N[Range[0, nz]]/nz},
    pathPoints[path, ts, 0.06 Table[If[i == 0 || i == nz, 0., (-1)^i], {i, 0, nz}]]]];
(* a gluon: a coil, an ellipse of semi-axes b (along the path) and a (across) traversed nc + 1/2 times while drifting along
   the path, so that it starts and ends on the path and is mirror symmetric about its middle (the first and the last half
   turns on the left of the path); on a self-loop the coil grows from nothing at the ends, so that it does not cross the
   line the loop hangs from *)
coilPoints[path_, nc_Integer, a_, b_, perTurn_ : 16] := Module[{m = perTurn nc + Round[perTurn/2], len = pathLength[path], u, th, f, s0},
   u = N[Range[0, m]]/m; th = 2. Pi (nc + 0.5) u;
   f = If[path[[1]] === "balloon", s0 = 2. a; Table[Min[1., (len w/s0)^2, (len (1 - w)/s0)^2], {w, u}], 1.];
   pathPoints[path, u + f (b/len) (1 - 2 u - Cos[th]), f a Sin[th]]];
pathCurly[path_] := coilPoints[path, Max[2, Round[pathLength[path]/0.13]], 0.065, 0.05, 24];
pathDashes[path_, period_ : 0.15] := With[{nd = Max[1, Round[pathLength[path]/period]]},
   Partition[pathPoints[path, Flatten[Table[{i + 0.2, i + 0.5, i + 0.8}, {i, 0, nd - 1}]]/nd, 0.], 3]];
pathDots[path_] := With[{nd = Max[2, Round[pathLength[path]/0.09]]}, pathPoints[path, N[Range[0, nd]]/nd, 0.]];
pathArrowHead[path_, t_ : 0.5] := With[{q = pathPoints[path, {t - 0.03, t, t + 0.03}, 0.]}, With[{m = q[[2]], tg = unitVectorOf[q[[3]] - q[[1]]]},
    {m + 0.55 $arrowSize tg, m - 0.45 $arrowSize tg + 0.36 $arrowSize rotate90[tg], m - 0.45 $arrowSize tg - 0.36 $arrowSize rotate90[tg]}]];
(* directions of the path at its two ends, pointing into the path *)
pathEndAngles[path_] := With[{q = pathPoints[path, {0., 0.02, 0.98, 1.}, 0.]}, {angleOf[q[[2]] - q[[1]]], angleOf[q[[3]] - q[[4]]]}];

(* bisector of the largest free sector around a vertex (directions angles); ties broken towards preferred; with
   fraction < 1, every sector at least fraction times as wide as the largest one is a candidate *)
gapDirection[angles_List, preferred_, fraction_ : 1.] := First[gapDirections[angles, preferred, fraction]];
gapDirections[angles_List, preferred_, fraction_ : 1.] := Module[{s, gaps, best, good, rest},
   If[angles === {}, angleOf[preferred] + {0., Pi, Pi/2, -Pi/2},
    s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles];
    gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
    best = Max[gaps[[All, 1]]];
    good = Select[gaps, #[[1]] > fraction best - 0.05 &]; rest = Select[gaps, #[[1]] <= fraction best - 0.05 &];
    Join[SortBy[(#[[2]] + #[[1]]/2) & /@ good, -Cos[# - angleOf[preferred]] &], (#[[2]] + #[[1]]/2) & /@ SortBy[rest, -#[[1]] &]]]];
(* {direction, width} of the largest free sector, of those nearly as large the one closest to preferred *)
outwardGap[angles_List, preferred_] := Module[{s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles], gaps, best},
   gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
   best = Max[gaps[[All, 1]]];
   First[SortBy[{#[[2]] + #[[1]]/2, #[[1]]} & /@ Select[gaps, #[[1]] > best - 0.05 &], -Cos[#[[1]] - angleOf[preferred]] &]]];
(* the directions of the free sectors at least fraction times the largest one, the closest to preferred first *)
largeGapDirections[angles_List, preferred_, fraction_] := If[angles === {}, {angleOf[preferred]},
   Module[{s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles], gaps},
    gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
    SortBy[(#[[2]] + #[[1]]/2) & /@ Select[gaps, #[[1]] >= fraction Max[gaps[[All, 1]]] &], -Cos[# - angleOf[preferred]] &]]];
(* the disc {centre, radius} of a self-loop path, and its smallest distance to the discs *)
loopDisc[{"arc", c_, r_, __}] := {c, r};
loopDisc[{"balloon", _, c_, r_, _}] := {c, r};
loopDisc[_] := None;
loopClearance[path_, discs_List] := With[{d = loopDisc[path], ds = DeleteCases[discs, None]},
   If[d === None || ds === {}, Infinity, Min[Table[vectorNorm[e[[1]] - d[[1]]] - e[[2]] - d[[2]], {e, ds}]]]];
boxesOverlapQ[{c1_, wh1_}, {c2_, wh2_}] := Abs[c1[[1]] - c2[[1]]] < (wh1[[1]] + wh2[[1]])/2 && Abs[c1[[2]] - c2[[2]]] < (wh1[[2]] + wh2[[2]])/2;

(* ---------------------------------------------------------------- small graph algorithms (pairs = unordered edges) *)
neighboursOf[v_, pairs_] := DeleteDuplicates[Join[Cases[pairs, {v, u_} :> u], Cases[pairs, {u_, v} :> u]]];

shortestPath[s_, t_, pairs_] := Module[{seen = {s}, parents = {}, queue = {s}, v, path, found = (s === t)},
   While[queue =!= {} && ! found,
    v = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[seen, u], AppendTo[seen, u]; AppendTo[parents, {u, v}]; AppendTo[queue, u]; If[u === t, found = True]],
     {u, neighboursOf[v, pairs]}]];
   If[! found, None,
    path = {t};
    While[First[path] =!= s, PrependTo[path, First[Select[parents, #[[1]] === First[path] &]][[2]]]];
    path]];

componentOf[s_, pairs_] := Module[{seen = {s}, queue = {s}, v},
   While[queue =!= {}, v = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[seen, u], AppendTo[seen, u]; AppendTo[queue, u]], {u, neighboursOf[v, pairs]}]];
   seen];

(* longest simple cycle (length >= 3) by depth-first search; graphs are small *)
longestCycle[nodes_, pairs_] := Module[{best = {}, count = 0, search, index},
   index[x_] := First[First[Position[nodes, x, {1}]]];
   search[path_, s_] := (count++;
     If[count < 3000 && Length[best] < Length[nodes],
      Do[Which[
        u === s, If[Length[path] >= 3 && Length[path] > Length[best], best = path],
        ! MemberQ[path, u] && index[u] > index[s], search[Append[path, u], s]], {u, neighboursOf[Last[path], pairs]}]]);
   Do[search[{s}, s], {s, nodes}];
   best];

(* remove free vertices of degree <= 1 repeatedly: {core, stripped (outermost first)} *)
stripPendantVertices[fixed_, free_, pairs_] := Module[{core = free, stripped = {}, again = True, alive},
   While[again, again = False;
    Do[If[MemberQ[core, v],
      alive = Select[neighboursOf[v, pairs], MemberQ[core, #] || MemberQ[fixed, #] &];
      If[Length[alive] <= 1, core = DeleteCases[core, v]; PrependTo[stripped, v]; again = True]], {v, core}]];
   {core, Reverse[stripped]}];

(* barycentric (Tutte) placement of the free vertices, the fixed ones being given *)
(* (the free vertices in outward are pulled 1.5 times as hard towards their fixed neighbours) *)
tutteEmbedding[fixedPositions_, free_, pairs_, outward_List : {}] := Module[{m = Length[free], M, bx, by, j, fx, wt},
   If[m == 0, {},
    M = 10.^-6 IdentityMatrix[m]; bx = Table[0., {m}]; by = Table[0., {m}];
    Do[wt = If[MemberQ[outward, free[[i]]], 1.5, 1.];
     Do[
       j = Position[free, u, {1}];
       fx = Select[fixedPositions, #[[1]] === u &];
       Which[
        j =!= {}, M[[i, i]] += 1.; M[[i, j[[1, 1]]]] -= 1.,
        fx =!= {}, M[[i, i]] += wt; bx[[i]] += wt fx[[1, 2, 1]]; by[[i]] += wt fx[[1, 2, 2]]],
       {u, neighboursOf[free[[i]], pairs]}], {i, m}];
    With[{xs = N[LinearSolve[M, bx]], ys = N[LinearSolve[M, by]]}, Table[{free[[i]], {xs[[i]], ys[[i]]}}, {i, m}]]]];

(* place the stripped (pendant) vertices outwards, in the largest free sector of their parent *)
placePendantVertices[positions0_, stripped_, pairs_, centre_, outward_ : {}] := Module[{positions = positions0, todo = stripped, v,
    parent, kids, pp, th0, k, angles},
   While[todo =!= {},
    v = Select[todo, Intersection[neighboursOf[#, pairs], positions[[All, 1]]] =!= {} &];
    If[v === {}, positions = Join[positions, Table[{todo[[i]], {1.5 i, -1.5}}, {i, Length[todo]}]]; todo = {},
     parent = First[Select[positions[[All, 1]], MemberQ[neighboursOf[First[v], pairs], #] &]];
     pp = Replace[parent, Rule @@@ positions];
     kids = Select[todo, MemberQ[neighboursOf[#, pairs], parent] &];
     angles = Cases[positions, {u_, q_} /; MemberQ[neighboursOf[parent, pairs], u] :> angleOf[q - pp]];
     th0 = gapDirection[angles, Which[vectorNorm[pp - centre] < 0.1, {0., 1.}, MemberQ[outward, parent], centre - pp, True, pp - centre]];
     k = Length[kids];
     (* one pendant part (a tadpole, a bubble with a tadpole, a chain of tadpoles): turned, within the free sector, in the
        direction where it stays farthest from the lines and points already placed (legs, other lines) *)
     If[k == 1, Module[{segs, pts, depth, score, best = th0, bestScore = -Infinity},
        segs = Cases[pairs, {a_, b_} /; MemberQ[positions[[All, 1]], a] && MemberQ[positions[[All, 1]], b] :>
           {Replace[a, Rule @@@ positions], Replace[b, Rule @@@ positions]}];
        pts = Cases[positions, {u_, q_} /; u =!= parent :> q];
        (* the vertices of the part, and one more for what may hang at its end (a self-loop) *)
        depth = Min[3, 1 + Length[componentOf[First[kids], Select[pairs, MemberQ[todo, #[[1]]] && MemberQ[todo, #[[2]]] &]]]];
        Do[With[{th = th0 + dth},
          If[AllTrue[angles, Abs[angleDifference[th, #]] > 0.6 &],
           score = Min[Join[{10.}, Flatten[Table[With[{q = pp + 0.9 j directionAt[th]},
                 Join[distancesFromPoint[q, segs], Table[vectorNorm[q - x], {x, pts}]]], {j, depth}]]]] - 0.05 Abs[dth];
           If[score > bestScore + 10.^-6, bestScore = score; best = th]]], {dth, {0., 0.26, -0.26, 0.52, -0.52, 0.79, -0.79, 1.05, -1.05}}];
        th0 = best]];
     positions = Join[positions, Table[{kids[[j]], pp + 0.9 directionAt[th0 + 0.9 (j - (k + 1)/2)]}, {j, k}]];
     todo = Select[todo, ! MemberQ[kids, #] &]]];
   positions];

(* push apart free vertices that are too close *)
pushApartVertices[positions0_, free_, minimumDistance_] := Module[{p = positions0, moved, v, d},
   Do[moved = False;
    Do[If[MemberQ[free, p[[i, 1]]],
      Do[If[j != i, v = p[[i, 2]] - p[[j, 2]]; d = vectorNorm[v];
        With[{dm = If[Head[p[[i, 1]]] === blob || Head[p[[j, 1]]] === blob, minimumDistance + 0.35, minimumDistance]},
         If[d < dm, moved = True;
          p[[i, 2]] = p[[i, 2]] + If[d < 10.^-6, 0.5 dm directionAt[2.4 i], 0.6 (dm - d) v/d]]]], {j, Length[p]}]], {i, Length[p]}];
    If[! moved, Break[]], {12}];
   p];

(* the vertices of a path on the horizontal axis, the others in layers above it *)
layeredLayout[base_, rest_, pairs_] := Module[{positions = base, remaining = rest, layer, depth = 0, xs, order},
   While[remaining =!= {},
    layer = Select[remaining, Intersection[neighboursOf[#, pairs], positions[[All, 1]]] =!= {} &];
    If[layer === {}, layer = {First[remaining]}];
    depth++;
    xs = Table[With[{nx = Cases[positions, {u_, q_} /; MemberQ[neighboursOf[v, pairs], u] :> q[[1]]]}, If[nx === {}, 0., Mean[nx]]], {v, layer}];
    order = Ordering[xs]; layer = layer[[order]]; xs = xs[[order]];
    xs = With[{sp = FoldList[Max[#2, #1 + 0.95] &, First[xs], Rest[xs]]}, sp - Mean[sp] + Mean[xs]];
    positions = Join[positions, Table[{layer[[i]], {xs[[i]], 0.9 depth}}, {i, Length[layer]}]];
    remaining = Select[remaining, ! MemberQ[layer, #] &]];
   positions];

(* ---------------------------------------------------------------- crossings and overlaps *)
(* with listable arithmetic: the number of segments of the list T that the segment {a, b} crosses (segments with a
   common end, or touching, do not cross) *)
crossingsWith[{a_, b_}, T_] := If[T === {}, 0, With[{x1 = T[[All, 1, 1]], y1 = T[[All, 1, 2]], x2 = T[[All, 2, 1]], y2 = T[[All, 2, 2]]},
    With[{o1 = (b[[1]] - a[[1]]) (y1 - a[[2]]) - (b[[2]] - a[[2]]) (x1 - a[[1]]), o2 = (b[[1]] - a[[1]]) (y2 - a[[2]]) - (b[[2]] - a[[2]]) (x2 - a[[1]]),
       o3 = (x2 - x1) (a[[2]] - y1) - (y2 - y1) (a[[1]] - x1), o4 = (x2 - x1) (b[[2]] - y1) - (y2 - y1) (b[[1]] - x1)},
     Total[UnitStep[-o1 o2 - 10.^-9] UnitStep[-o3 o4 - 10.^-9]]]]];
crossingCount[segments_, others_] := Total[Table[crossingsWith[sg, others], {sg, segments}]];
(* the bisector th of the largest free sector around the point p (directions angles, ties towards preferred); if the point
   at distance reach in that direction is enclosed by the segments T (inside a loop or a bubble), the bisector of the
   largest sector that leads outside *)
outsideGapDirection[p_, angles_List, preferred_, T_, reach_] := Module[{th = gapDirection[angles, preferred], ok, width},
   If[angles =!= {} && enclosedPointQ[p + reach directionAt[th], T],
    ok = Select[gapDirections[angles, preferred, 0.], ! enclosedPointQ[p + reach directionAt[#], T] &];
    width[t_] := 2 Min[Table[Abs[angleDifference[t, a]], {a, angles}]];
    If[ok =!= {}, With[{wmax = Max[width /@ ok]}, th = First[Select[ok, width[#] > wmax - 0.05 &]]]]];
   th];
(* the point q enclosed by the segments T (inside a loop or a bubble): every ray from it crosses one of them *)
enclosedPointQ[q_, T_] := T =!= {} && AllTrue[Range[0, 15], crossingsWith[{q, q + 50. directionAt[2. Pi (# + 0.37)/16]}, T] > 0 &];
mutualCrossingCount[segments_] := Total[Table[crossingsWith[segments[[i]], Drop[segments, i]], {i, Length[segments]}]];
(* distance of the point p from the segment {a, b} *)
segmentDistance[p_, {a_, b_}] := Module[{ab = b - a, t},
   t = If[ab . ab < 10.^-12, 0., Min[1., Max[0., (p - a) . ab/(ab . ab)]]];
   vectorNorm[p - a - t ab]];
(* listable: x clipped to [0, 1]; the distances of the point p from the segments T; of the points P from the segment {a, b} *)
clip01[x_] := With[{y = (x + Abs[x])/2}, 1 - (1 - y + Abs[1 - y])/2];
distancesFromPoint[p_, T_] := If[T === {}, {}, With[{x1 = T[[All, 1, 1]], y1 = T[[All, 1, 2]], x2 = T[[All, 2, 1]], y2 = T[[All, 2, 2]]},
    With[{dx = x2 - x1, dy = y2 - y1}, With[{t = clip01[((p[[1]] - x1) dx + (p[[2]] - y1) dy)/(dx^2 + dy^2 + 10.^-12)]},
      Sqrt[(p[[1]] - x1 - t dx)^2 + (p[[2]] - y1 - t dy)^2]]]]];
distancesToSegment[P_, {a_, b_}] := If[P === {}, {}, With[{px = P[[All, 1]], py = P[[All, 2]], dx = b[[1]] - a[[1]], dy = b[[2]] - a[[2]]},
    With[{t = clip01[((px - a[[1]]) dx + (py - a[[2]]) dy)/(dx^2 + dy^2 + 10.^-12)]},
     Sqrt[(px - a[[1]] - t dx)^2 + (py - a[[2]] - t dy)^2]]]];
countBelow[list_, x_] := If[list === {}, 0, Total[UnitStep[x - list - 10.^-12]]];

(* The brute-force choice of sides (0 or 1) of n items: the assignment with the lowest cost[sides], of the 2^n tried in
   increasing order of their binary codes; of equal costs (within 10^-9) the first. The callers bound n. *)
cheapestSides[n_Integer, cost_] := Module[{best = None, bestCost = Infinity, sides, c},
   Do[sides = IntegerDigits[code, 2, n]; c = cost[sides];
    If[c < bestCost - 10.^-9, bestCost = c; best = sides], {code, 0, 2^n - 1}];
   best];
(* two index spans {a, b} (a < b) that cross: they interleave; a span over one of the indices of a *)
spansCrossQ[s_, t_] := (s[[1]] < t[[1]] < s[[2]] < t[[2]]) || (t[[1]] < s[[1]] < t[[2]] < s[[2]]);
spanOverQ[s_, a_] := AnyTrue[a, s[[1]] < # < s[[2]] &];
(* the conflicts of arcs (spans, the first ns sides) and parts attached to the axis (index lists attached, their ranges,
   the other sides) on the same side: two crossing arcs, an arc over a vertex of a part, two interleaved parts *)
axisConflictCount[sides_, spans_, attached_, ranges_] := With[{ns = Length[spans], nc = Length[attached]},
   Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
    Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && spanOverQ[spans[[i]], attached[[j]]], {i, ns}, {j, nc}]], True] +
    Count[Flatten[Table[sides[[ns + i]] == sides[[ns + j]] && (spansCrossQ[ranges[[i]], ranges[[j]]] || spanOverQ[ranges[[i]], attached[[j]]] ||
         spanOverQ[ranges[[j]], attached[[i]]]), {i, nc}, {j, i + 1, nc}]], True]];

(* Sides of the arcs (lines joining non-adjacent vertices of the axis of a two-point function) and of the parts hanging
   from the axis. On the same side, two crossing arcs conflict, and so do an arc and a part attached to a vertex under
   it, or two parts attached at interleaved vertices. The assignment with the fewest conflicts is chosen; then arcs and
   parts above, long arcs above. Result: {node pairs of the arcs below, nodes of the parts below}. *)
axisSides[backbone_, positions_, pairs_] := Module[{ix, spans, rest, components = {}, attached, ranges, ns, nc, best},
   ix[v_] := First[First[Position[backbone, v, {1}]]];
   spans = SortBy[DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[backbone, u] && MemberQ[backbone, v] && Abs[ix[u] - ix[v]] > 1 :>
        Sort[{ix[u], ix[v]}]]], {-(#[[2]] - #[[1]]), #[[1]]} &];
   rest = Select[positions[[All, 1]], ! MemberQ[backbone, #] &];
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, Select[pairs, MemberQ[rest, #[[1]]] && MemberQ[rest, #[[2]]] &]]]],
    {v, rest}];
   attached = Table[Union[Flatten[Table[ix /@ Select[neighboursOf[v, pairs], MemberQ[backbone, #] &], {v, c}]]], {c, components}];
   ranges = Table[If[a === {}, {0, 0}, {Min[a], Max[a]}], {a, attached}];
   ns = Length[spans]; nc = Length[components];
   Which[ns + nc == 0, {{}, {}},
    ns + nc > 10,
    (* too many to try them all: the arcs greedily, the parts above *)
    {arcsBelowAxis[backbone, pairs], {}},
    True,
    best = cheapestSides[ns + nc, Function[sides, 10 axisConflictCount[sides, spans, attached, ranges] +
        Total[Take[sides, ns]] + 0.5 Total[Drop[sides, ns]] + 0.01 Total[Table[sides[[i]] (spans[[i, 2]] - spans[[i, 1]]), {i, ns}]]]];
    {Table[Sort[{backbone[[spans[[i, 1]]]], backbone[[spans[[i, 2]]]]}], {i, Select[Range[ns], best[[#]] == 1 &]}],
     Flatten[Table[components[[j]], {j, Select[Range[nc], best[[ns + #]] == 1 &]}]]}]];

(* lines joining non-adjacent vertices of the axis of a two-point function (G, Sigma) are arcs; when two of them cross,
   one of them is drawn below the axis (greedily, longer arcs first above). The node pairs drawn below. *)
arcsBelowAxis[backbone_, pairs_] := Module[{ix, spans, above = {}, below = {}},
   ix[v_] := First[First[Position[backbone, v, {1}]]];
   spans = DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[backbone, u] && MemberQ[backbone, v] && Abs[ix[u] - ix[v]] > 1 :>
       Sort[{ix[u], ix[v]}]]];
   spans = SortBy[spans, {-(#[[2]] - #[[1]]), #[[1]]} &];
   Do[Which[
     ! AnyTrue[above, spansCrossQ[sp, #] &], AppendTo[above, sp],
     ! AnyTrue[below, spansCrossQ[sp, #] &], AppendTo[below, sp],
     True, AppendTo[above, sp]], {sp, spans}];
   Table[Sort[{backbone[[sp[[1]]]], backbone[[sp[[2]]]]}], {sp, below}]];

(* ---------------------------------------------------------------- round fermion loops *)
(* Closed fermion loops of three or more vertices are drawn as circles. A loop through the whole axis of a two-point
   function (a polarisation bubble) has the external points at the ends of a horizontal diameter, the vertices on the
   axis below and the others above; any other loop hangs from its vertex nearest to the external points, and what
   hangs from its other vertices moves with them. $loopCircles holds {loop, centre, radius} while a frame is drawn. *)
$loopCircles = {};
(* the radius of each blob node (blob[k] -> r), set by diagramGeometry *)
$blobRadii = {};

(* the end kind of a fermion line {u, v, h, style} at its node x: "psi" (an arrow into x, the field psi(x)) or "bar" (an
   arrow out of x, psibar(x)); an anomalous line -<T psi psi> has two psi ends, -<T psibar psibar> two bar ends *)
lineEndKind[l_, x_] := Which[directedStyleQ[l[[4]]], If[l[[2]] === x, "psi", "bar"], l[[4]] === "DivergingArrows", "psi", True, "bar"];
anomalousLinesQ[lines_List] := AnyTrue[lines, anomalousStyleQ[#[[4]]] &];

(* the loops, in fermion order: cycles of directed lines through vertices with one directed line in and one out; with
   anomalous lines, cycles through vertices with one psi end and one bar end (the arrows turn inside the anomalous lines) *)
fermionLoops[nodes_, lines_, minimumLength_ : 3] /; anomalousLinesQ[lines] := Module[
   {edges = Select[lines, fermionStyleQ[#[[4]]] && #[[1]] =!= #[[2]] && MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &],
    endsAt, goodQ, loops = {}, seen = {}, p, i, v, ok, next},
   endsAt[x_] := Select[Range[Length[edges]], MemberQ[edges[[#, {1, 2}]], x] &];
   goodQ[x_] := Length[endsAt[x]] == 2 && Sort[Table[lineEndKind[edges[[j]], x], {j, endsAt[x]}]] === {"bar", "psi"};
   Do[If[! MemberQ[seen, start] && goodQ[start],
     p = {start}; i = First[Select[endsAt[start], lineEndKind[edges[[#]], start] === "bar" &]];
     v = If[edges[[i, 1]] === start, edges[[i, 2]], edges[[i, 1]]]; ok = True;
     While[ok && v =!= start,
      If[! goodQ[v] || MemberQ[p, v], ok = False,
       AppendTo[p, v]; next = DeleteCases[endsAt[v], i]; i = First[next];
       v = If[edges[[i, 1]] === v, edges[[i, 2]], edges[[i, 1]]]]];
     If[ok && Length[p] >= minimumLength, AppendTo[loops, p]];
     seen = Union[seen, p]], {start, nodes}];
   loops];
fermionLoops[nodes_, lines_, minimumLength_ : 3] := Module[{succ, loops = {}, seen = {}, path, v, outs, ok},
   succ = Cases[lines, {u_, w_, _, st_} /; u =!= w && directedStyleQ[st] && MemberQ[nodes, u] && MemberQ[nodes, w] :> {u, w}];
   Do[If[! MemberQ[seen, start],
     path = {start}; v = start; ok = True;
     While[ok,
      outs = Cases[succ, {v, w_} :> w];
      If[Length[outs] != 1 || Count[succ, {_, outs[[1]]}] != 1, ok = False,
       v = outs[[1]];
       Which[v === start, ok = False,
        MemberQ[path, v], ok = False; path = {},
        True, AppendTo[path, v]]]];
     If[v === start && Length[path] >= minimumLength, AppendTo[loops, path]];
     seen = Union[seen, path, {start}]], {start, nodes}];
   loops];

(* closed loops of three vertices joined by undirected propagators (phi^4 bubbles with an insertion on one of their lines,
   tadpoles carrying a vertex): drawn round like fermion loops. Only those not sharing a vertex with the loops already
   taken; with several lines between two of their vertices only if they hang from one vertex of the axis or of the
   external points; with two such vertices only outside two-point functions *)
scalarTriangles[nodes_, lines_, taken_, anchors_, mode_] := Module[{ls, pairs, mult, tris = {}, used = Flatten[taken], na, ms},
   ls = Cases[lines, {u_, w_, _, st_} /; u =!= w && ! fermionStyleQ[st] && ! interactionStyleQ[st] && st =!= "Spoke" &&
       MemberQ[nodes, u] && MemberQ[nodes, w] :> Sort[{u, w}]];
   pairs = DeleteDuplicates[ls]; mult[pr_] := Count[ls, pr];
   Do[If[w =!= pr[[1]] && w =!= pr[[2]] && MemberQ[pairs, Sort[{pr[[1]], w}]] && MemberQ[pairs, Sort[{pr[[2]], w}]],
      AppendTo[tris, Sort[{pr[[1]], pr[[2]], w}]]], {pr, pairs}, {w, nodes}];
   tris = SortBy[DeleteDuplicates[tris], {Count[#, _?(MemberQ[anchors, #] &)] != 1, Max[mult /@ Subsets[#, {2}]]} &];
   Join @@ Table[na = Count[t, _?(MemberQ[anchors, #] &)]; ms = Max[mult[Sort[#]] & /@ Subsets[t, {2}]];
     If[Intersection[t, used] === {} && (na == 1 || (ms == 1 && (na == 0 || (na == 2 && (mode[[1]] =!= "backbone" ||
           Complement[mode[[2]], t] === {}))))),
      used = Join[used, t]; {t}, {}], {t, tris}]];

consecutiveOnLoopQ[loop_, u_, v_] := With[{i = Position[loop, u, {1}], j = Position[loop, v, {1}], n = Length[loop]},
   i =!= {} && j =!= {} && (Mod[j[[1, 1]] - i[[1, 1]], n] == 1 || Mod[i[[1, 1]] - j[[1, 1]], n] == 1)];

(* the arc of the circle {loop, centre, r} from pu to pv that does not contain the other vertices of the loop *)
circleKind[pu_, pv_, circle_, others_] := Module[{c = circle[[2]], r = circle[[3]], m = (pu + pv)/2, nrm = rotate90[unitVectorOf[pv - pu]],
    d = vectorNorm[pv - pu], h, sideOthers, bulge},
   h = Sqrt[Max[r^2 - d^2/4, 0.]];
   sideOthers = If[others === {}, -1., Sign[nrm . (Mean[others] - m)]];
   bulge = If[sideOthers == 0, 1., -sideOthers];
   {"circle", bulge If[bulge (nrm . (c - m)) > 10.^-9, r + h, r - h]}];

(* the two lines of a bubble drawn as its circle {loop, centre, r} through the two vertices: the short arc on the side away
   from the centre, the long one around it *)
bubbleCircleKind[pu_, pv_, circle_] := Module[{c = circle[[2]], r = circle[[3]], m = (pu + pv)/2, nrm = rotate90[unitVectorOf[pv - pu]],
    d = vectorNorm[pv - pu], h, sc},
   h = Sqrt[Max[r^2 - d^2/4, 0.]]; sc = If[nrm . (c - m) >= 0, 1., -1.];
   {"bubbleCircle", -sc (r - h), sc (r + h)}];

(* distances in the graph from a set of nodes: {{node, distance}, ...} *)
graphDistances[roots_, pairs_] := Module[{dist = Table[{x, 0}, {x, roots}], frontier = roots, next, d = 0},
   While[frontier =!= {},
    d++;
    next = Select[DeleteDuplicates[Flatten[Table[neighboursOf[v, pairs], {v, frontier}]]], ! MemberQ[Cases[dist, {w_, _} :> w], #] &];
    dist = Join[dist, Table[{v, d}, {v, next}]];
    frontier = next];
   dist];
distanceOf[dist_, v_] := With[{m = Select[dist, #[[1]] === v &, 1]}, If[m === {}, 10^6, m[[1, 2]]]];

(* move some nodes to new points, and with them the parts of the diagram that hang only from them *)
moveWithDependents[positions0_, newPoints_, loop_, stay_, pairs_] := Module[{positions = positions0, shift, others, rest, done = {}, comp,
    attached},
   shift = Table[{np[[1]], np[[2]] - Replace[np[[1]], Rule @@@ positions0]}, {np, newPoints}];
   others = Select[pairs, ! MemberQ[loop, #[[1]]] && ! MemberQ[loop, #[[2]]] &];
   rest = Select[positions0[[All, 1]], ! MemberQ[loop, #] && Head[#] =!= leg &];
   Do[If[! MemberQ[done, w],
     comp = componentOf[w, others]; done = Join[done, comp];
     attached = Select[loop, Intersection[neighboursOf[#, pairs], comp] =!= {} &];
     If[Intersection[comp, stay] === {} && attached =!= {} && Complement[attached, Cases[shift, {a_, _} :> a]] === {},
      With[{v = Mean[Table[Replace[a, Rule @@@ shift], {a, attached}]]},
       positions = Table[If[MemberQ[comp, p[[1]]], {p[[1]], p[[2]] + v}, p], {p, positions}]]]],
    {w, rest}];
   Table[With[{m = Select[newPoints, #[[1]] === p[[1]] &, 1]}, If[m === {}, p, {p[[1]], m[[1, 2]]}]], {p, positions}]];

(* the links {v, w} of a loop to the vertices w nearer to the external points (v on the loop) *)
attachmentLinks[loop_, pairs_, dist_] := Flatten[Table[{v, w}, {v, loop},
    {w, Select[neighboursOf[v, pairs], ! MemberQ[loop, #] && distanceOf[dist, #] < distanceOf[dist, v] &]}], 1];

(* the vertices that stay when a loop is moved (those not hanging from it alone), and the lines between them *)
staticPartsFor[loop_, positions_, pairs_, stay_] := Module[{others = Select[pairs, ! MemberQ[loop, #[[1]]] && ! MemberQ[loop, #[[2]]] &],
    rest = Select[positions[[All, 1]], ! MemberQ[loop, #] && Head[#] =!= leg &], done = {}, comp, moving = {}, nodes, point},
   point[v_] := Replace[v, Rule @@@ positions];
   Do[If[! MemberQ[done, w], comp = componentOf[w, others]; done = Join[done, comp];
     If[Intersection[comp, stay] === {}, moving = Join[moving, comp]]], {w, rest}];
   nodes = Select[rest, ! MemberQ[moving, #] &];
   {nodes, Table[{point[pr[[1]]], point[pr[[2]]]}, {pr, Select[others, MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &]}]}];

(* the best circle of radius r for a loop attached by the links {v, w} (v on the loop, w a placed vertex): centres
   around the attached vertices, every rotation and both orientations; short links that cross nothing, no vertex or
   line inside the circle, near the preferred centre. Result: {centre, {{v, point}, ...}} *)
placeLoopCircle[loop_, r_, links_, point_, staticNodes_, staticPoints_, preferred_] := Module[{n = Length[loop], m, candidates,
    bestScore = Infinity, good = {}, pts, segs, base, score, ends, at, nodePoints, near, normal, choice},
   ends = Table[point[l[[2]]], {l, links}];
   at = Table[First[First[Position[loop, l[[1]], {1}]]], {l, links}];
   nodePoints = Table[point[w], {w, staticNodes}];
   m = Mean[ends];
   normal = rotate90[unitVectorOf[preferred - m]];
   (* {centre, radius}: away from the attached vertices (at the preferred side or around), or among them with a
      radius that leaves room for the links *)
   near[c_] := {c, Min[r, Max[0.35, Min[Table[vectorNorm[q - c], {q, ends}]] - 0.5]]};
   candidates = Join[{{preferred, r}}, Table[{m + (r + 0.8) directionAt[Pi k/6.], r}, {k, 0, 11}],
     {near[m]}, Table[near[m + 0.3 directionAt[Pi k/3.]], {k, 0, 5}]];
   (* the penalties (crossings) are computed only for the candidates that can still be among the best *)
   Do[With[{c = cand[[1]], rc = cand[[2]]},
     base = 0.3 vectorNorm[c - preferred] + 2. (r - rc) + 8 (Count[nodePoints, q_ /; vectorNorm[q - c] < rc + 0.15] +
          Count[staticPoints, sg_ /; segmentDistance[c, sg] < rc - 0.05]) + 6 Count[staticPoints, sg_ /; rc - 0.05 <= segmentDistance[c, sg] < rc + 0.2];
     If[base < bestScore + 0.3,
      Do[pts = Table[c + rc directionAt[th + s 2. Pi (k - 1)/n], {k, n}];
       score = base + Total[Table[(pts[[at[[i]]]] - ends[[i]]) . (pts[[at[[i]]]] - ends[[i]]), {i, Length[links]}]];
       If[score < bestScore + 0.3,
        segs = Table[{pts[[at[[i]]]], ends[[i]]}, {i, Length[links]}];
        (* crossings, and links that start into the circle instead of away from it *)
        score = score + 8 (mutualCrossingCount[segs] + crossingCount[segs, staticPoints] +
            Count[Table[(ends[[i]] - pts[[at[[i]]]]) . (pts[[at[[i]]]] - c) < 0, {i, Length[links]}], True]);
        If[score < bestScore + 0.3, bestScore = Min[bestScore, score];
         AppendTo[good, {score, Abs[(c - m) . normal], c, Table[{loop[[k]], pts[[k]]}, {k, n}], rc}]]],
       {s, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}]]],
    {cand, candidates}];
   (* among the candidates nearly as good as the best, the one nearest to the axis of the attached vertices in the
      preferred direction: symmetric diagrams stay symmetric *)
   choice = First[SortBy[Select[good, #[[1]] <= bestScore + 0.3 &], {Round[#[[2]], 0.01], #[[1]]} &]];
   {choice[[3]], choice[[4]], choice[[5]]}];

roundFermionLoops[positions0_, lines_, mode_, fixed_, pairs_] := Module[{positions = positions0, loops, circles = {}, point, roots, dist,
    backbone, a, b, pa, pb, c, r, rotated, j, pathAB, pathBA, lowerPath, upperPath, newPoints, anchor, parents, u, n, area, orient, th0, p,
    flip = False},
   point[v_] := Replace[v, Rule @@@ positions];
   (* bubbles (loops of two vertices) hanging from the axis of a two-point function are moved away from it, with what
      hangs from them, so that their two arcs keep clear of the axis *)
   If[mode[[1]] === "backbone" && ! mode[[3]],
    Do[If[Intersection[bubble, mode[[2]]] === {},
      With[{pu = point[bubble[[1]]], pv = point[bubble[[2]]]},
       With[{h = (pu[[2]] + pv[[2]])/2, need = 0.45 vectorNorm[pv - pu] + 0.4},
        If[0.01 < Abs[h] < need && Abs[pu[[2]] - pv[[2]]] < 0.3,
         With[{part = componentOf[bubble[[1]], Select[pairs, ! MemberQ[mode[[2]], #[[1]]] && ! MemberQ[mode[[2]], #[[2]]] &]],
           dy = Sign[h] (need - Abs[h])},
          positions = Table[If[MemberQ[part, p[[1]]], {p[[1]], p[[2]] + {0., dy}}, p], {p, positions}]]]]]],
     {bubble, Select[fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, 2], Length[#] == 2 &]}]];
   loops = fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines];
   roots = Which[MemberQ[{"backbone", "cycle"}, mode[[1]]], mode[[2]], fixed =!= {}, fixed, True, Take[positions0[[All, 1]], 1]];
   If[mode[[1]] =!= "loops", loops = Join[loops, scalarTriangles[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, loops,
       Join[roots, fixed], mode]]];
   If[mode[[1]] === "cycle", loops = Select[loops, Sort[#] =!= Sort[mode[[2]]] &]];
   dist = graphDistances[roots, pairs];
   (* in vertex functions, a bubble joined to the rest at both its vertices is placed like a round loop (it is drawn
      with two arcs between its vertices) *)
   If[mode[[1]] === "circle",
    Do[With[{links = attachmentLinks[bubble, pairs, dist]},
       If[Length[DeleteDuplicates[Table[l[[1]], {l, links}]]] == 2,
        With[{static = staticPartsFor[bubble, positions, pairs, Join[roots, fixed]], m = Mean[Table[point[l[[2]]], {l, links}]],
          now = Mean[Table[point[v], {v, bubble}]]},
         With[{best = placeLoopCircle[bubble, 0.5, links, point, static[[1]], static[[2]],
             m + 1.3 If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]]},
          positions = moveWithDependents[positions, best[[2]], bubble, Join[roots, fixed], pairs]]]]],
     {bubble, Select[fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, 2], Length[#] == 2 &]}]];
   (* loops nearer to the external points first: a loop hanging from another one follows it *)
   loops = SortBy[loops, Min[Table[distanceOf[dist, v], {v, #}]] &];
   Do[
    newPoints = None;
    Which[
     mode[[1]] === "backbone" && ! mode[[3]] && Length[mode[[2]]] >= 2 && backboneSegmentOfLoop[mode[[2]], loop] =!= None,
     (* a loop through the whole axis, or through a stretch of it (a loop closed on an external point): the circle on the
        stretch as a diameter, its vertices off the axis on the arcs *)
     backbone = mode[[2]]; {a, b} = backboneSegmentOfLoop[backbone, loop]; pa = point[a]; pb = point[b];
     c = (pa + pb)/2; r = vectorNorm[pb - pa]/2;
     rotated = RotateLeft[loop, First[First[Position[loop, a, {1}]]] - 1];
     j = First[First[Position[rotated, b, {1}]]];
     pathAB = Table[rotated[[i]], {i, 2, j - 1}];
     pathBA = Reverse[Table[rotated[[i]], {i, j + 1, Length[rotated]}]];
     If[Complement[pathAB, backbone] === {}, lowerPath = pathAB; upperPath = pathBA, lowerPath = pathBA; upperPath = pathAB];
     (* the external points joined by a propagator, everything else on the other line of the loop: that line is drawn
        above, and the picture is reflected if it runs from a to b, so that the loop turns counterclockwise and an
        insertion on one or the other line of a bubble is drawn above or below it *)
     If[Length[backbone] == 2 && pathAB =!= {} && pathBA === {}, flip = True];
     newPoints = Join[
       Table[{lowerPath[[k]], c + r directionAt[Pi + Pi k/(Length[lowerPath] + 1)]}, {k, Length[lowerPath]}],
       Table[{upperPath[[k]], c + r directionAt[Pi - Pi k/(Length[upperPath] + 1)]}, {k, Length[upperPath]}]];
     AppendTo[circles, {loop, c, r}],
     Intersection[loop, roots] === {} && Intersection[loop, fixed] === {} &&
      Length[DeleteDuplicates[Table[l[[1]], {l, attachmentLinks[loop, pairs, dist]}]]] >= 2,
     (* attached at several vertices: the circle where the links are short and cross nothing; a loop of three vertices
        attached at two of them is placed like a bubble between these two, the third vertex on one of its arcs *)
     n = Length[loop]; r = 0.55/Sin[Pi/n];
     With[{links = attachmentLinks[loop, pairs, dist], static = staticPartsFor[loop, positions, pairs, Join[roots, fixed]]},
      With[{m = Mean[Table[point[l[[2]]], {l, links}]], now = Mean[Table[point[v], {v, loop}]],
        attached = DeleteDuplicates[Table[l[[1]], {l, links}]]},
       If[n == 3 && Length[attached] == 2,
        With[{best = placeLoopCircle[attached, 0.5, links, point, static[[1]], static[[2]],
            m + 1.3 If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]], w = First[Complement[loop, attached]]},
         With[{pp = best[[2, 1, 2]], pq = best[[2, 2, 2]]}, With[{nrm = rotate90[unitVectorOf[pq - pp]], mid = (pp + pq)/2},
           c = mid; r = vectorNorm[pq - pp]/2;
           newPoints = Append[best[[2]], {w, mid + r If[nrm . (point[w] - mid) < 0, -1., 1.] nrm}]]]],
        With[{best = placeLoopCircle[loop, r, links, point, static[[1]], static[[2]],
            m + (r + 0.8) If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]]},
         c = best[[1]]; newPoints = best[[2]]; r = best[[3]]]]]];
     AppendTo[circles, {loop, c, r}],
     (* a loop through two external points (not on the axis of a two-point function): the circle through them, the
        other vertices on its arc on their side *)
     mode[[1]] =!= "backbone" && Length[Intersection[loop, Join[roots, fixed]]] == 2 && Length[loop] == 3,
     With[{ends = Intersection[loop, Join[roots, fixed]], w = First[Complement[loop, Join[roots, fixed]]]},
      With[{pp = point[ends[[1]]], pq = point[ends[[2]]]}, With[{nrm = rotate90[unitVectorOf[pq - pp]], m = (pp + pq)/2},
        With[{side = If[Abs[nrm . (point[w] - m)] > 0.05, Sign[nrm . (point[w] - m)], If[nrm . m < 0, -1., 1.]]},
         c = m; r = vectorNorm[pq - pp]/2; newPoints = {{w, m + side r nrm}}]]]];
     AppendTo[circles, {loop, c, r}],
     Length[Intersection[loop, Join[roots, fixed]]] <= 1,
     anchor = First[SortBy[loop, {distanceOf[dist, #], First[First[Position[loop, #, {1}]]]} &]];
     parents = Select[neighboursOf[anchor, pairs], ! MemberQ[loop, #] && distanceOf[dist, #] < distanceOf[dist, anchor] &];
     (* a loop hanging from a vertex of the axis or from an external point: standing on it, on the side of its vertices *)
     u = Which[parents =!= {}, unitVectorOf[point[anchor] - point[First[parents]]],
       ! MemberQ[Join[roots, fixed], anchor], {0., 1.},
       mode[[1]] === "backbone", {0., If[Mean[Table[point[w][[2]], {w, Complement[loop, {anchor}]}]] < 0, -1., 1.]},
       True, With[{dv = Mean[Table[point[w], {w, Complement[loop, {anchor}]}]] - point[anchor]},
        If[vectorNorm[dv] < 0.05, {0., 1.}, unitVectorOf[dv]]]];
     n = Length[loop]; r = 0.55/Sin[Pi/n]; c = point[anchor] + r u;
     rotated = RotateLeft[loop, First[First[Position[loop, anchor, {1}]]] - 1];
     area = Total[Table[With[{p = point[rotated[[k]]], q = point[rotated[[Mod[k, n] + 1]]]}, p[[1]] q[[2]] - q[[1]] p[[2]]], {k, n}]];
     orient = If[area < 0, -1., 1.];
     th0 = angleOf[-u];
     newPoints = Table[{rotated[[k]], c + r directionAt[th0 + orient 2. Pi (k - 1)/n]}, {k, 2, n}];
     AppendTo[circles, {loop, c, r}]];
    If[newPoints =!= None && newPoints =!= {},
     positions = moveWithDependents[positions, newPoints, loop, Join[roots, fixed], pairs]],
    {loop, loops}];
   If[flip, positions = Table[{q[[1]], {q[[2, 1]], -q[[2, 2]]}}, {q, positions}];
    circles = Table[{q[[1]], {q[[2, 1]], -q[[2, 2]]}, q[[3]]}, {q, circles}]];
   {positions, circles}];

(* the ends {a, b} of the stretch of the axis (backbone) that a loop runs along: two or more consecutive vertices of the
   axis on the loop, joined around the loop by a path through them only; None if there is no such stretch *)
backboneSegmentOfLoop[backbone_, loop_] := Module[{ix = Flatten[Position[backbone, #, {1}] & /@ Select[loop, MemberQ[backbone, #] &]], seg,
    rotated, j, pathAB, pathBA},
   If[Length[ix] < 2 || Max[ix] - Min[ix] + 1 != Length[ix], Return[None]];
   seg = Take[backbone, {Min[ix], Max[ix]}];
   If[Complement[backbone, loop] === {}, Return[{First[backbone], Last[backbone]}]];
   rotated = RotateLeft[loop, First[First[Position[loop, First[seg], {1}]]] - 1];
   j = First[First[Position[rotated, Last[seg], {1}]]];
   pathAB = Table[rotated[[i]], {i, 2, j - 1}]; pathBA = Table[rotated[[i]], {i, j + 1, Length[rotated]}];
   If[(Complement[pathAB, seg] === {} || Complement[pathBA, seg] === {}) && Length[loop] > Length[seg], {First[seg], Last[seg]}, None]];

axisPositions[path_, spacing_, minimumHalfWidth_] := With[{L = Length[path] - 1}, With[{X = Max[minimumHalfWidth, L spacing/2]},
    Table[{path[[i]], {If[L == 0, 0., -X + 2. X (i - 1)/L], 0.}}, {i, Length[path]}]]];

(* leaf order of a tree, starting from the first external label (planar placement) *)
treeLeafOrder[ext_, pairs_] := Module[{order = {}, minimumIndex, visit},
   minimumIndex[v_, p_] := Min[Join[If[MemberQ[ext, v], {First[First[Position[ext, v, {1}]]]}, {}],
      minimumIndex[#, v] & /@ DeleteCases[neighboursOf[v, pairs], p]]];
   visit[v_, p_] := (If[MemberQ[ext, v], AppendTo[order, v]];
     Scan[visit[#, v] &, SortBy[DeleteCases[neighboursOf[v, pairs], p], minimumIndex[#, v] &]]);
   visit[First[ext], None];
   Join[order, Complement[ext, order]]];

(* a layout whose lines (node pairs, taken straight) do not cross, with no two nodes at one place and no node on a line that
   does not end at it *)
cleanLayoutQ[positions_, pairs_] := Module[{rules = Rule @@@ positions, segs, pts},
   segs = Select[DeleteDuplicates[Sort /@ pairs], MatrixQ[Replace[#, rules, {1}], NumberQ] &];
   pts = Cases[positions, {v_, p_} /; Head[v] =!= leg :> {v, p}];
   mutualCrossingCount[Replace[segs, rules, {2}]] == 0 &&
    ! AnyTrue[Subsets[pts[[All, 2]], {2}], vectorNorm[#[[1]] - #[[2]]] < 0.1 &] &&
    ! AnyTrue[pts, Function[q, AnyTrue[segs, ! MemberQ[#, q[[1]]] && segmentDistance[q[[2]], Replace[#, rules, {1}]] < 0.06 &]]]];
(* if the graph is a forest, the leaf orders of its trees one after the other (planar placement); else the given order *)
forestLeafOrder[ext_, free_, pairs_] := Module[{components = {}},
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, pairs]]], {v, ext}];
   If[Length[pairs] =!= Length[ext] + Length[free] - Length[components], ext,
    Join @@ Table[treeLeafOrder[Select[ext, MemberQ[c, #] &], Select[pairs, MemberQ[c, #[[1]]] &]], {c, components}]]];

(* ---------------------------------------------------------------- layouts *)
(* Layout of the components that contain external labels. externalNodes: the node of each external label (in order);
   amputated: True for the labels drawn with a leg. Legs end on the virtual nodes leg[label].
   Result: {positions, mode}. *)
layoutWithExternals[externalLabels_, externalNodes_, amputated_, free_, pairs_, propagatorPairs_, externalOrder_] := Module[
   {n = Length[externalLabels], backbone = None, order, R, th, positions, core, stripped, legPairs, fixed, fixedNodes,
    freeNodes, labelDirection, cornerLegNodes = {}, below = {}, paths, pathNodes, arcs},
   legPairs = Join @@ Table[If[amputated[[k]], {{externalNodes[[k]], leg[externalLabels[[k]]]}}, {}], {k, n}];
   If[n == 2 || n == 1,
    backbone = If[n == 1, {externalNodes[[1]]},
      With[{p = shortestPath[externalNodes[[1]], externalNodes[[2]], propagatorPairs]},
       If[p =!= None, p, shortestPath[externalNodes[[1]], externalNodes[[2]], pairs]]]]];
   If[backbone =!= None,
    (* a two-point function: the path joining the external points on the horizontal axis *)
    positions = axisPositions[backbone, If[Length[backbone] <= 3, 1.1, 0.95], If[Length[backbone] == 1, 0., 0.9]];
    positions = layeredLayout[positions, Select[DeleteDuplicates[free], ! MemberQ[backbone, #] &], pairs];
    (* a one-point function whose vertex is joined to a single other vertex by a bundle of lines (a melon, a bubble):
       the bundle along the axis, on the right of the vertex, so that the leg does not leave along one of its arcs *)
    If[n == 1 && amputated[[1]] && With[{nb = neighboursOf[externalNodes[[1]], pairs]},
        Length[nb] == 1 && MemberQ[$multiplePairs, Sort[{externalNodes[[1]], nb[[1]]}]]],
     With[{p0 = Replace[externalNodes[[1]], Rule @@@ positions]},
      positions = Table[{q[[1]], p0 + {q[[2, 2]] - p0[[2]], -(q[[2, 1]] - p0[[1]])}}, {q, positions}]]];
    (* arcs and hanging parts above or below the axis; the parts below are reflected *)
    With[{sides = axisSides[backbone, positions, pairs]},
     below = sides[[1]];
     positions = Table[If[MemberQ[sides[[2]], p[[1]]], {p[[1]], {1, -1} p[[2]]}, p], {p, positions}]];
    positions = Join[positions, Join @@ Table[If[amputated[[k]],
        With[{p = Replace[externalNodes[[k]], Rule @@@ positions]}, {{leg[externalLabels[[k]]], p + If[k == 1, {-$legLength, 0.}, {$legLength, 0.}]}}],
        {}], {k, n}]];
    {positions, {"backbone", backbone, False, below, {0., -0.6}}},
    (* the external points on a circle, counterclockwise from the top left; the node of several external labels
       (joined by delta functions) at their mean position; the legs point away in the directions of their labels *)
    order = Which[ListQ[externalOrder], Join[Select[externalOrder, MemberQ[externalLabels, #] &], Complement[externalLabels, externalOrder]],
      externalOrder === Automatic && n >= 3 && free =!= {} && ! Or @@ amputated,
      forestLeafOrder[externalNodes, free, pairs] /. Thread[externalNodes -> externalLabels],
      True, externalLabels];
    (* four points without an order (the sources of Z[J]): one node with two of them at opposite corners, the other two on
       other nodes, or two nodes each with a pair at opposite corners (a bubble in the channel (x1 x3|x2 x4)): the points at
       the right corners are exchanged, so that the legs of a node are neighbours and nothing crosses. With an order (the
       external labels of a four-point function) a node with two labels at opposite corners, which would be in the middle
       with its legs across the picture, has the labels on the right exchanged in the same way, and the caller crosses
       their legs back (crossedLegPositions) *)
    (* an external point at the end of a single line counts as the vertex it hangs from (two points joined to one vertex
       of phi^4 theory are placed like two labels of one node) *)
    If[n == 4,
     With[{attach = Table[With[{v = externalNodes[[k]]}, With[{nb = neighboursOf[v, pairs]},
          If[Count[externalNodes, v] == 1 && Length[nb] == 1 && MemberQ[free, First[nb]], First[nb], v]]], {k, n}]},
      With[{corners = Table[Sort[Table[First[First[Position[order, externalLabels[[k]], {1}]]],
            {k, Select[Range[n], attach[[#]] === node &]}]], {node, DeleteDuplicates[attach]}]},
       Which[
        ! ListQ[externalOrder],
        If[(Count[corners, {1, 3} | {2, 4}] == 1 && Length[DeleteDuplicates[attach]] == 3) ||
          (Count[corners, {1, 3} | {2, 4}] == 2 && Length[DeleteDuplicates[attach]] == 2), order = order[[{1, 2, 4, 3}]]],
        Count[corners, {1, 3} | {2, 4}] >= 1 && ! TrueQ[$cornersAsGiven],
        (* unless the picture with the labels as given is clean (a vertex at the centre of an X, with tadpoles on its arms) *)
        With[{given = Block[{$cornersAsGiven = True}, layoutWithExternals[externalLabels, externalNodes, amputated, free, pairs,
             propagatorPairs, externalOrder]]},
         If[cleanLayoutQ[given[[1]], Join[pairs, legPairs]] && cornerArrangement[given[[1]], externalLabels, externalNodes, amputated] === order,
          Return[given]]];
        order = order[[{1, 2, 4, 3}]]]]]];
    (* three labels on two nodes and nothing else (a bubble of a three-point vertex): the order turned so that the node
       of two labels is on the left, its legs at the corners on the left, and the third leg points to the right *)
    If[n == 3 && ! ListQ[externalOrder] && Length[DeleteDuplicates[externalNodes]] == 2 &&
      Complement[DeleteDuplicates[free], externalNodes] === {},
     With[{single = First[Select[Range[n], Count[externalNodes, externalNodes[[#]]] == 1 &]]},
      order = RotateLeft[order, First[First[Position[order, externalLabels[[single]], {1}]]]]]];
    th = Which[n == 1, {Pi}, n == 2, {Pi, 0.}, True, Table[Pi - Pi/n + 2. Pi (k - 1)/n, {k, n}]];
    fixedNodes = DeleteDuplicates[externalNodes];
    freeNodes = Select[DeleteDuplicates[free], ! MemberQ[fixedNodes, #] &];
    R = If[freeNodes === {}, If[Or @@ amputated, 1.1, 1.], Min[1.7, 1.15 + 0.1 (Length[freeNodes] - 1)]];
    labelDirection[e_] := directionAt[th[[First[First[Position[order, e, {1}]]]]]];
    fixed = Table[{v, R Mean[Table[labelDirection[externalLabels[[k]]], {k, Select[Range[n], externalNodes[[#]] === v &]}]]}, {v, fixedNodes}];
    (* two nodes at the same place (legs 1, 3 on one node and 2, 4 on the other: a crossed channel) are moved apart
       horizontally, the node of the first label on the left; their legs keep the vertical side of their labels and
       point away from the centre, so that the picture is a plain bubble with the labels exchanged on the right *)
    cornerLegNodes = {};
    Do[If[vectorNorm[fixed[[i, 2]] - fixed[[j, 2]]] < 0.3,
      fixed[[i, 2]] = fixed[[i, 2]] + {-0.5 R, 0.}; fixed[[j, 2]] = fixed[[j, 2]] + {0.5 R, 0.};
      cornerLegNodes = Union[cornerLegNodes, {fixed[[i, 1]], fixed[[j, 1]]}]],
     {i, Length[fixed]}, {j, i + 1, Length[fixed]}];
    (* all the legs on two vertices and nothing else (a bubble of a four-point vertex): the vertices as far apart in
       every channel *)
    If[Length[fixed] == 2 && freeNodes === {} && And @@ amputated,
     With[{dd = vectorNorm[fixed[[1, 2]] - fixed[[2, 2]]]}, If[dd > 1.15, fixed = Table[{f[[1]], f[[2]] 1.1/dd}, {f, fixed}]]]];
    (* no free vertices and a bubble (two or more lines) between two of three or more external points: the points are
       brought closer, so that the bubble keeps about the width of a bubble and the picture the size of its neighbours *)
    If[Length[fixed] >= 3 && freeNodes === {},
     With[{dmax = Max[Join[{0.}, Table[vectorNorm[Replace[pr[[1]], Rule @@@ fixed] - Replace[pr[[2]], Rule @@@ fixed]],
          {pr, Select[$multiplePairs, MemberQ[fixedNodes, #[[1]]] && MemberQ[fixedNodes, #[[2]]] &]}]]]},
      If[dmax > 1.15, fixed = Table[{f[[1]], f[[2]] 1.15/dmax}, {f, fixed}]]]];
    (* the open propagator lines (fermion lines) between two external points run straight from one to the other,
       with their vertices evenly spaced; lines joining two vertices of such a line are arcs, outside or inside *)
    (* only the lines between neighbouring external points (sides of the polygon); lines across it are placed with the
       rest *)
    paths = With[{around = SortBy[fixed, angleOf[#[[2]]] &][[All, 1]]},
      Select[openPropagatorPaths[fixedNodes, freeNodes, propagatorPairs], Function[path,
        With[{i = First[First[Position[around, path[[1]], {1}]]], j = First[First[Position[around, Last[path], {1}]]]},
         Mod[i - j, Length[around]] == 1 || Mod[j - i, Length[around]] == 1]]]];
    pathNodes = Union[Flatten[Table[Take[Drop[path, 1], Length[path] - 2], {path, paths}]]];
    (* room for the vertices of long lines: the picture is stretched along them *)
    With[{stretch = Table[With[{d = Replace[Last[path], Rule @@@ fixed] - Replace[path[[1]], Rule @@@ fixed]},
         With[{need = If[vectorNorm[d] < 10.^-6, 1., Max[1., 0.8 (Length[path] - 1)/vectorNorm[d]]]},
          {1. + (need - 1.) Abs[unitVectorOf[d][[1]]], 1. + (need - 1.) Abs[unitVectorOf[d][[2]]]}]], {path, paths}]},
     If[stretch =!= {},
      With[{sx = Min[1.7, Max[stretch[[All, 1]]]], sy = Min[1.7, Max[stretch[[All, 2]]]]},
       fixed = Table[{f[[1]], {sx f[[2, 1]], sy f[[2, 2]]}}, {f, fixed}]]]];
    fixed = Join[fixed, Flatten[Table[With[{pu = Replace[path[[1]], Rule @@@ fixed], pv = Replace[Last[path], Rule @@@ fixed], L = Length[path] - 1},
        Table[{path[[k]], pu + (k - 1)/L (pv - pu)}, {k, 2, L}]], {path, paths}], 1]];
    {core, stripped} = stripPendantVertices[fixed[[All, 1]], Select[freeNodes, ! MemberQ[pathNodes, #] &], pairs];
    (* a vertex on a leg (two lines, one to an external point) with something hanging from it (a tadpole, a chain of
       them): nearer the external point, where the legs are farther apart, and the legs longer, so that the hanging part
       is not wedged between two legs *)
    With[{hanging = Select[core, Length[neighboursOf[#, pairs]] == 2 && Intersection[neighboursOf[#, pairs], fixed[[All, 1]]] =!= {} &&
          (MemberQ[$selfLoopNodes, #] || Intersection[neighboursOf[#, pairs], stripped] =!= {}) &]},
     If[hanging =!= {} && paths === {},
      fixed = Table[{f[[1]], 1.15 f[[2]]}, {f, fixed}]];
     positions = Join[fixed, tutteEmbedding[fixed, core, pairs, hanging]]];
    positions = placePendantVertices[positions, stripped, pairs, {0., 0.}, fixedNodes];
    positions = pushApartVertices[positions, Select[freeNodes, ! MemberQ[pathNodes, #] &], 0.5];
    positions = Join[positions, Join @@ Table[If[amputated[[k]],
        {{leg[externalLabels[[k]]], With[{p = Replace[externalNodes[[k]], Rule @@@ positions], u = labelDirection[externalLabels[[k]]]},
           If[MemberQ[cornerLegNodes, externalNodes[[k]]],
            p + $legLength {If[p[[1]] < 0, -1., 1.], If[u[[2]] < 0, -1., 1.]}/Sqrt[2.],
            p + $legLength u]]}}, {}], {k, n}]];
    arcs = Flatten[Table[pathArcSides[path, pairs, positions, {0., 0.}], {path, paths}], 1];
    positions = refineFreeVertices[positions, Select[freeNodes, ! MemberQ[pathNodes, #] &],
      Join[Select[pairs, ! MemberQ[Table[Sort[a[[1]]], {a, arcs}], Sort[#]] &], legPairs]];
    {positions, {"circle", arcs, {0., 0.}}}]];

(* the open propagator lines between external points: {fixed node, free vertices on two propagator lines each, another
   fixed node} *)
openPropagatorPaths[fixedNodes_, freeNodes_, propagatorPairs_] := Module[{paths = {}, used = {}, path, prev, cur, nb},
   Do[Do[If[MemberQ[freeNodes, w] && ! MemberQ[used, w],
       path = {u, w}; prev = u; cur = w;
       While[MemberQ[freeNodes, cur] && Length[neighboursOf[cur, propagatorPairs]] == 2,
        nb = DeleteCases[neighboursOf[cur, propagatorPairs], prev];
        If[Length[nb] != 1, Break[]];
        prev = cur; cur = First[nb]; AppendTo[path, cur]];
       If[MemberQ[fixedNodes, Last[path]] && Last[path] =!= u && ! MemberQ[Most[path], Last[path]],
        AppendTo[paths, path]; used = Join[used, path]]],
      {w, neighboursOf[u, propagatorPairs]}], {u, fixedNodes}];
   paths];

(* sides of the lines joining non-consecutive vertices of an open propagator line drawn straight: two arcs crossing on
   the same side, or an arc inside the picture over a vertex joined to something inside, conflict; the assignment with
   the fewest conflicts, arcs outside preferred (more than 12 arcs: all outside). Result: {{u, v}, rank, sign} with the
   sign of the sagitta (left of u -> v) *)
pathArcSides[path_, pairs_, positions_, centre_] := Module[{ix, spans, attached, ns, best, point},
   point[v_] := Replace[v, Rule @@@ positions];
   ix[v_] := First[First[Position[path, v, {1}]]];
   spans = SortBy[DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[path, u] && MemberQ[path, v] && Abs[ix[u] - ix[v]] > 1 :>
        Sort[{ix[u], ix[v]}]]], {#[[2]] - #[[1]], #[[1]]} &];
   attached = Select[Range[2, Length[path] - 1], Function[k, AnyTrue[neighboursOf[path[[k]], pairs], ! MemberQ[path, #] &]]];
   ns = Length[spans];
   If[ns == 0, {},
    best = If[ns > 12, Table[0, {ns}], cheapestSides[ns, Function[sides,
        10 (Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
           Count[Table[sides[[i]] == 1 && spanOverQ[spans[[i]], attached], {i, ns}], True]) + Total[sides]]]];
    Table[With[{u = path[[spans[[i, 1]]]], v = path[[spans[[i, 2]]]]},
      With[{out = If[# == 0, 1., #] &[Sign[rotate90[point[v] - point[u]] . ((point[u] + point[v])/2 - centre)]]},
       {{u, v}, Count[Take[best, i], best[[i]]], If[best[[i]] == 1, -out, out]}]], {i, ns}]]];

(* A free vertex lying on a line that does not end at it, or on top of another vertex, is moved a little (by 0.25 or
   0.45, in eight directions) to the nearest place free of these defects where its lines cross no more lines than
   before (lines are taken as straight). *)
refineFreeVertices[positions_, free_, pairs_] := Module[{nodes = positions[[All, 1]], pts = positions[[All, 2]], ix, segs, freeIx,
    incident, othersOf, neighbourOf, bad, crossings, n, b0, c0, best, q},
   n = Length[nodes];
   ix[v_] := First[First[Position[nodes, v, {1}]]];
   segs = Table[{ix[pr[[1]]], ix[pr[[2]]]}, {pr, Select[pairs, MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &]}];
   freeIx = ix /@ Select[free, MemberQ[nodes, #] &];
   incident = Table[Select[segs, MemberQ[#, i] &], {i, n}];
   othersOf = Table[Select[segs, ! MemberQ[#, i] &], {i, n}];
   neighbourOf[sg_, i_] := If[sg[[1]] == i, sg[[2]], sg[[1]]];
   (* the vertex i at q on another line, another vertex on one of its lines (their other ends excepted), vertices on top
      of each other *)
   bad[i_, q_] := With[{rest = Delete[pts, i], mine = Table[{q, pts[[neighbourOf[sg, i]]]}, {sg, incident[[i]]}]},
     countBelow[distancesFromPoint[q, Table[{pts[[sg[[1]]]], pts[[sg[[2]]]]}, {sg, othersOf[[i]]}]], 0.18] +
      Total[Table[countBelow[distancesToSegment[rest, sg], 0.18] - 1, {sg, mine}]] +
      countBelow[Sqrt[(rest[[All, 1]] - q[[1]])^2 + (rest[[All, 2]] - q[[2]])^2], 0.4]];
   crossings[i_, q_] := crossingCount[Table[{q, pts[[neighbourOf[sg, i]]]}, {sg, incident[[i]]}],
     Table[{pts[[sg[[1]]]], pts[[sg[[2]]]]}, {sg, othersOf[[i]]}]];
   Do[b0 = bad[i, pts[[i]]];
    If[b0 > 0,
     c0 = crossings[i, pts[[i]]]; best = None;
     (* vertical moves first: they keep a vertex on the vertical axis of a symmetric picture *)
     Do[If[best === None,
       q = pts[[i]] + dd directionAt[Pi k/4.];
       If[bad[i, q] == 0 && crossings[i, q] <= c0, best = q]], {dd, {0.25, 0.45}}, {k, {2, 6, 0, 4, 1, 3, 5, 7}}];
     If[best =!= None, pts[[i]] = best]],
    {i, freeIx}];
   Table[{nodes[[i]], pts[[i]]}, {i, n}]];

(* A vacuum part made of several fermion loops (and single vertices) joined by interaction lines: every loop is a circle,
   two side by side, more around a circle in the order in which the lines between them cross least; then each loop is
   rotated, and its orientation chosen, so that these lines are short and cross neither each other nor a loop.
   Result: {positions, {"loops", circles, centre}} *)
layoutVacuumLoops[component_, loops_, pairs_] := Module[{units, radii, k, unitOf, links, superEdges, orders, crossings, order, theta,
    bubbleCircles, R, centres, angles, signs, positionsOf, linkData, vertexPoint, segmentsOf, lengthEnergy, penalty, best, e, pts, circles, concentric},
   units = Join[loops, List /@ Select[component, ! MemberQ[Flatten[loops], #] &]];
   radii = Table[Which[Length[u] == 1, 0., Length[u] == 2, 0.5, True, 0.55/Sin[Pi/Length[u]]], {u, units}];
   k = Length[units];
   unitOf[v_] := First[First[Position[units, v, {2}]]];
   links = Select[pairs, unitOf[#[[1]]] =!= unitOf[#[[2]]] &];
   superEdges = DeleteDuplicates[Table[Sort[{unitOf[l[[1]]], unitOf[l[[2]]]}], {l, links}]];
   (* the order of the units around the circle: fewest crossings of the lines between them, then neighbours joined *)
   crossings[o_] := Module[{pos = Table[First[First[Position[o, i, {1}]]], {i, k}], between},
     between[a_, b_, x_] := With[{lo = Min[pos[[a]], pos[[b]]], hi = Max[pos[[a]], pos[[b]]]}, lo < pos[[x]] < hi];
     Count[Flatten[Table[With[{s1 = superEdges[[i]], s2 = superEdges[[j]]},
         Length[Union[s1, s2]] == 4 && Xor[between[s1[[1]], s1[[2]], s2[[1]]], between[s1[[1]], s1[[2]], s2[[2]]]]],
        {i, Length[superEdges]}, {j, i + 1, Length[superEdges]}]], True] +
      0.01 Total[Table[With[{d = Abs[pos[[se[[1]]]] - pos[[se[[2]]]]]}, Min[d, k - d]], {se, superEdges}]]];
   orders = If[k <= 6, Table[Prepend[q, 1], {q, Permutations[Range[2, k]]}], {Range[k]}];
   order = First[SortBy[orders, crossings]];
   R = If[k == 2, (radii[[1]] + radii[[2]] + 1.1)/2,
     Max[Table[(radii[[order[[i]]]] + radii[[order[[Mod[i, k] + 1]]]] + 1.)/(2 Sin[Pi/k]), {i, k}]]];
   centres = Table[{0., 0.}, {k}];
   concentric = k == 2 && Length[links] >= 3 && Min[Length /@ units] >= 3;
   If[concentric,
    (* two loops joined by three or more lines: one inside the other, the lines as spokes *)
    With[{outer = If[Length[units[[2]]] > Length[units[[1]]], 2, 1]},
     radii[[3 - outer]] = 0.45; radii[[outer]] = Max[radii[[outer]], 1.45]],
    Do[centres[[order[[i]]]] = R directionAt[If[k == 2, Pi, Pi/2] + 2. Pi (i - 1)/k], {i, k}]];
   (* each loop: its first vertex at the angle angles[[u]], the others following in the direction signs[[u]] *)
   angles = Table[With[{nb = Select[superEdges, MemberQ[#, u] &]},
      If[nb === {}, 0., angleOf[Mean[Table[centres[[First[DeleteCases[se, u]]]], {se, nb}]] - centres[[u]]]]], {u, k}];
   signs = Table[1., {k}];
   positionsOf[a_, s_] := Flatten[Table[Table[{units[[u, j]], centres[[u]] + radii[[u]] directionAt[a[[u]] + s[[u]] 2. Pi (j - 1)/Length[units[[u]]]]},
       {j, Length[units[[u]]]}], {u, k}], 1];
   (* the lines between units as {unit, index, unit, index} *)
   linkData = Table[{unitOf[l[[1]]], First[First[Position[units[[unitOf[l[[1]]]]], l[[1]], {1}]]],
      unitOf[l[[2]]], First[First[Position[units[[unitOf[l[[2]]]]], l[[2]], {1}]]]}, {l, links}];
   vertexPoint[u_, j_, a_, s_] := centres[[u]] + radii[[u]] directionAt[a[[u]] + s[[u]] 2. Pi (j - 1)/Length[units[[u]]]];
   segmentsOf[a_, s_] := Table[{vertexPoint[d[[1]], d[[2]], a, s], vertexPoint[d[[3]], d[[4]], a, s]}, {d, linkData}];
   lengthEnergy[segs_] := Total[Table[(sg[[1]] - sg[[2]]) . (sg[[1]] - sg[[2]]), {sg, segs}]];
   penalty[segs_] := 8 (mutualCrossingCount[segs] + If[concentric, 0, Count[Flatten[Table[If[Length[units[[u]]] >= 2,
            Table[segmentDistance[centres[[u]], sg] < radii[[u]] - 0.05, {sg, segs}], {}], {u, k}]], True]]);
   Do[Do[If[Length[units[[u]]] >= 2,
      With[{segs0 = segmentsOf[angles, signs]}, best = {lengthEnergy[segs0] + penalty[segs0], angles[[u]], signs[[u]]}];
      Do[With[{a2 = ReplacePart[angles, u -> th], s2 = ReplacePart[signs, u -> sg]},
        With[{segs = segmentsOf[a2, s2]},
         e = lengthEnergy[segs];
         If[e < best[[1]] - 10.^-9, e = e + penalty[segs]; If[e < best[[1]] - 10.^-9, best = {e, th, sg}]]]], {sg, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}];
      angles[[u]] = best[[2]]; signs[[u]] = best[[3]]], {u, k}], {3}];
   (* then the vertices joined to other units slide along their loop towards them (two lines to another unit from
      opposite vertices of a loop would make a wide ring around it), each at least half the even spacing from its
      neighbours on the loop; a move is kept only if it shortens the lines clearly *)
   theta = Table[angles[[u]] + signs[[u]] 2. Pi (j - 1)/Length[units[[u]]], {u, k}, {j, Length[units[[u]]]}];
   If[! concentric,
    With[{linked = DeleteDuplicates[Join[linkData[[All, {1, 2}]], linkData[[All, {3, 4}]]]]},
     Module[{pointOf, energy, n, sg, prev, next, dTot, eBest, e0, thBest, gmin},
      pointOf[th_, u_, j_] := centres[[u]] + radii[[u]] directionAt[th[[u, j]]];
      energy[th_] := With[{segs = Table[{pointOf[th, d[[1]], d[[2]]], pointOf[th, d[[3]], d[[4]]]}, {d, linkData}]},
        lengthEnergy[segs] + penalty[segs]];
      Do[Do[Do[If[Length[units[[u]]] >= 3 && MemberQ[linked, {u, j}],
         n = Length[units[[u]]]; sg = signs[[u]]; gmin = Pi/n;
         prev = theta[[u, Mod[j - 2, n] + 1]]; next = theta[[u, Mod[j, n] + 1]];
         dTot = sg (next - prev); dTot = dTot - 2. Pi Floor[dTot/(2. Pi)];
         If[dTot > 2 gmin + 10.^-6,
          e0 = energy[theta]; eBest = 0.9 e0 - 0.05; thBest = theta[[u, j]];
          Do[With[{th2 = ReplacePart[theta, {u, j} -> prev + sg (gmin + (dTot - 2 gmin) tt)]},
            With[{e2 = energy[th2]}, If[e2 < eBest, eBest = e2; thBest = th2[[u, j]]]]], {tt, 0., 1., 1./12}];
          theta[[u, j]] = thBest]], {j, Length[units[[u]]]}], {u, k}], {2}]]]];
   (* a bubble (a loop of two vertices) joined at each vertex by one line to another bubble (two bubbles side by side,
      the ring of second order): its vertices on the side facing that loop, 50 degrees from the line of the centres,
      so that the lines leave the circle at a wide angle (at the top and the bottom they would be tangent to it); the
      bubble is then drawn as its circle, a short and a long arc *)
   bubbleCircles = {};
   If[! concentric, Do[If[Length[units[[u]]] == 2,
      With[{ls = Table[Select[linkData, (#[[1]] == u && #[[2]] == j) || (#[[3]] == u && #[[4]] == j) &], {j, 2}]},
       If[Length[ls[[1]]] == 1 && Length[ls[[2]]] == 1,
        With[{w = Table[If[ls[[j, 1, 1]] == u, ls[[j, 1, 3]], ls[[j, 1, 1]]], {j, 2}]},
         If[w[[1]] == w[[2]] && w[[1]] != u && Length[units[[w[[1]]]]] == 2,
          With[{dir = unitVectorOf[centres[[w[[1]]]] - centres[[u]]], phi = angleOf[centres[[w[[1]]]] - centres[[u]]]},
           With[{sides = Table[With[{q = directionAt[theta[[u, j]]]}, Sign[dir[[1]] q[[2]] - dir[[2]] q[[1]]]], {j, 2}]},
            With[{sg = If[sides[[1]] != 0 && sides[[1]] != sides[[2]], sides, {1, -1}]},
             theta[[u, 1]] = phi + sg[[1]] 50. Degree; theta[[u, 2]] = phi + sg[[2]] 50. Degree;
             AppendTo[bubbleCircles, u]]]]]]]]], {u, k}]];
   pts = Flatten[Table[Table[{units[[u, j]], centres[[u]] + radii[[u]] directionAt[theta[[u, j]]]}, {j, Length[units[[u]]]}], {u, k}], 1];
   circles = Table[{units[[u]], centres[[u]], radii[[u]]}, {u, Select[Range[k], Length[units[[#]]] >= 3 || MemberQ[bubbleCircles, #] &]}];
   {pts, {"loops", circles, {0., 0.}}}];

(* layout of a vacuum component: several fermion loops, or one cycle with the rest around it *)
layoutVacuumComponent[component_, pairs_, propagatorPairs_, lines_ : {}] := With[{loops = fermionLoops[component, lines, 2],
    scalar = scalarLoops[component, lines]},
   Which[
    Length[loops] >= 2, layoutVacuumLoops[component, loops, pairs],
    (* every vertex on a closed loop of undirected propagators (a theory with a Hubbard-Stratonovich field): the loops
       drawn like fermion loops (two loops joined by three lines one inside the other) *)
    Length[scalar] >= 2 && Complement[component, Flatten[scalar]] === {}, layoutVacuumLoops[component, scalar, pairs],
    True, layoutVacuumCycle[component, pairs, propagatorPairs, lines]]];

(* the cycle, rotated so that the reflection about the vertical axis (through the top vertex, or through the middle of the
   top side) maps the lines between its vertices, with their multiplicities, and the number of line ends at each vertex
   to themselves; unchanged if no rotation does *)
symmetricCycleRotation[cycle_, lines_, topVertex_] := Module[{L = Length[cycle], refl, mult, ends, sym, rots},
   refl = If[topVertex, Table[Mod[1 - k, L] + 1, {k, L}], Table[L + 1 - k, {k, L}]];
   mult[u_, w_] := Count[lines, {a_, b_, _, _} /; Sort[{a, b}] === Sort[{u, w}]];
   ends[v_] := Count[lines, {v, _, _, _}] + Count[lines, {_, v, _, _}];
   sym[c_] := AllTrue[Range[L], ends[c[[#]]] == ends[c[[refl[[#]]]]] &] &&
     AllTrue[Subsets[Range[L], {2}], mult[c[[#[[1]]]], c[[#[[2]]]]] == mult[c[[refl[[#[[1]]]]]], c[[refl[[#[[2]]]]]]] &];
   (* only for a cycle through an insertion on a line (a vertex with two line ends, the mass lambda phibar^2/2), all its
      vertices on lines (no blobs) *)
   If[lines === {} || L < 3 || ! MemberQ[ends /@ cycle, 2] || Min[ends /@ cycle] < 2 || sym[cycle], cycle,
    rots = Select[Table[RotateLeft[cycle, s], {s, L - 1}], sym, 1];
    If[rots === {}, cycle, First[rots]]]];

(* the closed loops of undirected propagators through vertices that end exactly two of them (no self-loop), in order *)
scalarLoops[nodes_, lines_] := Module[{und, vs, loops = {}, comp, ordered, prev, cur},
   und = Cases[lines, {u_, w_, _, st_} /; u =!= w && ! fermionStyleQ[st] && ! interactionStyleQ[st] && MemberQ[nodes, u] && MemberQ[nodes, w] :> {u, w}];
   vs = Select[nodes, Count[Flatten[und], #] == 2 && ! MemberQ[Cases[lines, {x_, x_, _, st_} /; ! interactionStyleQ[st] :> x], #] &];
   und = Select[und, MemberQ[vs, #[[1]]] && MemberQ[vs, #[[2]]] &];
   Do[If[FreeQ[loops, v] && MemberQ[Flatten[und], v],
     comp = componentOf[v, und];
     If[Length[comp] >= 2 && AllTrue[comp, Count[Flatten[und], #] == 2 &],
      ordered = {v}; prev = None; cur = v;
      While[Length[ordered] < Length[comp],
       prev = cur; cur = First[Select[neighboursOf[prev, und], ! MemberQ[ordered, #] &]]; AppendTo[ordered, cur]];
      AppendTo[loops, ordered]]], {v, vs}];
   loops];

layoutVacuumCycle[component_, pairs_, propagatorPairs_, lines_ : {}] := Module[{cycle, propagatorCycle = True, L, r, positions, core, stripped,
    best, p, th0},
   cycle = If[Length[component] == 1, {}, longestCycle[component, propagatorPairs]];
   If[Length[component] > 1 && Length[cycle] < 3, propagatorCycle = False; cycle = longestCycle[component, pairs]];
   Which[
    Length[component] == 1, {{{First[component], {0., 0.}}}, {"free", {0., 0.}}},
    Length[cycle] < 3,
    (* no cycle: the longest shortest path on the axis *)
    best = {First[component]};
    Do[p = shortestPath[component[[i]], component[[j]], pairs]; If[p =!= None && Length[p] > Length[best], best = p],
     {i, Length[component]}, {j, i + 1, Length[component]}];
    {layeredLayout[axisPositions[best, 1., 0.], Select[component, ! MemberQ[best, #] &], pairs], {"backbone", best, True, {}, {0., -0.6}}},
    True,
    L = Length[cycle];
    If[propagatorCycle,
     r = 0.55/Sin[Pi/L]; th0 = Pi/2 - If[EvenQ[L], Pi/L, 0.];
     (* two diameters (two crossing chords): a vertex at the top, so that one diameter is vertical (drawn inside) and,
        in a loop of four, the other horizontal (drawn outside, over the top) *)
     If[EvenQ[L] && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + L/2]]}]], {k, L/2}], True] == 2, th0 = Pi/2];
     (* one diameter crossed by other chords, or three diameters: a diameter horizontal, so that the chords crossing it
        are vertical (a "#", not a slanted tangle); of three diameters the horizontal one is drawn outside, over the top,
        and the other two cross at the centre *)
     With[{dk = If[EvenQ[L], Select[Range[L/2], MemberQ[pairs, Sort[{cycle[[#]], cycle[[# + L/2]]}]] &], {}],
       chordCount = Length[Select[Subsets[Range[L], {2}], MemberQ[pairs, Sort[cycle[[#]]]] && Mod[#[[2]] - #[[1]], L] != 1 && Mod[#[[1]] - #[[2]], L] != 1 &]]},
      If[Length[dk] == 3 || Length[dk] == 1 && chordCount >= 2, th0 = Pi - 2. Pi (Last[dk] - 1)/L]];
     (* a rotation of the cycle whose picture is symmetric about the vertical axis, if the first one is not and the cycle
        goes through an insertion on a line (the basketball with a mass insertion on one line: the insertion at the top) *)
     cycle = symmetricCycleRotation[cycle, lines, OddQ[L] || th0 == Pi/2];
     (* four or more diameters would meet at the centre: the vertices are moved alternately apart, so that the
        diameters become chords crossing in pairs around the centre *)
     With[{shift = If[EvenQ[L] && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + L/2]]}]], {k, L/2}], True] >= 4,
         0.5 Pi/L, 0.]},
      positions = Table[{cycle[[k]], r directionAt[th0 + 2. Pi (k - 1)/L + shift (-1)^k]}, {k, L}];
      (* three diameters of a loop of six: one drawn outside, over the top, from two vertices raised 25 degrees above the
         horizontal, so that its arc leaves them away from the loop, not along it; the other two cross at the centre *)
      If[L == 6 && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + 3]]}]], {k, 3}], True] == 3,
       With[{k0 = With[{a = (Pi - th0) L/(2. Pi)}, Round[a - L Floor[a/L]]] + 1},
        positions = Table[{cycle[[k]], r directionAt[Pi + {-25., 55., 125., 205., 235., 305.}[[Mod[k - k0, 6] + 1]] Degree]}, {k, L}]]]];
     (* a ring through blobs (Tr ln(1 - G0 Sigma) with Sigma blobs): the blobs evenly around a circle wide enough for
        them, symmetric about the vertical axis (one blob at the bottom, two left and right, three with one at the bottom),
        the other vertices evenly between them *)
     With[{bk = Flatten[Position[cycle, _blob, {1}, Heads -> False]]},
      If[bk =!= {},
       With[{nb = Length[bk], rb = Max[Table[Replace[cycle[[k]], Append[$blobRadii, _ -> $blobRadius]], {k, bk}]]},
        With[{th = If[EvenQ[nb], 0., -Pi/2], gaps = Table[Mod[bk[[Mod[i, nb] + 1]] - bk[[i]], L, 1], {i, nb}]},
         r = Max[0.55/Sin[Pi/L], 1.7 rb, If[nb >= 2, (2 rb + 0.9)/(2 Sin[Pi/nb]), 0.]];
         positions = Flatten[Table[Table[{cycle[[Mod[bk[[i]] + j - 1, L] + 1]],
              r directionAt[th + 2. Pi (i - 1)/nb + (2. Pi/nb) j/gaps[[i]]]}, {j, 0, gaps[[i]] - 1}], {i, nb}], 1]]]]],
     (* sides made of interaction lines only are longer; then the principal axis is made horizontal *)
     With[{wt = Table[If[MemberQ[propagatorPairs, Sort[{cycle[[k]], cycle[[Mod[k, L] + 1]]}]], 1., 1.8], {k, L}]},
      With[{ang = 2. Pi FoldList[Plus, 0., Most[wt]]/Total[wt]},
       r = 0.5/Sin[Pi Min[wt]/Total[wt]];
       positions = Table[{cycle[[k]], r directionAt[ang[[k]]]}, {k, L}];
       With[{sxx = Total[(#[[2, 1]]^2) & /@ positions], syy = Total[(#[[2, 2]]^2) & /@ positions], sxy = Total[(#[[2, 1]] #[[2, 2]]) & /@ positions]},
        With[{phi = If[Abs[sxx - syy] + Abs[sxy] < 10.^-9, 0., ArcTan[sxx - syy, 2 sxy]/2]},
         positions = {#[[1]], {{Cos[phi], Sin[phi]}, {-Sin[phi], Cos[phi]}} . #[[2]]} & /@ positions]]]]];
    {core, stripped} = stripPendantVertices[cycle, Select[component, ! MemberQ[cycle, #] &], pairs];
    positions = Join[positions, tutteEmbedding[positions, core, pairs]];
    positions = placePendantVertices[positions, stripped, pairs, {0., 0.}];
    {pushApartVertices[positions, Select[component, ! MemberQ[cycle, #] &], 0.5], {"cycle", cycle, {0., 0.}, propagatorCycle, r}}]];

(* ---------------------------------------------------------------- fermion lines and units *)
(* In a diagram of fermions with density-type couplings (two-body interactions, phonons, impurities), every vertex lies
   on exactly one fermion line: an open line between external points, or a closed loop (a vertex with a line G(z, z)
   is a loop by itself). Such a diagram is drawn unit by unit. The open lines are straight: the horizontal axis of a
   two-point function, two parallel rails for a four-point function. Every loop is a circle, or a bubble when it has
   two vertices. The interaction lines between the units are straight, and what hangs from a line or a loop points
   away from it, radially; the arcs of a line go to the side where they cross nothing. The spacing along the lines
   leaves room for what hangs from them, so that nothing overlaps. *)

$linkLength = 0.85; $bubbleWidth = 1.; $axisSpacing = 1.; $railSeparation = 1.2;
unitRadius[k_] := If[k >= 3, 0.55/Sin[Pi/k], 0.];

(* {open lines, loops}, or None if some node is not on exactly one fermion line. An open line runs from its incoming
   to its outgoing end; an external node without fermion lines (a vertex of an amputated function with an incoming and
   an outgoing leg, its labels joined by a delta function) is an open line of length zero. Two inner nodes without
   fermion lines joined by two interaction lines (a gluon loop) are a loop too. *)
(* with anomalous lines: chains walked as undirected paths, every inner vertex with one psi end and one bar end (the
   arrows turn inside the anomalous lines); an open chain starts at its bar end (as above, where the arrow leaves), or at
   its first node when both its ends are of one kind (the chains of Delta, psi psi, and Delta-bar, bar bar); a loop is
   walked along the arrow that leaves its first vertex. Pictures without anomalous lines take the definition below. *)
fermionLines[nodes_List, lines_List, externalNodes_List] /; anomalousLinesQ[Select[lines, MemberQ[nodes, #[[1]]] &]] :=
  Catch[Module[{fl = Select[lines, MemberQ[nodes, #[[1]]] &], edges, selfLoops, endsAt, kindsAt, other, paths = {}, loops = {}, seen = {},
     walk, starts, undirected, bubbles, path},
    If[AnyTrue[fl, ! fermionStyleQ[#[[4]]] && ! interactionStyleQ[#[[4]]] &], Throw[None, fermionLines]];
    selfLoops = Cases[fl, {u_, u_, _, st_} /; directedStyleQ[st] :> u];
    If[Length[selfLoops] =!= Length[Union[selfLoops]], Throw[None, fermionLines]];
    edges = Select[fl, fermionStyleQ[#[[4]]] && #[[1]] =!= #[[2]] &];
    endsAt[x_] := Select[Range[Length[edges]], MemberQ[edges[[#, {1, 2}]], x] &];
    kindsAt[x_] := Sort[Table[lineEndKind[edges[[i]], x], {i, endsAt[x]}]];
    other[i_, x_] := If[edges[[i, 1]] === x, edges[[i, 2]], edges[[i, 1]]];
    undirected = Cases[fl, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w :> Sort[{u, w}]];
    bubbles = Select[DeleteDuplicates[undirected], Count[undirected, #] == 2 &&
        AllTrue[#, Function[x, endsAt[x] === {} && ! MemberQ[selfLoops, x] && ! MemberQ[externalNodes, x]]] &];
    If[Length[Flatten[bubbles]] =!= Length[Union[Flatten[bubbles]]], Throw[None, fermionLines]];
    Do[If[! Which[MemberQ[Flatten[bubbles], x], True, MemberQ[selfLoops, x], endsAt[x] === {},
        MemberQ[externalNodes, x], Length[endsAt[x]] <= 1, True, kindsAt[x] === {"bar", "psi"}], Throw[None, fermionLines]], {x, nodes}];
    (* the walk from x along the edge i0, to the end of the chain or back to its start *)
    walk[x_, i0_] := Module[{p = {x}, i = i0, v = other[i0, x], next},
      While[v =!= x,
       If[MemberQ[p, v], Throw[None, fermionLines]]; AppendTo[p, v];
       next = DeleteCases[endsAt[v], i];
       If[next === {}, Break[]]; i = First[next]; v = other[i, v]];
      p];
    starts = Select[nodes, Length[endsAt[#]] == 1 &];
    starts = Join[Select[starts, kindsAt[#] === {"bar"} &], Select[starts, kindsAt[#] =!= {"bar"} &]];
    Do[If[! MemberQ[seen, x], path = walk[x, First[endsAt[x]]]; AppendTo[paths, path]; seen = Join[seen, path]], {x, starts}];
    Do[If[MemberQ[externalNodes, x] && endsAt[x] === {} && ! MemberQ[selfLoops, x], AppendTo[paths, {x}]], {x, nodes}];
    seen = Join[Flatten[paths], Flatten[bubbles]];
    Do[If[! MemberQ[seen, x],
      If[MemberQ[selfLoops, x], AppendTo[loops, {x}]; AppendTo[seen, x],
       path = walk[x, First[Select[endsAt[x], lineEndKind[edges[[#]], x] === "bar" &]]];
       AppendTo[loops, path]; seen = Join[seen, path]]], {x, nodes}];
    {paths, Join[loops, bubbles]}], fermionLines];
fermionLines[nodes_List, lines_List, externalNodes_List] := Catch[Module[{directed, selfLoops, inDeg, outDeg, succ, paths = {},
     loops = {}, seen, path, v, undirected, bubbles},
    If[AnyTrue[lines, MemberQ[nodes, #[[1]]] && ! directedStyleQ[#[[4]]] && ! interactionStyleQ[#[[4]]] &], Throw[None, fermionLines]];
    directed = Cases[lines, {u_, w_, _, st_} /; directedStyleQ[st] && MemberQ[nodes, u] :> {u, w}];
    selfLoops = Cases[directed, {u_, u_} :> u];
    If[Length[selfLoops] =!= Length[Union[selfLoops]], Throw[None, fermionLines]];
    directed = Select[directed, #[[1]] =!= #[[2]] &];
    inDeg[x_] := Count[directed, {_, x}]; outDeg[x_] := Count[directed, {x, _}];
    succ[x_] := Cases[directed, {x, w_} :> w];
    undirected = Cases[lines, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w && MemberQ[nodes, u] :> Sort[{u, w}]];
    bubbles = Select[DeleteDuplicates[undirected], Count[undirected, #] == 2 &&
        AllTrue[#, Function[x, inDeg[x] == 0 && outDeg[x] == 0 && ! MemberQ[selfLoops, x] && ! MemberQ[externalNodes, x]]] &];
    If[Length[Flatten[bubbles]] =!= Length[Union[Flatten[bubbles]]], Throw[None, fermionLines]];
    Do[With[{i = inDeg[x], o = outDeg[x]},
      If[! Which[MemberQ[Flatten[bubbles], x], True, MemberQ[selfLoops, x], i == 0 && o == 0, MemberQ[externalNodes, x], i + o <= 1, True, i == 1 && o == 1],
       Throw[None, fermionLines]]], {x, nodes}];
    Do[If[inDeg[x] == 0 && outDeg[x] == 1,
      path = {x}; v = x;
      While[outDeg[v] == 1, v = First[succ[v]]; If[MemberQ[path, v], Throw[None, fermionLines]]; AppendTo[path, v]];
      AppendTo[paths, path]], {x, nodes}];
    Do[If[MemberQ[externalNodes, x] && inDeg[x] == 0 && outDeg[x] == 0 && ! MemberQ[selfLoops, x], AppendTo[paths, {x}]], {x, nodes}];
    seen = Join[Flatten[paths], Flatten[bubbles]];
    Do[If[! MemberQ[seen, x],
      If[MemberQ[selfLoops, x], AppendTo[loops, {x}]; AppendTo[seen, x],
       path = {x}; v = First[succ[x]];
       While[v =!= x, If[MemberQ[path, v], Throw[None, fermionLines]]; AppendTo[path, v]; v = First[succ[v]]];
       AppendTo[loops, path]; seen = Join[seen, path]]], {x, nodes}];
    {paths, Join[loops, bubbles]}], fermionLines];

(* the interaction lines of the nodes, as pairs (one entry per line) *)
interactionPairsOf[nodes_List, lines_List] := Cases[lines, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w && MemberQ[nodes, u] :> {u, w}];

(* blocks: the groups of loops joined to each other by interaction lines (not through the open lines) *)
loopBlocks[loops_List, ipairs_List] := Module[{loopNodes = Flatten[loops], links, blocks = {}, comp},
   links = Join[Select[ipairs, MemberQ[loopNodes, #[[1]]] && MemberQ[loopNodes, #[[2]]] &],
     Flatten[Table[Table[{l[[i]], l[[Mod[i, Length[l]] + 1]]}, {i, Length[l]}], {l, loops}], 1]];
   Do[If[FreeQ[blocks, First[lp]],
     comp = componentOf[First[lp], links];
     AppendTo[blocks, Select[loops, MemberQ[comp, First[#]] &]]], {lp, loops}];
   blocks];

(* the lines from the vertices of a unit to placed nodes: {{vertex, placed node}, ...} *)
attachmentsOf[unit_List, ipairs_List, placedNodes_List] := Flatten[Table[{v, w}, {v, unit},
    {w, Select[neighboursOf[v, ipairs], MemberQ[placedNodes, #] && ! MemberQ[unit, #] &]}], 1];

(* the difference of two angles, in (-Pi, Pi] *)
angleDifference[a_, b_] := With[{d = a - b}, d - 2. Pi Floor[(d + Pi)/(2. Pi)]];

(* Place one unit (a loop in fermion order) joined by the lines att = {{vertex, point}, ...} to points already placed,
   on the side dir of them. Result: {{{vertex, point}, ...}, centre, circle {loop, centre, radius} or None}. A loop of
   three or more vertices is a circle with its vertices evenly spaced, turned so that the joined vertices face their
   points (one joined vertex: exactly opposite to dir); a bubble lies along dir, or across it when both its vertices
   are joined. extra: points the other vertices of the unit are joined to (the unit turns towards them). *)
placeUnit[unit_List, att_List, dir_, extra_List : {}] := Module[{k = Length[unit], P = att[[All, 2]], m, n = unitVectorOf[dir], t0},
   m = Mean[P]; t0 = rotate90[n];
   Which[
    k == 1, With[{q = m + $linkLength n}, {{{unit[[1]], q}}, q, None}],
    k == 2 && Length[att] == 1,
    With[{a = att[[1, 1]], b = First[DeleteCases[unit, att[[1, 1]]]]},
     {{{a, m + $linkLength n}, {b, m + ($linkLength + $bubbleWidth) n}}, m + ($linkLength + $bubbleWidth/2) n, None}],
    k == 2,
    (* a bubble joined at both vertices: across dir, as wide as the points it is joined to are apart if they are close;
       if they are further apart, it keeps its width and goes further out, its lines bent into one round arc with it
       (frameItems) *)
    With[{p1 = Select[att, #[[1]] === unit[[1]] &][[1, 2]], p2 = Select[att, #[[1]] === unit[[2]] &][[1, 2]]},
     With[{sg = If[(p2 - p1) . t0 < 0, -1., 1.], spread = Abs[(p2 - p1) . t0]},
      With[{w = If[spread <= 1.25 $bubbleWidth, Max[$bubbleWidth, spread], $bubbleWidth],
        h = If[spread <= 1.25 $bubbleWidth, $linkLength, Max[$linkLength, Min[spread/2, $linkLength + $bubbleWidth/2]]]},
       {{{unit[[1]], m + h n - sg w/2 t0}, {unit[[2]], m + h n + sg w/2 t0}}, m + h n, None}]]],
    True,
    Module[{r0 = unitRadius[k], idx, best = None, bestScore = Infinity, offsets, mbar, rho, r, th0, c, pts, segs, score, candidates, ideal},
     idx = Table[First[First[Position[unit, a[[1]], {1}]]], {a, att}];
     Do[
      offsets = Table[s 2. Pi (i - 1)/k, {i, idx}];
      rho = vectorNorm[{Mean[Cos[offsets]], Mean[Sin[offsets]]}];
      mbar = angleOf[{Total[Cos[offsets]], Total[Sin[offsets]]}];
      (* two joined vertices: the radius grows (up to 1.25) so that they are as far apart as their points *)
      r = r0;
      If[Length[att] == 2, With[{chord = 2 Abs[Sin[angleDifference[offsets[[1]], offsets[[2]]]/2]], dd = Abs[(P[[2]] - P[[1]]) . t0]},
        If[chord > 0.1, r = Min[1.25, Max[r0, dd/chord]]]]];
      ideal = angleOf[-n] - mbar;
      candidates = If[rho > 0.15, {ideal}, Join[{ideal}, Table[Pi k2/12., {k2, 0, 23}]]];
      Do[
       pts = Table[{unit[[j]], directionAt[th + s 2. Pi (j - 1)/k]}, {j, k}];
       (* the centre that puts the joined vertices nearest to $linkLength from their points along n *)
       c = Mean[Table[P[[i]] + $linkLength n - r Replace[att[[i, 1]], Rule @@@ pts], {i, Length[att]}]];
       pts = Table[{p[[1]], c + r p[[2]]}, {p, pts}];
       segs = Table[{Replace[att[[i, 1]], Rule @@@ pts], P[[i]]}, {i, Length[att]}];
       score = 10 mutualCrossingCount[segs] + Total[Table[(vectorNorm[sg[[1]] - sg[[2]]] - $linkLength)^2, {sg, segs}]] +
         10 Boole[(c - m) . n < 0.5 r] + 0.001 Abs[angleDifference[th, ideal]] +
         (* the other vertices near the points they are joined to *)
         0.01 Total[Table[vectorNorm[Replace[e[[1]], Rule @@@ pts] - e[[2]]], {e, extra}]];
       If[score < bestScore - 10.^-6, bestScore = score; best = {pts, c, {unit, c, r}}],
       {th, candidates}],
      {s, {1., -1.}}];
     best]]];

(* A loop joined at several of its vertices to points on a line (the axis or a rail), on the side n of it: the joined
   vertices on the lower half of the circle, each right above its point, so that the lines are perpendicular to the
   line (if the points are further apart than the loop is wide, spread over its lowest 100 degrees); the other vertices evenly spaced between them, those with something hanging from them preferably on the
   upper half. Result as for placeUnit, or None if the order of the joined vertices around the loop does not match
   their order along the line. *)
placeLoopOnLine[unit_List, att_List, n_, hanging_List] := Module[{k = Length[unit], nn = unitVectorOf[n], t, us, sorted, idx, best = None,
    bestScore = Infinity, cw, joined, angleOf2, free, uc, r, hc, centre, pts, score, frame, v0, ang, prevJ, nextJ, segment, radial},
   t = -rotate90[nn];
   frame[phi_] := Cos[phi] t + Sin[phi] nn;
   us = Table[a[[2]] . t, {a, att}];
   sorted = att[[Ordering[us]]];
   v0 = Mean[Table[a[[2]] . nn, {a, att}]];
   idx[v_] := First[First[Position[unit, v, {1}]]];
   Do[
    (* counterclockwise position around the loop, from the leftmost joined vertex *)
    cw[v_] := Mod[s (idx[v] - idx[sorted[[1, 1]]]), k];
    If[SortBy[sorted[[All, 1]], cw] === sorted[[All, 1]],
     uc = (Min[us] + Max[us])/2;
     (* points further apart than the loop is wide: vertical lines would leave its outer vertices tangentially; the
        joined vertices are spread over the lower 100 degrees of a loop a little narrower than the points, in proportion
        to the positions of their points, and the lines lean inwards *)
     radial = (Max[us] - Min[us])/2 > 0.87 unitRadius[k];
     r = If[radial, Min[1.3, Max[unitRadius[k], 0.85 (Max[us] - Min[us])/2]], unitRadius[k]];
     joined = If[radial,
       Table[{a[[1]], 1.5 Pi + 50. Degree (a[[2]] . t - uc)/((Max[us] - Min[us])/2)}, {a, sorted}],
       Table[{a[[1]], 2. Pi - ArcCos[Max[-1., Min[1., (a[[2]] . t - uc)/r]]]}, {a, sorted}]];
     (* the free vertices evenly between the joined ones around them; the last segment closes over the top *)
     ang = joined;
     Do[If[! MemberQ[joined[[All, 1]], v],
       With[{p = cw[v], js = Table[{cw[j[[1]]], j[[2]]}, {j, joined}]},
        With[{before = Last[Select[js, #[[1]] < p &]], after = Select[js, #[[1]] > p &]},
         With[{aft = If[after === {}, {k + js[[1, 1]], js[[1, 2]] + 2. Pi}, First[after]]},
          AppendTo[ang, {v, before[[2]] + (aft[[2]] - before[[2]]) (p - before[[1]])/(aft[[1]] - before[[1]])}]]]]], {v, unit}];
     hc = v0 + $linkLength + r Max[Table[-Sin[j[[2]]], {j, joined}]];
     centre = uc t + hc nn;
     pts = Table[{a[[1]], centre + r frame[a[[2]]]}, {a, ang}];
     (* something hanging from a vertex between two joined ones (below the circle) is bad *)
     score = Count[Table[MemberQ[hanging, a[[1]]] && cw[a[[1]]] < cw[Last[sorted][[1]]], {a, ang}], True] +
       0.01 Count[Table[cw[a[[1]]] < cw[Last[sorted][[1]]] && ! MemberQ[joined[[All, 1]], a[[1]]], {a, ang}], True];
     If[score < bestScore - 10.^-9, bestScore = score; best = {pts, centre, {unit, centre, r}}]],
    {s, {1, -1}}];
   best];

(* the region of a placed unit, {centre, radius}, to keep units apart *)
unitRegion[res_, dir_] := Which[
   res[[3]] =!= None, {res[[3, 2]], res[[3, 3]]},
   Length[res[[1]]] == 2, {Mean[res[[1]][[All, 2]]], 0.5 vectorNorm[res[[1, 2, 2]] - res[[1, 1, 2]]] + 0.05},
   True, {res[[1, 1, 2]] + 0.28 unitVectorOf[dir], 0.3}];

(* the bisector of the largest free sector around the placed node w (at pw) among its lines to placed nodes: the units
   hanging from a tadpole are spread around it (the first one to the left, the second one to the right, then below) *)
freeDirectionAt[w_, pw_, placed_List, ipairs_List] := With[{angles = Table[angleOf[Replace[u, Rule @@@ placed] - pw],
      {u, Select[neighboursOf[w, ipairs], # =!= w && MemberQ[placed[[All, 1]], #] &]}]},
   If[angles === {}, {-1., 0.}, directionAt[gapDirection[angles, {0., -1.}]]]];

(* Place the units of a block, one after the other, starting from the nodes already placed. placed: {{node, point}, ...};
   centres: {{node, centre}, ...} for the nodes of placed units (what hangs from them points away from the centre);
   railOut: {{node, direction}, ...} for the nodes of open lines (what hangs from them points that way); regions: the
   regions {centre, radius, nodes} of the units placed before: a new unit that overlaps a unit it is joined to moves
   further out.
   Result: {placed, centres, circles, regions}. *)
placeBlock[units0_List, ipairs_List, placed0_List, centres0_List, railOut_List, regions0_List : {}] := Module[{units = units0, placed = placed0,
    centres = centres0, circles = {}, regions = regions0, ready, unit, att, att0, dir, res, nodes, onLine, hanging, region, tries, railNodes},
   While[units =!= {},
    nodes = placed[[All, 1]];
    ready = Select[units, attachmentsOf[#, ipairs, nodes] =!= {} &];
    If[ready === {}, Break[]];
    (* units joined to an open line first, placed from their lines to it only (their other lines are drawn as they
       come); then the units with most lines to placed nodes *)
    railNodes = railOut[[All, 1]];
    unit = First[SortBy[ready, {-Length[Select[attachmentsOf[#, ipairs, nodes], MemberQ[railNodes, #[[2]]] &]],
         -Length[attachmentsOf[#, ipairs, nodes]]} &]];
    att0 = attachmentsOf[unit, ipairs, nodes];
    If[AnyTrue[att0, MemberQ[railNodes, #[[2]]] &], att0 = Select[att0, MemberQ[railNodes, #[[2]]] &]];
    att = Table[{a[[1]], Replace[a[[2]], Rule @@@ placed]}, {a, att0}];
    (* the direction: away from the centres of the units joined to, or as the open lines say; from a tadpole (its centre
       is its vertex), into the largest free sector around its vertex *)
    dir = Total[Table[With[{w = a[[2]]}, With[{c = Select[centres, #[[1]] === w &, 1], o = Select[railOut, #[[1]] === w &, 1],
          pw = Replace[w, Rule @@@ placed]},
         Which[c =!= {}, If[vectorNorm[pw - c[[1, 2]]] > 10.^-6, unitVectorOf[pw - c[[1, 2]]], freeDirectionAt[w, pw, placed, ipairs]],
          o =!= {}, o[[1, 2]], True, {0., 1.}]]],
       {a, att0}]];
    dir = If[vectorNorm[dir] < 10.^-6, {0., 1.}, unitVectorOf[dir]];
    hanging = Select[unit, AnyTrue[neighboursOf[#, ipairs], Function[w, ! MemberQ[nodes, w] && ! MemberQ[unit, w]]] &];
    (* a loop joined to several points of one line *)
    onLine = Length[unit] >= 3 && Length[att] >= 2 && AllTrue[att0, MemberQ[railNodes, #[[2]]] &] &&
      Length[DeleteDuplicates[Table[Select[railOut, #[[1]] === a[[2]] &, 1][[1, 2]], {a, att0}]]] == 1;
    res = If[onLine, placeLoopOnLine[unit, att, dir, hanging], None];
    If[res === None, res = placeUnit[unit, att, dir]];
    (* away from the units it hangs from (other units that it overlaps are moved apart by the caller); a unit joined
       to its parent by two or more lines keeps them visible *)
    region = unitRegion[res, dir]; tries = 0;
    With[{parents = Select[regions, Intersection[#[[3]], att0[[All, 2]]] =!= {} &], margin = If[Length[att0] >= 2, 0.45, 0.12]},
     While[tries < 10 && AnyTrue[parents, vectorNorm[#[[1]] - region[[1]]] < #[[2]] + region[[2]] + margin &],
      res = {Table[{q[[1]], q[[2]] + 0.2 dir}, {q, res[[1]]}], res[[2]] + 0.2 dir, If[res[[3]] === None, None, {res[[3, 1]], res[[3, 2]] + 0.2 dir, res[[3, 3]]}]};
      region = unitRegion[res, dir]; tries++]];
    AppendTo[regions, Append[region, unit]];
    placed = Join[placed, res[[1]]];
    centres = Join[centres, Table[{v, res[[2]]}, {v, unit}]];
    If[res[[3]] =!= None, AppendTo[circles, res[[3]]]];
    units = DeleteCases[units, unit]];
   {placed, centres, circles, regions}];

(* the extent of placed nodes and circles: {{xmin, xmax}, {ymin, ymax}}, with a margin for tadpoles and bubbles *)
unitExtent[points_List, circles_List] := With[{xs = Join[points[[All, 1]] - 0.3, points[[All, 1]] + 0.3, Table[c[[2, 1]] - c[[3]], {c, circles}],
     Table[c[[2, 1]] + c[[3]], {c, circles}]], ys = Join[points[[All, 2]] - 0.3, points[[All, 2]] + 0.3, Table[c[[2, 2]] - c[[3]], {c, circles}],
     Table[c[[2, 2]] + c[[3]], {c, circles}]]},
   {{Min[xs], Max[xs]}, {Min[ys], Max[ys]}}];

(* ---------------------------------------------------------------- two-point functions: the axis *)
(* Sides of the arcs (index spans on the axis) and of the blocks (their lists of attachment indices): {arcs, blocks},
   1 for below. On the same side, two crossing arcs conflict, and so do an arc and a block attached under it, or two
   blocks attached at interleaved vertices; an arc nested under a block, or a block attached at an end of an arc, is
   avoided if possible. Then arcs and blocks above, long arcs above. *)
axisSideChoice[spans_List, attached_List] := Module[{ns = Length[spans], nc = Length[attached], ranges, nestedQ, best},
   ranges = Table[{Min[a], Max[a]}, {a, attached}];
   nestedQ[s_, rg_] := rg[[2]] > rg[[1]] && rg[[1]] <= s[[1]] && s[[2]] <= rg[[2]];
   Which[ns + nc == 0, {{}, {}},
    ns + nc > 12, {Table[0, {ns}], Table[0, {nc}]},
    True,
    best = cheapestSides[ns + nc, Function[sides, 10 axisConflictCount[sides, spans, attached, ranges] +
       3 Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && nestedQ[spans[[i]], ranges[[j]]], {i, ns}, {j, nc}]], True] +
       0.3 Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && (MemberQ[attached[[j]], spans[[i, 1]]] || MemberQ[attached[[j]], spans[[i, 2]]]),
          {i, ns}, {j, nc}]], True] +
       Total[Take[sides, ns]] + 0.5 Total[Drop[sides, ns]] + 0.01 Total[Table[sides[[i]] (spans[[i, 2]] - spans[[i, 1]]), {i, ns}]]]];
    {Take[best, ns], Drop[best, ns]}]];

(* A two-point function whose external points are the ends of one open fermion line: the line on the horizontal axis,
   with the blocks of loops hanging above or below it. Result: {positions, mode, circles}. *)
layoutAxisUnits[axis_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List, coiled_List : {}] := Module[
   {blocks, ix, attached, spans, sides, gaps, u, positions, circles, placedAll, regions, overlaps, pendantExtent, ext, side, out, res,
    railOut, n = Length[axis], below},
   blocks = loopBlocks[loops, ipairs];
   ix[v_] := First[First[Position[axis, v, {1}]]];
   attached = Table[Union[Flatten[Table[ix /@ Select[neighboursOf[v, ipairs], MemberQ[axis, #] &], {v, Flatten[b]}]]], {b, blocks}];
   (* the interaction lines between vertices of the axis: arcs, above or below it (also between neighbours) *)
   spans = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[axis, a] && MemberQ[axis, b] :> Sort[{ix[a], ix[b]}]]];
   sides = axisSideChoice[spans, attached];
   (* the extent, along the axis, of a block hanging from a single vertex *)
   pendantExtent = Table[If[Length[attached[[j]]] == 1,
      With[{v = axis[[attached[[j, 1]]]]}, With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, {0., 1.}}}]},
        unitExtent[Rest[r[[1]]][[All, 2]], r[[3]]][[1]]]], None], {j, Length[blocks]}];
   gaps = Table[$axisSpacing, {Max[n - 1, 0]}];
   (* a bubble joined by coils to two neighbouring vertices: more room between them, so that the bubble (narrower than
      their distance) does not touch the coils; its lines are then bent into one arc with it (bubbleBends) *)
   Do[If[Length[blocks[[j]]] == 1 && Length[blocks[[j, 1]]] == 2 && Length[attached[[j]]] == 2 && attached[[j, 2]] == attached[[j, 1]] + 1 &&
      AllTrue[Select[ipairs, MemberQ[blocks[[j, 1]], #[[1]]] =!= MemberQ[blocks[[j, 1]], #[[2]]] &], MemberQ[coiled, Sort[#]] &],
     gaps[[attached[[j, 1]]]] = Max[gaps[[attached[[j, 1]]]], 1.25 $bubbleWidth + 0.5]], {j, Length[blocks]}];
   Do[If[pendantExtent[[j1]] =!= None && pendantExtent[[j2]] =!= None && sides[[2, j1]] == sides[[2, j2]] &&
      attached[[j2, 1]] == attached[[j1, 1]] + 1,
     gaps[[attached[[j1, 1]]]] = Max[gaps[[attached[[j1, 1]]]], pendantExtent[[j1, 2]] - pendantExtent[[j2, 1]] + 0.25]],
    {j1, Length[blocks]}, {j2, Length[blocks]}];
   Do[
    u = If[n == 0, {}, With[{c = FoldList[Plus, 0., gaps]}, c - Last[c]/2]];
    positions = Table[{axis[[i]], {u[[i]], 0.}}, {i, n}];
    placedAll = positions; circles = {}; regions = {};
    Do[side = If[sides[[2, j]] == 1, -1., 1.]; out = {0., side};
     railOut = Table[{v, out}, {v, axis}];
     res = placeBlock[blocks[[j]], ipairs, positions, {}, railOut];
     placedAll = Join[placedAll, Drop[res[[1]], n]]; circles = Join[circles, res[[3]]]; regions = Join[regions, res[[4]]], {j, Length[blocks]}];
    (* units that overlap: more room on the axis between them *)
    overlaps = 0;
    If[n > 1,
     Do[With[{r1 = regions[[i1]], r2 = regions[[i2]]}, With[{d = vectorNorm[r1[[1]] - r2[[1]]], need = r1[[2]] + r2[[2]] + 0.3},
        If[d < need && Abs[r1[[1, 1]] - r2[[1, 1]]] > 0.05,
         With[{g = Max[1, Min[n - 1, Count[u, x_ /; x < (r1[[1, 1]] + r2[[1, 1]])/2]]]}, overlaps++; gaps[[g]] = gaps[[g]] + Max[0.2, need - d]]]]],
      {i1, Length[regions]}, {i2, i1 + 1, Length[regions]}]];
    If[overlaps == 0, Break[]], {5}];
   (* the legs, along the axis *)
   positions = Join[placedAll, Join @@ Table[If[amputated[[k]],
       {{leg[externalLabels[[k]]], Replace[externalNodes[[k]], Rule @@@ placedAll] + If[k == 1, {-$legLength, 0.}, {$legLength, 0.}]}}, {}],
      {k, Length[externalLabels]}]];
   below = Table[{axis[[spans[[i, 1]]]], axis[[spans[[i, 2]]]]}, {i, Select[Range[Length[spans]], sides[[1, #]] == 1 &]}];
   {positions, {"backbone", axis, False, Sort /@ below, {0., -0.6}}, circles}];

(* ---------------------------------------------------------------- four-point functions: two rails *)
(* Positions along two rails A and B (lists of nodes) with minimal gaps gapsA, gapsB between neighbours, and syncs
   {{i, j}, ...} (increasing in both): A[[i]] and B[[j]] at the same place. The rails start together and end together
   where nothing else holds them (with centred = True and no syncs, the shorter rail keeps its length and is centred:
   crossing lines between the rails then make a symmetric cross); a rail of one node is centred. Result: {uA, uB}. *)
railCoordinates[nA_Integer, nB_Integer, gapsA_List, gapsB_List, syncs_List, centred_ : False] := Module[{uA = Table[0., {nA}], uB = Table[0., {nB}],
    span, fill, s, u0, prev},
   span[gaps_, i_, j_] := If[j <= i, 0., Total[Take[gaps, {i, j - 1}]]];
   (* put the nodes i..j of a rail between the coordinates a and b, spacing them in proportion to their gaps *)
   fill[coords_, gaps_, i_, j_, a_, b_] := Module[{c = coords, tot = span[gaps, i, j]},
     If[j == i, c[[i]] = a,
      Do[c[[k]] = a + (b - a) If[tot < 10.^-9, (k - i)/(j - i), span[gaps, i, k]/tot], {k, i, j}]];
     c];
   If[syncs === {},
    (* without syncs the rails have the same length, or (centred) each its own, the shorter one in the middle *)
    With[{LA = span[gapsA, 1, nA], LB = span[gapsB, 1, nB]}, With[{L = Max[LA, LB]},
      uA = Which[nA == 1, {L/2}, centred, fill[uA, gapsA, 1, nA, (L - LA)/2, (L + LA)/2], True, fill[uA, gapsA, 1, nA, 0., L]];
      uB = Which[nB == 1, {L/2}, centred, fill[uB, gapsB, 1, nB, (L - LB)/2, (L + LB)/2], True, fill[uB, gapsB, 1, nB, 0., L]]]],
    (* before the first sync *)
    s = First[syncs]; u0 = Max[span[gapsA, 1, s[[1]]], span[gapsB, 1, s[[2]]]];
    uA = fill[uA, gapsA, 1, s[[1]], If[s[[1]] == 1, u0, 0.], u0];
    uB = fill[uB, gapsB, 1, s[[2]], If[s[[2]] == 1, u0, 0.], u0];
    prev = s;
    Do[With[{L = Max[span[gapsA, prev[[1]], t[[1]]], span[gapsB, prev[[2]], t[[2]]]], a = uA[[prev[[1]]]]},
      uA = fill[uA, gapsA, prev[[1]], t[[1]], a, a + L];
      uB = fill[uB, gapsB, prev[[2]], t[[2]], a, a + L];
      prev = t], {t, Rest[syncs]}];
    (* after the last sync *)
    With[{L = Max[span[gapsA, prev[[1]], nA], span[gapsB, prev[[2]], nB]], a = uA[[prev[[1]]]]},
     uA = fill[uA, gapsA, prev[[1]], nA, a, If[nA == prev[[1]], a, a + L]];
     uB = fill[uB, gapsB, prev[[2]], nB, a, If[nB == prev[[2]], a, a + L]]]];
   {uA, uB}];

(* the gaps i..j-1 stretched so that they add up to at least L *)
stretchedGaps[gaps_List, i_Integer, j_Integer, L_] := If[j <= i, gaps,
   With[{t = Total[Take[gaps, {i, j - 1}]]}, If[t >= L, gaps, Join[Take[gaps, i - 1], Take[gaps, {i, j - 1}] L/t, Drop[gaps, j - 1]]]]];

(* Sides of the arcs of a rail: 1 inside (towards the other rail), 0 outside. outsideAttached / insideAttached: indices
   of the rail nodes with something hanging outside / joined to the other rail (rungOnly: those joined to it by
   interaction lines alone). Crossing arcs on the same side, an arc over a node with something on its side, conflict;
   an arc inside that only crosses rungs conflicts less (two crossing arcs on one side nearly coincide); arcs inside
   are avoided. *)
railArcSides[spans_List, outsideAttached_List, insideAttached_List, rungOnly_List : {}] := Module[{ns = Length[spans], overQ},
   overQ[i_, nodes_] := spanOverQ[spans[[i]], nodes];
   Which[ns == 0, {},
    ns > 10, Table[0, {ns}],
    True,
    cheapestSides[ns, Function[sides,
      10 (Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
          Count[Table[If[sides[[i]] == 1, overQ[i, Complement[insideAttached, rungOnly]], overQ[i, outsideAttached]], {i, ns}], True]) +
       5 Count[Table[sides[[i]] == 1 && ! overQ[i, Complement[insideAttached, rungOnly]] && overQ[i, rungOnly], {i, ns}], True] +
       2 Total[sides]]]]];

(* Place a unit joined to points on both sides of it (a loop between the two rails): the circle, centred at the
   preferred point, turned and oriented so that its joined vertices are near their points and the lines cross nothing *)
placeUnitBetween[unit_List, att_List, preferred_] := Module[{k = Length[unit], r, best = None, bestScore = Infinity, pts, segs, score},
   r = If[k >= 3, unitRadius[k], $bubbleWidth/2];
   Do[pts = Table[{unit[[j]], preferred + r directionAt[th + s 2. Pi (j - 1)/k]}, {j, k}];
    segs = Table[{Replace[a[[1]], Rule @@@ pts], a[[2]]}, {a, att}];
    score = 10 mutualCrossingCount[segs] + Total[Table[(sg[[1]] - sg[[2]]) . (sg[[1]] - sg[[2]]), {sg, segs}]];
    If[score < bestScore - 10.^-6, bestScore = score; best = pts],
    {s, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}];
   {best, preferred, If[k >= 3, {unit, preferred, r}, None]}];

(* A four-point function whose external points are the ends of two open fermion lines. The external labels are at the
   corners of a rectangle in the given order, counterclockwise from the top left; each line joins two neighbouring
   corners (if it would join opposite corners, the labels at the two right corners are exchanged), so that the lines
   are two parallel rails, horizontal or vertical. Lines between the rails are straight, perpendicular to them where
   possible; loops joined to both rails lie between them, everything else hangs outside. Result: {positions, mode,
   circles}, or None. *)
layoutRailUnits[paths_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List, order0_List] := Catch[Module[
   {order = order0, cornerOf, labelsOf, pc, horizontal, outerLink, A, B, orient, blocks, side, attA, attB, rungs, syncs, allSyncs, crossQ, spansA, spansB,
    arcSidesA, arcSidesB, ixA, ixB, outsideA, outsideB, insideA, insideB, pendantWidth, gapsA, gapsB, uA, uB, h, bridgeHeight,
    positions, circles = {}, res, railOut, toPlane, arcs, corner, chainQ, centres, legs, labelNode, nA, nB, centre, regions, overlaps},
   cornerOf[e_] := First[First[Position[order, e, {1}]]];
   labelNode[e_] := externalNodes[[First[First[Position[externalLabels, e, {1}]]]]];
   labelsOf[p_] := Select[externalLabels, MemberQ[{First[p], Last[p]}, labelNode[#]] &];
   If[Length[paths] =!= 2 || Sort[Join[labelsOf[paths[[1]]], labelsOf[paths[[2]]]]] =!= Sort[externalLabels] ||
     Length[labelsOf[paths[[1]]]] =!= 2, Throw[None, layoutRailUnits]];
   pc = Sort[cornerOf /@ labelsOf[#]] & /@ paths;
   (* lines joining opposite corners: exchange the labels at the right corners (3 and 4) *)
   If[MemberQ[pc, {1, 3}] || MemberQ[pc, {2, 4}],
    order = ReplacePart[order, {3 -> order[[4]], 4 -> order[[3]]}];
    pc = Sort[cornerOf /@ labelsOf[#]] & /@ paths];
   horizontal = MemberQ[pc, {1, 4}];
   (* the vertical extent of the picture is kept small: along vertical rails the nodes are closer, and between horizontal
      rails the lines to loops are shorter and the bubbles narrower (above and below them the lines keep their length,
      outerLink: a short wavy line to a tadpole is a kink) *)
   outerLink = $linkLength;
   Block[{$axisSpacing = If[horizontal, $axisSpacing, 0.75], $linkLength = If[horizontal, 0.5, $linkLength],
     $bubbleWidth = If[horizontal, 0.7, $bubbleWidth]},
   (* the rails in the frame (u, v): A at v > 0, B at v < 0, u along them. Horizontal: A on top (corners 1 -> 4), B
      below (2 -> 3); vertical: A on the left (2 -> 1), B on the right (3 -> 4), u upwards *)
   orient[p_, from_] := If[MemberQ[labelsOf[p], order[[from]]] && labelNode[order[[from]]] === Last[p] && Length[p] > 1, Reverse[p], p];
   If[horizontal,
    A = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {1, 4} &]], 1];
    B = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {2, 3} &]], 2];
    toPlane[{uu_, vv_}] := {uu, vv},
    A = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {1, 2} &]], 2];
    B = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {3, 4} &]], 3];
    toPlane[{uu_, vv_}] := {-vv, uu}];
   nA = Length[A]; nB = Length[B];
   ixA[v_] := First[First[Position[A, v, {1}]]]; ixB[v_] := First[First[Position[B, v, {1}]]];
   blocks = loopBlocks[loops, ipairs];
   attA = Table[Union[Flatten[Table[ixA /@ Select[neighboursOf[v, ipairs], MemberQ[A, #] &], {v, Flatten[b]}]]], {b, blocks}];
   attB = Table[Union[Flatten[Table[ixB /@ Select[neighboursOf[v, ipairs], MemberQ[B, #] &], {v, Flatten[b]}]]], {b, blocks}];
   side = Table[Which[attA[[j]] =!= {} && attB[[j]] =!= {}, "bridge", attA[[j]] =!= {}, "A", True, "B"], {j, Length[blocks]}];
   (* a chain: a bridge joined to each rail at one vertex, every unit of it joined to at most two others *)
   chainQ[j_] := side[[j]] === "bridge" && Length[attA[[j]]] == 1 && Length[attB[[j]]] == 1 &&
     Length[Cases[ipairs, {a_, b_} /; (MemberQ[Flatten[blocks[[j]]], a] && (MemberQ[A, b] || MemberQ[B, b])) ||
         (MemberQ[Flatten[blocks[[j]]], b] && (MemberQ[A, a] || MemberQ[B, a]))]] == 2;
   rungs = Cases[ipairs, {a_, b_} /; (MemberQ[A, a] && MemberQ[B, b]) || (MemberQ[A, b] && MemberQ[B, a]) :>
      If[MemberQ[A, a], {ixA[a], ixB[b]}, {ixA[b], ixB[a]}]];
   (* syncs: rungs and chains that cross no other *)
   crossQ[s_, t_] := (s[[1]] - t[[1]]) (s[[2]] - t[[2]]) < 0;
   allSyncs = Join[rungs, Table[{attA[[j, 1]], attB[[j, 1]]}, {j, Select[Range[Length[blocks]], chainQ]}]];
   syncs = Sort[Select[allSyncs, Function[s, ! AnyTrue[allSyncs, crossQ[s, #] &]]]];
   syncs = DeleteDuplicates[syncs];
   If[! (OrderedQ[syncs[[All, 1]]] && OrderedQ[syncs[[All, 2]]]) || hasDuplicatesQ[syncs[[All, 1]]] || hasDuplicatesQ[syncs[[All, 2]]],
    syncs = {}];
   (* the arcs of each rail, outside or inside *)
   spansA = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[A, a] && MemberQ[A, b] && Abs[ixA[a] - ixA[b]] > 1 :> Sort[{ixA[a], ixA[b]}]]];
   spansB = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[B, a] && MemberQ[B, b] && Abs[ixB[a] - ixB[b]] > 1 :> Sort[{ixB[a], ixB[b]}]]];
   outsideA = Union[Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "A" &]]]]];
   outsideB = Union[Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "B" &]]]]];
   insideA = Union[rungs[[All, 1]], Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]];
   insideB = Union[rungs[[All, 2]], Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]];
   arcSidesA = railArcSides[spansA, outsideA, insideA, Complement[insideA, Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]]];
   arcSidesB = railArcSides[spansB, outsideB, insideB, Complement[insideB, Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]]];
   (* the room needed by what hangs from neighbouring vertices *)
   pendantWidth[rail_, out_, j_] := With[{v = rail[[First[If[rail === A, attA[[j]], attB[[j]]]]]]},
     With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, out}}]}, unitExtent[Rest[r[[1]]][[All, 2]], r[[3]]][[1]]]];
   gapsA = Table[$axisSpacing, {Max[nA - 1, 0]}]; gapsB = Table[$axisSpacing, {Max[nB - 1, 0]}];
   Do[Do[With[{j1 = j[[1]], j2 = j[[2]]},
       If[side[[j1]] === sd && side[[j2]] === sd && Length[If[sd === "A", attA, attB][[j1]]] == 1 && Length[If[sd === "A", attA, attB][[j2]]] == 1 &&
         If[sd === "A", attA, attB][[j2, 1]] == If[sd === "A", attA, attB][[j1, 1]] + 1,
        With[{e1 = pendantWidth[If[sd === "A", A, B], {0., 1.}, j1], e2 = pendantWidth[If[sd === "A", A, B], {0., 1.}, j2], g = If[sd === "A", attA, attB][[j1, 1]]},
         If[sd === "A", gapsA[[g]] = Max[gapsA[[g]], e1[[2]] - e2[[1]] + 0.25], gapsB[[g]] = Max[gapsB[[g]], e1[[2]] - e2[[1]] + 0.25]]]]],
      {j, Tuples[Range[Length[blocks]], 2]}], {sd, {"A", "B"}}];
   (* neighbouring chains between the rails *)
   Do[If[chainQ[j1] && chainQ[j2] && attA[[j2, 1]] == attA[[j1, 1]] + 1,
     gapsA[[attA[[j1, 1]]]] = Max[gapsA[[attA[[j1, 1]]]], 1.6]];
    If[chainQ[j1] && chainQ[j2] && attB[[j2, 1]] == attB[[j1, 1]] + 1,
     gapsB[[attB[[j1, 1]]]] = Max[gapsB[[attB[[j1, 1]]]], 1.6]], {j1, Length[blocks]}, {j2, Length[blocks]}];
   (* two lines between the rails that cross: room for the cross *)
   Do[If[crossQ[s1, s2] && s1[[1]] < s2[[1]],
     gapsA = stretchedGaps[gapsA, s1[[1]], s2[[1]], 1.6]; gapsB = stretchedGaps[gapsB, s2[[2]], s1[[2]], 1.6]],
    {s1, allSyncs}, {s2, allSyncs}];
   (* the distance of the rails: room for the chains and the loops between them *)
   bridgeHeight[j_] := If[chainQ[j],
     With[{v = A[[attA[[j, 1]]]], w = B[[attB[[j, 1]]]]}, With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, {0., -1.}}}]},
       With[{end = Select[r[[1]], MemberQ[neighboursOf[w, ipairs], #[[1]]] &]}, -end[[1, 2, 2]] + $linkLength]]],
     2 $linkLength + 2 Max[0.5, Max[unitRadius[Length[#]] & /@ blocks[[j]]]] + 0.2];
   h = Max[Join[{$railSeparation}, Table[bridgeHeight[j], {j, Select[Range[Length[blocks]], side[[#]] === "bridge" &]}]]];
   (* inside arcs and crossing lines need room too *)
   If[MemberQ[arcSidesA, 1] || MemberQ[arcSidesB, 1], h = Max[h, 1.7]];
   If[Length[syncs] < Length[DeleteDuplicates[allSyncs]], h = Max[h, 1.5]];
   Do[
    {uA, uB} = railCoordinates[nA, nB, gapsA, gapsB, syncs, syncs === {} && allSyncs =!= {}];
    positions = Join[Table[{A[[i]], {uA[[i]], h/2}}, {i, nA}], Table[{B[[i]], {uB[[i]], -h/2}}, {i, nB}]];
    centres = {}; circles = {}; regions = {};
    (* the blocks outside the rails *)
    Do[If[side[[j]] =!= "bridge",
      railOut = Join[Table[{v, {0., 1.}}, {v, A}], Table[{v, {0., -1.}}, {v, B}]];
      res = Block[{$linkLength = outerLink}, placeBlock[blocks[[j]], ipairs, positions, centres, railOut]];
      positions = res[[1]]; centres = res[[2]]; circles = Join[circles, res[[3]]];
      regions = Join[regions, Table[Append[rg, side[[j]]], {rg, res[[4]]}]]], {j, Length[blocks]}];
    (* the chains between them, from A towards B *)
    Do[If[chainQ[j],
      railOut = Table[{v, {0., -1.}}, {v, A}];
      With[{others = Select[positions, ! MemberQ[B, #[[1]]] &]},
       res = placeBlock[blocks[[j]], ipairs, others, centres, railOut];
       positions = Join[res[[1]], Select[positions, MemberQ[B, #[[1]]] &]]; centres = res[[2]]; circles = Join[circles, res[[3]]]]],
     {j, Length[blocks]}];
    (* other bridges: the loop joined to both rails first, between them, then the rest *)
    Do[If[side[[j]] === "bridge" && ! chainQ[j],
      Module[{units = blocks[[j]], first, att},
       first = First[SortBy[units, -Length[attachmentsOf[#, ipairs, Join[A, B]]] &]];
       att = Table[{a[[1]], Replace[a[[2]], Rule @@@ positions]}, {a, attachmentsOf[first, ipairs, Join[A, B]]}];
       res = placeUnitBetween[first, att, {Mean[att[[All, 2]]][[1]], 0.}];
       positions = Join[positions, res[[1]]]; centres = Join[centres, Table[{v, res[[2]]}, {v, first}]];
       If[res[[3]] =!= None, AppendTo[circles, res[[3]]]];
       If[Length[units] > 1,
        res = placeBlock[DeleteCases[units, first], ipairs, positions, centres, Join[Table[{v, {0., -1.}}, {v, A}], Table[{v, {0., 1.}}, {v, B}]]];
        positions = res[[1]]; centres = res[[2]]; circles = Join[circles, res[[3]]]]]], {j, Length[blocks]}];
    (* units outside a rail that overlap: more room along the rail between them *)
    overlaps = 0;
    Do[With[{r1 = regions[[i1]], r2 = regions[[i2]]}, With[{d = vectorNorm[r1[[1]] - r2[[1]]], need = r1[[2]] + r2[[2]] + 0.3},
       If[r1[[4]] === r2[[4]] && d < need && Abs[r1[[1, 1]] - r2[[1, 1]]] > 0.05,
        With[{rail = If[r1[[4]] === "A", uA, uB], um = (r1[[1, 1]] + r2[[1, 1]])/2},
         With[{g = Max[1, Min[Length[rail] - 1, Count[rail, x_ /; x < um]]]},
          If[Length[rail] > 1, overlaps++;
           If[r1[[4]] === "A", gapsA[[g]] = gapsA[[g]] + Max[0.2, need - d], gapsB[[g]] = gapsB[[g]] + Max[0.2, need - d]]]]]]]],
     {i1, Length[regions]}, {i2, i1 + 1, Length[regions]}];
    If[overlaps == 0, Break[]], {4}];
   (* the arcs, with the sign of their bulge relative to the line from their first to their second node *)
   arcs = Join[
     Table[{{A[[spansA[[i, 1]]]], A[[spansA[[i, 2]]]]}, spansA[[i, 2]] - spansA[[i, 1]], If[arcSidesA[[i]] == 1, -1., 1.], "A"}, {i, Length[spansA]}],
     Table[{{B[[spansB[[i, 1]]]], B[[spansB[[i, 2]]]]}, spansB[[i, 2]] - spansB[[i, 1]], If[arcSidesB[[i]] == 1, 1., -1.], "B"}, {i, Length[spansB]}]];
   (* ranks: 1 for the shortest arc on its side of its rail, higher for longer ones *)
   arcs = Table[{a[[1]], Count[arcs, b_ /; b[[4]] === a[[4]] && b[[3]] == a[[3]] && b[[2]] <= a[[2]]], a[[3]]}, {a, arcs}];
   (* the legs, towards the corners of their labels *)
   corner = {{-1., 1.}, {-1., -1.}, {1., -1.}, {1., 1.}}/Sqrt[2.];
   positions = Table[{p[[1]], toPlane[p[[2]]]}, {p, positions}];
   centre = Mean[positions[[All, 2]]];
   positions = Table[{p[[1]], p[[2]] - centre}, {p, positions}];
   circles = Table[{c[[1]], toPlane[c[[2]]] - centre, c[[3]]}, {c, circles}];
   (* the signs of the arcs refer to the left normal of the line from the first node to the second, in the frame:
      the frames are related by a rotation, which keeps them *)
   (* the legs of a rail of one node (a bare vertex) open wider (110 degrees) and are longer (1.25 times, up to 2.2 times
      beside a longer rail, towards the corners beside the ends of the other rail), so that a bare vertex is a wide V, not
      a narrow one that makes the picture tall; they stay a clear V, never a straight line *)
   With[{railDir = toPlane[{1., 0.}]}, With[{uOther = Table[(Replace[#, Rule @@@ positions] . railDir) & /@ If[rail === A, B, A], {rail, {A, B}}]},
     legs = Join @@ Table[If[amputated[[k]],
         With[{node = externalNodes[[k]], d = corner[[cornerOf[externalLabels[[k]]]]]}, With[{p = Replace[node, Rule @@@ positions],
             single = Which[nA == 1 && node === A[[1]], 1, nB == 1 && node === B[[1]], 2, True, 0]},
           {{leg[externalLabels[[k]]], p + If[single == 0, $legLength d,
               With[{s = Sign[d . railDir], u = p . railDir, nrm = unitVectorOf[d - (d . railDir) railDir]},
                With[{need = Abs[If[s > 0, Max[uOther[[single]]], Min[uOther[[single]]]] - u] + $legLength/Sqrt[2.]},
                 Min[2.2 $legLength, Max[1.25 $legLength, need/Sin[55. Degree]]] (Cos[55. Degree] nrm + Sin[55. Degree] s railDir)]]]}}]], {}],
        {k, Length[externalLabels]}]]];
   {Join[positions, legs], {"circle", arcs, {0., 0.}}, circles}]], layoutRailUnits];


(* Three or more open fermion lines, each with its two ends at external points, joined in a chain by interaction lines
   (the diagrams of Z[eta, etabar] with six or more sources): horizontal rails one above the other, in the order of the
   chain, the lines between neighbouring rails vertical where possible; interaction lines along the first or the last rail
   are arcs outside. The longest rail is placed first and the others are fitted to it, each in the direction that keeps the
   lines between them from crossing. Result: {positions, mode, circles}, or None (loops, lines between rails that are not
   neighbours in the chain, crossing lines). *)
layoutStackedRails[paths_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List] := Catch[Module[
   {k = Length[paths], railOf, edges, deg, order, t, prev, base, ucoord = {}, hasU, uOf, placed, fit, arcs = {}, positions, centre, legs, nbrs, minGap = 0.6},
   If[k < 3 || loops =!= {} || AnyTrue[paths, Length[#] < 2 || ! MemberQ[externalNodes, First[#]] || ! MemberQ[externalNodes, Last[#]] &],
    Throw[None, layoutStackedRails]];
   railOf[v_] := First[First[Position[paths, v, {2}]]];
   hasU[v_] := MemberQ[Cases[ucoord, {w_, _} :> w], v]; uOf[v_] := First[Cases[ucoord, {v, x_} :> x]];
   edges = DeleteDuplicates[Sort /@ Select[Table[{railOf[p[[1]]], railOf[p[[2]]]}, {p, ipairs}], #[[1]] =!= #[[2]] &]];
   deg[i_] := Count[Flatten[edges], i];
   If[Length[edges] =!= k - 1 || AnyTrue[Range[k], deg[#] > 2 || deg[#] == 0 &], Throw[None, layoutStackedRails]];
   (* the rails in the order of the chain *)
   order = {First[Select[Range[k], deg[#] == 1 &]]};
   While[Length[order] < k,
    nbrs = Complement[Flatten[Select[edges, MemberQ[#, Last[order]] &]], order];
    If[nbrs === {}, Throw[None, layoutStackedRails]]; AppendTo[order, First[nbrs]]];
   (* interaction lines along a rail only on the first and the last one *)
   If[AnyTrue[ipairs, railOf[#[[1]]] === railOf[#[[2]]] && ! MemberQ[{First[order], Last[order]}, railOf[#[[1]]]] &], Throw[None, layoutStackedRails]];
   base = First[SortBy[order, -Length[paths[[#]]] &]];
   ucoord = Table[{paths[[base, i]], (i - 1.) $axisSpacing}, {i, Length[paths[[base]]]}];
   placed = {base};
   (* the coordinates of the nodes of rail r along it, from the nodes of the placed rail joined to it *)
   fit[r_] := Module[{best = None, anchors, us, rail, c},
     Do[rail = If[dir == 1, paths[[r]], Reverse[paths[[r]]]];
      anchors = Select[Range[Length[rail]], AnyTrue[neighboursOf[rail[[#]], ipairs], hasU] &];
      us = Table[Mean[uOf /@ Select[neighboursOf[rail[[i]], ipairs], hasU]], {i, anchors}];
      If[best === None && anchors =!= {} && AllTrue[Range[Length[anchors] - 1],
          us[[# + 1]] - us[[#]] >= minGap (anchors[[# + 1]] - anchors[[#]]) - 10.^-9 &],
       c = Table[0., {Length[rail]}];
       Do[c[[anchors[[j]]]] = us[[j]], {j, Length[anchors]}];
       Do[Do[c[[i]] = us[[j]] + (us[[j + 1]] - us[[j]]) (i - anchors[[j]])/(anchors[[j + 1]] - anchors[[j]]), {i, anchors[[j]] + 1, anchors[[j + 1]] - 1}],
        {j, Length[anchors] - 1}];
       Do[c[[i]] = us[[1]] - (anchors[[1]] - i) $axisSpacing, {i, 1, anchors[[1]] - 1}];
       Do[c[[i]] = Last[us] + (i - Last[anchors]) $axisSpacing, {i, Last[anchors] + 1, Length[rail]}];
       best = Table[{rail[[i]], c[[i]]}, {i, Length[rail]}]], {dir, {1, -1}}];
     If[best === None, Throw[None, layoutStackedRails]];
     ucoord = Join[ucoord, best]];
   While[Length[placed] < k,
    t = First[Select[order, ! MemberQ[placed, #] && AnyTrue[edges, Function[e, MemberQ[e, #] && Intersection[e, placed] =!= {}]] &]];
    fit[t]; AppendTo[placed, t]];
   (* the rails aligned: their free ends (the nodes before the first and after the last node joined to another rail) are
      spread out to the common ends, so that the rails are equally long, one above the other *)
   With[{uMin = Min[ucoord[[All, 2]]], uMax = Max[ucoord[[All, 2]]]},
    Do[With[{rail = SortBy[paths[[r]], uOf]}, With[{joined = Select[Range[Length[rail]],
          AnyTrue[neighboursOf[rail[[#]], ipairs], Function[w, railOf[w] =!= r]] &]},
       If[joined =!= {},
        With[{i1 = First[joined], i2 = Last[joined], u1 = uOf[rail[[First[joined]]]], u2 = uOf[rail[[Last[joined]]]]},
         ucoord = Join[Select[ucoord, ! MemberQ[rail, #[[1]]] &],
           Table[{rail[[i]], Which[i < i1, uMin + (u1 - uMin) (i - 1)/(i1 - 1), i > i2, u2 + (uMax - u2) (i - i2)/(Length[rail] - i2), True, uOf[rail[[i]]]]},
            {i, Length[rail]}]]]]]], {r, Range[k]}]];
   positions = Flatten[Table[Table[{v, {uOf[v], -(First[First[Position[order, r]]] - 1.) $railSeparation}}, {v, paths[[r]]}], {r, order}], 1];
   (* the arcs along the first rail above it, along the last one below it, longer ones further out *)
   Do[If[railOf[p[[1]]] === railOf[p[[2]]] && Abs[Subtract @@ (Position[paths[[railOf[p[[1]]]]], #][[1, 1]] & /@ p)] > 1,
     With[{ends = SortBy[p, uOf]}, AppendTo[arcs, {ends, Abs[uOf[ends[[2]]] - uOf[ends[[1]]]], If[railOf[p[[1]]] === First[order], 1., -1.]}]]],
    {p, DeleteDuplicates[Sort /@ ipairs]}];
   arcs = Table[{a[[1]], Count[arcs, b_ /; b[[3]] == a[[3]] && b[[2]] <= a[[2]] + 10.^-9], a[[3]]}, {a, arcs}];
   centre = Mean[positions[[All, 2]]];
   positions = Table[{p[[1]], p[[2]] - centre}, {p, positions}];
   legs = Join @@ Table[If[amputated[[j]],
       With[{v = externalNodes[[j]]}, {{leg[externalLabels[[j]]], Replace[v, Rule @@@ positions] +
           {If[uOf[v] <= Min[uOf /@ paths[[railOf[v]]]] + 10.^-9, -$legLength, $legLength], 0.}}}], {}], {j, Length[externalLabels]}];
   {Join[positions, legs], {"circle", arcs, {0., 0.}}, {}}], layoutStackedRails];

(* ---------------------------------------------------------------- vacuum diagrams: trees of loops *)
(* A vacuum part made of three or more loops joined as a tree (two loops may be joined by several lines): the most
   central loop in the middle, turned so that as many lines as possible leave it horizontally or vertically, and the
   others around it, each pointing away from the loop it hangs from. Result: {positions, mode, circles}, or None. *)
layoutVacuumTree[loops_List, ipairs_List] := Module[{k = Length[loops], unitOf, superEdges, ecc, root, rootLoop, kr, r, best = None,
    bestScore = Infinity, linkVertices, pts, score, res, circles, outerOf, linkCount, kinds, symmetricQ, multiple},
   If[k < 3, Return[None]];
   unitOf[v_] := First[First[Position[loops, v, {2}]]];
   superEdges = DeleteDuplicates[Table[Sort[{unitOf[p[[1]]], unitOf[p[[2]]]}], {p, Select[ipairs, unitOf[#[[1]]] =!= unitOf[#[[2]]] &]}]];
   If[Length[superEdges] =!= k - 1 || Length[componentOf[1, superEdges]] =!= k, Return[None]];
   ecc = Table[Max[graphDistances[{i}, superEdges][[All, 2]]], {i, k}];
   root = First[SortBy[Range[k], {ecc[[#]], -Length[loops[[#]]], #} &]];
   rootLoop = loops[[root]]; kr = Length[rootLoop];
   linkVertices = Select[rootLoop, AnyTrue[neighboursOf[#, ipairs], Function[w, ! MemberQ[rootLoop, w]]] &];
   (* what a line leaving the root loop leads to: the number of lines to that loop and the size of its branch *)
   outerOf[v_] := unitOf /@ Select[neighboursOf[v, ipairs], ! MemberQ[rootLoop, #] &];
   linkCount[w_] := Count[linkVertices, v_ /; MemberQ[outerOf[v], w]];
   kinds = Table[Sort[Table[{linkCount[w], Total[Length[loops[[#]]] & /@ componentOf[w, Select[superEdges, ! MemberQ[#, root] &]]]},
       {w, outerOf[v]}]], {v, linkVertices}];
   (* the loops joined to the root loop by several lines *)
   multiple = Select[DeleteDuplicates[Flatten[outerOf /@ linkVertices]], linkCount[#] >= 2 &];
   symmetricQ[as_, f_] := AllTrue[Range[Length[as]], Function[i, AnyTrue[Range[Length[as]],
        kinds[[#]] === kinds[[i]] && Abs[angleDifference[f[as[[i]]], as[[#]]]] < 0.02 &]]];
   Which[
    kr == 1, pts = {{rootLoop[[1]], {0., 0.}}}; circles = {},
    kr == 2, pts = {{rootLoop[[1]], {-$bubbleWidth/2, 0.}}, {rootLoop[[2]], {$bubbleWidth/2, 0.}}}; circles = {},
    True,
    r = unitRadius[kr];
    (* the loop turned so that the lines leaving it are symmetric about the vertical or the horizontal axis, each mapped
       to a line that leads to an equal branch (hanging down rather than up; a loop joined by several lines beside it,
       like the other pairs of loops), then so that as many as possible leave it horizontally or vertically *)
    Do[With[{p = Table[{rootLoop[[j]], r directionAt[th + 2. Pi (j - 1)/kr]}, {j, kr}]},
       With[{as = Table[angleOf[Replace[v, Rule @@@ p]], {v, linkVertices}]},
         score = -10 Boole[symmetricQ[as, Pi - # &] || symmetricQ[as, -# &]] - Count[Table[Abs[Sin[2 a]] < 0.02, {a, as}], True] +
           0.5 Total[Table[Abs[Sin[angleOf[Total[Table[If[MemberQ[outerOf[linkVertices[[i]]], w], directionAt[as[[i]]], {0., 0.}],
                {i, Length[linkVertices]}]]]]], {w, multiple}]] + 0.01 Total[Sin[as]] + 0.001 th];
       If[score < bestScore - 10.^-9, bestScore = score; best = p]], {th, 0., 2. Pi - 0.01, Pi/12.}];
    pts = best; circles = {{rootLoop, {0., 0.}, r}}];
   res = placeBlock[Delete[loops, root], ipairs, pts, Table[{v, {0., 0.}}, {v, rootLoop}], {},
     {{{0., 0.}, Which[kr >= 3, r, kr == 2, 0.55, True, 0.3], rootLoop}}];
   {res[[1]], {"loops", Join[circles, res[[3]]], {0., 0.}}, Join[circles, res[[3]]]}];

(* ---------------------------------------------------------------- choice of the layout *)
(* the unit layout of the frame with the external labels, or None (then the general layout is used) *)
unitLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, externalOrder_] := Module[
   {frameLines = Select[lines, MemberQ[mainNodes, #[[1]]] &], fl, paths, loops, ipairs, axis},
   fl = fermionLines[mainNodes, frameLines, labelNodes];
   If[fl === None, Return[None]];
   {paths, loops} = fl;
   ipairs = interactionPairsOf[mainNodes, frameLines];
   (* propagators alone (e.g. the terms of Wick's theorem) keep their places *)
   If[ipairs === {}, Return[None]];
   Which[
    Length[labels] == 2 && Length[paths] == 1 && Sort[DeleteDuplicates[labelNodes]] === Sort[DeleteDuplicates[{First[paths[[1]]], Last[paths[[1]]]}]],
    axis = If[First[paths[[1]]] === labelNodes[[1]], paths[[1]], Reverse[paths[[1]]]];
    layoutAxisUnits[axis, loops, ipairs, labels, labelNodes, amputated, Cases[frameLines, {u_, w_, _, "Curly"} :> Sort[{u, w}]]],
    Length[labels] == 4 && Length[paths] == 2,
    layoutRailUnits[paths, loops, ipairs, labels, labelNodes, amputated,
     If[ListQ[externalOrder], Join[Select[externalOrder, MemberQ[labels, #] &], Complement[labels, externalOrder]], labels]],
    Length[paths] >= 3 && Length[labels] == 2 Length[paths],
    layoutStackedRails[paths, loops, ipairs, labels, labelNodes, amputated],
    True, None]];

(* ---------------------------------------------------------------- trees of vertex functions *)
(* A connected function written as a tree of vertex functions (blobs) joined by propagators, with the external points at
   its leaves (the tree expansion of ConnectedFunctionFromVertices). The central blob, or the two central blobs, in the
   middle; a chain of blobs is straight, the legs of a blob at the end of a chain are fanned outwards and those of a blob
   along it are perpendicular to it; a blob with three or more blob neighbours has them all around. The external points
   follow each other counterclockwise, in the order of their labels. {positions, mode, circles}, or None if the part is not such a tree.
   The same tree with free ends at its leaves (the labels integrated over: dummy points ending one propagator and nothing
   else; freeEnds -> True) is drawn alike, the free ends where the external points would be; its inner nodes may also be
   bare vertices (points of three or more propagators). Its leaves have no order: the children of a node are arranged
   by the shapes of their subtrees, as a palindrome when they can be (the smaller subtree first, a lone one in the
   middle), the legs along a chain on its upper side, so that a symmetric tree is drawn symmetric about the vertical axis. *)
$treeSpine = 1.; $treeLeg = 0.72;
treeFanOffsets[k_Integer] := Which[k == 1, {0.}, k == 2, {-45., 45.} Degree, k == 3, {-60., 0., 60.} Degree,
   k == 4, {-72., -24., 24., 72.} Degree, True, Table[(j - (k - 1)/2.) 150. Degree/(k - 1), {j, 0, k - 1}]];
(* the members of xs, sorted by key, as a palindrome of keys when they can be: the pairs of equal keys at both ends,
   the lone ones in the middle. Around a star, whose mirror axis goes through its first member and, for an even number,
   the one opposite: "Top" puts a lone one first, "TopBottom" the two lone ones first and opposite *)
loneCount[xs_List, key_] := Count[Length /@ GatherBy[xs, key], _?OddQ];
palindromicArrangement[xs_List, key_, mode_ : None] := Module[{groups = GatherBy[SortBy[xs, key], key], outer = {}, inner = {},
    middle = {}},
   Do[With[{h = Floor[Length[g]/2]}, outer = Join[outer, Take[g, h]]; inner = Join[Take[g, {h + 1, 2 h}], inner];
     If[OddQ[Length[g]], AppendTo[middle, Last[g]]]], {g, groups}];
   Which[
    mode === "Top" && middle =!= {}, Join[{First[middle]}, outer, Rest[middle], inner],
    mode === "TopBottom" && Length[middle] == 2, Join[{First[middle]}, outer, {Last[middle]}, inner],
    True, Join[outer, middle, inner]]];
(* the free ends of a part without external labels: the points of degree one that are not blobs and carry no source.
   {positions, mode, circles} of the tree layout, or None *)
freeEndTreeLayout[component_List, lines_List, blobs_List, stubs_List, pairs_List] := With[
   {leaves = Select[component, Head[#] =!= blob && Count[Flatten[Select[pairs, MemberQ[component, #[[1]]] &]], #] == 1 &]},
   If[Length[leaves] < 2 || Intersection[component, Cases[stubs, {u_, _} :> u]] =!= {}, None,
    treeLayoutWithExternals[leaves, leaves, Table[False, {Length[leaves]}], component, lines, blobs, True]]];
(* With four external points and their order (the corners counterclockwise from the top left), a tree of two vertex functions
   joined by a line whose pairs of labels are the first two and the last two is drawn as it is, one whose pairs are the
   first and the last two and the middle two upright (laid out turned, with the order turned, and turned back); in the
   channel that joins opposite corners it is drawn upright with the labels on the right exchanged, and the caller crosses
   their legs (crossedLegPositions) *)
treeLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, blobs_List, freeEnds_ : False,
   order_ : Automatic] := Module[{res, turned, found = None},
   If[Length[labels] =!= 4 || ! ListQ[order] || freeEnds,
    treeLayoutOfOrder[labels, labelNodes, amputated, mainNodes, lines, blobs, freeEnds, Automatic],
    res = treeLayoutOfOrder[labels, labelNodes, amputated, mainNodes, lines, blobs, False, order];
    If[res === None, None,
     Do[res = treeLayoutOfOrder[labels, labelNodes, amputated, mainNodes, lines, blobs, False, c];
      If[cornerArrangement[res[[1]], labels, labelNodes, amputated] === c, found = res; Break[]];
      (* turned clockwise: the corners top left, bottom left, bottom right, top right go to top right, top left, bottom
         left, bottom right *)
      turned = treeLayoutOfOrder[labels, labelNodes, amputated, mainNodes, lines, blobs, False, c[[{4, 1, 2, 3}]]];
      turned = {Table[{q[[1]], {q[[2, 2]], -q[[2, 1]]}}, {q, turned[[1]]}], turned[[2]], turned[[3]]};
      If[cornerArrangement[turned[[1]], labels, labelNodes, amputated] === c, found = turned; Break[]],
      {c, {order, order[[{1, 2, 4, 3}]]}}];
     If[found === None, treeLayoutOfOrder[labels, labelNodes, amputated, mainNodes, lines, blobs, False, order], found]]]];
treeLayoutOfOrder[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, blobs_List, freeEnds_, order_] := Module[
   {treeLines, blobNodes, nbrs, blobNbrs, distances, ecc, centres, pos = {}, place, labelRank, subtreeRank, sortChildren,
    perpendicular, innerQ, shapeKey, upperFirst},
   If[Or @@ amputated || Length[labels] < 2 || Length[DeleteDuplicates[labelNodes]] < Length[labelNodes], Return[None]];
   treeLines = Select[lines, MemberQ[mainNodes, #[[1]]] &];
   nbrs[v_] := Join[Cases[treeLines, {v, u_, _, _} :> u], Cases[treeLines, {u_, v, _, _} :> u]];
   (* the inner nodes: the blobs, and with free ends also the bare vertices (three or more propagators, no source) *)
   blobNodes = Select[mainNodes, Head[#] === blob || (freeEnds && ! MemberQ[labelNodes, #] && Length[nbrs[#]] >= 3) &];
   If[Select[blobNodes, Head[#] === blob &] === {} && ! freeEnds || blobNodes === {} ||
     Complement[mainNodes, Join[labelNodes, blobNodes]] =!= {} ||
     AnyTrue[treeLines, #[[1]] === #[[2]] || interactionStyleQ[#[[4]]] &] || Length[treeLines] =!= Length[mainNodes] - 1 ||
     AnyTrue[Select[blobs, MemberQ[blobNodes, #[[1]]] &], #[[2]] =!= {} &], Return[None]];
   If[AnyTrue[labelNodes, Length[nbrs[#]] =!= 1 &], Return[None]];
   innerQ[v_] := MemberQ[blobNodes, v];
   blobNbrs[v_] := Select[nbrs[v], innerQ];
   (* the distances from the blob s to the other blobs, along the blobs *)
   distances[s_] := Module[{seen = {s}, frontier = {s}, k = 0, next, out = {0}},
     While[frontier =!= {}, k++;
      next = DeleteDuplicates[Select[Flatten[blobNbrs /@ frontier], ! MemberQ[seen, #] &]];
      seen = Join[seen, next]; out = Join[out, Table[k, {Length[next]}]]; frontier = next];
     out];
   ecc = Table[Max[distances[b]], {b, blobNodes}];
   centres = Pick[blobNodes, Thread[ecc == Min[ecc]]];
   (* the children of a node in the order of the first label of their subtrees; with free ends, by the shapes of their
      subtrees (number of leaves, kind of node, shapes of the children), as a palindrome *)
   labelRank[v_] := If[ListQ[order], First[First[Position[order, labels[[First[First[Position[labelNodes, v, {1}]]]]], {1}]]],
     First[First[Position[labelNodes, v, {1}]]]];
   subtreeRank[v_, parent_] := If[! innerQ[v], labelRank[v],
     Min[Join[{Infinity}, Table[subtreeRank[u, v], {u, DeleteCases[nbrs[v], parent]}]]]];
   shapeKey[v_, parent_] := If[! innerQ[v], {1, "", {}},
     With[{ks = Sort[Table[shapeKey[u, v], {u, DeleteCases[nbrs[v], parent]}]]},
      {Total[ks[[All, 1]]], If[Head[v] === blob, ToString[blobHeadOf[v, blobs]], "*"], ks}]];
   sortChildren[cs_, v_, mode_ : None] := If[freeEnds, palindromicArrangement[cs, shapeKey[#, v] &, mode], SortBy[cs, subtreeRank[#, v] &]];
   (* directions of m legs on the side a, fanned *)
   perpendicular[a_, m_] := If[m == 0, {}, a + If[m == 1, {0.}, Table[(j - (m - 1)/2.) 40. Degree, {j, 0, m - 1}]]];
   (* the side of a chain in the direction phi that takes the first legs: on the right, counterclockwise after the
      next blob; with free ends the upper side, so that the two halves of a chain are mirror images *)
   upperFirst[phi_] := freeEnds && Cos[phi] > 10.^-9;
   (* place v at p, reached from its parent in the direction phi, and its subtree *)
   place[v_, p_, phi_, parent_] := Module[{children = sortChildren[DeleteCases[nbrs[v], parent], v], bs, ls, angles, k, up, sides},
     AppendTo[pos, {v, p}];
     If[innerQ[v],
      bs = Select[children, innerQ]; ls = Select[children, ! innerQ[#] &];
      k = Length[children];
      Which[
       k == 0, angles = {},
       (* the end of a chain: the legs fanned outwards, counterclockwise *)
       bs === {}, angles = phi + treeFanOffsets[k],
       (* along a chain: the next blob straight on, the legs perpendicular, the first ones on the right *)
       Length[bs] == 1, up = Ceiling[Length[ls]/2]; sides = If[upperFirst[phi], {phi + Pi/2, phi - Pi/2}, {phi - Pi/2, phi + Pi/2}];
       children = Join[bs, ls]; angles = Join[{phi}, perpendicular[sides[[1]], up], perpendicular[sides[[2]], Length[ls] - up]],
       (* a branching: all the children fanned outwards *)
       True, angles = phi + Table[(j - (k - 1)/2.) Min[60. Degree, 150. Degree/(k - 1)], {j, 0, k - 1}]];
      Do[With[{c = children[[j]], a = angles[[j]]},
        place[c, p + If[innerQ[c], $treeSpine, $treeLeg] directionAt[a], a, v]], {j, k}]]];
   If[Length[centres] == 2,
    (* two central blobs side by side, the one with the first label on the left (with free ends, the smaller subtree) *)
    With[{cs = If[freeEnds, SortBy[centres, shapeKey[#, First[DeleteCases[centres, #]]] &],
        SortBy[centres, subtreeRank[#, First[DeleteCases[centres, #]]] &]]},
     place[cs[[1]], {-$treeSpine/2, 0.}, Pi, cs[[2]]];
     place[cs[[2]], {$treeSpine/2, 0.}, 0., cs[[1]]]],
    With[{c = First[centres]}, Module[{children, bs, ls, k, angles, up, top},
      (* with free ends, a star of an even number with two lone subtrees has them at the top and at the bottom *)
      top = freeEnds && Length[blobNbrs[c]] =!= 2 && EvenQ[Length[nbrs[c]]] && loneCount[nbrs[c], shapeKey[#, c] &] == 2;
      children = sortChildren[nbrs[c], c, Which[Length[blobNbrs[c]] === 2, None, OddQ[Length[nbrs[c]]], "Top", top, "TopBottom", True, None]];
      bs = Select[children, innerQ]; ls = Select[children, ! innerQ[#] &]; k = Length[children];
      AppendTo[pos, {c, {0., 0.}}];
      If[Length[bs] == 2,
       (* the middle of a chain: the two blobs left and right, the legs above and below *)
       up = Ceiling[Length[ls]/2]; children = Join[bs, ls];
       angles = Join[{Pi, 0.}, perpendicular[Pi/2, up], perpendicular[-Pi/2, Length[ls] - up]],
       (* a star: all around, counterclockwise from the top (from the top left for an even number) *)
       angles = Table[Pi/2 + If[OddQ[k] || top, 0., Pi/k] + 2. Pi (j - 1)/k, {j, k}]];
      Do[With[{ch = children[[j]], a = angles[[j]]},
        place[ch, If[innerQ[ch], $treeSpine, $treeLeg] directionAt[a], a, c]], {j, k}]]]];
   {pos, {"circle", {}, {0., 0.}}, {}}];

(* ---------------------------------------------------------------- declared blobs *)
(* A diagram with blobs declared by DeclareBlob: each blob is a circle or a square of half size $declaredBlobSize (larger
   when its label needs it), or a triangle inscribed in a circle 1.45 times larger, its legs (ports) on its boundary, counterclockwise from the top
   left (the corners of a square for four legs, of the triangle for three). Blobs joined
   by lines are placed side by side, on the side of the ports that join them (Bethe-Salpeter ladders); the other points
   outside the ports they are joined to; the legs of external labels at ports point outwards, or along the side of the
   blob when they would meet a neighbouring blob. {positions, {"blob", blobs, bends, centre}, {}}, or None: blobs {centre,
   shape, size} (a turned box a "Diamond"), bends {point, port, centre, radius}, the lines bent along circles. *)
$declaredBlobSize = 0.6; $hatchSpacing = 0.1; $blobGap = 0.95; $blobPointDistance = 0.9; $circlePortAngle = 55. Degree;
blobHeadOf[v_, blobs_] := First[Cases[blobs, {v, _, h_} :> h], None];
(* the spec {label, shape, fill, size} of a declared blob: its size is $declaredBlobSize, or larger when its label needs it
   (declaredBlobSize) *)
blobSpecOf[h_] := First[Cases[$blobDeclarations, {h, l_, s_, f_, ___} :> {l, s, f, pictureBlobSize[l, s, f]}], None];
(* a factor of three or more labels that is not declared is a disc of radius $blobRadius without a name, filled by the rule
   of the declared blobs: a vertex function VertexFunction (Gamma^(n) of the tree expansion, a full 1PI vertex)
   cross-hatched, any other factor hatched, the default of DeclareBlob. Its node is {"N", p, kind} *)
unnamedBlobKind[h_] := If[h === VertexFunction, "vertexFunction", "blob"];
unnamedBlobKindQ[k_] := k === "blob" || k === "vertexFunction";
unnamedBlobSpec[k_] := {"", "Circle", If[k === "vertexFunction", "CrossHatched", "Hatched"], $blobRadius};
(* within one picture the blobs of one shape have one size, the largest their labels need: F beside Gamma_ph is as large
   as it, so that the lines between them are parallel; so do those of all the terms of a sum drawn together, so that a
   blob looks the same in every term (Gamma0 alone and beside Gamma4 in Gamma4 = Gamma0 - Gamma0 L0 Gamma4/2).
   $pictureBlobHeads: the heads of the blobs of the picture drawn, $sumBlobHeads those of the whole sum (geometriesOf) *)
$pictureBlobHeads = {}; $sumBlobHeads = {};
pictureBlobSize[l_, s_, f_] := Max[declaredBlobSize[l, s, f],
   Cases[$blobDeclarations, {hh_, l2_, s, f2_, ___} /; MemberQ[$pictureBlobHeads, hh] :> declaredBlobSize[l2, s, f2]],
   If[s === "Box", $pictureBoxFloor, 0.]];
(* the lines of a triangle leave from its corners: a box joined by two lines to two corners of a triangle (Gamma0 L0
   Lambda) is at least as wide as the side between them, so that the lines are parallel. $pictureBoxFloor: the least
   size of a box in the picture drawn, $sumBoxFloor in the whole sum (geometriesOf), as for $pictureBlobHeads *)
$pictureBoxFloor = 0.; $sumBoxFloor = 0.; $boxTriangleSpacing = 1.;
boxFloorOf[elements_List] := Module[{blobs = elements[[3]], lines = elements[[2]], shapeOf, joins},
   shapeOf[h_] := With[{sp = blobSpecOf[h]}, If[ListQ[sp], sp[[2]], None]];
   (* the legs of the triangle t joined by a line to legs of the box b *)
   joins[b_, t_] := DeleteDuplicates[Join[Cases[lines, {p_, q_, __} /; MemberQ[b[[2]], p] && MemberQ[t[[2]], q] :> q],
      Cases[lines, {q_, p_, __} /; MemberQ[b[[2]], p] && MemberQ[t[[2]], q] :> q]]];
   Max[0., Flatten[Table[If[shapeOf[b[[3]]] === "Box" && MemberQ[{"Triangle", "TriangleUp"}, shapeOf[t[[3]]]] &&
        Length[joins[b, t]] == 2,
       $boxTriangleSpacing Sqrt[3.] 1.45 blobSizeOfHead[t[[3]]]/2, 0.], {b, blobs}, {t, blobs}]]]];
blobAxialPorts[h_] := First[Cases[$blobDeclarations, {h, _, _, _, a_List} :> a], {}];
(* the corners of a triangular blob of size h centred at p: top left, bottom left, right *)
(* "Triangle": the first two corners on the left (top, bottom), the third on the right; "TriangleUp": the first two at the
   bottom (left, right), the third at the top *)
triangleShapeQ[shape_] := MemberQ[{"Triangle", "TriangleUp", "Diamond"}, shape];
(* "Diamond": a square turned by 45 degrees, its corners left, bottom, right, top. Both triangles are equilateral (the
   same triangle turned), so that a vertex looks the same in either orientation and its label has room *)
triangleCorners[p_, h_, shape_ : "Triangle"] := Table[p + If[shape === "Diamond", 1.3, 1.45] h {Cos[a], Sin[a]},
   {a, Switch[shape, "TriangleUp", {7. Pi/6, 11. Pi/6, Pi/2}, "Diamond", {Pi, 3. Pi/2, 0., Pi/2}, _, {2. Pi/3, 4. Pi/3, 0.}]}];
blobPortOffset[shape_, k_, n_, size_ : $declaredBlobSize] := With[{a = Pi/2 + Pi/n + 2. Pi (k - 1)/n},
   Which[
    triangleShapeQ[shape] && n == Length[triangleCorners[{0., 0.}, 1., shape]], triangleCorners[{0., 0.}, size, shape][[k]],
    shape === "Box", size {Cos[a], Sin[a]}/Max[Abs[Cos[a]], Abs[Sin[a]]],
    True, size {Cos[a], Sin[a]}]];
blobSizeOfHead[h_] := With[{sp = blobSpecOf[h]}, If[ListQ[sp], sp[[4]], $declaredBlobSize]];

(* The label of a blob fits it: its box (estimated from its LaTeX by blobLabelExtent, in units of the font size, the font
   taken as $blobLabelEm picture units: \footnotesize at 0.55 cm, \small at the default 0.7 cm and anything smaller fit),
   with the white clearing around it on a hatched blob, lies inside the blob $blobLabelMargin within its outline, so that
   a band of hatching is left all around it. The label sits where it has most room: the centre of a circle, a box or a
   diamond, near the incentre of a triangle (moved towards its widest side); a blob whose label does not fit at
   $declaredBlobSize is made larger, never its label smaller, except in a triangle. *)
$blobLabelEm = 0.6;
(* the label of a triangle is written 0.8 times smaller (TikZ scales its node), so that the triangle keeps the size of the
   other blobs (Lambda-tilde would be 0.73): the lines of a three-point vertex leave from its corners, and a box joined
   to two of them is as wide as the side between them (pictureBlobSize), so that a larger triangle would make it large *)
$triangleLabelScale = 0.8;
blobLabelScale[shape_] := If[MemberQ[{"Triangle", "TriangleUp"}, shape], $triangleLabelScale, 1.];
(* the fills drawn with lines or dots, on which the label sits in a clearing *)
texturedFillQ[fill_] := MemberQ[{"Hatched", "CrossHatched", "Dotted"}, fill];
blobClearingEm[fill_] := If[texturedFillQ[fill], 0.135, 0.07];
blobLabelMargin[fill_] := If[texturedFillQ[fill], 0.1, 0.06];
(* the box {width, height, depth} of a LaTeX label in units of its font size: the glyphs of Palatino and Computer Modern
   (the wider of the two), scripts at 0.7 (0.5 in scripts) shifted as TeX shifts them, accents and bars above. An
   estimate on the safe side, enough to size a blob *)
$labelGreekWidths = <|"Gamma" -> 0.63, "Delta" -> 0.75, "Theta" -> 0.8, "Lambda" -> 0.8, "Xi" -> 0.7, "Pi" -> 0.8, "Sigma" -> 0.7,
   "Upsilon" -> 0.72, "Phi" -> 0.8, "Psi" -> 0.82, "Omega" -> 0.8|>;
$labelAccents = {"tilde", "hat", "bar", "widetilde", "widehat", "vec", "dot", "ddot", "check", "breve", "acute", "grave"};
$labelSpaces = <|"," -> 0.17, ":" -> 0.22, ";" -> 0.28, "!" -> -0.17, "quad" -> 1., "qquad" -> 2.|>;
labelTokens[s_String] := StringCases[s, {"\\" ~~ c : LetterCharacter .. :> {"cs", c}, "\\" ~~ c_ :> {"cs", c},
    c : Except[WhitespaceCharacter] :> {"ch", c}}];
labelCharBox[c_String] := Which[
   MemberQ[CharacterRange["0", "9"], c], {0.5, 0.69, 0.},
   MemberQ[{"(", ")", "[", "]", "|"}, c], {0.45, 0.8, 0.25},
   MemberQ[{"+", "-", "=", "<", ">"}, c], {0.8, 0.58, 0.08},
   MemberQ[{",", ".", ";"}, c], {0.28, 0.12, If[c === ".", 0., 0.2]},
   c === "'", {0.3, 0.8, 0.},
   UpperCaseQ[c], {Switch[c, "W", 1., "M", 0.95, "I", 0.42, "J", 0.55, "L" | "T", 0.65, "F", 0.66, "P", 0.68, _, 0.75], 0.69, If[c === "Q", 0.2, 0.]},
   LowerCaseQ[c], {Switch[c, "i" | "j" | "l", 0.3, "f" | "t" | "r", 0.42, "m", 0.9, "w", 0.8, _, 0.58],
    If[StringContainsQ["bdfhklt", c], 0.72, 0.47], If[StringContainsQ["gjpqy", c], 0.24, 0.]},
   True, {0.6, 0.69, 0.}];
labelGreekBox[name_String] := Which[
   KeyExistsQ[$labelGreekWidths, name], {$labelGreekWidths[name], 0.7, 0.},
   MemberQ[{"alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda", "mu",
     "nu", "xi", "pi", "rho", "varrho", "sigma", "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega"}, name],
   {If[MemberQ[{"iota"}, name], 0.35, If[MemberQ[{"omega", "psi", "phi", "varphi"}, name], 0.72, 0.62]],
    If[MemberQ[{"beta", "delta", "zeta", "theta", "vartheta", "lambda", "xi", "phi", "psi"}, name], 0.72, 0.47],
    If[MemberQ[{"beta", "gamma", "zeta", "eta", "mu", "xi", "rho", "varrho", "phi", "varphi", "chi", "psi"}, name], 0.24, 0.]},
   name === "prime", {0.3, 0.8, 0.},
   True, {0.8, 0.7, 0.}];
blobLabelExtent[label_String] := Module[{toks = labelTokens[label], i = 1, sequence, atom, scripted, size = 1.},
   (* a sequence of atoms up to a closing brace, at the size s (\scriptstyle changes it for the rest of the group) *)
   sequence[s0_] := Module[{s = s0, w = 0., h = 0., d = 0., b},
     While[i <= Length[toks] && toks[[i]] =!= {"ch", "}"},
      Which[
       toks[[i]] === {"cs", "scriptstyle"}, s = 0.7; i++,
       toks[[i]] === {"cs", "scriptscriptstyle"}, s = 0.5; i++,
       MemberQ[{{"cs", "displaystyle"}, {"cs", "textstyle"}}, toks[[i]]], s = 1.; i++,
       MemberQ[{"rm", "it", "bf", "sf", "cal", "left", "right", "big", "Big", "limits", "nolimits"}, toks[[i, 2]]] && toks[[i, 1]] === "cs", i++,
       True, b = scripted[s]; w += b[[1]]; h = Max[h, b[[2]]]; d = Max[d, b[[3]]]]];
     {w, h, d}];
   atom[s_] := Module[{t, b},
     If[i > Length[toks], Return[{0., 0., 0.}]];
     t = toks[[i]]; i++;
     Which[
      t === {"ch", "{"}, b = sequence[s]; If[i <= Length[toks], i++]; b,
      t[[1]] === "ch", s labelCharBox[t[[2]]],
      MemberQ[$labelAccents, t[[2]]], b = atom[s]; {Max[b[[1]], 0.5 s], b[[2]] + 0.2 s, b[[3]]},
      t[[2]] === "overline", b = atom[s]; {b[[1]], b[[2]] + 0.22 s, b[[3]]},
      t[[2]] === "underline", b = atom[s]; {b[[1]], b[[2]], b[[3]] + 0.22 s},
      MemberQ[{"mathrm", "mathit", "mathbf", "mathsf", "mathcal", "text", "textrm", "operatorname", "boldsymbol", "mathbb"}, t[[2]]], atom[s],
      KeyExistsQ[$labelSpaces, t[[2]]], {s $labelSpaces[t[[2]]], 0., 0.},
      True, s labelGreekBox[t[[2]]]]];
   (* an atom with its superscript and subscript *)
   scripted[s_] := Module[{b = atom[s], sup = None, sub = None, ss = If[s >= 1., 0.7, 0.5], u, v},
     While[i <= Length[toks] && MemberQ[{{"ch", "^"}, {"ch", "_"}}, toks[[i]]],
      If[toks[[i]] === {"ch", "^"}, i++; sup = atom[ss], i++; sub = atom[ss]]];
     If[sup === None && sub === None, Return[b]];
     u = 0.41 s; v = If[sub === None, 0., Max[If[sup === None, 0.19, 0.25] s, sub[[2]] - 0.24 s]];
     {b[[1]] + Max[If[sup === None, 0., sup[[1]]], If[sub === None, 0., sub[[1]]]] + 0.05 s,
      Max[b[[2]], If[sup === None, 0., u + sup[[2]]], If[sub === None, 0., sub[[2]] - v]],
      Max[b[[3]], If[sup === None, 0., sup[[3]] - u], If[sub === None, 0., v + sub[[3]]]]}];
   With[{b = sequence[size]}, {1.03 b[[1]], b[[2]], b[[3]]}]];
(* the margin of a box {wd, ht} centred at the offset q in a blob of size h centred at the origin: the least distance
   from its corners to the outline (negative when it sticks out) *)
blobBoxMargin[shape_, h_, {wd_, ht_}, q_] := Which[
   shape === "Box", h - Max[Abs[q[[1]]] + wd/2, Abs[q[[2]]] + ht/2],
   triangleShapeQ[shape], With[{t = triangleCorners[{0., 0.}, h, shape]},
    Min[Table[With[{a = t[[k]], e = unitVectorOf[t[[Mod[k, Length[t]] + 1]] - t[[k]]]},
       Min[Table[e[[1]] (q + {sx wd/2, sy ht/2} - a)[[2]] - e[[2]] (q + {sx wd/2, sy ht/2} - a)[[1]], {sx, {-1, 1}}, {sy, {-1, 1}}]]],
      {k, Length[t]}]]],
   True, h - vectorNorm[Abs[q] + {wd, ht}/2]];
(* the incentre of a triangle (the centre of a diamond) *)
blobIncentre[shape_, h_] := With[{t = triangleCorners[{0., 0.}, h, shape]},
   If[Length[t] == 3, With[{l = Table[vectorNorm[t[[Mod[k, 3] + 1]] - t[[Mod[k + 1, 3] + 1]]], {k, 3}]}, l . t/Total[l]], Mean[t]]];
(* the offset of the label from the centre of a blob of size h, and the margin it has there: in a triangle the point on
   its axis of symmetry, near the incentre, with the largest margin (among the points within 0.005 of it, the nearest to
   the incentre) *)
blobLabelPlacement[shape_, h_, box_] := blobLabelPlacement[shape, h, box] = If[! MemberQ[{"Triangle", "TriangleUp"}, shape],
   {{0., 0.}, blobBoxMargin[shape, h, box, {0., 0.}]},
   Module[{c = blobIncentre[shape, h], cands, best},
    cands = Table[With[{q = c + h If[shape === "Triangle", {a, 0.}, {0., a}]}, {q, blobBoxMargin[shape, h, box, q], vectorNorm[q - c]}],
      {a, -0.4, 0.4, 0.01}];
    best = Max[cands[[All, 2]]];
    First[SortBy[Select[cands, #[[2]] >= best - 0.005 &], Last]][[{1, 2}]]]];
blobLabelBox[label_, fill_, shape_ : None] := With[{e = blobLabelExtent[label], c = blobClearingEm[fill]},
   blobLabelScale[shape] $blobLabelEm {e[[1]] + 2 c, e[[2]] + e[[3]] + 2 c}];
(* the size of a blob: $declaredBlobSize, or the least size (to 0.01) at which its label fits *)
declaredBlobSize[label_String, shape_, fill_] := declaredBlobSize[label, shape, fill] = Module[
   {box = blobLabelBox[label, fill, shape], m = blobLabelMargin[fill], lo = $declaredBlobSize, hi},
   If[blobLabelPlacement[shape, lo, box][[2]] >= m, $declaredBlobSize,
    hi = 2 lo; While[blobLabelPlacement[shape, hi, box][[2]] < m && hi < 5., hi *= 1.5];
    While[hi - lo > 0.005, With[{mid = (lo + hi)/2}, If[blobLabelPlacement[shape, mid, box][[2]] >= m, hi = mid, lo = mid]]];
    N[Ceiling[hi, 0.01]]]];
declaredBlobSize[_, _, _] := $declaredBlobSize;
(* the direction in which the legs and lines leave a port: outwards, but horizontally for the axial ports of the blob
   (option "AxialPorts" of DeclareBlob; the two bottom corners of a triangle with its apex up): the fermion line of a
   self-energy *)
portDirection[h_, shape_, k_, n_] := With[{offset = blobPortOffset[shape, k, n]},
   If[MemberQ[blobAxialPorts[h], k] && Abs[offset[[1]]] > 0.01, {Sign[offset[[1]]], 0.}, unitVectorOf[offset]]];
(* is the point q inside the blob {centre, shape, size} (not on its boundary)? *)
insideBlobQ[q_, {c_, shape_, h_}, margin_ : -0.03] := Which[
   shape === "Box", Max[Abs[q - c]] < h + margin,
   triangleShapeQ[shape], With[{t = triangleCorners[c, h, shape]},
    AllTrue[Range[Length[t]], Function[i, With[{a = t[[i]], b = t[[Mod[i, Length[t]] + 1]]},
       (b - a)[[1]] (q - a)[[2]] - (b - a)[[2]] (q - a)[[1]] > -margin vectorNorm[b - a]]]]],
   True, vectorNorm[q - c] < h + margin];
(* a line from a point to a point or a port that crosses a blob, or passes close to it away from its ends: bent around
   it, away from its centre. A line from a port of a blob that leaves it outwards, 30 degrees or more from its outline
   (both sides at a corner), moves away from it all along (the blobs are convex): it is only tested for crossing that
   blob, so that a line to the near corner of a box stays straight (in the Schwinger-Dyson equation of phi^4, the
   lines from the vertex to the two top corners of Gamma4 would otherwise both be bent, and leave the vertex together) *)
leavesBlobClearQ[e_, o_, d_] := With[{u = unitVectorOf[o - e]},
   insideBlobQ[e, d, 0.03] && ! insideBlobQ[e, d, -0.03] &&
    NoneTrue[{Pi/6., -Pi/6.}, insideBlobQ[e + 0.12 {{Cos[#], -Sin[#]}, {Sin[#], Cos[#]}} . u, d, -0.01] &]];
blobAvoidingKind[pu_, pv_, uv_, discs_] := Module[{n = rotate90[unitVectorOf[pv - pu]], s = 0., t, r},
   Do[If[AnyTrue[Table[pu + k (pv - pu), {k, 0.05, 0.95, 0.05}], insideBlobQ[#, d] &] ||
      ! leavesBlobClearQ[pu, pv, d] && ! leavesBlobClearQ[pv, pu, d] &&
      AnyTrue[Select[Table[pu + k (pv - pu), {k, 0.05, 0.95, 0.05}], vectorNorm[# - pu] > 0.3 && vectorNorm[# - pv] > 0.3 &],
       insideBlobQ[#, d, 0.22] &],
     t = (d[[1]] - pu) . n; r = Switch[d[[2]], "Box", Sqrt[2.] d[[3]], "Triangle" | "TriangleUp", 1.45 d[[3]], "Diamond", 1.3 d[[3]], _, d[[3]]] + 0.45;
     With[{s1 = If[t > 0, t - r, t + r]}, If[Abs[s1] > Abs[s], s = s1]]], {d, discs}];
   If[s == 0., "plain", {"bend", s}]];
(* the ports of a blob whose head is symmetric (DeclareSymmetric) can be taken in any order: the canonical order of its
   arguments would put the external points at arbitrary corners, different from one term to the next. The blobs are
   taken in the order in which they are placed (each joined to one placed before it); the arguments of a symmetric blob
   joined to a blob placed before it, or to a blob whose ports are fixed, take the ports facing that blob (the mirror images of the ports they are joined to),
   the others the remaining ports counterclockwise from the top left, in the order of the external labels they are, or are
   joined to (the corners x1, x2, x3, x4 of a four-point function), the rest in their order *)
$blobSharedGap = 0.4;
freelyOrderedHeadQ[h_] := (MemberQ[$symmetricHeads, h] || MemberQ[Attributes[h], Orderless]) && ! MemberQ[$antisymmetricHeads, h];
orderSymmetricBlobPorts[blobs0_List, labelNodes_List, frameLines_List] := Module[{blobs = blobs0, nb = Length[blobs0], partners, order = {1},
    queue = {1}, i, args, n, shape, offs, back, u, slot, free, k, rank, rest, freeQ},
   (* the arguments of blob i joined to an argument of blob j, by a line or as a common point: {{of i, of j}, ...} *)
   partners[a_, b_] := Join[Table[{x, x}, {x, Intersection[blobs[[a, 2]], blobs[[b, 2]]]}],
     Cases[frameLines, {p_, q_, _, _} /; MemberQ[blobs[[a, 2]], p] && MemberQ[blobs[[b, 2]], q] :> {p, q}],
     Cases[frameLines, {q_, p_, _, _} /; MemberQ[blobs[[a, 2]], p] && MemberQ[blobs[[b, 2]], q] :> {p, q}]];
   While[queue =!= {}, i = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[order, j] && partners[i, j] =!= {}, AppendTo[order, j]; AppendTo[queue, j]], {j, nb}]];
   rank[x_] := With[{ks = Flatten[Table[Position[labelNodes, w, {1}], {w, Join[{x}, Cases[frameLines, {x, w_, _, _} :> w],
          Cases[frameLines, {w_, x, _, _} :> w]]}]]}, If[ks === {}, 10^6, Min[ks]]];
   freeQ[h_] := freelyOrderedHeadQ[h];
   Do[i = order[[m]]; args = blobs[[i, 2]]; n = Length[args];
    If[n > 1 && freeQ[blobs[[i, 3]]],
     shape = blobSpecOf[blobs[[i, 3]]][[2]];
     offs = Table[blobPortOffset[shape, kk, n, blobSizeOfHead[blobs[[i, 3]]]], {kk, n}];
     (* {argument, offset of the port it is joined to} for the blobs placed before, and those whose ports are fixed *)
     back = Join @@ Table[With[{sj = blobSpecOf[blobs[[j, 3]]][[2]], nj = Length[blobs[[j, 2]]]},
         Table[{pq[[1]], blobPortOffset[sj, First[First[Position[blobs[[j, 2]], pq[[2]], {1}]]], nj, blobSizeOfHead[blobs[[j, 3]]]]}, {pq, partners[i, j]}]],
       {j, Join[Take[order, m - 1], Select[Drop[order, m], ! freeQ[blobs[[#, 3]]] &]]}];
     With[{xs = DeleteDuplicates[back[[All, 1]]]}, back = Table[First[Cases[back, {x, _}]], {x, xs}]];
     slot = Table[None, {n}]; free = Range[n];
     If[back =!= {},
      u = unitVectorOf[Total[back[[All, 2]]]];
      Do[With[{target = b[[2]] - 2 (b[[2]] . u) u},
        k = First[SortBy[free, {vectorNorm[offs[[#]] - target], #} &]];
        slot[[k]] = b[[1]]; free = DeleteCases[free, k]], {b, back}]];
     rest = Select[args, ! MemberQ[slot, #] &];
     rest = SortBy[rest, {rank[#], First[First[Position[args, #, {1}]]]} &];
     Do[slot[[free[[kk]]]] = rest[[kk]], {kk, Length[rest]}];
     blobs[[i, 2]] = slot], {m, Length[order]}];
   blobs];
(* a box joined by single lines from three of its corners to one point v (not a port), its fourth corner q elsewhere
   (Gamma4 in the Schwinger-Dyson equation of phi^4 theory, Sigma = lambda/6 G G G Gamma4): symmetric when turned by 45
   degrees, a diamond with q at one end of its horizontal diagonal and v on that diagonal, outside the opposite corner.
   {v, q, the arguments in the order of the corners of the diamond (left, bottom, right, top: those of the box turned, a
   rotation, so that the ports keep their order round it), its size}, or None. q is at the right corner, at the left one
   when it comes before v among the external labels (or joined to one that does); the diamond is the square turned, or
   larger when its upright label needs it *)
$turnedBoxPointDistance = 0.8;
turnedBoxJoin[b_, blobs_, lines_, labelNodes_] := Module[{args = b[[2]], sp = blobSpecOf[b[[3]]], ends, vs, v, m, q, rank, s},
   If[sp === None || sp[[2]] =!= "Box" || Length[args] =!= 4 || ! DuplicateFreeQ[args], Return[None]];
   (* the other end of the single line at each port that is not an external point, else None *)
   ends = Table[With[{ls = Select[lines, MemberQ[#[[{1, 2}]], x] &]},
      If[Length[ls] == 1 && ! MemberQ[labelNodes, x], If[ls[[1, 1]] === x, ls[[1, 2]], ls[[1, 1]]], None]], {x, args}];
   vs = Select[DeleteDuplicates[DeleteCases[ends, None]], Count[ends, #] == 3 &];
   If[Length[vs] =!= 1 || MemberQ[Flatten[blobs[[All, 2]]], First[vs]], Return[None]];
   v = First[vs]; m = First[Select[Range[4], ends[[#]] =!= v &]]; q = args[[m]];
   If[AnyTrue[lines, Sort[#[[{1, 2}]]] === Sort[{q, v}] &], Return[None]];
   rank[x_] := With[{ks = Flatten[Table[Position[labelNodes, w, {1}], {w, Join[{x}, Cases[lines, {x, w_, _, _} :> w], Cases[lines, {w_, x, _, _} :> w]]}]]},
     If[ks === {}, Infinity, Min[ks]]];
   s = If[rank[q] < rank[v], 1, 3] - m;
   {v, q, Table[args[[Mod[j - s - 1, 4] + 1]], {j, 4}],
    Max[Sqrt[2.] blobSizeOfHead[b[[3]]]/1.3, pictureBlobSize[sp[[1]], "Diamond", sp[[3]]]]}];
blobLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, blobs_List] := Module[
   {frameBlobs, ports, portNodes, portIndex, portOffsetIn, frameLines, centres, links, shared, progress, others, pos = {}, placedQ, posOf,
    nb, pn, dir, centroid, legPositions = {}, discs, nbrsOf, axialPoints = {}, sidePortsQ, legAngles, turned, shapeOf, sizeOf, reachOf, arcs = {}},
   frameBlobs = Select[blobs, MemberQ[mainNodes, #[[1]]] &];
   If[frameBlobs === {} || ! AllTrue[frameBlobs, declaredBlobHeadQ[#[[3]]] &], Return[None]];
   (* the blob with the first external label first: on the left when the ports do not decide *)
   With[{i1 = First[Select[Range[Length[frameBlobs]], MemberQ[frameBlobs[[#, 2]], First[labelNodes]] &], 1]},
    frameBlobs = Join[{frameBlobs[[i1]]}, Delete[frameBlobs, i1]]];
   frameLines = Select[lines, MemberQ[mainNodes, #[[1]]] && #[[1]] =!= #[[2]] &];
   nbrsOf[v_] := Join[Cases[frameLines, {v, u_, _, _} :> u], Cases[frameLines, {u_, v, _, _} :> u]];
   (* a point that is a port of three or more blobs, or three times a port of one blob: the general layout; so is a point
      twice a port of a blob whose arguments are symmetric (ordered below), or at two ports that face opposite ways *)
   sidePortsQ[b_] := With[{n = Length[b[[2]]], shape = blobSpecOf[b[[3]]][[2]], h = blobSizeOfHead[b[[3]]]},
     AllTrue[Select[DeleteDuplicates[b[[2]]], Count[b[[2]], #] > 1 &], Function[x,
       vectorNorm[Total[Table[blobPortOffset[shape, k, n, h], {k, Flatten[Position[b[[2]], x, {1}]]}]]] > 0.5 h]]];
   If[AnyTrue[Tally[Flatten[frameBlobs[[All, 2]]]], #[[2]] > 2 &] ||
     AnyTrue[frameBlobs, hasDuplicatesQ[#[[2]]] && (freelyOrderedHeadQ[#[[3]]] || ! sidePortsQ[#]) &], Return[None]];
   frameBlobs = orderSymmetricBlobPorts[frameBlobs, labelNodes, frameLines];
   (* a box joined by three corners to one point: turned by 45 degrees (turnedBoxJoin), a diamond *)
   turned = Table[turnedBoxJoin[frameBlobs[[i]], frameBlobs, frameLines, labelNodes], {i, Length[frameBlobs]}];
   Do[If[turned[[i]] =!= None, frameBlobs[[i, 2]] = turned[[i, 3]]], {i, Length[frameBlobs]}];
   shapeOf[i_] := If[turned[[i]] =!= None, "Diamond", blobSpecOf[frameBlobs[[i, 3]]][[2]]];
   sizeOf[i_] := If[turned[[i]] =!= None, turned[[i, 4]], blobSizeOfHead[frameBlobs[[i, 3]]]];
   (* how far a blob reaches along the line to a neighbouring one: the corners of a diamond *)
   reachOf[i_] := If[shapeOf[i] === "Diamond", 1.3, 1.] sizeOf[i];
   (* a point twice a port of one blob (the density of L, chi(z, z') = L(z, z; z', z'), or the end of an interaction line
      on it): the two ports (two corners of one side of a box) are points of their own (blobPortCorner), each joined to
      it by a line, and it is placed outside them as any other point, as the end of two propagators at two corners of a
      box (x <| F |> y): the lines of a box leave from its corners *)
   frameBlobs = Table[With[{b = frameBlobs[[i]]}, ReplacePart[b, 2 -> Table[If[Count[b[[2]], b[[2, k]]] > 1,
         blobPortCorner[b[[1]], b[[2, k]], k], b[[2, k]]], {k, Length[b[[2]]]}]]], {i, Length[frameBlobs]}];
   frameLines = Join[frameLines, Flatten[Table[Cases[b[[2]], c : blobPortCorner[_, x_, _] :> {c, x, None, "Spoke"}], {b, frameBlobs}], 1]];
   (* ports: {node, blob, offset from the centre of the blob} *)
   ports = Flatten[Table[With[{b = frameBlobs[[i]], n = Length[frameBlobs[[i, 2]]]},
       Table[{b[[2, k]], i, blobPortOffset[shapeOf[i], k, n, sizeOf[i]],
         If[turned[[i]] =!= None, unitVectorOf[blobPortOffset[shapeOf[i], k, n]], portDirection[b[[3]], shapeOf[i], k, n]]}, {k, n}]],
      {i, Length[frameBlobs]}], 1];
   portNodes = ports[[All, 1]];
   portIndex[v_] := First[First[Position[portNodes, v, {1}]]];
   portOffsetIn[v_, i_] := First[Cases[ports, {v, i, o_, _} :> o]];
   (* the blobs: each next to a blob already placed that it is joined to by lines between ports, or that it shares points
      with (two touching blobs, as L0 Gamma0 L: joined by short links, the common point in the middle) *)
   centres = Table[None, {Length[frameBlobs]}]; centres[[1]] = {0., 0.};
   links[i_, j_] := Select[frameLines, MemberQ[portNodes, #[[1]]] && MemberQ[portNodes, #[[2]]] &&
       Sort[{ports[[portIndex[#[[1]]], 2]], ports[[portIndex[#[[2]]], 2]]}] === Sort[{i, j}] &];
   shared[i_, j_] := If[i == j, {}, Intersection[frameBlobs[[i, 2]], frameBlobs[[j, 2]]]];
   (* a circle whose two ports are both joined to two ports of one other blob (the gap function closed on the
      particle-particle vertex, Delta = -1/2 Delta L0 Gamma_pp): its ports, otherwise on opposite sides, turned to face
      that blob, on either side of the line of the centres as the ports they are joined to (the corners of a box or a
      triangle: the lines of a box, as those of a triangle, always leave from its corners), at their spacing if the
      circle is wide enough, and the lines are parallel; else $circlePortAngle off that line, and the lines spread
      slightly towards the corners (a port further round the circle would leave its line nearly along the outline) *)
   Do[If[i != j && shapeOf[i] === "Circle" && Length[frameBlobs[[i, 2]]] == 2 && Length[links[i, j]] == 2,
     Module[{ends, cp, op, a, u, w, r, th},
      ends = Table[SortBy[l[[{1, 2}]], ports[[portIndex[#], 2]] == j &], {l, links[i, j]}];
      cp = ends[[All, 1]]; op = ends[[All, 2]];
      If[DuplicateFreeQ[cp] && DuplicateFreeQ[op] && AllTrue[Join[cp, op], Count[portNodes, #] == 1 &] &&
        vectorNorm[Total[Table[ports[[portIndex[q], 3]], {q, op}]]] > 0.1,
       a = Table[ports[[portIndex[q], 3]], {q, op}];
       (* u: from the other blob towards the circle, the side its two ports face; w across it *)
       u = unitVectorOf[Total[a]]; w = rotate90[u]; r = sizeOf[i];
       th = ArcSin[Min[vectorNorm[a[[1]] - a[[2]]]/(2 r), Sin[$circlePortAngle]]];
       Do[With[{o = r (-Cos[th] u + Sign[(a[[k]] - Mean[a]) . w] Sin[th] w)},
         ports[[portIndex[cp[[k]]], 3]] = o; ports[[portIndex[cp[[k]]], 4]] = unitVectorOf[o]], {k, 2}]]]],
    {i, Length[frameBlobs]}, {j, Length[frameBlobs]}];
   progress = True;
   While[progress && MemberQ[centres, None], progress = False;
    Do[If[centres[[j]] === None, Do[If[centres[[i]] =!= None && (links[i, j] =!= {} || shared[i, j] =!= {}) && centres[[j]] === None,
        With[{ends = Flatten[links[i, j][[All, {1, 2}]]], gap = If[links[i, j] === {}, $blobSharedGap, $blobGap]},
         With[{d = Total[Join[Table[If[ports[[portIndex[e], 2]] == i, 1, -1] ports[[portIndex[e], 3]], {e, ends}],
              Table[portOffsetIn[s, i] - portOffsetIn[s, j], {s, shared[i, j]}]]]},
          (* ports on opposite corners (the particle-particle ladder): side by side, further apart, as the lines cross *)
          centres[[j]] = centres[[i]] + With[{hh = reachOf[i] + reachOf[j]},
             If[vectorNorm[d] < 0.1, (hh + gap + 0.65) {1., 0.}, (hh + gap) unitVectorOf[d]]];
          progress = True]]], {i, Length[frameBlobs]}]], {j, Length[frameBlobs]}]];
   If[MemberQ[centres, None], Return[None]];
   (* a point shared by two blobs midway between their ports, which are kept (blobPortCorner) as the ends of the links *)
   pos = Join[Table[{frameBlobs[[i, 1]], centres[[i]]}, {i, Length[frameBlobs]}],
     Table[{v, Mean[Cases[ports, {v, i_, o_, _} :> centres[[i]] + o]]}, {v, DeleteDuplicates[portNodes]}],
     Join @@ Table[If[Count[portNodes, p[[1]]] > 1, {{blobPortCorner[frameBlobs[[p[[2]], 1]], p[[1]]], centres[[p[[2]]]] + p[[3]]}}, {}],
       {p, ports}]];
   placedQ[v_] := MemberQ[pos[[All, 1]], v];
   posOf[v_] := First[Cases[pos, {v, p_} :> p]];
   (* the point joined to three corners of a turned box: on its horizontal diagonal, $turnedBoxPointDistance outside the
      corner opposite the fourth; its lines to the top and bottom corners follow the circle through the three points
      (they leave it vertically, the line to the near corner horizontally), passed on to frameItems as bends *)
   Do[If[turned[[i]] =!= None && ! placedQ[turned[[i, 1]]],
     With[{v = turned[[i, 1]], q = turned[[i, 2]]}, With[{o = -portOffsetIn[q, i]},
       AppendTo[pos, {v, centres[[i]] + o + $turnedBoxPointDistance unitVectorOf[o]}];
       With[{tb = Select[frameBlobs[[i, 2]], # =!= q && vectorNorm[portOffsetIn[#, i] - o] > 10.^-6 &]},
        With[{circ = circleThrough[posOf[v], posOf[tb[[1]]], posOf[tb[[2]]]]},
         If[circ =!= None, arcs = Join[arcs, Table[{v, t, circ[[1]], circ[[2]]}, {t, tb}]]]]]]]], {i, Length[frameBlobs]}];
   (* the other points: outside the ports they are joined to, or next to a point already placed *)
   others = Select[mainNodes, Head[#] =!= blob && ! MemberQ[portNodes, #] &];
   progress = True;
   While[progress && ! AllTrue[others, placedQ], progress = False;
    centroid = Mean[pos[[All, 2]]];
    Do[If[! placedQ[v],
      nb = Select[nbrsOf[v], placedQ]; pn = Select[nb, MemberQ[portNodes, #] &];
      Which[
       pn =!= {},
       dir = Total[Table[ports[[portIndex[q], 3]], {q, pn}]];
       If[vectorNorm[dir] < 0.1, dir = Mean[posOf /@ pn] - centroid];
       With[{axial = Select[pn, Abs[ports[[portIndex[#], 4, 2]]] < 10.^-6 && ports[[portIndex[#], 4]] =!= unitVectorOf[ports[[portIndex[#], 3]]] &&
            AnyTrue[frameLines, Function[l, Sort[l[[{1, 2}]]] === Sort[{v, #}] && ! interactionStyleQ[l[[4]]]]] &]},
        If[axial =!= {},
         (* joined by a propagator to a bottom corner of a triangle with its apex up: on the line of its base, as the
            fermion line of a self-energy *)
         AppendTo[pos, {v, posOf[First[axial]] + 1.2 $blobPointDistance ports[[portIndex[First[axial]], 4]]}];
         AppendTo[axialPoints, {v, ports[[portIndex[First[axial]], 4]]}],
         (* further from two or more ports, so that its lines do not meet at a sharp angle *)
         AppendTo[pos, {v, Mean[posOf /@ pn] + If[Length[pn] > 1, 1.15, 1.] $blobPointDistance unitVectorOf[dir]}]]]; progress = True,
       nb =!= {},
       AppendTo[pos, {v, Mean[posOf /@ nb] + $blobPointDistance unitVectorOf[Mean[posOf /@ nb] - centroid]}]; progress = True]],
     {v, others}]];
   If[! AllTrue[others, placedQ], Return[None]];
   centroid = Mean[pos[[All, 2]]];
   (* the legs: outwards from a port (along the side of the blob if a neighbouring blob is in the way), else in the
      largest free sector; two or more legs at one point (the vertex of an interaction line) fanned 90 (60) degrees apart,
      counterclockwise from the top left as the corners of a box *)
   legAngles[q_] := legAngles[q] = Module[{ks = Select[Range[Length[labels]], amputated[[#]] && labelNodes[[#]] === q &], a, m},
     m = Length[ks];
     a = gapDirection[Table[angleOf[posOf[w] - posOf[q]], {w, nbrsOf[q]}], preferredDirection[posOf[q], centroid]];
     AssociationThread[ks, SortBy[If[m == 1, {a}, Table[a + (j - (m + 1)/2.) If[m == 2, 90., 60.] Degree, {j, m}]],
       Mod[# - 3. Pi/4 + 0.01, 2. Pi] &]]];
   Do[If[amputated[[k]], With[{q = labelNodes[[k]]},
      If[MemberQ[portNodes, q],
       With[{pr = ports[[portIndex[q]]]}, Module[{u = pr[[4]], c = centres[[pr[[2]]]]},
         Do[If[j =!= pr[[2]], With[{dc = centres[[j]] - c},
            Which[
             Abs[dc[[2]]] < 0.5 $declaredBlobSize && dc[[1]] u[[1]] > 0.01 && Abs[u[[2]]] > 0.01, u = {0., Sign[u[[2]]]},
             Abs[dc[[1]]] < 0.5 $declaredBlobSize && dc[[2]] u[[2]] > 0.01 && Abs[u[[1]]] > 0.01, u = {Sign[u[[1]]], 0.}]]],
          {j, Length[centres]}];
         AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength u}]]],
       If[MemberQ[axialPoints[[All, 1]], q],
        (* a point on the base of a triangle: its leg continues the base *)
        AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength First[Cases[axialPoints, {q, u_} :> u]]}],
        AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength directionAt[legAngles[q][k]]}]]]]],
    {k, Length[labels]}];
   (* the blobs centred on the axis of the formula *)
   With[{m = Mean[centres]},
    discs = Table[{centres[[i]] - m, shapeOf[i], sizeOf[i]}, {i, Length[frameBlobs]}];
    {Table[{q[[1]], q[[2]] - m}, {q, Join[pos, legPositions]}],
     {"blob", discs, Table[{a[[1]], a[[2]], a[[3]] - m, a[[4]]}, {a, arcs}], centroid - m}, {}}]];

(* the chords that hatch a blob, at 45 degrees (sense 1) or at -45 degrees (sense -1); for a triangle, the parts of the
   lines t n + s e inside it. A diamond, whose edges are at 45 degrees, is hatched vertically (sense 1) and horizontally
   (sense -1), so that no hatch line runs along an edge and doubles it *)
triangleChord[t_, e_, n_, corners_] := Module[{ss = {}},
   Do[With[{a = corners[[i]], b = corners[[Mod[i, Length[corners]] + 1]]},
     With[{den = (b - a) . n},
      If[Abs[den] > 10.^-9, With[{u = (t - a . n)/den}, If[0 <= u <= 1, AppendTo[ss, (a + u (b - a)) . e]]]]]], {i, Length[corners]}];
   If[Length[ss] < 2, None, {t n + Min[ss] e, t n + Max[ss] e}]];
blobHatchSegments[p_, shape : "Triangle" | "TriangleUp" | "Diamond", h_, sense_] := With[{e = If[shape === "Diamond", If[sense == 1, {0., 1.}, {1., 0.}], {1., sense}/Sqrt[2.]],
    n = If[shape === "Diamond", If[sense == 1, {1., 0.}, {0., 1.}], {1., -sense}/Sqrt[2.]],
    corners = triangleCorners[{0., 0.}, h, shape]},
   Select[Table[With[{c = triangleChord[t, e, n, corners]}, If[c === None, None, p + # & /@ c]],
     {t, $hatchSpacing Range[-Floor[1.5 h/$hatchSpacing], Floor[1.5 h/$hatchSpacing]]}], # =!= None && vectorNorm[#[[2]] - #[[1]]] > 0.02 &]];
blobHatchSegments[p_, shape_, h_, sense_] := Module[{e = {1., sense}/Sqrt[2.], n = {1., -sense}/Sqrt[2.], tmax, m},
   tmax = If[shape === "Box", Sqrt[2.] h, h]; m = Floor[(tmax - 0.02)/$hatchSpacing];
   Table[With[{half = If[shape === "Box", Sqrt[2.] h - Abs[t], Sqrt[Max[0., h^2 - t^2]]]},
     {p + t n - half e, p + t n + half e}], {t, $hatchSpacing Range[-m, m]}]];
blobHatchesOf[p_, {_, shape_, fill_, h_}] := Switch[fill,
   "Hatched", blobHatchSegments[p, shape, h, 1],
   "CrossHatched", Join[blobHatchSegments[p, shape, h, 1], blobHatchSegments[p, shape, h, -1]],
   _, {}];
(* the fill "Dotted", sparser than the hatching: dots on a square grid turned by 45 degrees (staggered rows), centred on
   the blob, about $dotFillSpacing apart (the hatching is $hatchSpacing apart), those at least $dotFillMargin inside the
   outline; the pattern is symmetric about the centre and the two axes, as the blob. In a box and in a diamond the grid is
   fitted, a little denser or sparser, so that its outermost dots lie $dotFillMargin inside every side, as the hatching
   runs to the outline *)
$dotFillSpacing = 0.14; $dotFillMargin = 0.06;
insideBlobOutlineQ[q_, p_, shape_, h_, m_] := Switch[shape,
   "Box", Max[Abs[q - p]] <= h - m,
   (* the corners counterclockwise: the inward normal of each edge on its left *)
   "Triangle" | "TriangleUp" | "Diamond", With[{c = triangleCorners[p, h, shape]},
    AllTrue[Range[Length[c]], Function[i, With[{e = c[[Mod[i, Length[c]] + 1]] - c[[i]]},
       {-e[[2]], e[[1]]} . (q - c[[i]])/vectorNorm[e] >= m - 10.^-9]]]],
   _, vectorNorm[q - p] <= h - m];
(* the step d of the grid, whose dots are {i + j, i - j} d: in a box the coordinates of the dots are multiples of d, up to
   h - margin; in a diamond (|x| + |y| <= 1.3 h) the sums |x| + |y| are multiples of 2 d *)
dotFillStep[shape_, h_] := With[{d0 = $dotFillSpacing/Sqrt[2.]}, Switch[shape,
    "Box", With[{r = h - $dotFillMargin}, r/Max[1, Round[r/d0]]],
    "Diamond", With[{r = 1.3 h - Sqrt[2.] $dotFillMargin}, r/(2 Max[1, Round[r/(2 d0)]])],
    _, d0]];
blobDotsOf[p_, {_, shape_, "Dotted", h_}] := With[{d = dotFillStep[shape, h]}, With[{n = Ceiling[1.5 h/d] + 1},
    Select[Flatten[Table[p + d {i + j, i - j}, {i, -n, n}, {j, -n, n}], 1],
     insideBlobOutlineQ[#, p, shape, h, $dotFillMargin - 10.^-9] &]]];
blobDotsOf[_, _] := {};
(* the label of a blob as plain text, for the pictures of Mathematica, read from the tokens of its LaTeX (labelTokens):
   Greek letters and a few symbols as characters, the scripts written on the line (Gamma_{\rm irr} as Gammairr), the
   commands of style, font and spacing dropped (\scriptstyle W^{(4)} is W(4)); an accent written above the atom that
   follows it (a combining character would be small and close to it), a bar or an overline over the whole group, so that
   Gamma_{\overline{\rm ph}} stays apart from Gamma_{\rm ph}. Another command is written as its name *)
$plainLabelCharacters = <|"alpha" -> "\[Alpha]", "beta" -> "\[Beta]", "gamma" -> "\[Gamma]", "delta" -> "\[Delta]",
   "epsilon" -> "\[Epsilon]", "varepsilon" -> "\[CurlyEpsilon]", "zeta" -> "\[Zeta]", "eta" -> "\[Eta]", "theta" -> "\[Theta]",
   "vartheta" -> "\[CurlyTheta]", "iota" -> "\[Iota]", "kappa" -> "\[Kappa]", "lambda" -> "\[Lambda]", "mu" -> "\[Mu]", "nu" -> "\[Nu]",
   "xi" -> "\[Xi]", "pi" -> "\[Pi]", "varpi" -> "\[CurlyPi]", "rho" -> "\[Rho]", "varrho" -> "\[CurlyRho]", "sigma" -> "\[Sigma]",
   "varsigma" -> "\[FinalSigma]", "tau" -> "\[Tau]", "upsilon" -> "\[Upsilon]", "phi" -> "\[Phi]", "varphi" -> "\[CurlyPhi]",
   "chi" -> "\[Chi]", "psi" -> "\[Psi]", "omega" -> "\[Omega]", "Gamma" -> "\[CapitalGamma]", "Delta" -> "\[CapitalDelta]",
   "Theta" -> "\[CapitalTheta]", "Lambda" -> "\[CapitalLambda]", "Xi" -> "\[CapitalXi]", "Pi" -> "\[CapitalPi]",
   "Sigma" -> "\[CapitalSigma]", "Upsilon" -> "\[CapitalUpsilon]", "Phi" -> "\[CapitalPhi]", "Psi" -> "\[CapitalPsi]",
   "Omega" -> "\[CapitalOmega]", "ell" -> "\[ScriptL]", "infty" -> "\[Infinity]", "partial" -> "\[PartialD]", "nabla" -> "\[Del]",
   "prime" -> "\[Prime]", "pm" -> "\[PlusMinus]", "times" -> "\[Times]", "cdot" -> "\[CenterDot]", " " -> " ", "quad" -> " ",
   "qquad" -> "  "|>;
$plainLabelAccents = <|"tilde" -> (Overscript[#, "~"] &), "widetilde" -> (Overscript[#, "~"] &), "hat" -> (Overscript[#, "^"] &),
   "widehat" -> (Overscript[#, "^"] &), "bar" -> OverBar, "overline" -> OverBar, "vec" -> OverVector, "dot" -> (Overscript[#, "."] &),
   "ddot" -> (Overscript[#, ".."] &)|>;
$plainLabelDropped = {"rm", "it", "bf", "sf", "cal", "mathrm", "mathit", "mathbf", "mathsf", "mathcal", "mathbb", "boldsymbol",
   "text", "textrm", "operatorname", "scriptstyle", "scriptscriptstyle", "textstyle", "displaystyle", "left", "right", "big",
   "Big", "limits", "nolimits", ",", ":", ";", "!", "check", "breve", "acute", "grave", "underline"};
(* a list of strings and accented atoms as one label: the strings next to each other joined *)
plainLabelJoin[pieces_List] := With[{r = Join @@ (If[AllTrue[#, StringQ], {StringJoin[#]}, #] & /@
       Split[pieces, StringQ[#1] && StringQ[#2] &])}, If[MatchQ[r, {_String}], First[r], Row[r]]];
plainBlobLabel[label_String] := Module[{toks = labelTokens[label], i = 1, sequence, atom},
   (* the atoms up to a closing brace *)
   sequence[] := Module[{out = {}}, While[i <= Length[toks] && toks[[i]] =!= {"ch", "}"}, out = Join[out, atom[]]]; out];
   atom[] := Module[{t, b},
     If[i > Length[toks], Return[{}]];
     t = toks[[i]]; i++;
     Which[
      t === {"ch", "{"}, b = sequence[]; If[i <= Length[toks], i++]; b,
      t[[1]] === "ch", If[MemberQ[{"^", "_"}, t[[2]]], {}, {t[[2]]}],
      KeyExistsQ[$plainLabelCharacters, t[[2]]], {$plainLabelCharacters[t[[2]]]},
      KeyExistsQ[$plainLabelAccents, t[[2]]], b = atom[]; If[b === {}, {}, {$plainLabelAccents[t[[2]]][plainLabelJoin[b]]}],
      MemberQ[$plainLabelDropped, t[[2]]], {},
      True, {t[[2]]}]];
   Module[{pieces = sequence[]},
    (* a stray closing brace is skipped *)
    While[i <= Length[toks], i++; pieces = Join[pieces, sequence[]]];
    If[pieces === {}, "", plainLabelJoin[pieces]]]];
(* where the label of a blob is written: the centre, or near the incentre of a triangle (blobLabelPlacement) *)
blobLabelPoint[p_, {label_, shape_, fill_, h_}] := p + If[triangleShapeQ[shape] && StringQ[label],
    blobLabelPlacement[shape, h, blobLabelBox[label, fill, shape]][[1]], {0., 0.}];
declaredBlobPrimitives[p_, spec : {label_, shape_, fill_, h_}, a_ : Automatic] := {
   If[fill === "Grey", diagramColor[a, "GreyFill"], diagramColor[a, "Paper"]],
   Switch[shape, "Box", Rectangle[p - {h, h}, p + {h, h}], "Triangle" | "TriangleUp" | "Diamond", Polygon[triangleCorners[p, h, shape]], _, Disk[p, h]],
   diagramColor[a, "Foreground"], {AbsoluteThickness[0.6], Line /@ blobHatchesOf[p, spec]},
   With[{dots = blobDotsOf[p, spec]}, If[dots === {}, {}, {AbsolutePointSize[1.3], Point[dots]}]],
   (* on a hatched or dotted blob the label sits in a clearing of the paper colour with rounded corners, the outline drawn
      over it (as in TikZ); a blob without a name (unnamedBlobSpec) has neither *)
   If[label === "", {}, Text[If[texturedFillQ[fill],
     Framed[Style[plainBlobLabel[label], blobLabelScale[shape] $fontPixels], Background -> diagramColor[a, "Paper"],
      FrameStyle -> diagramColor[a, "Paper"], RoundingRadius -> 3,
      FrameMargins -> {{2, 2}, {1, 2}}],
     Style[plainBlobLabel[label], blobLabelScale[shape] $fontPixels]], blobLabelPoint[p, spec]]],
   Switch[shape, "Box", Line[{p + {-h, -h}, p + {h, -h}, p + {h, h}, p + {-h, h}, p + {-h, -h}}],
    "Triangle" | "TriangleUp" | "Diamond", Line[Append[triangleCorners[p, h, shape], First[triangleCorners[p, h, shape]]]], _, Circle[p, h]]};

(* the unit layout of a vacuum part, or None *)
unitLayoutVacuum[component_List, lines_List] := Module[{frameLines = Select[lines, MemberQ[component, #[[1]]] &], fl},
   fl = fermionLines[component, frameLines, {}];
   If[fl === None || fl[[1]] =!= {}, None, layoutVacuumTree[fl[[2]], interactionPairsOf[component, frameLines]]]];

(* ---------------------------------------------------------------- the elements of a term *)
styleForHead[h_, opts_List] := Module[{style = Which[
      MemberQ[Flatten[{drawingOption[opts, "Directed"]}], h], "Arrow",
      MemberQ[Flatten[{drawingOption[opts, "Interactions"]}], h], "Wavy",
      True, lineStyleOf[h]], override = drawingOption[opts, "InteractionStyle"]},
   If[interactionStyleQ[style] && MemberQ[{"Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag"}, override], override, style]];

(* {weight, lines, blobs, stubs, classes}: lines {tail, head, factor head, style} (a directed line G[x, y] runs from y
   to x), blobs {id, legs, head}, stubs {label, head}; classes: the points joined by delta functions *)
diagramElements[diagramTerm[c_, o_, d_], ext_List, opts_List] := Module[{factors = expandIntegerPowers[Join[factorList[c], o]], points,
    deltaPairs = {}, weight = 1, lines = {}, blobs = {}, stubs = {}, blobCount = 0, classes, representative, rules},
   points = DeleteDuplicates[Join[ext, d, Flatten[Cases[factors, DeltaFunction[x_, y_] :> {x, y}]]]];
   Do[If[AtomQ[f], weight = weight f,
     With[{labels = Select[List @@ f, MemberQ[points, #] &]},
      Which[
       Head[f] === DeltaFunction && Length[labels] == 2, AppendTo[deltaPairs, labels],
       (* a local vertex: its labels are one point (without labels it is a factor of the weight) *)
       localVertexQ[f] && Length[labels] >= 1, deltaPairs = Join[deltaPairs, Table[{First[labels], l}, {l, Rest[labels]}]],
       labels === {}, weight = weight f,
       (* a declared blob, even with two labels *)
       declaredBlobQ[f] && Length[labels] >= 2, blobCount++; AppendTo[blobs, {blob[blobCount], labels, Head[f]}],
       Length[labels] == 1, AppendTo[stubs, {labels[[1]], Head[f]}],
       Length[labels] == 2 && Length[f] == 2, With[{style = styleForHead[Head[f], opts]},
        AppendTo[lines, If[directedStyleQ[style], {labels[[2]], labels[[1]], Head[f], style}, {labels[[1]], labels[[2]], Head[f], style}]]],
       True, blobCount++; AppendTo[blobs, {blob[blobCount], labels, Head[f]}]]]], {f, factors}];
   classes = mergeOverlapping[Table[{p}, {p, points}], deltaPairs];
   (* the representative of a class: its first external label, else its first point *)
   representative[x_] := With[{cl = First[Select[classes, MemberQ[#, x] &]]},
     With[{e = Select[ext, MemberQ[cl, #] &]}, If[e =!= {}, First[e], First[Select[points, MemberQ[cl, #] &]]]]];
   rules = Table[p -> representative[p], {p, points}];
   lines = Table[{li[[1]] /. rules, li[[2]] /. rules, li[[3]], li[[4]]}, {li, lines}];
   blobs = Table[{bl[[1]], bl[[2]] /. rules, bl[[3]]}, {bl, blobs}];
   stubs = Table[{st[[1]] /. rules, st[[2]]}, {st, stubs}];
   {weight, lines, blobs, stubs, classes, rules}];

(* a point joining one line to one leg of a blob is not drawn: the line ends on the blob. The legs of a declared blob
   stay: they are its ports, on its boundary *)
attachLinesToBlobs[lines0_, blobs0_, stubs_, d_] := Module[{lines = lines0, blobs = blobs0, li, bi},
   Do[li = Select[Range[Length[lines]], MemberQ[lines[[#, {1, 2}]], x] &];
    bi = Select[Range[Length[blobs]], MemberQ[blobs[[#, 2]], x] &];
    If[Length[li] == 1 && Count[lines[[li[[1]], {1, 2}]], x] == 1 && Length[bi] == 1 && Count[blobs[[bi[[1]], 2]], x] == 1 &&
      FreeQ[stubs, x] && ! declaredBlobHeadQ[blobs[[bi[[1]], 3]]],
     lines[[li[[1]]]] = Join[lines[[li[[1]], {1, 2}]] /. x -> blobs[[bi[[1]], 1]], lines[[li[[1]], {3, 4}]]];
     blobs[[bi[[1]], 2]] = DeleteCases[blobs[[bi[[1]], 2]], x]], {x, d}];
   {lines, blobs}];

(* is the external label drawn with a leg (amputated) or as an open circle at the end of a propagator? *)
amputatedLabelQ[e_, lines_, blobs_, stubs_, classes_, opts_List] := Module[{setting = drawingOption[opts, "AmputatedLegs"], ends, cl},
   Which[setting === True, True, setting === False, False,
    True,
    cl = First[Select[classes, MemberQ[#, e] &], {e}];
    ends = Count[Flatten[Cases[lines, {u_, v_, _, _} :> {u, v}]], e] + Count[Flatten[Cases[blobs, {_, l_, _} :> l]], e] +
      Count[Cases[stubs, {u_, _} :> u], e];
    (* a label alone on its point, at the end of a single line (a propagator or an interaction line), is drawn as an
       open circle; otherwise it is a vertex of an amputated function, drawn with a leg *)
    ! (Length[cl] == 1 && ends == 1 && Length[Cases[lines, {e, u_, _, _} /; u =!= e]] + Length[Cases[lines, {u_, e, _, _} /; u =!= e]] == 1)]];

(* directed lines entering minus leaving the node, for the arrows of the legs; an end of -<T psi psi> counts as entering,
   an end of -<T psibar psibar> as leaving *)
fermionImbalance[node_, lines_] := Count[lines, {u_, node, _, s_} /; u =!= node && directedStyleQ[s]] -
   Count[lines, {node, u_, _, s_} /; u =!= node && directedStyleQ[s]] +
   Count[lines, {u_, w_, _, "DivergingArrows"} /; u =!= w && (u === node || w === node)] -
   Count[lines, {u_, w_, _, "ConvergingArrows"} /; u =!= w && (u === node || w === node)];

(* directions ("Out", "In" or None) of the legs of the external labels, from all the terms of an expression (the
   diagramElements of each term) *)
legDirectionsFor[elementsList_List, ext_List, opts_List] := Module[{spec = drawingOption[opts, "LegDirections"], votes = {}, anyDirected = False,
    rules, dir},
   Which[
    ListQ[spec] && Length[spec] == Length[ext], Thread[ext -> spec],
    spec === None, Thread[ext -> None],
    spec === "Alternating", Thread[ext -> Table[If[OddQ[k], "Out", "In"], {k, Length[ext]}]],
    True,
    Do[If[AnyTrue[elements[[2]], fermionStyleQ[#[[4]]] &], anyDirected = True];
     Do[With[{cl = First[Select[elements[[5]], MemberQ[#, e] &], {}]},
       (* a label alone on its point: one more line entering than leaving means an outgoing leg, and vice versa;
          as many entering as leaving (e.g. a density vertex) means a leg without arrow *)
       If[cl =!= {} && Count[cl, x_ /; MemberQ[ext, x]] == 1 && MemberQ[Flatten[Cases[elements[[2]], {u_, v_, _, _} :> {u, v}]], e],
        With[{b = fermionImbalance[e, elements[[2]]]},
         AppendTo[votes, e -> Which[b == 1, "Out", b == -1, "In", True, None]]]]],
      {e, ext}], {elements, elementsList}];
    Table[With[{v = Union[Cases[votes, (ext[[k]] -> x_) :> x]]},
      ext[[k]] -> Which[Length[v] == 1, First[v], Length[v] > 1, None,
        anyDirected || MemberQ[votes, _ -> "Out" | "In"], If[OddQ[k], "Out", "In"], True, None]], {k, Length[ext]}]]];

(* The external labels at which a density (two fields at one point, a composite operator such as psibar psi or phi^2) is
   inserted: in every term the label is alone at its point and its point ends exactly two propagators, one entering and
   one leaving (fermions) or two undirected ones (bosons), and nothing else (no interaction line, blob or source). Such a
   point is the insertion of an external field U(z): it is drawn as the crossed circle of a source insertion, without a
   leg. The same label may be the third corner of a triangle blob (a vertex of the density, DeclareBlob with "Shape" ->
   "Triangle"), alone there, or share its point with exactly two other labels, one with an outgoing and one with an
   incoming leg, and nothing else (the bare vertex, delta(x1, z) delta(x2, z)): the crossed circle is then drawn at the
   corner or at that point, with the label. "AmputatedLegs" -> True or False turns this off. *)
$densityLabels = {}; $densityNodes = {}; $densityNodeLabels = {}; $multiplePairs = {}; $selfLoopNodes = {};
densityLabelsFor[terms_List, elementsList_List, ext_List, opts_List, directions_List : {}] := Module[{t, elements, ok, seen, node, ends, at,
    blobLegs, cl},
   If[drawingOption[opts, "AmputatedLegs"] === True || drawingOption[opts, "AmputatedLegs"] === False, Return[{}]];
   ok = Table[True, {Length[ext]}]; seen = Table[False, {Length[ext]}];
   Do[t = terms[[i]]; elements = elementsList[[i]];
    Do[node = ext[[k]] /. elements[[6]];
     at = Select[elements[[2]], #[[1]] === node || #[[2]] === node &];
     blobLegs = Flatten[Cases[elements[[3]], {_, l_, _} :> l]];
     cl = First[Select[elements[[5]], MemberQ[#, ext[[k]]] &], {ext[[k]]}];
     Which[
      (* the third corner of a triangle blob *)
      at === {} && Count[blobLegs, node] == 1 && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] &&
       Count[cl, x_ /; MemberQ[ext, x]] == 1 &&
       MatchQ[Cases[elements[[3]], {_, l_, h_} /; MemberQ[l, node] :> {l, blobSpecOf[h]}],
        {{l_, sp_}} /; Length[l] == 3 && l[[3]] === node && ListQ[sp] && MemberQ[{"Triangle", "TriangleUp"}, sp[[2]]]],
      seen[[k]] = True,
      (* a point twice a port of one blob and nothing else (the susceptibility chi = gamma L gamma): the end of two lines
         at two corners of the blob *)
      at === {} && Count[blobLegs, node] == 2 && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] &&
       Count[cl, x_ /; MemberQ[ext, x]] == 1 && Count[elements[[3]], {_, l_, _} /; Count[l, node] == 2] == 1,
      seen[[k]] = True,
      (* the bare vertex: three labels at a point without lines, the other two with an outgoing and an incoming leg; for
         bosons (legs without direction) the last label is the density *)
      at === {} && ! MemberQ[blobLegs, node] && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] && Length[cl] == 3 &&
       Count[cl, x_ /; MemberQ[ext, x]] == 3 && MemberQ[{None, ext[[k]]}, ext[[k]] /. directions] &&
       (Sort[(DeleteCases[cl, ext[[k]]] /. directions)] === {"In", "Out"} ||
        k == Length[ext] && AllTrue[DeleteCases[cl, ext[[k]]], MemberQ[{None, #}, # /. directions] &]),
      seen[[k]] = True,
      at =!= {} || MemberQ[blobLegs, node] || MemberQ[Cases[elements[[4]], {u_, _} :> u], node],
      seen[[k]] = True;
      ends = Count[Flatten[at[[All, {1, 2}]]], node];
      If[! (Count[First[Select[elements[[5]], MemberQ[#, ext[[k]]] &], {ext[[k]]}], x_ /; MemberQ[ext, x]] == 1 && ends == 2 &&
           (* a label of a local vertex (DeclareLocalVertex) is a leg of that vertex *)
           FreeQ[t[[1]], h_Symbol[___, ext[[k]], ___] /; MemberQ[$localVertexHeads, h]] &&
           ! MemberQ[Flatten[Cases[elements[[3]], {_, l_, _} :> l]], node] && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] &&
           ! AnyTrue[at, interactionStyleQ[#[[4]]] &] &&
           (AllTrue[at, fermionStyleQ[#[[4]]] &] && fermionImbalance[node, at] == 0 || ! AnyTrue[at, fermionStyleQ[#[[4]]] &])),
       ok[[k]] = False]], {k, Length[ext]}], {i, Length[terms]}];
   Pick[ext, Table[ok[[k]] && seen[[k]], {k, Length[ext]}]]];

(* offsets of the lines of a bundle between two nodes at distance dd; kind: "plain", "axis", {"skip", k}, {"cycle", s} *)
bundleOffsets[lines_, dd_, kind_] := Module[{propagators = Select[lines, ! interactionStyleQ[#[[4]]] &],
    interactions = Select[lines, interactionStyleQ[#[[4]]] &], k, h, symmetric, ordered},
   symmetric[m_] := If[m <= 1, Table[0., {m}], Table[0.9 dd ((j - 1)/(m - 1) - 1/2), {j, m}]];
   Which[
    (* only bosonic lines (gluons, phonons) between neighbours on the axis: a bubble, symmetric about the axis *)
    kind === "axis" && propagators === {} && Length[interactions] >= 2,
    {interactions, Table[0.5 dd (2 (j - 1)/(Length[interactions] - 1) - 1), {j, Length[interactions]}]},
    kind === "axis", {Join[propagators, interactions], Join[symmetric[Length[propagators]],
       Table[If[propagators === {}, 0.45 dd j - 0.45 dd, Max[symmetric[Length[propagators]]] + 0.45 dd j], {j, Length[interactions]}]]},
    kind === "axisBelow", {Join[propagators, interactions], Join[symmetric[Length[propagators]],
       Table[If[propagators === {}, -(0.45 dd j - 0.45 dd), -(Max[symmetric[Length[propagators]]] + 0.45 dd j)], {j, Length[interactions]}]]},
    ListQ[kind] && kind[[1]] === "skip", {lines, Table[kind[[3]] (If[Length[kind] >= 4 && NumberQ[kind[[4]]], kind[[4]], 0.26 + 0.1 (kind[[2]] - 1)] +
        0.12 (j - 1)) dd, {j, Length[lines]}]},
    (* along the arc of a round loop: an interaction line between the same two vertices well inside it, clear of the arc *)
    ListQ[kind] && kind[[1]] === "bubbleCircle", {Join[propagators, interactions], kind[[{2, 3}]]},
    ListQ[kind] && kind[[1]] === "circle", ordered = Join[propagators, interactions];
    (* three or more lines (a sunset on the loop): a melon about the chord, the innermost no deeper than the arc is high,
       not a second circle inside the loop *)
    With[{step = If[Length[ordered] >= 3, Max[0.3 dd, Min[0.5 dd, 2 Abs[kind[[2]]]/(Length[ordered] - 1)]], 0.5 dd]},
     {ordered, Table[kind[[2]] - Sign[kind[[2]]] step (j - 1), {j, Length[ordered]}]}],
    ListQ[kind], ordered = Join[propagators, interactions]; {ordered, Table[kind[[2]] - Sign[kind[[2]]] 0.35 dd (j - 1), {j, Length[ordered]}]},
    True,
    k = Length[propagators]; h = Ceiling[k/2];
    ordered = Join[Take[propagators, h], interactions, Drop[propagators, h]];
    {ordered, symmetric[Length[ordered]]}]];

preferredDirection[p_, centre_] := If[vectorNorm[p - centre] < 0.05, {1., 0.}, unitVectorOf[p - centre] + {0., 0.3}];

labelString[DummyVariable[_, k_]] := "z" <> ToString[k];
labelString[x_] := ToString[x];
labelBox[x_] := {0.62 $fontPixels StringLength[labelString[x]], 0.75 $fontPixels}/$pixelsPerUnit;

(* geometry of one frame (the part with the external labels, or one vacuum component). Items:
   {"E", path, style} lines; {"L", node point, end point, direction} legs; {"S", {p, q}} source insertions;
   {"N", p, kind} nodes ("dot", "ext", and "blob" or "vertexFunction" for a blob without a name); {"T", p, label, a, angle} labels centred at p, or attached at a in the direction angle. *)
(* the circle through three points: {centre, radius}, or None if they are (nearly) on one line *)
circleThrough[p1_, p2_, p3_] := Module[{a = p2 - p1, b = p3 - p1, d},
   d = 2 (a[[1]] b[[2]] - a[[2]] b[[1]]);
   If[Abs[d] < 0.1 vectorNorm[a] vectorNorm[b] + 10.^-12, None,
    With[{c = p1 + {b[[2]] (a . a) - a[[2]] (b . b), a[[1]] (b . b) - b[[1]] (a . a)}/d}, {c, vectorNorm[c - p1]}]]];

(* bubbles (closed loops of two vertices, of two propagators or of two interaction lines) joined by one interaction line at
   each vertex to two points further apart than the bubble is wide (a dressed interaction line hanging from a fermion
   line): {{point node, bubble vertex, centre, radius}, ...}, the circle through the two points and the centre of the
   bubble, along which the lines are bent *)
bubbleBends[lines_, point_, legEnds_List : {}] := Module[{nonSelf = Select[lines, #[[1]] =!= #[[2]] &], out = {}, linesAt, clearQ},
   linesAt[b_, pr_] := Select[nonSelf, MemberQ[#[[{1, 2}]], b] && Sort[#[[{1, 2}]]] =!= pr &];
   (* the bent line leaves its outer point a, towards b, along the circle {c, r}: not within 45 degrees of another line
      or leg there (it would run into the arrow of a leg) *)
   clearQ[a_, b_, c_] := With[{pa = point[a]}, With[{tg = With[{t = rotate90[unitVectorOf[pa - c]]}, If[t . (point[b] - pa) < 0, -t, t]]},
      AllTrue[Join[Table[point[First[DeleteCases[l[[{1, 2}]], a]]] - pa, {l, Select[nonSelf, MemberQ[#[[{1, 2}]], a] && ! MemberQ[#[[{1, 2}]], b] &]}],
        Cases[legEnds, {a, e_} :> e - pa]], vectorNorm[#] < 10.^-6 || Abs[angleDifference[angleOf[#], angleOf[tg]]] > 45. Degree &]]];
   Do[With[{b1 = pr[[1]], b2 = pr[[2]], bw = Select[nonSelf, Sort[#[[{1, 2}]]] === pr &], o1 = linesAt[pr[[1]], pr], o2 = linesAt[pr[[2]], pr]},
     If[Length[bw] == 2 && interactionStyleQ[bw[[1, 4]]] === interactionStyleQ[bw[[2, 4]]] && Length[o1] == 1 && Length[o2] == 1 &&
       interactionStyleQ[o1[[1, 4]]] && interactionStyleQ[o2[[1, 4]]],
      With[{i1 = DeleteCases[o1[[1, {1, 2}]], b1], i2 = DeleteCases[o2[[1, {1, 2}]], b2]},
       If[Length[i1] == 1 && Length[i2] == 1 && i1[[1]] =!= i2[[1]] && ! MemberQ[pr, i1[[1]]] && ! MemberQ[pr, i2[[1]]],
        With[{pa1 = point[i1[[1]]], pa2 = point[i2[[1]]], pb1 = point[b1], pb2 = point[b2]},
         If[(pa2 - pa1) . unitVectorOf[pb2 - pb1] > vectorNorm[pb2 - pb1] + 0.3,
          With[{circ = circleThrough[pa1, (pb1 + pb2)/2, pa2]},
           If[circ =!= None && clearQ[i1[[1]], b1, circ[[1]]] && clearQ[i2[[1]], b2, circ[[1]]],
            out = Join[out, {{i1[[1]], b1, circ[[1]], circ[[2]]}, {i2[[1]], b2, circ[[1]], circ[[2]]}}]]]]]]]]],
    {pr, DeleteDuplicates[Sort /@ nonSelf[[All, {1, 2}]]]}];
   out];

frameItems[frameNodes_, positions_, lines_, blobs_, stubs_, ext_, amputatedExt_, legDirections_, d_, mode_, vertexLabels_] := Module[
   {rules = Rule @@@ positions, point, centre = mode[[-1]], items = {}, placed = {}, frameLines, nonLoops, loops, spokes, groups, skips,
    skipsAbove, skipsBelow, skipBase, diameterQ, graphDiameterQ, threeDiameters, diameterCount, bends, ports, sourceEnds, bubbleGroupQ, lensLimit, outsideChords, loopChordCircles, linePointsForLabels,
    nodeIndex, directions, add, kind, uv, es2, offsets, pu, pv, path, p, th, angles, label, cycle, backbone, legs, loopDiscs},
   If[mode[[1]] === "cycle", centre = mode[[3]]];
   point[v_] := Replace[v, rules];
   nodeIndex[v_] := First[First[Position[frameNodes, v, {1}]]];
   frameLines = Select[lines, MemberQ[frameNodes, #[[1]]] &];
   ports = Flatten[Cases[blobs, {_, l_, h_} /; declaredBlobHeadQ[h] :> l]];
   nonLoops = Select[frameLines, #[[1]] =!= #[[2]] &]; loops = Select[frameLines, #[[1]] === #[[2]] &];
   (* the spokes of a blob to its ports, hidden by it; to a point shared with another blob, or twice a port of it, from
      its port (blobPortCorner) *)
   spokes = Flatten[Table[With[{x = b[[2, k]]}, {Which[MemberQ[positions[[All, 1]], blobPortCorner[b[[1]], x, k]], blobPortCorner[b[[1]], x, k],
        MemberQ[positions[[All, 1]], blobPortCorner[b[[1]], x]], blobPortCorner[b[[1]], x], True, b[[1]]], x, None, "Spoke"}],
      {b, Select[blobs, MemberQ[frameNodes, #[[1]]] &]}, {k, Length[b[[2]]]}], 1];
   directions = Table[{v, {}}, {v, frameNodes}];
   add[v_, a_] := With[{k = Position[frameNodes, v, {1}]}, If[k =!= {}, directions[[k[[1, 1]], 2]] = Append[directions[[k[[1, 1]], 2]], a]]];
   (* external legs *)
   legs = Select[amputatedExt, MemberQ[frameNodes, #[[2]]] &];
   Do[With[{q = point[l[[2]]], e = point[leg[l[[1]]]]},
      AppendTo[items, {"L", q, e, l[[1]] /. legDirections}]; add[l[[2]], angleOf[e - q]]], {l, legs}];
   bends = Join[bubbleBends[frameLines, point, Table[{l[[2]], point[leg[l[[1]]]]}, {l, legs}]], If[mode[[1]] === "blob", mode[[3]], {}]];
   groups = GatherBy[Join[nonLoops, spokes], Sort[#[[{1, 2}]]] &];
   (* lines joining non-adjacent vertices of the axis: arcs above it, higher for later ones *)
   skips = If[mode[[1]] === "backbone",
     With[{bb0 = mode[[2]]}, With[{ix = First[First[Position[bb0, #, {1}]]] &},
       SortBy[Select[SortBy[#, ix] & /@ Cases[groups[[All, 1, {1, 2}]], {u_, v_} /; MemberQ[bb0, u] && MemberQ[bb0, v]],
         Abs[ix[#[[2]]] - ix[#[[1]]]] > 1 &], {ix[#[[2]]] - ix[#[[1]]], ix[#[[1]]]} &]]], {}];
   (* arcs drawn below the axis (crossing lines of two-point functions), and those above; each set numbered apart *)
   skipsBelow = If[mode[[1]] === "backbone", Select[skips, MemberQ[mode[[4]], Sort[#]] &], {}];
   skipsAbove = Select[skips, ! MemberQ[skipsBelow, #] &];
   (* the bulge of each arc, in units of its chord: higher for later (longer) ones; two arcs on one side that cross over
      less than half of the span of one of them are round, so that they cross high above the axis at a wide angle
      (flat arcs crossed in a knot just above it); an arc over another one stays above it *)
   skipBase = If[mode[[1]] =!= "backbone", {},
     With[{ix = First[First[Position[mode[[2]], #, {1}]]] &},
      Flatten[Table[Module[{base, spans = Table[ix /@ s, {s, set}], dds = Table[vectorNorm[point[s[[2]]] - point[s[[1]]]], {s, set}]},
         base = Table[0.26 + 0.1 (i - 1), {i, Length[set]}];
         Do[If[i != j && spansCrossQ[spans[[i]], spans[[j]]] &&
             Min[spans[[i, 2]], spans[[j, 2]]] - Max[spans[[i, 1]], spans[[j, 1]]] <= (spans[[i, 2]] - spans[[i, 1]])/2,
            base[[i]] = Max[base[[i]], 0.5]], {i, Length[set]}, {j, Length[set]}];
         Do[If[spans[[i, 1]] <= spans[[j, 1]] && spans[[j, 2]] <= spans[[i, 2]] && dds[[i]] > 10.^-6,
            base[[i]] = Max[base[[i]], (base[[j]] dds[[j]] + 0.3)/dds[[i]]]], {i, Length[set]}, {j, i - 1}];
         Table[set[[i]] -> base[[i]], {i, Length[set]}]], {set, {skipsAbove, skipsBelow}}]]]];
   diameterQ[pq_] := mode[[1]] === "cycle" && MemberQ[mode[[2]], pq[[1]]] && MemberQ[mode[[2]], pq[[2]]] &&
     segmentDistance[centre, {point[pq[[1]]], point[pq[[2]]]}] < 0.08;
   diameterCount = Count[Table[diameterQ[es[[1, {1, 2}]]], {es, groups}], True];
   (* two chords of a round loop (single interaction lines between vertices that are not neighbours on it) that cross each
      other: one inside the loop, the other as an arc outside it (the rule of the arcs above and below an axis) *)
   outsideChords = {};
   (* of three diameters (opposite vertices of a loop of six), the horizontal one (layoutVacuumCycle) *)
   graphDiameterQ[pq_] := mode[[1]] === "cycle" && Length[mode[[2]]] == 6 && MemberQ[mode[[2]], pq[[1]]] && MemberQ[mode[[2]], pq[[2]]] &&
     Abs[First[First[Position[mode[[2]], pq[[1]], {1}]]] - First[First[Position[mode[[2]], pq[[2]], {1}]]]] == 3;
   threeDiameters = mode[[1]] === "cycle" && Count[Table[graphDiameterQ[es[[1, {1, 2}]]], {es, groups}], True] == 3;
   If[threeDiameters,
    outsideChords = Select[Table[Sort[es[[1, {1, 2}]]], {es, groups}], graphDiameterQ[#] && Abs[(point[#[[1]]] - point[#[[2]]])[[2]]] < 0.05 &, 1]];
   If[mode[[1]] === "cycle" && diameterCount < 3,
    With[{cyc = mode[[2]]}, With[{ix = First[First[Position[cyc, #, {1}]]] &, L = Length[mode[[2]]]},
      Module[{chords = Select[Sort /@ groups[[All, 1, {1, 2}]], MemberQ[cyc, #[[1]]] && MemberQ[cyc, #[[2]]] &&
           Mod[ix[#[[2]]] - ix[#[[1]]], L] != 1 && Mod[ix[#[[1]]] - ix[#[[2]]], L] != 1 &], inside = {}, interleaved},
       interleaved[c1_, c2_] := With[{a1 = Min[ix /@ c1], b1 = Max[ix /@ c1], a2 = ix[c2[[1]]], b2 = ix[c2[[2]]]},
         Intersection[c1, c2] === {} && Xor[a1 < a2 < b1, a1 < b2 < b1]];
       (* with three or more chords the layout of the cycle has already moved their crossings apart *)
       If[Length[chords] > 2, chords = {}];
       (* the chord drawn outside is the one that bulges most nearly up or down, away from the weight on the left *)
       chords = SortBy[chords, Function[ch, With[{pu = point[ch[[1]]], pv = point[ch[[2]]]}, With[{m = (pu + pv)/2},
            Abs[If[vectorNorm[m - centre] > 0.05, unitVectorOf[m - centre], rotate90[unitVectorOf[pv - pu]]][[2]]]]]]];
       Do[Which[
         ! AnyTrue[inside, interleaved[ch, #] &], AppendTo[inside, ch],
         ! AnyTrue[outsideChords, interleaved[ch, #] &], AppendTo[outsideChords, ch],
         True, AppendTo[inside, ch]], {ch, chords}]]]]];
   (* the same for a round loop hanging from a line or a point (not the cycle of a vacuum diagram): of two crossing chords,
      the one whose middle is farthest from where the loop is joined is drawn outside it *)
   loopChordCircles = {};
   Do[With[{lp = cs[[1]], cc = cs[[2]], rr = cs[[3]]},
     If[! (mode[[1]] === "cycle" && Sort[lp] === Sort[mode[[2]]]),
      Module[{chords, ixl, joinedAt, away, between},
       ixl[v_] := First[First[Position[lp, v, {1}]]];
       chords = Sort /@ Select[groups, Length[#] == 1 && interactionStyleQ[#[[1, 4]]] && #[[1, 1]] =!= #[[1, 2]] &&
            MemberQ[lp, #[[1, 1]]] && MemberQ[lp, #[[1, 2]]] && ! consecutiveOnLoopQ[lp, #[[1, 1]], #[[1, 2]]] &][[All, 1, {1, 2}]];
       between[c1_, c2_] := With[{a1 = Min[ixl /@ c1], b1 = Max[ixl /@ c1]},
         Intersection[c1, c2] === {} && Xor[a1 < ixl[c2[[1]]] < b1, a1 < ixl[c2[[2]]] < b1]];
       If[Length[chords] == 2 && between[chords[[1]], chords[[2]]],
        joinedAt = Select[lp, Function[v, AnyTrue[frameLines, (#[[1]] === v && ! MemberQ[lp, #[[2]]]) || (#[[2]] === v && ! MemberQ[lp, #[[1]]]) &] ||
            AnyTrue[legs, #[[2]] === v &]]];
        away = If[joinedAt === {} || vectorNorm[cc - Mean[point /@ joinedAt]] < 10.^-6, {0., 1.}, unitVectorOf[cc - Mean[point /@ joinedAt]]];
        With[{ch = Last[SortBy[chords, Function[c, With[{m = (point[c[[1]]] + point[c[[2]]])/2},
               If[vectorNorm[m - cc] > 0.05 rr, unitVectorOf[m - cc] . away, 0.]]]]]},
         AppendTo[outsideChords, ch]; AppendTo[loopChordCircles, {ch, cc, rr, away}]]]]]],
    {cs, $loopCircles}];
   (* the largest angle at which the arcs of a bubble {u, v} may leave its vertices: half the angle to another bubble at
      one of them, a little less than the angle to a single line or leg that would run into the bubble or leave along
      its arc (closer to the chord than a right angle, or about at a right angle: a leg beside a semicircle) *)
   bubbleGroupQ[g_] := Count[g, {_, _, _, s_} /; s =!= "Spoke" && ! interactionStyleQ[s]] >= 2;
   lensLimit[uv0_] := Module[{lim = Infinity, single},
     Do[With[{a = ab[[1]], th0 = angleOf[point[ab[[2]]] - point[ab[[1]]]]},
       (* the signed angles from the chord of the single lines and legs at a *)
       single = {};
       Do[If[Sort[g[[1, {1, 2}]]] =!= Sort[uv0] && MemberQ[g[[1, {1, 2}]], a],
         With[{w = First[DeleteCases[g[[1, {1, 2}]], a]]},
          With[{sd = angleDifference[angleOf[point[w] - point[a]], th0]},
           If[bubbleGroupQ[g], lim = Min[lim, Abs[sd]/2 - 0.05], AppendTo[single, sd]]]]],
        {g, groups}];
       Do[If[l[[2]] === a, AppendTo[single, angleDifference[angleOf[point[leg[l[[1]]]] - point[a]], th0]]], {l, legs}];
       (* a line at about a right angle to the chord on one side only (a leg beside a semicircle) limits the lens too; with
          lines on both sides (a loop sitting on a line) the bubble stays round *)
       Do[Which[Abs[sd] < Pi/2 - 0.05, lim = Min[lim, Abs[sd] - 0.15],
         Abs[sd] < Pi/2 + 0.3 && ! AnyTrue[single, # sd < 0 && Abs[Abs[#] - Pi/2] < 0.3 &], lim = Min[lim, Abs[sd] - 0.3]],
        {sd, single}]],
      {ab, {uv0, Reverse[uv0]}}];
     lim];
   Do[
    (* the end that is not a node of the frame (the port of a blob at a shared point) first *)
    uv = Sort[es[[1, {1, 2}]], If[MemberQ[frameNodes, #1], If[MemberQ[frameNodes, #2], nodeIndex[#1] < nodeIndex[#2], False], True] &];
    kind = "plain";
    Switch[mode[[1]],
     "backbone", backbone = mode[[2]];
     If[! mode[[3]] && MemberQ[backbone, uv[[1]]] && MemberQ[backbone, uv[[2]]],
      uv = SortBy[uv, First[First[Position[backbone, #, {1}]]] &];
      kind = Which[
        Abs[First[First[Position[backbone, uv[[1]], {1}]]] - First[First[Position[backbone, uv[[2]], {1}]]]] == 1,
        If[MemberQ[mode[[4]], Sort[uv]], "axisBelow", "axis"],
        MemberQ[skipsBelow, uv], {"skip", First[First[Position[skipsBelow, uv, {1}]]], -1., Replace[uv, skipBase]},
        True, {"skip", First[First[Position[skipsAbove, uv, {1}]]], 1., Replace[uv, skipBase]}]],
     "circle", With[{pa = Select[mode[[2]], Sort[#[[1]]] === Sort[uv] &, 1]},
      If[pa =!= {}, uv = pa[[1, 1]]; kind = {"skip", pa[[1, 2]], pa[[1, 3]]}]],
     "cycle", cycle = mode[[2]];
     If[mode[[4]] && MemberQ[cycle, uv[[1]]] && MemberQ[cycle, uv[[2]]],
      With[{i1 = First[First[Position[cycle, uv[[1]], {1}]]], i2 = First[First[Position[cycle, uv[[2]], {1}]]], L = Length[cycle]},
       Which[
        Mod[i2 - i1, L] == 1, kind = {"cycle", 0.},
        Mod[i1 - i2, L] == 1, uv = Reverse[uv]; kind = {"cycle", 0.}]]]];
    (* four or more diameters of the cycle would meet at its centre: they are bent, all in the same sense *)
    If[kind === "plain" && diameterCount >= 4 && diameterQ[uv],
     If[With[{a = angleOf[point[uv[[2]]] - point[uv[[1]]]]}, a - 2. Pi Floor[a/(2. Pi)]] >= Pi - 10.^-6, uv = Reverse[uv]];
     kind = {"bend", 0.17 vectorNorm[point[uv[[2]]] - point[uv[[1]]]]}];
    pu = point[uv[[1]]]; pv = point[uv[[2]]];
    If[kind === "plain" && MemberQ[outsideChords, Sort[uv]] && Length[es] == 1,
     With[{lc = Select[loopChordCircles, #[[1]] === Sort[uv] &, 1]},
     With[{nrm = rotate90[unitVectorOf[pv - pu]], mid = (pu + pv)/2, rc = If[lc =!= {}, lc[[1, 3]], mode[[5]]],
       cc = If[lc =!= {}, lc[[1, 2]], centre], up = If[lc =!= {}, lc[[1, 4]], {0., 1.}]},
      With[{dm = nrm . (mid - cc)}, With[{side = Which[Abs[dm] > 0.05, Sign[dm], nrm . up < -10.^-6, -1., True, 1.]},
        (* 0.4 outside the circle at its middle, more for a long chord, so that a long arc does not hug the circle *)
        kind = {"bend", side (rc - Abs[dm] + 0.4 + 0.3 Min[1., vectorNorm[pv - pu]/(2 rc)] + If[threeDiameters && lc === {}, 0.2, 0.])}]]]]];
    (* lines between neighbouring vertices of a round fermion loop follow its circle *)
    With[{cs = Select[$loopCircles, consecutiveOnLoopQ[#[[1]], uv[[1]], uv[[2]]] &, 1]},
     If[cs =!= {}, kind = If[Length[cs[[1, 1]]] == 2 && Length[es] == 2, bubbleCircleKind[pu, pv, cs[[1]]],
        circleKind[pu, pv, cs[[1]], Table[point[w], {w, Complement[cs[[1, 1]], uv]}]]]]];
    (* one propagator and interaction lines: the propagator straight, the others bulging away from the centre *)
    If[kind === "plain" && Count[es, {_, _, _, s_} /; ! interactionStyleQ[s]] == 1 && Count[es, {_, _, _, s_} /; interactionStyleQ[s]] >= 1,
     kind = "axis";
     If[rotate90[pv - pu] . ((pu + pv)/2 - centre) < 0, uv = Reverse[uv]; {pu, pv} = {pv, pu}]];
    If[ListQ[kind] && kind[[1]] === "cycle",
     (* the line follows the circle through the cycle vertices, outwards *)
     With[{r = mode[[5]], c = vectorNorm[pv - pu]}, With[{sg = r - Sqrt[Max[r^2 - c^2/4, 0.]]},
       kind = {"cycle", If[vectorNorm[(pu + pv)/2 + sg rotate90[unitVectorOf[pv - pu]] - centre] > vectorNorm[(pu + pv)/2 - centre], sg, -sg]}]]];
    (* the line from a point to a bubble that hangs between two points further apart than its width: bent along the
       circle through the two points and the centre of the bubble *)
    With[{bd = Select[bends, Sort[#[[{1, 2}]]] === Sort[uv] &, 1]},
     If[bd =!= {} && Length[es] == 1,
      uv = bd[[1, {1, 2}]]; pu = point[uv[[1]]]; pv = point[uv[[2]]];
      With[{c = vectorNorm[pv - pu], rr = bd[[1, 4]], cc = bd[[1, 3]]},
       With[{s = rr - Sqrt[Max[0., rr^2 - c^2/4]]},
        kind = {"bend", If[rotate90[unitVectorOf[pv - pu]] . ((pu + pv)/2 - cc) > 0, s, -s]}]]]];
    (* a line that would cross a declared blob is bent around it *)
    If[mode[[1]] === "blob" && kind === "plain" && Length[es] == 1 && es[[1, 4]] =!= "Spoke" &&
      ! (MemberQ[ports, uv[[1]]] && MemberQ[ports, uv[[2]]]),
     kind = blobAvoidingKind[pu, pv, uv, mode[[2]]]];
    (* a line between two ports of one blob (the interaction of a Fock term closed on L), both ends and its middle on
       the blob, would run along its side or across it: bent outwards, away from its centre, 0.35 outside the blob *)
    If[mode[[1]] === "blob" && kind === "plain" && Length[es] == 1 && MemberQ[ports, uv[[1]]] && MemberQ[ports, uv[[2]]],
     With[{own = Select[mode[[2]], insideBlobQ[pu, #, 0.05] && insideBlobQ[pv, #, 0.05] && insideBlobQ[(pu + pv)/2, #, 0.05] &, 1], nrm = rotate90[unitVectorOf[pv - pu]]},
      If[own =!= {}, With[{tc = (own[[1, 1]] - (pu + pv)/2) . nrm, hb = own[[1, 3]]},
        With[{reach = Switch[own[[1, 2]], "Box", hb (Abs[nrm[[1]]] + Abs[nrm[[2]]]), "Triangle" | "TriangleUp", 1.45 hb, "Diamond", 1.3 hb, _, hb]},
         kind = {"bend", If[tc > 10.^-6, -1., 1.] (Max[0., reach - Abs[tc]] + 0.35)}]]]]];
    {es2, offsets} = bundleOffsets[es, vectorNorm[pv - pu], kind];
    (* a bubble next to another bubble, or with other lines leaving its vertices on its sides (the bubble of a triangle):
       made flatter, a lens, so that it overlaps neither *)
    If[(kind === "plain" || MemberQ[{"axis", "axisBelow"}, kind] && ! AnyTrue[es2, interactionStyleQ[#[[4]]] &]) &&
      Length[es2] >= 2 && Max[Abs[offsets]] > 0,
     With[{lim = lensLimit[uv]}, If[lim < Infinity,
       With[{omax = vectorNorm[pv - pu] Tan[Max[lim, 0.25]/2]/2}, If[omax < Max[Abs[offsets]], offsets = offsets omax/Max[Abs[offsets]]]]]]];
    Do[path = arcThrough[pu, pv, offsets[[j]]];
     If[es2[[j, 1]] =!= uv[[1]], path = reversedPath[path]];
     AppendTo[items, {"E", path, es2[[j, 4]]}];
     With[{ea = pathEndAngles[path]}, add[es2[[j, 1]], ea[[1]]]; add[es2[[j, 2]], ea[[2]]]], {j, Length[es2]}],
    {es, groups}];
   (* self-loops and one-label factors, in the largest free sectors that lead outside the loops and bubbles drawn (a
      tadpole on a side of a triangle outside it); a loop that would touch one already drawn (two tadpoles on
      neighbouring vertices of a short leg) goes to another sector nearly as large, if there is one *)
   loopDiscs = {};
   Do[With[{v = l[[1]]},
     p = point[v]; angles = directions[[nodeIndex[v], 2]];
     th = outsideGapDirection[p, angles, preferredDirection[p, centre],
       Flatten[Cases[items, {"E", path_, _} :> Partition[pathPoints[path, N[Range[0, 24]]/24, 0.], 2, 1]], 1], 0.3];
     If[loopDiscs =!= {} && loopClearance[selfLoopPath[p, th, l[[4]], angles], loopDiscs] < 0.2,
      With[{best = Last[SortBy[largeGapDirections[angles, preferredDirection[p, centre], 0.8],
          loopClearance[selfLoopPath[p, #, l[[4]], angles], loopDiscs] &]]},
       If[loopClearance[selfLoopPath[p, best, l[[4]], angles], loopDiscs] > loopClearance[selfLoopPath[p, th, l[[4]], angles], loopDiscs] + 0.05,
        th = best]]];
     AppendTo[items, {"E", selfLoopPath[p, th, l[[4]], angles], l[[4]]}];
     AppendTo[loopDiscs, loopDisc[selfLoopPath[p, th, l[[4]], angles]]];
     directions[[nodeIndex[v], 2]] = Join[angles, {th, th + 0.7, th - 0.7}]], {l, loops}];
   (* a source at the end of a single line (a diagram of Z[J] at a non-zero source) is drawn at the point itself, without
      a stub; any other source insertion on a short stub *)
   sourceEnds = Select[DeleteDuplicates[Cases[Select[stubs, MemberQ[frameNodes, #[[1]]] &], {u_, _} :> u]],
     Count[stubs, {#, _}] == 1 && ! MemberQ[ext, #] && FreeQ[loops, {#, __}] &&
       Count[frameLines, {a_, b_, _, _} /; a =!= b && (a === # || b === #)] == 1 &];
   (* several means on a vertex with lines (the mass insertion lambda phibar^2/2 on a loop) are drawn together, spread
      evenly in the free sector closest to the outward direction (a V on a loop, a cross with a single line), so that none
      is inside a loop; on a vertex without lines they are spread around it *)
   Module[{seen = {}, grouped = {}},
    Do[With[{v = s[[1]], k = Count[stubs, {s[[1]], _}]},
      p = point[v];
      Which[
       MemberQ[sourceEnds, v], AppendTo[items, {"S", {p, p}}],
       MemberQ[grouped, v], Null,
       k >= 2 && ! MemberQ[seen, v] && directions[[nodeIndex[v], 2]] =!= {},
        AppendTo[grouped, v];
        With[{gap = outwardGap[directions[[nodeIndex[v], 2]], preferredDirection[p, centre]]},
         With[{dd = gap[[2]]/(k + 1)},
          Do[th = gap[[1]] + (i - (k + 1)/2) dd;
           AppendTo[items, {"S", {p, p + $stubLength directionAt[th]}}]; add[v, th], {i, k}]]],
       True,
        angles = directions[[nodeIndex[v], 2]];
        (* the largest free sector, unless the stub would end inside a loop or a bubble (the mean on a vertex of a bubble
           on the axis): then the largest sector that leaves it outside *)
        With[{segs = Flatten[Cases[items, {"E", path_, _} :> Partition[pathPoints[path, N[Range[0, 24]]/24, 0.], 2, 1]], 1]},
         th = outsideGapDirection[p, angles, preferredDirection[p, centre], segs, $stubLength]];
        AppendTo[items, {"S", {p, p + $stubLength directionAt[th]}}];
        add[v, th]];
      AppendTo[seen, v]], {s, Select[stubs, MemberQ[frameNodes, #[[1]]] &]}]];
   (* nodes and labels; the labels are kept off the lines *)
   linePointsForLabels = Flatten[Cases[items, {"E", path_, _} :> pathPoints[path, N[Range[0, 24]]/24, 0.]], 1];
   Do[If[Head[v] =!= leg,
     p = point[v];
     Which[
      MemberQ[$densityNodes, v], AppendTo[items, {"S", {p, p}}],
      MemberQ[ext, v] && ! MemberQ[amputatedExt[[All, 2]], v], AppendTo[items, {"N", p, "ext"}],
      (* the shape and size of the blob as laid out: a turned box is a diamond *)
      Head[v] === blob && declaredBlobHeadQ[blobHeadOf[v, blobs]], AppendTo[items, {"B", p, With[{sp = blobSpecOf[blobHeadOf[v, blobs]],
          dk = If[mode[[1]] === "blob", Select[mode[[2]], vectorNorm[#[[1]] - p] < 10.^-6 &, 1], {}]}, If[dk === {}, sp, ReplacePart[sp, {2 -> dk[[1, 2]], 4 -> dk[[1, 3]]}]]]}],
      Head[v] === blob, AppendTo[items, {"N", p, unnamedBlobKind[blobHeadOf[v, blobs]]}],
      (* a port of a declared blob, on its boundary, joined to at most one line: no dot (a point twice a port of a blob is
         outside it, joined to two of its corners) *)
      MemberQ[ports, v] && Count[frameLines, {a_, b_, _, _} /; a =!= b && (a === v || b === v)] <= 1 &&
       FreeQ[positions[[All, 1]], blobPortCorner[_, v, _]], Null,
      MemberQ[sourceEnds, v], Null,
      True, AppendTo[items, {"N", p, "dot"}]];
     label = Which[
       (* a density point carries its label, also when it is the point of the bare vertex *)
       MemberQ[$densityNodes, v] && vertexLabels =!= None, Replace[v, $densityNodeLabels],
       MemberQ[ext, v] && ! MemberQ[amputatedExt[[All, 2]], v] && vertexLabels =!= None, v,
       ! MemberQ[ext, v] && Head[v] =!= blob && vertexLabels === All, v,
       True, None];
     If[label =!= None,
      (* the preferred free sector whose label box does not overlap the labels already placed *)
      With[{wh = labelBox[label], r0 = Which[MemberQ[$densityNodes, v], 0.1, MemberQ[ext, v], $externalPointRadius, True, $dotRadius] + 0.07 +
          If[MemberQ[Cases[loops, {v0_, __} :> v0], v], 0.12, 0.]},
       With[{ts = gapDirections[directions[[nodeIndex[v], 2]], preferredDirection[p, centre], 0.5]},
        With[{cs = Table[p + (r0 + Abs[Cos[t]] wh[[1]]/2 + Abs[Sin[t]] wh[[2]]/2) directionAt[t], {t, ts}]},
         (* the first candidate that overlaps neither a label nor a line, else one that overlaps no label *)
         With[{k = With[{ok = Select[Range[Length[cs]], Function[j, ! AnyTrue[placed, boxesOverlapQ[{cs[[j]], wh}, #] &]]]},
             With[{clear = Select[ok, Function[j, ! AnyTrue[linePointsForLabels, Abs[#[[1]] - cs[[j, 1]]] < wh[[1]]/2 + 0.07 &&
                     Abs[#[[2]] - cs[[j, 2]]] < wh[[2]]/2 + 0.07 &]], 1]},
              Which[clear =!= {}, First[clear], ok =!= {}, First[ok], True, 1]]]},
          AppendTo[placed, {cs[[k]], wh}];
          (* {"T", centre of the label, label, point the label is attached to, direction in degrees} *)
          AppendTo[items, {"T", cs[[k]], label, p + (r0 + 0.02) directionAt[ts[[k]]], N[ts[[k]]/Degree]}]]]]]]],
    {v, frameNodes}];
   (* labels at the ends of the legs *)
   If[vertexLabels =!= None,
    Do[With[{q = point[l[[2]]], e = point[leg[l[[1]]]]}, With[{wh = labelBox[l[[1]]], u = unitVectorOf[e - q]},
       AppendTo[items, {"T", e + (0.08 + Abs[u[[1]]] wh[[1]]/2 + Abs[u[[2]]] wh[[2]]/2) u, l[[1]], e + 0.09 u, N[ArcTan[u[[1]], u[[2]]]/Degree]}]]],
     {l, legs}]];
   items];

lineCoarsePoints[path_] := With[{k = If[path[[1]] === "segment", 1, Max[4, Ceiling[Abs[path[[5]]]/0.4]]]}, pathPoints[path, N[Range[0, k]]/k, 0.]];
itemPoints[items_] := Join[Flatten[Cases[items, {"E", path_, _} :> lineCoarsePoints[path]], 1], Flatten[Cases[items, {"S", q_} :> q], 1],
   Flatten[Cases[items, {"L", p_, q_, _} :> {p, q}], 1],
   Flatten[Cases[items, {"T", p_, label_, ___} :> With[{b = labelBox[label]/2}, {p - b, p + b}]], 1],
   Flatten[Cases[items, {"N", p_, k_} :> With[{r = If[unnamedBlobKindQ[k], $blobRadius, $externalPointRadius]}, {p - {r, r}, p + {r, r}}]], 1],
   Flatten[Cases[items, {"B", p_, spec_} :> {p - {spec[[4]], spec[[4]]}, p + {spec[[4]], spec[[4]]}}], 1]];
boundingBox[items_] := With[{pts = itemPoints[items]}, If[pts === {}, {{-0.5, 0.5}, {-0.5, 0.5}},
    {{Min[pts[[All, 1]]], Max[pts[[All, 1]]]}, {Min[pts[[All, 2]]], Max[pts[[All, 2]]]}}]];
shiftItems[items_, v_] := items /. {x_?NumberQ, y_?NumberQ} :> {x + v[[1]], y + v[[2]]};

(* the general layout puts the source points on a circle: one at the end of a single line that is nearly vertical or
   nearly horizontal is moved along the circle's tangent so that the line is exactly vertical or horizontal *)
straightenedSourceLines[positions_, sources_, pairs_] := Module[{pos = positions, rules, nb},
   Do[rules = Rule @@@ pos;
    nb = DeleteDuplicates[Join[Cases[pairs, {s, w_} :> w], Cases[pairs, {w_, s} :> w]]];
    If[Length[nb] == 1 && ! MemberQ[sources, nb[[1]]], With[{ps = Replace[s, rules], pv = Replace[nb[[1]], rules]},
      With[{dd = ps - pv}, Which[
        Abs[dd[[1]]] > 10.^-6 && Abs[dd[[1]]] < 0.27 Abs[dd[[2]]], pos = Replace[pos, {s, _} :> {s, {pv[[1]], ps[[2]]}}, {1}],
        Abs[dd[[2]]] > 10.^-6 && Abs[dd[[2]]] < 0.27 Abs[dd[[1]]], pos = Replace[pos, {s, _} :> {s, {ps[[1]], pv[[2]]}}, {1}]]]]],
    {s, sources}];
   pos];

(* ---------------------------------------------------------------- the corners of a four-point picture *)
(* The four external labels are at the corners counterclockwise from the top left (top left, bottom left, bottom right,
   top right), in the order given: the external labels of the picture, or "ExternalOrder" -> {x1, ...}. A diagram whose
   channel joins two opposite corners is drawn crossed: its layout exchanges the labels of one side (the general layout and
   the rails of fermion lines the right ones, a tree of two vertex functions joined by a line is turned upright, a blob
   picture has the corners of its ports), and the two legs are put back, crossing as an X just outside the picture.
   "ExternalOrder" -> "Uncrossed" (or {"Uncrossed", {x1, ...}}) takes instead, per diagram, the first of the order, the
   order with the labels on the right exchanged and the order with the labels on the top exchanged (the three cyclic orders
   of four corners) whose picture has fewest crossings, the labels moving with their legs. Result: {order, uncrossed} *)
externalOrderSpec[eo_, labels_List] := With[{arrange = Function[l, Join[Select[l, MemberQ[labels, #] &], Select[labels, ! MemberQ[l, #] &]]]},
   Which[
    eo === "Uncrossed", {labels, True},
    MatchQ[eo, {"Uncrossed", _List}], {arrange[eo[[2]]], True},
    ListQ[eo], {arrange[eo], False},
    True, {labels, False}]];
(* the orders with the labels of one side exchanged: {order, the two labels, outward normal of the side} for the right,
   top, bottom and left sides *)
cornerExchanges[o_List] := {{o[[{1, 2, 4, 3}]], o[[{3, 4}]], {1., 0.}}, {o[[{4, 2, 3, 1}]], o[[{1, 4}]], {0., 1.}},
   {o[[{1, 3, 2, 4}]], o[[{2, 3}]], {0., -1.}}, {o[[{2, 1, 3, 4}]], o[[{1, 2}]], {-1., 0.}}};
(* the labels at the corners top left, bottom left, bottom right and top right of a layout: the ends of their legs (or their
   points) in the four quadrants around their mean; None if two are in one quadrant *)
cornerArrangement[positions_, labels_, labelNodes_, amputated_] := Module[{ends, m, q},
   ends = Table[Replace[If[amputated[[k]], leg[labels[[k]]], labelNodes[[k]]], Rule @@@ positions], {k, Length[labels]}];
   If[! MatrixQ[ends, NumberQ], Return[None]];
   m = Mean[ends];
   q = Table[With[{dd = e - m}, Which[dd[[1]] < -10.^-6 && dd[[2]] > 10.^-6, 1, dd[[1]] < -10.^-6 && dd[[2]] < -10.^-6, 2,
       dd[[1]] > 10.^-6 && dd[[2]] < -10.^-6, 3, dd[[1]] > 10.^-6 && dd[[2]] > 10.^-6, 4, True, 0]], {e, ends}];
   If[Sort[q] =!= {1, 2, 3, 4}, None, labels[[Ordering[q]]]]];
(* the crossings of the lines of a layout taken straight, its legs included; two nodes at one place count as two *)
layoutCrossingCount[positions_, lines_, labels_, labelNodes_, amputated_] := Module[{rules = Rule @@@ positions, segs, pts},
   segs = DeleteDuplicates[Join[Cases[lines, {u_, v_, _, _} /; u =!= v :> Sort[{u, v}]],
      Table[{labelNodes[[k]], leg[labels[[k]]]}, {k, Select[Range[Length[labels]], amputated[[#]] &]}]]];
   segs = Select[Replace[segs, rules, {2}], MatrixQ[#, NumberQ] &];
   pts = Cases[positions, {v_, p_} /; Head[v] =!= leg :> p];
   mutualCrossingCount[segs] + 2 Count[Flatten[Table[vectorNorm[pts[[i]] - pts[[j]]] < 0.05, {i, Length[pts]}, {j, i + 1, Length[pts]}]], True]];
(* the legs (or the lines to the points) of the labels la, lb, exchanged by the layout on the side of the outward normal n,
   put back: their ends exchanged (each at the height, along the side, of the vertex of the other leg) and moved out along n
   until the two lines cross as an X $legCrossingGap outside everything else drawn with the labels exchanged (items; a
   bubble the legs then leave towards is flattened into a lens, and the X is a little further out). The X is at about a
   right angle; where both legs leave blobs, or vertices of bubbles towards the bubbles (which they flatten: a chain of
   bubbles), the lines cross at $crowdedCrossingAngle, further out: the X stands clear of the blobs and the lenses are
   fuller *)
$legCrossingGap = 0.3;
$crowdedCrossingAngle = 60 Degree;
(* the node at the end of the leg of the label l (leg[l], or its point) and the node where the leg starts (the vertex of the
   label, or the other end of the single line at its point; None if several lines end there) *)
legEndAndStart[l_, labels_, labelNodes_, amputated_, lines_] := With[{k = First[First[Position[labels, l, {1}]]]}, With[{v = labelNodes[[k]]},
    Which[
     amputated[[k]], {leg[l], v},
     Count[labelNodes, v] == 1 && Count[lines, {v, _, _, _} | {_, v, _, _}] == 1, {v, First[Cases[lines, {v, w_, _, _} | {w_, v, _, _} :> w]]},
     True, {v, None}]]];
(* the lines drawn (items) that the legs of the labels la, lb cross, other than each other *)
crossedLegDefects[items_, positions_, {la_, lb_}, labels_, labelNodes_, amputated_, lines_] := Module[{rules = Rule @@@ positions, se, legs, ends, others},
   se = Table[legEndAndStart[l, labels, labelNodes, amputated, lines], {l, {la, lb}}];
   If[MemberQ[se[[All, 2]], None], Return[0]];
   legs = Replace[se[[All, {2, 1}]], rules, {2}]; ends = legs[[All, 2]];
   If[! MatrixQ[Flatten[legs, 1], NumberQ], Return[0]];
   others = Flatten[Table[With[{pts = lineCoarsePoints[it[[2]]]},
       If[AnyTrue[ends, Function[e, AnyTrue[pts, vectorNorm[# - e] < 0.02 &]]], {}, Partition[pts, 2, 1]]], {it, Cases[items, {"E", __}]}], 1];
   Total[Table[crossingsWith[{lg[[1]] + 0.08 unitVectorOf[lg[[2]] - lg[[1]]], lg[[2]]}, others], {lg, legs}]]];
crossedLegPositions[positions_, items_, {la_, lb_}, n_, labels_, labelNodes_, amputated_, lines_] := Module[
   {rules = Rule @@@ positions, endOf, vertexOf, pa, pb, qa, qb, t = {-n[[2]], n[[1]]}, near, reachOf, crossAt, solve, lo,
    ta, tb, placed, crowdedQ},
   endOf[l_] := legEndAndStart[l, labels, labelNodes, amputated, lines][[1]];
   vertexOf[l_] := legEndAndStart[l, labels, labelNodes, amputated, lines][[2]];
   pa = Replace[endOf[la], rules]; pb = Replace[endOf[lb], rules];
   (* a label at a vertex of its own (several lines end there): the two vertices change places, their lines crossing *)
   If[vertexOf[la] === None || vertexOf[lb] === None,
    Return[If[endOf[la] =!= endOf[lb] && Count[labelNodes, endOf[la]] == 1 && Count[labelNodes, endOf[lb]] == 1 && MatrixQ[{pa, pb}, NumberQ],
      Replace[positions, {{endOf[la], _} -> {endOf[la], pb}, {endOf[lb], _} -> {endOf[lb], pa}}, {1}], positions]]];
   qa = Replace[vertexOf[la], rules]; qb = Replace[vertexOf[lb], rules];
   If[! MatrixQ[{pa, pb, qa, qb}, NumberQ] || vectorNorm[qa - qb] < 0.05, Return[positions]];
   (* the ends change places along the side: each at the height of the other vertex, at least 0.6 apart *)
   {tb, ta} = With[{u = {t . qa, t . qb}, s = Sign[t . pa - t . pb]}, With[{m = Mean[u], h = Max[Abs[u[[1]] - u[[2]]]/2, 0.3]},
      If[s >= 0, {m + h, m - h}, {m - h, m + h}]]];
   placed[xx_] := Replace[positions, {{endOf[la], _} -> {endOf[la], xx n + ta t}, {endOf[lb], _} -> {endOf[lb], xx n + tb t}}, {1}];
   near[p_, e_] := vectorNorm[p - e] < 0.02;
   (* how far out along n everything else reaches, the ends of the two lines at e1, e2 *)
   reachOf[its_, e1_, e2_] := Max[Join[{n . qa, n . qb}, (n . #) & /@ itemPoints[Select[its, Function[it, Switch[it[[1]],
          "L", ! (near[it[[3]], e1] || near[it[[3]], e2]),
          "T", ! MemberQ[{la, lb}, it[[3]]],
          "N", ! (near[it[[2]], e1] || near[it[[2]], e2]),
          "E", ! AnyTrue[lineCoarsePoints[it[[2]]], near[#, e1] || near[#, e2] &],
          _, True]]]]]];
   (* how far out along n the two lines cross, with their ends at xx along n; -Infinity if they do not *)
   crossAt[xx_] := With[{a = xx n + ta t - qa, b = xx n + tb t - qb}, With[{det = a[[2]] b[[1]] - a[[1]] b[[2]]},
      If[Abs[det] < 10.^-9, -Infinity,
       With[{s = ((qb - qa)[[2]] b[[1]] - (qb - qa)[[1]] b[[2]])/det, r = (a[[1]] (qb - qa)[[2]] - a[[2]] (qb - qa)[[1]])/det},
        If[0 < s < 1 && 0 < r < 1, n . (qa + s a), -Infinity]]]]];
   lo = Max[n . qa, n . qb] + 0.2;
   (* a leg that leaves a blob, or a vertex of a bubble (two or more propagators to one vertex) towards the bubble *)
   crowdedQ[l_, q_, tEnd_] := With[{v = vertexOf[l], dir = n + Sign[tEnd - t . q] t},
     Head[v] === blob || AnyTrue[DeleteDuplicates[Cases[lines, {v, w_, _, s_} | {w_, v, _, s_} /; w =!= v && ! interactionStyleQ[s] :> w]],
       Function[w, Count[lines, {v, w, _, s_} | {w, v, _, s_} /; ! interactionStyleQ[s]] >= 2 && MatrixQ[{Replace[w, rules]}, NumberQ] &&
         (Replace[w, rules] - q) . dir > 0.05]]];
   (* the nearest ends whose lines cross $legCrossingGap beyond the reach r, and far enough beyond the vertices to cross
      at about a right angle, or at $crowdedCrossingAngle *)
   solve[r_] := Module[{a = lo, b = lo + 0.5, target = Max[r + $legCrossingGap, Max[n . qa, n . qb] +
         If[crowdedQ[la, qa, ta] && crowdedQ[lb, qb, tb], 0.5/Tan[$crowdedCrossingAngle/2], 0.45] Abs[ta - tb]]},
     Which[
      crossAt[a] >= target, a,
      crossAt[lo + 8.] === -Infinity, lo + 0.4,
      True,
      While[crossAt[b] < target && b < lo + 8., b = b + 0.5];
      Do[With[{mid = (a + b)/2}, If[crossAt[mid] < target, a = mid, b = mid]], {20}];
      b]];
   placed[solve[reachOf[items, pa, pb]]]];

(* geometry of a canonical term, with its diagramElements: {weight, items, bounding box} *)
diagramGeometry[t_diagramTerm, elements_List, ext0_List, opts_List, legDirections_] := Block[
   {$pictureBlobHeads = Union[$sumBlobHeads, Cases[elements[[3]], {_, _, h_} :> h]], $pictureBoxFloor = $sumBoxFloor},
   $pictureBoxFloor = Max[$sumBoxFloor, boxFloorOf[elements]];
   diagramGeometryOfPicture[t, elements, ext0, opts, legDirections]];
diagramGeometryOfPicture[t_diagramTerm, elements_List, ext0_List, opts_List, legDirections_] := Module[{vertexLabels = drawingOption[opts, "VertexLabels"], weight,
    lines, blobs, stubs, classes, rules, d = t[[3]], ext, present, externalNodes, amputated, amputatedExt, dummies, nodes, pairs, propagatorPairs,
    components, mainComponents, vacuumComponents, frames = {}, positions, mode, items, bb, x0, groups, circles, sourceLayout, unitQ, straighten, multiplePairs,
    axisFrame},
   vertexLabels = Which[vertexLabels === None || vertexLabels === False, None, vertexLabels === All || vertexLabels === True, All, True, Automatic];
   {weight, lines, blobs, stubs, classes, rules} = elements;
   {lines, blobs} = attachLinesToBlobs[lines, blobs, stubs, d];
   (* the points that appear in the picture (lines, blobs, source insertions, or a point made of several labels) *)
   present = Union[Flatten[{Cases[lines, {u_, v_, _, _} :> {u, v}], Cases[blobs, {b_, l_, _} :> Join[{b}, l]], Cases[stubs, {u_, _} :> u],
       Cases[classes, cl_List /; Length[cl] > 1 :> (cl /. rules)]}]];
   ext = Select[ext0, MemberQ[present, # /. rules] &];
   externalNodes = (# /. rules) & /@ ext;
   amputated = Table[amputatedLabelQ[e, lines, blobs, stubs, classes, opts] && ! MemberQ[$densityLabels, e], {e, ext}];
   amputatedExt = Join @@ Table[If[amputated[[k]], {{ext[[k]], externalNodes[[k]]}}, {}], {k, Length[ext]}];
   dummies = Select[DeleteDuplicates[(# /. rules) & /@ d], MemberQ[present, #] && ! MemberQ[externalNodes, #] &];
   nodes = DeleteDuplicates[Join[externalNodes, dummies, Cases[blobs, {b_, _, _} :> b]]];
   propagatorPairs = DeleteDuplicates[Join[Cases[lines, {u_, v_, _, s_} /; u =!= v && ! interactionStyleQ[s] :> Sort[{u, v}]],
      Flatten[Table[Sort[{b[[1]], x}], {b, blobs}, {x, b[[2]]}], 1]]];
   pairs = DeleteDuplicates[Join[propagatorPairs, Cases[lines, {u_, v_, _, _} /; u =!= v :> Sort[{u, v}]]]];
   With[{all = Cases[lines, {u_, v_, _, _} /; u =!= v :> Sort[{u, v}]]}, multiplePairs = Select[DeleteDuplicates[all], Count[all, #] >= 2 &]];
   components = {};
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, pairs]]], {v, nodes}];
   components = Table[Select[nodes, MemberQ[c, #] &], {c, components}];
   mainComponents = Select[components, Intersection[#, externalNodes] =!= {} &];
   vacuumComponents = Select[components, Intersection[#, externalNodes] === {} &];
   If[ext =!= {},
    (* the part with the external labels is one picture; a product of two one-point functions (e.g. <phi(x)> <phi(y)>)
       is drawn as two pictures side by side *)
    groups = If[Length[ext] == 2 && Length[mainComponents] == 2,
      Table[{{k}, First[Select[mainComponents, MemberQ[#, externalNodes[[k]]] &]]}, {k, 2}],
      {{Range[Length[ext]], Flatten[mainComponents]}}];
    Do[With[{ks = g[[1]], mainNodes = g[[2]]},
      Module[{layoutOf, itemsOf, spec, kind, res, count, best, bestCount, arrangement, exchange, frameLines},
       (* the layout of the part with the external labels, with the labels in the order ord: {positions, mode, circles, kind} *)
       layoutOf[ord_] := Module[{blob, unit, pos, md, cs},
         blob = blobLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines, blobs];
         unit = If[blob =!= None, None, Replace[unitLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines, ord],
            None :> treeLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines, blobs, False, ord]]];
         Which[
          blob =!= None, Append[blob, "blob"],
          unit =!= None, Append[unit, "unit"],
          True,
          {pos, md} = Block[{$multiplePairs = multiplePairs, $selfLoopNodes = Cases[lines, {u_, u_, _, _} :> u]}, layoutWithExternals[ext[[ks]],
             externalNodes[[ks]], amputated[[ks]], Select[mainNodes, ! MemberQ[externalNodes, #] &],
             Select[pairs, MemberQ[mainNodes, #[[1]]] &], Select[propagatorPairs, MemberQ[mainNodes, #[[1]]] &], ord]];
          {pos, cs} = roundFermionLoops[pos, lines, md, externalNodes[[ks]], Select[pairs, MemberQ[mainNodes, #[[1]]] &]];
          {pos, md, cs, "general"}]];
       itemsOf[pos_, md_, cs_] := Block[{$loopCircles = cs, $densityNodes = (# /. rules) & /@ Select[ext[[ks]], MemberQ[$densityLabels, #] &],
          $densityNodeLabels = Table[(e /. rules) -> e, {e, Select[ext[[ks]], MemberQ[$densityLabels, #] &]}]},
         frameItems[Join[Select[nodes, MemberQ[mainNodes, #] &], leg /@ Select[amputatedExt, MemberQ[ext[[ks]], #[[1]]] &][[All, 1]]],
          pos, lines, blobs, stubs, externalNodes, amputatedExt, legDirections, d, md, vertexLabels]];
       exchange = None;
       If[Length[ks] == 4,
        (* four external points: the labels at the corners counterclockwise from the top left, in the order given *)
        spec = externalOrderSpec[drawingOption[opts, "ExternalOrder"], ext[[ks]]];
        frameLines = Select[lines, MemberQ[mainNodes, #[[1]]] &];
        If[spec[[2]],
         (* "Uncrossed": the first of the order, the order with the legs on the right exchanged and the order with the legs on
            the top exchanged (the three cyclic orders) whose picture has fewest crossings; the labels go with their legs *)
         best = None; bestCount = Infinity;
         Do[res = layoutOf[c];
          count = layoutCrossingCount[res[[1]], frameLines, ext[[ks]], externalNodes[[ks]], amputated[[ks]]];
          If[count < bestCount, best = res; bestCount = count];
          If[bestCount == 0 || res[[4]] === "blob", Break[]], {c, {spec[[1]], cornerExchanges[spec[[1]]][[1, 1]], cornerExchanges[spec[[1]]][[2, 1]]}}];
         {positions, mode, circles, kind} = best,
         (* a layout that keeps its lines from crossing by exchanging the labels of one side (a channel that joins opposite
            corners): the two legs put back, crossing as an X outside the picture *)
         {positions, mode, circles, kind} = layoutOf[spec[[1]]];
         arrangement = cornerArrangement[positions, ext[[ks]], externalNodes[[ks]], amputated[[ks]]];
         If[arrangement =!= None && arrangement =!= spec[[1]],
          exchange = SelectFirst[cornerExchanges[spec[[1]]], #[[1]] === arrangement &, None]]],
        {positions, mode, circles, kind} = layoutOf[With[{eo = drawingOption[opts, "ExternalOrder"]},
           If[ListQ[eo] && ! MatchQ[eo, {"Uncrossed", _}], eo, Automatic]]]];
       items = itemsOf[positions, mode, circles];
       If[exchange =!= None,
        Module[{crossed, tried = {}, cand, defects, lay, best = None},
         (* the legs crossed on the side of the exchange; if they cross other lines (two vertices on opposite sides of a
            loop), on the other sides, the labels of that side exchanged in the layout, and the one with fewest such
            crossings *)
         (* a loop drawn as a circle through the vertex of a crossed leg, which would leave it across the circle, is drawn
            with straight lines (a polygon) when that crosses fewer lines *)
         crossed[lay0_, ex_] := Module[{attempt, res, starts, cs},
           attempt[cs0_] := Module[{its = itemsOf[lay0[[1]], lay0[[2]], cs0], pos, its2},
             pos = crossedLegPositions[lay0[[1]], its, ex[[2]], ex[[3]], ext[[ks]], externalNodes[[ks]], amputated[[ks]], frameLines];
             its2 = itemsOf[pos, lay0[[2]], cs0];
             {pos, lay0[[2]], cs0, its2, crossedLegDefects[its2, pos, ex[[2]], ext[[ks]], externalNodes[[ks]], amputated[[ks]], frameLines]}];
           res = attempt[lay0[[3]]];
           If[res[[5]] > 0,
            starts = Table[legEndAndStart[l, ext[[ks]], externalNodes[[ks]], amputated[[ks]], frameLines][[2]], {l, ex[[2]]}];
            cs = Select[lay0[[3]], Intersection[#[[1]], starts] === {} &];
            If[cs =!= lay0[[3]], With[{res2 = attempt[cs]}, If[res2[[5]] < res[[5]], res = res2]]]];
           res];
         best = crossed[{positions, mode, circles}, exchange]; AppendTo[tried, exchange[[1]]];
         If[best[[5]] > 0 && kind =!= "blob",
          Do[If[! MemberQ[tried, ex[[1]]],
            lay = layoutOf[ex[[1]]];
            If[cornerArrangement[lay[[1]], ext[[ks]], externalNodes[[ks]], amputated[[ks]]] === ex[[1]],
             cand = crossed[lay, ex];
             If[cand[[5]] < best[[5]], best = cand]];
            If[best[[5]] == 0, Break[]]], {ex, cornerExchanges[spec[[1]]]}]];
         {positions, mode, circles, items} = best[[1 ;; 4]]]];
       AppendTo[frames, {items, True}]]], {g, groups}]];
   Do[unitQ = False; straighten = {}; axisFrame = False; With[{unitLayout = unitLayoutVacuum[c, lines],
      (* the source insertions at the ends of single lines (the diagrams of Z[J] at a non-zero source): placed like the
         external points of a correlation function, on the axis or at the corners *)
      sourcePoints = Select[DeleteDuplicates[Cases[stubs, {u_, _} :> u]], MemberQ[c, #] && Count[Flatten[pairs], #] == 1 &&
          Count[stubs, {#, _}] == 1 &],
      (* the means on vertices (Gamma2[Psibar, Psi, G]): a fermion line between two means drawn like the open line of a
         self-energy, the means on its axis *)
      meanVertices = Select[DeleteDuplicates[Cases[stubs, {u_, _} :> u]], MemberQ[c, #] && Count[Flatten[pairs], #] > 1 &&
          Count[stubs, {#, _}] == 1 &]},
     Which[
      sourcePoints === {} && 2 <= Length[meanVertices] <= 8 &&
       (sourceLayout = meanUnitLayout[meanVertices, c, lines]) =!= None,
      {positions, mode, circles} = sourceLayout; unitQ = True; axisFrame = True,
      sourcePoints =!= {} && Length[sourcePoints] <= 8 &&
       (sourceLayout = unitLayoutWithExternals[sourcePoints, sourcePoints, Table[False, {Length[sourcePoints]}], c, lines, Automatic]) =!= None,
      {positions, mode, circles} = sourceLayout; unitQ = True; axisFrame = True,
      sourcePoints =!= {} && Length[sourcePoints] <= 8,
      {positions, mode} = layoutWithExternals[sourcePoints, sourcePoints, Table[False, {Length[sourcePoints]}],
        Select[c, ! MemberQ[sourcePoints, #] &], Select[pairs, MemberQ[c, #[[1]]] &], Select[propagatorPairs, MemberQ[c, #[[1]]] &],
        Automatic];
      {positions, circles} = roundFermionLoops[positions, lines, mode, sourcePoints, Select[pairs, MemberQ[c, #[[1]]] &]]; straighten = sourcePoints,
      (* a tree of vertex functions or bare vertices whose leaves are free ends (its labels integrated over): drawn as
         the trees with labelled leaves are, the free ends where the external points would be, its centre on the axis *)
      (sourceLayout = freeEndTreeLayout[c, lines, blobs, stubs, pairs]) =!= None,
      {positions, mode, circles} = sourceLayout; unitQ = True; axisFrame = True,
      unitLayout =!= None, {positions, mode, circles} = unitLayout; unitQ = True,
      True,
      {positions, mode} = Block[{$blobRadii = Table[b[[1]] -> If[blobSpecOf[b[[3]]] =!= None, blobSizeOfHead[b[[3]]], $blobRadius], {b, blobs}]},
        layoutVacuumComponent[c, Select[pairs, MemberQ[c, #[[1]]] &], Select[propagatorPairs, MemberQ[c, #[[1]]] &], lines]];
      {positions, circles} = If[mode[[1]] === "loops", {positions, mode[[2]]},
        roundFermionLoops[positions, lines, mode, {}, Select[pairs, MemberQ[c, #[[1]]] &]]]]];
    (* the unit layouts are upright by construction (open lines horizontal, rails parallel): only the general layout is turned *)
    If[! unitQ, {positions, circles} = alignPrincipalAxes[positions, circles, Select[lines, MemberQ[c, #[[1]]] && MemberQ[c, #[[2]]] &]]];
    If[straighten =!= {}, positions = straightenedSourceLines[positions, straighten, Select[pairs, MemberQ[c, #[[1]]] &]]];
    AppendTo[frames, {Block[{$loopCircles = circles}, frameItems[c, positions, lines, blobs, stubs, externalNodes, {}, {}, d, mode, vertexLabels]],
      axisFrame}], {c, vacuumComponents}];
   (* frames side by side: the parts with external labels, and the open lines between sources or means, keep their axis
      (y = 0), the other parts are centred on it, the first one too, so that equal pieces are drawn at the same height *)
   items = {}; x0 = None;
   Do[bb = boundingBox[f[[1]]];
    If[x0 === None,
     items = shiftItems[f[[1]], {0., If[f[[2]] || Length[frames] == 1, 0., -Mean[bb[[2]]]]}]; x0 = bb[[1, 2]],
     items = Join[items, shiftItems[f[[1]], {x0 + 0.5 - bb[[1, 1]], If[f[[2]], 0., -Mean[bb[[2]]]]}]]; x0 = x0 + 0.5 + bb[[1, 2]] - bb[[1, 1]]],
    {f, frames}];
   {weight, items, boundingBox[items]}];

(* the unit layout of a fermion line or of two rails between means (Gamma2[Psibar, Psi, G]); with four means, of the
   orders of the means at the corners the first one whose straight lines cross least (the corners are free here: the
   two rails of a ring of two Fock vertices antiparallel, a box, not an X of two wavy lines) *)
meanUnitLayout[means_, c_, lines_] := Module[{best = None, bestCount = Infinity, res, count, frameLines},
   If[Length[means] =!= 4, Return[unitLayoutWithExternals[means, means, Table[True, {Length[means]}], c, lines, Automatic]]];
   frameLines = Select[lines, MemberQ[c, #[[1]]] && MemberQ[c, #[[2]]] && #[[1]] =!= #[[2]] &];
   Do[res = unitLayoutWithExternals[o, o, Table[True, {4}], c, lines, o];
    If[res =!= None,
     count = With[{pt = Rule @@@ res[[1]]}, mutualCrossingCount[Table[{l[[1]] /. pt, l[[2]] /. pt}, {l, frameLines}]]];
     If[count < bestCount, best = res; bestCount = count]];
    If[bestCount == 0, Break[]], {o, Prepend[DeleteCases[Permutations[means], means], means]}];
   best];

(* a picture without external points turned so that the principal axes of its vertices are horizontal and vertical
   (by less than 45 degrees): a symmetric diagram laid out at an angle is drawn upright; nothing is turned when the
   vertices have no preferred axis *)
alignPrincipalAxes[positions_, circles_, lines_] := Module[{pts = Cases[positions, {v_, q_} /; Head[v] =!= leg :> q], m, dd, cxx, cyy, cxy,
    phi, rot, point, m0},
   If[Length[pts] < 3, Return[{positions, circles}]];
   (* the vertices, the middle of every line, and a point outside every self-loop *)
   point[v_] := Replace[v, Rule @@@ positions]; m0 = Mean[pts];
   pts = Join[pts, Table[If[l[[1]] === l[[2]], With[{q = point[l[[1]]]}, q + 0.5 If[vectorNorm[q - m0] < 0.05, {0., 1.}, unitVectorOf[q - m0]]],
       (point[l[[1]]] + point[l[[2]]])/2], {l, lines}]];
   m = Mean[pts]; dd = Table[q - m, {q, pts}];
   cxx = Total[dd[[All, 1]]^2]; cyy = Total[dd[[All, 2]]^2]; cxy = Total[dd[[All, 1]] dd[[All, 2]]];
   If[Sqrt[(cxx - cyy)^2 + 4 cxy^2] < 0.15 (cxx + cyy) + 10.^-9, Return[{positions, circles}]];
   phi = -ArcTan[cxx - cyy, 2 cxy]/2; phi = phi - Pi/2 Round[phi/(Pi/2)];
   If[Abs[phi] < 0.05, Return[{positions, circles}]];
   rot[q_] := m + {Cos[phi] (q - m)[[1]] - Sin[phi] (q - m)[[2]], Sin[phi] (q - m)[[1]] + Cos[phi] (q - m)[[2]]};
   {Table[{q[[1]], rot[q[[2]]]}, {q, positions}], Table[{q[[1]], rot[q[[2]]], q[[3]]}, {q, circles}]}];

(* ---------------------------------------------------------------- graphics primitives *)
labelPrimitive[label_, p_] := Text[Style[If[Head[label] === DummyVariable, Subscript["z", label[[2]]], label], $fontPixels, Italic], p];

(* parameter of the arrow on a directed line: the middle, unless another arrow is already close to it or another line
   crosses it there *)
arrowParameter[path_, used_, others_ : {}, blobs_ : {}] := Module[{freeQ, c, boxes},
   (* a line whose bounding box is farther than 0.15 from the point, in x or in y, is farther at every segment *)
   boxes = Table[{Min[o[[All, 1]]], Max[o[[All, 1]]], Min[o[[All, 2]]], Max[o[[All, 2]]]}, {o, others}];
   freeQ[t_] := With[{q = First[pathPoints[path, {t}, 0.]]},
     AllTrue[used, vectorNorm[# - q] > 0.25 &] && ! AnyTrue[blobs, insideBlobQ[q, #, 0.12] &] &&
      AllTrue[Range[Length[others]], Function[i, With[{b = boxes[[i]], o = others[[i]]},
        Max[b[[1]] - q[[1]], q[[1]] - b[[2]], b[[3]] - q[[2]], q[[2]] - b[[4]]] > 0.15 + 10.^-9 ||
         Min[Table[segmentDistance[q, {o[[k]], o[[k + 1]]}], {k, Length[o] - 1}]] > 0.15]]]];
   c = Select[{0.5, 0.3, 0.7, 0.4, 0.6}, freeQ, 1];
   If[c === {}, 0.5, First[c]]];
(* blobs: {centre, shape, half size} of the declared blobs, which hide what is under them *)
arrowParameters[items_, blobs_ : {}] := Module[{used = {}, ts = {}, t, path, coarse = Table[lineCoarsePoints[it[[2]]], {it, items}]},
   Do[If[directedStyleQ[items[[i, 3]]], path = items[[i, 2]];
     t = arrowParameter[path, used, Delete[coarse, i], blobs]; AppendTo[used, First[pathPoints[path, {t}, 0.]]];
     AppendTo[ts, t], AppendTo[ts, 0.5]], {i, Length[items]}];
   ts];

(* the half size of the cross on an impurity line *)
$impurityCross = 0.13;
crossAt[q_, size_] := {Line[{q + size {1, 1}, q - size {1, 1}}], Line[{q + size {1, -1}, q - size {1, -1}}]};

linePrimitives[{"E", path_, style_}, t_ : 0.5] := Switch[style,
   "Wavy", Line[pathWavy[path]],
   (* a dressed interaction (the screened W): two parallel waves *)
   "DoubleWavy", {Line[pathWavy[path, 0.03]], Line[pathWavy[path, -0.03]]},
   "Curly", Line[pathCurly[path]],
   "DottedArrow", {AbsolutePointSize[2.2], Point[pathDots[path]], Polygon[pathArrowHead[path, t]]},
   "Dashed", Line[pathDashes[path]],
   "Dotted", {AbsolutePointSize[2.2], Point[pathDots[path]]},
   "Zigzag", Line[pathZigzag[path]],
   "Impurity", {Line[pathDashes[path, 0.13]], crossAt[First[pathPoints[path, {0.5}, 0.]], $impurityCross]},
   "Arrow", {Line[pathPolyline[path]], Polygon[pathArrowHead[path, t]]},
   "DoubleArrow", {Line[pathPolyline[path, 0.035]], Line[pathPolyline[path, -0.035]], Polygon[pathArrowHead[path, t]]},
   "Double", {Line[pathPolyline[path, 0.035]], Line[pathPolyline[path, -0.035]]},
   "DivergingArrows" | "ConvergingArrows", {Line[pathPolyline[path]], Polygon[pathArrowHead[#[[1]], #[[2]]]] & /@ anomalousArrowHeads[path, style]},
   _, Line[pathPolyline[path]]];
(* the two arrowheads of an anomalous line, as {path, parameter}: pointing to the two ends at 0.3 and 0.7 (diverging), or
   to the middle at 0.25 and 0.75 (converging), the tips kept apart *)
anomalousArrowHeads[path_, "DivergingArrows"] := {{path, 0.7}, {reversedPath[path], 0.7}};
anomalousArrowHeads[path_, "ConvergingArrows"] := {{path, 0.25}, {reversedPath[path], 0.25}};

legPrimitives[{"L", q_, e_, direction_}] := With[{path = If[direction === "In", {"segment", e, q}, {"segment", q, e}]},
   If[direction === "In" || direction === "Out", {Line[{q, e}], Polygon[pathArrowHead[path, 0.55]]}, Line[{q, e}]]];

(* a: the appearance (diagramAppearance): every colour is that of the light or the dark notebook *)
itemPrimitives[items_, opts_List, a_ : Automatic] := With[{fg = diagramColor[a, "Foreground"], paper = diagramColor[a, "Paper"]},
  {AbsoluteThickness[With[{w = drawingOption[opts, "LineWidth"]}, If[NumberQ[w] && w > 0, w, 1.3]]], fg,
   With[{es = Cases[items, {"E", __}]}, MapThread[linePrimitives[#1, #2] &, {es, arrowParameters[es, Cases[items, {"B", p_, spec_} :> {p, spec[[2]], spec[[4]]}]]}]],
   legPrimitives /@ Cases[items, {"L", __}],
   Cases[items, {"S", {p_, q_}} :> {Line[{p, q}], {EdgeForm[{fg, AbsoluteThickness[1.]}], paper, Disk[q, 0.1]},
      AbsoluteThickness[1.], crossAt[q, 0.07]}],
   Cases[items, {"N", p_, k_?unnamedBlobKindQ} :> declaredBlobPrimitives[p, unnamedBlobSpec[k], a]],
   Cases[items, {"B", p_, spec_} :> declaredBlobPrimitives[p, spec, a]],
   Cases[items, {"N", p_, "dot"} :> Disk[p, $dotRadius]],
   Cases[items, {"N", p_, "ext"} :> {EdgeForm[{fg, AbsoluteThickness[1.]}], paper, Disk[p, $externalPointRadius]}],
   Cases[items, {"T", p_, label_, ___} :> labelPrimitive[label, p]]}];

weightPrimitive[w_, p_] := Text[Style[w, $fontPixels + 1], p, {1, 0}];

(* the Graphics of a picture; with a forced appearance ("Light" or "Dark") on a background of the paper colour, else
   transparent (the notebook shows through) *)
finishGraphics[primitives_, bb_, opts_List, a_ : Automatic] := Module[{x1 = bb[[1, 1]] - 0.12, x2 = bb[[1, 2]] + 0.12, y1 = bb[[2, 1]] - 0.12,
    y2 = bb[[2, 2]] + 0.12, imageSize = drawingOption[opts, "ImageSize"], pixelsPerUnit},
   pixelsPerUnit = If[NumberQ[imageSize], N[imageSize/(x2 - x1)], $pixelsPerUnit];
   Graphics[primitives, PlotRange -> {{x1, x2}, {y1, y2}}, ImageSize -> Round[pixelsPerUnit (x2 - x1)],
    AspectRatio -> Automatic, PlotRangePadding -> None, If[a === Automatic, Sequence @@ {}, Background -> diagramColor[a, "Paper"]]]];

(* the canonical terms in drawing order: fewer integration variables first *)
termsForDrawing[e_] := With[{ts = canonicalTermsOf[e]}, ts[[Ordering[Length[#[[3]]] & /@ ts]]]];

(* the weight written with a picture: the coefficient of the term divided by the values of its elements, in which an
   interaction line (DeclareInteractionLine, or the option "Interactions") stands for minus its kernel, a local vertex
   for minus its kernel and a vertex for minus its coupling (DeclareCoupling, or the option "Couplings": lambda^n ->
   (-1)^n), the signs of the
   expansion of exp(-S_int); "Weights" -> "Coefficients" gives the coefficient itself *)
interactionLineHeadQ[h_, opts_List] := MemberQ[$interactionLineHeads, h] || MemberQ[Flatten[{drawingOption[opts, "Interactions"]}], h];
displayedWeight[t_diagramTerm, elements_List, opts_List] := If[drawingOption[opts, "Weights"] === "Coefficients", elements[[1]],
   (-1)^(Count[elements[[2]], {_, _, h_, _} /; interactionLineHeadQ[h, opts]] +
       Count[expandIntegerPowers[Join[factorList[t[[1]]], t[[2]]]], f_ /; localVertexQ[f]]) (elements[[1]] /.
      Table[c -> -1, {c, Union[$couplingSymbols, Flatten[{drawingOption[opts, "Couplings"]}]]}])];

(* an anomalous line (-<T psi psi>, -<T psibar psibar>) or an antisymmetric blob of two labels (the gap function) shows
   no orientation, so the weight takes it read along the fermion line it lies on: the chains of fermion lines (directed
   or anomalous) and of blobs of two labels, a loop in either sense (it holds an even number of such elements, so the
   sense does not matter), an open line from its end at the first external label (else from its first end; the sense
   matters only when both ends are of one kind, psi psi or psibar psibar). The canonical term orients them by the order
   of its labels: the sign converts one reading into the other. Chains that branch are left as they are. *)
anomalousOrientationSign[elements_List, ext_List] := Module[{lines = elements[[2]], blobs = elements[[3]], rules = elements[[6]],
    edges, extNodes, sign = 1, done = {}, compNodes, compEdges, degree, ends, start, v, prev, next, k, grow},
   edges = Join[
     Cases[lines, {u_, w_, h_, st_} /; u =!= w && fermionStyleQ[st] :> {u, w, anomalousStyleQ[st] && MemberQ[$antisymmetricHeads, h]}],
     Cases[blobs, {_, {u_, w_}, h_} /; u =!= w :> {u, w, MemberQ[$antisymmetricHeads, h]}]];
   If[FreeQ[edges, {_, _, True}], 1,
    extNodes = (# /. rules) & /@ ext;
    degree[x_] := Count[Flatten[edges[[All, {1, 2}]]], x];
    Do[If[edges[[i, 3]] && ! MemberQ[done, i],
      (* the component of the edge i *)
      compNodes = {edges[[i, 1]], edges[[i, 2]]}; grow = True;
      While[grow, With[{more = Union[Flatten[Select[edges[[All, {1, 2}]], Intersection[#, compNodes] =!= {} &]]]},
        grow = Length[more] > Length[compNodes]; compNodes = more]];
      compEdges = Select[Range[Length[edges]], MemberQ[compNodes, edges[[#, 1]]] &];
      done = Join[done, compEdges];
      If[AllTrue[compNodes, degree[#] <= 2 &],
       ends = Select[compNodes, degree[#] == 1 &];
       start = Which[ends === {}, edges[[First[compEdges], 1]],
         Intersection[ends, extNodes] =!= {}, First[Select[extNodes, MemberQ[ends, #] &]],
         True, First[Select[Flatten[edges[[compEdges, {1, 2}]]], MemberQ[ends, #] &]]];
       (* the walk from start: at every node the edge not yet taken *)
       v = start; prev = {};
       While[(next = Select[compEdges, ! MemberQ[prev, #] && MemberQ[edges[[#, {1, 2}]], v] &]) =!= {},
        k = First[next]; AppendTo[prev, k];
        If[edges[[k, 3]] && edges[[k, 1]] =!= v, sign = -sign];
        v = If[edges[[k, 1]] === v, edges[[k, 2]], edges[[k, 1]]]]]], {i, Length[edges]}];
    sign]];

(* a kernel OperatorKernel[h^-1] in a term that also holds lines h (a derivative of a Tr ln or of a 1PI action before
   ExpandTrace): it is drawn as a plain line, like h, and the products h . h^-1, not contracted, look like chains of h *)
FunctionalDiagrams::inversekernel = "OperatorKernel[`1`] is drawn as a line like `2`, and its products with the lines `2` are not contracted: the picture is misleading. Contract them with ExpandTrace first.";
inverseKernelHeads[t_] := Select[Union[Cases[t, OperatorKernel[Power[h_Symbol, -1]] :> h, Infinity, Heads -> True]],
   ! FreeQ[t, #[_, _]] &];
geometriesOf[e_, ext_List, opts_List] := Module[{ts = termsForDrawing[e], elements, directions},
   checkLabels[e, ext];
   Scan[Message[FunctionalDiagrams::inversekernel, ToString[#] <> "^-1", #] &, Union @@ Append[inverseKernelHeads /@ ts, {}]];
   (* the elements of every term, computed once for the three steps that need them *)
   elements = Table[diagramElements[t, ext, opts], {t, ts}];
   directions = legDirectionsFor[elements, ext, opts];
   Block[{$densityLabels = densityLabelsFor[ts, elements, ext, opts, directions],
     $sumBlobHeads = Union @@ Append[Table[Cases[el[[3]], {_, _, h_} :> h], {el, elements}], {}], $sumBoxFloor = 0.},
    $sumBoxFloor = Block[{$pictureBlobHeads = $sumBlobHeads}, Max[0., boxFloorOf /@ elements]];
    Table[ReplacePart[diagramGeometry[ts[[i]], elements[[i]], ext, opts, directions], 1 ->
       anomalousOrientationSign[elements[[i]], ext] displayedWeight[ts[[i]], elements[[i]], opts]], {i, Length[ts]}]]];

DrawDiagram[t_, ext_List : {}, opts___] := Module[{ts = termsForDrawing[t], g, allOpts = drawingOptions[DrawDiagram, {opts}]},
   Which[
    ts === {}, Graphics[{}],
    Length[ts] > 1, DiagramPlot[t, ext, Sequence @@ allOpts],
    True,
    g = First[geometriesOf[t, ext, allOpts]];
    With[{a = diagramAppearance[allOpts]}, finishGraphics[itemPrimitives[g[[2]], allOpts, a], g[[3]], allOpts, a]]]];

DiagramPlot[e_, ext_List : {}, opts___] := Module[{allOpts = drawingOptions[DiagramPlot, {opts}], gs, n, ww, dw, dh, cw, ch, columns,
    cells, bb, a},
   a = diagramAppearance[allOpts];
   gs = geometriesOf[e, ext, allOpts]; n = Length[gs];
   Which[
    n == 0, Graphics[{}],
    drawingOption[allOpts, "Output"] === "Grid",
    Grid[Table[{g[[1]], finishGraphics[itemPrimitives[g[[2]], allOpts, a], g[[3]], allOpts, a]}, {g, gs}], Frame -> All,
     Alignment -> {Left, Center}, If[a === Automatic, Sequence @@ {},
      Sequence @@ {Background -> diagramColor[a, "Paper"], FrameStyle -> diagramColor[a, "CellFrame"],
        BaseStyle -> {FontColor -> diagramColor[a, "Foreground"]}}]],
    True,
    ww = If[TrueQ[drawingOption[allOpts, "Weights"]] || drawingOption[allOpts, "Weights"] === Automatic,
      Max[0.62 $fontPixels StringLength[ToString[#[[1]], InputForm]]/$pixelsPerUnit & /@ gs] + 0.3, 0.];
    dw = Max[(#[[3, 1, 2]] - #[[3, 1, 1]]) & /@ gs]; dh = Max[(#[[3, 2, 2]] - #[[3, 2, 1]]) & /@ gs];
    cw = ww + dw + 0.4; ch = dh + 0.4;
    columns = drawingOption[allOpts, "Columns"];
    (* about 16 units wide, or more for many pictures, so that the grid is not much taller than wide *)
    If[! (IntegerQ[columns] && columns >= 1), columns = Max[1, Min[n, Max[Floor[16./cw], Round[Sqrt[1.5 n ch/cw]]]]]];
    cells = Table[With[{r = Quotient[i - 1, columns], c = Mod[i - 1, columns], g = gs[[i]]},
       With[{x0 = c cw, y0 = -r ch},
        {shiftItems[g[[2]], {x0 + ww + 0.2 + dw/2 - Mean[g[[3, 1]]], y0 - ch/2 - Mean[g[[3, 2]]]}], g[[1]], {x0 + ww, y0 - ch/2},
         {{x0, y0}, {x0 + cw, y0}, {x0 + cw, y0 - ch}, {x0, y0 - ch}, {x0, y0}}}]], {i, n}];
    bb = {{0., Min[n, columns] cw}, {-Ceiling[n/columns] ch, 0.}};
    finishGraphics[{
      {diagramColor[a, "CellFrame"], AbsoluteThickness[0.6], Line[#[[4]]] & /@ cells},
      If[ww > 0, {diagramColor[a, "Foreground"], weightPrimitive[#[[2]], #[[3]]] & /@ cells}, {}],
      itemPrimitives[#[[1]], allOpts, a] & /@ cells}, bb, allOpts, a]]];

(* ========================================================================= *)
(*  15. TikZ export                                                          *)
(*  The same geometry as DiagramPlot, written as TikZ commands. Required:   *)
(*  \usetikzlibrary{decorations.pathmorphing} (straight zigzag lines).       *)
(* ========================================================================= *)

$greekNames = {"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu",
   "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi",
   "Sigma", "Phi", "Psi", "Omega"};

(* x12 -> x_{12}, lambda -> \lambda, lambda2 -> \lambda_{2}, etabar -> \bar{\eta}, G0 -> G_{0} *)
splitTrailingDigits[s_String] := Module[{c = Characters[s], k},
   k = Length[c]; While[k > 0 && DigitQ[c[[k]]], k--];
   If[k == 0 || k == Length[c], {s, ""}, {StringJoin[Take[c, k]], StringJoin[Drop[c, k]]}]];
latexBaseName[b_String] := Which[
   MemberQ[$greekNames, b], "\\" <> b,
   StringLength[b] > 3 && StringTake[b, -3] === "bar" && (MemberQ[$greekNames, StringDrop[b, -3]] || StringLength[b] == 4),
   "\\bar{" <> latexBaseName[StringDrop[b, -3]] <> "}",
   StringLength[b] > 1, "\\mathrm{" <> b <> "}",
   True, b];
(* a name is cut at its first $ (the local symbols of Module, x$123, are written x) *)
cutAtDollar[s_String] := With[{p = Position[Characters[s], "$", {1}, 1]}, If[p === {} || p[[1, 1]] == 1, s, StringTake[s, p[[1, 1]] - 1]]];
latexName[s0_String] := With[{parts = splitTrailingDigits[cutAtDollar[s0]]},
   With[{base = latexBaseName[parts[[1]]]}, If[parts[[2]] === "", base, base <> "_{" <> parts[[2]] <> "}"]]];

latexLabel[DummyVariable[_, k_], rules_] := "z_{" <> ToString[k] <> "}";
latexLabel[x_, rules_] := With[{r = Select[rules, #[[1]] === x &]}, Which[
    r =!= {}, ToString[r[[1, 2]]],
    Head[x] === Symbol, latexSymbolString[x],
    IntegerQ[x], ToString[x],
    True, latexName[ToString[x]]]];

latexNumber[r_Integer] := ToString[r];
latexNumber[r_Rational] := "\\frac{" <> ToString[Numerator[r]] <> "}{" <> ToString[Denominator[r]] <> "}";
(* a complex number: i, -i, 2\,i, \left(1+2\,i\right) *)
latexNumber[z_Complex] := With[{re = Re[z], im = Im[z]}, With[{ims = If[Abs[im] == 1, "i", latexNumber[Abs[im]] <> "\\,i"]},
    Which[re == 0, If[im < 0, "-", ""] <> ims, True, "\\left(" <> latexNumber[re] <> If[im < 0, "-", "+"] <> ims <> "\\right)"]]];
latexNumber[r_] := If[NumberQ[r] && r < 0, "-" <> latexNumber[-r], ToString[r]];
(* a numerical coefficient as {sign, magnitude, factor i}: -2 I -> {-1, 2, "i"}, (1 + I)/2 -> {1, 1, "\left(...\right)"} *)
coefficientParts[c_] := Which[
   Im[c] == 0, {If[c < 0, -1, 1], Abs[c], ""},
   Re[c] == 0, {If[Im[c] < 0, -1, 1], Abs[Im[c]], "i"},
   True, {1, 1, latexNumber[c]}];
(* the factors of a product that are not numbers *)
nonNumericPart[w_] := Which[NumberQ[w], 1, Head[w] === Times, Times @@ Select[List @@ w, ! NumberQ[#] &], True, w];
(* the constants Pi and E (I is a number) *)
latexSymbolString[s_Symbol] := Which[s === Pi, "\\pi", s === E, "e", True, latexName[SymbolName[s]]];

(* LaTeX of a weight: rational number times a product of powers of symbols; anything else via TeXForm *)
latexMonomialFactor[s_Symbol] := latexSymbolString[s];
latexMonomialFactor[Power[s_Symbol, n_Integer]] := latexSymbolString[s] <> "^{" <> ToString[n] <> "}";
latexMonomialFactor[f_] := "(" <> ToString[TeXForm[f]] <> ")";
latexWeight[w_] := Module[{parts = coefficientParts[numericFactor[w]], rest = nonNumericPart[w], numberStrings},
   numberStrings = Join[If[parts[[2]] === 1 && parts[[3]] =!= "", {}, {latexNumber[parts[[2]]]}], If[parts[[3]] === "", {}, {parts[[3]]}]];
   {parts[[1]], If[rest === 1, joinStrings[numberStrings, "\\,"],
     joinStrings[Join[If[numberStrings === {"1"}, {}, numberStrings],
       latexMonomialFactor /@ If[Head[rest] === Times, List @@ rest, {rest}]], "\\,"]]}];

(* a number with three decimals, written out in full (no scientific notation) *)
numberString[x_] := With[{m = Round[1000 N[x]]}, With[{a = Abs[m]}, With[{f = Mod[a, 1000]},
     StringJoin[If[m < 0, "-", ""], ToString[Quotient[a, 1000]], Which[f >= 100, ".", f >= 10, ".0", True, ".00"], ToString[f]]]]];
coordinateString[{x_, y_}] := "(" <> numberString[x] <> "," <> numberString[y] <> ")";
(* the same for the points of a polyline, the numbers rounded all at once *)
coordinateStrings[pts_List] := If[pts === {}, {}, With[{m = Round[1000 N[pts]]}, With[{a = Abs[m]}, With[{q = Quotient[a, 1000], f = Mod[a, 1000]},
     MapThread[StringJoin["(", If[#1 < 0, "-", ""], ToString[#3], Which[#5 >= 100, ".", #5 >= 10, ".0", True, ".00"], ToString[#5], ",",
        If[#2 < 0, "-", ""], ToString[#4], Which[#6 >= 100, ".", #6 >= 10, ".0", True, ".00"], ToString[#6], ")"] &,
      {m[[All, 1]], m[[All, 2]], q[[All, 1]], q[[All, 2]], f[[All, 1]], f[[All, 2]]}]]]]];
degreesString[a_] := numberString[a 180./Pi];

(* lines drawn with tikzPathString are segments and arcs: balloons (self-loops of waves and coils) are drawn through
   computed points by tikzLine *)
tikzPathString[{"segment", u_, v_}] := coordinateString[u] <> " -- " <> coordinateString[v];
(* the start angle is reduced to [0, 360) degrees; the arc is the same *)
tikzPathString[{"arc", c_, r_, a0_, da_}] := With[{a = a0 - 2 Pi Floor[a0/(2 Pi)]}, coordinateString[c + r {Cos[a0], Sin[a0]}] <> " arc[start angle=" <>
    degreesString[a] <> ", end angle=" <> degreesString[a + da] <> ", radius=" <> numberString[r] <> "]"];

(* the line width in pt: "LineWidth" (Automatic: 0.7) *)
tikzLineWidth[opts_List] := With[{w = drawingOption[opts, "LineWidth"]}, If[NumberQ[w] && w > 0, N[w], 0.7]];
(* the length of a unit in cm: "Scale" (anything but a positive number: the default, 0.7) *)
tikzScale[opts_List] := With[{s = drawingOption[opts, "Scale"]}, If[NumberQ[s] && Im[s] == 0 && s > 0, N[s], 0.7]];

(* an arrow head on a path, centred at the parameter t and pointing along the path: a filled stealth shape, 5.5 lw long and
   4.7 lw wide (times size) (in pt, converted to units of the picture). It is a polygon, not a decoration: the markings decoration of
   TikZ fails (Dimension too large) on small arcs *)
tikzArrowHead[path_, t_, scale_, lw_ : 0.7, size_ : 1.] := Module[{q = pathPoints[path, {t - 0.01, t, t + 0.01}, 0.], m, d, n, u, len, wid, back},
   m = q[[2]]; d = unitVectorOf[q[[3]] - q[[1]]]; n = rotate90[d];
   u = 0.0351459804/scale; len = 5.5 size lw u; wid = 4.7 size lw u; back = m - len/2 d;
   "  \\fill " <> coordinateString[m + len/2 d] <> " -- " <> coordinateString[back + wid/2 n] <> " -- " <>
    coordinateString[back + 0.325 len d] <> " -- " <> coordinateString[back - wid/2 n] <> " -- cycle;\n"];

(* waves through computed points: one smooth curve of amplitude amp, nw waves and 8 points per wave, for each offset *)
tikzWaves[path_, nw_, amp_, offsets_List, crossings_List : {}] := With[{ts = N[Range[0, Round[8 nw]]]/Round[8 nw]},
   With[{env = waveEnvelope[ts, crossings, 0.9/nw]},
    StringJoin[Table["  \\draw plot[smooth, tension=0.7] coordinates {" <>
       joinStrings[coordinateStrings[pathPoints[path, ts, ds + amp env Sin[2. Pi nw ts]]], " "] <> "};\n", {ds, offsets}]]]];
(* the amplitude of the waves along a line, 1 but near the parameters cs where it crosses another wavy line: there it
   falls smoothly to 0 within w on either side, so that the crossing reads as two lines crossing, not as a knot *)
waveEnvelope[ts_List, cs_List, w_] := If[cs === {}, Table[1., {Length[ts]}],
   Table[Times @@ Table[With[{x = Min[1., Abs[t - c]/w]}, x^2 (3 - 2 x)], {c, cs}], {t, ts}]];
(* the parameters (fractions of its length) at which each line of es crosses a wavy line, away from the ends of both *)
$crossingWaveStyles = {"Wavy", "DoubleWavy"};
waveCrossingParameters[es_List] := Module[{n = 32, polys},
   polys = Table[If[MemberQ[$crossingWaveStyles, e[[3]]], pathPoints[e[[2]], N[Range[0, n]]/n, 0.], None], {e, es}];
   Table[If[polys[[i]] === None, {},
     Flatten[Table[If[j == i || polys[[j]] === None, {}, polylineCrossings[polys[[i]], polys[[j]], n]], {j, Length[es]}]]], {i, Length[es]}]];
(* the parameters along p where the polylines p and q (n segments each) cross, between 0.1 and 0.9 of both. Only the
   pairs of segments that can give such a parameter (the segments i with i > 0.1 n and i - 1 < 0.9 n) and whose bounding
   boxes meet (within 10^-9) are computed: the boxes of all the pairs at once, by Outer. Both are necessary conditions for
   a crossing counted below, so the result is the same as with every pair *)
polylineCrossings[p_, q_, n_] := Module[{out = {}, d1, d2, den, s, t, cross2, middle, is, js, box, bp, bq, near, i, j},
   cross2[u_, v_] := u[[1]] v[[2]] - u[[2]] v[[1]];
   middle[r_] := Select[Range[Length[r] - 1], # > 0.1 n && # - 1 < 0.9 n &];
   (* the lower and upper ends of the boxes of the segments ks of r, in the coordinate c *)
   box[r_, ks_, c_] := With[{a = r[[ks, c]], b = r[[ks + 1, c]]}, {(a + b - Abs[a - b])/2, (a + b + Abs[a - b])/2}];
   is = middle[p]; js = middle[q];
   near = If[is === {} || js === {}, {},
     bp = {box[p, is, 1], box[p, is, 2]}; bq = {box[q, js, 1], box[q, js, 2]};
     Position[Sign[Times @@ Table[(1 + Sign[10.^-9 - Outer[Plus, bp[[c, 1]], -bq[[c, 2]]]]) (1 + Sign[10.^-9 + Outer[Plus, bp[[c, 2]], -bq[[c, 1]]]]),
        {c, 2}]], 1]];
   Do[i = is[[k[[1]]]]; j = js[[k[[2]]]];
    d1 = p[[i + 1]] - p[[i]]; d2 = q[[j + 1]] - q[[j]]; den = cross2[d1, d2];
    If[Abs[den] > 10.^-12,
     s = cross2[q[[j]] - p[[i]], d2]/den; t = cross2[q[[j]] - p[[i]], d1]/den;
     If[0. <= s <= 1. && 0. <= t <= 1. && 0.1 < (i - 1 + s)/n < 0.9 && 0.1 < (j - 1 + t)/n < 0.9, AppendTo[out, (i - 1 + s)/n]]],
    {k, near}];
   Module[{res = {}}, Do[If[! AnyTrue[res, Abs[# - c] < 0.02 &], AppendTo[res, c]], {c, out}]; res]];
(* a cross of half size h at q (an impurity line, a source insertion) *)
tikzCross[q_, h_] := coordinateString[q + h {1, 1}] <> " -- " <> coordinateString[q - h {1, 1}] <> " " <>
   coordinateString[q + h {1, -1}] <> " -- " <> coordinateString[q - h {1, -1}];

(* the amplitude of a TikZ wave relative to its own (0.7 for the flatter waves of a double wavy line); the pictures of
   Mathematica have $waveAmplitude *)
$tikzWaveFactor = 1.;
tikzLine[{"E", path_, style_}, t_, scale_, lw_ : 0.7, crossings_List : {}] := Module[{physical = 10 scale pathLength[path], wl = Max[1.3, Min[1.6, 2.3 scale]], nw,
    options},
   (* waves 1.6 mm long at the default scale, down to 1.3 mm in small pictures (shorter ones look like noise), and an
      amplitude in proportion *)
   nw = Max[1, Floor[physical/wl]];
   Which[
    (* gluons: a coil, on segments and arcs alike; wavy and zigzag arcs (on arcs the decorations of TikZ can fail,
       Dimension too large): curves through computed points. The dressed interaction: the wavy line drawn double, its
       two strokes 1.2 line widths apart along the normal of the wave itself (two waves shifted along the line would
       close up on the slopes), in proportion to the line width wherever the picture is copied with heavier lines; its
       waves flatter than those of a plain wavy line, so that the inner stroke keeps clear of itself at the crests *)
    style === "DoubleWavy", StringReplace[Block[{$tikzWaveFactor = 0.7}, tikzLine[{"E", path, "Wavy"}, t, scale, lw, crossings]],
     "\\draw plot" -> "\\draw[double, double distance=1.2\\pgflinewidth] plot"],
    style === "Curly", "  \\draw plot[smooth, tension=0.6] coordinates {" <>
     joinStrings[coordinateStrings[coilPoints[path, Max[2, nw], 0.36 wl/(10 scale), 0.4 wl/(10 scale)]], " "] <> "};\n",
    (* a wavy arc: a half-integer number of waves, so that it is mirror symmetric about its middle (it leaves both ends
       on the same side), and on a short or low arc (the Fock arc between neighbouring vertices) shorter waves of
       smaller amplitude, in proportion to the height of the arc, down to 0.95 mm: a few large waves on a small arc
       read as a lumpy hump, not as an arc *)
    path[[1]] =!= "segment" && style === "Wavy", Module[{h = 10 scale If[path[[1]] === "arc", Min[path[[3]], path[[3]] (1 - Cos[path[[5]]/2])], $bosonLoopRadius],
       wa, na},
      wa = Min[wl, Max[0.95, 0.6 h]]; na = Max[1.5, Round[physical/wa - 0.5] + 0.5];
      tikzWaves[path, na, 0.26 $tikzWaveFactor (physical/na)/(10 scale), {0.}, crossings]],
    (* a straight wavy line: the same explicit curve, a half-integer number of waves, mirror symmetric about its middle
       (the snake decoration of TikZ ends in a straight remainder at one end) *)
    style === "Wavy", Module[{na = Max[1.5, Round[physical/wl - 0.5] + 0.5]},
      tikzWaves[path, na, 0.27 $tikzWaveFactor (physical/na)/(10 scale), {0.}, crossings]],
    path[[1]] =!= "segment" && style === "Zigzag", "  \\draw " <> joinStrings[coordinateString /@ pathPoints[path, N[Range[0, 2 nw]]/(2 nw),
       0.25 wl/(10 scale) Table[If[i == 0 || i == 2 nw, 0., (-1.)^i], {i, 0, 2 nw}]], " -- "] <> ";\n",
    True,
    options = Switch[style,
      (* a straight zigzag line (wavy lines and zigzag arcs are explicit curves, above) *)
      "Zigzag", "decorate, decoration={zigzag, amplitude=" <> numberString[0.25 wl] <> "mm, segment length=" <>
       numberString[physical/nw] <> "mm}",
      (* dashes in proportion to the size of the picture *)
      "Dashed" | "Impurity", "dash pattern=on " <> numberString[3.75 scale] <> "pt off " <> numberString[3.75 scale] <> "pt",
      "Dotted" | "DottedArrow", "dotted, line width=" <> numberString[1.57 lw] <> "pt",
      "Double" | "DoubleArrow", "double, double distance=" <> numberString[1.86 lw] <> "pt",
      _, ""];
    "  \\draw" <> If[options === "", " ", "[" <> options <> "] "] <> tikzPathString[path] <> ";\n" <>
     (* on a double line (3.9 lw wide) the head is larger, so that it stands out of the two strokes *)
     If[directedStyleQ[style], tikzArrowHead[path, t, scale, lw, If[style === "DoubleArrow", 1.45, 1.]], ""] <>
     If[anomalousStyleQ[style], StringJoin[tikzArrowHead[#[[1]], #[[2]], scale, lw] & /@ anomalousArrowHeads[path, style]], ""] <>
     If[style === "Impurity", "  \\draw " <> tikzCross[First[pathPoints[path, {0.5}, 0.]], $impurityCross] <> ";\n", ""]]];

(* a declared blob, or one without a name (unnamedBlobSpec, label ""): the shape filled, the hatching as explicit chords
   and the dots as small discs (no patterns library), the label, the outline. On a hatched or dotted blob the label sits
   in a white clearing with rounded corners, a little larger than the glyphs, so that no hatch line or dot runs through
   or touches them; the clearing is clipped to the blob and the outline drawn over it, so that a wide label never cuts
   the outline *)
tikzDeclaredBlob[p_, spec : {label_, shape_, fill_, h_}, lw_, font_, rules_] := Module[{shapeString, hatches = blobHatchesOf[p, spec],
    dots = blobDotsOf[p, spec],
    at = coordinateString[blobLabelPoint[p, spec]],
    (* the label of a triangle is smaller (blobLabelScale): the node scaled, its clearing with it *)
    scaleString = If[blobLabelScale[shape] == 1., "", ", scale=" <> numberString[blobLabelScale[shape]]]},
   shapeString = Switch[shape,
     "Box", coordinateString[p - {h, h}] <> " rectangle " <> coordinateString[p + {h, h}],
     "Triangle" | "TriangleUp" | "Diamond", joinStrings[coordinateString /@ triangleCorners[p, h, shape], " -- "] <> " -- cycle",
     _, coordinateString[p] <> " circle (" <> numberString[h] <> ")"];
   "  \\fill[" <> If[fill === "Grey", "black!20", "white"] <> "] " <> shapeString <> ";\n" <>
    If[hatches === {}, "", "  \\draw[line width=" <> numberString[0.55 lw] <> "pt] " <>
      joinStrings[Table[coordinateString[hs[[1]]] <> " -- " <> coordinateString[hs[[2]]], {hs, hatches}], " "] <> ";\n"] <>
    (* the dots 1.4 line widths across (the hatch lines are 0.55), so that they read as dots in print; their radius in
       units of the line width of the picture, so that they follow it when the line width is changed *)
    If[dots === {}, "", "  \\fill " <> joinStrings[Table[coordinateString[q] <> " circle (0.7\\pgflinewidth)", {q, dots}], " "] <>
      ";\n"] <>
    Which[
     (* a blob without a name (unnamedBlobSpec): the fill and the outline *)
     label === "", "  \\draw " <> shapeString <> ";\n",
     texturedFillQ[fill],
     "  \\begin{scope}\n  \\clip " <> shapeString <> ";\n  \\node[font=" <> ToString[font] <> scaleString <>
      ", inner sep=1.2pt, fill=white, rounded corners=2pt] at " <> at <> " {$" <> label <> "$};\n  \\end{scope}\n" <>
      "  \\draw " <> shapeString <> ";\n",
     True, "  \\draw " <> shapeString <> ";\n  \\node[font=" <> ToString[font] <> scaleString <> ", inner sep=0.6pt] at " <> at <> " {$" <> label <> "$};\n"]];

tikzItems[items_, opts_List] := Module[{scale = tikzScale[opts], rules = Flatten[{drawingOption[opts, "Labels"]}], es,
    lw = tikzLineWidth[opts], font = drawingOption[opts, "LabelFont"]},
   es = Cases[items, {"E", __}];
   StringJoin[
    MapThread[tikzLine[#1, #2, scale, lw, #3] &, {es, arrowParameters[es, Cases[items, {"B", p_, spec_} :> {p, spec[[2]], spec[[4]]}]],
      waveCrossingParameters[es]}],
    Cases[items, {"L", q_, e_, direction_} :> "  \\draw " <> coordinateString[q] <> " -- " <> coordinateString[e] <> ";\n" <>
       Which[direction === "Out", tikzArrowHead[{"segment", q, e}, 0.55, scale, lw],
        direction === "In", tikzArrowHead[{"segment", e, q}, 0.55, scale, lw], True, ""]],
    Cases[items, {"S", {p_, q_}} :> If[p === q, "", "  \\draw " <> coordinateString[p] <> " -- " <> coordinateString[q] <> ";\n"] <>
       "  \\draw[fill=white] " <> coordinateString[q] <> " circle (" <> numberString[0.1] <> ");\n" <>
       "  \\draw[line width=" <> numberString[0.71 lw] <> "pt] " <> tikzCross[q, 0.07] <> ";\n"],
    Cases[items, {"N", p_, k_?unnamedBlobKindQ} :> tikzDeclaredBlob[p, unnamedBlobSpec[k], lw, font, rules]],
    Cases[items, {"B", p_, spec_} :> tikzDeclaredBlob[p, spec, lw, font, rules]],
    Cases[items, {"N", p_, "dot"} :> "  \\fill " <> coordinateString[p] <> " circle (" <> numberString[0.85 $dotRadius] <> ");\n"],
    Cases[items, {"N", p_, "ext"} :> "  \\draw[fill=white] " <> coordinateString[p] <> " circle (" <> numberString[$externalPointRadius] <> ");\n"],
    (* a label is anchored on the side facing its point, so that it never overlaps it *)
    Cases[items, {"T", p_, label_, a_, angle_} :> "  \\node[font=" <> ToString[font] <> ", inner sep=0pt, anchor=" <>
       ToString[Mod[Round[angle] + 180, 360]] <> "] at " <> coordinateString[a] <> " {$" <> latexLabel[label, rules] <> "$};\n"]]];

tikzPicture[items_, opts_List] := "\\begin{tikzpicture}[x=" <> numberString[tikzScale[opts]] <> "cm, y=" <>
   numberString[tikzScale[opts]] <> "cm, line width=" <> numberString[tikzLineWidth[opts]] <>
   "pt, baseline={([yshift=-0.6ex]0,0)}]\n" <> tikzItems[items, opts] <> "\\end{tikzpicture}";

DiagramToTikZ[e_, ext_List : {}, opts___] := Module[{allOpts = drawingOptions[DiagramToTikZ, {opts}], gs, parts, output},
   gs = geometriesOf[e, ext, allOpts]; output = drawingOption[allOpts, "Output"];
   (* a term without lines (a number) has no picture *)
   parts = Table[With[{w = latexWeight[g[[1]]]}, {w[[1]], w[[2]], If[g[[2]] === {}, "", tikzPicture[g[[2]], allOpts]]}], {g, gs}];
   If[output === "List", Table[{If[p[[1]] < 0, "-", ""] <> p[[2]], p[[3]]}, {p, parts}],
    StringJoin[Table[With[{p = parts[[i]]},
       (* the signs between two pictures with some room, so that "b - a" (the last label of a picture, the first of the
          next one) does not read as one expression; the sign is an ordinary symbol with a break point before it, so that
          a line never ends with a sign *)
       If[i == 1, If[p[[1]] < 0, "-", ""], If[p[[1]] < 0, "\n\\allowbreak\\;{-}\\; ", "\n\\allowbreak\\;{+}\\; "]] <>
        Which[p[[3]] === "", p[[2]], p[[2]] === "1", If[i == 1 && p[[1]] < 0, "\\,", ""], True, p[[2]] <> "\\,"] <> p[[3]]],
      {i, Length[parts]}]]]];

(* ========================================================================= *)
(*  16. Formulas in LaTeX                                                    *)
(*  A sum of terms written as LaTeX: integrals with their variables as a     *)
(*  subscript, functions with their arguments in parentheses, delta          *)
(*  functions, Grassmann products in their order (after the commuting        *)
(*  factors), rational coefficients as fractions.                            *)
(* ========================================================================= *)

$latexNames = {}; $latexLabels = {}; $latexSubscripts = {};

(* the strings of a list joined by a separator *)
joinStrings[list_List, sep_String] := If[list === {}, "", StringJoin[First[list], Table[sep <> x, {x, Rest[list]}]]];

(* the LaTeX of a head or of a symbol: "Names", then the display name (DeclareDisplayName), then the symbol name *)
(* the objects of the tree expansion (ConnectedFunctionFromVertices) have their usual names *)
$latexDefaultNames = {FullPropagator -> "G", MeanField -> "\\bar\\phi", ExternalSource -> "J"};
latexSymbolName[h_Symbol] := Module[{m = Select[$latexNames, #[[1]] === h &, 1], d = Select[$displayNames, #[[1]] === h &, 1],
    k = Select[$latexDefaultNames, #[[1]] === h &, 1]},
   Which[m =!= {}, ToString[m[[1, 2]]],
    d =!= {}, If[StringQ[d[[1, 2]]], latexName[d[[1, 2]]], ToString[TeXForm[d[[1, 2]]]]],
    k =!= {}, k[[1, 2]],
    True, latexSymbolString[h]]];

(* arguments of functions: labels *)
latexArgument[DummyVariable[depth_Integer, k_Integer]] := {"z", "w", "u", "v", "t"}[[Min[depth, 5]]] <> "_{" <> ToString[k] <> "}";
latexArgument[x_Symbol] := With[{r = Select[$latexLabels, #[[1]] === x &, 1]}, If[r =!= {}, ToString[r[[1, 2]]], latexSymbolName[x]]];
latexArgument[x_?NumberQ] := latexNumber[x];
latexArgument[x_] := latexOfExpression[x];

latexOfExpression[e_Plus] := latexSignedSum[latexSignedTerm /@ latexPlusTerms[e], latexDiracSumQ[e]];
(* the {sign, magnitude} parts of the terms of a sum joined; with positiveFirst the positive terms first (sums of Dirac
   matrices and tensors: m + pslash - kslash, g - (1 - xi) k k/k^2) *)
latexSignedSum[parts0_List, positiveFirst_] := With[{parts = If[TrueQ[positiveFirst], Join[Select[parts0, #[[1]] > 0 &],
      Select[parts0, #[[1]] < 0 &]], parts0]},
   StringJoin[Table[If[i == 1, If[parts[[i, 1]] < 0, "-", ""], If[parts[[i, 1]] < 0, " - ", " + "]] <> parts[[i, 2]], {i, Length[parts]}]]];
latexOfExpression[0] := "0";
latexOfExpression[e_] := With[{p = latexSignedTerm[e]}, If[p[[1]] < 0, "-", ""] <> p[[2]]];

(* the heads of integrals and sums, whose integrand is not delimited *)
$latexIntegralHeads = {IntegralOver, MomentumSum, LoopIntegral, FeynmanParameterIntegral};

(* {sign, LaTeX of the magnitude} of a product; factors with negative powers go to a denominator; Grassmann products last.
   One integral (Feynman parameters, a loop) with a fraction in front: the fraction, then the integral *)
latexIntegralAfterFractionQ[t_] := Head[t] === Times && Count[List @@ t, f_ /; MemberQ[{ParameterIntegral, SimplexIntegral, MinkowskiLoopIntegral}, Head[f]]] == 1 &&
   Count[List @@ t, f_ /; MemberQ[$latexIntegralHeads, Head[f]] || MemberQ[{GrassmannProduct, NonCommutativeMultiply}, Head[f]]] == 1 &&
   MemberQ[List @@ t, Power[b_, n_?NumberQ] /; n < 0 && ! NumberQ[b]];
latexSignedTerm[t_] /; latexIntegralAfterFractionQ[t] := With[{i = First[Select[List @@ t, MemberQ[$latexIntegralHeads, Head[#]] &]]},
   With[{p = latexSignedTerm[t/i]}, {p[[1]], p[[2]] <> "\\," <> latexFactor[i]}]];
latexSignedTerm[t_] := Module[{parts = coefficientParts[numericFactor[t]], c, unit, rest, factors, numerator, denominator, num, den,
    body},
   {c, unit} = parts[[{2, 3}]]; rest = nonNumericPart[t];
   factors = Which[rest === 1, {}, Head[rest] === Times, List @@ rest, True, {rest}];
   (* the unit matrix after the other plain factors (4 a.b 1, not 4 1 a.b), integrals and momentum sums after them (their
      integrand is not delimited), Grassmann products last; an integral that is not the last one is put in parentheses *)
   factors = Join[Select[factors, ! MemberQ[Join[$latexIntegralHeads, {GrassmannProduct, NonCommutativeMultiply}], Head[#]] && # =!= DiracProduct[] &],
     Select[factors, # === DiracProduct[] &],
     Select[factors, MemberQ[$latexIntegralHeads, Head[#]] &], Select[factors, MemberQ[{GrassmannProduct, NonCommutativeMultiply}, Head[#]] &]];
   With[{ints = Flatten[Position[Head /@ factors, Alternatives @@ $latexIntegralHeads, {1}, Heads -> False]]},
    If[Length[ints] > 1 || (ints =!= {} && Last[ints] < Length[factors]),
     factors = MapAt[parenthesizedIntegral, factors, List /@ If[Last[ints] == Length[factors], Most[ints], ints]]]];
   numerator = Select[factors, ! (Head[#] === Power && NumberQ[#[[2]]] && #[[2]] < 0) &];
   denominator = Cases[factors, Power[b_, n_?NumberQ] /; n < 0 :> If[n == -1, b, Power[b, -n]]];
   (* a single sum in a numerator or denominator needs no parentheses *)
   num = If[Length[numerator] == 1 && Head[First[numerator]] === Plus && denominator =!= {} && unit === "", latexOfExpression[First[numerator]],
     joinStrings[Join[If[unit === "", {}, {unit}], latexFactor /@ numerator], "\\,"]];
   den = Which[
     Length[denominator] == 1 && Head[First[denominator]] === Plus && Denominator[c] === 1, latexOfExpression[First[denominator]],
     (* the denominator q^2 - m^2 of a single line *)
     Length[denominator] == 1 && Head[First[denominator]] === latexLine && Denominator[c] === 1, latexLineString @@ First[denominator],
     True, joinStrings[latexFactor /@ denominator, "\\,"]];
   body = Which[
     denominator =!= {}, "\\frac{" <> If[Numerator[c] === 1, If[num === "", "1", num],
        latexNumber[Numerator[c]] <> If[num === "", "", "\\," <> num]] <> "}{" <>
       If[Denominator[c] === 1, den, latexNumber[Denominator[c]] <> "\\," <> den] <> "}",
     num === "", latexNumber[c],
     c === 1, num,
     True, latexNumber[c] <> "\\," <> num];
   {parts[[1]], body}];

(* the LaTeX of a factor: a subexpression named by the option "Names" (n_F(xi(p1)) -> "n_1") is written with its name *)
$latexExpressionNames = {};
latexFactor[e_] := With[{m = Select[$latexExpressionNames, #[[1]] === e &, 1]}, If[m =!= {}, ToString[m[[1, 2]]], latexFactorOf[e]]];

(* an integral that other factors follow, in parentheses *)
latexFactorOf[parenthesizedIntegral[i_]] := "\\left(" <> latexFactor[i] <> "\\right)";
(* a base of a power in parentheses: sums, products, powers, negative numbers, and objects with a superscript of their own
   (p^2, p^\mu, \gamma^\mu, g^{\mu\nu}) *)
latexParenthesized[e_] := If[MemberQ[{Plus, Times, Power, ScalarProduct, FourVector, GammaMatrix, LorentzMetric}, Head[e]] || (NumberQ[e] && e < 0),
   "\\left(" <> latexOfExpression[e] <> "\\right)", latexFactor[e]];

latexFactorOf[x_?NumberQ] := latexNumber[x];
latexFactorOf[IntegralOver[f_, v_List]] := "\\int_{" <> joinStrings[latexArgument /@ v, "\\,"] <> "}" <>
   If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[(GrassmannProduct | NonCommutativeMultiply)[a__]] := joinStrings[latexFactor /@ {a}, "\\,"];
latexFactorOf[DeltaFunction[x_, y_]] := "\\delta(" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";
latexFactorOf[Power[E, x_]] := "e^{" <> latexOfExpression[x] <> "}";
latexFactorOf[Power[b_, n_]] := If[n === 1/2, "\\sqrt{" <> latexOfExpression[b] <> "}",
   latexParenthesized[b] <> "^{" <> latexOfExpression[n] <> "}"];
latexFactorOf[e_Plus] := "\\left(" <> latexOfExpression[e] <> "\\right)";
(* the elementary functions with their usual names *)
$latexElementaryFunctions = {Log -> "\\log", Sin -> "\\sin", Cos -> "\\cos", Tan -> "\\tan", Cot -> "\\cot", Sinh -> "\\sinh",
   Cosh -> "\\cosh", Tanh -> "\\tanh", Coth -> "\\coth", ArcSin -> "\\arcsin", ArcCos -> "\\arccos", ArcTan -> "\\arctan"};
latexFactorOf[(h : Log | Sin | Cos | Tan | Cot | Sinh | Cosh | Tanh | Coth | ArcSin | ArcCos | ArcTan)[x_]] :=
  (h /. $latexElementaryFunctions) <> "\\left(" <> latexOfExpression[x] <> "\\right)";
(* heads with LaTeX rules of their own, which the generic rules below (a function of labels, its derivatives) leave alone;
   the later sections (Matsubara, dimensional regularization, QED, effective actions) add their heads *)
$latexHeadsWithRules = {Plus, Times, Power, IntegralOver, GrassmannProduct, NonCommutativeMultiply, DeltaFunction, parenthesizedIntegral,
   VertexFunction};
latexFactorOf[(Derivative[n__])[h_Symbol][args___]] /; ! MemberQ[$latexHeadsWithRules, h] := latexSymbolName[h] <>
   If[Length[{n}] == 1, If[n <= 3, "^{" <> StringJoin[Table["\\prime", {n}]] <> "}", "^{(" <> ToString[n] <> ")}"],
    "^{(" <> joinStrings[ToString /@ {n}, ","] <> ")}"] <> "(" <> joinStrings[latexArgument /@ {args}, ","] <> ")";
latexFactorOf[VertexFunction[args__]] /; FreeQ[$latexNames, VertexFunction] :=
  "\\Gamma^{(" <> ToString[Length[{args}]] <> ")}(" <> joinStrings[latexArgument /@ {args}, ","] <> ")";
latexFactorOf[h_Symbol[args___]] /; ! MemberQ[Join[$latexHeadsWithRules, First /@ $latexElementaryFunctions], h] || (h === VertexFunction && ! FreeQ[$latexNames, VertexFunction]) :=
  latexSymbolName[h] <> If[MemberQ[$latexSubscripts, h], "_{" <> joinStrings[latexArgument /@ {args}, ","] <> "}",
    "(" <> joinStrings[latexArgument /@ {args}, ","] <> ")"];
latexFactorOf[s_Symbol] := latexArgument[s];

(* ---- QED (section 19): Dirac matrices, Lorentz tensors, loop and Feynman-parameter integrals. A slashed momentum is
   \slashed{p} (\usepackage{slashed}); the indices are written upper, a repeated one is contracted, as in the package ---- *)
$latexDiracHeads = {GammaMatrix, SlashedMomentum, DiracProduct, DiracTrace, SpinorChain, LorentzMetric, FourVector, latexSlashed};
$latexHeadsWithRules = Join[$latexHeadsWithRules, $latexDiracHeads, {ScalarProduct, FeynmanDenominator, MinkowskiLoopIntegral,
    ParameterIntegral, SimplexIntegral, FeynmanParameter, latexLine}];
$latexIntegralHeads = Join[$latexIntegralHeads, {MinkowskiLoopIntegral, ParameterIntegral, SimplexIntegral}];
$latexDefaultNames = Join[$latexDefaultNames, {SpacetimeDimension -> "d"}];
latexDiracSumQ[e_] := ! FreeQ[e, Alternatives @@ $latexDiracHeads];
(* the terms of a sum, a slashed sum of momenta among them written as its terms: m + (p - k)slash -> m + pslash - kslash *)
latexPlusTerms[e_Plus] := If[latexDiracSumQ[e], Flatten[latexSlashedTerms /@ (List @@ e)], List @@ e];
latexSlashedTerms[t_] := Which[
   MatchQ[t, SlashedMomentum[_]], Table[c[[1]] latexSlashed[c[[2]]], {c, latexMomentumTerms[t[[1]]]}],
   MatchQ[t, Times[_?NumberQ, SlashedMomentum[_]]], Table[t[[1]] c[[1]] latexSlashed[c[[2]]], {c, latexMomentumTerms[t[[2, 1]]]}],
   True, {t}];
(* a momentum as its terms {coefficient, name}: numbers and Feynman parameters are coefficients *)
latexMomentumCoefficient[t_] := Which[NumericQ[t], t, Head[t] === Times, Times @@ Select[List @@ t, NumericQ[#] || MatchQ[#, FeynmanParameter[_]] &],
   True, 1];
latexMomentumTerms[q_] := With[{x = Expand[q]}, Table[With[{c = latexMomentumCoefficient[t]}, {c, t/c}], {t, If[Head[x] === Plus, List @@ x, {x}]}]];
latexMomentumSum[q_] := latexSignedSum[Table[latexSignedTerm[c[[1]] c[[2]]], {c, latexMomentumTerms[q]}], True];
(* a momentum as a factor, in (small) parentheses when it is a sum: p, (p - k) *)
latexMomentumFactor[q_] := With[{ts = latexMomentumTerms[q]}, If[Length[ts] == 1 && ts[[1, 1]] === 1, latexArgument[ts[[1, 2]]],
    "(" <> latexMomentumSum[q] <> ")"]];
(* pslash, and p1slash = \slashed{p}{}_{1} (the slash on the letter; the empty group keeps the subscript at the height of
   that of p_1, not below the box of \slashed) *)
latexSlashedName[n_Symbol] /; FreeQ[{$latexLabels, $latexNames, $displayNames}, n] := With[{parts = splitTrailingDigits[cutAtDollar[SymbolName[n]]]},
   "\\slashed{" <> latexBaseName[parts[[1]]] <> "}" <> If[parts[[2]] === "", "", "{}_{" <> parts[[2]] <> "}"]];
(* a name written base_{i} (the default loop momentum ell_i, LoopMomentum[i] outside its integral) in the same way:
   \slashed{\ell}{}_{1}; the base is a letter or a command \name *)
slashBaseQ[b_String] := (StringLength[b] == 1 && LetterQ[b]) || (StringLength[b] > 1 && StringTake[b, 1] === "\\" && LetterQ[StringDrop[b, 1]]);
latexSlashedName[n_] := With[{s = latexArgument[n]}, With[{pos = StringPosition[s, "_{"]},
    If[pos =!= {} && StringTake[s, -1] === "}" && slashBaseQ[StringTake[s, pos[[1, 1]] - 1]] &&
      StringFreeQ[StringTake[s, {pos[[1, 2]] + 1, -2}], "{"] && StringFreeQ[StringTake[s, {pos[[1, 2]] + 1, -2}], "}"],
     "\\slashed{" <> StringTake[s, pos[[1, 1]] - 1] <> "}{}" <> StringDrop[s, pos[[1, 1]] - 1], "\\slashed{" <> s <> "}"]]];
latexFactorOf[latexSlashed[n_]] := latexSlashedName[n];
latexFactorOf[SlashedMomentum[q_]] := With[{ts = latexMomentumTerms[q]}, If[Length[ts] == 1 && ts[[1, 1]] === 1, latexSlashedName[ts[[1, 2]]],
    "\\left(" <> latexOfExpression[Total[Table[c[[1]] latexSlashed[c[[2]]], {c, ts}]]] <> "\\right)"]];
latexFactorOf[GammaMatrix[mu_]] := "\\gamma^{" <> latexArgument[mu] <> "}";
latexFactorOf[LorentzMetric[a_, b_]] := "g^{" <> latexArgument[a] <> latexArgument[b] <> "}";
latexFactorOf[FourVector[q_, mu_]] := latexMomentumFactor[q] <> "^{" <> latexArgument[mu] <> "}";
latexFactorOf[ScalarProduct[a_, b_]] := If[a === b, latexMomentumFactor[a] <> "^{2}", latexMomentumFactor[a] <> "\\cdot " <> latexMomentumFactor[b]];
latexFactorOf[FeynmanParameter[i_]] := "x_{" <> latexOfExpression[i] <> "}";
(* the factors of a product of Dirac matrices: sums in parentheses, and a matrix with a coefficient, (i\gamma^\mu) *)
latexDiracFactor[f_] := Which[
   Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)",
   Head[f] === Times && ! MemberQ[{Plus, Times}, Head[nonNumericPart[f]]], "\\left(" <> latexOfExpression[f] <> "\\right)",
   True, latexOfExpression[f]];
latexFactorOf[DiracProduct[]] := "\\mathbf{1}";
latexFactorOf[DiracProduct[a__]] := joinStrings[latexDiracFactor /@ {a}, "\\,"];
latexFactorOf[DiracTrace[x_]] := "\\operatorname{tr}\\left[" <> If[Head[x] === Plus, latexOfExpression[x], latexFactor[x]] <> "\\right]";
latexFactorOf[SpinorChain[{p2_, m2_}, x_, {p1_, m1_}]] := "\\bar{u}(" <> latexMomentumSum[p2] <> ")\\," <>
   If[x === DiracProduct[], "", If[Head[x] === Plus, "\\left(" <> latexOfExpression[x] <> "\\right)", latexFactor[x]] <> "\\,"] <>
   "u(" <> latexMomentumSum[p1] <> ")";
(* the denominator q^2 - m^2 of a line (latexDisplayForm), in parentheses next to other factors *)
latexLineString[q_, m_] := latexMomentumFactor[q] <> "^{2} - " <> latexParenthesized[m] <> "^{2}";
latexFactorOf[latexLine[q_, m_]] := "\\left(" <> latexLineString[q, m] <> "\\right)";
latexIntegrand[f_] := If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[MinkowskiLoopIntegral[f_, ks_List]] := "\\int_{" <> joinStrings[latexArgument /@ ks, "\\,"] <> "}" <> latexIntegrand[f];
latexFactorOf[(ParameterIntegral | FeynmanParameterIntegral)[f_, x_]] := "\\int_0^1 d" <> latexArgument[x] <> "\\," <> latexIntegrand[f];
latexFactorOf[SimplexIntegral[f_, xs_List]] := "\\int_0^1 " <> joinStrings[Table["d" <> latexArgument[x], {x, xs}], "\\,"] <>
   "\\,\\delta(1 - " <> joinStrings[latexArgument /@ xs, " - "] <> ")\\," <> latexIntegrand[f];
(* before the LaTeX: the lines 1/(q^2 - m^2) as denominators, so that the lines of a term make one fraction; the numbers
   in front of an integral taken out of it *)
latexDisplayForm[e_] := e //. {FeynmanDenominator[q_, m_] :> If[m === 0, 1/ScalarProduct[q, q], 1/latexLine[q, m]],
    (h : MinkowskiLoopIntegral | ParameterIntegral | SimplexIntegral)[Times[c_?NumericQ, f__], v_] :> c h[Times[f], v]};
latexFactorOf[e_] := ToString[TeXForm[e]];

(* the default loop momenta LoopMomentum[i] of QED (MinkowskiLoopIntegral) are \ell_i: p_i, as in the Matsubara formulas,
   would be the external momenta p1, p2. So in an integrand taken out of its integral (Dirac and Lorentz objects, no
   MomentumSum of the Matsubara formulas) *)
qedLatexQ[e_] := ! FreeQ[e, MinkowskiLoopIntegral | FeynmanDenominator] ||
   (FreeQ[e, MomentumSum] && ! FreeQ[e, SlashedMomentum | DiracProduct | DiracTrace | SpinorChain | FourVector | ScalarProduct]);
latexLoopMomentumNames[e_] := If[! qedLatexQ[e], {},
   Table[LoopMomentum[i] -> "\\ell_{" <> ToString[i] <> "}", {i, Union[Cases[{e}, LoopMomentum[i_] :> i, Infinity]]}]];
(* "Names" names heads and symbols, and also subexpressions, such as n_F(xi(p1)) -> "n_1"; a name given wins *)
ExpressionToLaTeX[e_, opts___Rule] := With[{names = Flatten[{optionValue[{opts}, {"Names" -> {}}, "Names"]}]},
   Block[{$latexNames = Select[names, AtomQ[#[[1]]] &], $latexExpressionNames = Join[Select[names, ! AtomQ[#[[1]]] &], latexLoopMomentumNames[e]],
     $latexLabels = Flatten[{optionValue[{opts}, {"Labels" -> {}}, "Labels"]}],
     $latexSubscripts = Flatten[{optionValue[{opts}, {"Subscripts" -> {}}, "Subscripts"]}]},
    latexOfExpression[latexDisplayForm[e]]]];

(* ========================================================================= *)
(*  17. Matsubara sums and the evaluation of diagrams                        *)
(*  A diagram of the package lives in label space: a label stands for a      *)
(*  position, an imaginary time and a spin. In a translation-invariant       *)
(*  system it is evaluated in momentum and Matsubara frequency: momenta and  *)
(*  frequencies are routed through the lines (each integral over an         *)
(*  internal label is their conservation), the frequency of each loop is    *)
(*  summed by residues, and the result can be continued to real frequency.  *)
(*  The imaginary part of a retarded two-point function is also given by    *)
(*  the cutting rules at finite temperature, computed independently, in     *)
(*  imaginary time.                                                          *)
(*  Conventions: G0(k, i w) = 1/(i w - xi_k), G(tau) = T Sum_n               *)
(*  e^(-i w_n tau) G(i w_n); a line G[x, y] goes from y to x and carries its *)
(*  momentum and frequency from y to x; a line between labels at equal times *)
(*  (G(z, z), or two labels joined by an instantaneous interaction) is       *)
(*  G(tau = 0-): factor e^(i w 0+), T Sum e^(i w 0+)/(i w - xi) = n_F(xi),   *)
(*  the ordering psibar psi; "ConvergenceFactor" -> -1 takes G(0+),          *)
(*  e^(-i w 0+), n_F - 1 (the ordering psi psibar), and 0 the mean of the    *)
(*  two, the symmetric sum, n_F - 1/2 (the ordering 1/2 [psibar, psi]).      *)
(*  Residues are taken without Residue, Limit or Apart: the poles are read   *)
(*  from the linear denominators of each term.                               *)
(* ========================================================================= *)

(* ---- statistics of Matsubara frequencies ---- *)

statisticalFunction["Fermion"] = FermiFunction;
statisticalFunction["Boson"] = BoseFunction;
otherStatisticalFunction[FermiFunction] = BoseFunction;
otherStatisticalFunction[BoseFunction] = FermiFunction;

(* the frequencies declared by the user: {symbol, statistics}. The loop frequencies LoopFrequency[i, statistics] carry
   their statistics with them; during the sums they are replaced by symbols declared here (withFrequencySymbols) *)
$matsubaraFrequencies = {};
MatsubaraSum::statistics = "The statistics `1` is neither \"Fermion\" nor \"Boson\".";
DeclareMatsubaraFrequency::symbol = "`1` is not a symbol: a Matsubara frequency is declared as a symbol w, written I w in expressions.";
DeclareMatsubaraFrequency[w_Symbol, s_] := DeclareMatsubaraFrequency[{w}, s];
DeclareMatsubaraFrequency[ws_List, s_] := Which[
   ! MemberQ[{"Fermion", "Boson"}, s], Message[MatsubaraSum::statistics, s],
   ! AllTrue[ws, Head[#] === Symbol &], Scan[If[Head[#] =!= Symbol, Message[DeclareMatsubaraFrequency::symbol, #]] &, ws],
   True, Scan[warnIfBuiltin, ws];
   $matsubaraFrequencies = Join[Select[$matsubaraFrequencies, ! MemberQ[ws, #[[1]]] &], ({#, s} &) /@ ws];];
DeclareMatsubaraFrequency[w_, s_] /; Head[w] =!= Symbol && Head[w] =!= List := Message[DeclareMatsubaraFrequency::symbol, w];
frequencyStatistics[LoopFrequency[_, s_String]] := s;
frequencyStatistics[w_] := With[{m = Select[$matsubaraFrequencies, #[[1]] === w &]}, If[m === {}, None, m[[1, 2]]]];
frequencyAtoms[e_] := Select[Table[f[[1]], {f, $matsubaraFrequencies}], ! FreeQ[e, #] &];

(* the loop frequencies of e replaced by symbols, declared with their statistics, while g[e with symbols, rules back] is
   computed; the result is written with the loop frequencies again. So Coefficient, Exponent, Factor and Together work
   on plain symbols *)
withFrequencySymbols[e_, g_] := Module[{fs = Union[Cases[{e}, LoopFrequency[_, "Fermion" | "Boson"], Infinity]], symbols, forward, back},
   symbols = Table[Unique["FunctionalDiagrams`Private`loopFrequency"], {Length[fs]}];
   forward = Table[fs[[i]] -> symbols[[i]], {i, Length[fs]}];
   back = Table[symbols[[i]] -> fs[[i]], {i, Length[fs]}];
   Block[{$matsubaraFrequencies = Join[$matsubaraFrequencies, Table[{symbols[[i]], fs[[i, 2]]}, {i, Length[fs]}]]},
    g[e /. forward, back] /. back]];

(* ---- periodicity: n_F(e + i nu) = n_F(e), n_F(e + i w) = -n_B(e), n_B(e + i nu) = n_B(e), n_B(e + i w) = -n_F(e),
   for bosonic nu and fermionic w, and the same for the derivatives ---- *)

(* the shift of an energy by Matsubara frequencies (written i w): {energy without them, 1 if the shift is fermionic,
   0 if it is bosonic} *)
MatsubaraSum::shift = "`1` is shifted by a combination of `2` that is not a Matsubara frequency (integer coefficients of i w, with statistics declared by DeclareMatsubaraFrequency).";
matsubaraShift[a_] := Module[{fs = frequencyAtoms[a], ex, cs, stats},
   If[fs === {}, {a, 0},
    ex = Expand[a]; cs = Table[Coefficient[ex, f]/I, {f, fs}]; stats = frequencyStatistics /@ fs;
    If[! AllTrue[cs, IntegerQ] || MemberQ[stats, None], Message[MatsubaraSum::shift, a, fs]; {a, 0},
     {Expand[ex /. Table[f -> 0, {f, fs}]], Mod[Total[Table[If[stats[[i]] === "Fermion", cs[[i]], 0], {i, Length[fs]}]], 2]}]]];

periodicStatistics[h_, k_, a_] := Module[{s = matsubaraShift[a], g},
   g = If[s[[2]] == 1, otherStatisticalFunction[h], h];
   If[s[[2]] == 1, -1, 1] signCanonicalStatistics[g, k, s[[1]]]];

(* the argument with a positive leading term: n_F(-x) = 1 - n_F(x), n_B(-x) = -1 - n_B(x), and for the derivatives
   n^(k)(-x) = (-1)^(k + 1) n^(k)(x). The leading term of a sum is its first term, in canonical order, that is neither a
   number nor a multiple of a single symbol, if there is one: a constant such as the chemical potential does not decide
   the sign, n_F(eps(p) - mu) and n_F(p^2/(2 m) - mu) stay as they are. n_F(0) = 1/2 at every temperature. *)
FermiFunction[0] = 1/2;
FermiFunction[0.] = 0.5;
constantTermQ[t_] := NumberQ[t] || MatchQ[t, _Symbol | Times[_?NumberQ, _Symbol]];
leadingTerm[x_Plus] := With[{main = Select[List @@ x, ! constantTermQ[#] &]}, If[main === {}, First[x], First[main]]];
leadingTerm[x_] := x;
leadingSign[e_] := With[{x = Expand[e]}, If[x === 0, 0, signOfTerm[leadingTerm[x]]]];
signOfTerm[t_] := If[TrueQ[Re[numericFactor[t]] < 0], -1, 1];
signCanonicalStatistics[h_, k_, a_] := Which[
   leadingSign[a] >= 0, If[k == 0, h[a], Derivative[k][h][a]],
   k == 0, If[h === FermiFunction, 1 - h[Expand[-a]], -1 - h[Expand[-a]]],
   True, (-1)^(k + 1) Derivative[k][h][Expand[-a]]];

simplifyStatisticalFactors[e_] := e /. {Derivative[k_][h : FermiFunction | BoseFunction][a_] :> periodicStatistics[h, k, a],
    (h : FermiFunction | BoseFunction)[a_] :> periodicStatistics[h, 0, a]};

(* ---- terms, poles and Laurent coefficients of a rational function of z ---- *)

(* the terms of a sum (a product: itself); {base, exponent} of a factor *)
productTermsOf[x_Plus] := List @@ x;
productTermsOf[x_] := {x};
baseAndExponent[p_] := If[Head[p] === Power, {p[[1]], p[[2]]}, {p, 1}];
(* the terms of e, as a list of products: products and positive powers of sums that contain z are multiplied out,
   factors free of z are kept as they are *)
distributeLists[lists_List] := Fold[Flatten[Table[a b, {a, #1}, {b, #2}]] &, {1}, lists];
termsInVariable[e_Plus, z_] := Join @@ (termsInVariable[#, z] & /@ (List @@ e));
termsInVariable[e_Times, z_] := With[{free = Times @@ Select[List @@ e, FreeQ[#, z] &]},
   (free #) & /@ distributeLists[termsInVariable[#, z] & /@ Select[List @@ e, ! FreeQ[#, z] &]]];
termsInVariable[Power[b_Plus, n_Integer?Positive], z_] /; ! FreeQ[b, z] := distributeLists[Table[termsInVariable[b, z], {n}]];
termsInVariable[e_, z_] := {e};

(* a summand that is not split into poles (a denominator of degree three or more that does not factor, a function of z
   that is not rational) makes the sum, or the imaginary part, fail: the flag is set with the message *)
$poleFailure = False;
(* denominators of degree two (boson propagators) are split into linear factors, by Factor or by the roots *)
MatsubaraSum::degree = "The denominator `1` has degree `2` in `3`: only linear and quadratic denominators are split into poles.";
quadraticFactors[b_, z_] := Module[{x = Expand[b], c, s},
   If[Exponent[x, z] != 2, Message[MatsubaraSum::degree, b, Exponent[x, z], z]; $poleFailure = True; b,
    c = Table[Coefficient[x, z, j], {j, 0, 2}]; s = Sqrt[c[[2]]^2 - 4 c[[3]] c[[1]]];
    c[[3]] (z - (-c[[2]] + s)/(2 c[[3]])) (z - (-c[[2]] - s)/(2 c[[3]]))]];
(* a linear factor is written c (z - root), so that its poles give the denominators of the textbooks *)
linearFactor[b_, z_] := With[{x = Expand[b]}, With[{c = Coefficient[x, z]}, c (z - Expand[-(x /. z -> 0)/c])]];
linearFactorsOf[b_, z_] := Module[{f = Factor[b]},
   Times @@ Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
      Which[FreeQ[base, z], base, Exponent[Expand[base], z] <= 1, linearFactor[base, z], True, quadraticFactors[base, z]]^n],
     {p, factorList[f]}]];
linearizeDenominators[t_, z_] := t /. Power[b_, m_Integer?Negative] /; ! FreeQ[b, z] && Exponent[Expand[b], z] > 1 :>
    linearFactorsOf[b, z]^m;

(* the poles of a product t: {root, positions in factorList[t] of the factors 1/(c z + d)^m with root -d/c} *)
MatsubaraSum::pole = "The factor `1` contains `2` in a way that is not a power of a linear function: it is ignored in the search of poles.";
polesOf[t_, z_] := Module[{factors = factorList[t], found = {}},
   Do[With[{f = factors[[i]]},
     If[! FreeQ[f, z],
      If[Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] < 0 && Exponent[Expand[f[[1]]], z] == 1 &&
        FreeQ[Coefficient[Expand[f[[1]]], z], z],
       With[{b = Expand[f[[1]]]}, AppendTo[found, {Expand[-(b /. z -> 0)/Coefficient[b, z]], i}]],
       If[! (Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] > 0) && Head[f] =!= Plus && f =!= z,
        Message[MatsubaraSum::pole, f, z]; $poleFailure = True]]]], {i, Length[factors]}];
   ({#[[1, 1]], #[[All, 2]]} &) /@ GatherBy[found, First]];

(* the coefficient of 1/(z - root)^k in the Laurent expansion of t h(z) at a pole: the factors with that root are
   replaced by c psi, psi^M cancels, and the (M - k)-th derivative of what is left is taken at the root *)
laurentCoefficient[t_, z_, {root_, positions_List}, h_, k_Integer] := Module[{factors = factorList[t], psi, m, phi},
   m = poleOrder[factors, positions];
   phi = psi^m Times @@ Table[If[MemberQ[positions, i], (Coefficient[Expand[factors[[i, 1]]], z] psi)^factors[[i, 2]], factors[[i]]],
       {i, Length[factors]}];
   If[k > m, 0, (If[m == k, phi h, D[phi h, {z, m - k}]] /. z -> root)/(m - k)!]];
(* the order of a pole: the total power of its factors *)
poleOrder[factors_List, positions_List] := Total[Table[-factors[[i, 2]], {i, positions}]];
(* the principal parts of t at its poles in z: {root, k, coefficient of 1/(z - root)^k} for k = 1 ... the order *)
laurentTerms[t_, z_] := Join @@ Table[Table[{pole[[1]], k, laurentCoefficient[t, z, pole, 1, k]}, {k, poleOrder[factorList[t], pole[[2]]]}],
    {pole, polesOf[t, z]}];

(* zero, using the relations between statistical factors: n_F and n_B written with e^(beta e); approximate numbers close
   to zero are zero *)
thermalZeroQ[e_] := With[{x = Together[thermalExponentialForm[e]]}, x === 0 || Chop[x] === 0];
exponentialOf[a_] := With[{x = Expand[a]},
   Times @@ Table[thermalVariable[t/numericFactor[t]]^numericFactor[t], {t, If[Head[x] === Plus, List @@ x, If[x === 0, {}, {x}]]}]];
thermalFunctionInExponentials[h_, k_, a_] := Module[{x},
   Nest[inverseTemperature x D[#, x] &, If[h === FermiFunction, 1/(x + 1), 1/(x - 1)], k] /. x -> exponentialOf[a]];
thermalExponentialForm[e_] := e /. {Derivative[k_][h : FermiFunction | BoseFunction][a_] :> thermalFunctionInExponentials[h, k, a],
    (h : FermiFunction | BoseFunction)[a_] :> thermalFunctionInExponentials[h, 0, a]};

(* ---- the results written as in the textbooks ----
   Terms with the same denominator (up to its sign) are put together, n_F(a)/(z - a + b) + n_F(b)/(-z + a - b) =
   (n_F(a) - n_F(b))/(z - a + b). In their numerators, the statistical factors of a sum of energies that also occur
   alone, such as n_B(e1 + e2) = n_F(e1) n_F(e2)/(1 - n_F(e1) - n_F(e2)) after two fermionic sums, are written with
   those; and a polynomial in the factors n_i of the single energies is written as a sum over processes, each line
   emitted (1 - n_F, 1 + n_B) or absorbed (n_F, n_B), when this has fewer terms:
     N = Sum_S N(S) Prod_(i in S) n_i Prod_(i not in S) (1 - n_i)     (fermions; for bosons -n_i and 1 + n_i)
   with N(S) the value of N at n_i = 1 (bosons: -1) for i in S and n_i = 0 otherwise, as for
   n_2 (1 - n_1) (1 - n_3) + n_1 n_3 (1 - n_2) in the second-order self-energy. *)
energyTerms[a_] := With[{x = Expand[a]}, Which[x === 0, {}, Head[x] === Plus, List @@ x, True, {x}]];
singleEnergyQ[a_] := With[{t = energyTerms[a]}, Length[t] == 1 && numericFactor[t[[1]]] === 1];
singleStatisticalFactors[e_] := Union[Cases[{e}, (h : FermiFunction | BoseFunction)[a_?singleEnergyQ] :> h[a], Infinity]];
(* a factor of a sum of single energies, written with their factors (x = e^(e/T) = 1/n_F - 1 = 1 + 1/n_B) *)
compositeStatisticsRules[factors_List] := With[{energies = Table[f[[1]], {f, factors}],
    exponentials = Table[thermalVariable[f[[1]]] -> If[Head[f] === FermiFunction, 1/f - 1, 1 + 1/f], {f, factors}]},
   {(h : FermiFunction | BoseFunction)[a_] /; ! singleEnergyQ[a] &&
       Complement[Table[t/numericFactor[t], {t, energyTerms[a]}], energies] === {} :>
     (thermalFunctionInExponentials[h, 0, a] /. exponentials)}];
(* otherwise the expression is factored, when it is small *)
factoredForm[e_] := If[LeafCount[e] < 400, Factor[e], e];
textbookStatistics[e_] := Module[{factors = singleStatisticalFactors[e], f, vars, poly, signs, values, monomials, groups},
   Which[
    factors === {} || Length[factors] > 6 || Length[Union[Table[g[[1]], {g, factors}]]] < Length[factors], factoredForm[e],
    f = Together[e /. compositeStatisticsRules[factors]]; ! FreeQ[Denominator[f], FermiFunction | BoseFunction], factoredForm[e],
    True,
    vars = Table[Unique["FunctionalDiagrams`Private`occupation"], {Length[factors]}];
    poly = Expand[Numerator[f] /. Thread[factors -> vars]];
    signs = Table[If[Head[g] === FermiFunction, 1, -1], {g, factors}];
    values = Select[Table[{s, Together[(poly /. Thread[vars -> s signs])/Denominator[f]]}, {s, Tuples[{0, 1}, Length[factors]]}],
      #[[2]] =!= 0 &];
    monomials = Union[Table[Times @@ Select[factorList[t], MemberQ[vars, #] &], {t, productTermsOf[poly]}]];
    If[! AllTrue[vars, Exponent[poly, #] <= 1 &] || 2 Length[values] > Length[monomials], factoredForm[f],
     (* the processes with the same coefficient (up to its sign) together *)
     groups = GatherBy[values, Sort[{Expand[#[[2]]], Expand[-#[[2]]]}] &];
     Total[Table[With[{c = factoredForm[gr[[1, 2]]]}, c Total[Table[
          Together[v[[2]]/gr[[1, 2]]] Times @@ Table[With[{n = factors[[i]]}, If[v[[1, i]] == 1, signs[[i]] n, 1 - signs[[i]] n]],
            {i, Length[factors]}], {v, gr}]]], {gr, groups}]]]]];

(* the sign that makes a denominator i w + ... (its frequency term, if it has one) or else its leading term positive *)
denominatorSign[b_] := With[{x = Expand[b]}, With[{frequency = Select[energyTerms[x], Re[numericFactor[#]] == 0 &]},
    With[{c = numericFactor[If[frequency =!= {}, First[frequency], leadingTerm[x]]]}, If[Re[c] != 0, Sign[Re[c]], If[Im[c] < 0, -1, 1]]]]];
(* a term as {numerator, denominator}, the denominator a product of sums with a positive leading term *)
numeratorDenominator[t_] := Module[{num = 1, den = 1},
   Do[If[Head[f] === Power && IntegerQ[f[[2]]] && f[[2]] < 0 && Head[f[[1]]] === Plus,
     With[{s = denominatorSign[f[[1]]]}, num = num s^f[[2]]; den = den Expand[s f[[1]]]^(-f[[2]])], num = num f], {f, factorList[t]}];
   {num, den}];
collectDenominators[e_, numeratorForm_] := With[{pairs = numeratorDenominator /@ productTermsOf[e]},
   Total[(numeratorForm[Total[#[[All, 1]]]]/#[[1, 2]] &) /@ GatherBy[pairs, Last]]];

(* ---- the Matsubara sum ----
   T Sum_n g(i w_n) e^(i eta w_n 0+) = Sum_poles Res[g(z) n_F(z)] - (1 - eta)/2 c        (fermions)
   T Sum_n g(i nu_n) e^(i eta nu_n 0+) = -Sum_poles Res[g(z) n_B(z)] - (1 - eta)/2 c     (bosons)
   with c = lim z g(z) = Sum_poles Res[g]: n_F(z) has poles at i w_n with residue -T, n_B(z) at i nu_n with residue T,
   and the contour is closed around the poles of g. eta = 1 (e^(i w 0+)), -1, or 0 (symmetric sum). *)
MatsubaraSum::conv = "The summand decays only like 1/`1` at large frequency, and the sum depends on the convergence factor: the symmetric sum is returned (\"ConvergenceFactor\" -> 1 for e^(i w 0+), -1 for e^(-i w 0+)).";
MatsubaraSum::axis = "The summand has a pole at `1` = `2`, on the axis of the Matsubara frequencies: the sum is not defined (an anomalous term).";
MatsubaraSum::free = "The summand does not depend on `1`: the sum diverges.";
MatsubaraSum::diverge = "The summand does not decay at large `1`: the sum diverges.";
MatsubaraSum::factor = "\"ConvergenceFactor\" -> `1` is not one of 1 (e^(i w 0+)), -1 (e^(-i w 0+)), 0 (symmetric sum) and Automatic.";
MatsubaraSum::variable = "The variable of the sum, `1`, is not a symbol.";
EvaluateMatsubara::conv = "The sum over the loop frequency `1` depends on the convergence factor (its summand decays only like 1/z at large frequency), and no line at equal times gives one: the symmetric sum is taken. A boson line that tends to a constant at large frequency should be split, its constant given as an \"Instantaneous\" line.";
EvaluateMatsubara::diverge = "The summand of the loop frequency `1` does not decay at large frequency: the sum diverges (a boson line that does not vanish at large frequency?).";
EvaluateMatsubara::convergence = "\"ConvergenceFactor\" -> `1` is not one of 1 (e^(i w 0+) on the lines at equal times, G(0-) = <psibar psi>, the default), -1 (e^(-i w 0+), G(0+) = -<psi psibar>) and 0 (no factor, the symmetric sum, the mean of the two).";

(* unknown options give a message, as for the drawing functions *)
checkOptionNames[f_Symbol, opts_List, known_List] := Scan[If[! MemberQ[known, optionKey[#[[1]]]],
     Message[FunctionalDiagrams::optx, #[[1]], f, known]] &, opts];

MatsubaraSum[f_, z_Symbol, statistics_, opts___Rule] := With[{eta = optionValue[{opts}, {"ConvergenceFactor" -> Automatic}, "ConvergenceFactor"]},
   checkOptionNames[MatsubaraSum, {opts}, {"ConvergenceFactor"}];
   Which[
    ! MemberQ[{"Fermion", "Boson"}, statistics], Message[MatsubaraSum::statistics, statistics]; $Failed,
    ! MemberQ[{Automatic, 1, -1, 0}, eta], Message[MatsubaraSum::factor, eta]; $Failed,
    FreeQ[f, z] && f =!= 0, Message[MatsubaraSum::free, z]; $Failed,
    True, withFrequencySymbols[f, matsubaraSum[#1, z, statistics, eta, MatsubaraSum, z, #2] &]]];
MatsubaraSum[f_, z_, statistics_, opts___Rule] /; Head[z] =!= Symbol := (Message[MatsubaraSum::variable, z]; $Failed);

(* the degree in z of a product, and its leading coefficient *)
degreeInVariable[t_, z_] := Total[Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
     If[FreeQ[base, z], 0, n Exponent[Expand[base], z]]], {p, factorList[t]}]];
leadingCoefficientIn[t_, z_] := Times @@ Table[With[{base = baseAndExponent[p][[1]], n = baseAndExponent[p][[2]]},
     If[FreeQ[base, z], p, With[{x = Expand[base]}, Coefficient[x, z, Exponent[x, z]]^n]]], {p, factorList[t]}];
(* the degree of a sum, after its leading terms cancel *)
degreeOfSum[e_, z_] := With[{f = Together[e]}, With[{num = Expand[Numerator[f]]},
    Module[{k = Exponent[num, z]}, While[k >= 0 && thermalZeroQ[Coefficient[num, z, k]], k--]; k - Exponent[Denominator[f], z]]]];
(* how a sum of products decays at large z: like z^d, d = -2 standing for 1/z^2 or faster (the sum converges and does not
   depend on the convergence factor). It is read from the degrees of the terms; those of degree -1 decay like 1/z unless
   their leading coefficients cancel, and those that do not decay are put together, as they may cancel at leading order.
   (The value of the residues at infinity is not used: with numbers, its rounding error would look like a 1/z tail.) *)
decayDegree[terms_List, z_] := Module[{degrees = degreeInVariable[#, z] & /@ terms, top, d},
   top = Max[Append[Select[degrees, NumberQ], -2]];
   Which[
    top <= -2, -2,
    top == -1, If[thermalZeroQ[Total[Table[If[TrueQ[degrees[[i]] == -1], leadingCoefficientIn[terms[[i]], z], 0], {i, Length[terms]}]]], -2, -1],
    True, d = degreeOfSum[Total[Table[If[TrueQ[degrees[[i]] >= -1], terms[[i]], 0], {i, Length[terms]}]], z];
    Which[TrueQ[d >= 0], d, TrueQ[d == -1], -1, True, -2]]];

(* ---- sums of logarithms (the traces Tr ln of section 20) ----
   T Sum_n e^(i eta w_n 0+) ln(a - i w_n) = -T ln(1 - n_F(a)) + (1 - eta)/2 a = T ln(1 + e^(-a/T)) + (1 - eta)/2 a  (fermions)
   T Sum_n e^(i eta v_n 0+) ln(a - i v_n) = -T ln(1 + n_B(a)) + (1 - eta)/2 a = T ln(1 - e^(-a/T)) + (1 - eta)/2 a  (bosons)
   Their derivative in a is -MatsubaraSum[1/(z - a)] with the same convergence factor, and the constant is fixed by
   a -> +Infinity (an empty level, Z = 1). A constant factor in the logarithm gives nothing: T Sum_n e^(+-i w_n 0+) = 0,
   and the symmetric sum of 1 vanishes too (in zeta regularization the number of Matsubara frequencies is zero): the
   trace of the identity is zero. A rational argument is split into the linear factors of its numerator (zeros) and
   denominator (poles). A factor written ln(-a + i w_n) instead, i.e. with w_n -> -w_n (which reverses eta), gives the
   same for fermions and differs by i Pi T for bosons: the form whose energy has a positive leading sign is used, so that
   1/2 Tr ln of a real boson, ln(W^2 - z^2), gives the real W/2 + T ln(1 - e^(-W/T)) with every convergence factor. *)
MatsubaraSum::log = "The term `1` contains a logarithm of `2` that is not of the form c Log[r], with c free of `2` and r a rational function of `2`.";
statisticalLog[a_, "Fermion"] := -Temperature Log[1 - FermiFunction[a]];
statisticalLog[a_, "Boson"] := -Temperature Log[1 + BoseFunction[a]];
logSumOfEnergy[a_, statistics_, eta_] := If[leadingSign[a] < 0,
   statisticalLog[-a, statistics] - (1 + eta)/2 a,
   statisticalLog[a, statistics] + (1 - eta)/2 a];
(* the roots {root, multiplicity} of a polynomial in z, of degree two at most in each factor *)
polynomialRoots[p_, z_] := Join @@ Table[With[{base = Expand[baseAndExponent[f][[1]]], m = baseAndExponent[f][[2]]},
     Which[
      FreeQ[base, z], {},
      Exponent[base, z] == 1, {{Expand[-(base /. z -> 0)/Coefficient[base, z]], m}},
      Exponent[base, z] == 2, With[{c = Table[Coefficient[base, z, j], {j, 0, 2}]}, With[{s = Sqrt[c[[2]]^2 - 4 c[[3]] c[[1]]]},
         {{Expand[(-c[[2]] + s)/(2 c[[3]])], m}, {Expand[(-c[[2]] - s)/(2 c[[3]])], m}}]],
      True, Message[MatsubaraSum::degree, base, Exponent[base, z], z]; $poleFailure = True; {}]],
    {f, factorList[Factor[p]]}];
(* T Sum c Log[r(z)]: the zeros of r count +1, its poles -1. The convergence factor matters when the sum of the zeros
   differs from that of the poles (ln r decays like 1/z only); Automatic then gives the message and the symmetric sum *)
logSum[c_, r_, z_, statistics_, eta0_, caller_, name_] := Module[{f = Together[r], zeros, poles, tail, eta = eta0},
   zeros = polynomialRoots[Numerator[f], z]; poles = polynomialRoots[Denominator[f], z];
   tail = Total[(#[[1]] #[[2]] &) /@ zeros] - Total[(#[[1]] #[[2]] &) /@ poles];
   If[eta === Automatic, If[thermalZeroQ[tail], eta = 1,
     If[caller === MatsubaraSum, Message[MatsubaraSum::conv, name], Message[EvaluateMatsubara::conv, name]]; eta = 0]];
   c (Total[(#[[2]] logSumOfEnergy[#[[1]], statistics, eta] &) /@ zeros] - Total[(#[[2]] logSumOfEnergy[#[[1]], statistics, eta] &) /@ poles])];
logTermQ[t_, z_] := ! FreeQ[t, Log[a_] /; ! FreeQ[a, z]];
splitLogTerm[t_, z_] := With[{dependent = Select[factorList[t], ! FreeQ[#, z] &]},
   If[Length[dependent] == 1 && Head[dependent[[1]]] === Log && FreeQ[dependent[[1, 1]], Log],
    {Times @@ Select[factorList[t], FreeQ[#, z] &], dependent[[1, 1]]}, None]];
matsubaraSum[f_, z_, statistics_, eta0_, caller_, name_, shown_List] /; logTermQ[f, z] := Block[{$poleFailure = False},
   Module[{terms = termsInVariable[f, z], logs, rest, split, logPart, restPart},
    logs = Select[terms, logTermQ[#, z] &]; rest = Select[terms, ! logTermQ[#, z] &];
    split = splitLogTerm[#, z] & /@ logs;
    If[MemberQ[split, None],
     Message[MatsubaraSum::log, logs[[First[First[Position[split, None, {1}]]]]] /. shown, z]; $Failed,
     logPart = Total[logSum[#[[1]], #[[2]], z, statistics, eta0, caller, name] & /@ split];
     restPart = If[rest === {}, 0, matsubaraSum[Total[rest], z, statistics, eta0, caller, name, shown]];
     If[$poleFailure || restPart === $Failed, $Failed, Expand[logPart + restPart]]]]];

(* the sum, for MatsubaraSum and EvaluateMatsubara (caller); name is the frequency of the messages and shown the rules
   that write their expressions with the loop frequencies. A summand that does not decay gives $Failed; one that decays
   like 1/z gives the term of the convergence factor, with c = lim z g(z) the sum of the residues of g *)
matsubaraSum[f_, z_, statistics_, eta0_, caller_, name_, shown_List] := Block[{$poleFailure = False}, Module[
    {eta = eta0, terms, decay, tail = 0, total = 0, poles, result},
    terms = DeleteCases[linearizeDenominators[#, z] & /@ termsInVariable[f, z], 0];
    decay = decayDegree[terms, z];
    If[decay >= 0,
     If[caller === MatsubaraSum, Message[MatsubaraSum::diverge, name], Message[EvaluateMatsubara::diverge, name]]; $Failed,
     Do[poles = polesOf[t, z];
      (* a pole on the axis of the frequencies summed: i w_n (fermions), i nu_n (bosons); a pole at a frequency of the
         other statistics is an ordinary pole, n_F(i nu_m) = n_F(0) = 1/2, n_B(i w_n) = -n_F(0) = -1/2 *)
      Scan[With[{shift = matsubaraShift[#[[1]]]}, If[Expand[shift[[1]]] === 0 && shift[[2]] == If[statistics === "Fermion", 1, 0],
          Message[MatsubaraSum::axis, If[caller === MatsubaraSum, z, I name], #[[1]] /. shown]]] &, poles];
      total += Total[laurentCoefficient[t, z, #, statisticalFunction[statistics][z], 1] & /@ poles];
      If[decay == -1, tail += Total[laurentCoefficient[t, z, #, 1, 1] & /@ poles]], {t, terms}];
     If[decay == -1 && eta === Automatic,
      If[caller === MatsubaraSum, Message[MatsubaraSum::conv, name], Message[EvaluateMatsubara::conv, name]]; eta = 0];
     result = collectDenominators[Expand[simplifyStatisticalFactors[If[statistics === "Fermion", total, -total] -
          If[decay == -1, (1 - eta)/2 tail, 0]]], Identity];
     If[$poleFailure, $Failed, result]]]];

(* ---- routing of momenta and frequencies through a diagram ---- *)

EvaluateMatsubara::rule = "`1` is not a line rule: use head -> {\"Fermion\", dispersion}, head -> {\"Instantaneous\", potential} or head -> {\"Boson\", Function[{q, z}, propagator]} (z = i nu).";
EvaluateMatsubara::factor = "The factor `1` is neither a line of the rules, a delta function nor a constant.";
EvaluateMatsubara::grassmann = "A term contains Grassmann-odd factors `1`: only diagrams with the sources set to zero are evaluated.";
EvaluateMatsubara::conservation = "The external momenta and frequencies do not add up to zero on a connected part of a diagram (`1`).";
EvaluateMatsubara::frequency = "The external frequency `1` has no statistics: declare it with DeclareMatsubaraFrequency.";
(* a rule head -> {kind, value} with one of the kinds; the others give the message caller::rule *)
lineRuleQ[r_, kinds_List] := MatchQ[r, (Rule | RuleDelayed)[_Symbol, {_String, _}]] && MemberQ[kinds, r[[2, 1]]];
checkRuleKinds[rules_List, kinds_List, caller_Symbol] := With[{bad = Select[rules, ! lineRuleQ[#, kinds] &]},
   Scan[Message[MessageName[caller, "rule"], #] &, bad]; bad === {}];
checkLineRules[rules_List] := checkRuleKinds[rules, {"Fermion", "Instantaneous", "Boson"}, EvaluateMatsubara];
checkExternalFrequencies[externals_List] := With[{bad = Select[Union[Cases[Table[e[[2, 2]], {e, externals}], _Symbol, {0, Infinity}]],
      Context[#] =!= "System`" && frequencyStatistics[#] === None &]},
   Scan[Message[EvaluateMatsubara::frequency, #] &, bad]; bad === {}];
(* each external point is label -> {momentum, frequency}, the frequency I times an integer combination of declared
   Matsubara frequencies (or 0) *)
EvaluateMatsubara::external = "`1` is not an external point label -> {momentum, I w}, with w a Matsubara frequency declared by DeclareMatsubaraFrequency (or an integer combination of them, or 0).";
EvaluateMatsubara::loopnames = "\"LoopMomenta\" -> `1` is neither Automatic nor a list of names.";
EvaluateMatsubara::loops = "\"LoopMomenta\" -> `1` gives fewer names than the `2` loops of a diagram.";
matsubaraCombinationQ[f_] := With[{x = Expand[f], ws = Union[Cases[{f}, s_Symbol /; Context[s] =!= "System`", Infinity]]},
   Expand[x /. Table[s -> 0, {s, ws}]] === 0 && AllTrue[ws, IntegerQ[Coefficient[x, #]/I] &]];
checkExternals[externals_List] := With[{bad = Select[externals, ! MatchQ[#, (Rule | RuleDelayed)[_, {_, _}]] &]},
   Scan[Message[EvaluateMatsubara::external, #] &, bad];
   bad === {} && checkExternalFrequencies[externals] &&
    With[{wrong = Select[externals, ! matsubaraCombinationQ[#[[2, 2]]] &]}, Scan[Message[EvaluateMatsubara::external, #] &, wrong]; wrong === {}]];
checkLoopNames[names_] := If[names === Automatic || ListQ[names], True, Message[EvaluateMatsubara::loopnames, names]; False];

(* the sign of the first term, in canonical order, of a momentum or a frequency *)
firstTermSign[e_] := With[{x = Expand[e]}, Which[x === 0, 0, Head[x] === Plus, signOfTerm[First[x]], True, signOfTerm[x]]];
(* the value of a line with momentum p and frequency nu (i nu is its Matsubara frequency) flowing along it; an
   instantaneous interaction and a boson propagator are even, and are written with the momentum of positive sign *)
flowSign[p_, nu_] := If[Expand[p] =!= 0, firstTermSign[p], If[firstTermSign[nu] < 0, -1, 1]];
lineValue[{"Fermion", disp_}, {p_, nu_}] := 1/(I nu - disp[p]);
lineValue[{"Instantaneous", v_}, {p_, nu_}] := v[Expand[flowSign[p, 0] p]];
lineValue[{"Boson", f_}, {p_, nu_}] := With[{s = flowSign[p, nu]}, f[Expand[s p], Expand[I s nu]]];

(* A term of the package as a graph: points (labels) joined by delta functions are one node; lines go from their second
   to their first argument. Spanning tree (instantaneous and boson lines first), a loop momentum and frequency on each
   chord, and the flows of the tree lines from conservation at the nodes, peeling the leaves of the tree.
   Result: rules with the data of the routed term. The messages are those of caller (EvaluateMatsubara by default):
   caller::grassmann, ::factor, ::loops and ::conservation. *)
routeTerm[t : diagramTerm[c_, o_, d_], externals_List, rules_List, loopNames_, caller_Symbol: EvaluateMatsubara] := Catch[Module[
    {heads = Table[r[[1]], {r, rules}], kindOf, labels, factors, lines = {}, deltas = {}, constants = {}, points, index,
     components, nodeOf, nNodes, extFlow, comp, chords = {}, tree = {}, flows, remaining, u, e, net, roots, nLoops,
     loopMomenta, loopFrequencies, times, timeOf, lineData, equalTime},
    If[o =!= {}, Message[MessageName[caller, "grassmann"], o]; Throw[$Failed, routeFailure]];
    kindOf[h_] := First[Select[rules, #[[1]] === h &]][[2]];
    labels = Join[Table[x[[1]], {x, externals}], d];
    factors = expandIntegerPowers[factorList[c]];
    Do[Which[
      ! AtomQ[f] && MemberQ[heads, Head[f]] && Length[f] == 2, AppendTo[lines, {f[[2]], f[[1]], Head[f]}],
      Head[f] === DeltaFunction && Length[f] == 2, AppendTo[deltas, List @@ f],
      FreeQ[f, Alternatives @@ labels], AppendTo[constants, f],
      True, Message[MessageName[caller, "factor"], f]; Throw[$Failed, routeFailure]], {f, factors}];
    (* nodes *)
    points = Union[Table[x[[1]], {x, externals}], Flatten[Table[{l[[1]], l[[2]]}, {l, lines}]], Flatten[deltas]];
    index[x_] := First[First[Position[points, x, {1}]]];
    components = graphComponents[Length[points], Map[index, deltas, {2}]];
    nNodes = Length[components];
    nodeOf[x_] := First[First[Position[components, index[x]]]];
    lineData = Table[{nodeOf[l[[1]]], nodeOf[l[[2]]], l[[3]], kindOf[l[[3]]]}, {l, lines}];
    extFlow = Table[{0, 0}, {nNodes}];
    Do[With[{n = nodeOf[x[[1]]]}, extFlow[[n]] = extFlow[[n]] + {x[[2, 1]], x[[2, 2]]/I}], {x, externals}];
    (* spanning tree *)
    comp = Range[nNodes];
    Do[With[{a = comp[[lineData[[j, 1]]]], b = comp[[lineData[[j, 2]]]]},
      If[a =!= b, AppendTo[tree, j]; comp = comp /. b -> a, AppendTo[chords, j]]],
     {j, Join[Select[Range[Length[lines]], lineData[[#, 4, 1]] =!= "Fermion" &], Select[Range[Length[lines]], lineData[[#, 4, 1]] === "Fermion" &]]}];
    nLoops = Length[chords];
    If[ListQ[loopNames] && Length[loopNames] < nLoops, Message[MessageName[caller, "loops"], loopNames, nLoops]; Throw[$Failed, routeFailure]];
    loopMomenta = If[ListQ[loopNames], Take[loopNames, nLoops], Table[LoopMomentum[i], {i, nLoops}]];
    loopFrequencies = Table[LoopFrequency[i, If[lineData[[chords[[i]], 4, 1]] === "Fermion", "Fermion", "Boson"]], {i, nLoops}];
    flows = Table[None, {Length[lines]}];
    Do[flows[[chords[[i]]]] = {loopMomenta[[i]], loopFrequencies[[i]]}, {i, nLoops}];
    (* conservation on each connected part *)
    roots = Table[First[Select[Range[nNodes], comp[[#]] === r &]], {r, Union[comp]}];
    Do[If[Expand[Total[Table[extFlow[[n]], {n, Select[Range[nNodes], comp[[#]] === comp[[r]] &]}]]] =!= {0, 0},
      Message[MessageName[caller, "conservation"], termToExpression[t]]; Throw[$Failed, routeFailure]], {r, roots}];
    remaining = tree;
    While[remaining =!= {},
     u = First[Select[Complement[Range[nNodes], roots],
        Count[remaining, j_ /; lineData[[j, 1]] == # || lineData[[j, 2]] == #] == 1 &]];
     e = First[Select[remaining, lineData[[#, 1]] == u || lineData[[#, 2]] == u &]];
     net = extFlow[[u]] + Total[Table[If[flows[[j]] === None, 0,
          If[lineData[[j, 2]] == u, flows[[j]], 0] - If[lineData[[j, 1]] == u, flows[[j]], 0]], {j, Length[lines]}]];
     flows[[e]] = Expand[If[lineData[[e, 2]] == u, -net, net]];
     remaining = DeleteCases[remaining, e]];
    (* time vertices: nodes joined by instantaneous interactions are at equal times *)
    times = graphComponents[nNodes, Table[{l[[1]], l[[2]]}, {l, Select[lineData, #[[4, 1]] === "Instantaneous" &]}]];
    timeOf = Table[First[First[Position[times, n]]], {n, nNodes}];
    equalTime = Select[Range[Length[lines]], lineData[[#, 4, 1]] === "Fermion" && timeOf[[lineData[[#, 1]]]] == timeOf[[lineData[[#, 2]]]] &];
    {"Coefficient" -> Times @@ constants, "ClosedLoops" -> closedLoopCountOfTerm[t, Select[heads, kindOf[#][[1]] === "Fermion" &]],
     "Integrand" -> Times @@ Table[lineValue[lineData[[j, 4]], flows[[j]]], {j, Length[lines]}],
     "LoopMomenta" -> loopMomenta, "LoopFrequencies" -> loopFrequencies,
     "EqualTimeFrequency" -> Expand[Total[Table[flows[[j, 2]], {j, equalTime}]]],
     (* each line: {its start node, its end node, head, kind, {momentum, frequency}, {start label, end label}} *)
     "Lines" -> Table[Join[lineData[[j]], {flows[[j]], Take[lines[[j]], 2]}], {j, Length[lines]}], "TimeOf" -> timeOf,
     "ExternalNodes" -> Table[x[[1]] -> nodeOf[x[[1]]], {x, externals}]}], routeFailure];

routeData[key_String, data_List] := key /. data;
(* the factors that do not depend on the loop momenta are taken out of the momentum sum: those of a product, and those
   common to all the terms of a sum (with the sign, when all the terms are negative) *)
momentumSum[0, _] := 0;
momentumSum[f_, {}] := f;
momentumSum[f_Plus, ps_List] := Module[{terms = List @@ f, common},
   common = Select[factorList[First[terms]], ! NumberQ[#] && FreeQ[#, Alternatives @@ ps] &];
   common = Times @@ Select[common, Function[c, AllTrue[Rest[terms], MemberQ[factorList[#], c] &]]];
   If[AllTrue[terms, TrueQ[Re[numericFactor[#]] < 0] &], common = -common];
   common MomentumSum[Total[Table[t/common, {t, terms}]], ps]];
momentumSum[f_, ps_List] := With[{outside = Times @@ Select[factorList[f], FreeQ[#, Alternatives @@ ps] &]},
   outside MomentumSum[f/outside, ps]];

(* the frequencies of the loops are summed one after the other; the convergence factor of each sum comes from the
   lines at equal times that carry its frequency: e^(i eta0 w 0+) for the frequency w along each of them (eta0 the
   option "ConvergenceFactor": 1, -1 or 0). During the sums the loop frequencies are symbols (withFrequencySymbols);
   $Failed if a sum fails *)
sumLoopFrequencies[integrand_, frequencies_List, exponent_, eta0_] := withFrequencySymbols[{integrand, frequencies, exponent},
   Function[{data, shown}, Module[{g = data[[1]], z},
     Catch[Do[With[{f = data[[2, i]], sign = Sign[Coefficient[data[[3]], data[[2, i]]]]},
        g = matsubaraSum[g /. f -> -I z, z, frequencyStatistics[f], If[sign == 0, Automatic, eta0 sign], EvaluateMatsubara, frequencies[[i]], shown];
        If[g === $Failed, Throw[$Failed, sumFailure]]], {i, Length[frequencies]}];
      g, sumFailure]]]];

(* the options, checked by name: {spin, loop names, convergence factor}; DiagramIntegrand sums no frequency and has no
   convergence factor. The routed terms: $Failed if the rules, the external points or the options are not valid *)
matsubaraOptions[f_Symbol, opts_List] := (checkOptionNames[f, opts,
    Join[{"SpinDegeneracy", "LoopMomenta"}, If[f === DiagramIntegrand, {}, {"ConvergenceFactor"}]]];
   {optionValue[opts, {"SpinDegeneracy" -> 1}, "SpinDegeneracy"], optionValue[opts, {"LoopMomenta" -> Automatic}, "LoopMomenta"],
    optionValue[opts, {"ConvergenceFactor" -> 1}, "ConvergenceFactor"]});
checkConvergenceFactor[eta_] := If[MemberQ[{1, -1, 0}, eta], True, Message[EvaluateMatsubara::convergence, eta]; False];
(* the lines of the package in momentum and frequency are directed, 1/(i w - xi_k), or bosonic: an antisymmetric two-label
   kernel (an anomalous propagator -<T psi psi>, a superfield propagator) is refused rather than misread *)
EvaluateMatsubara::anomalous = "The antisymmetric kernels `1` (anomalous or superfield propagators) cannot be evaluated in momentum and frequency yet: write the expression with the normal propagators (SuperfieldComponents at zero anomalous propagators).";
anomalousLinesFree[e_] := With[{heads = Select[Union[Cases[{e}, h_Symbol[_, _] :> h, Infinity]], MemberQ[$antisymmetricHeads, #] &]},
   If[heads === {}, True, Message[EvaluateMatsubara::anomalous, heads]; False]];
routedTerms[e_, externals_List, rules_List, loopNames_, eta_: 1] := If[! anomalousLinesFree[e] || ! checkLineRules[rules] || ! checkExternals[externals] ||
    ! checkLoopNames[loopNames] || ! checkConvergenceFactor[eta], $Failed, routeTerm[#, externals, rules, loopNames] & /@ canonicalTermsOf[e]];

(* the weight of a routed term: its coefficient and the spin factor of its closed fermion loops *)
weightOf[data_List, spin_] := routeData["Coefficient", data] spin^routeData["ClosedLoops", data];
(* the integrand after the frequency sums *)
summedIntegrand[data_List, eta_] := sumLoopFrequencies[routeData["Integrand", data], routeData["LoopFrequencies", data],
   routeData["EqualTimeFrequency", data], eta];

DiagramIntegrand[e_, externals_List, rules_List, opts___Rule] := Module[{spin, names, routed},
   {spin, names} = Take[matsubaraOptions[DiagramIntegrand, {opts}], 2];
   routed = routedTerms[e, externals, rules, names];
   If[routed === $Failed, $Failed,
    If[# === $Failed, $Failed, {"Weight" -> weightOf[#, spin],
        "Integrand" -> routeData["Integrand", #], "LoopMomenta" -> routeData["LoopMomenta", #],
        "LoopFrequencies" -> routeData["LoopFrequencies", #], "EqualTimeFrequency" -> routeData["EqualTimeFrequency", #]}] & /@ routed]];

(* partial fractions in the external frequency w (a function of one external frequency): a term that vanishes at large
   z = I w is replaced by the principal parts at its poles, (n_B + 1 - n_F)/(I w - xi - Omega) + ... (a term whose
   denominators are not split into poles is kept as it is) *)
principalParts[t_, z_] := Total[Table[c[[3]]/linearFactor[z - c[[1]], z]^c[[2]], {c, laurentTerms[t, z]}]];
properInQ[t_, z_] := Exponent[Numerator[t], z] < Exponent[Denominator[t], z];
externalPartialFractions[e_, None] := e;
externalPartialFractions[e_, w_] := Module[{z},
   Total[Table[If[FreeQ[t, z] || ! properInQ[t, z], t,
       Block[{$poleFailure = False}, With[{parts = principalParts[linearizeDenominators[t, z], z]}, If[$poleFailure, t, parts]]]],
      {t, termsInVariable[e /. w -> -I z, z]}]] /. z -> I w];
externalFrequency[externals_List] := With[{ws = Union[Cases[Table[x[[2, 2]], {x, externals}], s_Symbol /; Context[s] =!= "System`", {0, Infinity}]]},
   If[Length[ws] == 1, ws[[1]], None]];

(* The parts of a term between the lines that carry the external frequency w and no loop frequency: the external legs of
   a function that is not amputated (G = G0 Sigma G0 + ...), the lines that join its irreducible parts. Partial fractions
   across such a line would evaluate the parts on the poles of one another (the self-energy of G0 Sigma G0 on shell), with
   denominators free of w that vanish there: they are taken in each part, and these lines are kept as factors.
   Result: {the product of these lines, {{integrand, loop frequencies} of each part}}; the lines that carry neither w nor
   a loop frequency go with the first part. An amputated two-point function has no such line: one part, the whole term *)
externalParts[data_List, w_] := Module[{lines = routeData["Lines", data], fs = routeData["LoopFrequencies", data], loopsOf, valueOf,
    joining, inner, constant, components, parts},
   (* the loops whose frequency flows through a line: momenta and frequencies are routed along the same loops *)
   loopsOf[l_] := Select[Range[Length[fs]], ! FreeQ[l[[5, 2]], fs[[#]]] &];
   valueOf[j_] := lineValue[lines[[j, 4]], lines[[j, 5]]];
   joining = If[w === None, {}, Select[Range[Length[lines]], loopsOf[lines[[#]]] === {} && ! FreeQ[Expand[lines[[#, 5, 2]]], w] &]];
   If[joining === {}, {1, {{routeData["Integrand", data], fs}}},
    inner = Select[Range[Length[lines]], loopsOf[lines[[#]]] =!= {} &];
    constant = Complement[Range[Length[lines]], joining, inner];
    components = Sort[Sort /@ graphComponents[Length[fs], Flatten[Table[With[{ls = loopsOf[lines[[j]]]},
         Table[{ls[[1]], m}, {m, Rest[ls]}]], {j, inner}], 1]]];
    parts = Table[{Times @@ (valueOf /@ Select[inner, Intersection[loopsOf[lines[[#]]], c] =!= {} &]), fs[[c]]}, {c, components}];
    If[parts === {}, parts = {{1, {}}}];
    parts[[1, 1]] = parts[[1, 1]] Times @@ (valueOf /@ constant);
    {Times @@ (valueOf /@ joining), parts}]];

(* a routed term after the sums, in partial fractions part by part; $Failed if a sum fails *)
evaluatedTerm[data_List, spin_, w_, eta_] := Module[{parts = externalParts[data, w], sums},
   sums = Table[sumLoopFrequencies[part[[1]], part[[2]], routeData["EqualTimeFrequency", data], eta], {part, parts[[2]]}];
   If[MemberQ[sums, $Failed], $Failed,
    weightOf[data, spin] parts[[1]] momentumSum[Times @@ Table[collectDenominators[externalPartialFractions[s, w], textbookStatistics],
       {s, sums}], routeData["LoopMomenta", data]]]];

(* ---- traces of operators (Tr ln A, Tr A) in a translation-invariant system ----
   An operator made of kernels with line rules is diagonal in momentum and frequency: G0 -> 1/(z - xi[k]) ("Fermion"),
   v[k] ("Instantaneous", e.g. a static self-energy), f[k, z] ("Boson"), with z = i w. Per unit of beta times volume,
   Tr ln A = (2s + 1) Integral_k T Sum_n e^(i eta w_n 0+) ln A(k, i w_n), and Tr A the same with A: the trace is taken at
   equal times, with the convergence factor of the option "ConvergenceFactor" (1, the default: the ordering psibar psi,
   normal order). The frequencies are fermionic when a kernel of A has a "Fermion" rule, bosonic otherwise. In Tr A the
   constant part of A(k, z) is dropped (Tr 1 = 0, as in the sums of logarithms); a part that grows with z diverges. *)
EvaluateMatsubara::trace = "The term `1` is not a number times one trace (TraceLog, OperatorTrace) free of labels.";
EvaluateMatsubara::kernel = "The kernels `1` of the operator `2` have no line rule.";
EvaluateMatsubara::tracegrows = "The trace of `1` diverges: its summand grows at large frequency.";
EvaluateMatsubara::limit = "\"Limit\" -> `1` is not one of None, \"Static\" (all external momenta and frequencies 0) and \"Dynamic\" (external momenta 0, then the external frequency -> 0).";
kernelMomentumValue[{"Fermion", disp_}, k_, z_] := 1/(z - disp[k]);
kernelMomentumValue[{"Instantaneous", v_}, k_, z_] := v[k];
kernelMomentumValue[{"Boson", f_}, k_, z_] := f[k, z];
operatorSymbols[a_] := Union[Cases[a /. Dot -> List, s_Symbol /; Context[s] =!= "System`", {0, Infinity}]];
operatorMomentumValue[a_, rules_List, k_, z_] := (a /. Dot -> Times) /. Table[r[[1]] -> kernelMomentumValue[r[[2]], k, z], {r, rules}];
traceStatistics[a_, rules_List] := If[MemberQ[Table[If[MemberQ[operatorSymbols[a], r[[1]]], r[[2, 1]], None], {r, rules}], "Fermion"],
   "Fermion", "Boson"];
traceValue[tr : (TraceLog | OperatorTrace)[a_], rules_List, spin_, k_, eta_] := Module[{z, missing, value, stat, num, den, quotient, summand, sum},
   missing = Complement[operatorSymbols[a], rules[[All, 1]]];
   Which[
    NumericQ[a], 0,
    missing =!= {}, Message[EvaluateMatsubara::kernel, missing, a]; $Failed,
    True,
    value = Together[operatorMomentumValue[a, rules, k, z]];
    stat = traceStatistics[a, rules];
    If[Head[tr] === TraceLog, summand = Log[value],
     num = Expand[Numerator[value]]; den = Denominator[value];
     quotient = PolynomialQuotient[num, Expand[den], z];
     If[! FreeQ[quotient, z], Message[EvaluateMatsubara::tracegrows, a]; Return[$Failed]];
     summand = Together[PolynomialRemainder[num, Expand[den], z]/den]];
    sum = If[summand === 0 || FreeQ[summand, z], 0, matsubaraSum[summand, z, stat, eta, EvaluateMatsubara, z, {}]];
    If[sum === $Failed, $Failed, spin momentumSum[sum, {k}]]]];
(* the limits of a function of one external momentum q and one external frequency: "Static" sets both to zero (the
   limit q -> 0 at zero frequency, the thermodynamic response: at q = 0 and frequency 0 double poles give n_F'),
   "Dynamic" sets the momenta to zero and takes the frequency to zero afterwards (zero for a conserved quantity) *)
EvaluateMatsubara::limitterms = "The terms of the function at zero momentum that are singular as `1` -> 0 do not cancel as they are written (they may cancel after a change of the loop momenta, as at second order, or the approximation does not conserve the quantity): the function at zero momentum is returned.";
(* the limit w -> 0 of a rational function of w, by l'Hopital's rule; None if it is singular *)
frequencyLimit[e_, w_] := Module[{x = Together[e], num, den, k = 0},
   num = Numerator[x]; den = Denominator[x];
   While[k < 6 && thermalZeroQ[den /. w -> 0],
    If[! thermalZeroQ[num /. w -> 0], Return[None]];
    num = D[num, w]; den = D[den, w]; k++];
   Together[(num /. w -> 0)/(den /. w -> 0)]];
mergedMomentumSums[e_] := Module[{terms = productTermsOf[Expand[e]], sums, other, keys},
   sums = Select[terms, Count[factorList[#], _MomentumSum] == 1 &];
   other = Total[Select[terms, Count[factorList[#], _MomentumSum] != 1 &]];
   sums = Table[With[{m = First[Cases[factorList[t], _MomentumSum]]}, {m[[2]], (t/m) m[[1]]}], {t, sums}];
   keys = Union[sums[[All, 1]]];
   {other, Table[{key, Total[Select[sums, #[[1]] === key &][[All, 2]]]}, {key, keys}]}];
(* the terms are put together by their loop momenta, and the limit is taken in each group *)
dynamicLimit[e_, w_] := Module[{m = mergedMomentumSums[e], outside, groups},
   outside = frequencyLimit[m[[1]], w];
   groups = Table[{s[[1]], frequencyLimit[s[[2]], w]}, {s, m[[2]]}];
   If[outside === None || MemberQ[groups[[All, 2]], None], Message[EvaluateMatsubara::limitterms, w]; e,
    outside + Total[Table[momentumSum[s[[2]], s[[1]]], {s, groups}]]]];

traceOrLimitQ[e_, opts_List] := ! FreeQ[e, TraceLog | OperatorTrace] || MemberQ[optionKey /@ opts[[All, 1]], "Limit"];
EvaluateMatsubara[e_, externals_List, rules_List, opts___Rule] /; traceOrLimitQ[e, {opts}] := Module[
   {limit = optionValue[{opts}, {"Limit" -> None}, "Limit"], rest = Select[{opts}, optionKey[#[[1]]] =!= "Limit" &], spin, names, eta,
    terms, traceTerms, diagrams, k, values, diagramValue, w, ext},
   {spin, names, eta} = {optionValue[rest, {"SpinDegeneracy" -> 1}, "SpinDegeneracy"], optionValue[rest, {"LoopMomenta" -> Automatic}, "LoopMomenta"],
     optionValue[rest, {"ConvergenceFactor" -> 1}, "ConvergenceFactor"]};
   If[! MemberQ[{None, "Static", "Dynamic"}, limit], Message[EvaluateMatsubara::limit, limit]; Return[$Failed]];
   ext = Switch[limit, None, externals, "Static", (#[[1]] -> {0, 0} &) /@ externals, "Dynamic", (#[[1]] -> {0, #[[2, 2]]} &) /@ externals];
   terms = productTermsOf[Expand[e]];
   traceTerms = Select[terms, ! FreeQ[#, TraceLog | OperatorTrace] &];
   diagrams = Total[Select[terms, FreeQ[#, TraceLog | OperatorTrace] &]];
   k = If[ListQ[names] && names =!= {}, First[names], LoopMomentum[1]];
   values = Table[With[{traces = Select[factorList[t], MatchQ[#, _TraceLog | _OperatorTrace] &]},
      If[Length[traces] != 1 || ! FreeQ[t/traces[[1]], TraceLog | OperatorTrace | IntegralOver | DeltaFunction] || externals =!= {},
       Message[EvaluateMatsubara::trace, t]; $Failed,
       With[{v = traceValue[traces[[1]], rules, spin, k, eta]}, If[v === $Failed, $Failed, (t/traces[[1]]) v]]]], {t, traceTerms}];
   diagramValue = If[diagrams === 0, 0, EvaluateMatsubara[diagrams, ext, rules, Sequence @@ rest]];
   Which[
    MemberQ[Append[values, diagramValue], $Failed], $Failed,
    limit === "Dynamic" && (w = externalFrequency[externals]) =!= None, Total[values] + dynamicLimit[diagramValue, w],
    True, Total[values] + diagramValue]];

(* $Failed if a term fails *)
EvaluateMatsubara[e_, externals_List, rules_List, opts___Rule] := Module[{spin, names, eta, routed, w, results},
   {spin, names, eta} = matsubaraOptions[EvaluateMatsubara, {opts}];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed, $Failed,
    w = externalFrequency[externals];
    results = Table[If[data === $Failed, $Failed, evaluatedTerm[data, spin, w, eta]], {data, routed}];
    If[MemberQ[results, $Failed], $Failed, Total[results]]]];

(* ---- analytic continuation and imaginary part ---- *)

AnalyticContinuation::periodicity = "Statistical factors still depend on `1`: declare the statistics of the frequencies (DeclareMatsubaraFrequency) before the sums.";
AnalyticContinuation[e_, w_Symbol -> omega_] := (
   If[! FreeQ[Cases[{e}, (FermiFunction | BoseFunction)[_] | Derivative[_][FermiFunction | BoseFunction][_], Infinity], w],
    Message[AnalyticContinuation::periodicity, w]];
   e /. w -> -I omega);

(* 1/(x + i 0+)^k = P 1/x^k - i Pi (-1)^(k - 1)/(k - 1)! delta^(k - 1)(x); the coefficients are taken to be real *)
energyDelta[0, x_] := EnergyDelta[x];
energyDelta[n_Integer, x_] := Derivative[n][EnergyDelta][x];
(* inside a momentum sum; the factors outside that depend on omega (external lines) are taken inside first *)
imaginaryPartOfTerm[t_, omega_] := Module[{ms = Select[factorList[t], Head[#] === MomentumSum &]},
   If[ms =!= {}, With[{outside = t/ms[[1]]}, If[FreeQ[outside, omega],
      outside momentumSum[retardedImaginaryPartOf[ms[[1, 1]], omega], ms[[1, 2]]],
      momentumSum[retardedImaginaryPartOf[outside ms[[1, 1]], omega], ms[[1, 2]]]]],
    Total[Table[c[[3]] (-Pi) (-1)^(c[[2]] - 1)/(c[[2]] - 1)! energyDelta[c[[2]] - 1, Expand[omega - c[[1]]]], {c, laurentTerms[t, omega]}]]]];
retardedImaginaryPartOf[e_, omega_] := collectDeltas[Total[imaginaryPartOfTerm[#, omega] & /@
     (linearizeDenominators[#, omega] & /@ termsInVariable[e, omega])]];
(* $Failed if a factor is not split into poles *)
RetardedImaginaryPart[e_, omega_] := Block[{$poleFailure = False}, With[{result = retardedImaginaryPartOf[e, omega]},
    If[$poleFailure, $Failed, result]]];

(* a sum of delta functions, each with its coefficient written as in the textbooks *)
collectDeltas[e_] := If[! FreeQ[e, MomentumSum], e, Total[(textbookStatistics[#[[2]]] #[[1]] &) /@ deltaCoefficients[e]]];

(* the coefficients of the delta functions: {delta, coefficient} *)
deltaCoefficients[e_] := Module[{terms = termsInVariable[e, EnergyDelta], pairs},
   pairs = Table[With[{ds = Select[factorList[t], ! FreeQ[#, EnergyDelta] &]}, {Times @@ ds, t/(Times @@ ds)}], {t, terms}];
   ({#[[1, 1]], Total[#[[All, 2]]]} &) /@ GatherBy[Select[pairs, #[[1]] =!= 1 &], First]];

(* ---- the cutting rules at finite temperature ----
   A two-point function F whose propagating lines form chains from the time vertex where the external frequency enters
   (in) to the one where it leaves (out); a chain passes through vertices with static insertions only (Hartree, Fock:
   lines and interactions at equal times inside the vertex), and all its lines carry the same frequency. Then
   F(tau) = C Prod_chains L_c(tau) for 0 < tau < beta. A chain written as a function of the frequency x flowing from in
   to out, L(x) = Sum_j Sum_k r_jk/(x - E_j)^k, is L(tau) = Sum_j Sum_k r_jk/(k - 1)! d^(k-1)/dE^(k-1)
   [-(1 -+ n(E)) e^(-E tau)] at E = E_j (- for fermions, + for bosons): for a simple pole, the line on shell with the
   factor of emission (1 - n_F(E), 1 + n_B(E)), which for E < 0 is the absorption n(|E|); a static self-energy on the
   line gives a double pole and the derivative. Hence
     F^>(omega) = C (-1)^(N + 1) Sum Prod_c r_c/(k_c - 1)! d^(k_c - 1)/dE_c^(k_c - 1) [Prod_c (1 -+ n_c(E_c)) delta(omega - Sum E_c)],
   with F(tau) = -Integral dE F^>(E) e^(-E tau), and Im F^R(omega) = -Pi F^>(omega) (1 +- e^(-beta omega))
   = -Pi F^>(omega)/(1 -+ n(omega)), the second factor being the inverse process. Lines inside a time vertex are at equal
   times: n_F(xi) for a fermion line (G(0-)), n_F(xi) - 1 (G(0+)) or n_F(xi) - 1/2 with "ConvergenceFactor" -> -1 or 0,
   that is n_F(xi) - (1 - eta)/2; an (even) boson line is the same at 0- and 0+. *)
CuttingRuleImaginaryPart::class = "The cutting rules are implemented for two-point functions whose propagating lines form chains from one external time vertex to the other, through vertices with static insertions only (bubbles, sunsets, Hartree and Fock insertions): a term with external labels `1` is not of this kind.";
staticLineValue[{"Fermion", disp_}, {p_, _}, eta_] := FermiFunction[disp[p]] - (1 - eta)/2;
staticLineValue[{"Instantaneous", v_}, flow_, _] := lineValue[{"Instantaneous", v}, flow];
staticLineValue[{"Boson", f_}, {p_, _}, _] := Module[{x}, With[{g = linearizeDenominators[lineValue[{"Boson", f}, {p, -I x}], x]},
    -Total[(laurentCoefficient[g, x, #, 1, 1] (1 + BoseFunction[#[[1]]]) &) /@ polesOf[g, x]]]];

(* h(omega) delta^(m)(omega - E) = Sum_j Binomial[m, j] (-1)^j h^(j)(E) delta^(m - j)(omega - E): coefficients free of omega *)
deltaNormalForm[e_, omega_] := Total[Table[With[{ds = Select[factorList[t], ! FreeQ[#, EnergyDelta] &]},
     If[Length[ds] != 1 || FreeQ[t/ds[[1]], omega], t,
      With[{c = t/ds[[1]], md = ds[[1]] /. {EnergyDelta[x_] :> {0, x}, Derivative[m_][EnergyDelta][x_] :> {m, x}}},
       Sum[Binomial[md[[1]], j] (-1)^j (If[j == 0, c, D[c, {omega, j}]] /. omega -> Expand[omega - md[[2]]]) energyDelta[md[[1]] - j, md[[2]]],
        {j, 0, md[[1]]}]]]], {t, termsInVariable[e, EnergyDelta]}]];

(* the chains of propagating lines from tIn to tOut: lists of {line, orientation}; $Failed if the lines do not form
   chains through vertices with two propagating lines *)
cutChains[cut_List, timeOf_List, tIn_, tOut_] := Catch[Module[{ends, chains = {}, used = {}, chain, v, next, l},
    ends[j_] := {timeOf[[cut[[j, 1]]]], timeOf[[cut[[j, 2]]]]};
    Do[If[! MemberQ[used, j] && MemberQ[ends[j], tIn],
      chain = {}; v = tIn; next = j;
      While[True,
       l = next; AppendTo[used, l];
       AppendTo[chain, {l, If[ends[l][[1]] == v, 1, -1]}];
       v = If[ends[l][[1]] == v, ends[l][[2]], ends[l][[1]]];
       If[v == tOut, Break[]];
       If[v == tIn, Throw[$Failed, chainFailure]];
       next = Select[Range[Length[cut]], ! MemberQ[used, #] && MemberQ[ends[#], v] &];
       If[Length[next] != 1 || Count[Join @@ Table[ends[i], {i, Length[cut]}], v] != 2, Throw[$Failed, chainFailure]];
       next = First[next]];
      AppendTo[chains, chain]], {j, Length[cut]}];
    If[Length[used] != Length[cut], $Failed, chains]], chainFailure];

cutIntegrand[data_List, spin_, w_, omega_, eta_] := Catch[Module[{lines = routeData["Lines", data], timeOf = routeData["TimeOf", data],
     externals = routeData["Externals", data], nodes = routeData["ExternalNodes", data], inflow, tIn, tOut, static, cut,
     chains, constant, spectra, energies, statistics, thermal, n},
    (* the external frequency entering each time vertex *)
    inflow[tv_] := Total[Table[If[timeOf[[x[[1]] /. nodes]] == tv, Coefficient[Expand[x[[2, 2]]/I], w], 0], {x, externals}]];
    tIn = Select[Union[timeOf], inflow[#] === 1 &]; tOut = Select[Union[timeOf], inflow[#] === -1 &];
    If[tIn === {} && tOut === {}, Throw[0, cutFailure]];
    If[Length[tIn] != 1 || Length[tOut] != 1, Message[CuttingRuleImaginaryPart::class, Table[x[[1]], {x, externals}]]; Throw[$Failed, cutFailure]];
    {tIn, tOut} = {tIn[[1]], tOut[[1]]};
    static = Select[lines, timeOf[[#[[1]]]] == timeOf[[#[[2]]]] &];
    cut = Select[lines, timeOf[[#[[1]]]] != timeOf[[#[[2]]]] &];
    chains = cutChains[cut, timeOf, tIn, tOut];
    If[chains === $Failed, Message[CuttingRuleImaginaryPart::class, Table[x[[1]], {x, externals}]]; Throw[$Failed, cutFailure]];
    constant = weightOf[data, spin] Times @@ (staticLineValue[#[[4]], #[[5]], eta] &) /@ static;
    (* each chain as a function of the frequency x flowing from in to out: {pole, order k, Laurent coefficient r_k/(k - 1)!} *)
    spectra = Table[Module[{x}, With[{f = linearizeDenominators[Times @@ Table[With[{l = cut[[c[[1]]]]},
            lineValue[l[[4]], {l[[5, 1]], -I c[[2]] x}]], {c, chain}], x]},
        Table[{c[[1]], c[[2]], c[[3]]/(c[[2]] - 1)!}, {c, laurentTerms[f, x]}]]], {chain, chains}];
    n = Length[chains]; energies = Table[Unique["FunctionalDiagrams`Private`energy"], {n}];
    thermal = Times @@ Table[If[cut[[chains[[c, 1, 1]], 4, 1]] === "Fermion", 1 - FermiFunction[energies[[c]]], 1 + BoseFunction[energies[[c]]]], {c, n}];
    statistics = frequencyStatistics[w];
    collectDeltas[deltaNormalForm[simplifyStatisticalFactors[Total[Table[
        -Pi constant (-1)^(n + 1) (Times @@ choice[[All, 3]]) (Fold[If[#2[[2]] == 0, #1, D[#1, {#2[[1]], #2[[2]]}]] &,
             thermal EnergyDelta[omega - Total[energies]], Table[{energies[[c]], choice[[c, 2]] - 1}, {c, n}]] /.
            Thread[energies -> choice[[All, 1]]])/If[statistics === "Fermion", 1 - FermiFunction[omega], 1 + BoseFunction[omega]],
        {choice, Tuples[spectra]}]] /. (h : EnergyDelta | Derivative[_][EnergyDelta])[a_] :> h[Expand[a]]], omega]]], cutFailure];

(* the frequency w enters the diagram at one external point and leaves it at another *)
CuttingRuleImaginaryPart::frequency = "The frequency `1` must enter the diagram at one external point and leave it at another: I `1` and -I `1` in the frequencies of exactly two external points.";
cutFrequencyQ[externals_List, w_] := If[Sort[DeleteCases[Table[Coefficient[Expand[x[[2, 2]]/I], w], {x, externals}], 0]] === {-1, 1}, True,
   Message[CuttingRuleImaginaryPart::frequency, w]; False];

(* $Failed if a term is not of the implemented kind *)
CuttingRuleImaginaryPart[e_, externals_List, rules_List, w_Symbol -> omega_, opts___Rule] := Module[{spin, names, eta, routed, results},
   {spin, names, eta} = matsubaraOptions[CuttingRuleImaginaryPart, {opts}];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed || ! cutFrequencyQ[externals, w], $Failed,
    results = Table[If[data === $Failed, $Failed, With[{cut = cutIntegrand[Append[data, "Externals" -> externals], spin, w, omega, eta]},
        If[cut === $Failed, $Failed, momentumSum[cut, routeData["LoopMomenta", data]]]]], {data, routed}];
    If[MemberQ[results, $Failed], $Failed, Total[results]]]];

(* the two sides, term by term and delta function by delta function: the list of the differences that do not vanish *)
CheckCuttingRules[e_, externals_List, rules_List, w_Symbol -> omega_, opts___Rule] := Module[{spin, names, eta, routed, differences = {}},
   {spin, names, eta} = matsubaraOptions[CheckCuttingRules, {opts}];
   routed = routedTerms[e, externals, rules, names, eta];
   If[routed === $Failed || ! cutFrequencyQ[externals, w], $Failed,
    Do[If[data === $Failed, AppendTo[differences, $Failed],
      Module[{summed = summedIntegrand[data, eta], residues, cuts},
       residues = If[summed === $Failed, $Failed, RetardedImaginaryPart[AnalyticContinuation[weightOf[data, spin] summed, w -> omega], omega]];
       cuts = cutIntegrand[Append[data, "Externals" -> externals], spin, w, omega, eta];
       If[residues === $Failed || cuts === $Failed, AppendTo[differences, $Failed],
        Scan[If[! thermalZeroQ[#[[2]]], AppendTo[differences, #]] &, deltaCoefficients[residues - cuts]]]]],
     {data, routed}];
    If[differences === {}, 0, differences]]];

(* T -> 0 of the sums of logarithms (tried first): T ln(1 - n_F(a)) -> a theta(-a), T ln(1 + n_B(a)) -> 0 (a > 0) *)
ZeroTemperatureLimit[e_] /; ! FreeQ[e, Temperature] && ! TrueQ[$temperatureLogsDone] := Block[{$temperatureLogsDone = True},
   ZeroTemperatureLimit[(e /. {Log[1 - FermiFunction[a_]] :> a UnitStep[-a]/Temperature, Log[1 + BoseFunction[_]] :> 0}) /.
     MomentumSum[f_, ps_] :> momentumSum[Expand[f], ps]]];

(* ---- zero temperature: n_F(e) -> theta(-e), n_B(e) -> -theta(-e); n_F(0) = 1/2 is already evaluated ---- *)
ZeroTemperatureLimit[e_] := e /. {Derivative[k_][FermiFunction][a_] :> -energyDelta[k - 1, a],
    Derivative[k_][BoseFunction][a_] :> energyDelta[k - 1, a], FermiFunction[a_] :> UnitStep[-a], BoseFunction[a_] :> -UnitStep[-a]};

(* ---- display and LaTeX of the new heads ---- *)
defineDisplayForm[FermiFunction[e_], Interpretation[Row[{Subscript["n", "F"], "(", e, ")"}], FermiFunction[e]]];
defineDisplayForm[BoseFunction[e_], Interpretation[Row[{Subscript["n", "B"], "(", e, ")"}], BoseFunction[e]]];
defineDisplayForm[EnergyDelta[e_], Interpretation[Row[{"\[Delta]", "(", e, ")"}], EnergyDelta[e]]];
defineDisplayForm[MomentumSum[f_, ps_List], Interpretation[Row[{Subscript["\[Sum]", Row[ps, ","]], " ", f}], MomentumSum[f, ps]]];
defineDisplayForm[LoopMomentum[i_], Interpretation[Subscript["p", i], LoopMomentum[i]]];
defineDisplayForm[LoopFrequency[i_, s_String], Interpretation[Subscript[If[s === "Fermion", "\[Omega]", "\[Nu]"], i], LoopFrequency[i, s]]];

(* the generic rules of latexFactorOf (a function of labels h(x, y), its derivatives) must not apply to the new heads *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {FermiFunction, BoseFunction, EnergyDelta, MomentumSum, LoopMomentum, LoopFrequency,
    UnitStep}];
latexStatistical[h_, k_, e_] := If[h === FermiFunction, "n_F", "n_B"] <>
   If[k == 0, "", "^{" <> If[k <= 3, StringJoin[Table["\\prime", {k}]], "(" <> ToString[k] <> ")"] <> "}"] <> "(" <> latexOfExpression[e] <> ")";
latexFactorOf[(h : FermiFunction | BoseFunction)[e_]] := latexStatistical[h, 0, e];
latexFactorOf[Derivative[k_][h : FermiFunction | BoseFunction][e_]] := latexStatistical[h, k, e];
latexFactorOf[EnergyDelta[e_]] := "\\delta(" <> latexOfExpression[e] <> ")";
latexFactorOf[Derivative[k_][EnergyDelta][e_]] := "\\delta^{(" <> ToString[k] <> ")}(" <> latexOfExpression[e] <> ")";
latexFactorOf[MomentumSum[f_, ps_List]] := "\\sum_{" <> joinStrings[latexArgument /@ ps, ","] <> "}" <>
   If[Head[f] === Plus || TrueQ[numericFactor[f] < 0], "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[LoopMomentum[i_]] := "p_{" <> ToString[i] <> "}";
(* the step function of the zero-temperature limit *)
latexFactorOf[UnitStep[e_]] := "\\theta(" <> latexOfExpression[e] <> ")";
(* the temperature of the sums of logarithms *)
latexFactorOf[Temperature] := "T";
latexFactorOf[LoopFrequency[i_, s_String]] := If[s === "Fermion", "\\omega_{", "\\nu_{"] <> ToString[i] <> "}";

(* ========================================================================= *)
(*  18. Feynman integrals in dimensional regularization                      *)
(*  The diagrams of the package, for a translation-invariant theory at zero  *)
(*  temperature in Euclidean space, as momentum integrals in d = 4 - 2 eps:  *)
(*  the momenta are routed through the lines by the routing of section 17   *)
(*  (routeTerm), the loops are done one after the other as one-loop          *)
(*  integrals in closed form (tadpoles, massless G-functions, massive        *)
(*  bubbles by Feynman parameters), and the results are expanded in eps      *)
(*  by a Laurent expansion of our own (no Series, Limit or Residue).         *)
(*  Conventions: a line is 1/(q^2 + m^2); every loop is                      *)
(*    mu^(2 eps) (e^gammaE/(4 pi))^eps Int d^dk/(2 pi)^d,                     *)
(*  so that Int_k 1/(k^2 + Delta)^n = e^(eps gammaE) mu^(2 eps)               *)
(*  Gamma(n - d/2)/(16 pi^2 Gamma(n)) Delta^(d/2 - n): no gammaE and no       *)
(*  ln 4 pi appear, and minimal subtraction is the MS-bar scheme.            *)
(* ========================================================================= *)

(* ---- the measure and the one-loop integrals in closed form ---- *)

loopMeasure[eps_] := Exp[eps EulerGamma] (RenormalizationScale^2)^eps/(16 Pi^2);
nonPositiveIntegerQ[a_] := IntegerQ[a] && a <= 0;
(* x^n, with 0^0 = 1 *)
powerOrOne[x_, 0] := 1;
powerOrOne[x_, n_] := x^n;

GFunction[a_, b_, eps_] := Gamma[2 - eps - a] Gamma[2 - eps - b] Gamma[a + b - 2 + eps]/(Gamma[a] Gamma[b] Gamma[4 - 2 eps - a - b]);

TadpoleIntegral[a_, 0, eps_] := 0;
(* a polynomial in k: 0 in dimensional regularization *)
TadpoleIntegral[a_Integer, m_, eps_] /; a <= 0 := 0;
TadpoleIntegral[a_, m_, eps_] /; m =!= 0 := loopMeasure[eps] Gamma[a - 2 + eps]/Gamma[a] (m^2)^(2 - eps - a);

(* closed forms; otherwise BubbleIntegral stays, as its Feynman-parameter integral (expanded by EpsilonExpand) *)
BubbleIntegral[{a_, b_}, p2_, {0, 0}, eps_] := If[p2 === 0 || nonPositiveIntegerQ[a] || nonPositiveIntegerQ[b], 0,
   loopMeasure[eps] GFunction[a, b, eps] p2^(2 - eps - a - b)];
(* a power that is a non-positive integer: a numerator, reduced to tadpoles *)
BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_] /; (m1 =!= 0 || m2 =!= 0) && (nonPositiveIntegerQ[a] || nonPositiveIntegerQ[b]) :=
  If[nonPositiveIntegerQ[b], numeratorBubble[-b, m2, a, m1, p2, eps], numeratorBubble[-a, m1, b, m2, p2, eps]];
BubbleIntegral[{a_, b_}, 0, {m_, m_}, eps_] /; m =!= 0 := TadpoleIntegral[a + b, m, eps];
(* a massless line at zero momentum: Int_k (k^2)^-a (k^2 + m^2)^-b = Gamma(d/2 - a) Gamma(a + b - d/2)/(Gamma(d/2) Gamma(b)) (m^2)^(d/2-a-b) *)
BubbleIntegral[{a_, b_}, 0, {0, m_}, eps_] /; m =!= 0 :=
  loopMeasure[eps] Gamma[2 - eps - a] Gamma[a + b - 2 + eps]/(Gamma[2 - eps] Gamma[b]) (m^2)^(2 - eps - a - b);
BubbleIntegral[{a_, b_}, 0, {m_, 0}, eps_] /; m =!= 0 := BubbleIntegral[{b, a}, 0, {0, m}, eps];
(* unequal masses at zero momentum, a = b = 1: partial fractions *)
BubbleIntegral[{1, 1}, 0, {m1_, m2_}, eps_] /; m1 =!= 0 && m2 =!= 0 && m1 =!= m2 :=
  (TadpoleIntegral[1, m1, eps] - TadpoleIntegral[1, m2, eps])/(m2^2 - m1^2);

(* Int_q ((q - P)^2 + mn^2)^n/(q^2 + md^2)^b for an integer n >= 0, P^2 = p2: ((q - P)^2)^s expanded, the odd powers of q.P
   dropped and (q.P)^(2j) replaced by its angular average (q^2 P^2)^j (2j - 1)!!/(d (d + 2) ... (d + 2j - 2)); then
   q^(2l) = ((q^2 + md^2) - md^2)^l gives tadpoles. With md = 0 every term is scaleless: 0 *)
numeratorBubble[n_, mn_, b_, md_, p2_, eps_] := If[md === 0, 0, Module[{d = 4 - 2 eps},
   Sum[Binomial[n, s] powerOrOne[mn^2, n - s] Binomial[s, 2 j] 4^j Product[(2 r + 1)/(d + 2 r), {r, 0, j - 1}] Binomial[s - 2 j, t]
      powerOrOne[p2, s - j - t] Sum[Binomial[j + t, i] powerOrOne[-md^2, j + t - i] TadpoleIntegral[b - i, md, eps], {i, 0, j + t}],
    {s, 0, n}, {j, 0, Floor[s/2]}, {t, 0, s - 2 j}]]];

MomentumSquared[0] = 0;
MomentumSquared[p_Symbol] := p^2;
MomentumSquared[c_?NumberQ p_Symbol] := c^2 p^2;
MomentumSquared[q_] /; ! NumberQ[q] && firstTermSign[q] < 0 := MomentumSquared[Expand[-q]];

(* ---- the momentum-space integrand of a diagram: the routing of section 17, each line a propagator ---- *)

DiagramLoopIntegral::rule = "`1` is not a line rule: use head -> {\"Scalar\", m} (the line is 1/(q^2 + m^2)) or head -> {\"Kernel\", f} (the line is f[q], q its momentum).";
DiagramLoopIntegral::external = "`1` is not a list of external momenta label -> momentum.";
DiagramLoopIntegral::loopnames = EvaluateMatsubara::loopnames;
DiagramLoopIntegral::loops = EvaluateMatsubara::loops;
DiagramLoopIntegral::factor = EvaluateMatsubara::factor;
DiagramLoopIntegral::grassmann = EvaluateMatsubara::grassmann;
DiagramLoopIntegral::conservation = "The external momenta do not add up to zero on a connected part of a diagram (`1`).";
InfraredRearrangedIntegral::rule = DiagramLoopIntegral::rule;
InfraredRearrangedIntegral::factor = DiagramLoopIntegral::factor;
InfraredRearrangedIntegral::grassmann = DiagramLoopIntegral::grassmann;
InfraredRearrangedIntegral::conservation = DiagramLoopIntegral::conservation;

$loopLineKinds = {"Scalar", "Kernel"};
routingRule[(Rule | RuleDelayed)[h_, {"Scalar", m_}]] := h -> {"Instantaneous", Function[q, ScalarPropagator[q, m]]};
routingRule[(Rule | RuleDelayed)[h_, {"Kernel", f_}]] := h -> {"Instantaneous", f};
externalMomentaQ[externals_] := ListQ[externals] && AllTrue[externals, MatchQ[#, (Rule | RuleDelayed)[_, _]] &];

loopIntegralOfRoute[data_List] := With[{c = routeData["Coefficient", data], f = routeData["Integrand", data], ks = routeData["LoopMomenta", data]},
   If[ks === {}, c f, c LoopIntegral[f, ks]]];
(* external points label -> momentum as those of routeTerm, label -> {momentum, 0} *)
momentumExternals[externals_List] := Table[x[[1]] -> {x[[2]], 0}, {x, externals}];
(* the messages of the routing are those of caller *)
termLoopIntegral[t_, externals_List, rules_List, names_, caller_] :=
  With[{data = routeTerm[t, momentumExternals[externals], routingRule /@ rules, names, caller]},
   If[data === $Failed, $Failed, loopIntegralOfRoute[data]]];

DiagramLoopIntegral[e_, externals_, rules_List, opts___Rule] := Module[{names, terms},
   checkOptionNames[DiagramLoopIntegral, {opts}, {"LoopMomenta"}];
   names = optionValue[{opts}, {"LoopMomenta" -> Automatic}, "LoopMomenta"];
   Which[
    ! externalMomentaQ[externals], Message[DiagramLoopIntegral::external, externals]; $Failed,
    ! checkRuleKinds[rules, $loopLineKinds, DiagramLoopIntegral], $Failed,
    names =!= Automatic && ! ListQ[names], Message[DiagramLoopIntegral::loopnames, names]; $Failed,
    True, terms = termLoopIntegral[#, externals, rules, names, DiagramLoopIntegral] & /@ canonicalTermsOf[e];
     If[MemberQ[terms, $Failed], $Failed, Total[terms]]]];

(* ---- the loops one after the other ----
   A loop momentum k that appears in at most two different propagators (k + r1, m1)^a, (k + r2, m2)^b (with coefficient
   +-1) is integrated: a tadpole, or a bubble of momentum P = r2 - r1 (if there is none, the loop momenta are first changed,
   k -> c (k - r), so that the momentum c k + r of a line becomes a loop momentum). Two massless lines give G(a, b) and a new
   massless line of momentum P and power a + b - 2 + eps; a massive bubble, or a numerator (a power that is a non-positive
   integer) next to a massive line, only when P is free of the other loop momenta.
   Each term of the expanded integrand (a piece) gives {value, flags}. Flags: "Scaleless" (a scaleless integral that is
   only power divergent in the ultraviolet: 0, also for the ultraviolet counterterms of a massless theory),
   "ScalelessLog" (a scaleless integral with a logarithmic divergence: 0 because its ultraviolet and infrared poles
   cancel), "IR" (an infrared divergence: a massless line of power 2 or more at eps = 0 in a loop, a scaleless integral
   that diverges only in the infrared, or a massless line of zero momentum), "Irreducible" and {"Numerator", factor}
   (value $Failed). A piece with a power-divergent scaleless loop is 0 whatever else it contains (the loop is a factor of
   it), so such a loop is looked for first. *)
IntegrateLoops::irreducible = "The loop integral `1` is not reduced to nested one-loop integrals (at each step a loop momentum in at most two different propagators, a massive bubble or a numerator next to a massive line only with an external momentum): it is left unevaluated.";
IntegrateLoops::numerator = "The factor `1` of a loop integral is neither a propagator ScalarPropagator[q, m]^a nor the square of a momentum: the integral is left unevaluated.";
IntegrateLoops::ir = "The loop integral `1` has an infrared divergence (a massless line of power 2 or more at \[Epsilon] = 0, or of zero momentum): its poles in \[Epsilon] are not only ultraviolet ones. For ultraviolet counterterms give the lines a mass, or use InfraredRearrangedIntegral.";
IntegrateLoops::scaleless = "The loop integral `1` contains a logarithmically divergent scaleless integral, which is 0 in dimensional regularization because its ultraviolet and infrared poles cancel: its ultraviolet pole is lost. For ultraviolet counterterms give the lines a mass, let a momentum flow through it, or use InfraredRearrangedIntegral.";
IntegrateLoops::zero = "A massless line of zero momentum, ScalarPropagator[0, 0], is infinite (an infrared divergence): it is left as it is.";


(* the pieces {value, flags} of LoopIntegral[f, ks]; the loop momenta are replaced by symbols during the algebra (Coefficient
   and Expand work on plain symbols) *)
loopPieces[f_, ks_List, eps_] := Module[{symbols = Table[Unique["FunctionalDiagrams`Private`loopMomentum"], {Length[ks]}], fs},
   fs = f /. Table[ks[[i]] -> symbols[[i]], {i, Length[ks]}];
   Table[integratedTerm[t, symbols, eps], {t, productTermsOf[Expand[squaresAsPropagators[fs, symbols]]]}] /.
    Table[symbols[[i]] -> ks[[i]], {i, Length[ks]}]];

(* numerators that are squares of momenta (from kernel lines such as a counterterm c q^2): q^2 = 1/ScalarPropagator[q, m] - m^2
   with the mass of a line of the same momentum, 1/ScalarPropagator[q, 0] otherwise *)
squaresAsPropagators[f_, ks_List] := Module[{lines = Cases[{f}, ScalarPropagator[q_, m_] /; m =!= 0 :> {Expand[q], m}, Infinity]},
   f /. {MomentumSquared[q_] /; ! FreeQ[q, Alternatives @@ ks] :> inversePropagator[q, lines],
     Power[k_, n_Integer] /; MemberQ[ks, k] && n > 0 && EvenQ[n] :> inversePropagator[k, lines]^(n/2)}];
inversePropagator[q_, lines_List] := With[{match = Select[lines, Expand[#[[1]] - q] === 0 || Expand[#[[1]] + q] === 0 &]},
   If[match === {}, 1/ScalarPropagator[q, 0], 1/ScalarPropagator[match[[1, 1]], match[[1, 2]]] - match[[1, 2]]^2]];

integratedTerm[t_, ks_List, eps_] := Module[{props = {}, constant = 1, bad = {}, flags = {}},
   Do[Which[
     FreeQ[f, Alternatives @@ ks], constant = constant f;
      If[! FreeQ[f, ScalarPropagator[0, 0]], AppendTo[flags, "IR"]],
     MatchQ[f, ScalarPropagator[_, _]], AppendTo[props, {f[[1]], f[[2]], 1}],
     MatchQ[f, Power[ScalarPropagator[_, _], _]], AppendTo[props, {f[[1, 1]], f[[1, 2]], f[[2]]}],
     True, AppendTo[bad, f]], {f, factorList[t]}];
   If[bad =!= {}, {$Failed, {{"Numerator", First[bad]}}},
    With[{r = oneLoopSteps[props, ks, eps]}, {If[r[[1]] === $Failed, $Failed, constant r[[1]]], Union[flags, r[[2]]]}]]];

propagatorProduct[props_List] := Times @@ Table[ScalarPropagator[p[[1]], p[[2]]]^p[[3]], {p, props}];
(* an infrared divergence: a massless line of power 2 or more at eps = 0 *)
irPowerQ[a_, eps_] := With[{a0 = a /. eps -> 0}, NumericQ[a0] && a0 >= 2];
(* a scaleless Int_k (k^2)^(-s): power divergent in the ultraviolet (s < 2 at eps = 0), logarithmic (s = 2), or infrared (s > 2) *)
scalelessPowerClass[s_, eps_] := With[{s0 = s /. eps -> 0}, Which[! NumericQ[s0], "ScalelessLog", s0 < 2, "Scaleless", s0 == 2, "ScalelessLog", True, "IR"]];
scalelessClass[powers_List, eps_] := With[{cs = Table[scalelessPowerClass[s, eps], {s, powers}]},
   If[AllTrue[cs, # === "Scaleless" &], {"Scaleless"}, Union[DeleteCases[cs, "Scaleless"]]]];
(* None, or the flags of a scaleless step: no line (Int_k 1), one massless line, or a numerator next to a massless line
   (shifted to the momentum of that line: (k^2)^(l - b), l = 0 ... n) *)
stepScalelessClass[groups_List, eps_] := Switch[Length[groups],
   0, {"Scaleless"},
   1, If[groups[[1, 2]] === 0, scalelessClass[{groups[[1, 3]]}, eps], None],
   2, With[{g1 = groups[[1]], g2 = groups[[2]]}, Which[
      nonPositiveIntegerQ[g1[[3]]] && nonPositiveIntegerQ[g2[[3]]], {"Scaleless"},
      nonPositiveIntegerQ[g1[[3]]], If[g2[[2]] === 0, scalelessClass[Table[g2[[3]] - l, {l, 0, -g1[[3]]}], eps], None],
      nonPositiveIntegerQ[g2[[3]]], If[g1[[2]] === 0, scalelessClass[Table[g1[[3]] - l, {l, 0, -g2[[3]]}], eps], None],
      True, None]]];
(* whether a two-line step depends on the momentum P of the bubble other than through a new massless line: a massive
   bubble, or tadpoles times powers of P^2 (a numerator next to a massive line) *)
momentumDependentStepQ[{g1_, g2_}] := Which[
   nonPositiveIntegerQ[g1[[3]]] && nonPositiveIntegerQ[g2[[3]]], False,
   nonPositiveIntegerQ[g1[[3]]], g2[[2]] =!= 0,
   nonPositiveIntegerQ[g2[[3]]], g1[[2]] =!= 0,
   True, g1[[2]] =!= 0 || g2[[2]] =!= 0];

(* the step for the loop momentum k: {k, groups {momentum k + r, mass, power}, the other propagators}, or $Failed *)
loopStepFor[props_List, ks_List, k_] := Module[{dep = Select[props, ! FreeQ[#[[1]], k] &], cs, normalized, groups},
   cs = Table[Coefficient[Expand[p[[1]]], k], {p, dep}];
   If[! AllTrue[cs, # === 1 || # === -1 &], $Failed,
    normalized = Table[{Expand[cs[[i]] dep[[i, 1]]], dep[[i, 2]], dep[[i, 3]]}, {i, Length[dep]}];
    groups = Table[{g[[1, 1]], g[[1, 2]], Total[g[[All, 3]]]}, {g, GatherBy[normalized, {#[[1]], #[[2]]} &]}];
    Which[
     Length[groups] > 2, $Failed,
     Length[groups] == 2 && momentumDependentStepQ[groups] &&
       ! FreeQ[Expand[groups[[2, 1]] - groups[[1, 1]]], Alternatives @@ DeleteCases[ks, k]], $Failed,
     True, {k, groups, Select[props, FreeQ[#[[1]], k] &]}]]];

changedLoopMomenta[props_List, ks_List] := Catch[Do[With[{c = Coefficient[Expand[pr[[1]]], k]},
     If[c === 1 || c === -1, With[{new = Table[{Expand[q[[1]] /. k -> c (k - Expand[pr[[1]] - c k])], q[[2]], q[[3]]}, {q, props}]},
       With[{steps = Select[Table[loopStepFor[new, ks, l], {l, ks}], # =!= $Failed &]},
        If[steps =!= {}, Throw[{new, steps}, changeFound]]]]]], {pr, props}, {k, ks}]; $Failed, changeFound];

(* {value, flags} of the product of propagators props over the loop momenta ks *)
oneLoopSteps[props0_List, ks0_List, eps_] := Catch[Module[{props = props0, ks = ks0, value = 1, flags = {}, steps, classes,
     k, groups, rest},
    While[ks =!= {},
     steps = Select[Table[loopStepFor[props, ks, q], {q, ks}], # =!= $Failed &];
     (* otherwise a change of loop momenta k -> c (k - r) that makes the momentum c k + r of a line a loop momentum *)
     If[steps === {}, With[{changed = changedLoopMomenta[props, ks]}, If[changed =!= $Failed, {props, steps} = changed]]];
     If[steps === {}, Throw[{$Failed, {"Irreducible"}}, loopFailure]];
     (* a scaleless loop makes the piece 0; a power-divergent one is a legitimate 0, so it is looked for first *)
     classes = DeleteCases[Table[stepScalelessClass[s[[2]], eps], {s, steps}], None];
     If[MemberQ[classes, {"Scaleless"}], Throw[{0, {"Scaleless"}}, loopFailure]];
     If[classes =!= {}, Throw[{0, Union[flags, First[classes]]}, loopFailure]];
     {k, groups, rest} = First[SortBy[steps, Length[#[[2]]] &]];
     If[Length[groups] == 1,
      (* a massive tadpole (a massless one is scaleless) *)
      value = value TadpoleIntegral[groups[[1, 3]], groups[[1, 2]], eps],
      Module[{g1 = groups[[1]], g2 = groups[[2]], p = Expand[groups[[2, 1]] - groups[[1, 1]]], numerator, line},
       Which[
        (* a numerator next to a massive line (next to a massless one it is scaleless) *)
        nonPositiveIntegerQ[g1[[3]]] || nonPositiveIntegerQ[g2[[3]]],
         {numerator, line} = If[nonPositiveIntegerQ[g1[[3]]], {g1, g2}, {g2, g1}];
         value = value numeratorBubble[-numerator[[3]], numerator[[2]], line[[3]], line[[2]], If[p === 0, 0, MomentumSquared[p]], eps],
        g1[[2]] === 0 && g2[[2]] === 0,
         If[irPowerQ[g1[[3]], eps] || irPowerQ[g2[[3]], eps], AppendTo[flags, "IR"]];
         value = value loopMeasure[eps] GFunction[g1[[3]], g2[[3]], eps];
         rest = Append[rest, {p, 0, g1[[3]] + g2[[3]] - 2 + eps}],
        True,
         If[(g1[[2]] === 0 && irPowerQ[g1[[3]], eps]) || (g2[[2]] === 0 && irPowerQ[g2[[3]], eps]), AppendTo[flags, "IR"]];
         value = value BubbleIntegral[{g1[[3]], g2[[3]]}, If[p === 0, 0, MomentumSquared[p]], {g1[[2]], g2[[2]]}, eps]]]];
     props = rest; ks = DeleteCases[ks, k]];
    {value propagatorProduct[props], Union[flags]}], loopFailure];

(* the loop integrals of e evaluated: {value, problems}, problems a list of {object, problem} with problem "IR",
   "ScalelessLog", "ZeroMomentum" (a massless tree line of zero momentum), "Irreducible" or {"Numerator", f} (the
   integral is then left unevaluated); the lines outside the integrals become 1/(q^2 + m^2)^a *)
evaluatedLoops[e_, eps_] := Module[{problems = {}, value},
   value = e /. LoopIntegral[f_, ks_List] :> With[{pieces = loopPieces[f, ks, eps]},
       If[MemberQ[pieces[[All, 1]], $Failed],
        problems = Join[problems, Table[{LoopIntegral[f, ks], x}, {x, Union @@ Select[pieces, #[[1]] === $Failed &][[All, 2]]}]];
        LoopIntegral[f, ks],
        problems = Join[problems, Table[{LoopIntegral[f, ks], x}, {x, DeleteCases[Union @@ pieces[[All, 2]], "Scaleless"]}]];
        Total[pieces[[All, 1]]]]];
   value = value /. {li_LoopIntegral :> li,
      ScalarPropagator[0, 0]^a_. :> (AppendTo[problems, {ScalarPropagator[0, 0], "ZeroMomentum"}]; ScalarPropagator[0, 0]^a),
      ScalarPropagator[q_, m_]^a_. :> (MomentumSquared[q] + m^2)^(-a)};
   {value, problems}];

IntegrateLoops[e_, eps_Symbol] := With[{r = evaluatedLoops[e, eps]},
   Scan[Function[pr, Switch[pr[[2]],
      "IR", Message[IntegrateLoops::ir, pr[[1]]],
      "ScalelessLog", Message[IntegrateLoops::scaleless, pr[[1]]],
      "ZeroMomentum", Message[IntegrateLoops::zero],
      "Irreducible", Message[IntegrateLoops::irreducible, pr[[1]]],
      {"Numerator", _}, Message[IntegrateLoops::numerator, pr[[2, 2]]]]], Union[r[[2]]]];
   r[[1]]];

(* ---- the infrared rearrangement: one momentum p through two external labels ---- *)

InfraredRearrangedIntegral::norouting = "No routing of one momentum through two of the labels `1` gives the term `2` without infrared divergences and logarithmically divergent scaleless integrals, as nested one-loop integrals (found: `3`).";
problemText[p_] := Switch[p, "IR", "an infrared divergence", "ScalelessLog", "a logarithmically divergent scaleless integral",
   "ZeroMomentum", "a massless line of zero momentum", "Irreducible", "an integral that is not nested one-loop",
   {"Numerator", _}, "the numerator " <> ToString[p[[2]], InputForm], _, ToString[p, InputForm]];

InfraredRearrangedIntegral[e_, labels_List, p_, rules_List, eps_Symbol, opts___Rule] := Module[{routing, results},
   checkOptionNames[InfraredRearrangedIntegral, {opts}, {"Routing"}];
   routing = optionValue[{opts}, {"Routing" -> False}, "Routing"];
   If[! checkRuleKinds[rules, $loopLineKinds, InfraredRearrangedIntegral], $Failed,
    results = rearrangedTerm[#, labels, p, rules, eps] & /@ canonicalTermsOf[e];
    Which[MemberQ[results, $Failed], $Failed, TrueQ[routing], results, True, Total[Table[r[[3]], {r, results}]]]]];

(* the first routing in which no piece of the term has a problem; pieces with a power-divergent scaleless loop are 0 *)
rearrangedTerm[t_, labels_List, p_, rules_List, eps_] := Catch[Module[{found = {}, externals, li, r},
    Do[externals = Table[labels[[i]] -> Which[i == pair[[1]], p, i == pair[[2]], -p, True, 0], {i, Length[labels]}];
     li = termLoopIntegral[t, externals, rules, Automatic, InfraredRearrangedIntegral];
     If[li === $Failed, Throw[$Failed, rearranged]];
     r = evaluatedLoops[li, eps];
     If[r[[2]] === {}, Throw[{termToExpression[t], labels[[pair]], r[[1]]}, rearranged]];
     found = Union[found, r[[2, All, 2]]], {pair, Subsets[Range[Length[labels]], {2}]}];
    Message[InfraredRearrangedIntegral::norouting, labels, termToExpression[t], joinStrings[problemText /@ found, ", "]]; $Failed],
   rearranged];

(* ---- -d/dm^2 through the lines: G0[x, y] -> G0[x, z] G0[z, y] ---- *)

MassInsertion::label = "The label `1` of the insertion is not a new symbol: it must be a symbol that does not appear in the diagrams.";
MassInsertion[e_, g_Symbol, z_] := If[Head[z] =!= Symbol || ! FreeQ[e, z], Message[MassInsertion::label, z]; $Failed,
   SimplifyDiagrams[Total[insertMassTerm[#, g, z] & /@ canonicalTermsOf[e]]]];
insertMassTerm[diagramTerm[c_, o_, d_], g_, z_] := Module[{t},
   With[{c1 = Expand[D[c /. g[x_, y_] :> g[x, y] + t g[x, z] g[z, y], t] /. t -> 0]},
    Total[termToExpression[diagramTerm[#, o, d]] & /@ productTermsOf[c1]]]];

(* ---- truncated Laurent series: {v, {c0, c1, ...}} = Sum_i c_i eps^(v + i), known up to eps^(v + Length - 1) ---- *)

EpsilonExpand::function = "The factor `1` is not expanded in `2`: only Gamma functions of arguments linear in it, powers, exponentials, rational functions and massive bubbles are.";
EpsilonExpand::pole = "The expansion of `1` in `2` did not converge: an essential singularity or a series that vanishes identically.";
EpsilonExpand::loop = "`1` is an unevaluated loop integral: its dependence on `2` (through d = 4 - 2 `2`) is not explicit. Evaluate it first with IntegrateLoops (LoopIntegral) or IntegrateOneLoop (MinkowskiLoopIntegral).";
EpsilonExpand::variable = "The bubble `1` is in d = 4 - 2 `3`, not in d = 4 - 2 `2`: it is not expanded.";
EpsilonExpand::bubble = "The bubble `1` is not expanded in `2`: its powers must be positive and independent of `2`, and those of its massless lines less than 2 (otherwise the bubble is infrared divergent).";

serKnown[s_List] := s[[1]] + Length[s[[2]]] - 1;
serConstant[c_, order_] := {0, Table[If[i == 0, c, 0], {i, 0, order}]};
serMonomial[k_, order_] := {k, Table[If[i == 0, 1, 0], {i, 0, order - k}]};
serTimes[s1_List, s2_List, order_] := Module[{v = s1[[1]] + s2[[1]], top, c1 = s1[[2]], c2 = s2[[2]]},
   top = Min[order, serKnown[s1] + s2[[1]], serKnown[s2] + s1[[1]]];
   {v, Table[Sum[c1[[i + 1]] c2[[j - i + 1]], {i, Max[0, j - Length[c2] + 1], Min[j, Length[c1] - 1]}], {j, 0, top - v}]}];
serPlus[ss_List, order_] := Module[{v = Min[Table[s[[1]], {s, ss}]], top = Min[order, Min[Table[serKnown[s], {s, ss}]]]},
   {v, Table[Sum[With[{i = j - s[[1]]}, If[0 <= i < Length[s[[2]]], s[[2, i + 1]], 0]], {s, ss}], {j, v, top}]}];
serScale[s_List, c_] := {s[[1]], c s[[2]]};
(* exp of a series without constant term (valuation >= 1) *)
serExp[s_List, order_] := Module[{result = serConstant[1, order], power = serConstant[1, order]},
   Do[power = serScale[serTimes[power, s, order], 1/j]; result = serPlus[{result, power}, order], {j, 1, Max[order, 0]}];
   result];
(* a coefficient is 0: exact tests first; a value at generic positive values of its symbols that is clearly not 0 proves
   it is not (so Simplify, slow, is called only for coefficients that look like 0) *)
zeroQ[c_] := c === 0 || Expand[c] === 0 || Together[c] === 0 ||
   (! numericallyNonZeroQ[c] && LeafCount[c] < 400 && Simplify[c] === 0);
numericallyNonZeroQ[c_] := FreeQ[c, FeynmanParameterIntegral] && Module[{symbols = Union[Cases[{c}, s_Symbol /; ! NumericQ[s], Infinity]], v},
   v = Quiet[N[c /. Table[symbols[[i]] -> Prime[i + 4]/7, {i, Length[symbols]}], 30]];
   NumberQ[v] && Abs[v] > 10^-12];
serStrip[s_List] := Module[{v = s[[1]], cs = s[[2]]},
   While[cs =!= {} && zeroQ[First[cs]], cs = Rest[cs]; v = v + 1];
   {v, cs}];
(* 1/s, s with a non-zero first coefficient *)
serInverse[s_List, order_] := Module[{c = s[[2]], n, b = {}},
   n = Min[order + s[[1]] + 1, Length[c]];
   Do[AppendTo[b, If[k == 0, 1/c[[1]], -Sum[c[[i + 1]] b[[k - i + 1]], {i, 1, k}]/c[[1]]]], {k, 0, n - 1}];
   {-s[[1]], b}];

(* the valuation of a factor: a lower bound, exact for the factors whose series is computed from it *)
valuation[f_, eps_] /; FreeQ[f, eps] := 0;
valuation[f_, eps_] /; f === eps := 1;
valuation[f_Times, eps_] := Total[valuation[#, eps] & /@ (List @@ f)];
valuation[f_Plus, eps_] := Min[valuation[#, eps] & /@ (List @@ f)];
valuation[Power[b_, e_], eps_] := Which[FreeQ[b, eps], 0, IntegerQ[e] && e > 0, e valuation[b, eps],
   IntegerQ[e] && e < 0, e strippedSeries[b, eps, 0][[1]], True, 0];
valuation[Gamma[x_], eps_] := With[{x0 = x /. eps -> 0}, If[IntegerQ[x0] && x0 <= 0, -1, 0]];
valuation[_, eps_] := 0;

(* within one expansion (EpsilonExpand, ExpandParameterIntegrals) the series of each factor and of each base are
   remembered: the same 1/eps, Gamma functions and powers recur in every term (a failure throws and is not stored) *)
$seriesMemo = False;
ser[f_, eps_, order_] := If[$seriesMemo, With[{v = serTable[f, eps, order]},
    If[Head[v] === serTable, serTable[f, eps, order] = serValue[f, eps, order], v]], serValue[f, eps, order]];
strippedSeries[b_, eps_, extra_] := If[$seriesMemo, With[{v = strippedTable[b, eps, extra]},
    If[Head[v] === strippedTable, strippedTable[b, eps, extra] = strippedSeriesValue[b, eps, extra], v]], strippedSeriesValue[b, eps, extra]];
SetAttributes[withSeriesMemo, HoldAll];
withSeriesMemo[e_] := If[$seriesMemo, e, Block[{$seriesMemo = True, serTable, strippedTable}, e]];

(* the series of b with its true valuation, known to at least `extra` orders beyond it *)
strippedSeriesValue[b_, eps_, extra_] := Module[{v0 = valuation[b, eps], s, k = 0},
   s = serStrip[ser[b, eps, v0 + extra + 2]];
   While[s[[2]] === {} && k < 6, k++; s = serStrip[ser[b, eps, v0 + extra + 2 + 3 k]]];
   If[s[[2]] === {}, Message[EpsilonExpand::pole, b, eps]; Throw[$Failed, expansionFailure]];
   If[serKnown[s] < s[[1]] + extra, s = serStrip[ser[b, eps, s[[1]] + extra]]];
   s];

serValue[f_, eps_, order_] /; FreeQ[f, eps] := serConstant[f, order];
serValue[f_, eps_, order_] /; f === eps := serMonomial[1, order];
serValue[f_Plus, eps_, order_] := serPlus[ser[#, eps, order] & /@ (List @@ f), order];
serValue[f_Times, eps_, order_] := Module[{fs = List @@ f, vs, total, acc},
   vs = valuation[#, eps] & /@ fs; total = Total[vs];
   acc = ser[fs[[1]], eps, order - (total - vs[[1]])];
   Do[acc = serTimes[acc, ser[fs[[i]], eps, order - (total - vs[[i]])], order - Total[Drop[vs, i]]], {i, 2, Length[fs]}];
   acc];
(* an integer power b^e: the j-th partial product b^(j+1) is needed to the order that the e - 1 - j factors still to be
   multiplied, each of valuation v, leave at `order` (beyond it when they have a pole, v < 0); the same for (1/b)^k *)
serValue[Power[b_, e_], eps_, order_] := Which[
   FreeQ[b, eps], With[{e0 = e /. eps -> 0},
    serTimes[serConstant[b^e0, order], serExp[serScale[ser[Expand[e - e0], eps, order], Log[b]], order], order]],
   IntegerQ[e] && e > 0, Module[{v = valuation[b, eps], s, acc},
    s = ser[b, eps, order - (e - 1) v]; acc = s;
    Do[acc = serTimes[acc, s, order - (e - 1 - j) Min[v, 0]], {j, 1, e - 1}]; acc],
   IntegerQ[e] && e < 0, Module[{s0 = strippedSeries[b, eps, 0], vb, s, inv, k = -e, acc},
    vb = s0[[1]];
    (* 1/b, of valuation -vb, to the order needed by its k-th power, from the series of b to that many terms *)
    s = strippedSeries[b, eps, order + (k - 1) vb + vb + 1];
    inv = serInverse[s, order + (k - 1) vb];
    acc = inv; Do[acc = serTimes[acc, inv, order + (k - 1 - j) Max[vb, 0]], {j, 1, k - 1}]; acc],
   True, Message[EpsilonExpand::function, Power[b, e], eps]; Throw[$Failed, expansionFailure]];
(* Gamma(x0 + c eps) = Gamma(x0) exp(Sum_k PolyGamma(k - 1, x0) (c eps)^k/k!), through the recurrence at the poles *)
serValue[Gamma[x_], eps_, order_] := Module[{xe = Expand[x], x0, c},
   x0 = xe /. eps -> 0; c = Coefficient[xe, eps];
   Which[
    Expand[xe - x0 - c eps] =!= 0, Message[EpsilonExpand::function, Gamma[x], eps]; Throw[$Failed, expansionFailure],
    IntegerQ[x0] && x0 <= 0, ser[Gamma[xe + 1 - x0] Times @@ Table[Power[xe + j, -1], {j, 0, -x0}], eps, order],
    True, serTimes[serConstant[Gamma[x0], order],
     serExp[{1, Table[polyGammaValue[k - 1, x0] c^k/k!, {k, 1, order}]}, order], order]]];
(* PolyGamma(n, x) at a positive integer x through the zeta function: (-1)^(n+1) n! (zeta(n + 1) - Sum_(j < x) j^-(n+1)) *)
polyGammaValue[n_, x_] := If[IntegerQ[x] && x >= 1 && n >= 1, (-1)^(n + 1) n! (Zeta[n + 1] - Sum[1/j^(n + 1), {j, 1, x - 1}]), PolyGamma[n, x]];
serValue[bubbleParameterIntegral[a_, b_, p2_, m1_, m2_, eps_], eps_, order_] :=
  {0, Table[(-1)^k/k! feynmanParameterCoefficient[a, b, p2, m1, m2, k], {k, 0, order}]};
serValue[f_, eps_, order_] := (Message[EpsilonExpand::function, f, eps]; Throw[$Failed, expansionFailure]);

(* the powers X^(c eps) of a product written as (X/mu^2)^(c eps) when the powers of mu = RenormalizationScale balance them *)
scaleLogs[t_, eps_] := Module[{fs = factorList[t], isPower, powers, others, mu2 = RenormalizationScale^2, sum},
   isPower[f_] := MatchQ[f, Power[b_, e_] /; FreeQ[b, eps] && ! FreeQ[e, eps] && ! NumericQ[b]];
   powers = Select[fs, isPower]; others = Select[fs, ! isPower[#] &];
   sum = Expand[Total[Table[With[{e = f[[2]]}, e - (e /. eps -> 0)], {f, powers}]]];
   If[sum =!= 0 || ! MemberQ[Table[f[[1]], {f, powers}], mu2], t,
    (Times @@ others) Times @@ Table[With[{b = f[[1]], e = f[[2]]}, With[{e0 = e /. eps -> 0},
        If[b === mu2, mu2^e0, b^e0 (b/mu2)^Expand[e - e0]]]], {f, powers}]]];

(* a massive bubble left by BubbleIntegral: e^(eps gammaE) Gamma(a + b - 2 + eps)/(16 pi^2 Gamma(a) Gamma(b)) times
   Int_0^1 dx x^(a-1) (1-x)^(b-1) Delta^(2-a-b) (Delta/mu^2)^(-eps), the last factor expanded in eps under the integral:
   valid for powers independent of eps, positive, and less than 2 on a massless line (no infrared divergence) *)
expandableBubbleQ[{a_, b_}, {m1_, m2_}, eps_] := FreeQ[{a, b}, eps] && ! (NumericQ[a] && a <= 0) && ! (NumericQ[b] && b <= 0) &&
   ! (m1 === 0 && irPowerQ[a, eps]) && ! (m2 === 0 && irPowerQ[b, eps]);
bubbleForExpansion[e_, eps_] := e /. BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps] :>
    If[expandableBubbleQ[{a, b}, {m1, m2}, eps],
     Exp[eps EulerGamma] Gamma[a + b - 2 + eps]/(16 Pi^2 Gamma[a] Gamma[b]) bubbleParameterIntegral[a, b, p2, m1, m2, eps],
     Message[EpsilonExpand::bubble, BubbleIntegral[{a, b}, p2, {m1, m2}, eps], eps]; Throw[$Failed, expansionFailure]];

(* the series of a sum, term by term *)
sumSeries[g_, eps_, n_] := serPlus[Table[ser[scaleLogs[t, eps], eps, n], {t, productTermsOf[Expand[g]]}], n];
EpsilonExpand[e_, eps_Symbol, n_Integer] := withSeriesMemo[Catch[Module[{loops, others, result},
    loops = Cases[{e}, _LoopIntegral | _MinkowskiLoopIntegral, Infinity];
    If[loops =!= {}, Message[EpsilonExpand::loop, First[loops], eps]; Throw[$Failed, expansionFailure]];
    others = Cases[{e}, BubbleIntegral[_, _, _, x_] /; x =!= eps, Infinity];
    If[others =!= {}, Message[EpsilonExpand::variable, First[others], eps, Last[First[others]]]; Throw[$Failed, expansionFailure]];
    result = sumSeries[bubbleForExpansion[e, eps], eps, n];
    Sum[Expand[result[[2, j + 1]]] eps^(result[[1]] + j), {j, 0, Length[result[[2]]] - 1}]], expansionFailure]];

PolePart[e_, eps_Symbol] := EpsilonExpand[e, eps, -1];

(* ---- the Feynman-parameter coefficients of a massive bubble: Int_0^1 dx w(x) Delta^n0 ln^k(Delta/mu^2),
   w = x^(a-1) (1-x)^(b-1), n0 = 2 - a - b, Delta = x(1-x) p2 + x m1^2 + (1-x) m2^2. In closed form for k = 0, n0 >= 0
   (a polynomial), for k = 1, n0 = 0 (a = b = 1) and for k = 0, n0 = -1 (a + b = 3); FeynmanParameterIntegral otherwise.
   For p2 > 0, Delta = p2 (xp - x)(x - xm) with xp >= 1 and xm <= 0, the roots in the square root of the Kallen function
   (xp = 1 for a massless first line, xm = 0 for a massless second line) ---- *)

SetAttributes[FeynmanParameterIntegral, NHoldAll];
(* the options of NIntegrate for N to a precision (also those of the integrals of QED) *)
precisionOptions[{p_, _}] := precisionOptions[p];
precisionOptions[p_?NumberQ] /; p > MachinePrecision := {WorkingPrecision -> p + 10, PrecisionGoal -> p};
precisionOptions[___] := {};
FeynmanParameterIntegral /: N[FeynmanParameterIntegral[f_, x_Symbol], precision___] :=
  NIntegrate[f, {x, 0, 1}, Evaluate[Sequence @@ precisionOptions[precision]]];

(* Int_0^u dx Sum_j cs[[j]] x^(j - 1) *)
powerSumIntegral[cs_List, u_] := Sum[cs[[j]] u^j/j, {j, Length[cs]}];
polynomialIntegral[poly_, x_] := powerSumIntegral[CoefficientList[Expand[poly], x], 1];
bubbleRoots[p2_, m1_, m2_] := Which[
   m1 === 0 && m2 === 0, {1, 0},
   m2 === 0, {1 + m1^2/p2, 0},
   m1 === 0, {1, -m2^2/p2},
   m1 === m2, With[{beta = Sqrt[1 + 4 m1^2/p2]}, {(1 + beta)/2, (1 - beta)/2}],
   True, With[{s = Sqrt[(p2 + m1^2 - m2^2)^2 + 4 p2 m2^2]}, {(p2 + m1^2 - m2^2 + s)/(2 p2), (p2 + m1^2 - m2^2 - s)/(2 p2)}]];
(* Int_0^1 x^j ln(r - x) dx for r >= 1, and Int_0^1 x^j ln(x - r) dx for r <= 0 *)
rightRootLogMoment[r_, j_] := If[r === 1, -Sum[1/i, {i, 1, j + 1}]/(j + 1),
   ((1 - r^(j + 1)) Log[r - 1] + r^(j + 1) Log[r])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)];
leftRootLogMoment[r_, j_] := If[r === 0, -1/(j + 1)^2,
   ((1 - r^(j + 1)) Log[1 - r] + r^(j + 1) Log[-r])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)];
(* Int_0^1 x^j/(x - r) dx, r outside [0, 1]; logRatio = ln|(1 - r)/r| *)
inverseMoment[r_, j_, logRatio_] := Sum[r^(j - 1 - i)/(i + 1), {i, 0, j - 1}] + r^j logRatio;
(* Int_0^1 dx poly(x)/(x - r) and poly(x)/(r - x), from the coefficients of the polynomial *)
inverseMoments[cs_List, r_, logRatio_] := Sum[cs[[j + 1]] inverseMoment[r, j, logRatio], {j, 0, Length[cs] - 1}];

feynmanParameterCoefficient[a_, b_, p2_, m1_, m2_, k_] := Module[{x = FeynmanParameter, n0 = 2 - a - b, w, delta, cs, mu2 = RenormalizationScale^2},
   w = x^(a - 1) (1 - x)^(b - 1); delta = x (1 - x) p2 + x m1^2 + (1 - x) m2^2;
   cs = If[IntegerQ[a] && IntegerQ[b] && a >= 1 && b >= 1, CoefficientList[Expand[w], x], {}];
   Which[
    cs === {}, FeynmanParameterIntegral[w delta^n0 Log[delta/mu2]^k, x],
    k == 0 && n0 >= 0, polynomialIntegral[w delta^n0, x],
    k == 1 && n0 == 0, Which[
     p2 === 0 && m1 === m2, Log[m1^2/mu2] polynomialIntegral[w, x],
     p2 === 0 && m1 =!= 0 && m2 =!= 0, With[{r = m2^2/(m2^2 - m1^2)},
      Sum[cs[[j + 1]] (((1 - r^(j + 1)) Log[m1^2/mu2] + r^(j + 1) Log[m2^2/mu2])/(j + 1) - Sum[r^(j - i)/(i + 1), {i, 0, j}]/(j + 1)),
       {j, 0, Length[cs] - 1}]],
     p2 === 0, FeynmanParameterIntegral[w Log[delta/mu2], x],
     m1 === m2 && m1 =!= 0 && cs === {1}, With[{beta = Sqrt[1 + 4 m1^2/p2]}, Log[m1^2/mu2] - 2 + 2 beta ArcTanh[1/beta]],
     True, With[{roots = bubbleRoots[p2, m1, m2]},
      Log[p2/mu2] polynomialIntegral[w, x] + Sum[cs[[j + 1]] (rightRootLogMoment[roots[[1]], j] + leftRootLogMoment[roots[[2]], j]), {j, 0, Length[cs] - 1}]]],
    k == 0 && n0 == -1, Which[
     p2 === 0 && m1 === m2, polynomialIntegral[w, x]/m1^2,
     p2 === 0 && m1 =!= 0 && m2 =!= 0, With[{r = m2^2/(m2^2 - m1^2)},
      Sum[cs[[j + 1]] inverseMoment[r, j, Log[m1^2/m2^2]], {j, 0, Length[cs] - 1}]/(m1^2 - m2^2)],
     p2 === 0, FeynmanParameterIntegral[w/delta, x],
     (* a massless second line: Delta = p2 x (xp - x), w = x x^(a-2) (1-x)^(b-1) (finite for a >= 2) *)
     m2 === 0 && m1 =!= 0 && a >= 2, With[{xp = bubbleRoots[p2, m1, m2][[1]]},
      -inverseMoments[CoefficientList[Expand[x^(a - 2) (1 - x)^(b - 1)], x], xp, Log[(xp - 1)/xp]]/p2],
     (* a massless first line: Delta = p2 (1 - x)(x - xm), w = (1-x) x^(a-1) (1-x)^(b-2) (finite for b >= 2) *)
     m1 === 0 && m2 =!= 0 && b >= 2, With[{xm = bubbleRoots[p2, m1, m2][[2]]},
      inverseMoments[CoefficientList[Expand[x^(a - 1) (1 - x)^(b - 2)], x], xm, Log[(1 - xm)/(-xm)]]/p2],
     m1 === 0 || m2 === 0, FeynmanParameterIntegral[w/delta, x],
     True, With[{roots = bubbleRoots[p2, m1, m2]},
      (* 1/Delta = (1/(p2 (xp - xm))) (1/(xp - x) + 1/(x - xm)) *)
      (inverseMoments[cs, roots[[2]], Log[(1 - roots[[2]])/(-roots[[2]])]] -
         inverseMoments[cs, roots[[1]], Log[(roots[[1]] - 1)/roots[[1]]]])/(p2 (roots[[1]] - roots[[2]]))]],
    True, FeynmanParameterIntegral[w delta^n0 Log[delta/mu2]^k, x]]];

(* ---- renormalization-group functions from renormalization constants (MS-bar, d = 4 - 2 eps) ---- *)

BetaFunctionFromZ::pole = "The limit `1` -> 0 of `2` does not exist: the poles of the renormalization constant are not consistent.";
AnomalousDimensionFromZ::pole = BetaFunctionFromZ::pole;
(* the coefficients c0 ... cn of the series in g of a rational function of g *)
gSeriesCoefficients[e_, g_, n_] := Module[{t = Together[e], num, den},
   num = Take[PadRight[CoefficientList[Expand[Numerator[t]], g], n + 1], n + 1];
   den = Take[PadRight[CoefficientList[Expand[Denominator[t]], g], n + 1], n + 1];
   Take[PadRight[Together /@ serTimes[{0, num}, serInverse[{0, den}, n], n][[2]], n + 1], n + 1]];
atZeroEpsilon[c_, eps_, what_, caller_] := With[{t = Together[c]},
   If[(Denominator[t] /. eps -> 0) === 0, Message[MessageName[caller, "pole"], eps, what]; $Failed, Together[t /. eps -> 0]]];
(* g_B = mu^(s eps) Z g: 0 = s eps + (1/g + d ln Z/dg) beta_eps, beta_eps = -s eps g/(1 + g d ln Z/dg) = -s eps g + beta(g) *)
BetaFunctionFromZ[z_, g_Symbol, eps_Symbol, n_Integer, opts___Rule] := Module[{s, cs, values},
   checkOptionNames[BetaFunctionFromZ, {opts}, {"ScalePower"}];
   s = optionValue[{opts}, {"ScalePower" -> 2}, "ScalePower"];
   cs = gSeriesCoefficients[-s eps g/(1 + g D[z, g]/z) + s eps g, g, n];
   values = Table[atZeroEpsilon[cs[[j + 1]], eps, "the beta function", BetaFunctionFromZ], {j, 0, n}];
   If[MemberQ[values, $Failed], $Failed, Sum[values[[j + 1]] g^j, {j, 0, n}]]];
AnomalousDimensionFromZ[z_, g_Symbol, eps_Symbol, betaEps_, n_Integer] := Module[{cs, values},
   cs = gSeriesCoefficients[betaEps D[z, g]/z, g, n];
   values = Table[atZeroEpsilon[cs[[j + 1]], eps, "the anomalous dimension", AnomalousDimensionFromZ], {j, 0, n}];
   If[MemberQ[values, $Failed], $Failed, Sum[values[[j + 1]] g^j, {j, 0, n}]]];


(* ---- display and LaTeX of the new heads ---- *)
(* (GFunction and TadpoleIntegral are always evaluated) *)
defineDisplayForm[ScalarPropagator[q_, m_], Interpretation[1/If[m === 0, HoldForm[#] &[MomentumSquared[q]], HoldForm[#1 + #2] &[MomentumSquared[q], m^2]],
   ScalarPropagator[q, m]]];
defineDisplayForm[MomentumSquared[q_], Interpretation[HoldForm[#^2] &[q], MomentumSquared[q]]];
defineDisplayForm[RenormalizationScale, Interpretation["\[Mu]", RenormalizationScale]];
defineDisplayForm[FeynmanParameter, Interpretation["x", FeynmanParameter]];
defineDisplayForm[LoopIntegral[f_, ks_List], Interpretation[Row[{Subscript["\[Integral]", Row[ks, ","]], " ", f}], LoopIntegral[f, ks]]];
defineDisplayForm[BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_],
  Interpretation[Row[{Subscript["B", Row[{a, b}, ","]], "(", p2, "; ", m1, ", ", m2, ")"}], BubbleIntegral[{a, b}, p2, {m1, m2}, eps]]];
defineDisplayForm[FeynmanParameterIntegral[f_, x_], Interpretation[Row[{Subsuperscript["\[Integral]", 0, 1], "d", x, " ", f}],
   FeynmanParameterIntegral[f, x]]];

(* the generic rules of latexFactorOf must not apply to the new heads; mu, x and gammaE by their names *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {ScalarPropagator, MomentumSquared, LoopIntegral, GFunction, TadpoleIntegral,
    BubbleIntegral, FeynmanParameterIntegral, ArcTanh, Zeta}];
$latexDefaultNames = Join[$latexDefaultNames, {RenormalizationScale -> "\\mu", FeynmanParameter -> "x", EulerGamma -> "\\gamma_E"}];
latexMomentumSquared[q_] := If[Head[q] === Plus, "(" <> latexOfExpression[q] <> ")^{2}", latexParenthesized[q] <> "^{2}"];
(* the denominator (q^2 + m^2)^a of a line, and whether it needs parentheses next to other factors *)
latexPropagatorDenominator[q_, m_, a_] := With[{s = If[m === 0, latexMomentumSquared[q], latexMomentumSquared[q] <> " + " <> latexParenthesized[m] <> "^{2}"]},
   If[a === 1, s, "\\left(" <> s <> "\\right)^{" <> latexOfExpression[a] <> "}"]];
propagatorFactorQ[f_] := MatchQ[f, ScalarPropagator[_, _]] || MatchQ[f, Power[ScalarPropagator[_, _], a_?NumericQ] /; a > 0];
latexPropagatorFactor[ScalarPropagator[q_, m_]] := {m =!= 0, latexPropagatorDenominator[q, m, 1]};
latexPropagatorFactor[Power[ScalarPropagator[q_, m_], a_]] := {False, latexPropagatorDenominator[q, m, a]};
(* a product with lines: one fraction, the lines in its denominator *)
latexWithPropagators[f_] := Module[{fs = If[Head[f] === Times, List @@ f, {f}], lines, others, dens},
   lines = Select[fs, propagatorFactorQ]; others = Times @@ Select[fs, ! propagatorFactorQ[#] &];
   If[lines === {}, latexOfExpression[f],
    dens = latexPropagatorFactor /@ lines;
    If[NumberQ[others] && others < 0, "-", ""] <> "\\frac{" <> If[others === 1 || others === -1, "1", latexOfExpression[If[NumberQ[others], Abs[others], others]]] <> "}{" <>
     joinStrings[Table[If[Length[dens] > 1 && d[[1]], "(" <> d[[2]] <> ")", d[[2]]], {d, dens}], "\\,"] <> "}"]];
latexFactorOf[ScalarPropagator[q_, m_]] := "\\frac{1}{" <> latexPropagatorDenominator[q, m, 1] <> "}";
latexFactorOf[Power[ScalarPropagator[q_, m_], a_]] := "\\frac{1}{" <> latexPropagatorDenominator[q, m, a] <> "}";
latexFactorOf[MomentumSquared[q_]] := latexMomentumSquared[q];
latexFactorOf[LoopIntegral[f_, ks_List]] := "\\int_{" <> joinStrings[latexArgument /@ ks, "\\,"] <> "}" <>
   If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexWithPropagators[f]];
latexFactorOf[GFunction[a_, b_, eps_]] := "G(" <> latexOfExpression[a] <> "," <> latexOfExpression[b] <> ")";
latexFactorOf[TadpoleIntegral[a_, m_, eps_]] := "A_{" <> latexOfExpression[a] <> "}(" <> latexArgument[m] <> ")";
latexFactorOf[BubbleIntegral[{a_, b_}, p2_, {m1_, m2_}, eps_]] := "B_{" <> latexOfExpression[a] <> "," <> latexOfExpression[b] <> "}(" <>
   latexOfExpression[p2] <> ";" <> latexArgument[m1] <> "," <> latexArgument[m2] <> ")";
latexFactorOf[ArcTanh[x_]] := "\\operatorname{artanh}\\left(" <> latexOfExpression[x] <> "\\right)";
latexFactorOf[Zeta[x_]] := "\\zeta(" <> latexOfExpression[x] <> ")";
(* ========================================================================= *)
(*  19. QED at one loop in dimensional regularization                       *)
(*  The diagrams of the package with spinor and Lorentz indices (condensed  *)
(*  notation: a label stands for a point and its indices; the vertex is a   *)
(*  local kernel V[x, y, z] = c gamma^(mu_z)_(a_x a_y)) in momentum space:  *)
(*  the momenta are routed as in section 17, the spinor indices followed     *)
(*  along the fermion lines (an open line is a product of Dirac matrices, a *)
(*  closed loop a trace), and the one-loop integrals done by Feynman         *)
(*  parameters, shift and tensor reduction in d = 4 - 2 eps.                 *)
(*  Conventions: Minkowski metric (+,-,-,-), {gamma^mu, gamma^nu} = 2 g^munu, *)
(*  Tr 1 = 4, no gamma5 (GammaMatrix[5] is refused); abstract Lorentz        *)
(*  indices (explicit components are refused), a repeated index contracted  *)
(*  (one upper, one lower); momenta are linear combinations of momentum     *)
(*  names (symbols, p[1], LoopMomentum[i]) with coefficients that are       *)
(*  numbers or polynomials in the Feynman parameters;                      *)
(*  FeynmanDenominator[q, m] = 1/(q^2 - m^2 + i0), so that the Feynman-      *)
(*  parameter integrals have Delta - i0 (their N takes it); every loop is    *)
(*  mu^(2 eps) (e^gammaE/(4 pi))^eps Int d^dk/(2 pi)^d, the measure of       *)
(*  section 18, so that minimal subtraction is the MS-bar scheme.            *)
(*  The algebra is done with symbols in place of compound momenta and of the *)
(*  Feynman parameters (Expand and Coefficient work on plain symbols).       *)
(* ========================================================================= *)

DiracSimplify::momentum = "`1` is not a linear combination of momenta with coefficients that are numbers or polynomials in FeynmanParameter[i] (a momentum is a symbol or an object such as p[1] or LoopMomentum[i]): the slashed momenta, four-vectors and scalar products that contain it are left as they are.";
GammaMatrix::gamma5 = "GammaMatrix[5] is not \[Gamma]5: \[Gamma]5 is not implemented (its traces and contractions would be those of an ordinary index called 5); the expression is not evaluated.";
DiracSimplify::component = "`1` has an explicit component as a Lorentz index: the indices are abstract (d-dimensional, contracted when repeated), components are not supported; the expression is not evaluated.";
DiracSimplify::product = "`1` is a commutative product (Times or Power) of Dirac matrices: a product of matrices is DiracProduct[a, b, ...]; the expression is not evaluated.";
DiracSimplify::index = "The Lorentz index `1` appears more than twice in the term `2`; the expression is not evaluated.";
(* an integrand that is 0 (e.g. a trace of an odd number of gamma matrices taken inside the integral) *)
MinkowskiLoopIntegral[0, _List] := 0;
SetAttributes[LorentzMetric, Orderless];
SetAttributes[ScalarProduct, Orderless];

(* ---- momenta: linear combinations of momentum names with coefficients that are numbers or polynomials in the Feynman
   parameters ---- *)
(* the symbols that are coefficients (the Feynman parameters, as symbols, during IntegrateOneLoop) *)
$coefficientSymbols = {};
nonMomentumHeads = GammaMatrix | SlashedMomentum | DiracProduct | DiracTrace | SpinorChain | LorentzMetric | FourVector |
   ScalarProduct | FeynmanDenominator | MinkowskiLoopIntegral | ParameterIntegral | SimplexIntegral | FeynmanParameterIntegral |
   FeynmanParameter | List | Rule | RuleDelayed;
momentumNameQ[s_Symbol] := ! NumericQ[s] && s =!= SpacetimeDimension && ! MemberQ[$coefficientSymbols, s];
momentumNameQ[s_] := ! AtomQ[s] && ! NumericQ[s] && Head[Head[s]] === Symbol && ! MatchQ[Head[s], nonMomentumHeads] &&
   ! MemberQ[Attributes[Evaluate[Head[s]]], NumericFunction];
coefficientQ[c_] := NumericQ[c /. FeynmanParameter[_] -> 1/3 /. Thread[$coefficientSymbols -> 1/3]];
(* a term of a momentum: {coefficient, name}, or $Failed *)
momentumTerm[t_] := Which[
   momentumNameQ[t], {1, t},
   Head[t] === Times, With[{names = Select[List @@ t, momentumNameQ]},
    If[Length[names] == 1 && coefficientQ[t/names[[1]]], {t/names[[1]], names[[1]]}, $Failed]],
   True, $Failed];
(* the terms {coefficient, name} of a linear combination, or $Failed *)
momentumTerms[q_] := With[{x = Expand[q]}, If[x === 0, {},
    With[{ts = Table[momentumTerm[t], {t, productTermsOf[x]}]}, If[MemberQ[ts, $Failed], $Failed, ts]]]];
linearMomentumQ[q_] := ! momentumNameQ[q] && momentumTerms[q] =!= $Failed;
splittableQ[q_] := momentumTerms[q] =!= $Failed;
linearMomentumRules = {
   SlashedMomentum[q_] /; linearMomentumQ[q] :> Total[Table[t[[1]] SlashedMomentum[t[[2]]], {t, momentumTerms[q]}]],
   FourVector[q_, mu_] /; linearMomentumQ[q] :> Total[Table[t[[1]] FourVector[t[[2]], mu], {t, momentumTerms[q]}]],
   ScalarProduct[a_, b_] /; (linearMomentumQ[a] || linearMomentumQ[b]) && splittableQ[a] && splittableQ[b] :>
    Total[Flatten[Table[s[[1]] t[[1]] ScalarProduct[s[[2]], t[[2]]], {s, momentumTerms[a]}, {t, momentumTerms[b]}]]]};
scalarProductOf[a_, b_] := Expand[ScalarProduct[a, b] //. linearMomentumRules];
(* the momenta of an expression that are not linear combinations (they are left as they are, with a message) *)
momentumProblems[e_] := Union[Select[Join[Cases[{e}, SlashedMomentum[q_] :> q, Infinity], Cases[{e}, FourVector[q_, _] :> q, Infinity],
     Flatten[Cases[{e}, ScalarProduct[a_, b_] :> {a, b}, Infinity]]], ! splittableQ[#] &]];
(* the compound momentum names (p[1], LoopMomentum[i]): replaced by symbols during the algebra *)
compoundMomentumNames[e_] := Union[Cases[Join[Cases[{e}, (SlashedMomentum | FeynmanDenominator)[q_, ___] :> q, Infinity],
     Cases[{e}, FourVector[q_, _] :> q, Infinity], Flatten[Cases[{e}, ScalarProduct[a_, b_] :> {a, b}, Infinity]],
     Cases[{e}, SpinorChain[{q2_, _}, _, {q1_, _}] :> Sequence[q2, q1], Infinity]], s_ /; ! AtomQ[s] && momentumNameQ[s], {0, Infinity}]];
symbolsFor[xs_List, name_String] := Table[Unique["FunctionalDiagrams`Private`" <> name], {Length[xs]}];

(* ---- products of Dirac matrices: linearity ---- *)
diracFreeQ[x_] := FreeQ[x, GammaMatrix | SlashedMomentum | DiracProduct | DiracTrace | SpinorChain];
(* a matrix outside DiracProduct is a product of one factor *)
wrapDiracElements[e_] := e /. {g_GammaMatrix :> DiracProduct[g], s_SlashedMomentum :> DiracProduct[s]};
(* a loop integral inside a spinor chain or a trace is taken out of it (the spinors do not depend on the loop momentum,
   and the trace is linear), so that the indices are counted term by term in its integrand; a loop momentum with the name
   of a momentum of the spinors is renamed first (it is a bound variable) *)
freshLoopMomenta[ks_List] := Table[Unique["FunctionalDiagrams`Private`k"], {Length[ks]}];
productRules = {
   SpinorChain[u_, MinkowskiLoopIntegral[f_, ks_List], v_] :> With[{new = If[FreeQ[{u, v}, Alternatives @@ ks], ks, freshLoopMomenta[ks]]},
     MinkowskiLoopIntegral[SpinorChain[u, f /. Thread[ks -> new], v], new]],
   DiracTrace[MinkowskiLoopIntegral[f_, ks_]] :> MinkowskiLoopIntegral[DiracTrace[f], ks],
   DiracProduct[a___, DiracProduct[b___], c___] :> DiracProduct[a, b, c],
   DiracProduct[a___, b_Plus, c___] :> (DiracProduct[a, #, c] & /@ b),
   DiracProduct[a___, b_?diracFreeQ, c___] :> b DiracProduct[a, c],
   DiracProduct[a___, Times[b_?diracFreeQ, r__], c___] :> b DiracProduct[a, Times[r], c],
   DiracTrace[b_Plus] :> (DiracTrace /@ b),
   DiracTrace[b_?diracFreeQ] :> 4 b,
   DiracTrace[Times[b_?diracFreeQ, r__]] :> b DiracTrace[Times[r]],
   SpinorChain[u_, b_Plus, v_] :> (SpinorChain[u, #, v] & /@ b),
   SpinorChain[u_, b_?diracFreeQ, v_] :> b SpinorChain[u, DiracProduct[], v],
   SpinorChain[u_, Times[b_?diracFreeQ, r__], v_] :> b SpinorChain[u, Times[r], v]};
(* the integrand of an unevaluated loop integral is expanded too, so that its indices are contracted *)
diracLinear[e_] := Expand[Expand[wrapDiracElements[e /. MinkowskiLoopIntegral[f_, ks_] :> MinkowskiLoopIntegral[Expand[f], ks]] //.
      linearMomentumRules] //. Join[productRules, linearMomentumRules]];

(* ---- input that is not evaluated: gamma5, components, commutative products of matrices, an index three times ---- *)
matrixValuedQ[f_] := ! FreeQ[f /. {_DiracTrace -> 1, _SpinorChain -> 1}, GammaMatrix | SlashedMomentum | DiracProduct];
componentIndexPattern = GammaMatrix[_?NumericQ] | FourVector[_, _?NumericQ] | LorentzMetric[_?NumericQ, ___] | LorentzMetric[_, _?NumericQ, ___];
(* True (after the message) when e is not evaluated *)
diracInputProblemQ[e_] := Module[{g5 = Cases[{e}, GammaMatrix[5], Infinity], comps, prods},
   comps = Cases[{e}, componentIndexPattern, Infinity];
   (* the test function only on the products that contain a matrix at all *)
   prods = Join[Cases[{e}, t_Times /; ! FreeQ[t, GammaMatrix | SlashedMomentum | DiracProduct] && Count[List @@ t, _?matrixValuedQ] > 1, Infinity],
     Cases[{e}, Power[_?matrixValuedQ, _], Infinity]];
   Which[
    g5 =!= {}, Message[GammaMatrix::gamma5]; True,
    comps =!= {}, Message[DiracSimplify::component, First[comps]]; True,
    prods =!= {}, Message[DiracSimplify::product, First[prods]]; True,
    True, False]];
termIndices[t_] := Flatten[Table[Join[Cases[{f}, GammaMatrix[a_] :> a, Infinity], Flatten[Cases[{f}, LorentzMetric[a_, b_] :> {a, b}, Infinity]],
     Cases[{f}, FourVector[_, a_] :> a, Infinity]], {f, expandIntegerPowers[factorList[t]]}]];
(* the free indices of e (those once in a term), after checking every level: in a term, the indices of the Dirac and
   tensor factors count, and a factor of another kind (a MinkowskiLoopIntegral, a Feynman-parameter integral, a power)
   counts as the free indices of its arguments, each checked in the same way; the elements of a list are independent *)
algebraFactorQ[f_] := MatchQ[f, _GammaMatrix | _SlashedMomentum | _DiracProduct | _DiracTrace | _SpinorChain | _LorentzMetric |
    _FourVector | _ScalarProduct];
indexUse[e_List] := (Scan[indexUse, e]; {});
(* an expression without indices (a Feynman-parameter integral, a Gamma function): nothing to count, nothing to expand *)
indexUse[e_] /; FreeQ[e, GammaMatrix | LorentzMetric | FourVector] := {};
indexUse[e_] := Union @@ Table[With[{fs = expandIntegerPowers[factorList[t]]},
     With[{all = Join[termIndices[Times @@ Select[fs, algebraFactorQ]],
         Flatten[Table[indexUse /@ (List @@ f), {f, Select[fs, ! AtomQ[#] && ! algebraFactorQ[#] &]}]]]},
      If[MemberQ[Tally[all][[All, 2]], n_ /; n > 2],
       Message[DiracSimplify::index, Select[Tally[all], #[[2]] > 2 &][[1, 1]], t]; Throw[True, overused]];
      Cases[Tally[all], {a_, 1} :> a]]], {t, productTermsOf[Expand[e]]}];
(* True (after the message) when an index appears more than twice in a term, at some level *)
overusedIndexQ[e_] := Catch[indexUse[e]; False, overused];

(* ---- contractions ---- *)
(* gamma^a X gamma_a for X = x1 ... xn, as {coefficient, product} pairs: C(X a) = 2 a X - C(X) a, C() = d *)
gammaContraction[{}] := {{SpacetimeDimension, {}}};
gammaContraction[xs_List] := Join[{{2, Prepend[Most[xs], Last[xs]]}},
   Table[{-c[[1]], Append[c[[2]], Last[xs]]}, {c, gammaContraction[Most[xs]]}]];
elementDot[GammaMatrix[a_], GammaMatrix[b_]] := LorentzMetric[a, b];
elementDot[GammaMatrix[a_], SlashedMomentum[p_]] := FourVector[p, a];
elementDot[SlashedMomentum[p_], GammaMatrix[a_]] := FourVector[p, a];
elementDot[SlashedMomentum[p_], SlashedMomentum[q_]] := ScalarProduct[p, q];
contractionRules = {
   LorentzMetric[a_, a_] :> SpacetimeDimension,
   Power[LorentzMetric[a_, b_], 2] :> SpacetimeDimension,
   LorentzMetric[a_, b_] LorentzMetric[b_, c_] :> LorentzMetric[a, c],
   LorentzMetric[a_, b_] FourVector[p_, b_] :> FourVector[p, a],
   FourVector[p_, a_] FourVector[q_, a_] :> ScalarProduct[p, q],
   Power[FourVector[p_, a_], 2] :> ScalarProduct[p, p],
   LorentzMetric[a_, b_] (h : DiracProduct | DiracTrace | SpinorChain)[x___] /; ! FreeQ[{x}, GammaMatrix[b]] :> (h[x] /. GammaMatrix[b] -> GammaMatrix[a]),
   FourVector[p_, a_] (h : DiracProduct | DiracTrace | SpinorChain)[x___] /; ! FreeQ[{x}, GammaMatrix[a]] :> (h[x] /. GammaMatrix[a] -> SlashedMomentum[p]),
   DiracProduct[x___, GammaMatrix[a_], y___, GammaMatrix[a_], z___] :>
    Total[Table[c[[1]] DiracProduct[x, Sequence @@ c[[2]], z], {c, gammaContraction[{y}]}]],
   DiracProduct[x___, SlashedMomentum[p_], SlashedMomentum[p_], z___] :> ScalarProduct[p, p] DiracProduct[x, z]};

(* ---- traces: Tr(a1 ... an) = Sum_k (-1)^k (a1.ak) Tr(a2 ... (no ak) ... an), Tr 1 = 4; memoized in traceTable, which
   DiracSimplify makes local (Block) ---- *)
diracTraceOf[xs_List] := With[{v = traceTable[xs]}, If[Head[v] === traceTable, traceTable[xs] = diracTraceValue[xs], v]];
diracTraceValue[xs_List] := Which[xs === {}, 4, OddQ[Length[xs]], 0, True,
   Sum[(-1)^k elementDot[xs[[1]], xs[[k]]] diracTraceOf[Delete[Rest[xs], k - 1]], {k, 2, Length[xs]}]];
traceRules = {DiracTrace[DiracProduct[xs___]] :> diracTraceOf[{xs}]};

(* ---- on-shell spinor chains ubar(p2) X u(p1): pslash1 moved to the right (pslash1 u(p1) = m1 u(p1)), pslash2 to the
   left (ubar(p2) pslash2 = m2 ubar(p2)), by {a, b} = 2 a.b ---- *)
chainReduce[xs_List, u_List, v_List] := Module[{n = Length[xs], s1 = SlashedMomentum[v[[1]]], s2 = SlashedMomentum[u[[1]]], i},
   Which[
    n > 0 && Last[xs] === s1, v[[2]] chainReduce[Most[xs], u, v],
    n > 0 && First[xs] === s2, u[[2]] chainReduce[Rest[xs], u, v],
    MemberQ[xs, s1], i = Position[xs, s1, {1}][[1, 1]];
     If[xs[[i + 1]] === s1, ScalarProduct[v[[1]], v[[1]]] chainReduce[Delete[xs, {{i}, {i + 1}}], u, v],
      2 elementDot[s1, xs[[i + 1]]] chainReduce[Delete[xs, {{i}, {i + 1}}], u, v] -
       chainReduce[ReplacePart[xs, {i -> xs[[i + 1]], i + 1 -> s1}], u, v]],
    MemberQ[xs, s2], i = Position[xs, s2, {1}][[1, 1]];
     If[xs[[i - 1]] === s2, ScalarProduct[u[[1]], u[[1]]] chainReduce[Delete[xs, {{i - 1}, {i}}], u, v],
      2 elementDot[xs[[i - 1]], s2] chainReduce[Delete[xs, {{i - 1}, {i}}], u, v] -
       chainReduce[ReplacePart[xs, {i - 1 -> s2, i -> xs[[i - 1]]}], u, v]],
    True, SpinorChain[u, DiracProduct @@ xs, v]]];
chainRules = {SpinorChain[u : {p2_, m2_}, DiracProduct[xs___], v : {p1_, m1_}] /;
     MemberQ[{xs}, SlashedMomentum[p1] | SlashedMomentum[p2]] :> chainReduce[{xs}, u, v]};
onShellRules[e_] := Union[Flatten[Cases[{e}, SpinorChain[{p2_, m2_}, _, {p1_, m1_}] :>
      {ScalarProduct[p1, p1] -> m1^2, ScalarProduct[p2, p2] -> m2^2}, Infinity]]];

diracStep[e_] := Module[{f = diracLinear[e]},
   f = Expand[f //. Join[contractionRules, productRules, linearMomentumRules]];
   f = Expand[f //. traceRules];
   f = Expand[f //. Join[contractionRules, productRules]];
   f = Expand[f //. chainRules];
   f /. onShellRules[f]];
DiracSimplify[e_] := Catch[Module[{names, syms, f, blobs, masks, ruleSets = {}, g},
    If[diracInputProblemQ[e], Throw[$Failed, diracFailure]];
    Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[e]];
    names = compoundMomentumNames[e]; syms = symbolsFor[names, "momentum"];
    f = diracLinear[e /. Thread[names -> syms]];
    If[overusedIndexQ[f], Throw[$Failed, diracFailure]];
    (* the Feynman-parameter integrals carry no matrix and no index: symbols during the algebra, which changes them only
       by the on-shell rules of the spinor chains, applied to them afterwards, those of each step in turn *)
    blobs = Select[Union[Cases[{f}, _ParameterIntegral | _SimplexIntegral | _FeynmanParameterIntegral, Infinity]],
      diracFreeQ[#] && FreeQ[#, LorentzMetric | FourVector] &];
    masks = symbolsFor[blobs, "integral"];
    g = Block[{traceTable}, FixedPoint[With[{s = diracStep[#]}, AppendTo[ruleSets, onShellRules[s]]; s] &, f /. Thread[blobs -> masks], 30]];
    g /. Thread[masks -> Fold[ReplaceAll, blobs, ruleSets]] /. Thread[syms -> names]], diracFailure];

(* ---- a canonical order of the factors of each product (by anticommutation), to compare products ---- *)
orderedProduct[xs_List] := Module[{n = Length[xs], i},
   i = Select[Range[n - 1], xs[[#]] === xs[[# + 1]] || ! OrderedQ[{xs[[#]], xs[[# + 1]]}] &];
   If[i === {}, DiracProduct @@ xs, i = First[i];
    If[xs[[i]] === xs[[i + 1]], elementDot[xs[[i]], xs[[i]]] orderedProduct[Delete[xs, {{i}, {i + 1}}]],
     2 elementDot[xs[[i]], xs[[i + 1]]] orderedProduct[Delete[xs, {{i}, {i + 1}}]] -
      orderedProduct[ReplacePart[xs, {i -> xs[[i + 1]], i + 1 -> xs[[i]]}]]]]];
DiracOrder[e_] := With[{s = DiracSimplify[e]}, If[s === $Failed, $Failed, DiracSimplify[s /. DiracProduct[xs___] :> orderedProduct[{xs}]]]];

(* ---- the momentum-space integrand of the diagrams of the package ---- *)
QEDIntegrand::rule = "`1` is not a rule of QEDIntegrand: use S -> {\"Fermion\", m}, Ph -> {\"Photon\", \[Xi]} or Ph -> {\"Photon\", {\[Xi], \[Lambda]}} (\[Lambda] a photon mass), V -> {\"Vertex\", c} (V[x, y, z] = c \[Gamma]^\[Mu], \[Mu] the Lorentz index of z, the spinor indices of x and y).";
QEDIntegrand::external = DiagramLoopIntegral::external;
QEDIntegrand::factor = DiagramLoopIntegral::factor;
QEDIntegrand::grassmann = DiagramLoopIntegral::grassmann;
QEDIntegrand::conservation = DiagramLoopIntegral::conservation;
QEDIntegrand::loops = "The diagram has more than one loop (`2`): only one-loop diagrams are evaluated.";
QEDIntegrand::spinor = "The spinor indices of `1` do not form fermion lines (each label once as a row, once as a column), or there is more than one open fermion line.";
(* the rules are checked as those of DiagramLoopIntegral (checkRuleKinds); the photon parameters are xi or {xi, lambda} *)
photonParameters[par_] := If[ListQ[par], par, {par, 0}];
qedRulesQ[rules_List] := checkRuleKinds[rules, {"Fermion", "Photon", "Vertex"}, QEDIntegrand] && With[
    {bad = Select[rules, #[[2, 1]] === "Photon" && ListQ[#[[2, 2]]] && Length[#[[2, 2]]] =!= 2 &]},
    Scan[Message[QEDIntegrand::rule, #] &, bad]; bad === {}];
QEDIntegrand[e_, externals_, rules_List, opts___Rule] := Module[{k, indexRules, terms},
   checkOptionNames[QEDIntegrand, {opts}, {"LoopMomentum", "Indices"}];
   k = optionValue[{opts}, {"LoopMomentum" -> LoopMomentum[1]}, "LoopMomentum"];
   indexRules = optionValue[{opts}, {"Indices" -> {}}, "Indices"];
   Which[
    ! qedRulesQ[rules], $Failed,
    ! externalMomentaQ[externals], Message[QEDIntegrand::external, externals]; $Failed,
    True, terms = qedTerm[#, externals, rules, k, indexRules] & /@ canonicalTermsOf[e];
     If[MemberQ[terms, $Failed], $Failed, Total[terms]]]];

(* the fermion lines of a list of spinor pieces {row label, column label, matrix}: one open line (a product) and closed
   loops (traces) *)
spinorLines[pieces_List, t_] := Module[{rows = Table[p[[1]], {p, pieces}], cols = Table[p[[2]], {p, pieces}], left = pieces,
    starts, chain, result = 1, open = 0, follow},
   If[Length[Union[rows]] < Length[rows] || Length[Union[cols]] < Length[cols], Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
   follow[c_] := Module[{ch = c, nx},
     While[(nx = Select[left, #[[1]] === Last[ch][[2]] &]) =!= {}, AppendTo[ch, First[nx]]; left = DeleteCases[left, First[nx]]]; ch];
   starts = Select[pieces, ! MemberQ[cols, #[[1]]] &];
   Do[left = DeleteCases[left, s]; chain = follow[{s}]; open++;
    result = result DiracProduct @@ Table[c[[3]], {c, chain}], {s, starts}];
   While[left =!= {},
    chain = {First[left]}; left = Rest[left]; chain = follow[chain];
    If[Last[chain][[2]] =!= First[chain][[1]], Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
    result = result DiracTrace[DiracProduct @@ Table[c[[3]], {c, chain}]]];
   If[open > 1, Message[QEDIntegrand::spinor, t]; Throw[$Failed, qedFailure]];
   result];

qedTerm[t : diagramTerm[c_, o_, d_], externals_List, rules_List, k_, indexRules_] := Catch[Module[
    {kindOf, vertexHeads, lineHeads, c2, data, index, pieces = {}, others = 1, vertices},
    kindOf[h_] := First[Select[rules, #[[1]] === h &]][[2]];
    vertexHeads = Table[r[[1]], {r, Select[rules, #[[2, 1]] === "Vertex" &]}];
    lineHeads = Table[r[[1]], {r, Select[rules, MemberQ[{"Fermion", "Photon"}, #[[2, 1]]] &]}];
    index[x_] := Replace[x, indexRules];
    (* the local vertices identify their labels, for the routing *)
    c2 = c /. v_[a_, b_, z_] /; MemberQ[vertexHeads, v] :> DeltaFunction[a, b] DeltaFunction[a, z];
    data = routeTerm[diagramTerm[c2, o, d], momentumExternals[externals],
      Table[h -> {"Instantaneous", Function[q, 1]}, {h, lineHeads}], {k}, QEDIntegrand];
    If[data === $Failed, Throw[$Failed, qedFailure]];
    (* the lines of routeTerm, h[x, y] with its momentum flowing from y to x *)
    Do[With[{x = line[[6, 2]], y = line[[6, 1]], p = line[[5, 1]], kind = kindOf[line[[3]]]},
      If[kind[[1]] === "Fermion",
       AppendTo[pieces, {x, y, I (SlashedMomentum[p] + kind[[2]])}]; others = others FeynmanDenominator[p, kind[[2]]],
       With[{xi = photonParameters[kind[[2]]][[1]], lambda = photonParameters[kind[[2]]][[2]], mu = index[x], nu = index[y]},
        others = others (-I) FeynmanDenominator[p, lambda] (LorentzMetric[mu, nu] -
            (1 - xi) FourVector[p, mu] FourVector[p, nu] FeynmanDenominator[p, lambda])]]], {line, routeData["Lines", data]}];
    vertices = Select[expandIntegerPowers[factorList[c]], ! AtomQ[#] && MemberQ[vertexHeads, Head[#]] && Length[#] == 3 &];
    Do[AppendTo[pieces, {v[[1]], v[[2]], kindOf[Head[v]][[2]] GammaMatrix[index[v[[3]]]]}], {v, vertices}];
    With[{f = spinorLines[pieces, termToExpression[t]] others, coefficient = routeData["Coefficient", data]},
     If[routeData["LoopMomenta", data] === {}, coefficient f, coefficient MinkowskiLoopIntegral[f, {k}]]]], qedFailure];

(* ---- one-loop integrals: Feynman parameters, shift, tensor reduction, d-dimensional master formula ---- *)
IntegrateOneLoop::loops = "`1` has more than one loop momentum: only one-loop integrals are evaluated.";
IntegrateOneLoop::momentum = "The denominator momentum `1` is not +-`2` plus external momenta.";
IntegrateOneLoop::numerator = "The factor `1` of a numerator depends on the loop momentum `2` in a way that is not reduced (only slashed momenta, four-vectors and scalar products are, also in Dirac products, traces and spinor chains): the integral is left unevaluated.";
IntegrateOneLoop::power = "The power `1` of a denominator that depends on the loop momentum is not an integer: the integral is left unevaluated.";
IntegrateOneLoop[e_, eps_Symbol] := e /. MinkowskiLoopIntegral[f_, ks_List] :>
    If[Length[ks] == 1, With[{r = oneLoopIntegral[f, ks[[1]], eps]}, If[r === $Failed, MinkowskiLoopIntegral[f, ks], r]],
     Message[IntegrateOneLoop::loops, MinkowskiLoopIntegral[f, ks]]; MinkowskiLoopIntegral[f, ks]];

(* every factor of a term that depends on the loop momentum l: a denominator (an integer power of it), or reduced *)
reducedFactorQ[f_, l_] := MatchQ[f, ScalarProduct[l, _] | FourVector[l, _]] ||
   (MatchQ[f, (DiracProduct | DiracTrace | SpinorChain)[___]] && FreeQ[f /. SlashedMomentum[l] -> 1, l]);
checkNumerator[t_, l_, k_, back_List] := Scan[Which[
     FreeQ[#, l] || Head[#] === FeynmanDenominator || reducedFactorQ[#, l], Null,
     Head[#] === Power && Head[#[[1]]] === FeynmanDenominator, Message[IntegrateOneLoop::power, # /. back]; Throw[$Failed, loopFailure],
     True, Message[IntegrateOneLoop::numerator, # /. back, k]; Throw[$Failed, loopFailure]] &, expandIntegerPowers[factorList[t]]];

(* the loop momentum and the compound external momenta are symbols during the algebra; a negative integer power of a
   denominator is a numerator *)
oneLoopIntegral[f_, k_, eps_] := Catch[Module[{names, syms, back, l, g, terms, groups},
    If[diracInputProblemQ[f], Throw[$Failed, loopFailure]];
    Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[f]];
    names = Union[compoundMomentumNames[f], {k}]; syms = symbolsFor[names, "momentum"]; back = Thread[syms -> names];
    l = Replace[k, Thread[names -> syms]];
    g = f /. Thread[names -> syms] /. Power[FeynmanDenominator[q_, m_], n_Integer?Negative] /; ! FreeQ[q, l] :> (ScalarProduct[q, q] - m^2)^(-n);
    terms = productTermsOf[diracLinear[g]];
    Scan[checkNumerator[#, l, k, back] &, terms];
    groups = GatherBy[Table[splitDenominators[t, l, back], {t, terms}], First];
    Total[Table[feynmanParametrized[gr[[1, 1]], Total[Table[h[[2]], {h, gr}]], l, eps], {gr, groups}]] /. back], loopFailure];

(* a term: {denominators {q, m, power} with q = k + r (sorted), numerator}; back: the symbols back to the momenta, for the
   message *)
splitDenominators[t_, k_, back_List] := Module[{dens = {}, num = 1, q, c},
   Do[With[{base = baseAndExponent[f][[1]], n = baseAndExponent[f][[2]]},
     If[Head[base] === FeynmanDenominator && ! FreeQ[base[[1]], k] && IntegerQ[n] && n > 0,
      q = Expand[base[[1]]]; c = Coefficient[q, k];
      If[c =!= 1 && c =!= -1, Message[IntegrateOneLoop::momentum, base[[1]] /. back, k /. back]; Throw[$Failed, loopFailure]];
      AppendTo[dens, {Expand[c q], base[[2]], n}],
      num = num f]], {f, factorList[t]}];
   {Sort[Table[{g[[1, 1]], g[[1, 2]], Total[Table[h[[3]], {h, g}]]}, {g, GatherBy[dens, Take[#, 2] &]}]], num}];

(* the pairings of the indices of l^a1 ... l^a2n *)
pairingSum[{}] := 1;
pairingSum[{a_, rest___}] := Total[Table[LorentzMetric[a, {rest}[[j]]] pairingSum[Delete[{rest}, j]], {j, Length[{rest}]}]];
$tensorIndexCount = 0;
newTensorIndex[] := tensorIndex[++$tensorIndexCount];
(* a term of the shifted numerator: {J, structure} with l^a1 ... l^a2n -> (l^2)^n Sym(g...)/(d (d + 2) ... (d + 2n - 2)) *)
tensorReduce[t_, l_] := Module[{idx = {}, rest = 1, l2 = 0, n},
   Do[Which[
     f === ScalarProduct[l, l], l2++,
     Head[f] === ScalarProduct && ! FreeQ[f, l], With[{a = newTensorIndex[], v = If[f[[1]] === l, f[[2]], f[[1]]]},
      AppendTo[idx, a]; rest = rest FourVector[v, a]],
     MatchQ[f, FourVector[l, _]], AppendTo[idx, f[[2]]],
     MatchQ[f, (DiracProduct | DiracTrace | SpinorChain)[___]] && FreeQ[f /. SlashedMomentum[l] -> 1, l],
      rest = rest (f /. SlashedMomentum[l] :> With[{a = newTensorIndex[]}, AppendTo[idx, a]; GammaMatrix[a]]),
     ! FreeQ[f, l], Message[IntegrateOneLoop::numerator, f, l]; Throw[$Failed, loopFailure],
     True, rest = rest f], {f, expandIntegerPowers[factorList[t]]}];
   n = Length[idx]/2;
   If[OddQ[Length[idx]], {}, {{l2 + n, rest pairingSum[idx]/Product[SpacetimeDimension + 2 j, {j, 0, n - 1}]}}]];

(* no denominator: a polynomial in k (the numerator has been checked), scaleless, 0 in dimensional regularization *)
feynmanParametrized[{}, num_, k_, eps_] := 0;
(* the Feynman parameters are symbols during the algebra, FeynmanParameter[i] in the result *)
feynmanParametrized[dens_List, num_, k_, eps_] := Catch[Module[{n = Length[dens], d = 4 - 2 eps, xs, rs, ms, as, bigA, r, delta, l,
    shifted, reduced, scalar, result},
   xs = If[n == 1, {1}, symbolsFor[Range[n], "parameter"]];
   Block[{$coefficientSymbols = Join[$coefficientSymbols, If[n == 1, {}, xs]]},
    rs = Table[Expand[dd[[1]] - k], {dd, dens}]; ms = Table[dd[[2]], {dd, dens}]; as = Table[dd[[3]], {dd, dens}];
    bigA = Total[as];
    r = Expand[Total[xs rs]];
    delta = Expand[scalarProductOf[r, r] - Sum[xs[[i]] (scalarProductOf[rs[[i]], rs[[i]]] - ms[[i]]^2), {i, n}]];
    (* a scaleless integral (a massless tadpole): 0 *)
    If[delta === 0, Throw[0, scalelessIntegral]];
    l = Unique["FunctionalDiagrams`Private`l"];
    shifted = diracLinear[num /. k -> (l - r)];
    reduced = Flatten[Table[tensorReduce[t, l], {t, productTermsOf[shifted]}], 1];
    (* the scalar factor of a term {J, structure}: the master formula for (l^2)^J *)
    scalar[j_] := I (-1)^(bigA + j) loopMeasure[eps] Gamma[Expand[j + d/2]] Gamma[Expand[bigA - j - d/2]]/
       (Gamma[Expand[d/2]] Product[Gamma[a], {a, as}]) Product[xs[[i]]^(as[[i]] - 1), {i, n}] Power[delta, Expand[d/2 + j - bigA]];
    result = groupedDiracSimplify[reduced, scalar, onShellRules[shifted]] /. SpacetimeDimension -> d;
    collectStructures[result, xs]] /. If[n == 1, {}, Thread[xs -> Table[FeynmanParameter[i], {i, n}]]]], scalelessIntegral];

(* Sum_J scalar[J] structure through DiracSimplify, for the terms {J, structure}: DiracSimplify is linear over the factors
   without matrices and indices, so the algebra runs on the structures of each J alone (the on-shell rules of the spinor
   chains act on the scalar factors). The messages are those of DiracSimplify of the whole sum: when a group fails (an
   index more than twice in a term), the whole sum goes through DiracSimplify, as it is *)
groupedDiracSimplify[reduced_List, scalar_, onShell_List] := Module[{groups = GatherBy[reduced, First], parts, whole},
   whole := Total[Table[rd[[2]] scalar[rd[[1]]], {rd, reduced}]];
   parts = Quiet[Table[DiracSimplify[Total[Table[rd[[2]], {rd, gr}]]], {gr, groups}],
     {GammaMatrix::gamma5, DiracSimplify::component, DiracSimplify::product, DiracSimplify::momentum, DiracSimplify::index}];
   If[MemberQ[parts, $Failed], DiracSimplify[whole],
    If[momentumProblems[{reduced, Table[scalar[gr[[1, 1]]], {gr, groups}]}] =!= {},
     Scan[Message[DiracSimplify::momentum, #] &, momentumProblems[whole]]];
    Expand[Total[Table[(scalar[groups[[i, 1, 1]]] /. onShell) parts[[i]], {i, Length[groups]}]]]]];
(* Sum over the tensor and Dirac structures of structure x (Feynman-parameter integral of its scalar coefficient) *)
structureFactorQ[f_] := ! diracFreeQ[f] || ! FreeQ[f, LorentzMetric | FourVector];
collectStructures[e_, xs_List] := Module[{pairs},
   pairs = Table[With[{s = Times @@ Select[factorList[t], structureFactorQ]}, {s, t/s}], {t, productTermsOf[Expand[e]]}];
   Total[Table[g[[1, 1]] parameterIntegralOf[Total[Table[h[[2]], {h, g}]], xs], {g, GatherBy[pairs, First]}]]];
(* the bases of the powers (the Delta of the master formula) are written as expanded polynomials *)
expandedBases[f_] := f /. Power[b_, e_] /; ! IntegerQ[e] && Head[b] === Plus :> Power[Expand[b], e];
parameterIntegralOf[f_, xs_List] := Which[
   Length[xs] == 1, f,
   Length[xs] == 2, ParameterIntegral[expandedBases[Expand[f /. xs[[2]] -> 1 - xs[[1]]]], xs[[1]]],
   True, SimplexIntegral[expandedBases[f], xs]];

(* ---- the Feynman-parameter integrals ---- *)
(* d/dv of an expression linear in the integrals: the coefficients and the integrands are differentiated (D itself would
   apply the chain rule to the inert heads) *)
DifferentiateUnderIntegral[e_, v_] := Module[{ints = Union[Cases[{e}, _ParameterIntegral | _SimplexIntegral, Infinity]], syms, f},
   syms = Table[Unique["FunctionalDiagrams`Private`integral"], {Length[ints]}];
   f = e /. Thread[ints -> syms];
   (D[f, v] + Total[Table[D[f, syms[[i]]] (ints[[i]] /. (h : ParameterIntegral | SimplexIntegral)[g_, x_] :> h[D[g, v], x]), {i, Length[ints]}]]) /.
    Thread[syms -> ints]];
SetAttributes[{SimplexIntegral, ParameterIntegral}, NHoldAll];
ParameterIntegral[0, _] := 0;
SimplexIntegral[0, _] := 0;

(* N: the Feynman prescription Delta - i0 for the logarithms and the non-integer powers of the bases that depend on the
   variables (Log|b| - i pi, |b|^a e^(-i pi a) where b < 0, above a threshold), the interval split at the real roots of
   those bases (one NIntegrate for each piece); a power <= -1 of a base with a root inside the region (finite with
   Delta - i0, a distribution, but a pole that NIntegrate does not integrate): a message and Indeterminate. The simplex is
   mapped to the unit cube (no limits that depend on the other variables). *)
ParameterIntegral::threshold = "The integrand `1` has a pole inside the region of integration that N does not integrate (a power <= -1 of a base that vanishes there, above a threshold; with \[CapitalDelta] - i0 the integral is finite): it is not evaluated.";
SimplexIntegral::threshold = ParameterIntegral::threshold;
logMinusI0[b_?NumericQ] := If[Im[b] == 0 && Re[b] < 0, Log[-b] - I Pi, Log[b]];
powerMinusI0[b_?NumericQ, a_?NumericQ] := If[Im[b] == 0 && Re[b] < 0, (-b)^a Exp[-I Pi a], b^a];
i0Form[f_, vars_List] := f //. {Log[b_] /; ! FreeQ[b, Alternatives @@ vars] :> logMinusI0[b],
    Power[b_, a_] /; ! FreeQ[b, Alternatives @@ vars] && ! IntegerQ[a] :> powerMinusI0[b, a]};
(* the bases {b, exponent} of the logarithms (exponent 0) and powers of f that depend on the variables *)
branchBases[f_, vars_List] := Union[Join[Cases[{f}, Log[b_] /; ! FreeQ[b, Alternatives @@ vars] :> {b, 0}, Infinity],
    Cases[{f}, Power[b_, a_] /; ! FreeQ[b, Alternatives @@ vars] && ! (IntegerQ[a] && a > 0) && Head[b] =!= Symbol :> {b, a}, Infinity]]];
numericPolynomialQ[p_, vars_List] := PolynomialQ[p, vars] && NumericQ[p /. Thread[vars -> 1/3]];
(* the roots to the working precision of N (Solve, then N: the roots of NSolve were machine numbers, and the pieces of
   the interval then did not end at the roots beyond 16 digits) *)
realRootsInside[b_, x_, opts_List] := With[{p = Numerator[Together[b]]},
   If[numericPolynomialQ[p, {x}] && ! FreeQ[p, x],
    Union[Select[Quiet[N[x /. Solve[p == 0, x], WorkingPrecision /. opts /. WorkingPrecision -> MachinePrecision]],
       Abs[Im[#]] < 10^-10 && 0 < Re[#] < 1 &] /. c_Complex :> Re[c]], {}]];
singularRootQ[{b_, a_}, roots_List] := NumericQ[a] && a != 0 && Re[a] <= -1 && roots =!= {};
parameterNIntegrate[f_, x_, opts_List] := Module[{t = Unique["FunctionalDiagrams`Private`t"], g, bases, roots},
   g = f /. x -> t; bases = branchBases[g, {t}];
   roots = Table[realRootsInside[b[[1]], t, opts], {b, bases}];
   If[Or @@ Table[singularRootQ[bases[[i]], roots[[i]]], {i, Length[bases]}], Message[ParameterIntegral::threshold, f]; Indeterminate,
    (* one NIntegrate for each piece between the roots: with the roots as intermediate points of one NIntegrate, the
       value above machine precision is wrong beyond about 17 digits (NIntegrate::ncvb) *)
    With[{integrand = i0Form[g, {t}], points = Join[{0}, Sort[Union[Flatten[roots]]], {1}]},
     Sum[NIntegrate[integrand, Evaluate[{t, points[[i]], points[[i + 1]]}], Evaluate[Sequence @@ opts]], {i, Length[points] - 1}]]]];
(* x1 = y1, x2 = (1 - y1) y2, ..., xn = (1 - y1) ... (1 - y(n-1)); dx = Prod_i (1 - yi)^(n - 1 - i) dy *)
simplexNIntegrate[f_, xs_List, opts_List] := Module[{n = Length[xs], ys, map, jacobian, g, bases, sample},
   ys = symbolsFor[Range[n - 1], "y"];
   map = Table[xs[[i]] -> If[i < n, ys[[i]], 1] Product[1 - ys[[j]], {j, 1, i - 1}], {i, n}];
   jacobian = Product[(1 - ys[[i]])^(n - 1 - i), {i, 1, n - 2}];
   g = (f /. map) jacobian; bases = Select[branchBases[f, xs], NumericQ[#[[2]]] && #[[2]] != 0 && Re[#[[2]]] <= -1 &];
   (* a base of a power <= -1 that changes sign on the simplex (sampled on a grid) *)
   sample = Table[Union[Sign[Re[N[Flatten[Table[b[[1]] /. map /. Thread[ys -> v], {v, Tuples[Range[1, 9]/10, n - 1]}]]]]]], {b, bases}];
   If[MemberQ[sample, s_ /; MemberQ[s, -1] && MemberQ[s, 1]], Message[SimplexIntegral::threshold, f]; Indeterminate,
    (* above machine precision the product Gauss-Kronrod rule: the default multidimensional rule takes minutes there (the
       method name is Mathematica's; N to machine precision passes no method) *)
    With[{integrand = i0Form[g, ys], iterators = Table[{y, 0, 1}, {y, ys}], method = If[opts === {}, {}, {Method -> "GaussKronrodRule"}]},
     NIntegrate[integrand, Evaluate[Sequence @@ iterators], Evaluate[Sequence @@ opts], Evaluate[Sequence @@ method]]]]];
ParameterIntegral /: N[ParameterIntegral[f_, x_], precision___] := parameterNIntegrate[f, x, precisionOptions[precision]];
SimplexIntegral /: N[SimplexIntegral[f_, xs_List], precision___] := simplexNIntegrate[f, xs, precisionOptions[precision]];

(* the integration variables are symbols while an integral is reduced or expanded *)
withVariableSymbols[fn_, f_, xs_List] := With[{ys = symbolsFor[xs, "x"]}, fn[f /. Thread[xs -> ys], ys] /. Thread[ys -> xs]];
(* the bases of the non-integer powers of f that depend on the variables *)
parameterBases[f_, xs_List] := Union[Cases[{f}, Power[b_, e_] /; ! FreeQ[b, Alternatives @@ xs] && ! IntegerQ[e] :> Expand[b], Infinity]];
(* Int_0^U dy f, f a polynomial in y *)
integrateOutVariable[f_, y_, u_] := Expand[powerSumIntegral[CoefficientList[Expand[f], y], u]];
(* Int over the simplex of a polynomial (Dirichlet: Int x1^a1 ... xn^an = a1! ... an!/(a1 + ... + an + n - 1)!) *)
simplexPolynomialIntegral[poly_, xs_List] := Module[{g = Expand[poly /. Last[xs] -> 1 - Total[Most[xs]]]},
   Do[g = integrateOutVariable[g, xs[[j]], 1 - Total[Take[xs, j - 1]]], {j, Length[xs] - 1, 1, -1}];
   g];
(* a simplex integral whose non-polynomial factors depend on one variable only (after the delta function is used), and
   polynomially on the others, is done in the other variables; the rest stays *)
reducedSimplexIntegral[f_, xs_List] := Catch[Module[{n = Length[xs]},
    If[n == 2, Throw[closedParameterIntegral[expandedBases[Expand[f /. xs[[2]] -> 1 - xs[[1]]]], xs[[1]]], reduced]];
    Do[Module[{others = Delete[xs, i], free, g},
      free = Most[others];
      g = expandedBases[Expand[f /. Last[others] -> 1 - xs[[i]] - Total[free]]];
      If[FreeQ[parameterBases[g, xs], Alternatives @@ free] && PolynomialQ[g, free],
       Do[g = integrateOutVariable[g, free[[j]], 1 - xs[[i]] - Total[Take[free, j - 1]]], {j, Length[free], 1, -1}];
       Throw[closedParameterIntegral[expandedBases[g], xs[[i]]], reduced]]], {i, n}];
    SimplexIntegral[f, xs]], reduced];

(* b = c x^al (1 - x)^be, or $Failed *)
monomialForm[b_, x_] := Module[{r = Together[b], al = 0, be = 0},
   While[al < 12 && r =!= 0 && Together[r /. x -> 0] === 0, r = Cancel[r/x]; al++];
   While[be < 12 && r =!= 0 && Together[r /. x -> 1] === 0, r = Cancel[r/(1 - x)]; be++];
   If[FreeQ[r, x] && r =!= 0, {r, al, be}, $Failed]];
(* c^a for the coefficient c of a base (Delta - i0): for a negative number, (-c)^a e^(-i pi a) *)
coefficientPower[c_, a_] := If[NumericQ[c] && Im[c] == 0 && Re[c] < 0, (-c)^a Exp[-I Pi a], c^a];
(* Int_0^1 dx of a product of x^a and powers of monomials c x^al (1 - x)^be, with exponents that do not depend on x: Beta
   functions, exact in d (the analytic continuation in the exponents); an exponent at a pole of Gamma (a + 1 or b + 1 a
   number 0, -1, -2, ...: a divergence that nothing regulates) is not done *)
gammaPoleQ[z_] := With[{v = Together[z]}, NumericQ[v] && TrueQ[Im[v] == 0] && TrueQ[Re[v] <= 0] && TrueQ[Re[v] == Round[Re[v]]]];
betaIntegral[t_, x_] := Catch[Module[{coefficient = 1, a = 0, b = 0},
    Do[Which[
      FreeQ[f, x], coefficient = coefficient f,
      f === x, a = a + 1,
      (* an exponent that depends on x (E^x, x^x, 2^x): not a Beta function *)
      Head[f] === Power && ! FreeQ[f[[2]], x], Throw[$Failed, beta],
      Head[f] === Power && f[[1]] === x, a = a + f[[2]],
      Head[f] === Power && f[[1]] === 1 - x, b = b + f[[2]],
      Head[f] === Power, With[{mf = monomialForm[f[[1]], x]}, If[mf === $Failed, Throw[$Failed, beta]];
        coefficient = coefficient coefficientPower[mf[[1]], f[[2]]]; a = a + mf[[2]] f[[2]]; b = b + mf[[3]] f[[2]]],
      True, Throw[$Failed, beta]], {f, factorList[t]}];
    If[gammaPoleQ[a + 1] || gammaPoleQ[b + 1], Throw[$Failed, beta]];
    coefficient Gamma[a + 1] Gamma[b + 1]/Gamma[a + b + 2]], beta];
closedParameterIntegral[f_, x_] := Module[{open = 0, total = 0},
   Do[With[{c = betaIntegral[t, x]}, If[c === $Failed, open = open + t, total = total + c]], {t, productTermsOf[Expand[f]]}];
   total + If[open === 0, 0, ParameterIntegral[open, x]]];
closedParameterIntegral[f_, {x_}] := closedParameterIntegral[f, x];
(* the integrals with the same variables and the same factor in front (structures and the symbols that are not in the
   integrand, such as the coupling) combined into one; the numbers go into the integrand *)
integralFactorQ[f_] := MatchQ[f, _ParameterIntegral | _SimplexIntegral];
CombineParameterIntegrals[e_] := Module[{plain = 0, pairs, keyed},
   pairs = Table[With[{ints = Select[factorList[t], integralFactorQ]},
      If[Length[ints] == 1, With[{rest = t/ints[[1]]}, With[{s = Times @@ Select[factorList[rest], structureFactorQ[#] ||
             (! NumberQ[#] && FreeQ[#, FeynmanParameter] && FreeQ[ints[[1]], #]) &]},
         {{s, Head[ints[[1]]], ints[[1, 2]]}, rest/s ints[[1, 1]]}]], {None, t}]], {t, productTermsOf[Expand[e]]}];
   keyed = GatherBy[Select[pairs, #[[1]] =!= None &], First];
   Total[Table[If[p[[1]] === None, p[[2]], 0], {p, pairs}]] +
    Total[Table[With[{key = g[[1, 1]], f = Expand[Total[Table[h[[2]], {h, g}]]]}, If[f === 0, 0, key[[1]] key[[2]][f, key[[3]]]]], {g, keyed}]]];
ReduceParameterIntegrals[e_] := CombineParameterIntegrals[e /. i : (SimplexIntegral[_, _List] | ParameterIntegral[_, _]) :> reducedIntegral[i]];
(* the reduction of an integral is a function of the integral alone (its variables are FeynmanParameter[i]): remembered,
   since every expansion of a result (pole, finite part, on shell, ...) reduces the same integrals again; the table is
   cleared beyond 5000 entries *)
reducedIntegral[i_] := With[{v = reductionTable[i]}, If[Head[v] =!= reductionTable, v,
    If[Length[DownValues[reductionTable]] > 5000, Clear[reductionTable]];
    reductionTable[i] = Replace[i, {SimplexIntegral[f_, xs_List] :> withVariableSymbols[reducedSimplexIntegral, f, xs],
       ParameterIntegral[f_, x_] :> withVariableSymbols[closedParameterIntegral, f, {x}]}]]];

(* ---- integrability at eps = 0 near the faces of the region, and the subtraction of a singular face ----
   The region is the simplex of n >= 2 variables vs (a ParameterIntegral in x is the simplex of {x, 1 - x}). Near the face
   where the variables of S vanish, x_i = lam t_i (i in S), x_j = (1 - lam) u_j (j not in S), Sum t = Sum u = 1, and
   dx = lam^(|S| - 1) (1 - lam)^(|C| - 1) dlam dt du. A polynomial base b is lam^k W(lam, t, u), W(0, t, u) not 0; a term
   c Prod b^e is lam^(p + q eps) G(lam, t, u) with p = Sum k e(0), q = Sum k e'(0): integrable at eps = 0 iff |S| + p > 0.
   A singular group of terms (same p, q, q not 0) is subtracted: with G_j the Taylor coefficients of G in lam (j <= K =
   Floor[-(|S| + p)]), Int lam^(p + q eps) Sum_j lam^j G_j = Sum_j B(|S| + p + j + q eps, |C|) Int dt du G_j (exact in d),
   and the remainder (the integrand minus lam^(p + q eps) Sum_j lam^j G_j(t(x), u(x)), lam = Sum_S x, t = x_S/lam,
   u = x_C/Sum_C x) has integrable coefficients. Supported: one singular face (both endpoints in one dimension), |S| = 1 or
   |C| = 1, G_j regular on the face (its bases do not vanish at the faces of the t or u simplex). ---- *)
ExpandParameterIntegrals::ir = "The integrand of `1` is not integrable at \[Epsilon] = 0 near the face where `2` vanish (an infrared divergence), and its divergence is not of the form that is subtracted (one singular face, every factor of the singular terms that depends on the parameters a power of a polynomial, the exponents of those that vanish there linear in \[Epsilon] and regulated by \[Epsilon]): it is not expanded.";
ExpandParameterIntegrals::failed = "The integrand `1` was not expanded in `2`.";
(* the factors of a term that depend on vs: {coefficient, {{base, exponent}, ...}, all factors powers of polynomials} *)
dependentFactors[t_, vs_List] := Module[{c = 1, bs = {}, ok = True, pat = Alternatives @@ vs},
   Do[Which[
      FreeQ[f, pat], c = c f,
      Head[f] === Power && PolynomialQ[f[[1]], vs] && FreeQ[f[[2]], pat], AppendTo[bs, {f[[1]], f[[2]]}],
      PolynomialQ[f, vs], AppendTo[bs, {f, 1}],
      True, ok = False; c = c f], {f, factorList[t]}];
   {c, bs, ok}];
(* the coordinates near the face S of the simplex of vs: {rules vs -> lam t, (1 - lam) u, independent t, independent u} *)
faceCoordinates[vs_List, s_List, lam_] := Module[{c = Complement[Range[Length[vs]], s], ts, us, tl, ul},
   ts = symbolsFor[Range[Length[s] - 1], "t"]; us = symbolsFor[Range[Length[c] - 1], "u"];
   tl = Append[ts, 1 - Total[ts]]; ul = Append[us, 1 - Total[us]];
   {Join[Table[vs[[s[[i]]]] -> lam tl[[i]], {i, Length[s]}], Table[vs[[c[[j]]]] -> (1 - lam) ul[[j]], {j, Length[c]}]], ts, us, c}];
(* the order k of a base at lam = 0 and W = b/lam^k; {Infinity, 0} if b vanishes identically there *)
baseOrder[b_, rules_List, lam_] := Module[{cs = CoefficientList[Expand[b /. rules], lam], k = 1},
   While[k <= Length[cs] && Expand[cs[[k]]] === 0, k++];
   If[k > Length[cs], {Infinity, 0}, {k - 1, Sum[cs[[j]] lam^(j - k), {j, k, Length[cs]}]}]];
exponentParts[e_, eps_] := With[{e0 = e /. eps -> 0, e1 = Coefficient[Expand[e], eps]},
   {e0, If[FreeQ[Expand[e - e0 - e1 eps], eps], e1, $Failed]}];
(* the analysis of the face S: {S, lam, coordinates, data of each term {p, q, known, ok, {{W, exponent}, ...}}}; known: the
   order is determined (numeric exponents, linear in eps, of the bases that vanish there); ok: every factor is a power of a
   polynomial *)
faceAnalysis[facs_List, vs_List, eps_, s_List] := Module[{lam = Unique["FunctionalDiagrams`Private`lambda"], coords, bases, table},
   coords = faceCoordinates[vs, s, lam];
   bases = Union[Flatten[Table[Table[b[[1]], {b, fc[[2]]}], {fc, facs}]]];
   table = Table[b -> baseOrder[b, coords[[1]], lam], {b, bases}];
   {s, lam, coords, Table[Module[{p = 0, q = 0, known = True, ws = {}},
      Do[With[{bo = Replace[b[[1]], table], ep = exponentParts[b[[2]], eps]},
        If[bo[[1]] === Infinity, known = False,
         If[bo[[1]] > 0, If[NumericQ[ep[[1]]] && ep[[2]] =!= $Failed && NumericQ[ep[[2]]], p = p + bo[[1]] ep[[1]]; q = q + bo[[1]] ep[[2]], known = False]]];
        AppendTo[ws, {bo[[2]], b[[2]]}]], {b, fc[[2]]}];
      {p, q, known, fc[[3]], ws}], {fc, facs}]}];
singularTermQ[o_List, size_] := ! o[[3]] || Re[o[[1]]] + size <= 0;
(* a polynomial b in the independent coordinates ind of a simplex (the last coordinate is 1 - Total[ind]) does not vanish
   identically at any face of it *)
nonVanishingOnFacesQ[b_, ind_List, lam_] := ind === {} || With[{full = Append[ind, Unique["FunctionalDiagrams`Private`last"]]},
   AllTrue[Subsets[Range[Length[full]], {1, Length[full] - 1}], Function[f,
     baseOrder[b, Select[faceCoordinates[full, f, lam][[1]], MemberQ[ind, #[[1]]] &], lam][[1]] === 0]]];
(* G is analytic at lam = 0 on the whole face: the bases of its non-polynomial factors do not vanish at the faces of the
   t and u simplices *)
faceRegularQ[ws_List, coords_List, lam_] := With[{bases = Union[Table[Expand[w[[1]] /. lam -> 0], {w, Select[ws, ! (IntegerQ[#[[2]]] && #[[2]] > 0) &]}]]},
   AllTrue[bases, nonVanishingOnFacesQ[#, coords[[2]], lam] && nonVanishingOnFacesQ[#, coords[[3]], lam] &]];
subtractFace[g_, facs_List, vs_List, eps_, analysis_List, make_] := Module[{s, lam, coords, orders, c, size, csize, sing, groups,
    closed = 0, subtraction = 0, lamx, back, ts, us},
   {s, lam, coords, orders} = analysis; c = coords[[4]]; size = Length[s]; csize = Length[c]; ts = coords[[2]]; us = coords[[3]];
   sing = Select[Range[Length[facs]], singularTermQ[orders[[#]], size] &];
   If[! AllTrue[sing, orders[[#, 3]] && orders[[#, 4]] &] || (size > 1 && csize > 1),
    irMessage[make[g], vs[[s]]]];
   groups = GatherBy[sing, orders[[#, {1, 2}]] &];
   (* lam, t and u as functions of the variables *)
   lamx = Total[vs[[s]]];
   back = Join[Table[ts[[i]] -> vs[[s[[i]]]]/lamx, {i, Length[ts]}], Table[us[[j]] -> vs[[c[[j]]]]/Total[vs[[c]]], {j, Length[us]}]];
   Do[Module[{p = orders[[gr[[1]], 1]], q = orders[[gr[[1]], 2]], bigK, bigG, gs},
      If[TrueQ[q == 0] || ! faceRegularQ[Flatten[Table[orders[[i, 5]], {i, gr}], 1], coords, lam],
       irMessage[make[g], vs[[s]]]];
      bigK = Floor[-(size + p)];
      bigG = Total[Table[facs[[i, 1]] Product[Power[w[[1]], w[[2]]], {w, orders[[i, 5]]}], {i, gr}]];
      gs = Table[(D[bigG, {lam, j}] /. lam -> 0)/j!, {j, 0, bigK}];
      closed = closed + Sum[Gamma[size + p + j + q eps] Gamma[csize]/Gamma[size + p + j + q eps + csize] faceIntegral[gs[[j + 1]], ts, us, vs, s, c], {j, 0, bigK}];
      subtraction = subtraction + lamx^(p + q eps) Sum[lamx^j (gs[[j + 1]] /. back), {j, 0, bigK}]], {gr, groups}];
   {closed, subtraction}];
(* Int over the t (or u) simplex of a coefficient G_j: a number (one point), a ParameterIntegral, or a SimplexIntegral, in
   the variables of the face *)
faceIntegral[gj_, ts_List, us_List, vs_List, s_List, c_List] := Which[
   ts === {} && us === {}, gj,
   us === {}, faceSimplexIntegral[gj, ts, vs[[s]]],
   True, faceSimplexIntegral[gj, us, vs[[c]]]];
faceSimplexIntegral[gj_, vars_List, names_List] := If[Length[names] == 2,
   closedParameterIntegral[expandedBases[gj /. vars[[1]] -> names[[1]]], names[[1]]],
   SimplexIntegral[expandedBases[gj /. Thread[vars -> Most[names]]], names]];
(* the integral h[g, xs] ready for the expansion under the integral sign *)
$variablesBack = {};
irMessage[i_, vars_] := (Message[ExpandParameterIntegrals::ir, i /. $variablesBack, vars /. $variablesBack]; Throw[$Failed, seriesFailure]);
preparedIntegral[g_, vs_List, eps_, make_] := Module[{terms, facs, analyses, singular, closed = 0, subtraction = 0},
   terms = productTermsOf[Expand[g]];
   facs = Table[dependentFactors[t, vs], {t, terms}];
   analyses = Table[faceAnalysis[facs, vs, eps, s], {s, Subsets[Range[Length[vs]], {1, Length[vs] - 1}]}];
   singular = Select[analyses, Function[a, AnyTrue[a[[4]], singularTermQ[#, Length[a[[1]]]] &]]];
   If[singular === {}, make[g],
    If[Length[vs] > 2 && Length[singular] > 1, irMessage[make[g], vs[[singular[[1, 1]]]]]];
    Do[With[{cs = subtractFace[g, facs, vs, eps, a, make]}, closed = closed + cs[[1]]; subtraction = subtraction + cs[[2]]], {a, singular}];
    closed + make[g - subtraction]]];
(* a ParameterIntegral in x is the simplex of {x, xc}, xc = 1 - x *)
prepareIntegral[ParameterIntegral[g_, x_], eps_] := With[{y = Unique["FunctionalDiagrams`Private`x"], yc = Unique["FunctionalDiagrams`Private`xc"]},
   Block[{$variablesBack = {y -> x, yc -> 1 - x}},
    preparedIntegral[g /. x -> y, {y, yc}, eps, ParameterIntegral[# /. yc -> 1 - y, y] &] /. $variablesBack]];
prepareIntegral[SimplexIntegral[g_, xs_List], eps_] := With[{ys = symbolsFor[xs, "x"]},
   Block[{$variablesBack = Thread[ys -> xs]}, preparedIntegral[g /. Thread[xs -> ys], ys, eps, SimplexIntegral[#, ys] &] /. $variablesBack]];

(* ---- the expansion in eps under the integral sign ---- *)
(* the polynomial terms of a coefficient integrated exactly, the others inert; the rational coefficients of each product of
   logarithms and non-integer powers are put over a common denominator, so that the singular terms that cancel after a
   subtraction (1/x - 1/(x (1 - x)) + ..., ln x/x - ln x/(x (1 - x))) are not left to N *)
transcendentalFactorQ[f_, xs_List] := ! FreeQ[f, Alternatives @@ xs] && (Head[f] === Log ||
     (Head[f] === Power && (Head[f[[1]]] === Log || ! IntegerQ[f[[2]]])));
combinedRationalCoefficients[e_, xs_List] := Module[{pairs},
   pairs = Table[With[{key = Times @@ Select[factorList[t], transcendentalFactorQ[#, xs] &]}, {key, t/key}], {t, productTermsOf[e]}];
   Total[Table[g[[1, 1]] Together[Total[Table[h[[2]], {h, g}]]], {g, GatherBy[pairs, First]}]]];
inertParameterIntegral[c_, {x_}] := Module[{poly = 0, rest = 0},
   Do[If[PolynomialQ[t, x], poly = poly + t, rest = rest + t], {t, productTermsOf[Expand[c]]}];
   rest = combinedRationalCoefficients[rest, {x}];
   polynomialIntegral[poly, x] + If[rest === 0, 0, ParameterIntegral[rest, x]]];
inertParameterIntegral[c_, xs_List] := Module[{poly = 0, rest = 0},
   Do[If[PolynomialQ[t, xs], poly = poly + t, rest = rest + t], {t, productTermsOf[Expand[c]]}];
   rest = combinedRationalCoefficients[rest, xs];
   simplexPolynomialIntegral[poly, xs] + If[rest === 0, 0, SimplexIntegral[rest, xs]]];
(* the series of an integral to eps^order: the integrand expanded (09's ser, from its true lowest order), the coefficients
   integrated; the variables are plain symbols during the expansion (its zero tests evaluate the coefficients numerically) *)
integralSeries[(h : ParameterIntegral | SimplexIntegral)[g_, x_], eps_, order_] := Module[{xs = If[ListQ[x], x, {x}], ys, s},
   ys = symbolsFor[xs, "x"];
   s = Catch[sumSeries[g /. Thread[xs -> ys], eps, order], expansionFailure];
   If[! ListQ[s], Message[ExpandParameterIntegrals::failed, g, eps]; Throw[$Failed, seriesFailure]];
   Sum[eps^(s[[1]] + j) (inertParameterIntegral[Expand[s[[2, j + 1]]], ys] /. Thread[ys -> xs]), {j, 0, Length[s[[2]]] - 1}]];
(* a lower bound of the valuation of a factor; an integral: that of its integrand *)
factorValuation[f_, eps_] := With[{v = Catch[If[integralFactorQ[f], valuation[f[[1]], eps], valuation[f, eps]], expansionFailure]},
   If[IntegerQ[v], v, Message[ExpandParameterIntegrals::failed, f, eps]; Throw[$Failed, seriesFailure]]];
(* the order to which each integral is expanded: n minus the valuation of what multiplies it, in every term *)
integralOrders[r_, ints_List, eps_, n_] := Module[{orders = Table[n, {Length[ints]}]},
   Do[With[{fs = expandIntegerPowers[factorList[t]]}, With[{vs = Table[factorValuation[f, eps], {f, fs}]},
      Do[If[integralFactorQ[fs[[i]]], With[{j = Position[ints, fs[[i]], {1}][[1, 1]]}, orders[[j]] = Max[orders[[j]], n - (Total[vs] - vs[[i]])]]],
       {i, Length[fs]}]]], {t, productTermsOf[Expand[r]]}];
   orders];
ExpandParameterIntegrals[e_List, eps_Symbol, n_Integer] := Table[ExpandParameterIntegrals[x, eps, n], {x, e}];
ExpandParameterIntegrals[e_, eps_Symbol, n_Integer] := withSeriesMemo[Catch[Module[{r, ints, orders},
    r = ReduceParameterIntegrals[e /. SpacetimeDimension -> 4 - 2 eps];
    r = r /. i : (_ParameterIntegral | _SimplexIntegral) :> prepareIntegral[i, eps];
    ints = Union[Cases[{r}, _ParameterIntegral | _SimplexIntegral, Infinity]];
    orders = integralOrders[r, ints, eps, n];
    EpsilonExpand[r /. Table[ints[[i]] -> integralSeries[ints[[i]], eps, orders[[i]]], {i, Length[ints]}], eps, n]], seriesFailure]];

(* ---- form factors of a vertex between on-shell spinors of equal mass ---- *)
VertexFormFactors::rest = "The vertex has a part `1` that is not of the form gamma^mu, p1^mu, p2^mu between the spinors: no form factors are given.";
VertexFormFactors[g_, mu_, {p1_, p2_, m_}] := Module[{f = DiracSimplify[g], cg, c1, a = 0, b1 = 0, b2 = 0, rest = 0, scalarQ},
   cg = SpinorChain[{p2, m}, DiracProduct[GammaMatrix[mu]], {p1, m}]; c1 = SpinorChain[{p2, m}, DiracProduct[], {p1, m}];
   scalarQ[x_] := diracFreeQ[x] && FreeQ[x, mu];
   If[f === $Failed, $Failed,
    (* each term: a scalar coefficient times one of the structures *)
    Do[With[{fs = factorList[t]}, Which[
        MemberQ[fs, cg] && scalarQ[t/cg], a = a + t/cg,
        MemberQ[fs, c1] && MemberQ[fs, FourVector[p1, mu]] && scalarQ[t/(c1 FourVector[p1, mu])], b1 = b1 + t/(c1 FourVector[p1, mu]),
        MemberQ[fs, c1] && MemberQ[fs, FourVector[p2, mu]] && scalarQ[t/(c1 FourVector[p2, mu])], b2 = b2 + t/(c1 FourVector[p2, mu]),
        True, rest = rest + t]], {t, productTermsOf[Expand[f]]}];
    If[rest =!= 0, Message[VertexFormFactors::rest, rest]; $Failed, {a + m (b1 + b2), -m (b1 + b2), (b2 - b1)/2}]]];

(* ---- display ---- *)
defineDisplayForm[GammaMatrix[mu_], Interpretation[Superscript["\[Gamma]", mu], GammaMatrix[mu]]];
defineDisplayForm[SlashedMomentum[p_], Interpretation[Overscript[p, "/"], SlashedMomentum[p]]];
defineDisplayForm[DiracProduct[], Interpretation["\[DoubleStruckOne]", DiracProduct[]]];
defineDisplayForm[DiracProduct[a__], Interpretation[Row[{a}, " "], DiracProduct[a]]];
defineDisplayForm[DiracTrace[a_], Interpretation[Row[{"tr[", a, "]"}], DiracTrace[a]]];
defineDisplayForm[SpinorChain[{p2_, m2_}, x_, {p1_, m1_}], Interpretation[Row[{OverBar["u"], "(", p2, ") ", x, " u(", p1, ")"}], SpinorChain[{p2, m2}, x, {p1, m1}]]];
defineDisplayForm[LorentzMetric[a_, b_], Interpretation[Superscript["g", Row[{a, b}]], LorentzMetric[a, b]]];
defineDisplayForm[FourVector[p_, a_], Interpretation[Superscript[p, a], FourVector[p, a]]];
defineDisplayForm[ScalarProduct[p_, q_], Interpretation[Row[{p, "\[CenterDot]", q}], ScalarProduct[p, q]]];
defineDisplayForm[SpacetimeDimension, Interpretation["d", SpacetimeDimension]];
defineDisplayForm[FeynmanDenominator[q_, m_], Interpretation[1/HoldForm[#1^2 - #2^2] &[q, m], FeynmanDenominator[q, m]]];
defineDisplayForm[MinkowskiLoopIntegral[f_, ks_List], Interpretation[Row[{Subscript["\[Integral]", Row[ks, ","]], " ", f}], MinkowskiLoopIntegral[f, ks]]];
defineDisplayForm[ParameterIntegral[f_, x_], Interpretation[Row[{Subsuperscript["\[Integral]", 0, 1], "d", x, " ", f}], ParameterIntegral[f, x]]];
defineDisplayForm[SimplexIntegral[f_, xs_List], Interpretation[Row[{Subscript["\[Integral]", Row[{"\[Delta](1-", Row[xs, "-"], ")"}]], " ", f}], SimplexIntegral[f, xs]]];
defineDisplayForm[FeynmanParameter[i_], Interpretation[Subscript["x", i], FeynmanParameter[i]]];

(* ========================================================================= *)
(*  20. Effective actions as objects                                         *)
(*                                                                           *)
(*  Operator traces: Tr ln A (TraceLog), Tr A (OperatorTrace) and kernels of *)
(*  operator expressions (OperatorKernel); their functional derivatives and  *)
(*  their expansion into diagrams (ExpandTrace); functional derivatives with *)
(*  respect to two-index objects. The 2PI effective action Gamma[G] and its  *)
(*  field dependence Gamma[phibar, G] (EffectiveAction2PI), the 1PI          *)
(*  effective action Gamma[phibar] (EffectiveAction1PI, phibar = <phi>),     *)
(*  Potthoff's universal functional                                          *)
(*  F[Sigma] (LegendreTransform, LegendrePropagator) and self-energy         *)
(*  functional (SelfEnergyFunctional), and the value of diagrams in zero     *)
(*  dimensions (ZeroDimensionalValue). The sums of logarithms, the traces in *)
(*  momentum and frequency and the limits of a response are in section 17,   *)
(*  the exact equations of the response in section 12a.                      *)
(* ========================================================================= *)


(* ---- operator expressions ---- *)
(* An operator is a kernel (a symbol h standing for the function h[x, y] of two labels, or a LabelFunction), a number
   (times the identity), a sum, a number times an operator, a product A . B (Dot) or an inverse A^-1. The kernel of
   A . B is Integral A(x, u) B(u, y) and Tr(A . B) = Integral A(x, y) B(y, x): with this trace, Tr(Sigma . G) is the
   Integral Sigma(x, y) G(y, x) of SelfEnergy (Sigma(x, y) = dPhi/dG(y, x) for fermions). *)

FunctionalDiagrams::optimes = "`1` multiplies two operators with Times: write a product of operators with Dot (A . B).";
FunctionalDiagrams::opscalar = "In `1` the operator cannot be told from its coefficients: declare its kernel (DeclareDirectedPropagator, DeclareSymmetric, ...). A single symbol in an operator is a kernel: a chemical potential is LabelFunction[mu DeltaFunction[a, b], {a, b}].";
FunctionalDiagrams::opvariation = "The variation of `1` with respect to `2` is not known (sums, coefficients times operators, products, integer powers and inverses are): it is taken as 0.";
FunctionalDiagrams::opfunction = "The kernel `1` depends on `2` through a function of labels (LabelFunction): its derivative inside a trace is not taken.";
FunctionalDiagrams::opzero = "The inverse of the operator `1`, which is zero, is not defined.";
FunctionalDiagrams::opdotpower = "`1` is a power whose exponent is a product: Mathematica reads A^-1 . B as A^(-1 . B). It is taken as `2`; write A^(-1) . B.";
FunctionalDiagrams::opsource = "The operator `1` depends on `2` in a way the derivative cannot follow, and the derivative of `3` is taken as 0: a source enters an operator through a kernel of two labels, a LabelFunction (LabelFunction[lambda/2 phibar[u]^2 DeltaFunction[u, v], {u, v}]) or a head whose derivative is declared (DeclareFunctionalDerivative).";

(* the input of an operator: Inverse[A] is A^-1, and A^(-(1 . B)), which is how Mathematica reads A^-1 . B, is A^-1 . B
   (with a message; likewise A^(n . B)) *)
dotPowerFixed[p_, q_] := (Message[FunctionalDiagrams::opdotpower, p, q]; q);
operatorInput[a_] := a /. {Inverse[b_] :> Power[b, -1],
    p : Power[b_, Times[-1, Dot[n_?NumericQ, c__]]] :> dotPowerFixed[p, Dot[Power[b, -n], c]],
    p : Power[b_, Dot[n_?NumericQ, c__]] :> dotPowerFixed[p, Dot[Power[b, n], c]]};
operatorInputQ[a_] := ! FreeQ[a, _Inverse | Power[_, Dot[_?NumericQ, __] | Times[-1, Dot[_?NumericQ, __]]]];
TraceLog[a_] /; operatorInputQ[a] := TraceLog[operatorInput[a]];
OperatorTrace[a_] /; operatorInputQ[a] := OperatorTrace[operatorInput[a]];
OperatorKernel[a_] /; operatorInputQ[a] := OperatorKernel[operatorInput[a]];

orderlessQ[h_Symbol] := MemberQ[Attributes[h], Orderless];
orderlessQ[_] := False;

dotOf[l_List] := With[{m = DeleteCases[l, 1]}, Which[m === {}, 1, Length[m] == 1, First[m], True, Dot @@ m]];
atomInverse[Power[a_, -1]] := a;
atomInverse[a_] := Power[a, -1];
(* the heads known to be kernels: declared lines, symmetric and antisymmetric heads, sources (the variable of a
   derivative with respect to a kernel is one, through the copy of derivativeWithRespectToPropagator) and heads with a
   declared derivative *)
kernelHeadQ[h_Symbol] := MemberQ[Cases[$lineStyles, {k_, _} :> k], h] || orderlessQ[h] || MemberQ[$antisymmetricHeads, h] ||
   MemberQ[sourceHeads[], h] || MemberQ[Cases[$derivativePairs, {k_, _} :> k], h];
kernelHeadQ[_] := False;
(* a factor of a product that is certainly an operator: it contains a known kernel, a product of operators, a kernel of an
   operator or a function of labels *)
operatorFactorQ[f_] := ! FreeQ[f, _Dot | _Function | _OperatorKernel] || ! FreeQ[f, h_Symbol /; kernelHeadQ[h]];
(* a coefficient and the one operator factor of a product: numbers, and symbols or expressions free of known kernels
   beside a factor that is certainly an operator, are coefficients (lambda G0, -mu S with S declared). A single
   non-numeric factor is the operator (-U, 2 S: an undeclared kernel). Otherwise a message, and $Failed thrown to the
   entry point (ExpandTrace, the derivatives of traces and kernels) *)
splitScalar[a_Times] := Module[{num = Select[List @@ a, NumericQ], rest = Select[List @@ a, ! NumericQ[#] &], ops},
   ops = Select[rest, operatorFactorQ];
   Which[
    Length[rest] == 1, {Times @@ num, First[rest]},
    Length[ops] == 1, {Times @@ Join[num, Select[rest, ! operatorFactorQ[#] &]], First[ops]},
    Length[ops] == 0, Message[FunctionalDiagrams::opscalar, userForm[a]]; Throw[$Failed, operatorFailure],
    True, Message[FunctionalDiagrams::optimes, userForm[a]]; Throw[$Failed, operatorFailure]]];

(* the monomials {c, {a1, ..., ak}} of an operator, c a1 . a2 ... ak, with atoms: kernel symbols, functions of labels,
   and inverses Power[b, -1] of a kernel or of a sum (the sum in normal form); adjacent inverse pairs cancel *)
opMonomials[a_?NumericQ] := {{a, {}}};
opMonomials[a_Symbol] := {{1, {a}}};
opMonomials[f_Function] := {{1, {f}}};
opMonomials[a_Plus] := Join @@ (opMonomials /@ List @@ a);
opMonomials[a_Times] := With[{s = splitScalar[a]}, ({s[[1]] #[[1]], #[[2]]} &) /@ opMonomials[s[[2]]]];
opMonomials[a_Dot] := Fold[productOfMonomials, {{1, {}}}, opMonomials /@ cancelAdjacentInverses[List @@ a]];
opMonomials[Power[b_, -1]] := inverseMonomials[b];
opMonomials[Power[b_, n_Integer?Positive]] := opMonomials[Dot @@ Table[b, {n}]];
opMonomials[Power[b_, n_Integer?Negative]] := opMonomials[Dot @@ Table[Power[b, -1], {-n}]];
opMonomials[a_] := {{1, {a}}};

inverseMonomials[b_?NumericQ] := {{1/b, {}}};
inverseMonomials[b_Symbol] := {{1, {Power[b, -1]}}};
inverseMonomials[f_Function] := {{1, {Power[f, -1]}}};
inverseMonomials[Power[b_, -1]] := opMonomials[b];
inverseMonomials[b_Times] := With[{s = splitScalar[b]}, ({#[[1]]/s[[1]], #[[2]]} &) /@ inverseMonomials[s[[2]]]];
inverseMonomials[b_Dot] := opMonomials[Dot @@ Reverse[Power[#, -1] & /@ (List @@ b)]];
inverseMonomials[b_] := With[{m = collectMonomials[opMonomials[b]]}, Which[
    m === {}, Message[FunctionalDiagrams::opzero, b]; {},
    Length[m] == 1, {{1/m[[1, 1]], Reverse[atomInverse /@ m[[1, 2]]]}},
    (* the inverse of a sum, normalised with the coefficient of its first monomial 1: (G0^-1 - S)^-1 and -(S - G0^-1)^-1
       are the same atom *)
    True, With[{c = m[[1, 1]]}, {{1/c, {Power[opFromMonomials[({#[[1]]/c, #[[2]]} &) /@ m], -1]}}}]]];

productOfMonomials[l1_List, l2_List] := Flatten[Table[{a[[1]] b[[1]], cancelAtoms[Join[a[[2]], b[[2]]]]}, {a, l1}, {b, l2}], 1];
(* adjacent atoms x, x^-1 cancel *)
cancelAtoms[l_List] := Module[{m = l, k},
   While[(k = Catch[Do[If[m[[i + 1]] === atomInverse[m[[i]]], Throw[i]], {i, Length[m] - 1}]; None]) =!= None, m = Drop[m, {k, k + 1}]];
   m];
(* adjacent factors of a product that are inverse to each other, e.g. (G0^-1 - S) . (G0^-1 - S)^-1, cancel before the
   product is expanded *)
operatorNormal[a_] := opFromMonomials[opMonomials[a]];
inverseOperatorQ[p_, q_] := With[{iq = opMonomials[q], ip = inverseMonomials[p]},
   collectMonomials[iq] === collectMonomials[ip]];
cancelAdjacentInverses[l_List] := Module[{m = l, k},
   While[(k = Catch[Do[If[! NumericQ[m[[i]]] && inverseOperatorQ[m[[i]], m[[i + 1]]], Throw[i]], {i, Length[m] - 1}]; None]) =!= None,
    m = Drop[m, {k, k + 1}]];
   If[m === {}, {1}, m]];

(* equal monomials combined (gathered once), zeros dropped, in the canonical order of the atom lists *)
collectMonomials[m_List] := With[{g = GatherBy[m, Last]},
   Select[({Together[Total[#[[All, 1]]]], #[[1, 2]]} &) /@ g[[Ordering[g[[All, 1, 2]]]]], #[[1]] =!= 0 &]];
opFromMonomials[m_List] := Total[(#[[1]] dotOf[#[[2]]] &) /@ collectMonomials[m]];

(* ---- kernels and traces of monomials ---- *)
(* a kernel symbol h gives h[x, y]; a function of labels is applied; an inverse is the kernel OperatorKernel[h^-1][x, y],
   symmetric (its labels in canonical order) when h is *)
OperatorKernel[p : Power[h_Symbol, -1]][x_, y_] /; orderlessQ[h] && ! OrderedQ[{x, y}] := OperatorKernel[p][y, x];
(* the inverse of an antisymmetric kernel (a superfield propagator) is antisymmetric *)
OperatorKernel[Power[h_Symbol, -1]][x_, x_] /; MemberQ[$antisymmetricHeads, h] := 0;
OperatorKernel[p : Power[h_Symbol, -1]][x_, y_] /; MemberQ[$antisymmetricHeads, h] && ! OrderedQ[{x, y}] := -OperatorKernel[p][y, x];
atomKernel[a : Power[_, -1], x_, y_] := OperatorKernel[a][x, y];
atomKernel[a_, x_, y_] := a[x, y];
monomialKernelNow[{c_, {}}, x_, y_] := c DeltaFunction[x, y];
monomialKernelNow[{c_, atoms_List}, x_, y_] := Module[{k = Length[atoms], u = freshSymbols[Length[atoms] - 1], points},
   points = Join[{x}, u, {y}];
   c If[k == 1, atomKernel[First[atoms], x, y], IntegralOver[Times @@ Table[atomKernel[atoms[[i]], points[[i]], points[[i + 1]]], {i, k}], u]]];
(* the monomials of an operator (memoised during one ExpandTrace: the same operator stands for every line of Phi[G]) *)
monomialsOf[a_] := collectMonomials[opMonomials[a]];
kernelOf[a_, x_, y_] := Total[monomialKernel[#, x, y] & /@ monomialsOf[a]];
(* the trace is cyclic: x . ... . x^-1 cancels too *)
cyclicCancel[l_List] := Module[{m = cancelAtoms[l]},
   While[Length[m] >= 2 && Last[m] === atomInverse[First[m]], m = Take[m, {2, -2}]]; m];
monomialTraceNow[{c_, atoms0_List}] := Module[{atoms = cyclicCancel[atoms0], k, u},
   k = Length[atoms];
   If[k == 0, c OperatorTrace[1],
    u = freshSymbols[k];
    c IntegralOver[Times @@ Table[atomKernel[atoms[[i]], u[[i]], u[[If[i == k, 1, i + 1]]]], {i, k}], u]]];
traceOf[a_] := Total[monomialTrace /@ monomialsOf[a]];
(* With "MaximumOrder" -> {x, n} in ExpandTrace ($operatorOrder = {x, headQ}), a monomial of a kernel or a trace is the
   inert orderBounded[m, k, labels] with m a lower bound of the orders of its terms and k the number of the monomial in
   lazyMonomial (blocked in ExpandTrace), written out only when the budget of truncatedRawTerms leaves room for m: the
   long products of series (Phi[G] at G = (G0^-1 - S)^-1) are not written out for the orders that are cut. A function of
   labels in an operator (a LabelFunction, e.g. a self-energy series) is split into its parts of each lowest order
   (gradedFunctions), so that their products are bounded too. *)
$operatorOrder = None;
lazyMonomialNumber[data_List] := ($lazyCount++; lazyMonomial[$lazyCount] = data; $lazyCount);
(* the labels of a kernel stay in the inert expression, orderBounded[m, k, x, y], so that the renaming of integration
   variables (scopedReplace) reaches them *)
monomialKernel[m_, x_, y_] := If[$operatorOrder === None, monomialKernelNow[m, x, y],
   orderBounded[monomialLowerOrder[m], lazyMonomialNumber[{monomialKernelNow, m}], x, y]];
monomialTrace[m_] := If[$operatorOrder === None, monomialTraceNow[m], orderBounded[monomialLowerOrder[m], lazyMonomialNumber[{monomialTraceNow, m}]]];
writtenOut[orderBounded[_, k_, labels___]] := With[{data = lazyMonomial[k]}, data[[1]][data[[2]], labels]];
(* whether the terms of e, with its monomials written out, contain factors x[..] (the headQ of truncatedRawTerms): x is a
   kernel of a monomial, or a function of labels or an inverse in a monomial contains x[..] *)
lazyHeadQ[e_, x_] := ! FreeQ[e, x[___]] || With[{atoms = Union @@ Table[lazyMonomial[k][[2, 2]], {k, $lazyCount}]},
    MemberQ[atoms, x] || ! FreeQ[atoms, x[___]] || ! FreeQ[Table[lazyMonomial[k][[2, 1]], {k, $lazyCount}], x[___]]];
monomialLowerOrder[{c_, atoms_List}] := coefficientLowerOrder[c] + Total[atomLowerOrder /@ atoms];
coefficientLowerOrder[c_] := With[{x = $operatorOrder[[1]]},
   If[$operatorOrder[[2]] || FreeQ[c, x], 0, With[{k = Exponent[c, x]}, If[FreeQ[c/x^k, x], k, 0]]]];
atomLowerOrder[h_Symbol] := If[$operatorOrder[[2]] && h === $operatorOrder[[1]], 1, 0];
atomLowerOrder[f_Function] := functionLowerOrder[f];
atomLowerOrder[_] := 0;
(* the lowest order of the terms of a function of labels (computed once per ExpandTrace: functionLowerOrder is blocked) *)
functionLowerOrderOf[f_Function] := With[{t = toTermList[f @@ freshSymbols[Length[First[f]]]]},
   If[t === {}, 0, Min[termOrder[#, $operatorOrder[[1]], $operatorOrder[[2]]] & /@ t]]];
gradedFunctions[f_Function] := If[Length[f] != 2 || Head[f[[2]]] =!= Plus, f,
   With[{groups = GatherBy[List @@ f[[2]], functionLowerOrder[Function @@ {f[[1]], #}] &]},
    If[Length[groups] < 2, f, Total[(Function @@ {f[[1]], Plus @@ #} &) /@ groups]]]];

(* ---- contraction of inverse kernels: Integral A^-1(a, u) A(u, b) du = delta(a, b) ---- *)
(* the other label of a two-label factor f that contains u at position p (any position for a symmetric kernel) *)
otherLabel[f_, u_, p_, symmetric_] := Which[
   f[[p]] === u, f[[3 - p]],
   symmetric && f[[3 - p]] === u, f[[p]],
   True, None];
(* the other label of an antisymmetric kernel f that contains u, and the sign that brings u to position q *)
antisymmetricOtherLabel[f_, u_, q_] := Which[f[[q]] === u, {f[[3 - q]], 1}, f[[3 - q]] === u, {f[[q]], -1}, True, {None, 0}];
findInversePair[factors_List, d_List] := Catch[
   Do[With[{f = factors[[i]]}, If[MatchQ[f, OperatorKernel[Power[_, -1]][_, _]],
      With[{h = f[[0, 1, 1]], symmetric = orderlessQ[f[[0, 1, 1]]], antisymmetric = MemberQ[$antisymmetricHeads, f[[0, 1, 1]]]},
       Do[With[{u = f[[p]]}, If[MemberQ[d, u] && Count[factors, _?(! FreeQ[#, u] &)] == 2,
           Do[If[j != i && ! AtomQ[factors[[j]]] && Head[factors[[j]]] === h && Length[factors[[j]]] == 2,
             (* h^-1(a, u) h(u, b) or h(a, u) h^-1(u, b); for an antisymmetric h also with u in the other slot of h,
                h(b, u) = -h(u, b) *)
             With[{os = If[antisymmetric, antisymmetricOtherLabel[factors[[j]], u, If[p == 2, 1, 2]],
                 {otherLabel[factors[[j]], u, If[p == 2, 1, 2], symmetric], 1}], own = f[[3 - p]]},
              With[{other = os[[1]]},
               If[other =!= None && u =!= other && u =!= own,
                Throw[{i, j, u, os[[2]] If[p == 2, DeltaFunction[own, other], DeltaFunction[other, own]]}]]]]],
            {j, Length[factors]}]]],
        {p, {1, 2}}]]]],
    {i, Length[factors]}];
   None];
contractInverseTerm[diagramTerm[c_, o_, d_]] := Module[{factors = expandIntegerPowers[factorList[c]], dd = d, pair},
   While[(pair = findInversePair[factors, dd]) =!= None,
    factors = Append[Delete[factors, {{pair[[1]]}, {pair[[2]]}}], pair[[4]]]; dd = DeleteCases[dd, pair[[3]]]];
   integrateDeltaFunctions[diagramTerm[Times @@ factors, o, dd]]];
(* Integral delta(u, u) du, with u in no other factor, is the trace of the identity, OperatorTrace[1] *)
identityTraceTerm[t : diagramTerm[c_, o_, d_]] := Module[{factors = expandIntegerPowers[factorList[c]], k},
   k = Catch[Do[If[MatchQ[factors[[i]], DeltaFunction[u_, u_] /; MemberQ[d, u]] &&
        Count[factors, _?(! FreeQ[#, factors[[i, 1]]] &)] == 1 && FreeQ[o, factors[[i, 1]]], Throw[i]], {i, Length[factors]}]; None];
   If[k === None, t, identityTraceTerm[diagramTerm[(Times @@ Delete[factors, k]) OperatorTrace[1], o, DeleteCases[d, factors[[k, 1]]]]]]];

(* ---- expansion around G0 ---- *)
(* the operator a = c (G0^-1 - X), as {c, G0 . X}; None if a has no single term c G0^-1 *)
splitAroundBare[a_, g0_] := Module[{m = collectMonomials[opMonomials[a]], free, c},
   free = Select[m, #[[2]] === {Power[g0, -1]} &];
   If[Length[free] != 1 || Length[m] == 1, None,
    c = free[[1, 1]];
    {c, operatorNormal[Dot[g0, opFromMonomials[({-#[[1]]/c, #[[2]]} &) /@ DeleteCases[m, free[[1]]]]]]}]];
(* Tr ln(c (G0^-1 - X)) = Tr ln(c G0^-1) - Sum_k Tr((G0 . X)^k)/k *)
traceLogExpansion[a_, None, _] := TraceLog[a];
traceLogExpansion[a_, g0_, n_Integer] := With[{s = splitAroundBare[a, g0]},
   If[s === None, TraceLog[a],
    TraceLog[s[[1]] Power[g0, -1]] - Sum[traceOf[If[k == 1, s[[2]], Dot @@ Table[s[[2]], {k}]]]/k, {k, n}]]];
(* (c (G0^-1 - X))^-1 = (1/c) Sum_k (G0 . X)^k . G0, the Dyson (Neumann) series, to n insertions of X *)
neumannInverse[b_, g0_, n_Integer] := With[{s = splitAroundBare[b, g0]},
   If[s === None, Power[b, -1], 1/s[[1]] Sum[Dot @@ Append[Table[s[[2]], {k}], g0], {k, 0, n}]]];
expandInverses[a_, None, _] := a;
expandInverses[a_, g0_, n_] := a /. Power[b_, -1] /; ! AtomQ[b] && Head[b] =!= Function :> neumannInverse[b, g0, n];

(* the raw terms of e, with the products truncated at order nmax in x as they are formed (orders add up and are not
   negative), so that long products of series are not expanded beyond the order wanted. Factors without Grassmann-odd
   parts commute: the shortest lists (single factors, e.g. the interaction lines) are multiplied first, so that the
   order they carry cuts the products of the long ones *)
(* Without Grassmann-odd factors (vacuum diagrams, two-point functions at zero source) the terms are generated with a
   budget: in a product, the single factors first, then every composite factor (a series) expanded only up to the order
   that the product built so far leaves; so that, e.g., the lines of a third-order diagram of Phi[G] with G a Dyson series
   give only their bare term. *)
truncatedRawTerms[e_, {x_, nmax_Integer}] /; FreeQ[e, _NonCommutativeMultiply | _GrassmannProduct] && FreeQ[e, _?grassmannOddAtomQ] :=
  With[{headQ = ! FreeQ[e, x[___]]}, integrateDeltaFunctions /@ budgetTerms[e, nmax, x, headQ]];
budgetOrder[t_, x_, headQ_] := termOrder[t, x, headQ];
budgetTerms[e_, b_, x_, headQ_] := If[b < 0, {}, budgetTermsOf[e, b, x, headQ]];
budgetTermsOf[0, _, _, _] := {};
budgetTermsOf[e_Plus, b_, x_, headQ_] := Join @@ (budgetTerms[#, b, x, headQ] & /@ List @@ e);
budgetTermsOf[e_Times, b_, x_, headQ_] := budgetProduct[List @@ e, b, x, headQ];
budgetTermsOf[Power[base_, n_Integer?Positive], b_, x_, headQ_] /; MemberQ[{Plus, Times, IntegralOver, orderBounded}, Head[base]] :=
  budgetProduct[Table[base, {n}], b, x, headQ];
budgetTermsOf[o : orderBounded[m_, __], b_, x_, headQ_] := If[m > b, {}, budgetTerms[writtenOut[o], b, x, headQ]];
budgetTermsOf[IntegralOver[f_, v_List], b_, x_, headQ_] := Module[{new = freshSymbols[Length[v]]},
   (diagramTerm[#[[1]], #[[2]], Join[new, #[[3]]]] &) /@ budgetTerms[scopedReplace[f, Thread[v -> new]], b, x, headQ]];
budgetTermsOf[e_, b_, x_, headQ_] := Select[toTermList[e /. o_orderBounded :> writtenOut[o]], budgetOrder[#, x, headQ] <= b &];
budgetProduct[factors_List, b_, x_, headQ_] := Module[{plain = Select[factors, plainFactorQ[#] && Head[#] =!= orderBounded &],
    composite = Select[factors, ! plainFactorQ[#] || Head[#] === orderBounded &], acc},
   acc = Select[{diagramTerm[Times @@ plain, {}, {}]}, budgetOrder[#, x, headQ] <= b &];
   Do[If[acc =!= {},
     acc = Select[multiplyTermLists[acc, budgetTerms[f, b - Min[budgetOrder[#, x, headQ] & /@ acc], x, headQ]], budgetOrder[#, x, headQ] <= b &]],
    {f, SortBy[composite, LeafCount]}];
   acc];
truncatedRawTerms[e_, {x_, nmax_Integer}] := With[{headQ = ! FreeQ[e, x[___]]},
   With[{keep = (termOrder[#, x, headQ] <= nmax &)},
    Block[{multiplyTermLists, multiplyTermListsInOrder},
     multiplyTermLists[l1_List, l2_List] := Select[Flatten[Table[multiplyTwoTerms[a, b], {a, l1}, {b, l2}], 1], keep];
     multiplyTermListsInOrder[lists_List] := Fold[multiplyTermLists, unitTermList,
       If[AllTrue[lists, Function[l, AllTrue[l, #[[2]] === {} &]]], SortBy[lists, Length], lists]];
     integrateDeltaFunctions /@ Select[toTermList[e], keep]]]];

(* Potthoff's F[S] written out: Phi[G] + kappa Tr(S . G), with G standing for G[S]; G[S] (LegendrePropagator) as G *)
legendreWrittenOut[e_] := e /. {LegendreTransform[phi_, g_ -> s_, opts___Rule] :>
     phi + statisticsFactors[{opts}][[2]] OperatorTrace[Dot[s, g]],
    LegendrePropagator[g_ -> _, ___][x_, y_] :> g[x, y]};

Options[ExpandTrace] = {"MaximumOrder" -> None};
ExpandTrace[e_, opts___Rule] := Catch[expandTraces[e, None, 0, {opts}], operatorFailure];
ExpandTrace[e_, g0_Symbol, n_Integer, opts___Rule] := Catch[expandTraces[e, g0, n, {opts}], operatorFailure];
(* the monomials are written out lazily (orderBounded) only for terms without Grassmann-odd factors (those of the budget of
   truncatedRawTerms) *)
gradedSymbolQ[s_Symbol] := MemberQ[$grassmannSources, s] || MemberQ[$grassmannOddHeads, s] || MemberQ[$gradedHeads, s];
gradedSymbolQ[_] := False;
expandTraces[e_, g0_, n_, opts_List] := Module[{maximum = optionValue[opts, Options[ExpandTrace], "MaximumOrder"], r, terms, lazy},
   lazy = MatchQ[maximum, {_, _Integer}] && FreeQ[e, _NonCommutativeMultiply | _GrassmannProduct] && FreeQ[e, _?gradedSymbolQ];
   Block[{$operatorOrder = If[lazy, {maximum[[1]], ! FreeQ[e, maximum[[1]][___]]}, None], functionLowerOrder, graded, lazyMonomial, $lazyCount = 0,
     monomialsOf, expanded},
    functionLowerOrder[f_] := functionLowerOrder[f] = functionLowerOrderOf[f];
    monomialsOf[a_] := monomialsOf[a] = collectMonomials[opMonomials[a]];
    expanded[a_] := expanded[a] = expandInverses[graded[a], g0, n];
    (* the same operator (a propagator replaced by an operator) is expanded once *)
    graded[a_] := graded[a] = If[$operatorOrder === None, a, a /. f_Function :> gradedFunctions[f]];
    r = legendreWrittenOut[e] /. {
      TraceLog[a_] :> traceLogExpansion[expandInverses[operatorNormal[graded[a]], g0, n], g0, n],
      OperatorTrace[a_] :> traceOf[expanded[a]],
      OperatorKernel[a_][x_, y_] :> kernelOf[expanded[a], x, y],
      (* a propagator replaced by an operator, e.g. G -> (G0^-1 - S)^-1 in Phi[G] *)
      (a : (_Power | _Dot | _Plus | _Times))[x_, y_] :> kernelOf[expanded[a], x, y]};
    terms = Which[maximum === None, rawTermsOf[r],
      (* the bounds were computed with the headQ of e: the budget runs if it is that of the terms, else everything is
         written out *)
      lazy && lazyHeadQ[r, maximum[[1]]] === $operatorOrder[[2]],
      integrateDeltaFunctions /@ budgetTerms[r, maximum[[2]], maximum[[1]], $operatorOrder[[2]]],
      True, truncatedRawTerms[r /. o_orderBounded :> writtenOut[o], maximum]]];
   terms = identityTraceTerm /@ (contractInverseTerm /@ terms);
   termListToExpression[canonicalTermList[terms]] /. OperatorTrace[1] DeltaFunction[x_, x_] :> OperatorTrace[1]];

(* ---- functional derivatives of traces and kernels with respect to a kernel s (a source s[x1, x2], or a propagator
   through the copy that SelfEnergy differentiates) ---- *)
(* the variation of an operator: a list of {c, L, R} with delta A = Sum c L . delta s . R *)
opVariation[a_, s_] /; FreeQ[a, s] := {};
opVariation[a_, s_] /; a === s := {{1, 1, 1}};
opVariation[a_Plus, s_] := Join @@ (opVariation[#, s] & /@ List @@ a);
opVariation[a_Times, s_] := With[{sc = splitScalar[a]}, ({sc[[1]] #[[1]], #[[2]], #[[3]]} &) /@ opVariation[sc[[2]], s]];
opVariation[a_Dot, s_] := With[{args = List @@ a}, Join @@ Table[
    ({#[[1]], dotOf[Append[Take[args, i - 1], #[[2]]]], dotOf[Prepend[Drop[args, i], #[[3]]]]} &) /@ opVariation[args[[i]], s],
    {i, Length[args]}]];
opVariation[Power[b_, -1], s_] := ({-#[[1]], dotOf[{Power[b, -1], #[[2]]}], dotOf[{#[[3]], Power[b, -1]}]} &) /@ opVariation[b, s];
opVariation[Power[b_, n_Integer?Positive], s_] := opVariation[Dot @@ Table[b, {n}], s];
opVariation[Power[b_, n_Integer], s_] /; n < -1 := opVariation[Dot @@ Table[Power[b, -1], {-n}], s];
opVariation[f_Function, s_] := (Message[FunctionalDiagrams::opfunction, userForm[f], userForm[s]]; {});
(* reached only when a depends on s (non-integer powers, unknown functions of operators) *)
opVariation[a_, s_] := (Message[FunctionalDiagrams::opvariation, userForm[a], userForm[s]]; {});

(* delta Tr(A) = Sum c Tr(R . L . delta s), delta Tr ln A = Sum c Tr(R . A^-1 . L . delta s); the derivative with
   respect to s(x1, x2) is the kernel at (x2, x1) *)
traceDerivative[OperatorTrace[a_], s_Symbol[x1_, x2_]] := kernelOf[Total[(#[[1]] dotOf[{#[[3]], #[[2]]}] &) /@ opVariation[a, s]], x2, x1];
traceDerivative[TraceLog[a_], s_Symbol[x1_, x2_]] := kernelOf[Total[(#[[1]] dotOf[{#[[3]], Power[a, -1], #[[2]]}] &) /@ opVariation[a, s]], x2, x1];
(* delta A(x, y) = Sum c (L . delta s . R)(x, y): the derivative with respect to s(x1, x2) is Sum c L(x, x1) R(x2, y) *)
kernelDerivative[OperatorKernel[a_][x_, y_], s_Symbol[x1_, x2_]] := Total[(#[[1]] kernelOf[#[[2]], x, x1] kernelOf[#[[3]], x2, y] &) /@ opVariation[a, s]];
(* F[S] = Phi[G] + kappa Tr(S G) at G = G[S], where F is stationary in G: dF/dS(x1, x2) = kappa G[S](x2, x1), with G[S]
   the inert LegendrePropagator[G -> S, {V, ...}] (V, ... the other kernels of Phi, on which G[S] depends too); with
   respect to any other kernel or source v of Phi (an interaction), dF/dv = dPhi/dv at G = G[S] (Hellmann-Feynman) *)
legendreKernels[phi_, g_] := Select[Union[Cases[phi, h_Symbol[a_, b_] /; ! ListQ[a] && ! ListQ[b] && ! NumericQ[a] :> h, {0, Infinity}]],
   # =!= g && Context[#] =!= "System`" && ! MemberQ[{IntegralOver, GrassmannProduct, DummyVariable, OperatorTrace, TraceLog}, #] &];
legendrePropagatorOf[phi_, g_ -> s_] := LegendrePropagator[g -> s, legendreKernels[phi, g]];
legendreDerivative[LegendreTransform[phi_, g_ -> s_, opts___Rule], s_[x1_, x2_]] := statisticsFactors[{opts}][[2]] legendrePropagatorOf[phi, g -> s][x2, x1];
legendreDerivative[LegendreTransform[phi_, g_ -> s_, ___Rule], source_] /; Head[source] =!= s && Head[source] =!= g :=
  FunctionalDerivative[phi, source] /. g[x_, y_] :> legendrePropagatorOf[phi, g -> s][x, y];
traceDerivative[_, _] := 0;
kernelDerivative[_, _] := 0;
legendreDerivative[_, _] := 0;

(* the traces and kernels depend on a source explicitly or through a kernel with a declared derivative: dependsOnSourceQ
   of TraceLog, OperatorTrace and OperatorKernel with the variations below. The transform depends on S and on what Phi
   depends on besides G (G is the variable of the transform) *)
dependsOnSourceQ[f_LegendreTransform, source_] := Head[source] =!= f[[2, 1]] && (! FreeQ[f[[1]], Head[source]] || ! FreeQ[f[[2, 2]], Head[source]]);
differentiateFactor[f_TraceLog, source_] := toTermList[Catch[traceDerivative[f, source], operatorFailure]];
differentiateFactor[f_OperatorTrace, source_] := toTermList[Catch[traceDerivative[f, source], operatorFailure]];
differentiateFactor[f : OperatorKernel[_][_, _], source_] := toTermList[Catch[kernelDerivative[f, source], operatorFailure]];
differentiateFactor[f_LegendreTransform, source_] := toTermList[legendreDerivative[f, source]];

(* ---- G[S], the propagator at the stationary point of F[S] ----
   Its derivatives (dG[S]/dS = (dSigma/dG)^-1, the inverse of the irreducible vertex, and dG[S]/dV) are not available:
   a message, and the term is $Failed *)
LegendrePropagator::derivative = "The derivatives of G[S] (LegendrePropagator), the propagator at the stationary point of Potthoff's F[S], are not available: dG[S]/dS is the inverse of the irreducible vertex, (\[Delta]\[CapitalSigma]/\[Delta]G)^-1. The terms that need one are $Failed.";
LegendrePropagator[g_ -> s_][x_, y_] := LegendrePropagator[g -> s, {}][x, y];
LegendrePropagator[g_ -> s_, k_List][x_, y_] /; orderlessQ[g] && ! OrderedQ[{x, y}] := LegendrePropagator[g -> s, k][y, x];
dependsOnSourceQ[f : LegendrePropagator[g_ -> s_, k_List][_, _], source_] := MemberQ[Prepend[k, s], Head[source]] && Head[source] =!= g;
differentiateFactor[f : LegendrePropagator[_ -> _, _List][_, _], source_] := If[dependsOnSourceQ[f, source],
   Message[LegendrePropagator::derivative]; {diagramTerm[$Failed, {}, {}]}, {}];

(* the derivative with respect to a kernel that is not a source, G[x, y] (a propagator, a self-energy): every G is an
   independent variable, as in SelfEnergy (the derivative symmetrised for a symmetric G, antisymmetrised for an
   antisymmetric one); zero for an expression free of
   a known kernel G (an undeclared two-label head gives the message nosource) *)
FunctionalDerivative[e_, g_Symbol[x_, y_]] /; ! sourceAtomQ[g[x, y]] && Context[g] =!= "System`" && ! FreeQ[e, g] :=
  (checkSuperfieldTraces[e, g]; derivativeWithRespectToPropagator[e, g, {x, y}, propagatorDerivativeMode[g]]);
FunctionalDerivative[e_, g_Symbol[x_, y_]] /; ! sourceAtomQ[g[x, y]] && FreeQ[e, g] && kernelHeadQ[g] := (checkSuperfieldTraces[e, g]; 0);
(* a trace that SuperfieldComponents kept as it is (Tr ln of a Nambu matrix with anomalous blocks) depends on the
   components of its superfield heads, but not through anything the derivative can see: a message, not a silent 0 *)
FunctionalDerivative::supertrace = "`1`, kept by SuperfieldComponents, depends on `2` through the superfield `3`, but it has no component form: its derivative with respect to `2` is taken as 0. Take the derivative of the superfield functional with respect to `3` first, then the components (SuperfieldComponents with `3` -> {G, 0, 0}, at zero anomalous blocks): FunctionalDerivative[Gamma, `3`[a, b]] with a -> psi[x], b -> psi[y] is the derivative with respect to the anomalous propagator Gpsipsi(x, y).";
checkSuperfieldTraces[e_, g_Symbol] := Scan[Function[t, Scan[Function[r,
      If[! FreeQ[t, r[[1]]] && MemberQ[r[[2]], g], Message[FunctionalDerivative::supertrace, t, g, r[[1]]]]], $superfieldTraceComponents]],
   If[$superfieldTraceComponents === {}, {}, Union[Cases[e, (TraceLog | OperatorTrace)[_], {0, Infinity}]]]];

(* ---- display ---- *)
operatorDisplay[a_] := a /. {Power[h_, -1] :> Superscript[h, "-1"], Dot[x__] :> Row[{x}]};
defineDisplayForm[TraceLog[a_], Interpretation[Row[{"Tr ln(", operatorDisplay[a], ")"}], TraceLog[a]]];
defineDisplayForm[OperatorTrace[a_], Interpretation[Row[{"Tr(", operatorDisplay[a], ")"}], OperatorTrace[a]]];
defineDisplayForm[OperatorKernel[a_][x_, y_], Interpretation[Subscript[If[MatchQ[a, Power[_Symbol, -1]], operatorDisplay[a],
     Row[{"(", operatorDisplay[a], ")"}]], Row[{x, y}]], OperatorKernel[a][x, y]]];
defineDisplayForm[LegendreTransform[phi_, g_ -> s_, opts___Rule], Interpretation[Row[{"\[ScriptCapitalF][", s, "]"}],
    LegendreTransform[phi, g -> s, opts]]];
defineDisplayForm[LegendrePropagator[g_ -> s_, k_List][x_, y_], Interpretation[Subscript[Row[{g, "[", s, "]"}], Row[{x, y}]],
    LegendrePropagator[g -> s, k][x, y]]];

(* ---- LaTeX (ExpressionToLaTeX) ---- *)
$latexHeadsWithRules = Join[$latexHeadsWithRules, {TraceLog, OperatorTrace, LegendreTransform, operatorProduct, inverseOperator}];
latexOperator[a_] := latexOfExpression[a /. {Dot -> operatorProduct} /. Power[h_, -1] :> inverseOperator[h]];
latexFactorOf[inverseOperator[h_]] := If[Head[h] === Plus, "\\left(" <> latexOperator[h] <> "\\right)", latexFactor[h]] <> "^{-1}";
latexFactorOf[operatorProduct[a__]] := joinStrings[(If[Head[#] === Plus, "\\left(" <> latexOperator[#] <> "\\right)", latexFactor[#]] &) /@ {a}, "\\,"];
latexFactorOf[TraceLog[a_]] := "\\mathrm{Tr}\\ln\\left(" <> latexOperator[a] <> "\\right)";
latexFactorOf[OperatorTrace[a_]] := "\\mathrm{Tr}\\left(" <> latexOperator[a] <> "\\right)";
latexFactorOf[OperatorKernel[a_][x_, y_]] := If[MatchQ[a, Power[_, -1]], latexOperator[a], "\\left(" <> latexOperator[a] <> "\\right)"] <>
   "(" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";
latexFactorOf[LegendreTransform[_, _ -> s_, ___]] := "\\mathcal{F}[" <> latexFactor[s] <> "]";
latexFactorOf[LegendrePropagator[g_ -> s_, ___][x_, y_]] := latexFactor[g] <> "[" <> latexFactor[s] <> "](" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";

(* ========================================================================= *)
(*  The 2PI effective action Gamma[G]                                        *)
(* ========================================================================= *)

(* {zeta, kappa}: zeta = -1 for fermions, +1 for bosons; kappa = zeta, 1/2 for real bosons:
     Gamma[G] = Phi[G] + kappa Tr ln(zeta G^-1) + kappa Tr(G0^-1 G - 1)
   fermions      Phi - Tr ln(-G^-1) - Tr(G0^-1 G - 1)            (G = -<psi psibar>, Sigma(x, y) = dPhi/dG(y, x))
   real bosons   Phi + 1/2 Tr ln G^-1 + 1/2 Tr(D0^-1 G - 1)      (G = <phi phi>, Sigma = -2 dPhi/dG)
   complex bosons Phi + Tr ln G^-1 + Tr(G0^-1 G - 1)             (G = <phi phibar>, Sigma(x, y) = -dPhi/dG(y, x))
   stationary where G^-1 = G0^-1 - Sigma[G] (Dyson), with the value -log Z there. *)
statisticsFactors[opts_List, allowed_List : {"Fermion", "Boson", "ComplexBoson"}] := Switch[statisticsOption[opts, allowed],
   "Fermion", {-1, -1}, "Boson", {1, 1/2}, "Superfield", {-1, -1/2}, _, {1, 1}];
(* superfields (a Grassmann field with an antisymmetric propagator calG = -<T NambuField NambuField>, the Nambu field NambuField = (psi, psibar)):
     Gamma[calG] = Phi - 1/2 Tr ln(-calG^-1) - 1/2 Tr((calG0^-1 - K) calG - 1)
   (the Gaussian Grassmann integral of one field is a Pfaffian: the factor 1/2), Sigma = -2 dPhi/dcalG, stationary where
   calG^-1 = calG0^-1 - K - Sigma (Dyson in Nambu form, the Gor'kov equations in components). The two-point source K
   contains the pairing sources. Accepted by the 2PI and 1PI effective actions (bare lines) only. *)
$superfieldActionStatistics = {"Fermion", "Boson", "ComplexBoson", "Superfield"};

EffectiveAction2PI::meanrule = "\"MeanFields\" -> `1` is not a list of rules field -> head of its mean field: one real boson phi -> phiMean, one superfield NambuField -> X, or both fields of fermions and complex bosons (psi -> Psi, psibar -> Psibar).";
EffectiveAction2PI::barred = "Which of the means `1` belongs to the barred field cannot be told: give Z0 = Exp[Integral Jbar(x) G0(x, y) J(y)] (the source of the unbarred field in the first slot) or declare the barred field with the sign -1 (DeclareField[phibar, J, -1]).";
Options[EffectiveAction2PI] = {"Statistics" -> "Fermion", "MeanFields" -> {}, "Method" -> Automatic, "Source" -> None, "MeanFieldDegree" -> All};
(* "Source" -> U: a one-body source in the bare inverse propagator, so that the action always gains Integral (barred
   field) U (field): G0^-1 -> G0^-1 - U for fermions (free action -psibar G0^-1 psi; it gains Integral psibar U psi),
   G0^-1 + U for complex bosons (free action phibar G0^-1 phi; Integral phibar U phi), D0^-1 + U for real bosons (1/2
   Integral phi U phi, as for the density vertex of PerturbativeDensityVertex). Gamma depends on U only through the trace
   term, so that at the stationary point dOmega/dU(x, y) = |kappa| G(y, x): <psibar(x) psi(y)> for fermions, <phibar(x)
   phi(y)> for complex bosons, 1/2 <phi(x) phi(y)> for real bosons *)
bareInverse[g0_, opts_List] := With[{u = optionValue[opts, Options[EffectiveAction2PI], "Source"]},
   Which[u === None, Power[g0, -1], statisticsFactors[opts, $superfieldActionStatistics][[1]] === 1, Power[g0, -1] + u, True, Power[g0, -1] - u]];
(* the default "Statistics" -> "Fermion" on a symmetric or antisymmetric G (a real boson or a superfield): a message, as
   for SelfEnergy and IrreducibleVertex *)
EffectiveAction2PI::statisticsdefault = "`1` is `2`, and \"Statistics\" is not given: EffectiveAction2PI uses the fermionic trace terms, -Tr ln(-`1`^-1) - Tr(G0^-1 `1` - 1). For `3` give \"Statistics\" -> \"`4`\".";
checkActionStatisticsGiven[g_, opts_List] := If[! MemberQ[optionKey /@ opts[[All, 1]], "Statistics"],
   Which[orderlessQ[g], Message[EffectiveAction2PI::statisticsdefault, g, "symmetric", "a real boson", "Boson"],
    MemberQ[$antisymmetricHeads, g], Message[EffectiveAction2PI::statisticsdefault, g, "antisymmetric", "a superfield", "Superfield"]]];
EffectiveAction2PI[phi_, g0_Symbol -> g_Symbol, opts___Rule] := Module[{zk = statisticsFactors[{opts}, $superfieldActionStatistics]},
   checkActionStatisticsGiven[g, {opts}];
   If[zk[[2]] === 1/2, checkRealBoson[g]];
   If[zk === {-1, -1/2}, checkSuperfield[g]];
   phi + zk[[2]] TraceLog[zk[[1]] Power[g, -1]] + zk[[2]] OperatorTrace[Dot[bareInverse[g0, {opts}], g] - 1]];
EffectiveAction2PI[z0_, interaction_, n_Integer, g0_Symbol -> g_Symbol, opts___Rule] := Module[
   {means = optionValue[{opts}, Options[EffectiveAction2PI], "MeanFields"], method = optionValue[{opts}, Options[EffectiveAction2PI], "Method"]},
   If[! MatchQ[means, {(_Symbol -> _Symbol) ...}], Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
   Which[
    means === {}, EffectiveAction2PI[If[n == 0, 0, LuttingerWardFunctional[z0, interaction, n, g0 -> g, "Method" -> method]], g0 -> g, opts],
    interaction === 0, With[{free = classicalFreeAction[z0, means, g0, {opts}]}, If[free === $Failed, $Failed, free + EffectiveAction2PI[0, g0 -> g, opts]]],
    True, meanFieldEffectiveAction[z0, interaction, n, g0 -> g, means, {opts}]]];

(* the free action at the mean fields: 1/2 phibar . D0^-1 . phibar (real bosons, phibar = <phi>), -Psibar . G0^-1 . Psi
   (fermions, Psi = <psi>, Psibar = <psibar>, G0 = -<psi psibar>_0), <phi>* . G0^-1 . <phi> (complex bosons, G0 = <phi phibar>_0). The means are declared as sources
   (Grassmann for fermions), so that FunctionalDerivative differentiates with respect to them. *)
(* the sources in the first slot of G0 in Z0 = Exp[Integral Jbar(x) G0(x, y) J(y)]: those of the unbarred fields *)
firstSlotSources[z0_, g0_] := Union[Flatten[Cases[z0, t_Times /; ! FreeQ[t, g0[_, _]] :> With[{first = Cases[t, g0[a_, _] :> a, {1}]},
      If[Length[first] == 1, Cases[t, h_Symbol[first[[1]]] /; MemberQ[sourceHeads[], h] :> h, Infinity], {}]], {0, Infinity}]]];
(* the mean of the unbarred field, from Z0, else from the sign -1 of the barred field (DeclareField[psibar, eta, -1]);
   None if neither tells *)
plainMean[z0_, g0_, means_List] := Module[{first = firstSlotSources[z0, g0], fromZ0, fromSign},
   fromZ0 = Select[means, Function[r, MemberQ[first, Replace[Cases[$fieldSourceMap, {r[[1]], src_, _} :> src], {{} -> None, {src_, ___} :> src}]]]];
   fromSign = Select[means, ! MemberQ[$fieldSourceMap, {#[[1]], _, -1}] &];
   Which[Length[fromZ0] == 1, fromZ0[[1]], Length[fromSign] == 1, fromSign[[1]], True, None]];
classicalFreeAction[z0_, means_List, g0_, opts_List] := Module[{zk = statisticsFactors[opts, $superfieldActionStatistics], x, y, barred, plain},
   {x, y} = freshSymbols[2];
   (* a superfield: one Grassmann-odd mean X = <NambuField>, -1/2 X . calG0^-1 . X *)
   If[zk === {-1, -1/2},
    If[Length[means] != 1, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    If[! MemberQ[sourceHeads[], means[[1, 2]]], DeclareGrassmannSource[means[[1, 2]]]];
    Return[SimplifyDiagrams[-1/2 IntegralOver[OperatorKernel[Power[g0, -1]][x, y] means[[1, 2]][x] ** means[[1, 2]][y], {x, y}]]]];
   If[zk[[2]] === 1/2,
    (* one real boson with one propagator *)
    If[Length[means] != 1, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    If[! MemberQ[sourceHeads[], means[[1, 2]]], DeclareSource[means[[1, 2]]]];
    SimplifyDiagrams[1/2 IntegralOver[OperatorKernel[Power[g0, -1]][x, y] means[[1, 2]][x] means[[1, 2]][y], {x, y}]],
    If[Length[means] != 2, Message[EffectiveAction2PI::meanrule, means]; Return[$Failed]];
    plain = plainMean[z0, g0, means];
    If[plain === None, Message[EffectiveAction2PI::barred, means]; Return[$Failed]];
    barred = DeleteCases[means, plain];
    If[zk[[1]] === -1,
     Scan[If[! MemberQ[sourceHeads[], #], DeclareGrassmannSource[#]] &, means[[All, 2]]];
     SimplifyDiagrams[-IntegralOver[OperatorKernel[Power[g0, -1]][x, y] barred[[1, 2]][x] ** plain[[2]][y], {x, y}]],
     Scan[If[! MemberQ[sourceHeads[], #], DeclareSource[#]] &, means[[All, 2]]];
     SimplifyDiagrams[IntegralOver[OperatorKernel[Power[g0, -1]][x, y] barred[[1, 2]][x] plain[[2]][y], {x, y}]]]]];

(* ========================================================================= *)
(*  Potthoff: the universal functional F[Sigma] and the self-energy          *)
(*  functional Omega_G0[Sigma]                                              *)
(* ========================================================================= *)

(* LegendreTransform[Phi, G -> S] is inert: F[S] = Phi[G[S]] + kappa Tr(S G[S]), with G[S] the propagator at which the
   self-energy of Phi (SelfEnergy) equals S. Its derivative with respect to S(x, y) is kappa G[S](y, x), written with
   the inert LegendrePropagator; ExpandTrace writes it out as Phi[G] + kappa Tr(S . G). *)
SelfEnergyFunctional[phi_, g0_Symbol -> s_, g_Symbol, opts___Rule] := With[{zk = statisticsFactors[{opts}]},
   LegendreTransform[phi, g -> s, opts] + zk[[2]] TraceLog[zk[[1]] (Power[g0, -1] - s)]];

(* ========================================================================= *)
(*  Zero dimensions                                                          *)
(* ========================================================================= *)

(* integrals dropped, delta functions 1, the kernels replaced by their values, a factor n (the number of internal states,
   2s + 1) for every closed loop of kernels and every trace; Tr ln A = Log[a^n] for an integer n (the sign of a then does
   not matter, e.g. Tr ln(-G^-1) = Log[g^-2] for spin 1/2), n Log[a] otherwise. The kernels of operators
   (OperatorKernel[A][x, y]) are given temporary heads, so that the loops through them are counted too (as in the trace
   Integral A(x, y) B(y, x) = Tr(A . B)). "Loops" -> Automatic: the heads of every two-label kernel that is neither
   symmetric nor an interaction line (propagators, self-energies, one-body sources), the kernels of operators made of
   such heads included *)
Options[ZeroDimensionalValue] = {"LoopFactor" -> 1, "Loops" -> Automatic, "Indices" -> {}};
zeroDimensionalLoopHeadQ[h_Symbol] := Context[h] =!= "System`" && ! orderlessQ[h] && ! MemberQ[$antisymmetricHeads, h] &&
   ! interactionStyleQ[lineStyleOf[h]] && ! MemberQ[{IntegralOver, GrassmannProduct, DummyVariable, OperatorTrace, TraceLog, LegendreTransform}, h];
zeroDimensionalLoopHeadQ[_] := False;
ZeroDimensionalValue[e_, rules_List, opts___Rule] /; AnyTrue[rules, MatchQ[#, _Symbol -> _List] &] :=
  Catch[zeroDimensionalArrayValue[e, rules, {opts}], arrayFailure];
ZeroDimensionalValue[e_, rules_List, opts___Rule] := Module[{n = optionValue[{opts}, Options[ZeroDimensionalValue], "LoopFactor"],
    loops = optionValue[{opts}, Options[ZeroDimensionalValue], "Loops"], headRules, valueRules, operatorValue, e1, operators, kernelHeads},
   headRules = Cases[rules, (h_Symbol -> v_) :> (h -> v)];
   valueRules = Join[Cases[rules, (h_Symbol -> v_) :> (h[___] -> v)], Cases[rules, r : ((_[___] -> _) | (_[___] :> _))]];
   operatorValue[a_] := ((a /. Dot -> Times) /. headRules);
   e1 = legendreWrittenOut[e] /. {TraceLog[a_] :> If[IntegerQ[n], Log[operatorValue[a]^n], n Log[operatorValue[a]]],
      OperatorTrace[a_] :> n operatorValue[a]};
   operators = Union[Cases[e1, OperatorKernel[a_][_, _] :> a, {0, Infinity}]];
   kernelHeads = Table[Unique["FunctionalDiagrams`Private`kernel"], {Length[operators]}];
   e1 = e1 /. OperatorKernel[a_][x_, y_] :> Replace[a, Thread[operators -> kernelHeads]][x, y];
   valueRules = Join[valueRules, MapThread[(#1[___] -> operatorValue[#2]) &, {kernelHeads, operators}]];
   If[loops === Automatic, loops = Join[
      Select[Union[Cases[e1, h_Symbol[a_, b_] /; ! ListQ[a] && ! ListQ[b] && ! NumericQ[a] :> h, {0, Infinity}]],
       ! MemberQ[kernelHeads, #] && zeroDimensionalLoopHeadQ[#] &],
      Pick[kernelHeads, (! FreeQ[#, h_Symbol /; zeroDimensionalLoopHeadQ[h]] &) /@ operators]],
    loops = Join[loops, Pick[kernelHeads, (! FreeQ[#, h_Symbol /; MemberQ[loops, h]] &) /@ operators]]];
   Expand[Total[(If[loops === {}, 1, n^ClosedLoopCount[#, loops]] ((# //. IntegralOver[f_, _] :> f) /. DeltaFunction[__] -> 1 /. valueRules)) & /@
      DiagramTerms[e1]]]];

(* ---- array values: the labels run over internal states ----
   A kernel with a list value (an array of rank = its number of labels, e.g. the 4 x 4 Nambu matrix of one site with spin
   1/2) makes every label an index: DeltaFunction is the identity, integrals are sums (the arrays of each term contracted
   pairwise, as sparse tensors), operators are matrices (numbers times the identity), Tr ln A = Log[Det[A]], Tr A = Tr[A],
   OperatorKernel[A][x, y] the entry; "Indices" -> {label -> i, ...} gives the internal state of the free labels. *)
ZeroDimensionalValue::loopfactor = "With array values the internal states are summed explicitly: \"LoopFactor\" is ignored.";
ZeroDimensionalValue::indices = "The free labels `1` need an internal state: \"Indices\" -> {label -> i, ...}.";
ZeroDimensionalValue::operator = "The operator `1` has no matrix value: give an array for each of its kernels.";
ZeroDimensionalValue::rank = "The array of `1` has not the rank `2` of its labels.";
SetAttributes[arrayFailure, HoldFirst];
arrayFailure[message_, args___] := (Message[message, args]; Throw[$Failed, arrayFailure]);
zeroDimensionalArrayValue[e_, rules_List, opts_List] := Module[{indices = optionValue[opts, Options[ZeroDimensionalValue], "Indices"],
    arrays, scalars, dim, e1, operators, kernelHeads},
   If[optionValue[opts, Options[ZeroDimensionalValue], "LoopFactor"] =!= 1, Message[ZeroDimensionalValue::loopfactor]];
   arrays = Cases[rules, (h_Symbol -> v_List) :> (h -> v)];
   scalars = Join[Cases[rules, (h_Symbol -> v_) /; ! ListQ[v] :> (h[___] -> v)], Cases[rules, r : ((_[___] -> _) | (_[___] :> _))]];
   dim = Length[arrays[[1, 2]]];
   e1 = legendreWrittenOut[e] /. {TraceLog[a_] :> Log[Factor[Det[arrayMatrix[a, arrays, dim]]]], OperatorTrace[a_] :> Tr[arrayMatrix[a, arrays, dim]]};
   operators = Union[Cases[e1, OperatorKernel[a_][_, _] :> a, {0, Infinity}]];
   kernelHeads = Table[Unique["FunctionalDiagrams`Private`kernel"], {Length[operators]}];
   e1 = e1 /. OperatorKernel[a_][x_, y_] :> Replace[a, Thread[operators -> kernelHeads]][x, y];
   arrays = Join[arrays, MapThread[(#1 -> arrayMatrix[#2, arrays, dim]) &, {kernelHeads, operators}]];
   Expand[Total[arrayTermValue[#, arrays, scalars, indices, dim] & /@ canonicalTermsOf[e1]]]];

(* the matrix of an operator *)
arrayOperatorQ[a_, arrays_List] := ! FreeQ[a, Alternatives @@ (arrays[[All, 1]])] || ! FreeQ[a, _Dot];
arrayMatrix[a_?NumericQ, _, dim_] := a IdentityMatrix[dim];
arrayMatrix[h_Symbol, arrays_List, dim_] := Which[MemberQ[arrays[[All, 1]], h], h /. arrays,
   kernelHeadQ[h], arrayFailure[ZeroDimensionalValue::operator, h], True, h IdentityMatrix[dim]];
arrayMatrix[a_Plus, arrays_List, dim_] := Total[arrayMatrix[#, arrays, dim] & /@ (List @@ a)];
arrayMatrix[a_Times, arrays_List, dim_] := With[{ops = Select[List @@ a, arrayOperatorQ[#, arrays] &]},
   Which[ops === {}, a IdentityMatrix[dim],
    Length[ops] == 1, (Times @@ Select[List @@ a, ! arrayOperatorQ[#, arrays] &]) arrayMatrix[First[ops], arrays, dim],
    True, arrayFailure[ZeroDimensionalValue::operator, a]]];
arrayMatrix[a_Dot, arrays_List, dim_] := Dot @@ (arrayMatrix[#, arrays, dim] & /@ (List @@ a));
arrayMatrix[Power[b_, -1], arrays_List, dim_] := Inverse[arrayMatrix[b, arrays, dim]];
arrayMatrix[Power[b_, n_Integer?Positive], arrays_List, dim_] := Dot @@ Table[arrayMatrix[b, arrays, dim], {n}];
arrayMatrix[Power[b_, n_Integer?Negative], arrays_List, dim_] := Inverse[Dot @@ Table[arrayMatrix[b, arrays, dim], {-n}]];
arrayMatrix[a_, _, _] := arrayFailure[ZeroDimensionalValue::operator, a];

(* the value of a term: its arrays contracted, the free labels fixed by one-entry tensors *)
arrayTermValue[diagramTerm[c_, o_List, d_List], arrays_List, scalars_List, indices_List, dim_] := Module[{factors = expandIntegerPowers[factorList[c]],
    tensorQ, tensors, rest, free, missing},
   tensorQ = (! AtomQ[#] && (Head[#] === DeltaFunction || MemberQ[arrays[[All, 1]], Head[#]])) &;
   tensors = (If[Head[#] === DeltaFunction, sparseTensor[IdentityMatrix[dim], List @@ #, dim], arrayTensor[#, arrays, dim]] &) /@ Select[factors, tensorQ];
   rest = (Times @@ Select[factors, ! tensorQ[#] &]) /. scalars;
   free = Complement[Union[Flatten[First /@ tensors]], d];
   missing = Select[free, ! MemberQ[First /@ indices, #] &];
   If[missing =!= {}, arrayFailure[ZeroDimensionalValue::indices, missing]];
   tensors = Join[tensors, ({{#}, {{{# /. indices}, 1}}} &) /@ free];
   rest grassmannProductOrOne[o] If[tensors === {}, 1, contractTensors[tensors]]];
arrayTensor[f_, arrays_List, dim_] := With[{v = Head[f] /. arrays},
   If[ArrayDepth[v] =!= Length[f], arrayFailure[ZeroDimensionalValue::rank, Head[f], Length[f]]]; sparseTensor[v, List @@ f, dim]];
(* a sparse tensor {labels, {{indices, value}, ...}} (nonzero entries only) of distinct labels; a repeated label takes the
   diagonal *)
sparseTensor[array_, labels_List, dim_] := With[{distinct = DeleteDuplicates[labels]},
   With[{where = (First[Flatten[Position[distinct, #, {1}, Heads -> False]]] &) /@ labels},
    {distinct, Select[({#, Extract[array, #[[where]]]} &) /@ Tuples[Range[dim], Length[distinct]], #[[2]] =!= 0 &]}]];
(* two tensors contracted over their shared labels; the labels in summed are summed over *)
contractTensorPair[{la_, ea_}, {lb_, eb_}, summed_List] := Module[{shared = Intersection[la, lb], pa, pb, out, keyRules, res},
   pa = (First[Flatten[Position[la, #, {1}, Heads -> False]]] &) /@ shared;
   pb = (First[Flatten[Position[lb, #, {1}, Heads -> False]]] &) /@ shared;
   out = Select[DeleteDuplicates[Join[la, lb]], ! MemberQ[summed, #] &];
   keyRules = Append[(#[[1, 1, pb]] -> #) & /@ GatherBy[eb, #[[1, pb]] &], _ -> {}];
   res = Join @@ (Function[a1, ({Replace[out, Join[Thread[la -> a1[[1]]], Thread[lb -> #[[1]]]], {1}], a1[[2]] #[[2]]} &) /@
        Replace[a1[[1, pa]], keyRules]] /@ ea);
   {out, Select[({#[[1, 1]], Expand[Total[#[[All, 2]]]]} &) /@ GatherBy[res, First], #[[2]] =!= 0 &]}];
(* all tensors contracted, the pair that leaves the fewest open labels first; every label is summed *)
contractTensors[tensors_List] := Module[{ts = tensors, count, pairs, best, summed},
   While[Length[ts] > 1,
    count[l_] := Count[Flatten[ts[[All, 1]]], l];
    pairs = Flatten[Table[{i, j}, {i, Length[ts]}, {j, i + 1, Length[ts]}], 1];
    best = First[SortBy[pairs, With[{l = Union[ts[[#[[1]], 1]], ts[[#[[2]], 1]]]},
        Length[Select[l, Function[x, count[x] > Count[Join[ts[[#[[1]], 1]], ts[[#[[2]], 1]]], x]]]]] &]];
    summed = Select[Union[ts[[best[[1]], 1]], ts[[best[[2]], 1]]], count[#] == Count[Join[ts[[best[[1]], 1]], ts[[best[[2]], 1]]], #] &];
    ts = Append[Delete[ts, {{best[[1]]}, {best[[2]]}}], contractTensorPair[ts[[best[[1]]]], ts[[best[[2]]]], summed]]];
   Expand[Total[(#[[2]] &) /@ ts[[1, 2]]]]];

(* ========================================================================= *)
(*  The field dependence: Gamma[phibar, G] of Cornwall, Jackiw and Tomboulis *)
(*  (phibar = <phi>), Grassmann-odd means, the 1PI effective action          *)
(*  Gamma[phibar]                                                            *)
(* ========================================================================= *)

(* ---- the interaction at the shifted field ----
   S_int[M + phi] by degree in the fluctuation phi: the list {S_int[M], linear, quadratic, cubic, ...}. means: {field ->
   head of its mean}; for fermions {psi -> Psi, psibar -> Psibar}. *)
EffectiveAction2PI::shift = "The interaction at the shifted field could not be split by degree in the fluctuations.";
shiftedInteractionParts[interaction_, means_List] := Module[{t, shifted, degree},
   shifted = SimplifyDiagrams[interaction /. (With[{f = #[[1]], m = #[[2]]}, f[a___] :> m[a] + t f[a]] & /@ means)];
   If[! FreeQ[Cases[shifted, _IntegralOver, Infinity], t], Message[EffectiveAction2PI::shift]; Return[$Failed]];
   degree = Exponent[shifted, t];
   Table[Coefficient[shifted, t, k], {k, 0, Max[degree, 2]}]];

(* ---- vacuum diagrams of vertices that carry Grassmann-odd means ----
   The Wick contraction of the package pairs fields only. A vertex with odd means M (constants for the contraction) is
   written s M F, its means in front of its fields F (s the sign of that reordering), with commuting proxies of the means
   in its coefficient, so that the symmetries of the vertex respect them; the fields are paired as usual, and the product
   v1 ... vk = s1 ... sk (-1)^(sum_{i<j} |F_i| |M_j|) M1 ... Mk F1 ... Fk gives the ordered odd factors of each term. For
   commuting means (bosons) nothing changes: they stay in the coefficient. *)
oddMeanSplit[diagramTerm[c_, o_List, d_], oddHeads_List] := Module[{isMean = (MemberQ[oddHeads, Head[#]] &) /@ o, means, fields, sign},
   means = Pick[o, isMean]; fields = Pick[o, isMean, False];
   sign = (-1)^Total[Table[If[isMean[[i]], Count[Take[isMean, i - 1], False], 0], {i, Length[o]}]];
   {sign c (Times @@ ((meanProxy[Head[#]] @@ #) & /@ means)), fields, d, means}];
meanVertex[term_diagramTerm, oddHeads_List] := With[{s = oddMeanSplit[term, oddHeads]},
   Append[splitInteractionTerm[diagramTerm[s[[1]], s[[2]], s[[3]]]], s[[4]]]];
(* keep: a selection of the products of vertices by their coefficients (with the proxies of the means), e.g. a given
   number of means; all by default *)
meanConnectedVacuum[z0_, interaction_, orders_List, oddHeads_List, keep_ : (True &)] :=
  termListToExpression[canonicalTermList[meanConnectedVacuumTerms[z0, interaction, orders, oddHeads, keep]]];
(* the n-particle irreducible ones (n = 1, 2) with lines of the head line, selected before the canonical form *)
meanIrreducibleVacuum[z0_, interaction_, orders_List, oddHeads_List, n_Integer, line_, keep_ : (True &)] :=
  termListToExpression[selectCanonicalTermList[integrateDeltaFunctions /@ meanConnectedVacuumTerms[z0, interaction, orders, oddHeads, keep],
    particleIrreducibleTermQ[#, n, {}, {line}] &]];
(* the raw terms *)
meanConnectedVacuumTerms[z0_, interaction_, orders_List, oddHeads_List, keep_ : (True &)] := Module[{types = toTermList[interaction], split, terms = {}},
   If[interaction === 0, Return[{}]];
   split = meanVertex[#, oddHeads] & /@ types;
   Do[Do[Module[{counts, weight, vs, fieldParity, meanParity, sign, meansOrdered},
       counts = Table[Count[m, ty], {ty, Length[types]}];
       weight = (-1)^k/(Times @@ (Factorial /@ counts));
       vs = Table[With[{v = split[[m[[i]]]], new = freshSymbols[Length[split[[m[[i]], 3]]]]},
          {m[[i]], v[[1]] /. Thread[v[[3]] -> new], v[[2]] /. Thread[v[[3]] -> new], new, v[[4]] /. Thread[v[[3]] -> new]}], {i, Length[m]}];
       fieldParity = (Count[#, _?grassmannOddAtomQ] &) /@ vs[[All, 3]];
       meanParity = Length /@ vs[[All, 5]];
       sign = (-1)^Sum[meanParity[[j]] Total[Take[fieldParity, j - 1]], {j, Length[vs]}];
       meansOrdered = Join @@ vs[[All, 5]];
       If[TrueQ[keep[vs[[All, 2]]]], terms = Join[terms, (diagramTerm[weight sign (#[[1]] /. meanProxy[_][___] -> 1), Join[meansOrdered, #[[2]]], #[[3]]] &) /@
          pairingTermsToTermList[wickPairingSum[z0, {}, vs[[All, 1 ;; 4]], "Connected"], vs[[All, 1 ;; 4]]]]]],
      {m, Select[Tuples[Range[Length[types]], k], OrderedQ]}], {k, orders}];
   terms];

(* ---- Gamma[phibar, G] ----
   Gamma[M, G] (M = phibar = <phi>) = S[M] + kappa Tr ln(zeta G^-1) + kappa Tr(D^-1[M] G - 1) + Gamma2[M, G]
   (Cornwall, Jackiw and Tomboulis): S[M] = S0[M] + S_int[M] the classical action, D^-1[M] = S''[M] = G0^-1 + S_int''[M]
   its second derivative, the bare inverse propagator in the background (for fermions d^2 S/dPsibar(x) dPsi(y), the Psi
   derivative first: G0^-1 - Sigma_HF, the Hartree and Fock potentials of the means, the first-order self-energy with
   -Psi(x) Psibar(y) in place of G(x, y); phi^4: S_int'' = lambda M^2/2). Its part kappa Tr(S_int''[M] G) (mass below), the
   vertex of degree two in the fluctuation closed by one line G, gives the self-energy its mean-field part (stationarity:
   G^-1 = D^-1[M] - Sigma[M, G], Sigma from Gamma2); it is computed as minus the connected vacuum diagram of that vertex
   with one line G (its Gaussian average with the propagator G). Gamma2[M, G] = minus the 2PI vacuum diagrams with
   lines G and the vertices of S_int[M + phi] of degree three or more in the fluctuation (phi^4: lambda M phi^3/6 and
   lambda phi^4/24). The linear part of S_int[M + phi] does not appear: it is cancelled by the source (the one-point
   counterterm j of the self-consistent representation). At M = 0 it is Gamma[G] built on the Luttinger-Ward functional.
   Fermions: the means Psi, Psibar are Grassmann-odd (declared as Grassmann sources), and the vertices with an odd number
   of fluctuating fields carry one of them; Gamma is then Nambu's functional at zero anomalous propagator. *)
(* "MeanFieldDegree" -> {k, ...}: only the terms of these degrees in the means (the Taylor coefficients that give the vertex
   functions at zero mean field; {4} for the fully irreducible vertex), computed directly *)
meanDegreeOfTerm[diagramTerm[c_, o_List, _], heads_List] := Module[{tt},
   Exponent[c /. h_Symbol[a___] /; MemberQ[heads, h] :> tt h[a], tt] + Count[o, h_Symbol[___] /; MemberQ[heads, h]]];
selectMeanDegree[e_, _, All] := e;
selectMeanDegree[e_, heads_List, degrees_List] := termListToExpression[Select[canonicalTermsOf[e], MemberQ[degrees, meanDegreeOfTerm[#, heads]] &]];
meanFieldEffectiveAction[z0_, interaction_, n_Integer, g0_ -> g_, means_List, opts_List] := Module[
   {zk = statisticsFactors[opts, $superfieldActionStatistics], free, oddHeads, parts, mass, vertices, gamma2, degrees, keep, tt},
   free = classicalFreeAction[z0, means, g0, opts];
   If[free === $Failed, Return[$Failed]];
   oddHeads = If[zk[[1]] === -1, means[[All, 2]], {}];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   mass = -meanConnectedVacuum[z0 /. g0 -> g, parts[[3]], {1}, oddHeads];
   vertices = Total[Drop[parts, 3]];
   degrees = optionValue[opts, Options[EffectiveAction2PI], "MeanFieldDegree"];
   If[IntegerQ[degrees], degrees = {degrees}];
   keep = If[degrees === All, True &, MemberQ[degrees, Exponent[(Times @@ #) /. {h_Symbol[a___] /; MemberQ[means[[All, 2]], h] :> tt h[a],
         p_meanProxy[a___] :> tt p[a]}, tt]] &];
   gamma2 = If[n == 0 || vertices === 0, 0,
     -meanIrreducibleVacuum[z0, vertices, Range[n], oddHeads, 2, g0, keep] /. g0 -> g];
   selectMeanDegree[free + parts[[1]] + EffectiveAction2PI[0, g0 -> g, Sequence @@ opts] + mass + gamma2, means[[All, 2]], degrees]];

(* ---- the 1PI effective action Gamma[M] (EffectiveAction1PI) ----
   Gamma[M] = S[M] + 1/2 Tr ln S''[M] + Gamma1[M], Gamma1 = -(connected 1PI vacuum diagrams, with two or more loops, with lines
   D[M] and the vertices of S_int[M + phi] of degree three or more), D[M] = S''[M]^-1 = (D0^-1 + S_int''[M])^-1 the propagator in
   the background, written with the head given (D0 -> Dm). Dm depends on M: dDm(x, y)/dM(a) = -Integral Dm(x, u) S_int'''[M](u, v, a) Dm(v, y), declared with
   DeclareFunctionalDerivative, so that FunctionalDerivative differentiates through the lines and the trace-log. At M = 0,
   Dm = D0 if the interaction has no quadratic part. Real bosons only.
   With bare lines (D0 -> D0, or G0 -> G0 for fermions) the vertices of degree two stay vertices: Gamma[M] = S[M] +
   kappa Tr ln(zeta G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of S_int[M + phi] of degree two
   or more): the rings of the degree-two vertices are the one loop, the diagrams with two or more loops Gamma1 with its lines
   expanded in G0. For fermions ({psi -> Psi, psibar -> Psibar}, Grassmann-odd means) this is the only form: in a Grassmann
   background the vertices of degree two include the anomalous ones V Psi Psi psibar psibar, S'' is a Nambu matrix and the
   propagator in the background S''^-1 has anomalous parts; Gamma = S - 1/2 Tr ln(-S'') + Gamma1, as for a superfield. *)
EffectiveAction1PI::onepi = "The 1PI effective action is available for a real boson with one mean field (phi -> phiMean, lines in the background D0 -> Dm or bare D0 -> D0), for fermions with their two Grassmann-odd means ({psi -> Psi, psibar -> Psibar}, bare lines G0 -> G0) and for a superfield with its Grassmann-odd mean (NambuField -> X, \"Statistics\" -> \"Superfield\", bare lines).";
Options[EffectiveAction1PI] = {"Statistics" -> "Boson"};
EffectiveAction1PI[z0_, interaction_, n_Integer, field_Symbol -> mean_Symbol, d0_Symbol -> dm_Symbol, opts___Rule] :=
  onePIEffectiveAction[z0, interaction, n, d0 -> dm, {field -> mean}, Join[{opts}, Options[EffectiveAction1PI]]];
EffectiveAction1PI[z0_, interaction_, n_Integer, {field_Symbol -> mean_Symbol}, d0_Symbol -> dm_Symbol, opts___Rule] :=
  EffectiveAction1PI[z0, interaction, n, field -> mean, d0 -> dm, opts];
(* fermions: two fields with Grassmann sources *)
EffectiveAction1PI[z0_, interaction_, n_Integer, means : {_Symbol -> _Symbol, _Symbol -> _Symbol}, g0_Symbol -> dm_Symbol, opts___Rule] :=
  If[AllTrue[means[[All, 1]], MemberQ[$grassmannSources, fieldSourceOf[#]] &] && dm === g0,
   onePIBareLines[z0, interaction, n, g0, means, Join[{"Statistics" -> "Fermion"}, DeleteCases[{opts}, "Statistics" -> _]]],
   Message[EffectiveAction1PI::onepi]; $Failed];
fieldSourceOf[field_] := Replace[Cases[$fieldSourceMap, {field, s_, _} :> s], {{} -> None, {s_, ___} :> s}];
onePIEffectiveAction[z0_, interaction_, n_Integer, g0_ -> dm_, means_List, opts_List] := Module[
   {free, parts, mean, field, source, cubic, x, y, a, u, v, rhs, vacuum},
   (* a superfield: bare lines only *)
   If[statisticsOption[opts, $superfieldActionStatistics] === "Superfield",
    Return[If[Length[means] == 1 && dm === g0 && MemberQ[$grassmannSources, fieldSourceOf[means[[1, 1]]]],
      onePIBareLines[z0, interaction, n, g0, means, opts], Message[EffectiveAction1PI::onepi]; $Failed]]];
   If[statisticsFactors[opts][[2]] =!= 1/2 || Length[means] != 1, Message[EffectiveAction1PI::onepi]; Return[$Failed]];
   {field, mean} = {means[[1, 1]], means[[1, 2]]};
   source = fieldSourceOf[field];
   If[source === None || MemberQ[$grassmannSources, source], Message[EffectiveAction1PI::onepi]; Return[$Failed]];
   If[dm === g0, Return[onePIBareLines[z0, interaction, n, g0, means, opts]]];
   free = classicalFreeAction[z0, means, g0, opts];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   If[! orderlessQ[dm], DeclareSymmetric[dm]];
   (* S_int'''[M](u, v, a): the third derivative of the cubic part, fields replaced by their sources *)
   {x, y, a, u, v} = freshSymbols[5];
   cubic = If[Length[parts] >= 4, parts[[4]], 0] /. field[b___] :> source[b];
   rhs = SimplifyDiagrams[-IntegralOver[dm[x, u] FunctionalDerivative[cubic, source[u], source[v], source[a]] dm[v, y], {u, v}]];
   $derivativeRules = DeleteCases[$derivativeRules, r_ /; ! FreeQ[r[[1]], dm] && ! FreeQ[r[[1]], mean]];
   With[{d = dm, m = mean, xx = x, yy = y, aa = a, r = rhs},
    DeclareFunctionalDerivative[d[xx_, yy_], m[aa_], r]];
   vacuum = If[n == 0 || Total[Drop[parts, 3]] === 0, 0,
     meanIrreducibleVacuum[z0 /. g0 -> dm, Total[Drop[parts, 3]], Range[n], {}, 1, dm]];
   free + parts[[1]] + 1/2 TraceLog[Power[dm, -1]] - vacuum];
(* bare lines: S[M] + kappa Tr ln(zeta G0^-1) - (connected 1PI vacuum diagrams with lines G0 and the vertices of degree two
   or more); the means are declared as sources (Grassmann for fermions) by classicalFreeAction *)
onePIBareLines[z0_, interaction_, n_Integer, g0_, means_List, opts_List] := Module[{zk = statisticsFactors[opts, $superfieldActionStatistics], free, parts, vertices, vacuum},
   free = classicalFreeAction[z0, means, g0, opts];
   If[free === $Failed, Return[$Failed]];
   parts = shiftedInteractionParts[interaction, means];
   If[parts === $Failed, Return[$Failed]];
   vertices = Total[Drop[parts, 2]];
   vacuum = If[n == 0 || vertices === 0, 0, meanIrreducibleVacuum[z0, vertices, Range[n], If[zk[[1]] === -1, means[[All, 2]], {}], 1, g0]];
   free + parts[[1]] + zk[[2]] TraceLog[zk[[1]] Power[g0, -1]] - vacuum];

(* ---- derivatives through kernels that depend on a source implicitly (the background propagator) ----
   d Tr ln A = Tr(A^-1 dA), d Tr A = Tr dA, d A(x, y) = (dA)(x, y), with dA from opVariation for every head h of A whose
   derivative is declared: dA = c L dh R, and dh(u, w)/dsource from FunctionalDerivative *)
(* the implicit kernels of A: heads with a declared derivative, and functions of two labels (LabelFunction) whose body
   contains the source, d f(u, w)/dsource being the derivative of the body *)
implicitHeadsIn[a_, source_] := Join[Select[Union[Cases[{a}, h_Symbol, {0, Infinity}, Heads -> True]],
    MemberQ[$derivativePairs, {#, Head[source]}] &],
   Union[Cases[{a}, f_Function /; ! FreeQ[f, Head[source]], {0, Infinity}]]];
dependsOnSourceQ[f_TraceLog, source_] := ! FreeQ[f, Head[source]] || implicitHeadsIn[f[[1]], source] =!= {};
dependsOnSourceQ[f_OperatorTrace, source_] := ! FreeQ[f, Head[source]] || implicitHeadsIn[f[[1]], source] =!= {};
dependsOnSourceQ[f : OperatorKernel[_][__], source_] := ! FreeQ[f[[0]], Head[source]] || implicitHeadsIn[f[[0, 1]], source] =!= {};
implicitVariation[a_, source_, inner_] := Total[Function[h, Module[{u, w}, {u, w} = freshSymbols[2];
      IntegralOver[Total[(#[[1]] inner[#[[2]], #[[3]], u, w] &) /@ opVariation[a, h]] FunctionalDerivative[h[u, w], source], {u, w}]]] /@
    implicitHeadsIn[a, source]];
traceDerivative[TraceLog[a_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[dotOf[{#2, Power[a, -1], #1}], #4, #3] &];
traceDerivative[OperatorTrace[a_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[dotOf[{#2, #1}], #4, #3] &];
kernelDerivative[OperatorKernel[a_][x_, y_], source : _[_]] /; implicitHeadsIn[a, source] =!= {} :=
  implicitVariation[a, source, kernelOf[#1, x, #3] kernelOf[#2, #4, y] &];
(* a source in an operator, not in a kernel (D0^-1 + lambda/2 phibar^2): a message, and 0 *)
traceDerivative[f : (_TraceLog | _OperatorTrace), source : _[_]] /; ! FreeQ[f[[1]], Head[source]] :=
  (Message[FunctionalDiagrams::opsource, userForm[f[[1]]], Head[source], userForm[f]]; 0);
kernelDerivative[f : OperatorKernel[a_][_, _], source : _[_]] /; ! FreeQ[a, Head[source]] :=
  (Message[FunctionalDiagrams::opsource, userForm[a], Head[source], userForm[f]]; 0);

(* ========================================================================= *)
(*  21. Load-time check and banner                                           *)
(* ========================================================================= *)

(* warn if an exported name is shadowed by a symbol of another context (e.g. a built-in of a later version) *)
FunctionalDiagrams::clash = "The names `1` are already defined in other contexts; the package versions are not used.";
$publicNames = {"IntegralOver", "DeltaFunction", "GrassmannProduct", "DummyVariable", "DeclareSource", "DeclareGrassmannSource",
   "DeclareGrassmannOdd", "DeclareGrassmannGraded", "DeclareSymmetric", "DeclareAntisymmetric",
   "DeclareFunctionalDerivative", "DeclareDiscreteIndexValues", "DeclareField", "DeclareLineStyle",
   "DeclareDirectedPropagator", "DeclareInteractionLine", "DeclareCoupling", "DeclareDisplayName", "DeclareLocalVertex", "DeclareBlob", "ResetDeclarations",
   "FunctionalDerivative", "SimplifyDiagrams", "DiagramTerms", "SetSourcesToZero", "FieldDerivative",
   "FieldExpectationValue", "GaussianExpectationValue", "PerturbativeCorrelationFunction", "ConnectedCorrelationFunction",
   "ConnectedVacuumDiagrams", "OrderPart", "ConnectedDiagrams", "DropVacuumBubbles", "ParticleIrreducibleQ",
   "ParticleIrreducibleDiagrams", "SkeletonDiagrams", "ClosedLoopCount", "DropClosedLoops", "AmputateExternalLegs",
   "LuttingerWardFunctional", "SelfEnergy", "IrreducibleVertex", "PerturbativeFullVertex", "TwoParticleReducibleQ",
   "ReducibleDiagrams", "IrreducibleDiagrams", "ParquetDecomposition", "BetheSalpeterLadder", "CheckParquetEquations", "PerturbativeDensityVertex", "LabelFunction", "ExactEquation", "CheckEquation", "HubbardStratonovich", "IntegrateOutField",
   "DysonSeries", "ConnectedFunctionFromVertices", "FullPropagator", "VertexFunction", "MeanField", "ExternalSource",
   "DrawDiagram", "DiagramPlot", "DiagramToTikZ", "ExpressionToLaTeX",
   "FermiFunction", "BoseFunction", "DeclareMatsubaraFrequency", "MatsubaraSum", "LoopMomentum", "LoopFrequency", "MomentumSum", "DiagramIntegrand",
   "EvaluateMatsubara", "AnalyticContinuation", "RetardedImaginaryPart", "EnergyDelta", "CuttingRuleImaginaryPart", "CheckCuttingRules", "ZeroTemperatureLimit", "Temperature",
   "ScalarPropagator", "MomentumSquared", "RenormalizationScale", "LoopIntegral", "DiagramLoopIntegral", "IntegrateLoops", "GFunction",
   "TadpoleIntegral", "BubbleIntegral", "FeynmanParameterIntegral", "FeynmanParameter", "EpsilonExpand", "PolePart",
   "InfraredRearrangedIntegral", "MassInsertion", "BetaFunctionFromZ", "AnomalousDimensionFromZ",
   "GammaMatrix", "SlashedMomentum", "DiracProduct", "DiracTrace", "SpinorChain", "LorentzMetric", "FourVector", "ScalarProduct", "SpacetimeDimension", "DiracSimplify", "DiracOrder", "QEDIntegrand", "FeynmanDenominator", "MinkowskiLoopIntegral", "IntegrateOneLoop", "SimplexIntegral", "ParameterIntegral", "ReduceParameterIntegrals", "CombineParameterIntegrals", "ExpandParameterIntegrals", "DifferentiateUnderIntegral", "VertexFormFactors",
   "EffectiveAction2PI", "EffectiveAction1PI", "TraceLog", "OperatorTrace", "OperatorKernel", "ExpandTrace", "LegendreTransform",
   "LegendrePropagator", "SelfEnergyFunctional", "ZeroDimensionalValue", "SuperfieldComponents",
   "FunctionalDiagramsUpdateCheck"};
With[{bad = Select[$publicNames, With[{sym = Symbol[#]}, Context[sym]] =!= "FunctionalDiagrams`" &]},
  If[bad =!= {}, Message[FunctionalDiagrams::clash, bad]]];

(* the banner shown when the package is loaded in a notebook: the logo (the exchange vacuum diagram on a blue badge,
   the geometry of Development/make_logo.py in a 128 x 128 box, y upwards), the attribution "FunctionalDiagrams, by
   Nazzareno Africani" word for word (term (b) of NOTICE), the version, license and how to cite, with links to the
   repository and the manual; built here, printed by the footer only when $Notebooks *)
$logoAccent = RGBColor[31/255, 78/255, 121/255];
$logoLight = RGBColor[159/255, 197/255, 232/255];
logoWavyPoints[{ax_, ay_}, {bx_, by_}, amplitude_, halfwaves_, n_:120] :=
  Module[{length = Sqrt[(bx - ax)^2 + (by - ay)^2], nx, ny},
    {nx, ny} = {-(by - ay), bx - ax}/length;
    Table[{ax + (bx - ax) t, ay + (by - ay) t} + {nx, ny} amplitude Min[1, 6 t, 6 (1 - t)] Sin[Pi halfwaves t],
      {t, 0., 1., 1./n}]];
(* the arrow heads wider than twice the loop (make_logo.py: 2 x 0.58 x size long, 2 x 0.72 x size wide), so that they
   stand out of it at small sizes *)
logoArrowhead[r_, angle_, size_] :=
  Module[{a = angle Degree, p, tangent, normal},
    p = {64 + r Cos[a], 64 + r Sin[a]}; tangent = {-Sin[a], Cos[a]}; normal = {Cos[a], Sin[a]};
    Polygon[{p + 0.58 size tangent, p - 0.58 size tangent + 0.72 size normal, p - 0.58 size tangent - 0.72 size normal}]];
(* the badge as drawn for small sizes (Logo/FunctionalDiagrams_logo_small.svg: no arrows or dots, heavier lines),
   packageLogo[size, False] the full one *)
packageLogo[size_:30, small_:True] :=
  Module[{r, loop, wave, amplitude, halfwaves},
    {r, loop, wave, amplitude, halfwaves} = If[small, {42, 12, 10, 9, 4}, {38, 6.5, 6, 7.5, 6}];
    Graphics[{
       $logoAccent, Rectangle[{0, 0}, {128, 128}, RoundingRadius -> 28],
       {EdgeForm[{White, Opacity[0.16], Thickness[2/128]}], FaceForm[None],
        Rectangle[{1, 1}, {127, 127}, RoundingRadius -> 27]},
       CapForm["Round"], JoinForm["Round"], $logoLight, Thickness[wave/128],
       Line[logoWavyPoints[{64 - r, 64}, {64 + r, 64}, amplitude, halfwaves]],
       White, Thickness[loop/128], Circle[{64, 64}, r],
       If[small, {}, {logoArrowhead[r, 90, 13], logoArrowhead[r, -90, 13], Disk[{64 - r, 64}, 7], Disk[{64 + r, 64}, 7]}]},
      PlotRange -> {{0, 128}, {0, 128}}, PlotRangePadding -> None, ImagePadding -> None, ImageSize -> size,
      BaselinePosition -> Scaled[0.2]]];
(* lightDarkColor (a colour for light notebooks and one for dark notebooks) is defined with the pictures *)
$repositoryURL = "https://github.com/n4sd4q/FunctionalDiagrams";
(* the name in the blue of the logo (light blue in a dark notebook), the rest in grey (contrast above 4.5:1 on both
   backgrounds); U+2010 (a hyphen, not a minus sign), U+00B7 and U+00A0 by character code, so that the file stays plain
   ASCII. The front end may break a line between any two items of a Row, so the attribution, the version, the license,
   "please cite it: CITATION.cff" and GitHub are each one unbreakable group (LineBreakWithin -> False) ending with its
   separator and a breakable space: in a narrow window the lines wrap after a separator, never before one.
   loadBanner[release] adds a third line, the notice of a newer release (updateNotice) *)
bannerDot[] := With[{nbsp = FromCharacterCode[160]}, nbsp <> nbsp <> FromCharacterCode[183] <> nbsp <> " "];
bannerGroup[items__] := Style[Row[{items}], LineBreakWithin -> False];
bannerBlue[] := lightDarkColor[$logoAccent, $logoLight];
bannerGrey[] := lightDarkColor[GrayLevel[0.45], GrayLevel[0.6]];
loadBanner[release_ : None] :=
  Module[{dot = bannerDot[], group = bannerGroup, blue = bannerBlue[], grey = bannerGrey[],
     file = $repositoryURL <> "/blob/main/"},
    Style[Grid[{{packageLogo[30],
       Column[{
         Row[{group[Style["FunctionalDiagrams", Bold, 14, blue], Style[", by Nazzareno Africani", 14, blue],
             Style[dot, 12, grey]],
           Style["functional derivatives and the Feynman diagrams they generate", 12, grey]}],
         Style[Row[{group["version " <> $FunctionalDiagramsVersion, dot],
             group[StringReplace["GPL-3.0-or-later", "-" -> FromCharacterCode[8208]], dot],
             group["please cite it: ", Hyperlink["CITATION.cff", file <> "CITATION.cff"], dot],
             group[Hyperlink["GitHub", $repositoryURL], dot],
             Hyperlink["Manual", file <> "Documentation/FunctionalDiagrams_Manual.pdf"]}], 11, grey],
         If[AssociationQ[release], updateNotice[release], Nothing]},
        Spacings -> 0.1]}},
      Alignment -> {Left, Center}, Spacings -> {0.7, 0}], FontFamily -> "Source Sans Pro", LineIndent -> 0]];

(* the update check. In a notebook (never in a script), at most once a day, the package asks GitHub for its latest
   release in the background (URLSubmit: loading never waits for the network) and, if it is newer than the version
   loaded, the banner gets a third line with the PacletInstall line of its paclet. Any failure (offline, a proxy, a
   time limit, the rate limit of GitHub, an answer without the fields) is silent. The handler of the answer runs after
   the evaluation that loaded the package, when a Print would go to the Messages window: it writes the banner again
   in its own cell, found by a tag given to it at load, without moving the selection (NotebookWrite to a CellObject).
   FunctionalDiagramsUpdateCheck[] asks by hand, and waits for the answer *)
$latestReleaseURL = "https://api.github.com/repos/n4sd4q/FunctionalDiagrams/releases/latest";
latestReleaseRequest[] := HTTPRequest[$latestReleaseURL, <|"Headers" -> {"Accept" -> "application/vnd.github+json"}|>];
(* the date of the last check at load, in a small file of the user's base directory *)
$updateCheckDirectory := FileNameJoin[{$UserBaseDirectory, "ApplicationData", "FunctionalDiagrams"}];
updateCheckDate[] := DateString[{"Year", "-", "Month", "-", "Day"}];
(* options of URLRead and URLSubmit, those that the version of Mathematica knows *)
networkOptions[f_, opts_List] := FilterRules[opts, Options[f]];

(* the numbers of a version "2.10.0" or of a tag "v2.10.0"; $Failed for anything else (a pre-release "v2.5.0-rc1") *)
versionNumbers[s_String] := If[StringMatchQ[s, RegularExpression["v?[0-9]+\\.[0-9]+\\.[0-9]+"]],
   FromDigits /@ StringSplit[StringReplace[s, StartOfString ~~ "v" -> ""], "."], $Failed];
versionNumbers[_] := $Failed;
(* whether version a is newer than version b, number by number (2.10.0 is newer than 2.9.0); False if either is not a
   version. The canonical order of two lists of three integers is that of their numbers, from the first *)
newerVersionQ[a_, b_] := With[{x = versionNumbers[a], y = versionNumbers[b]},
   ListQ[x] && ListQ[y] && Order[y, x] === 1];

(* the latest release in the answer of GitHub (JSON): <|"Version" -> "2.5.0", "Paclet" -> url of its .paclet,
   "Page" -> url of its page|>, or $Failed for a draft or a pre-release, a malformed tag or no paclet of this
   repository. ImportString reads bytes: the text (with the release notes) is encoded in UTF-8 first. The paclet's
   address must be one of this repository's downloads, made of plain characters, since it is shown inside a line of
   code to copy *)
latestReleaseOf[body_String] := Module[{json, tag, version, paclets},
   json = Quiet[ImportString[FromCharacterCode[ToCharacterCode[body, "UTF8"]], "RawJSON"]];
   If[!AssociationQ[json], Return[$Failed]];
   tag = Lookup[json, "tag_name"];
   If[TrueQ[Lookup[json, "draft"]] || TrueQ[Lookup[json, "prerelease"]] || !ListQ[versionNumbers[tag]], Return[$Failed]];
   version = StringReplace[tag, StartOfString ~~ "v" -> ""];
   paclets = Cases[Lookup[json, "assets", {}], KeyValuePattern["browser_download_url" -> url_String] /;
       StringMatchQ[url, RegularExpression[StringReplace[$repositoryURL, "." -> "\\."] <> "/releases/download/" <>
          StringReplace[tag, "." -> "\\."] <> "/[A-Za-z0-9._-]+\\.paclet"]] :> url];
   If[paclets === {}, $Failed,
    <|"Version" -> version, "Paclet" -> First[paclets], "Page" -> $repositoryURL <> "/releases/tag/" <> tag|>]];
latestReleaseOf[_] := $Failed;
installLine[release_Association] := "PacletInstall[\"" <> release["Paclet"] <> "\"]";

(* the third line of the banner: the newer version, a link to its page and the line that installs it, as code to copy
   (its hyphens not drawn as dashes: "OperatorSubstitution" -> False); in a script, the same as text *)
updateNotice[release_Association] := With[{dot = bannerDot[], group = bannerGroup},
   Style[Row[{group[Style["version " <> release["Version"] <> " is available", Bold, bannerBlue[]], dot],
      group[Hyperlink["what is new", release["Page"]], dot],
      With[{url = release["Paclet"]}, Style[Defer[PacletInstall[url]], ShowStringCharacters -> True,
        PrivateFontOptions -> {"OperatorSubstitution" -> False}]]}], 11, bannerGrey[]]];
updateNoticeText[release_Association] :=
  "FunctionalDiagrams " <> release["Version"] <> " is available (" <> $FunctionalDiagramsVersion <> " is loaded): " <>
   installLine[release];

(* whether the check at load is due today: the file LastUpdateCheck.txt of dir holds the date of the last check; the
   date of today is written into it before asking. Any error reading or writing it: the check is due *)
updateCheckDueQ[dir_String, today_String] := Module[{file = FileNameJoin[{dir, "LastUpdateCheck.txt"}], last, stream},
   last = Quiet[ReadString[file]];
   If[StringQ[last] && StringTrim[last] === today, Return[False]];
   Quiet[If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    stream = OpenWrite[file];
    If[Head[stream] === OutputStream, WriteString[stream, today <> "\n"]; Close[stream]]];
   True];

(* the asynchronous request at load; its handler writes the banner with the notice into the cell tagged tag of the
   notebook nb, if the latest release is newer. Every step is quiet: a handler's messages would go to the Messages
   window *)
$updateCheckTask = None;
submitUpdateCheck[nb_, tag_String] := Quiet[
   $updateCheckTask = URLSubmit[latestReleaseRequest[],
     HandlerFunctions -> <|"TaskFinished" -> (showUpdateNotice[nb, tag, #] &)|>,
     HandlerFunctionsKeys -> {"StatusCode", "Body"}, Sequence @@ networkOptions[URLSubmit, {TimeConstraint -> 30}]]];
showUpdateNotice[nb_, tag_String, answer_] := Quiet[Module[{release, cells},
    release = If[Lookup[answer, "StatusCode"] === 200, latestReleaseOf[Lookup[answer, "Body"]], $Failed];
    If[AssociationQ[release] && newerVersionQ[release["Version"], $FunctionalDiagramsVersion],
     cells = Cells[nb, CellTags -> tag];
     If[ListQ[cells] && Length[cells] == 1,
      NotebookWrite[First[cells], ReplacePart[NotebookRead[First[cells]], 1 -> BoxData[ToBoxes[loadBanner[release]]]]]]]]];

FunctionalDiagramsUpdateCheck::latest = "The latest release could not be read from GitHub (`1`).";
FunctionalDiagramsUpdateCheck[] := Module[{response, status, release},
   response = Quiet[URLRead[latestReleaseRequest[], Sequence @@ networkOptions[URLRead, {TimeConstraint -> 10}]]];
   status = If[Head[response] === HTTPResponse, response["StatusCode"], None];
   release = If[status === 200, latestReleaseOf[response["Body"]], $Failed];
   If[!AssociationQ[release],
    Message[FunctionalDiagramsUpdateCheck::latest, Which[status === None, "no answer", status =!= 200,
       "status " <> ToString[status], True, "no release with a paclet in the answer"]];
    $Failed,
    If[newerVersionQ[release["Version"], $FunctionalDiagramsVersion],
     Print[If[TrueQ[$Notebooks], Style[updateNotice[release], FontFamily -> "Source Sans Pro"], updateNoticeText[release]]]];
    <|"InstalledVersion" -> $FunctionalDiagramsVersion, "LatestVersion" -> release["Version"],
     "InstallLine" -> installLine[release]|>]];

(* the check at load is on unless $FunctionalDiagramsUpdateCheck was set before loading (kept by the start of the
   package, 00_header.wl) *)
If[!ValueQ[$FunctionalDiagramsUpdateCheck], $FunctionalDiagramsUpdateCheck = True];

(* in a notebook (not in scripts, which stay quiet), the banner, and the update check when it is due *)
If[TrueQ[$Notebooks],
  If[$FunctionalDiagramsUpdateCheck =!= False && $AllowInternet =!= False &&
     updateCheckDueQ[$updateCheckDirectory, updateCheckDate[]],
   With[{tag = "FunctionalDiagramsBanner" <> ToString[Round[10^6 AbsoluteTime[]]]},
    CellPrint[ExpressionCell[loadBanner[], "Print", CellTags -> tag]];
    submitUpdateCheck[EvaluationNotebook[], tag]],
   Print[loadBanner[]]]];

End[];
EndPackage[];
