DeclareSource[Jph]; DeclareField[ph, Jph]; DeclareSymmetric[Dph];
Zep = Zf Exp[1/2 IntegralOver[Jph[x] Dph[x, y] Jph[y], {x, y}]];
greenPhonon = -PerturbativeCorrelationFunction[Zep, {psi[x], psibar[y]},
    g IntegralOver[ph[z] psibar[z] ** psi[z], {z}], {2}];
sigmaPhonon = AmputateExternalLegs[
   ParticleIrreducibleDiagrams[greenPhonon, 1, {x, y}, {G0}], {x, y}, {G0}];
phononRules = {G0 -> {"Fermion", xi},
   Dph -> {"Boson", Function[{s, zz}, 2 Om[s]/(Om[s]^2 - zz^2)]}};
EvaluateMatsubara[sigmaPhonon, {y -> {k, I w}, x -> {-k, -I w}}, phononRules,
  "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]