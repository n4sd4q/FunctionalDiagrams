greenFunction = -PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]},
    CoulombVertex, {2}];
selfEnergy2 = AmputateExternalLegs[ParticleIrreducibleDiagrams[greenFunction, 1,
    {x, y}, {G0}], {x, y}, {G0}]