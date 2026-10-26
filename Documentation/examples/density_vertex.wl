sigma1 = AmputateExternalLegs[ParticleIrreducibleDiagrams[green1, 1, {x, y}, {G0}],
   {x, y}, {G0}];
dSigma = FunctionalDerivative[sigma1 /. {x -> x1, y -> x2}, LocalPotential[z]];
lambdaU = DeltaFunction[x1, z] DeltaFunction[x2, z] +
   (SkeletonDiagrams[dSigma, {x1, x2, z}, {G0}] /. G0 -> G)