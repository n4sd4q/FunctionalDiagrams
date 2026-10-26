DeclareSymmetric[DressedPropagator];
gammaPhi = EffectiveAction1PI[Z0, Phi4Vertex, 2, phi -> phiMean, D0 -> D0];
gamma4Phi = SetSourcesToZero[FunctionalDerivative[gammaPhi, phiMean[a], phiMean[b],
   phiMean[c], phiMean[d]]];
vertexPhi = SkeletonDiagrams[gamma4Phi, {a, b, c, d},
    {D0}] /. D0 -> DressedPropagator