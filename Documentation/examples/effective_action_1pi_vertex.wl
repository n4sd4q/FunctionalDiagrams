gamma1PI = EffectiveAction1PI[Z0, Phi4Vertex, 1, phi -> phiMean, D0 -> Dm];
OrderPart[ExpandTrace[SetSourcesToZero[FunctionalDerivative[gamma1PI,
    phiMean[a], phiMean[b]]] /. Dm -> D0], lambda, {0, 1}]