gammaPhi4 = EffectiveAction1PI[Z0, Phi4Vertex, 2, phi -> phiMean, D0 -> D0];
sigmaPhi4 = OrderPart[ExpandTrace[OperatorKernel[Power[D0, -1]][a, b] -
    SetSourcesToZero[FunctionalDerivative[gammaPhi4, phiMean[a],
        phiMean[b]]]], lambda, {1, 2}]