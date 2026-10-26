DeclareSource[phibar];
oneLoop = 1/2 TraceLog[D0^-1 +
    LabelFunction[lambda/2 phibar[u]^2 DeltaFunction[u, v], {u, v}]];
ExpandTrace[SetSourcesToZero[FunctionalDerivative[oneLoop,
    phibar[a], phibar[b], phibar[c], phibar[d]]], D0, 2]