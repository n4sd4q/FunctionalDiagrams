DeclareSource[Jphoton]; DeclareGrassmannSource[eta, etabar];
DeclareField[photon, Jphoton]; DeclareField[electron, etabar];
DeclareField[electronbar, eta, -1];
DeclareSymmetric[PhotonPropagator]; DeclareLineStyle[PhotonPropagator, "Wavy"];
DeclareDirectedPropagator[ElectronPropagator]; DeclareLocalVertex[QEDVertex];
qedZ0 = Exp[1/2 IntegralOver[Jphoton[x] PhotonPropagator[x, y] Jphoton[y], {x, y}] +
    IntegralOver[ElectronPropagator[x, y] etabar[x] ** eta[y], {x, y}]];
qedInteraction = e IntegralOver[QEDVertex[x, y, z] electronbar[x] ** electron[y]
    photon[z], {x, y, z}];
qedLines = {PhotonPropagator, ElectronPropagator};
qedVertexFunction[fields_, labels_, n_] := ParticleIrreducibleDiagrams[
   AmputateExternalLegs[ConnectedCorrelationFunction[qedZ0, fields, qedInteraction,
     n, "CouplingOrder" -> {e, n}], labels, qedLines], 1, labels, qedLines];
sigmaQED = qedVertexFunction[{electron[x], electronbar[y]}, {x, y}, 2]