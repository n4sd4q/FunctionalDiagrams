qedRules = {ElectronPropagator -> {"Fermion", m},
   PhotonPropagator -> {"Photon", xi}, QEDVertex -> {"Vertex", -I}};
QEDIntegrand[qedVertexFunction[{electron[x1], electronbar[x2], photon[x3]},
   {x1, x2, x3}, 1], {x2 -> p1, x3 -> p2 - p1, x1 -> -p2}, qedRules,
  "Indices" -> {x3 -> mu}]