bareVertex = kernel - (kernel /. {x2 -> x4, x4 -> x2});
DiagramPlot[BetheSalpeterLadder[bareVertex, bareVertex, {x1, x2, x3, x4},
   "ParticleParticle", G], {x1, x2, x3, x4},
  "ExternalOrder" -> {"Uncrossed", {x4, x2, x3, x1}}]