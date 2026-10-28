DeclareDirectedPropagator[Gb];
DeclareBlob[Kb, "\\Gamma^0", "Shape" -> "Box"];
DeclareBlob[Fb, "F", "Shape" -> "Box", "Fill" -> "CrossHatched"];
Kb[x1, x2, x3, x4] + BetheSalpeterLadder[Kb[x1, x2, x3, x4], Fb[x1, x2, x3, x4],
  {x1, x2, x3, x4}, "ParticleHole", Gb]