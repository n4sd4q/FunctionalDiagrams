DeclareBlob[Irr, "\\Gamma_{\\rm irr}", "Shape" -> "Box", "Fill" -> "Dotted"];
DeclareBlob[Gph, "\\Gamma_{\\rm ph}", "Shape" -> "Box"];
DeclareBlob[Gphbar, "\\Gamma_{\\overline{\\rm ph}}", "Shape" -> "Box"];
Irr[x1, x2, x3, x4] + BetheSalpeterLadder[Gph[x1, x2, x3, x4], Fb[x1, x2, x3, x4],
  {x1, x2, x3, x4}, "ParticleHole", Gb] + BetheSalpeterLadder[Gphbar[x1, x2, x3, x4],
  Fb[x1, x2, x3, x4], {x1, x2, x3, x4}, "TransverseParticleHole", Gb]