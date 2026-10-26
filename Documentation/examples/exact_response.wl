DeclareBlob[Lbox, "L", "Shape" -> "Box"];
DeclareBlob[K0box, "\\Gamma^0", "Shape" -> "Box", "Fill" -> "CrossHatched"];
Last[ExactEquation["Response", {"L" -> Lbox, "Gamma0" -> K0box, "G" -> G},
    {x1, x2, x3, x4}]]