DeclareSymmetric[Gs];
DeclareBlob[Vertex4, "\\Gamma^{(4)}", "Shape" -> "Box"];
lambda/6 IntegralOver[Gs[a, u] Gs[a, v] Gs[a, w] Vertex4[u, v, w, b], {u, v, w}]