vertex = PerturbativeFullVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, c, d},
    D0 -> DressedPropagator];
Map[Length[DiagramTerms[#]] &, ParquetDecomposition[OrderPart[vertex, lambda, 2],
    {a, b, c, d}, {DressedPropagator},
  "Statistics" -> "Boson"]]