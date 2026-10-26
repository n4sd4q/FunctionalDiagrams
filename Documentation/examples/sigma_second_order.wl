sigma2 = EvaluateMatsubara[OrderPart[sigmaG, V, 2],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, "SpinDegeneracy" -> 2,
  "LoopMomenta" -> {p1, p2}]