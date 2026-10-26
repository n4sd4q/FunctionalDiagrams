bubble = ConnectedCorrelationFunction[Zf, {psibar[a], psi[a], psibar[b], psi[b]},
   CoulombVertex, {0}];
lindhard = EvaluateMatsubara[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules,
  "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}]