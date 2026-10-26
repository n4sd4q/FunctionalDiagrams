Table[EvaluateMatsubara[bubble, {b -> {q, I nu}, a -> {-q, -I nu}}, rules,
  "SpinDegeneracy" -> 2, "LoopMomenta" -> {p}, "Limit" -> limit],
  {limit, {"Static", "Dynamic"}}]