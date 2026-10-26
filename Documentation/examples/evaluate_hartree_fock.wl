rules = {G0 -> {"Fermion", xi}, G -> {"Fermion", xi}, V -> {"Instantaneous", v}};
EvaluateMatsubara[OrderPart[sigmaG, V, 1],
  {y -> {k, I w}, x -> {-k, -I w}}, rules, "SpinDegeneracy" -> 2]