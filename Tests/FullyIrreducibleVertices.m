(* Written by Tests/EffectiveActions.wl (RECOMPUTE_REFERENCES=1): {phi^4 to lambda^5, electron gas to V^4},
   the "FullyIrreducible" part of ParquetDecomposition of PerturbativeFullVertex. *)
{lambda*DeltaFunction[x1, x2]*DeltaFunction[x1, x3]*DeltaFunction[x1, x4] - 
  lambda^4*Gb[x1, x2]*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x3]*Gb[x2, x4]*Gb[x3, x4] + 
  (lambda^5*Gb[x1, x4]*Gb[x2, x3]*Gb[x2, x4]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]^2*
      Gb[x2, DummyVariable[1, 1]]*Gb[x3, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x4]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x2, DummyVariable[1, 1]]^2*Gb[x3, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x4]*Gb[x2, x4]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x2, DummyVariable[1, 1]]*Gb[x3, DummyVariable[1, 1]]^2, {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x3]*Gb[x2, x3]*Gb[x2, x4]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]^2*
      Gb[x2, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x3]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x2, DummyVariable[1, 1]]^2*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x2, x3]*Gb[x2, x4]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]^2*
      Gb[x3, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  lambda^5*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x3]*Gb[x2, x4]*
   IntegralOver[Gb[x1, DummyVariable[1, 1]]*Gb[x2, DummyVariable[1, 1]]*Gb[x3, DummyVariable[1, 1]]*
     Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}] + lambda^5*Gb[x1, x2]*Gb[x1, x4]*Gb[x2, x3]*Gb[x3, x4]*
   IntegralOver[Gb[x1, DummyVariable[1, 1]]*Gb[x2, DummyVariable[1, 1]]*Gb[x3, DummyVariable[1, 1]]*
     Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}] + lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x2, x4]*Gb[x3, x4]*
   IntegralOver[Gb[x1, DummyVariable[1, 1]]*Gb[x2, DummyVariable[1, 1]]*Gb[x3, DummyVariable[1, 1]]*
     Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}] + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x1, x4]*Gb[x3, x4]*IntegralOver[Gb[x2, DummyVariable[1, 1]]^2*
      Gb[x3, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x4]*Gb[x2, x3]*Gb[x2, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x3, DummyVariable[1, 1]]^2*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x4]*IntegralOver[Gb[x2, DummyVariable[1, 1]]*
      Gb[x3, DummyVariable[1, 1]]^2*Gb[x4, DummyVariable[1, 1]], {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x2, x3]*Gb[x3, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x2, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]]^2, {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x2, x3]*Gb[x2, x4]*IntegralOver[Gb[x1, DummyVariable[1, 1]]*
      Gb[x3, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]]^2, {DummyVariable[1, 1]}])/2 + 
  (lambda^5*Gb[x1, x2]*Gb[x1, x3]*Gb[x1, x4]*Gb[x2, x3]*IntegralOver[Gb[x2, DummyVariable[1, 1]]*
      Gb[x3, DummyVariable[1, 1]]*Gb[x4, DummyVariable[1, 1]]^2, {DummyVariable[1, 1]}])/2, 
 IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 4]]*G[DummyVariable[1, 1], x2]*
    G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*G[DummyVariable[1, 4], DummyVariable[1, 1]]*
    V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 3]]*V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 1]], 
   {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  IntegralOver[G[x1, DummyVariable[1, 2]]*G[x3, DummyVariable[1, 3]]*G[DummyVariable[1, 1], x2]*
    G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*G[DummyVariable[1, 4], DummyVariable[1, 1]]*
    V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 3]]*V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 1]], 
   {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  IntegralOver[G[x1, DummyVariable[1, 1]]*G[x3, DummyVariable[1, 3]]*G[DummyVariable[1, 1], x2]*
    G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*G[DummyVariable[1, 4], DummyVariable[1, 2]]*
    V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 3]]*V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 1]], 
   {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  IntegralOver[G[x1, DummyVariable[1, 1]]*G[x3, DummyVariable[1, 3]]*G[DummyVariable[1, 1], x2]*
    G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*G[DummyVariable[1, 4], DummyVariable[1, 2]]*
    V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 4]]*V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 1]], 
   {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 4]]*
     V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 4]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 4]]*
    V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 1]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 4]]*
    V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 2]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
    V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 1]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
    V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 1]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 4]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 4]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 4]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 2]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 2]]*G[x3, DummyVariable[1, 3]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 4]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 1]]*G[x3, DummyVariable[1, 3]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 4]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 1]]*G[x3, DummyVariable[1, 3]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 4]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 1]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 4]]*
    V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 2]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 1]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 4]]*V[x4, DummyVariable[1, 3]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 2]]*G[x3, DummyVariable[1, 3]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 2]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
    V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 1]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 3]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 2]]*G[x3, DummyVariable[1, 3]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
    V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x4]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 3]]*
     V[x3, DummyVariable[1, 2]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 2]]*V[x2, DummyVariable[1, 1]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 4]]*
    G[DummyVariable[1, 1], x2]*G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
    G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
    V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
    DummyVariable[1, 3], DummyVariable[1, 4]}] + G[x1, x2]*G[x3, x4]*
   IntegralOver[G[DummyVariable[1, 1], DummyVariable[1, 3]]*G[DummyVariable[1, 2], DummyVariable[1, 4]]*
     G[DummyVariable[1, 3], DummyVariable[1, 2]]*G[DummyVariable[1, 4], DummyVariable[1, 1]]*V[x1, DummyVariable[1, 1]]*
     V[x2, DummyVariable[1, 2]]*V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], 
    {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  IntegralOver[G[x1, DummyVariable[1, 3]]*G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
    G[DummyVariable[1, 2], x4]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*G[DummyVariable[1, 4], DummyVariable[1, 1]]*
    V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], 
   {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x2]*G[x3, x4]*IntegralOver[G[DummyVariable[1, 1], DummyVariable[1, 4]]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - G[x1, x4]*G[x3, x2]*
   IntegralOver[G[DummyVariable[1, 1], DummyVariable[1, 3]]*G[DummyVariable[1, 2], DummyVariable[1, 1]]*
     G[DummyVariable[1, 3], DummyVariable[1, 4]]*G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*
     V[x2, DummyVariable[1, 2]]*V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], 
    {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 3]]*G[DummyVariable[1, 3], DummyVariable[1, 4]]*
     G[DummyVariable[1, 4], DummyVariable[1, 2]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x3, x2]*IntegralOver[G[x1, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 2]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 1]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - G[x1, x4]*G[x3, x2]*
   IntegralOver[G[DummyVariable[1, 1], DummyVariable[1, 2]]*G[DummyVariable[1, 2], DummyVariable[1, 4]]*
     G[DummyVariable[1, 3], DummyVariable[1, 1]]*G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*
     V[x2, DummyVariable[1, 2]]*V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], 
    {DummyVariable[1, 1], DummyVariable[1, 2], DummyVariable[1, 3], DummyVariable[1, 4]}] - 
  G[x1, x4]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x2]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] + 
  G[x1, x2]*IntegralOver[G[x3, DummyVariable[1, 1]]*G[DummyVariable[1, 1], x4]*
     G[DummyVariable[1, 2], DummyVariable[1, 4]]*G[DummyVariable[1, 3], DummyVariable[1, 2]]*
     G[DummyVariable[1, 4], DummyVariable[1, 3]]*V[x1, DummyVariable[1, 1]]*V[x2, DummyVariable[1, 2]]*
     V[x3, DummyVariable[1, 3]]*V[x4, DummyVariable[1, 4]], {DummyVariable[1, 1], DummyVariable[1, 2], 
     DummyVariable[1, 3], DummyVariable[1, 4]}] - DeltaFunction[x1, x4]*DeltaFunction[x2, x3]*V[x1, x2] + 
  DeltaFunction[x1, x2]*DeltaFunction[x3, x4]*V[x1, x3]}
