gammaMeans = EffectiveAction2PI[Zf, CoulombVertex, 2, G0 -> G,
   "MeanFields" -> {psi -> Psi, psibar -> Psibar}];
{SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaMeans, Psibar[x],
    Psi[y]]] -
   (OperatorKernel[G0^(-1)][x, y] - FunctionalDerivative[Phi, G[y, x]])],
 SimplifyDiagrams[SetSourcesToZero[FunctionalDerivative[gammaMeans,
    G[x4, x3], Psibar[x1], Psi[x2]]] +
   FunctionalDerivative[Phi, G[x4, x3], G[x2, x1]]]}