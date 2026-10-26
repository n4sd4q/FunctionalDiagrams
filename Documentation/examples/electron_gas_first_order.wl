DeclareSymmetric[V]; DeclareDirectedPropagator[G0]; DeclareInteractionLine[V];
CoulombVertex = 1/2 IntegralOver[V[x, y] psibar[x] ** psibar[y] ** psi[y] ** psi[x],
    {x, y}];
-PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]}, CoulombVertex, 1]