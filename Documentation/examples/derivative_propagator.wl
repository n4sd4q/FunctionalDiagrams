DeclareDirectedPropagator[G];
PhiHF = 1/2 IntegralOver[V[u, v] G[u, u] G[v, v],
    {u, v}] - 1/2 IntegralOver[V[u, v] G[u, v] G[v, u], {u, v}];
FunctionalDerivative[PhiHF, G[y, x]]