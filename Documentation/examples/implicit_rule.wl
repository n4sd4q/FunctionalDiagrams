DeclareSymmetric[PropagatorOfJ, V];
DeclareFunctionalDerivative[PropagatorOfJ[p_, q_], J[r_],
    PropagatorOfJ[p, r] PropagatorOfJ[r, q]];
FunctionalDerivative[IntegralOver[V[u, v] PropagatorOfJ[u, v], {u, v}], J[a], J[b]]