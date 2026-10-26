DeclareSource[LocalPotential];
DeclareFunctionalDerivative[G0[p_, q_], LocalPotential[r_], G0[p, r] G0[r, q]];
green1 = -PerturbativeCorrelationFunction[Zf, {psi[x], psibar[y]}, CoulombVertex, 1];
rhoRho = -FunctionalDerivative[green1 /. {x -> a, y -> a}, LocalPotential[b]];
OrderPart[rhoRho, V, 1]