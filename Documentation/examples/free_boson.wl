DeclareSymmetric[D0]; DeclareField[phi, J];
Z0 = Exp[1/2 IntegralOver[J[x] D0[x, y] J[y], {x, y}]];
FieldDerivative[Z0, {phi[a], phi[b]}]