DeclareSymmetric[W2];
Zw = Exp[IntegralOver[W1[x] J[x], {x}] + 1/2 IntegralOver[W2[x, y] J[x] J[y],
    {x, y}] +
    1/24 IntegralOver[W4[x] J[x]^4, {x}]];
FieldExpectationValue[Zw, {phi[a], phi[a]}]