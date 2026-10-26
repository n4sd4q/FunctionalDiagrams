DeclareField[psi, etabar]; DeclareField[psibar, eta, -1];
Zf = Exp[-IntegralOver[G0[x, y] etabar[x] ** eta[y], {x, y}]];
FieldExpectationValue[Zf, {psi[a], psibar[b]}]