DeclareSource[Jph]; DeclareField[phonon, Jph]; DeclareSymmetric[Dph];
Zep = Zf Exp[1/2 IntegralOver[Jph[x] Dph[x, y] Jph[y], {x, y}]];
IntegrateOutField[{Zep, g IntegralOver[phonon[z] psibar[z] ** psi[z], {z}]}, phonon,
    Jph]["Interaction"]