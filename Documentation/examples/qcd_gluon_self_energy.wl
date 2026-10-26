DeclareSource[Jgluon]; DeclareGrassmannSource[chi, chibar, sigma, sigmabar];
DeclareField[gluon, Jgluon]; DeclareField[quark, chibar];
DeclareField[quarkbar, chi, -1]; DeclareField[ghost, sigmabar];
DeclareField[ghostbar, sigma, -1];
DeclareSymmetric[Dg, V3, V4]; DeclareLocalVertex[Vq, V3, V4, Vc];
DeclareLineStyle[Dg, "Curly"]; DeclareLineStyle[Dc, "DottedArrow"];
DeclareDirectedPropagator[Sq];
Zqcd = Exp[1/2 IntegralOver[Jgluon[x] Dg[x, y] Jgluon[y], {x, y}] +
    IntegralOver[Sq[x, y] chibar[x] ** chi[y], {x, y}] +
    IntegralOver[Dc[x, y] sigmabar[x] ** sigma[y], {x, y}]];
qcd = IntegralOver[g Vq[x, y, z] quarkbar[x] ** quark[y] gluon[z] +
     g Vc[x, y, z] ghostbar[x] ** ghost[y] gluon[z] +
     g/6 V3[x, y, z] gluon[x] gluon[y] gluon[z], {x, y, z}] +
   g^2/24 IntegralOver[V4[w, x, y, z] gluon[w] gluon[x] gluon[y] gluon[z],
     {w, x, y, z}];
lines = {Dg, Sq, Dc};
correlation = OrderPart[ConnectedCorrelationFunction[Zqcd, {gluon[a], gluon[b]},
    qcd, 2], g, 2];
ParticleIrreducibleDiagrams[AmputateExternalLegs[correlation, {a, b}, lines], 1,
  {a, b}, lines]