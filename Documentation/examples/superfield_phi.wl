DeclareGrassmannSource[NambuSource]; DeclareField[NambuField, NambuSource];
DeclareAntisymmetric[BareNambuPropagator, NambuPropagator, NambuVertex];
DeclareLocalVertex[NambuVertex];
Znambu = Exp[1/2 IntegralOver[
    BareNambuPropagator[a, b] NambuSource[a] ** NambuSource[b], {a, b}]];
nambuInteraction = 1/24 IntegralOver[NambuVertex[a, b, c, d]
    NambuField[a] ** NambuField[b] ** NambuField[c] ** NambuField[d], {a, b, c, d}];
PhiNambu = LuttingerWardFunctional[Znambu, nambuInteraction, 2,
  BareNambuPropagator -> NambuPropagator]