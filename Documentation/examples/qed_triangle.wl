triangle = IntegrateOneLoop[MinkowskiLoopIntegral[FeynmanDenominator[k, 0]
   FeynmanDenominator[k + p1, m] FeynmanDenominator[k + p2, m], {k}], eps];
onShell = {ScalarProduct[p1, p1] -> 1, ScalarProduct[p2, p2] -> 1,
   ScalarProduct[p1, p2] -> 1, m -> 1};
triangle /. onShell