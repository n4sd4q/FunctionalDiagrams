DeclareAntisymmetric[AnomalousGreenFunction, AnomalousGreenFunctionBar];
DeclareLineStyle[AnomalousGreenFunction, "DivergingArrows"];
DeclareLineStyle[AnomalousGreenFunctionBar, "ConvergingArrows"];
nambuRules = {NambuField -> {psi, psibar},
   NambuPropagator -> {G, AnomalousGreenFunction, AnomalousGreenFunctionBar},
   NambuVertex -> CoulombVertex};
SuperfieldComponents[OrderPart[PhiNambu, NambuVertex, 1], {}, nambuRules]