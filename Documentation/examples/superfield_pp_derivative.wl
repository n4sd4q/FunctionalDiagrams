PhiComponents = SuperfieldComponents[PhiNambu, {}, nambuRules];
ppKernel = -4 FunctionalDerivative[PhiComponents, AnomalousGreenFunction[x2, x4],
     AnomalousGreenFunctionBar[x1, x3]] /. {AnomalousGreenFunction[__] -> 0,
     AnomalousGreenFunctionBar[__] -> 0};
OrderPart[ppKernel, V, 1]