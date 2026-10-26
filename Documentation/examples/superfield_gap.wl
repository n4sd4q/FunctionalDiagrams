sigmaNambu = -2 FunctionalDerivative[PhiNambu, NambuPropagator[a, b]];
SuperfieldComponents[OrderPart[sigmaNambu, NambuVertex, 1],
  {a -> psibar[x1], b -> psibar[x2]}, nambuRules]