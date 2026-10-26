lambdaRho = PerturbativeDensityVertex[Z0, Phi4Vertex, 2, {phi}, {a, b, z},
    D0 -> DressedPropagator];
gamma2 = LuttingerWardFunctional[Z0, Phi4Vertex, 2, D0 -> DressedPropagator];
objects = {"G" -> DressedPropagator, "Lambda" -> LabelFunction[lambdaRho,
    {a, b, z}], "Gamma0" ->
   LabelFunction[4 FunctionalDerivative[gamma2, DressedPropagator[a, b],
       DressedPropagator[c, d]], {a, b, c, d}]};
CheckEquation[ExactEquation["Vertex", objects, {a, b, z}, "Statistics" -> "Boson",
    "MaximumOrder" -> {lambda, 2}], lambda, 2]