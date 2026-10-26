sigmaOneLoop = DiracSimplify[I IntegrateOneLoop[sigmaIntegrand, eps]];
sigmaPole = ExpandParameterIntegrals[sigmaOneLoop, eps, -1];
Expand[16 Pi^2 eps sigmaPole/e^2]