{sa, sb} = {Coefficient[sigmaOneLoop, DiracProduct[]]/m, Coefficient[sigmaOneLoop,
     DiracProduct[SlashedMomentum[p]]]} /. ScalarProduct[p, p] -> p2;
deltaZ2 = ExpandParameterIntegrals[
   (sb + 2 m^2 DifferentiateUnderIntegral[sa + sb, p2]) /. p2 -> m^2, eps, 0];
Expand[16 Pi^2 deltaZ2/e^2]