piMuNu = DiracSimplify[-I IntegrateOneLoop[piIntegrand, eps]];
{piG, piQQ} = {Coefficient[piMuNu, LorentzMetric[mu, nu]],
   Coefficient[piMuNu, FourVector[q, mu] FourVector[q, nu]]};
Simplify[ReduceParameterIntegrals[piG + ScalarProduct[q, q] piQQ] /.
  ParameterIntegral[f_, x_] :> ParameterIntegral[FullSimplify[f], x]]