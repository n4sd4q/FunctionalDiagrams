deltaGamma = -I/e IntegrateOneLoop[vertexIntegrand, eps];
chain = DiracSimplify[SpinorChain[{p2, m}, deltaGamma, {p1, m}]] /.
   ScalarProduct[p1, p2] -> m^2 - q2/2;
{f1, f2, f3} = VertexFormFactors[chain, mu, {p1, p2, m}];
ExpandParameterIntegrals[f2 /. q2 -> 0, eps, 0]