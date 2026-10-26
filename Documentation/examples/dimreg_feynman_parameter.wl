e1 = Coefficient[EpsilonExpand[BubbleIntegral[{1, 1}, 5, {1, 2}, eps], eps, 1],
   eps, 1];
{Cases[e1, _FeynmanParameterIntegral, Infinity], N[e1 /. RenormalizationScale -> 1]}