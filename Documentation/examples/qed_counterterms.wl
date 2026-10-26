delta2 = Coefficient[sigmaPole, DiracProduct[SlashedMomentum[p]]];
deltaM = -Coefficient[sigmaPole, DiracProduct[]];
{delta2, Expand[deltaM/m - delta2]}