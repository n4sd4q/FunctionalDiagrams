kappa = 1/(16 Pi^2);
z[c_] := 1 + c[1] lambda kappa/eps + lambda^2 kappa^2 (c[2]/eps^2 + c[3]/eps);
{zPhi, zLambda, zM2} = {z[zf], z[zl], z[zm]};
upTo[e_, n_] := Sum[Coefficient[e, lambda, j] lambda^j, {j, 0, n}];
finite = {upTo[zPhi (p^2 - (sigmaB /. lambda -> lambda zLambda)), 2],
   upTo[zPhi^2 (gamma4B /. lambda -> lambda zLambda), 3],
   upTo[zM2 (1 - (sigma21B /. lambda -> lambda zLambda)), 2]};
equations = Flatten[CoefficientList[Expand[eps^4 PolePart[#, eps]] /.
      Log[p^2/RenormalizationScale^2] -> L, {lambda, eps, L, p}] & /@ finite];
solution = First[Solve[Thread[DeleteCases[equations, 0] == 0],
   Flatten[Table[c[i], {c, {zf, zl, zm}}, {i, 3}]]]]