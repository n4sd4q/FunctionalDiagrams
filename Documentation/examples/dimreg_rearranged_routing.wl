routed = InfraredRearrangedIntegral[OrderPart[gamma4, lambda, 3], {a, b, c, d}, p,
   massless, eps, "Routing" -> True];
Tally[Simplify[(16 Pi^2)^2 PolePart[#[[3]], eps]/lambda^3 /.
     Log[p^2/RenormalizationScale^2] -> L] & /@ routed]