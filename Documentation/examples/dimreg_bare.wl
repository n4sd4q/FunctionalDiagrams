sigmaB = InfraredRearrangedIntegral[sigmaPhi4, {a, b}, p, massless, eps];
gamma4B = InfraredRearrangedIntegral[gamma4, {a, b, c, d}, p, massless, eps];
sigma21B = InfraredRearrangedIntegral[sigma21, {a, b, u}, p, massless, eps];
Expand[PolePart[sigma21B, eps] /. lambda -> 16 Pi^2 lambda]