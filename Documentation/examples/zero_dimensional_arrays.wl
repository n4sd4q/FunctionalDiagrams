site = {{0, pair, g, 0}, {-pair, 0, 0, g}, {-g, 0, 0, pairb}, {0, -g, -pairb, 0}};
siteVertex = Table[If[Sort[{i, j, k, l}] === {1, 2, 3, 4},
    -u Signature[{i, j, k, l}], 0], {i, 4}, {j, 4}, {k, 4}, {l, 4}];
Factor[ZeroDimensionalValue[PhiNambu, {NambuPropagator -> site,
    NambuVertex -> siteVertex}]]