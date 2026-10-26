gammaS = 4 FunctionalDerivative[PhiNambu, NambuPropagator[a, b],
   NambuPropagator[c, d]];
channels = <|"s" -> gammaS, "t" -> (-gammaS /. {b -> c, c -> b}),
   "u" -> (gammaS /. {b -> d, c -> b, d -> c})|>;
shortcut = IrreducibleVertex[PhiNambu, NambuPropagator, {a, b, c, d},
   "Statistics" -> "Superfield", "Channel" -> All];
{Length[DiagramTerms[OrderPart[#, NambuVertex, 2]]] & /@ channels,
 Table[SimplifyDiagrams[channels[k] - shortcut[k]], {k, {"s", "t", "u"}}]}