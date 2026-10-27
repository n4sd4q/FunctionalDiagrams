fockDiagram = -IntegralOver[G0[x, z] G0[z, w] G0[w, y] V[z, w], {z, w}];
DiagramPlot[fockDiagram, {x, y}, "VertexLabels" -> All]