shapes = SimplifyDiagrams[IntegralOver[
    ConnectedFunctionFromVertices[{x1, x2, x3, x4, x5}, "Boson"],
   {x1, x2, x3, x4, x5}]];
{# /. IntegralOver[__] -> 1, Count[#, _VertexFunction, Infinity]} & /@
  DiagramTerms[shapes]