
(* ========================================================================= *)
(*  15. TikZ export                                                          *)
(*  The same geometry as DiagramPlot, written as TikZ commands. Required:   *)
(*  \usetikzlibrary{decorations.pathmorphing} (straight zigzag lines).       *)
(* ========================================================================= *)

$greekNames = {"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu", "nu",
   "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi",
   "Sigma", "Phi", "Psi", "Omega"};

(* x12 -> x_{12}, lambda -> \lambda, lambda2 -> \lambda_{2}, etabar -> \bar{\eta}, G0 -> G_{0} *)
splitTrailingDigits[s_String] := Module[{c = Characters[s], k},
   k = Length[c]; While[k > 0 && DigitQ[c[[k]]], k--];
   If[k == 0 || k == Length[c], {s, ""}, {StringJoin[Take[c, k]], StringJoin[Drop[c, k]]}]];
latexBaseName[b_String] := Which[
   MemberQ[$greekNames, b], "\\" <> b,
   StringLength[b] > 3 && StringTake[b, -3] === "bar" && (MemberQ[$greekNames, StringDrop[b, -3]] || StringLength[b] == 4),
   "\\bar{" <> latexBaseName[StringDrop[b, -3]] <> "}",
   StringLength[b] > 1, "\\mathrm{" <> b <> "}",
   True, b];
(* a name is cut at its first $ (the local symbols of Module, x$123, are written x) *)
cutAtDollar[s_String] := With[{p = Position[Characters[s], "$", {1}, 1]}, If[p === {} || p[[1, 1]] == 1, s, StringTake[s, p[[1, 1]] - 1]]];
latexName[s0_String] := With[{parts = splitTrailingDigits[cutAtDollar[s0]]},
   With[{base = latexBaseName[parts[[1]]]}, If[parts[[2]] === "", base, base <> "_{" <> parts[[2]] <> "}"]]];

latexLabel[DummyVariable[_, k_], rules_] := "z_{" <> ToString[k] <> "}";
latexLabel[x_, rules_] := With[{r = Select[rules, #[[1]] === x &]}, Which[
    r =!= {}, ToString[r[[1, 2]]],
    Head[x] === Symbol, latexSymbolString[x],
    IntegerQ[x], ToString[x],
    True, latexName[ToString[x]]]];

latexNumber[r_Integer] := ToString[r];
latexNumber[r_Rational] := "\\frac{" <> ToString[Numerator[r]] <> "}{" <> ToString[Denominator[r]] <> "}";
(* a complex number: i, -i, 2\,i, \left(1+2\,i\right) *)
latexNumber[z_Complex] := With[{re = Re[z], im = Im[z]}, With[{ims = If[Abs[im] == 1, "i", latexNumber[Abs[im]] <> "\\,i"]},
    Which[re == 0, If[im < 0, "-", ""] <> ims, True, "\\left(" <> latexNumber[re] <> If[im < 0, "-", "+"] <> ims <> "\\right)"]]];
latexNumber[r_] := If[NumberQ[r] && r < 0, "-" <> latexNumber[-r], ToString[r]];
(* a numerical coefficient as {sign, magnitude, factor i}: -2 I -> {-1, 2, "i"}, (1 + I)/2 -> {1, 1, "\left(...\right)"} *)
coefficientParts[c_] := Which[
   Im[c] == 0, {If[c < 0, -1, 1], Abs[c], ""},
   Re[c] == 0, {If[Im[c] < 0, -1, 1], Abs[Im[c]], "i"},
   True, {1, 1, latexNumber[c]}];
(* the factors of a product that are not numbers *)
nonNumericPart[w_] := Which[NumberQ[w], 1, Head[w] === Times, Times @@ Select[List @@ w, ! NumberQ[#] &], True, w];
(* the constants Pi and E (I is a number) *)
latexSymbolString[s_Symbol] := Which[s === Pi, "\\pi", s === E, "e", True, latexName[SymbolName[s]]];

(* LaTeX of a weight: rational number times a product of powers of symbols; anything else via TeXForm *)
latexMonomialFactor[s_Symbol] := latexSymbolString[s];
latexMonomialFactor[Power[s_Symbol, n_Integer]] := latexSymbolString[s] <> "^{" <> ToString[n] <> "}";
latexMonomialFactor[f_] := "(" <> ToString[TeXForm[f]] <> ")";
latexWeight[w_] := Module[{parts = coefficientParts[numericFactor[w]], rest = nonNumericPart[w], numberStrings},
   numberStrings = Join[If[parts[[2]] === 1 && parts[[3]] =!= "", {}, {latexNumber[parts[[2]]]}], If[parts[[3]] === "", {}, {parts[[3]]}]];
   {parts[[1]], If[rest === 1, joinStrings[numberStrings, "\\,"],
     joinStrings[Join[If[numberStrings === {"1"}, {}, numberStrings],
       latexMonomialFactor /@ If[Head[rest] === Times, List @@ rest, {rest}]], "\\,"]]}];

(* a number with three decimals, written out in full (no scientific notation) *)
numberString[x_] := With[{m = Round[1000 N[x]]}, With[{a = Abs[m]}, With[{f = Mod[a, 1000]},
     StringJoin[If[m < 0, "-", ""], ToString[Quotient[a, 1000]], Which[f >= 100, ".", f >= 10, ".0", True, ".00"], ToString[f]]]]];
coordinateString[{x_, y_}] := "(" <> numberString[x] <> "," <> numberString[y] <> ")";
(* the same for the points of a polyline, the numbers rounded all at once *)
coordinateStrings[pts_List] := If[pts === {}, {}, With[{m = Round[1000 N[pts]]}, With[{a = Abs[m]}, With[{q = Quotient[a, 1000], f = Mod[a, 1000]},
     MapThread[StringJoin["(", If[#1 < 0, "-", ""], ToString[#3], Which[#5 >= 100, ".", #5 >= 10, ".0", True, ".00"], ToString[#5], ",",
        If[#2 < 0, "-", ""], ToString[#4], Which[#6 >= 100, ".", #6 >= 10, ".0", True, ".00"], ToString[#6], ")"] &,
      {m[[All, 1]], m[[All, 2]], q[[All, 1]], q[[All, 2]], f[[All, 1]], f[[All, 2]]}]]]]];
degreesString[a_] := numberString[a 180./Pi];

(* lines drawn with tikzPathString are segments and arcs: balloons (self-loops of waves and coils) are drawn through
   computed points by tikzLine *)
tikzPathString[{"segment", u_, v_}] := coordinateString[u] <> " -- " <> coordinateString[v];
(* the start angle is reduced to [0, 360) degrees; the arc is the same *)
tikzPathString[{"arc", c_, r_, a0_, da_}] := With[{a = a0 - 2 Pi Floor[a0/(2 Pi)]}, coordinateString[c + r {Cos[a0], Sin[a0]}] <> " arc[start angle=" <>
    degreesString[a] <> ", end angle=" <> degreesString[a + da] <> ", radius=" <> numberString[r] <> "]"];

(* the line width in pt: "LineWidth" (Automatic: 0.7) *)
tikzLineWidth[opts_List] := With[{w = drawingOption[opts, "LineWidth"]}, If[NumberQ[w] && w > 0, N[w], 0.7]];
(* the length of a unit in cm: "Scale" (anything but a positive number: the default, 0.7) *)
tikzScale[opts_List] := With[{s = drawingOption[opts, "Scale"]}, If[NumberQ[s] && Im[s] == 0 && s > 0, N[s], 0.7]];

(* an arrow head on a path, centred at the parameter t and pointing along the path: a filled stealth shape, 5.5 lw long and
   4.7 lw wide (times size) (in pt, converted to units of the picture). It is a polygon, not a decoration: the markings decoration of
   TikZ fails (Dimension too large) on small arcs *)
tikzArrowHead[path_, t_, scale_, lw_ : 0.7, size_ : 1.] := Module[{q = pathPoints[path, {t - 0.01, t, t + 0.01}, 0.], m, d, n, u, len, wid, back},
   m = q[[2]]; d = unitVectorOf[q[[3]] - q[[1]]]; n = rotate90[d];
   u = 0.0351459804/scale; len = 5.5 size lw u; wid = 4.7 size lw u; back = m - len/2 d;
   "  \\fill " <> coordinateString[m + len/2 d] <> " -- " <> coordinateString[back + wid/2 n] <> " -- " <>
    coordinateString[back + 0.325 len d] <> " -- " <> coordinateString[back - wid/2 n] <> " -- cycle;\n"];

(* waves through computed points: one smooth curve of amplitude amp, nw waves and 8 points per wave, for each offset *)
tikzWaves[path_, nw_, amp_, offsets_List, crossings_List : {}] := With[{ts = N[Range[0, Round[8 nw]]]/Round[8 nw]},
   With[{env = waveEnvelope[ts, crossings, 0.9/nw]},
    StringJoin[Table["  \\draw plot[smooth, tension=0.7] coordinates {" <>
       joinStrings[coordinateStrings[pathPoints[path, ts, ds + amp env Sin[2. Pi nw ts]]], " "] <> "};\n", {ds, offsets}]]]];
(* the amplitude of the waves along a line, 1 but near the parameters cs where it crosses another wavy line: there it
   falls smoothly to 0 within w on either side, so that the crossing reads as two lines crossing, not as a knot *)
waveEnvelope[ts_List, cs_List, w_] := If[cs === {}, Table[1., {Length[ts]}],
   Table[Times @@ Table[With[{x = Min[1., Abs[t - c]/w]}, x^2 (3 - 2 x)], {c, cs}], {t, ts}]];
(* the parameters (fractions of its length) at which each line of es crosses a wavy line, away from the ends of both *)
$crossingWaveStyles = {"Wavy", "DoubleWavy"};
waveCrossingParameters[es_List] := Module[{n = 32, polys},
   polys = Table[If[MemberQ[$crossingWaveStyles, e[[3]]], pathPoints[e[[2]], N[Range[0, n]]/n, 0.], None], {e, es}];
   Table[If[polys[[i]] === None, {},
     Flatten[Table[If[j == i || polys[[j]] === None, {}, polylineCrossings[polys[[i]], polys[[j]], n]], {j, Length[es]}]]], {i, Length[es]}]];
(* the parameters along p where the polylines p and q (n segments each) cross, between 0.1 and 0.9 of both. Only the
   pairs of segments that can give such a parameter (the segments i with i > 0.1 n and i - 1 < 0.9 n) and whose bounding
   boxes meet (within 10^-9) are computed: the boxes of all the pairs at once, by Outer. Both are necessary conditions for
   a crossing counted below, so the result is the same as with every pair *)
polylineCrossings[p_, q_, n_] := Module[{out = {}, d1, d2, den, s, t, cross2, middle, is, js, box, bp, bq, near, i, j},
   cross2[u_, v_] := u[[1]] v[[2]] - u[[2]] v[[1]];
   middle[r_] := Select[Range[Length[r] - 1], # > 0.1 n && # - 1 < 0.9 n &];
   (* the lower and upper ends of the boxes of the segments ks of r, in the coordinate c *)
   box[r_, ks_, c_] := With[{a = r[[ks, c]], b = r[[ks + 1, c]]}, {(a + b - Abs[a - b])/2, (a + b + Abs[a - b])/2}];
   is = middle[p]; js = middle[q];
   near = If[is === {} || js === {}, {},
     bp = {box[p, is, 1], box[p, is, 2]}; bq = {box[q, js, 1], box[q, js, 2]};
     Position[Sign[Times @@ Table[(1 + Sign[10.^-9 - Outer[Plus, bp[[c, 1]], -bq[[c, 2]]]]) (1 + Sign[10.^-9 + Outer[Plus, bp[[c, 2]], -bq[[c, 1]]]]),
        {c, 2}]], 1]];
   Do[i = is[[k[[1]]]]; j = js[[k[[2]]]];
    d1 = p[[i + 1]] - p[[i]]; d2 = q[[j + 1]] - q[[j]]; den = cross2[d1, d2];
    If[Abs[den] > 10.^-12,
     s = cross2[q[[j]] - p[[i]], d2]/den; t = cross2[q[[j]] - p[[i]], d1]/den;
     If[0. <= s <= 1. && 0. <= t <= 1. && 0.1 < (i - 1 + s)/n < 0.9 && 0.1 < (j - 1 + t)/n < 0.9, AppendTo[out, (i - 1 + s)/n]]],
    {k, near}];
   Module[{res = {}}, Do[If[! AnyTrue[res, Abs[# - c] < 0.02 &], AppendTo[res, c]], {c, out}]; res]];
(* a cross of half size h at q (an impurity line, a source insertion) *)
tikzCross[q_, h_] := coordinateString[q + h {1, 1}] <> " -- " <> coordinateString[q - h {1, 1}] <> " " <>
   coordinateString[q + h {1, -1}] <> " -- " <> coordinateString[q - h {1, -1}];

(* the amplitude of a TikZ wave relative to its own (0.7 for the flatter waves of a double wavy line); the pictures of
   Mathematica have $waveAmplitude *)
$tikzWaveFactor = 1.;
tikzLine[{"E", path_, style_}, t_, scale_, lw_ : 0.7, crossings_List : {}] := Module[{physical = 10 scale pathLength[path], wl = Max[1.3, Min[1.6, 2.3 scale]], nw,
    options},
   (* waves 1.6 mm long at the default scale, down to 1.3 mm in small pictures (shorter ones look like noise), and an
      amplitude in proportion *)
   nw = Max[1, Floor[physical/wl]];
   Which[
    (* gluons: a coil, on segments and arcs alike; wavy and zigzag arcs (on arcs the decorations of TikZ can fail,
       Dimension too large): curves through computed points. The dressed interaction: the wavy line drawn double, its
       two strokes 1.2 line widths apart along the normal of the wave itself (two waves shifted along the line would
       close up on the slopes), in proportion to the line width wherever the picture is copied with heavier lines; its
       waves flatter than those of a plain wavy line, so that the inner stroke keeps clear of itself at the crests *)
    style === "DoubleWavy", StringReplace[Block[{$tikzWaveFactor = 0.7}, tikzLine[{"E", path, "Wavy"}, t, scale, lw, crossings]],
     "\\draw plot" -> "\\draw[double, double distance=1.2\\pgflinewidth] plot"],
    style === "Curly", "  \\draw plot[smooth, tension=0.6] coordinates {" <>
     joinStrings[coordinateStrings[coilPoints[path, Max[2, nw], 0.36 wl/(10 scale), 0.4 wl/(10 scale)]], " "] <> "};\n",
    (* a wavy arc: a half-integer number of waves, so that it is mirror symmetric about its middle (it leaves both ends
       on the same side), and on a short or low arc (the Fock arc between neighbouring vertices) shorter waves of
       smaller amplitude, in proportion to the height of the arc, down to 0.95 mm: a few large waves on a small arc
       read as a lumpy hump, not as an arc *)
    path[[1]] =!= "segment" && style === "Wavy", Module[{h = 10 scale If[path[[1]] === "arc", Min[path[[3]], path[[3]] (1 - Cos[path[[5]]/2])], $bosonLoopRadius],
       wa, na},
      wa = Min[wl, Max[0.95, 0.6 h]]; na = Max[1.5, Round[physical/wa - 0.5] + 0.5];
      tikzWaves[path, na, 0.26 $tikzWaveFactor (physical/na)/(10 scale), {0.}, crossings]],
    (* a straight wavy line: the same explicit curve, a half-integer number of waves, mirror symmetric about its middle
       (the snake decoration of TikZ ends in a straight remainder at one end) *)
    style === "Wavy", Module[{na = Max[1.5, Round[physical/wl - 0.5] + 0.5]},
      tikzWaves[path, na, 0.27 $tikzWaveFactor (physical/na)/(10 scale), {0.}, crossings]],
    path[[1]] =!= "segment" && style === "Zigzag", "  \\draw " <> joinStrings[coordinateString /@ pathPoints[path, N[Range[0, 2 nw]]/(2 nw),
       0.25 wl/(10 scale) Table[If[i == 0 || i == 2 nw, 0., (-1.)^i], {i, 0, 2 nw}]], " -- "] <> ";\n",
    True,
    options = Switch[style,
      (* a straight zigzag line (wavy lines and zigzag arcs are explicit curves, above) *)
      "Zigzag", "decorate, decoration={zigzag, amplitude=" <> numberString[0.25 wl] <> "mm, segment length=" <>
       numberString[physical/nw] <> "mm}",
      (* dashes in proportion to the size of the picture *)
      "Dashed" | "Impurity", "dash pattern=on " <> numberString[3.75 scale] <> "pt off " <> numberString[3.75 scale] <> "pt",
      "Dotted" | "DottedArrow", "dotted, line width=" <> numberString[1.57 lw] <> "pt",
      "Double" | "DoubleArrow", "double, double distance=" <> numberString[1.86 lw] <> "pt",
      _, ""];
    "  \\draw" <> If[options === "", " ", "[" <> options <> "] "] <> tikzPathString[path] <> ";\n" <>
     (* on a double line (3.9 lw wide) the head is larger, so that it stands out of the two strokes *)
     If[directedStyleQ[style], tikzArrowHead[path, t, scale, lw, If[style === "DoubleArrow", 1.45, 1.]], ""] <>
     If[anomalousStyleQ[style], StringJoin[tikzArrowHead[#[[1]], #[[2]], scale, lw] & /@ anomalousArrowHeads[path, style]], ""] <>
     If[style === "Impurity", "  \\draw " <> tikzCross[First[pathPoints[path, {0.5}, 0.]], $impurityCross] <> ";\n", ""]]];

(* a declared blob: the shape filled, the hatching as explicit chords and the dots as small discs (no patterns library),
   the label, the outline. On a hatched or dotted blob the label sits in a white clearing with rounded corners, a little
   larger than the glyphs, so that no hatch line or dot runs through or touches them; the clearing is clipped to the blob
   and the outline drawn over it, so that a wide label never cuts the outline *)
tikzDeclaredBlob[p_, spec : {label_, shape_, fill_, h_}, lw_, font_, rules_] := Module[{shapeString, hatches = blobHatchesOf[p, spec],
    dots = blobDotsOf[p, spec],
    at = coordinateString[blobLabelPoint[p, spec]],
    (* the label of a triangle is smaller (blobLabelScale): the node scaled, its clearing with it *)
    scaleString = If[blobLabelScale[shape] == 1., "", ", scale=" <> numberString[blobLabelScale[shape]]]},
   shapeString = Switch[shape,
     "Box", coordinateString[p - {h, h}] <> " rectangle " <> coordinateString[p + {h, h}],
     "Triangle" | "TriangleUp" | "Diamond", joinStrings[coordinateString /@ triangleCorners[p, h, shape], " -- "] <> " -- cycle",
     _, coordinateString[p] <> " circle (" <> numberString[h] <> ")"];
   "  \\fill[" <> If[fill === "Grey", "black!20", "white"] <> "] " <> shapeString <> ";\n" <>
    If[hatches === {}, "", "  \\draw[line width=" <> numberString[0.55 lw] <> "pt] " <>
      joinStrings[Table[coordinateString[hs[[1]]] <> " -- " <> coordinateString[hs[[2]]], {hs, hatches}], " "] <> ";\n"] <>
    (* the dots 1.4 line widths across (the hatch lines are 0.55), so that they read as dots in print *)
    If[dots === {}, "", "  \\fill " <> joinStrings[Table[coordinateString[q] <> " circle (" <> numberString[0.7 lw] <> "pt)", {q, dots}], " "] <>
      ";\n"] <>
    If[texturedFillQ[fill],
     "  \\begin{scope}\n  \\clip " <> shapeString <> ";\n  \\node[font=" <> ToString[font] <> scaleString <>
      ", inner sep=1.2pt, fill=white, rounded corners=2pt] at " <> at <> " {$" <> label <> "$};\n  \\end{scope}\n" <>
      "  \\draw " <> shapeString <> ";\n",
     "  \\draw " <> shapeString <> ";\n  \\node[font=" <> ToString[font] <> scaleString <> ", inner sep=0.6pt] at " <> at <> " {$" <> label <> "$};\n"]];

tikzItems[items_, opts_List] := Module[{scale = tikzScale[opts], rules = Flatten[{drawingOption[opts, "Labels"]}], es,
    lw = tikzLineWidth[opts], font = drawingOption[opts, "LabelFont"]},
   es = Cases[items, {"E", __}];
   StringJoin[
    MapThread[tikzLine[#1, #2, scale, lw, #3] &, {es, arrowParameters[es, Cases[items, {"B", p_, spec_} :> {p, spec[[2]], spec[[4]]}]],
      waveCrossingParameters[es]}],
    Cases[items, {"L", q_, e_, direction_} :> "  \\draw " <> coordinateString[q] <> " -- " <> coordinateString[e] <> ";\n" <>
       Which[direction === "Out", tikzArrowHead[{"segment", q, e}, 0.55, scale, lw],
        direction === "In", tikzArrowHead[{"segment", e, q}, 0.55, scale, lw], True, ""]],
    Cases[items, {"S", {p_, q_}} :> If[p === q, "", "  \\draw " <> coordinateString[p] <> " -- " <> coordinateString[q] <> ";\n"] <>
       "  \\draw[fill=white] " <> coordinateString[q] <> " circle (" <> numberString[0.1] <> ");\n" <>
       "  \\draw[line width=" <> numberString[0.71 lw] <> "pt] " <> tikzCross[q, 0.07] <> ";\n"],
    Cases[items, {"N", p_, "blob"} :> "  \\draw[fill=black!22] " <> coordinateString[p] <> " circle (" <> numberString[$blobRadius] <> ");\n"],
    Cases[items, {"B", p_, spec_} :> tikzDeclaredBlob[p, spec, lw, font, rules]],
    Cases[items, {"N", p_, "dot"} :> "  \\fill " <> coordinateString[p] <> " circle (" <> numberString[0.85 $dotRadius] <> ");\n"],
    Cases[items, {"N", p_, "ext"} :> "  \\draw[fill=white] " <> coordinateString[p] <> " circle (" <> numberString[$externalPointRadius] <> ");\n"],
    (* a label is anchored on the side facing its point, so that it never overlaps it *)
    Cases[items, {"T", p_, label_, a_, angle_} :> "  \\node[font=" <> ToString[font] <> ", inner sep=0pt, anchor=" <>
       ToString[Mod[Round[angle] + 180, 360]] <> "] at " <> coordinateString[a] <> " {$" <> latexLabel[label, rules] <> "$};\n"]]];

tikzPicture[items_, opts_List] := "\\begin{tikzpicture}[x=" <> numberString[tikzScale[opts]] <> "cm, y=" <>
   numberString[tikzScale[opts]] <> "cm, line width=" <> numberString[tikzLineWidth[opts]] <>
   "pt, baseline={([yshift=-0.6ex]0,0)}]\n" <> tikzItems[items, opts] <> "\\end{tikzpicture}";

DiagramToTikZ[e_, ext_List : {}, opts___] := Module[{allOpts = drawingOptions[DiagramToTikZ, {opts}], gs, parts, output},
   gs = geometriesOf[e, ext, allOpts]; output = drawingOption[allOpts, "Output"];
   (* a term without lines (a number) has no picture *)
   parts = Table[With[{w = latexWeight[g[[1]]]}, {w[[1]], w[[2]], If[g[[2]] === {}, "", tikzPicture[g[[2]], allOpts]]}], {g, gs}];
   If[output === "List", Table[{If[p[[1]] < 0, "-", ""] <> p[[2]], p[[3]]}, {p, parts}],
    StringJoin[Table[With[{p = parts[[i]]},
       (* the signs between two pictures with some room, so that "b - a" (the last label of a picture, the first of the
          next one) does not read as one expression; the sign is an ordinary symbol with a break point before it, so that
          a line never ends with a sign *)
       If[i == 1, If[p[[1]] < 0, "-", ""], If[p[[1]] < 0, "\n\\allowbreak\\;{-}\\; ", "\n\\allowbreak\\;{+}\\; "]] <>
        Which[p[[3]] === "", p[[2]], p[[2]] === "1", If[i == 1 && p[[1]] < 0, "\\,", ""], True, p[[2]] <> "\\,"] <> p[[3]]],
      {i, Length[parts]}]]]];

(* ========================================================================= *)
(*  16. Formulas in LaTeX                                                    *)
(*  A sum of terms written as LaTeX: integrals with their variables as a     *)
(*  subscript, functions with their arguments in parentheses, delta          *)
(*  functions, Grassmann products in their order (after the commuting        *)
(*  factors), rational coefficients as fractions.                            *)
(* ========================================================================= *)

$latexNames = {}; $latexLabels = {}; $latexSubscripts = {};

(* the strings of a list joined by a separator *)
joinStrings[list_List, sep_String] := If[list === {}, "", StringJoin[First[list], Table[sep <> x, {x, Rest[list]}]]];

(* the LaTeX of a head or of a symbol: "Names", then the display name (DeclareDisplayName), then the symbol name *)
(* the objects of the tree expansion (ConnectedFunctionFromVertices) have their usual names *)
$latexDefaultNames = {FullPropagator -> "G", MeanField -> "\\bar\\phi", ExternalSource -> "J"};
latexSymbolName[h_Symbol] := Module[{m = Select[$latexNames, #[[1]] === h &, 1], d = Select[$displayNames, #[[1]] === h &, 1],
    k = Select[$latexDefaultNames, #[[1]] === h &, 1]},
   Which[m =!= {}, ToString[m[[1, 2]]],
    d =!= {}, If[StringQ[d[[1, 2]]], latexName[d[[1, 2]]], ToString[TeXForm[d[[1, 2]]]]],
    k =!= {}, k[[1, 2]],
    True, latexSymbolString[h]]];

(* arguments of functions: labels *)
latexArgument[DummyVariable[depth_Integer, k_Integer]] := {"z", "w", "u", "v", "t"}[[Min[depth, 5]]] <> "_{" <> ToString[k] <> "}";
latexArgument[x_Symbol] := With[{r = Select[$latexLabels, #[[1]] === x &, 1]}, If[r =!= {}, ToString[r[[1, 2]]], latexSymbolName[x]]];
latexArgument[x_?NumberQ] := latexNumber[x];
latexArgument[x_] := latexOfExpression[x];

latexOfExpression[e_Plus] := latexSignedSum[latexSignedTerm /@ latexPlusTerms[e], latexDiracSumQ[e]];
(* the {sign, magnitude} parts of the terms of a sum joined; with positiveFirst the positive terms first (sums of Dirac
   matrices and tensors: m + pslash - kslash, g - (1 - xi) k k/k^2) *)
latexSignedSum[parts0_List, positiveFirst_] := With[{parts = If[TrueQ[positiveFirst], Join[Select[parts0, #[[1]] > 0 &],
      Select[parts0, #[[1]] < 0 &]], parts0]},
   StringJoin[Table[If[i == 1, If[parts[[i, 1]] < 0, "-", ""], If[parts[[i, 1]] < 0, " - ", " + "]] <> parts[[i, 2]], {i, Length[parts]}]]];
latexOfExpression[0] := "0";
latexOfExpression[e_] := With[{p = latexSignedTerm[e]}, If[p[[1]] < 0, "-", ""] <> p[[2]]];

(* the heads of integrals and sums, whose integrand is not delimited *)
$latexIntegralHeads = {IntegralOver, MomentumSum, LoopIntegral, FeynmanParameterIntegral};

(* {sign, LaTeX of the magnitude} of a product; factors with negative powers go to a denominator; Grassmann products last.
   One integral (Feynman parameters, a loop) with a fraction in front: the fraction, then the integral *)
latexIntegralAfterFractionQ[t_] := Head[t] === Times && Count[List @@ t, f_ /; MemberQ[{ParameterIntegral, SimplexIntegral, MinkowskiLoopIntegral}, Head[f]]] == 1 &&
   Count[List @@ t, f_ /; MemberQ[$latexIntegralHeads, Head[f]] || MemberQ[{GrassmannProduct, NonCommutativeMultiply}, Head[f]]] == 1 &&
   MemberQ[List @@ t, Power[b_, n_?NumberQ] /; n < 0 && ! NumberQ[b]];
latexSignedTerm[t_] /; latexIntegralAfterFractionQ[t] := With[{i = First[Select[List @@ t, MemberQ[$latexIntegralHeads, Head[#]] &]]},
   With[{p = latexSignedTerm[t/i]}, {p[[1]], p[[2]] <> "\\," <> latexFactor[i]}]];
latexSignedTerm[t_] := Module[{parts = coefficientParts[numericFactor[t]], c, unit, rest, factors, numerator, denominator, num, den,
    body},
   {c, unit} = parts[[{2, 3}]]; rest = nonNumericPart[t];
   factors = Which[rest === 1, {}, Head[rest] === Times, List @@ rest, True, {rest}];
   (* the unit matrix after the other plain factors (4 a.b 1, not 4 1 a.b), integrals and momentum sums after them (their
      integrand is not delimited), Grassmann products last; an integral that is not the last one is put in parentheses *)
   factors = Join[Select[factors, ! MemberQ[Join[$latexIntegralHeads, {GrassmannProduct, NonCommutativeMultiply}], Head[#]] && # =!= DiracProduct[] &],
     Select[factors, # === DiracProduct[] &],
     Select[factors, MemberQ[$latexIntegralHeads, Head[#]] &], Select[factors, MemberQ[{GrassmannProduct, NonCommutativeMultiply}, Head[#]] &]];
   With[{ints = Flatten[Position[Head /@ factors, Alternatives @@ $latexIntegralHeads, {1}, Heads -> False]]},
    If[Length[ints] > 1 || (ints =!= {} && Last[ints] < Length[factors]),
     factors = MapAt[parenthesizedIntegral, factors, List /@ If[Last[ints] == Length[factors], Most[ints], ints]]]];
   numerator = Select[factors, ! (Head[#] === Power && NumberQ[#[[2]]] && #[[2]] < 0) &];
   denominator = Cases[factors, Power[b_, n_?NumberQ] /; n < 0 :> If[n == -1, b, Power[b, -n]]];
   (* a single sum in a numerator or denominator needs no parentheses *)
   num = If[Length[numerator] == 1 && Head[First[numerator]] === Plus && denominator =!= {} && unit === "", latexOfExpression[First[numerator]],
     joinStrings[Join[If[unit === "", {}, {unit}], latexFactor /@ numerator], "\\,"]];
   den = Which[
     Length[denominator] == 1 && Head[First[denominator]] === Plus && Denominator[c] === 1, latexOfExpression[First[denominator]],
     (* the denominator q^2 - m^2 of a single line *)
     Length[denominator] == 1 && Head[First[denominator]] === latexLine && Denominator[c] === 1, latexLineString @@ First[denominator],
     True, joinStrings[latexFactor /@ denominator, "\\,"]];
   body = Which[
     denominator =!= {}, "\\frac{" <> If[Numerator[c] === 1, If[num === "", "1", num],
        latexNumber[Numerator[c]] <> If[num === "", "", "\\," <> num]] <> "}{" <>
       If[Denominator[c] === 1, den, latexNumber[Denominator[c]] <> "\\," <> den] <> "}",
     num === "", latexNumber[c],
     c === 1, num,
     True, latexNumber[c] <> "\\," <> num];
   {parts[[1]], body}];

(* the LaTeX of a factor: a subexpression named by the option "Names" (n_F(xi(p1)) -> "n_1") is written with its name *)
$latexExpressionNames = {};
latexFactor[e_] := With[{m = Select[$latexExpressionNames, #[[1]] === e &, 1]}, If[m =!= {}, ToString[m[[1, 2]]], latexFactorOf[e]]];

(* an integral that other factors follow, in parentheses *)
latexFactorOf[parenthesizedIntegral[i_]] := "\\left(" <> latexFactor[i] <> "\\right)";
(* a base of a power in parentheses: sums, products, powers, negative numbers, and objects with a superscript of their own
   (p^2, p^\mu, \gamma^\mu, g^{\mu\nu}) *)
latexParenthesized[e_] := If[MemberQ[{Plus, Times, Power, ScalarProduct, FourVector, GammaMatrix, LorentzMetric}, Head[e]] || (NumberQ[e] && e < 0),
   "\\left(" <> latexOfExpression[e] <> "\\right)", latexFactor[e]];

latexFactorOf[x_?NumberQ] := latexNumber[x];
latexFactorOf[IntegralOver[f_, v_List]] := "\\int_{" <> joinStrings[latexArgument /@ v, "\\,"] <> "}" <>
   If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[(GrassmannProduct | NonCommutativeMultiply)[a__]] := joinStrings[latexFactor /@ {a}, "\\,"];
latexFactorOf[DeltaFunction[x_, y_]] := "\\delta(" <> latexArgument[x] <> "," <> latexArgument[y] <> ")";
latexFactorOf[Power[E, x_]] := "e^{" <> latexOfExpression[x] <> "}";
latexFactorOf[Power[b_, n_]] := If[n === 1/2, "\\sqrt{" <> latexOfExpression[b] <> "}",
   latexParenthesized[b] <> "^{" <> latexOfExpression[n] <> "}"];
latexFactorOf[e_Plus] := "\\left(" <> latexOfExpression[e] <> "\\right)";
(* the elementary functions with their usual names *)
$latexElementaryFunctions = {Log -> "\\log", Sin -> "\\sin", Cos -> "\\cos", Tan -> "\\tan", Cot -> "\\cot", Sinh -> "\\sinh",
   Cosh -> "\\cosh", Tanh -> "\\tanh", Coth -> "\\coth", ArcSin -> "\\arcsin", ArcCos -> "\\arccos", ArcTan -> "\\arctan"};
latexFactorOf[(h : Log | Sin | Cos | Tan | Cot | Sinh | Cosh | Tanh | Coth | ArcSin | ArcCos | ArcTan)[x_]] :=
  (h /. $latexElementaryFunctions) <> "\\left(" <> latexOfExpression[x] <> "\\right)";
(* heads with LaTeX rules of their own, which the generic rules below (a function of labels, its derivatives) leave alone;
   the later sections (Matsubara, dimensional regularization, QED, effective actions) add their heads *)
$latexHeadsWithRules = {Plus, Times, Power, IntegralOver, GrassmannProduct, NonCommutativeMultiply, DeltaFunction, parenthesizedIntegral,
   VertexFunction};
latexFactorOf[(Derivative[n__])[h_Symbol][args___]] /; ! MemberQ[$latexHeadsWithRules, h] := latexSymbolName[h] <>
   If[Length[{n}] == 1, If[n <= 3, "^{" <> StringJoin[Table["\\prime", {n}]] <> "}", "^{(" <> ToString[n] <> ")}"],
    "^{(" <> joinStrings[ToString /@ {n}, ","] <> ")}"] <> "(" <> joinStrings[latexArgument /@ {args}, ","] <> ")";
latexFactorOf[VertexFunction[args__]] /; FreeQ[$latexNames, VertexFunction] :=
  "\\Gamma^{(" <> ToString[Length[{args}]] <> ")}(" <> joinStrings[latexArgument /@ {args}, ","] <> ")";
latexFactorOf[h_Symbol[args___]] /; ! MemberQ[Join[$latexHeadsWithRules, First /@ $latexElementaryFunctions], h] || (h === VertexFunction && ! FreeQ[$latexNames, VertexFunction]) :=
  latexSymbolName[h] <> If[MemberQ[$latexSubscripts, h], "_{" <> joinStrings[latexArgument /@ {args}, ","] <> "}",
    "(" <> joinStrings[latexArgument /@ {args}, ","] <> ")"];
latexFactorOf[s_Symbol] := latexArgument[s];

(* ---- QED (section 19): Dirac matrices, Lorentz tensors, loop and Feynman-parameter integrals. A slashed momentum is
   \slashed{p} (\usepackage{slashed}); the indices are written upper, a repeated one is contracted, as in the package ---- *)
$latexDiracHeads = {GammaMatrix, SlashedMomentum, DiracProduct, DiracTrace, SpinorChain, LorentzMetric, FourVector, latexSlashed};
$latexHeadsWithRules = Join[$latexHeadsWithRules, $latexDiracHeads, {ScalarProduct, FeynmanDenominator, MinkowskiLoopIntegral,
    ParameterIntegral, SimplexIntegral, FeynmanParameter, latexLine}];
$latexIntegralHeads = Join[$latexIntegralHeads, {MinkowskiLoopIntegral, ParameterIntegral, SimplexIntegral}];
$latexDefaultNames = Join[$latexDefaultNames, {SpacetimeDimension -> "d"}];
latexDiracSumQ[e_] := ! FreeQ[e, Alternatives @@ $latexDiracHeads];
(* the terms of a sum, a slashed sum of momenta among them written as its terms: m + (p - k)slash -> m + pslash - kslash *)
latexPlusTerms[e_Plus] := If[latexDiracSumQ[e], Flatten[latexSlashedTerms /@ (List @@ e)], List @@ e];
latexSlashedTerms[t_] := Which[
   MatchQ[t, SlashedMomentum[_]], Table[c[[1]] latexSlashed[c[[2]]], {c, latexMomentumTerms[t[[1]]]}],
   MatchQ[t, Times[_?NumberQ, SlashedMomentum[_]]], Table[t[[1]] c[[1]] latexSlashed[c[[2]]], {c, latexMomentumTerms[t[[2, 1]]]}],
   True, {t}];
(* a momentum as its terms {coefficient, name}: numbers and Feynman parameters are coefficients *)
latexMomentumCoefficient[t_] := Which[NumericQ[t], t, Head[t] === Times, Times @@ Select[List @@ t, NumericQ[#] || MatchQ[#, FeynmanParameter[_]] &],
   True, 1];
latexMomentumTerms[q_] := With[{x = Expand[q]}, Table[With[{c = latexMomentumCoefficient[t]}, {c, t/c}], {t, If[Head[x] === Plus, List @@ x, {x}]}]];
latexMomentumSum[q_] := latexSignedSum[Table[latexSignedTerm[c[[1]] c[[2]]], {c, latexMomentumTerms[q]}], True];
(* a momentum as a factor, in (small) parentheses when it is a sum: p, (p - k) *)
latexMomentumFactor[q_] := With[{ts = latexMomentumTerms[q]}, If[Length[ts] == 1 && ts[[1, 1]] === 1, latexArgument[ts[[1, 2]]],
    "(" <> latexMomentumSum[q] <> ")"]];
(* pslash, and p1slash = \slashed{p}{}_{1} (the slash on the letter; the empty group keeps the subscript at the height of
   that of p_1, not below the box of \slashed) *)
latexSlashedName[n_Symbol] /; FreeQ[{$latexLabels, $latexNames, $displayNames}, n] := With[{parts = splitTrailingDigits[cutAtDollar[SymbolName[n]]]},
   "\\slashed{" <> latexBaseName[parts[[1]]] <> "}" <> If[parts[[2]] === "", "", "{}_{" <> parts[[2]] <> "}"]];
(* a name written base_{i} (the default loop momentum ell_i, LoopMomentum[i] outside its integral) in the same way:
   \slashed{\ell}{}_{1}; the base is a letter or a command \name *)
slashBaseQ[b_String] := (StringLength[b] == 1 && LetterQ[b]) || (StringLength[b] > 1 && StringTake[b, 1] === "\\" && LetterQ[StringDrop[b, 1]]);
latexSlashedName[n_] := With[{s = latexArgument[n]}, With[{pos = StringPosition[s, "_{"]},
    If[pos =!= {} && StringTake[s, -1] === "}" && slashBaseQ[StringTake[s, pos[[1, 1]] - 1]] &&
      StringFreeQ[StringTake[s, {pos[[1, 2]] + 1, -2}], "{"] && StringFreeQ[StringTake[s, {pos[[1, 2]] + 1, -2}], "}"],
     "\\slashed{" <> StringTake[s, pos[[1, 1]] - 1] <> "}{}" <> StringDrop[s, pos[[1, 1]] - 1], "\\slashed{" <> s <> "}"]]];
latexFactorOf[latexSlashed[n_]] := latexSlashedName[n];
latexFactorOf[SlashedMomentum[q_]] := With[{ts = latexMomentumTerms[q]}, If[Length[ts] == 1 && ts[[1, 1]] === 1, latexSlashedName[ts[[1, 2]]],
    "\\left(" <> latexOfExpression[Total[Table[c[[1]] latexSlashed[c[[2]]], {c, ts}]]] <> "\\right)"]];
latexFactorOf[GammaMatrix[mu_]] := "\\gamma^{" <> latexArgument[mu] <> "}";
latexFactorOf[LorentzMetric[a_, b_]] := "g^{" <> latexArgument[a] <> latexArgument[b] <> "}";
latexFactorOf[FourVector[q_, mu_]] := latexMomentumFactor[q] <> "^{" <> latexArgument[mu] <> "}";
latexFactorOf[ScalarProduct[a_, b_]] := If[a === b, latexMomentumFactor[a] <> "^{2}", latexMomentumFactor[a] <> "\\cdot " <> latexMomentumFactor[b]];
latexFactorOf[FeynmanParameter[i_]] := "x_{" <> latexOfExpression[i] <> "}";
(* the factors of a product of Dirac matrices: sums in parentheses, and a matrix with a coefficient, (i\gamma^\mu) *)
latexDiracFactor[f_] := Which[
   Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)",
   Head[f] === Times && ! MemberQ[{Plus, Times}, Head[nonNumericPart[f]]], "\\left(" <> latexOfExpression[f] <> "\\right)",
   True, latexOfExpression[f]];
latexFactorOf[DiracProduct[]] := "\\mathbf{1}";
latexFactorOf[DiracProduct[a__]] := joinStrings[latexDiracFactor /@ {a}, "\\,"];
latexFactorOf[DiracTrace[x_]] := "\\operatorname{tr}\\left[" <> If[Head[x] === Plus, latexOfExpression[x], latexFactor[x]] <> "\\right]";
latexFactorOf[SpinorChain[{p2_, m2_}, x_, {p1_, m1_}]] := "\\bar{u}(" <> latexMomentumSum[p2] <> ")\\," <>
   If[x === DiracProduct[], "", If[Head[x] === Plus, "\\left(" <> latexOfExpression[x] <> "\\right)", latexFactor[x]] <> "\\,"] <>
   "u(" <> latexMomentumSum[p1] <> ")";
(* the denominator q^2 - m^2 of a line (latexDisplayForm), in parentheses next to other factors *)
latexLineString[q_, m_] := latexMomentumFactor[q] <> "^{2} - " <> latexParenthesized[m] <> "^{2}";
latexFactorOf[latexLine[q_, m_]] := "\\left(" <> latexLineString[q, m] <> "\\right)";
latexIntegrand[f_] := If[Head[f] === Plus, "\\left(" <> latexOfExpression[f] <> "\\right)", latexOfExpression[f]];
latexFactorOf[MinkowskiLoopIntegral[f_, ks_List]] := "\\int_{" <> joinStrings[latexArgument /@ ks, "\\,"] <> "}" <> latexIntegrand[f];
latexFactorOf[(ParameterIntegral | FeynmanParameterIntegral)[f_, x_]] := "\\int_0^1 d" <> latexArgument[x] <> "\\," <> latexIntegrand[f];
latexFactorOf[SimplexIntegral[f_, xs_List]] := "\\int_0^1 " <> joinStrings[Table["d" <> latexArgument[x], {x, xs}], "\\,"] <>
   "\\,\\delta(1 - " <> joinStrings[latexArgument /@ xs, " - "] <> ")\\," <> latexIntegrand[f];
(* before the LaTeX: the lines 1/(q^2 - m^2) as denominators, so that the lines of a term make one fraction; the numbers
   in front of an integral taken out of it *)
latexDisplayForm[e_] := e //. {FeynmanDenominator[q_, m_] :> If[m === 0, 1/ScalarProduct[q, q], 1/latexLine[q, m]],
    (h : MinkowskiLoopIntegral | ParameterIntegral | SimplexIntegral)[Times[c_?NumericQ, f__], v_] :> c h[Times[f], v]};
latexFactorOf[e_] := ToString[TeXForm[e]];

(* the default loop momenta LoopMomentum[i] of QED (MinkowskiLoopIntegral) are \ell_i: p_i, as in the Matsubara formulas,
   would be the external momenta p1, p2. So in an integrand taken out of its integral (Dirac and Lorentz objects, no
   MomentumSum of the Matsubara formulas) *)
qedLatexQ[e_] := ! FreeQ[e, MinkowskiLoopIntegral | FeynmanDenominator] ||
   (FreeQ[e, MomentumSum] && ! FreeQ[e, SlashedMomentum | DiracProduct | DiracTrace | SpinorChain | FourVector | ScalarProduct]);
latexLoopMomentumNames[e_] := If[! qedLatexQ[e], {},
   Table[LoopMomentum[i] -> "\\ell_{" <> ToString[i] <> "}", {i, Union[Cases[{e}, LoopMomentum[i_] :> i, Infinity]]}]];
(* "Names" names heads and symbols, and also subexpressions, such as n_F(xi(p1)) -> "n_1"; a name given wins *)
ExpressionToLaTeX[e_, opts___Rule] := With[{names = Flatten[{optionValue[{opts}, {"Names" -> {}}, "Names"]}]},
   Block[{$latexNames = Select[names, AtomQ[#[[1]]] &], $latexExpressionNames = Join[Select[names, ! AtomQ[#[[1]]] &], latexLoopMomentumNames[e]],
     $latexLabels = Flatten[{optionValue[{opts}, {"Labels" -> {}}, "Labels"]}],
     $latexSubscripts = Flatten[{optionValue[{opts}, {"Subscripts" -> {}}, "Subscripts"]}]},
    latexOfExpression[latexDisplayForm[e]]]];
