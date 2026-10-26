
(* ========================================================================= *)
(*  14. Pictures                                                             *)
(*  Only basic graphics primitives (Line, Disk, Polygon, Text) and a         *)
(*  deterministic layout, so that the same geometry can be exported to TikZ. *)
(*                                                                           *)
(*  A term is drawn as follows. Labels joined by a DeltaFunction are one     *)
(*  point. Two-label factors are lines (propagators or interaction lines),   *)
(*  factors with three or more labels are gray blobs, one-label factors are  *)
(*  source insertions. An external label that ends a single propagator is   *)
(*  an external point of a correlation function (open circle); any other     *)
(*  external label sits on a vertex of an amputated function (self-energy,   *)
(*  vertex) and gets an external leg, with an arrow for fermions.            *)
(* ========================================================================= *)

$drawingDefaults = {"VertexLabels" -> Automatic, "Interactions" -> {}, "Directed" -> {}, "InteractionStyle" -> Automatic,
   "Columns" -> Automatic, "ImageSize" -> Automatic, "Output" -> "Graphics", "Weights" -> True, "ExternalOrder" -> Automatic,
   "LegDirections" -> Automatic, "AmputatedLegs" -> Automatic, "Scale" -> 0.7, "Labels" -> {}, "LineWidth" -> Automatic,
   "LabelFont" -> "\\small", "Couplings" -> {}};
drawingOption[opts_List, name_String] := optionValue[opts, $drawingDefaults, name];

(* the options of a call: the explicit ones first, then those of the function (which SetOptions changes); unknown
   names give a message *)
Options[DiagramPlot] = $drawingDefaults; Options[DrawDiagram] = $drawingDefaults; Options[DiagramToTikZ] = $drawingDefaults;
FunctionalDiagrams::optx = "Unknown option `1` of `2`; the options are `3`.";
drawingOptions[f_Symbol, opts_List] := Module[{given = Select[Flatten[opts], MatchQ[#, _Rule | _RuleDelayed] &]},
   Scan[If[! MemberQ[$drawingDefaults[[All, 1]], optionKey[#[[1]]]], Message[FunctionalDiagrams::optx, #[[1]], f,
       $drawingDefaults[[All, 1]]]] &, given];
   Join[given, Options[f]]];

(* sizes, in layout units *)
$pixelsPerUnit = 46.; $dotRadius = 0.075; $externalPointRadius = 0.08; $blobRadius = 0.2; $loopRadius = 0.28; $bosonLoopRadius = 0.36;
$arrowSize = 0.2; $waveAmplitude = 0.055; $waveLength = 0.17; $stubLength = 0.4; $legLength = 0.6; $fontPixels = 10.;

(* ---------------------------------------------------------------- plane geometry *)
vectorNorm[v_] := Sqrt[v . v];
unitVectorOf[v_] := With[{n = vectorNorm[v]}, If[n < 10.^-9, {1., 0.}, v/n]];
rotate90[{x_, y_}] := {-y, x};
angleOf[{x_, y_}] := If[Abs[x] + Abs[y] < 10.^-12, 0., ArcTan[x, y]];
directionAt[t_] := {Cos[t], Sin[t]};
signedAngle[p_, q_] := ArcTan[p . q, p[[1]] q[[2]] - p[[2]] q[[1]]];

(* A path is {"segment", u, v}, {"arc", centre, radius, a0, da} (from the angle a0 to a0 + da) or a balloon (below).
   Points along it are computed with listable arithmetic, which is much faster than point-by-point evaluation. *)
(* circular arc from u to v whose midpoint is displaced by s along the left normal of v - u *)
arcThrough[u_, v_, s_] := Module[{c = vectorNorm[v - u], m = (u + v)/2, normal, t, centre},
   normal = rotate90[unitVectorOf[v - u]];
   If[c < 10.^-9 || Abs[s] < 10.^-3 c, {"segment", N[u], N[v]},
    t = (s^2 - c^2/4)/(2 s); centre = m + t normal;
    {"arc", centre, Abs[s - t], angleOf[u - centre], 2 signedAngle[u - centre, m + s normal - centre]}]];
loopPath[p_, th_] := {"arc", p + $loopRadius directionAt[th], $loopRadius, th + Pi, 2. Pi};
(* A self-loop of coils or waves is a balloon, {"balloon", p, centre, radius, 2 Pi}: a larger circle and the two lines
   from p tangent to it, which leave p at an angle, so that the coils do not touch the line the loop hangs from. It is
   traversed counterclockwise, from p along the right tangent. *)
selfLoopPath[p_, th_, style_, others_List : {}] := Which[
   (* coils need a larger circle to be read as a loop *)
   style === "Curly", balloonAt[p, th, 1.4 $bosonLoopRadius, 1.9/1.4, others],
   MemberQ[{"Wavy", "DoubleWavy", "Zigzag"}, style], balloonAt[p, th, $bosonLoopRadius, 1.4, others],
   True, loopPath[p, th]];
(* the balloon of radius r in the direction th, its centre at q r from p. At a vertex with two more lines (directions
   others, e.g. a gluon loop at the four-gluon vertex) the tangents must not continue those lines, or the picture reads as
   an X: if they would (within 30 degrees), the neck is made narrower than the angle of the continued lines (or wider,
   when that angle is small) *)
balloonAt[p_, th_, r_, q_, others_List] := With[{a0 = ArcSin[1./q]},
   If[Length[others] =!= 2, {"balloon", p, p + q r directionAt[th], r, 2. Pi},
    With[{g = Mean[Table[Abs[angleDifference[o + Pi, th]], {o, others}]]},
     With[{a = If[Abs[a0 - g] >= 30. Degree, a0, If[g > 45. Degree, Max[20. Degree, g - 35. Degree], Min[75. Degree, g + 35. Degree]]]},
      {"balloon", p, p + (r/Sin[a]) directionAt[th], r, 2. Pi}]]]];
balloonParts[{"balloon", p_, c_, r_, _}] := With[{dd = vectorNorm[c - p], phi = angleOf[p - c]}, With[{beta = ArcCos[r/dd]},
    (* {length of a tangent, length of the arc, start angle of the arc, first tangent point, second tangent point} *)
    {Sqrt[dd^2 - r^2], r (2. Pi - 2 beta), phi + beta, c + r directionAt[phi + beta], c + r directionAt[phi - beta]}]];
reversedPath[{"segment", u_, v_}] := {"segment", v, u};
reversedPath[{"arc", c_, r_, a0_, da_}] := {"arc", c, r, a0 + da, -da};
pathLength[{"segment", u_, v_}] := vectorNorm[v - u];
pathLength[{"arc", c_, r_, a0_, da_}] := r Abs[da];
pathLength[b : {"balloon", __}] := With[{q = balloonParts[b]}, 2 q[[1]] + q[[2]]];

(* points at the parameters ts (in [0, 1]), displaced by ds along the left normal *)
pathPoints[{"segment", u_, v_}, ts_, ds_] := With[{n = rotate90[unitVectorOf[v - u]]},
   Transpose[{u[[1]] + (v[[1]] - u[[1]]) ts + n[[1]] ds, u[[2]] + (v[[2]] - u[[2]]) ts + n[[2]] ds}]];
pathPoints[{"arc", c_, r_, a0_, da_}, ts_, ds_] := With[{an = a0 + da ts, rr = r - Sign[da] ds},
   Transpose[{c[[1]] + rr Cos[an], c[[2]] + rr Sin[an]}]];
pathPoints[b : {"balloon", p_, c_, r_, _}, ts_, ds_] := Module[{q = balloonParts[b], len, u1, u2, dl = If[ListQ[ds], ds, Table[ds, {Length[ts]}]]},
   len = 2 q[[1]] + q[[2]]; u1 = unitVectorOf[q[[4]] - p]; u2 = unitVectorOf[p - q[[5]]];
   Table[With[{x = len ts[[i]], d = dl[[i]]}, Which[
      x <= q[[1]], p + x u1 + d rotate90[u1],
      x <= q[[1]] + q[[2]], c + (r - d) directionAt[q[[3]] + (x - q[[1]])/r],
      True, q[[5]] + (x - q[[1]] - q[[2]]) u2 + d rotate90[u2]]], {i, Length[ts]}]];
pathPolyline[path_, ds_ : 0.] := With[{k = If[path[[1]] === "segment", 1, Max[6, Ceiling[Abs[path[[5]]]/0.12]]]},
   pathPoints[path, N[Range[0, k]]/k, ds]];
pathWavy[path_, ds_ : 0.] := With[{nw = Max[1, Round[pathLength[path]/$waveLength]]}, With[{ts = N[Range[0, 8 nw]]/(8 nw)},
    pathPoints[path, ts, ds + $waveAmplitude Sin[2. Pi nw ts]]]];
pathZigzag[path_] := With[{nz = Max[2, 2 Round[pathLength[path]/$waveLength]]}, With[{ts = N[Range[0, nz]]/nz},
    pathPoints[path, ts, 0.06 Table[If[i == 0 || i == nz, 0., (-1)^i], {i, 0, nz}]]]];
(* a gluon: a coil, an ellipse of semi-axes b (along the path) and a (across) traversed nc + 1/2 times while drifting along
   the path, so that it starts and ends on the path and is mirror symmetric about its middle (the first and the last half
   turns on the left of the path); on a self-loop the coil grows from nothing at the ends, so that it does not cross the
   line the loop hangs from *)
coilPoints[path_, nc_Integer, a_, b_, perTurn_ : 16] := Module[{m = perTurn nc + Round[perTurn/2], len = pathLength[path], u, th, f, s0},
   u = N[Range[0, m]]/m; th = 2. Pi (nc + 0.5) u;
   f = If[path[[1]] === "balloon", s0 = 2. a; Table[Min[1., (len w/s0)^2, (len (1 - w)/s0)^2], {w, u}], 1.];
   pathPoints[path, u + f (b/len) (1 - 2 u - Cos[th]), f a Sin[th]]];
pathCurly[path_] := coilPoints[path, Max[2, Round[pathLength[path]/0.13]], 0.065, 0.05, 24];
pathDashes[path_, period_ : 0.15] := With[{nd = Max[1, Round[pathLength[path]/period]]},
   Partition[pathPoints[path, Flatten[Table[{i + 0.2, i + 0.5, i + 0.8}, {i, 0, nd - 1}]]/nd, 0.], 3]];
pathDots[path_] := With[{nd = Max[2, Round[pathLength[path]/0.09]]}, pathPoints[path, N[Range[0, nd]]/nd, 0.]];
pathArrowHead[path_, t_ : 0.5] := With[{q = pathPoints[path, {t - 0.03, t, t + 0.03}, 0.]}, With[{m = q[[2]], tg = unitVectorOf[q[[3]] - q[[1]]]},
    {m + 0.55 $arrowSize tg, m - 0.45 $arrowSize tg + 0.36 $arrowSize rotate90[tg], m - 0.45 $arrowSize tg - 0.36 $arrowSize rotate90[tg]}]];
(* directions of the path at its two ends, pointing into the path *)
pathEndAngles[path_] := With[{q = pathPoints[path, {0., 0.02, 0.98, 1.}, 0.]}, {angleOf[q[[2]] - q[[1]]], angleOf[q[[3]] - q[[4]]]}];

(* bisector of the largest free sector around a vertex (directions angles); ties broken towards preferred; with
   fraction < 1, every sector at least fraction times as wide as the largest one is a candidate *)
gapDirection[angles_List, preferred_, fraction_ : 1.] := First[gapDirections[angles, preferred, fraction]];
gapDirections[angles_List, preferred_, fraction_ : 1.] := Module[{s, gaps, best, good, rest},
   If[angles === {}, angleOf[preferred] + {0., Pi, Pi/2, -Pi/2},
    s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles];
    gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
    best = Max[gaps[[All, 1]]];
    good = Select[gaps, #[[1]] > fraction best - 0.05 &]; rest = Select[gaps, #[[1]] <= fraction best - 0.05 &];
    Join[SortBy[(#[[2]] + #[[1]]/2) & /@ good, -Cos[# - angleOf[preferred]] &], (#[[2]] + #[[1]]/2) & /@ SortBy[rest, -#[[1]] &]]]];
(* {direction, width} of the largest free sector, of those nearly as large the one closest to preferred *)
outwardGap[angles_List, preferred_] := Module[{s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles], gaps, best},
   gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
   best = Max[gaps[[All, 1]]];
   First[SortBy[{#[[2]] + #[[1]]/2, #[[1]]} & /@ Select[gaps, #[[1]] > best - 0.05 &], -Cos[#[[1]] - angleOf[preferred]] &]]];
(* the directions of the free sectors at least fraction times the largest one, the closest to preferred first *)
largeGapDirections[angles_List, preferred_, fraction_] := If[angles === {}, {angleOf[preferred]},
   Module[{s = Sort[(# - 2. Pi Floor[#/(2. Pi)]) & /@ angles], gaps},
    gaps = Table[{If[i < Length[s], s[[i + 1]] - s[[i]], s[[1]] + 2. Pi - s[[i]]], s[[i]]}, {i, Length[s]}];
    SortBy[(#[[2]] + #[[1]]/2) & /@ Select[gaps, #[[1]] >= fraction Max[gaps[[All, 1]]] &], -Cos[# - angleOf[preferred]] &]]];
(* the disc {centre, radius} of a self-loop path, and its smallest distance to the discs *)
loopDisc[{"arc", c_, r_, __}] := {c, r};
loopDisc[{"balloon", _, c_, r_, _}] := {c, r};
loopDisc[_] := None;
loopClearance[path_, discs_List] := With[{d = loopDisc[path], ds = DeleteCases[discs, None]},
   If[d === None || ds === {}, Infinity, Min[Table[vectorNorm[e[[1]] - d[[1]]] - e[[2]] - d[[2]], {e, ds}]]]];
boxesOverlapQ[{c1_, wh1_}, {c2_, wh2_}] := Abs[c1[[1]] - c2[[1]]] < (wh1[[1]] + wh2[[1]])/2 && Abs[c1[[2]] - c2[[2]]] < (wh1[[2]] + wh2[[2]])/2;

(* ---------------------------------------------------------------- small graph algorithms (pairs = unordered edges) *)
neighboursOf[v_, pairs_] := DeleteDuplicates[Join[Cases[pairs, {v, u_} :> u], Cases[pairs, {u_, v} :> u]]];

shortestPath[s_, t_, pairs_] := Module[{seen = {s}, parents = {}, queue = {s}, v, path, found = (s === t)},
   While[queue =!= {} && ! found,
    v = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[seen, u], AppendTo[seen, u]; AppendTo[parents, {u, v}]; AppendTo[queue, u]; If[u === t, found = True]],
     {u, neighboursOf[v, pairs]}]];
   If[! found, None,
    path = {t};
    While[First[path] =!= s, PrependTo[path, First[Select[parents, #[[1]] === First[path] &]][[2]]]];
    path]];

componentOf[s_, pairs_] := Module[{seen = {s}, queue = {s}, v},
   While[queue =!= {}, v = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[seen, u], AppendTo[seen, u]; AppendTo[queue, u]], {u, neighboursOf[v, pairs]}]];
   seen];

(* longest simple cycle (length >= 3) by depth-first search; graphs are small *)
longestCycle[nodes_, pairs_] := Module[{best = {}, count = 0, search, index},
   index[x_] := First[First[Position[nodes, x, {1}]]];
   search[path_, s_] := (count++;
     If[count < 3000 && Length[best] < Length[nodes],
      Do[Which[
        u === s, If[Length[path] >= 3 && Length[path] > Length[best], best = path],
        ! MemberQ[path, u] && index[u] > index[s], search[Append[path, u], s]], {u, neighboursOf[Last[path], pairs]}]]);
   Do[search[{s}, s], {s, nodes}];
   best];

(* remove free vertices of degree <= 1 repeatedly: {core, stripped (outermost first)} *)
stripPendantVertices[fixed_, free_, pairs_] := Module[{core = free, stripped = {}, again = True, alive},
   While[again, again = False;
    Do[If[MemberQ[core, v],
      alive = Select[neighboursOf[v, pairs], MemberQ[core, #] || MemberQ[fixed, #] &];
      If[Length[alive] <= 1, core = DeleteCases[core, v]; PrependTo[stripped, v]; again = True]], {v, core}]];
   {core, Reverse[stripped]}];

(* barycentric (Tutte) placement of the free vertices, the fixed ones being given *)
(* (the free vertices in outward are pulled 1.5 times as hard towards their fixed neighbours) *)
tutteEmbedding[fixedPositions_, free_, pairs_, outward_List : {}] := Module[{m = Length[free], M, bx, by, j, fx, wt},
   If[m == 0, {},
    M = 10.^-6 IdentityMatrix[m]; bx = Table[0., {m}]; by = Table[0., {m}];
    Do[wt = If[MemberQ[outward, free[[i]]], 1.5, 1.];
     Do[
       j = Position[free, u, {1}];
       fx = Select[fixedPositions, #[[1]] === u &];
       Which[
        j =!= {}, M[[i, i]] += 1.; M[[i, j[[1, 1]]]] -= 1.,
        fx =!= {}, M[[i, i]] += wt; bx[[i]] += wt fx[[1, 2, 1]]; by[[i]] += wt fx[[1, 2, 2]]],
       {u, neighboursOf[free[[i]], pairs]}], {i, m}];
    With[{xs = N[LinearSolve[M, bx]], ys = N[LinearSolve[M, by]]}, Table[{free[[i]], {xs[[i]], ys[[i]]}}, {i, m}]]]];

(* place the stripped (pendant) vertices outwards, in the largest free sector of their parent *)
placePendantVertices[positions0_, stripped_, pairs_, centre_, outward_ : {}] := Module[{positions = positions0, todo = stripped, v,
    parent, kids, pp, th0, k, angles},
   While[todo =!= {},
    v = Select[todo, Intersection[neighboursOf[#, pairs], positions[[All, 1]]] =!= {} &];
    If[v === {}, positions = Join[positions, Table[{todo[[i]], {1.5 i, -1.5}}, {i, Length[todo]}]]; todo = {},
     parent = First[Select[positions[[All, 1]], MemberQ[neighboursOf[First[v], pairs], #] &]];
     pp = Replace[parent, Rule @@@ positions];
     kids = Select[todo, MemberQ[neighboursOf[#, pairs], parent] &];
     angles = Cases[positions, {u_, q_} /; MemberQ[neighboursOf[parent, pairs], u] :> angleOf[q - pp]];
     th0 = gapDirection[angles, Which[vectorNorm[pp - centre] < 0.1, {0., 1.}, MemberQ[outward, parent], centre - pp, True, pp - centre]];
     k = Length[kids];
     (* one pendant part (a tadpole, a bubble with a tadpole, a chain of tadpoles): turned, within the free sector, in the
        direction where it stays farthest from the lines and points already placed (legs, other lines) *)
     If[k == 1, Module[{segs, pts, depth, score, best = th0, bestScore = -Infinity},
        segs = Cases[pairs, {a_, b_} /; MemberQ[positions[[All, 1]], a] && MemberQ[positions[[All, 1]], b] :>
           {Replace[a, Rule @@@ positions], Replace[b, Rule @@@ positions]}];
        pts = Cases[positions, {u_, q_} /; u =!= parent :> q];
        (* the vertices of the part, and one more for what may hang at its end (a self-loop) *)
        depth = Min[3, 1 + Length[componentOf[First[kids], Select[pairs, MemberQ[todo, #[[1]]] && MemberQ[todo, #[[2]]] &]]]];
        Do[With[{th = th0 + dth},
          If[AllTrue[angles, Abs[angleDifference[th, #]] > 0.6 &],
           score = Min[Join[{10.}, Flatten[Table[With[{q = pp + 0.9 j directionAt[th]},
                 Join[distancesFromPoint[q, segs], Table[vectorNorm[q - x], {x, pts}]]], {j, depth}]]]] - 0.05 Abs[dth];
           If[score > bestScore + 10.^-6, bestScore = score; best = th]]], {dth, {0., 0.26, -0.26, 0.52, -0.52, 0.79, -0.79, 1.05, -1.05}}];
        th0 = best]];
     positions = Join[positions, Table[{kids[[j]], pp + 0.9 directionAt[th0 + 0.9 (j - (k + 1)/2)]}, {j, k}]];
     todo = Select[todo, ! MemberQ[kids, #] &]]];
   positions];

(* push apart free vertices that are too close *)
pushApartVertices[positions0_, free_, minimumDistance_] := Module[{p = positions0, moved, v, d},
   Do[moved = False;
    Do[If[MemberQ[free, p[[i, 1]]],
      Do[If[j != i, v = p[[i, 2]] - p[[j, 2]]; d = vectorNorm[v];
        With[{dm = If[Head[p[[i, 1]]] === blob || Head[p[[j, 1]]] === blob, minimumDistance + 0.35, minimumDistance]},
         If[d < dm, moved = True;
          p[[i, 2]] = p[[i, 2]] + If[d < 10.^-6, 0.5 dm directionAt[2.4 i], 0.6 (dm - d) v/d]]]], {j, Length[p]}]], {i, Length[p]}];
    If[! moved, Break[]], {12}];
   p];

(* the vertices of a path on the horizontal axis, the others in layers above it *)
layeredLayout[base_, rest_, pairs_] := Module[{positions = base, remaining = rest, layer, depth = 0, xs, order},
   While[remaining =!= {},
    layer = Select[remaining, Intersection[neighboursOf[#, pairs], positions[[All, 1]]] =!= {} &];
    If[layer === {}, layer = {First[remaining]}];
    depth++;
    xs = Table[With[{nx = Cases[positions, {u_, q_} /; MemberQ[neighboursOf[v, pairs], u] :> q[[1]]]}, If[nx === {}, 0., Mean[nx]]], {v, layer}];
    order = Ordering[xs]; layer = layer[[order]]; xs = xs[[order]];
    xs = With[{sp = FoldList[Max[#2, #1 + 0.95] &, First[xs], Rest[xs]]}, sp - Mean[sp] + Mean[xs]];
    positions = Join[positions, Table[{layer[[i]], {xs[[i]], 0.9 depth}}, {i, Length[layer]}]];
    remaining = Select[remaining, ! MemberQ[layer, #] &]];
   positions];

(* ---------------------------------------------------------------- crossings and overlaps *)
(* with listable arithmetic: the number of segments of the list T that the segment {a, b} crosses (segments with a
   common end, or touching, do not cross) *)
crossingsWith[{a_, b_}, T_] := If[T === {}, 0, With[{x1 = T[[All, 1, 1]], y1 = T[[All, 1, 2]], x2 = T[[All, 2, 1]], y2 = T[[All, 2, 2]]},
    With[{o1 = (b[[1]] - a[[1]]) (y1 - a[[2]]) - (b[[2]] - a[[2]]) (x1 - a[[1]]), o2 = (b[[1]] - a[[1]]) (y2 - a[[2]]) - (b[[2]] - a[[2]]) (x2 - a[[1]]),
       o3 = (x2 - x1) (a[[2]] - y1) - (y2 - y1) (a[[1]] - x1), o4 = (x2 - x1) (b[[2]] - y1) - (y2 - y1) (b[[1]] - x1)},
     Total[UnitStep[-o1 o2 - 10.^-9] UnitStep[-o3 o4 - 10.^-9]]]]];
crossingCount[segments_, others_] := Total[Table[crossingsWith[sg, others], {sg, segments}]];
(* the bisector th of the largest free sector around the point p (directions angles, ties towards preferred); if the point
   at distance reach in that direction is enclosed by the segments T (inside a loop or a bubble), the bisector of the
   largest sector that leads outside *)
outsideGapDirection[p_, angles_List, preferred_, T_, reach_] := Module[{th = gapDirection[angles, preferred], ok, width},
   If[angles =!= {} && enclosedPointQ[p + reach directionAt[th], T],
    ok = Select[gapDirections[angles, preferred, 0.], ! enclosedPointQ[p + reach directionAt[#], T] &];
    width[t_] := 2 Min[Table[Abs[angleDifference[t, a]], {a, angles}]];
    If[ok =!= {}, With[{wmax = Max[width /@ ok]}, th = First[Select[ok, width[#] > wmax - 0.05 &]]]]];
   th];
(* the point q enclosed by the segments T (inside a loop or a bubble): every ray from it crosses one of them *)
enclosedPointQ[q_, T_] := T =!= {} && AllTrue[Range[0, 15], crossingsWith[{q, q + 50. directionAt[2. Pi (# + 0.37)/16]}, T] > 0 &];
mutualCrossingCount[segments_] := Total[Table[crossingsWith[segments[[i]], Drop[segments, i]], {i, Length[segments]}]];
(* distance of the point p from the segment {a, b} *)
segmentDistance[p_, {a_, b_}] := Module[{ab = b - a, t},
   t = If[ab . ab < 10.^-12, 0., Min[1., Max[0., (p - a) . ab/(ab . ab)]]];
   vectorNorm[p - a - t ab]];
(* listable: x clipped to [0, 1]; the distances of the point p from the segments T; of the points P from the segment {a, b} *)
clip01[x_] := With[{y = (x + Abs[x])/2}, 1 - (1 - y + Abs[1 - y])/2];
distancesFromPoint[p_, T_] := If[T === {}, {}, With[{x1 = T[[All, 1, 1]], y1 = T[[All, 1, 2]], x2 = T[[All, 2, 1]], y2 = T[[All, 2, 2]]},
    With[{dx = x2 - x1, dy = y2 - y1}, With[{t = clip01[((p[[1]] - x1) dx + (p[[2]] - y1) dy)/(dx^2 + dy^2 + 10.^-12)]},
      Sqrt[(p[[1]] - x1 - t dx)^2 + (p[[2]] - y1 - t dy)^2]]]]];
distancesToSegment[P_, {a_, b_}] := If[P === {}, {}, With[{px = P[[All, 1]], py = P[[All, 2]], dx = b[[1]] - a[[1]], dy = b[[2]] - a[[2]]},
    With[{t = clip01[((px - a[[1]]) dx + (py - a[[2]]) dy)/(dx^2 + dy^2 + 10.^-12)]},
     Sqrt[(px - a[[1]] - t dx)^2 + (py - a[[2]] - t dy)^2]]]];
countBelow[list_, x_] := If[list === {}, 0, Total[UnitStep[x - list - 10.^-12]]];

(* The brute-force choice of sides (0 or 1) of n items: the assignment with the lowest cost[sides], of the 2^n tried in
   increasing order of their binary codes; of equal costs (within 10^-9) the first. The callers bound n. *)
cheapestSides[n_Integer, cost_] := Module[{best = None, bestCost = Infinity, sides, c},
   Do[sides = IntegerDigits[code, 2, n]; c = cost[sides];
    If[c < bestCost - 10.^-9, bestCost = c; best = sides], {code, 0, 2^n - 1}];
   best];
(* two index spans {a, b} (a < b) that cross: they interleave; a span over one of the indices of a *)
spansCrossQ[s_, t_] := (s[[1]] < t[[1]] < s[[2]] < t[[2]]) || (t[[1]] < s[[1]] < t[[2]] < s[[2]]);
spanOverQ[s_, a_] := AnyTrue[a, s[[1]] < # < s[[2]] &];
(* the conflicts of arcs (spans, the first ns sides) and parts attached to the axis (index lists attached, their ranges,
   the other sides) on the same side: two crossing arcs, an arc over a vertex of a part, two interleaved parts *)
axisConflictCount[sides_, spans_, attached_, ranges_] := With[{ns = Length[spans], nc = Length[attached]},
   Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
    Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && spanOverQ[spans[[i]], attached[[j]]], {i, ns}, {j, nc}]], True] +
    Count[Flatten[Table[sides[[ns + i]] == sides[[ns + j]] && (spansCrossQ[ranges[[i]], ranges[[j]]] || spanOverQ[ranges[[i]], attached[[j]]] ||
         spanOverQ[ranges[[j]], attached[[i]]]), {i, nc}, {j, i + 1, nc}]], True]];

(* Sides of the arcs (lines joining non-adjacent vertices of the axis of a two-point function) and of the parts hanging
   from the axis. On the same side, two crossing arcs conflict, and so do an arc and a part attached to a vertex under
   it, or two parts attached at interleaved vertices. The assignment with the fewest conflicts is chosen; then arcs and
   parts above, long arcs above. Result: {node pairs of the arcs below, nodes of the parts below}. *)
axisSides[backbone_, positions_, pairs_] := Module[{ix, spans, rest, components = {}, attached, ranges, ns, nc, best},
   ix[v_] := First[First[Position[backbone, v, {1}]]];
   spans = SortBy[DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[backbone, u] && MemberQ[backbone, v] && Abs[ix[u] - ix[v]] > 1 :>
        Sort[{ix[u], ix[v]}]]], {-(#[[2]] - #[[1]]), #[[1]]} &];
   rest = Select[positions[[All, 1]], ! MemberQ[backbone, #] &];
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, Select[pairs, MemberQ[rest, #[[1]]] && MemberQ[rest, #[[2]]] &]]]],
    {v, rest}];
   attached = Table[Union[Flatten[Table[ix /@ Select[neighboursOf[v, pairs], MemberQ[backbone, #] &], {v, c}]]], {c, components}];
   ranges = Table[If[a === {}, {0, 0}, {Min[a], Max[a]}], {a, attached}];
   ns = Length[spans]; nc = Length[components];
   Which[ns + nc == 0, {{}, {}},
    ns + nc > 10,
    (* too many to try them all: the arcs greedily, the parts above *)
    {arcsBelowAxis[backbone, pairs], {}},
    True,
    best = cheapestSides[ns + nc, Function[sides, 10 axisConflictCount[sides, spans, attached, ranges] +
        Total[Take[sides, ns]] + 0.5 Total[Drop[sides, ns]] + 0.01 Total[Table[sides[[i]] (spans[[i, 2]] - spans[[i, 1]]), {i, ns}]]]];
    {Table[Sort[{backbone[[spans[[i, 1]]]], backbone[[spans[[i, 2]]]]}], {i, Select[Range[ns], best[[#]] == 1 &]}],
     Flatten[Table[components[[j]], {j, Select[Range[nc], best[[ns + #]] == 1 &]}]]}]];

(* lines joining non-adjacent vertices of the axis of a two-point function (G, Sigma) are arcs; when two of them cross,
   one of them is drawn below the axis (greedily, longer arcs first above). The node pairs drawn below. *)
arcsBelowAxis[backbone_, pairs_] := Module[{ix, spans, above = {}, below = {}},
   ix[v_] := First[First[Position[backbone, v, {1}]]];
   spans = DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[backbone, u] && MemberQ[backbone, v] && Abs[ix[u] - ix[v]] > 1 :>
       Sort[{ix[u], ix[v]}]]];
   spans = SortBy[spans, {-(#[[2]] - #[[1]]), #[[1]]} &];
   Do[Which[
     ! AnyTrue[above, spansCrossQ[sp, #] &], AppendTo[above, sp],
     ! AnyTrue[below, spansCrossQ[sp, #] &], AppendTo[below, sp],
     True, AppendTo[above, sp]], {sp, spans}];
   Table[Sort[{backbone[[sp[[1]]]], backbone[[sp[[2]]]]}], {sp, below}]];

(* ---------------------------------------------------------------- round fermion loops *)
(* Closed fermion loops of three or more vertices are drawn as circles. A loop through the whole axis of a two-point
   function (a polarisation bubble) has the external points at the ends of a horizontal diameter, the vertices on the
   axis below and the others above; any other loop hangs from its vertex nearest to the external points, and what
   hangs from its other vertices moves with them. $loopCircles holds {loop, centre, radius} while a frame is drawn. *)
$loopCircles = {};
(* the radius of each blob node (blob[k] -> r), set by diagramGeometry *)
$blobRadii = {};

(* the end kind of a fermion line {u, v, h, style} at its node x: "psi" (an arrow into x, the field psi(x)) or "bar" (an
   arrow out of x, psibar(x)); an anomalous line -<T psi psi> has two psi ends, -<T psibar psibar> two bar ends *)
lineEndKind[l_, x_] := Which[directedStyleQ[l[[4]]], If[l[[2]] === x, "psi", "bar"], l[[4]] === "DivergingArrows", "psi", True, "bar"];
anomalousLinesQ[lines_List] := AnyTrue[lines, anomalousStyleQ[#[[4]]] &];

(* the loops, in fermion order: cycles of directed lines through vertices with one directed line in and one out; with
   anomalous lines, cycles through vertices with one psi end and one bar end (the arrows turn inside the anomalous lines) *)
fermionLoops[nodes_, lines_, minimumLength_ : 3] /; anomalousLinesQ[lines] := Module[
   {edges = Select[lines, fermionStyleQ[#[[4]]] && #[[1]] =!= #[[2]] && MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &],
    endsAt, goodQ, loops = {}, seen = {}, p, i, v, ok, next},
   endsAt[x_] := Select[Range[Length[edges]], MemberQ[edges[[#, {1, 2}]], x] &];
   goodQ[x_] := Length[endsAt[x]] == 2 && Sort[Table[lineEndKind[edges[[j]], x], {j, endsAt[x]}]] === {"bar", "psi"};
   Do[If[! MemberQ[seen, start] && goodQ[start],
     p = {start}; i = First[Select[endsAt[start], lineEndKind[edges[[#]], start] === "bar" &]];
     v = If[edges[[i, 1]] === start, edges[[i, 2]], edges[[i, 1]]]; ok = True;
     While[ok && v =!= start,
      If[! goodQ[v] || MemberQ[p, v], ok = False,
       AppendTo[p, v]; next = DeleteCases[endsAt[v], i]; i = First[next];
       v = If[edges[[i, 1]] === v, edges[[i, 2]], edges[[i, 1]]]]];
     If[ok && Length[p] >= minimumLength, AppendTo[loops, p]];
     seen = Union[seen, p]], {start, nodes}];
   loops];
fermionLoops[nodes_, lines_, minimumLength_ : 3] := Module[{succ, loops = {}, seen = {}, path, v, outs, ok},
   succ = Cases[lines, {u_, w_, _, st_} /; u =!= w && directedStyleQ[st] && MemberQ[nodes, u] && MemberQ[nodes, w] :> {u, w}];
   Do[If[! MemberQ[seen, start],
     path = {start}; v = start; ok = True;
     While[ok,
      outs = Cases[succ, {v, w_} :> w];
      If[Length[outs] != 1 || Count[succ, {_, outs[[1]]}] != 1, ok = False,
       v = outs[[1]];
       Which[v === start, ok = False,
        MemberQ[path, v], ok = False; path = {},
        True, AppendTo[path, v]]]];
     If[v === start && Length[path] >= minimumLength, AppendTo[loops, path]];
     seen = Union[seen, path, {start}]], {start, nodes}];
   loops];

(* closed loops of three vertices joined by undirected propagators (phi^4 bubbles with an insertion on one of their lines,
   tadpoles carrying a vertex): drawn round like fermion loops. Only those not sharing a vertex with the loops already
   taken; with several lines between two of their vertices only if they hang from one vertex of the axis or of the
   external points; with two such vertices only outside two-point functions *)
scalarTriangles[nodes_, lines_, taken_, anchors_, mode_] := Module[{ls, pairs, mult, tris = {}, used = Flatten[taken], na, ms},
   ls = Cases[lines, {u_, w_, _, st_} /; u =!= w && ! fermionStyleQ[st] && ! interactionStyleQ[st] && st =!= "Spoke" &&
       MemberQ[nodes, u] && MemberQ[nodes, w] :> Sort[{u, w}]];
   pairs = DeleteDuplicates[ls]; mult[pr_] := Count[ls, pr];
   Do[If[w =!= pr[[1]] && w =!= pr[[2]] && MemberQ[pairs, Sort[{pr[[1]], w}]] && MemberQ[pairs, Sort[{pr[[2]], w}]],
      AppendTo[tris, Sort[{pr[[1]], pr[[2]], w}]]], {pr, pairs}, {w, nodes}];
   tris = SortBy[DeleteDuplicates[tris], {Count[#, _?(MemberQ[anchors, #] &)] != 1, Max[mult /@ Subsets[#, {2}]]} &];
   Join @@ Table[na = Count[t, _?(MemberQ[anchors, #] &)]; ms = Max[mult[Sort[#]] & /@ Subsets[t, {2}]];
     If[Intersection[t, used] === {} && (na == 1 || (ms == 1 && (na == 0 || (na == 2 && (mode[[1]] =!= "backbone" ||
           Complement[mode[[2]], t] === {}))))),
      used = Join[used, t]; {t}, {}], {t, tris}]];

consecutiveOnLoopQ[loop_, u_, v_] := With[{i = Position[loop, u, {1}], j = Position[loop, v, {1}], n = Length[loop]},
   i =!= {} && j =!= {} && (Mod[j[[1, 1]] - i[[1, 1]], n] == 1 || Mod[i[[1, 1]] - j[[1, 1]], n] == 1)];

(* the arc of the circle {loop, centre, r} from pu to pv that does not contain the other vertices of the loop *)
circleKind[pu_, pv_, circle_, others_] := Module[{c = circle[[2]], r = circle[[3]], m = (pu + pv)/2, nrm = rotate90[unitVectorOf[pv - pu]],
    d = vectorNorm[pv - pu], h, sideOthers, bulge},
   h = Sqrt[Max[r^2 - d^2/4, 0.]];
   sideOthers = If[others === {}, -1., Sign[nrm . (Mean[others] - m)]];
   bulge = If[sideOthers == 0, 1., -sideOthers];
   {"circle", bulge If[bulge (nrm . (c - m)) > 10.^-9, r + h, r - h]}];

(* the two lines of a bubble drawn as its circle {loop, centre, r} through the two vertices: the short arc on the side away
   from the centre, the long one around it *)
bubbleCircleKind[pu_, pv_, circle_] := Module[{c = circle[[2]], r = circle[[3]], m = (pu + pv)/2, nrm = rotate90[unitVectorOf[pv - pu]],
    d = vectorNorm[pv - pu], h, sc},
   h = Sqrt[Max[r^2 - d^2/4, 0.]]; sc = If[nrm . (c - m) >= 0, 1., -1.];
   {"bubbleCircle", -sc (r - h), sc (r + h)}];

(* distances in the graph from a set of nodes: {{node, distance}, ...} *)
graphDistances[roots_, pairs_] := Module[{dist = Table[{x, 0}, {x, roots}], frontier = roots, next, d = 0},
   While[frontier =!= {},
    d++;
    next = Select[DeleteDuplicates[Flatten[Table[neighboursOf[v, pairs], {v, frontier}]]], ! MemberQ[Cases[dist, {w_, _} :> w], #] &];
    dist = Join[dist, Table[{v, d}, {v, next}]];
    frontier = next];
   dist];
distanceOf[dist_, v_] := With[{m = Select[dist, #[[1]] === v &, 1]}, If[m === {}, 10^6, m[[1, 2]]]];

(* move some nodes to new points, and with them the parts of the diagram that hang only from them *)
moveWithDependents[positions0_, newPoints_, loop_, stay_, pairs_] := Module[{positions = positions0, shift, others, rest, done = {}, comp,
    attached},
   shift = Table[{np[[1]], np[[2]] - Replace[np[[1]], Rule @@@ positions0]}, {np, newPoints}];
   others = Select[pairs, ! MemberQ[loop, #[[1]]] && ! MemberQ[loop, #[[2]]] &];
   rest = Select[positions0[[All, 1]], ! MemberQ[loop, #] && Head[#] =!= leg &];
   Do[If[! MemberQ[done, w],
     comp = componentOf[w, others]; done = Join[done, comp];
     attached = Select[loop, Intersection[neighboursOf[#, pairs], comp] =!= {} &];
     If[Intersection[comp, stay] === {} && attached =!= {} && Complement[attached, Cases[shift, {a_, _} :> a]] === {},
      With[{v = Mean[Table[Replace[a, Rule @@@ shift], {a, attached}]]},
       positions = Table[If[MemberQ[comp, p[[1]]], {p[[1]], p[[2]] + v}, p], {p, positions}]]]],
    {w, rest}];
   Table[With[{m = Select[newPoints, #[[1]] === p[[1]] &, 1]}, If[m === {}, p, {p[[1]], m[[1, 2]]}]], {p, positions}]];

(* the links {v, w} of a loop to the vertices w nearer to the external points (v on the loop) *)
attachmentLinks[loop_, pairs_, dist_] := Flatten[Table[{v, w}, {v, loop},
    {w, Select[neighboursOf[v, pairs], ! MemberQ[loop, #] && distanceOf[dist, #] < distanceOf[dist, v] &]}], 1];

(* the vertices that stay when a loop is moved (those not hanging from it alone), and the lines between them *)
staticPartsFor[loop_, positions_, pairs_, stay_] := Module[{others = Select[pairs, ! MemberQ[loop, #[[1]]] && ! MemberQ[loop, #[[2]]] &],
    rest = Select[positions[[All, 1]], ! MemberQ[loop, #] && Head[#] =!= leg &], done = {}, comp, moving = {}, nodes, point},
   point[v_] := Replace[v, Rule @@@ positions];
   Do[If[! MemberQ[done, w], comp = componentOf[w, others]; done = Join[done, comp];
     If[Intersection[comp, stay] === {}, moving = Join[moving, comp]]], {w, rest}];
   nodes = Select[rest, ! MemberQ[moving, #] &];
   {nodes, Table[{point[pr[[1]]], point[pr[[2]]]}, {pr, Select[others, MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &]}]}];

(* the best circle of radius r for a loop attached by the links {v, w} (v on the loop, w a placed vertex): centres
   around the attached vertices, every rotation and both orientations; short links that cross nothing, no vertex or
   line inside the circle, near the preferred centre. Result: {centre, {{v, point}, ...}} *)
placeLoopCircle[loop_, r_, links_, point_, staticNodes_, staticPoints_, preferred_] := Module[{n = Length[loop], m, candidates,
    bestScore = Infinity, good = {}, pts, segs, base, score, ends, at, nodePoints, near, normal, choice},
   ends = Table[point[l[[2]]], {l, links}];
   at = Table[First[First[Position[loop, l[[1]], {1}]]], {l, links}];
   nodePoints = Table[point[w], {w, staticNodes}];
   m = Mean[ends];
   normal = rotate90[unitVectorOf[preferred - m]];
   (* {centre, radius}: away from the attached vertices (at the preferred side or around), or among them with a
      radius that leaves room for the links *)
   near[c_] := {c, Min[r, Max[0.35, Min[Table[vectorNorm[q - c], {q, ends}]] - 0.5]]};
   candidates = Join[{{preferred, r}}, Table[{m + (r + 0.8) directionAt[Pi k/6.], r}, {k, 0, 11}],
     {near[m]}, Table[near[m + 0.3 directionAt[Pi k/3.]], {k, 0, 5}]];
   (* the penalties (crossings) are computed only for the candidates that can still be among the best *)
   Do[With[{c = cand[[1]], rc = cand[[2]]},
     base = 0.3 vectorNorm[c - preferred] + 2. (r - rc) + 8 (Count[nodePoints, q_ /; vectorNorm[q - c] < rc + 0.15] +
          Count[staticPoints, sg_ /; segmentDistance[c, sg] < rc - 0.05]) + 6 Count[staticPoints, sg_ /; rc - 0.05 <= segmentDistance[c, sg] < rc + 0.2];
     If[base < bestScore + 0.3,
      Do[pts = Table[c + rc directionAt[th + s 2. Pi (k - 1)/n], {k, n}];
       score = base + Total[Table[(pts[[at[[i]]]] - ends[[i]]) . (pts[[at[[i]]]] - ends[[i]]), {i, Length[links]}]];
       If[score < bestScore + 0.3,
        segs = Table[{pts[[at[[i]]]], ends[[i]]}, {i, Length[links]}];
        (* crossings, and links that start into the circle instead of away from it *)
        score = score + 8 (mutualCrossingCount[segs] + crossingCount[segs, staticPoints] +
            Count[Table[(ends[[i]] - pts[[at[[i]]]]) . (pts[[at[[i]]]] - c) < 0, {i, Length[links]}], True]);
        If[score < bestScore + 0.3, bestScore = Min[bestScore, score];
         AppendTo[good, {score, Abs[(c - m) . normal], c, Table[{loop[[k]], pts[[k]]}, {k, n}], rc}]]],
       {s, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}]]],
    {cand, candidates}];
   (* among the candidates nearly as good as the best, the one nearest to the axis of the attached vertices in the
      preferred direction: symmetric diagrams stay symmetric *)
   choice = First[SortBy[Select[good, #[[1]] <= bestScore + 0.3 &], {Round[#[[2]], 0.01], #[[1]]} &]];
   {choice[[3]], choice[[4]], choice[[5]]}];

roundFermionLoops[positions0_, lines_, mode_, fixed_, pairs_] := Module[{positions = positions0, loops, circles = {}, point, roots, dist,
    backbone, a, b, pa, pb, c, r, rotated, j, pathAB, pathBA, lowerPath, upperPath, newPoints, anchor, parents, u, n, area, orient, th0, p,
    flip = False},
   point[v_] := Replace[v, Rule @@@ positions];
   (* bubbles (loops of two vertices) hanging from the axis of a two-point function are moved away from it, with what
      hangs from them, so that their two arcs keep clear of the axis *)
   If[mode[[1]] === "backbone" && ! mode[[3]],
    Do[If[Intersection[bubble, mode[[2]]] === {},
      With[{pu = point[bubble[[1]]], pv = point[bubble[[2]]]},
       With[{h = (pu[[2]] + pv[[2]])/2, need = 0.45 vectorNorm[pv - pu] + 0.4},
        If[0.01 < Abs[h] < need && Abs[pu[[2]] - pv[[2]]] < 0.3,
         With[{part = componentOf[bubble[[1]], Select[pairs, ! MemberQ[mode[[2]], #[[1]]] && ! MemberQ[mode[[2]], #[[2]]] &]],
           dy = Sign[h] (need - Abs[h])},
          positions = Table[If[MemberQ[part, p[[1]]], {p[[1]], p[[2]] + {0., dy}}, p], {p, positions}]]]]]],
     {bubble, Select[fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, 2], Length[#] == 2 &]}]];
   loops = fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines];
   roots = Which[MemberQ[{"backbone", "cycle"}, mode[[1]]], mode[[2]], fixed =!= {}, fixed, True, Take[positions0[[All, 1]], 1]];
   If[mode[[1]] =!= "loops", loops = Join[loops, scalarTriangles[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, loops,
       Join[roots, fixed], mode]]];
   If[mode[[1]] === "cycle", loops = Select[loops, Sort[#] =!= Sort[mode[[2]]] &]];
   dist = graphDistances[roots, pairs];
   (* in vertex functions, a bubble joined to the rest at both its vertices is placed like a round loop (it is drawn
      with two arcs between its vertices) *)
   If[mode[[1]] === "circle",
    Do[With[{links = attachmentLinks[bubble, pairs, dist]},
       If[Length[DeleteDuplicates[Table[l[[1]], {l, links}]]] == 2,
        With[{static = staticPartsFor[bubble, positions, pairs, Join[roots, fixed]], m = Mean[Table[point[l[[2]]], {l, links}]],
          now = Mean[Table[point[v], {v, bubble}]]},
         With[{best = placeLoopCircle[bubble, 0.5, links, point, static[[1]], static[[2]],
             m + 1.3 If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]]},
          positions = moveWithDependents[positions, best[[2]], bubble, Join[roots, fixed], pairs]]]]],
     {bubble, Select[fermionLoops[Select[positions0[[All, 1]], Head[#] =!= leg &], lines, 2], Length[#] == 2 &]}]];
   (* loops nearer to the external points first: a loop hanging from another one follows it *)
   loops = SortBy[loops, Min[Table[distanceOf[dist, v], {v, #}]] &];
   Do[
    newPoints = None;
    Which[
     mode[[1]] === "backbone" && ! mode[[3]] && Length[mode[[2]]] >= 2 && backboneSegmentOfLoop[mode[[2]], loop] =!= None,
     (* a loop through the whole axis, or through a stretch of it (a loop closed on an external point): the circle on the
        stretch as a diameter, its vertices off the axis on the arcs *)
     backbone = mode[[2]]; {a, b} = backboneSegmentOfLoop[backbone, loop]; pa = point[a]; pb = point[b];
     c = (pa + pb)/2; r = vectorNorm[pb - pa]/2;
     rotated = RotateLeft[loop, First[First[Position[loop, a, {1}]]] - 1];
     j = First[First[Position[rotated, b, {1}]]];
     pathAB = Table[rotated[[i]], {i, 2, j - 1}];
     pathBA = Reverse[Table[rotated[[i]], {i, j + 1, Length[rotated]}]];
     If[Complement[pathAB, backbone] === {}, lowerPath = pathAB; upperPath = pathBA, lowerPath = pathBA; upperPath = pathAB];
     (* the external points joined by a propagator, everything else on the other line of the loop: that line is drawn
        above, and the picture is reflected if it runs from a to b, so that the loop turns counterclockwise and an
        insertion on one or the other line of a bubble is drawn above or below it *)
     If[Length[backbone] == 2 && pathAB =!= {} && pathBA === {}, flip = True];
     newPoints = Join[
       Table[{lowerPath[[k]], c + r directionAt[Pi + Pi k/(Length[lowerPath] + 1)]}, {k, Length[lowerPath]}],
       Table[{upperPath[[k]], c + r directionAt[Pi - Pi k/(Length[upperPath] + 1)]}, {k, Length[upperPath]}]];
     AppendTo[circles, {loop, c, r}],
     Intersection[loop, roots] === {} && Intersection[loop, fixed] === {} &&
      Length[DeleteDuplicates[Table[l[[1]], {l, attachmentLinks[loop, pairs, dist]}]]] >= 2,
     (* attached at several vertices: the circle where the links are short and cross nothing; a loop of three vertices
        attached at two of them is placed like a bubble between these two, the third vertex on one of its arcs *)
     n = Length[loop]; r = 0.55/Sin[Pi/n];
     With[{links = attachmentLinks[loop, pairs, dist], static = staticPartsFor[loop, positions, pairs, Join[roots, fixed]]},
      With[{m = Mean[Table[point[l[[2]]], {l, links}]], now = Mean[Table[point[v], {v, loop}]],
        attached = DeleteDuplicates[Table[l[[1]], {l, links}]]},
       If[n == 3 && Length[attached] == 2,
        With[{best = placeLoopCircle[attached, 0.5, links, point, static[[1]], static[[2]],
            m + 1.3 If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]], w = First[Complement[loop, attached]]},
         With[{pp = best[[2, 1, 2]], pq = best[[2, 2, 2]]}, With[{nrm = rotate90[unitVectorOf[pq - pp]], mid = (pp + pq)/2},
           c = mid; r = vectorNorm[pq - pp]/2;
           newPoints = Append[best[[2]], {w, mid + r If[nrm . (point[w] - mid) < 0, -1., 1.] nrm}]]]],
        With[{best = placeLoopCircle[loop, r, links, point, static[[1]], static[[2]],
            m + (r + 0.8) If[vectorNorm[now - m] < 0.05, {0., 1.}, unitVectorOf[now - m]]]},
         c = best[[1]]; newPoints = best[[2]]; r = best[[3]]]]]];
     AppendTo[circles, {loop, c, r}],
     (* a loop through two external points (not on the axis of a two-point function): the circle through them, the
        other vertices on its arc on their side *)
     mode[[1]] =!= "backbone" && Length[Intersection[loop, Join[roots, fixed]]] == 2 && Length[loop] == 3,
     With[{ends = Intersection[loop, Join[roots, fixed]], w = First[Complement[loop, Join[roots, fixed]]]},
      With[{pp = point[ends[[1]]], pq = point[ends[[2]]]}, With[{nrm = rotate90[unitVectorOf[pq - pp]], m = (pp + pq)/2},
        With[{side = If[Abs[nrm . (point[w] - m)] > 0.05, Sign[nrm . (point[w] - m)], If[nrm . m < 0, -1., 1.]]},
         c = m; r = vectorNorm[pq - pp]/2; newPoints = {{w, m + side r nrm}}]]]];
     AppendTo[circles, {loop, c, r}],
     Length[Intersection[loop, Join[roots, fixed]]] <= 1,
     anchor = First[SortBy[loop, {distanceOf[dist, #], First[First[Position[loop, #, {1}]]]} &]];
     parents = Select[neighboursOf[anchor, pairs], ! MemberQ[loop, #] && distanceOf[dist, #] < distanceOf[dist, anchor] &];
     (* a loop hanging from a vertex of the axis or from an external point: standing on it, on the side of its vertices *)
     u = Which[parents =!= {}, unitVectorOf[point[anchor] - point[First[parents]]],
       ! MemberQ[Join[roots, fixed], anchor], {0., 1.},
       mode[[1]] === "backbone", {0., If[Mean[Table[point[w][[2]], {w, Complement[loop, {anchor}]}]] < 0, -1., 1.]},
       True, With[{dv = Mean[Table[point[w], {w, Complement[loop, {anchor}]}]] - point[anchor]},
        If[vectorNorm[dv] < 0.05, {0., 1.}, unitVectorOf[dv]]]];
     n = Length[loop]; r = 0.55/Sin[Pi/n]; c = point[anchor] + r u;
     rotated = RotateLeft[loop, First[First[Position[loop, anchor, {1}]]] - 1];
     area = Total[Table[With[{p = point[rotated[[k]]], q = point[rotated[[Mod[k, n] + 1]]]}, p[[1]] q[[2]] - q[[1]] p[[2]]], {k, n}]];
     orient = If[area < 0, -1., 1.];
     th0 = angleOf[-u];
     newPoints = Table[{rotated[[k]], c + r directionAt[th0 + orient 2. Pi (k - 1)/n]}, {k, 2, n}];
     AppendTo[circles, {loop, c, r}]];
    If[newPoints =!= None && newPoints =!= {},
     positions = moveWithDependents[positions, newPoints, loop, Join[roots, fixed], pairs]],
    {loop, loops}];
   If[flip, positions = Table[{q[[1]], {q[[2, 1]], -q[[2, 2]]}}, {q, positions}];
    circles = Table[{q[[1]], {q[[2, 1]], -q[[2, 2]]}, q[[3]]}, {q, circles}]];
   {positions, circles}];

(* the ends {a, b} of the stretch of the axis (backbone) that a loop runs along: two or more consecutive vertices of the
   axis on the loop, joined around the loop by a path through them only; None if there is no such stretch *)
backboneSegmentOfLoop[backbone_, loop_] := Module[{ix = Flatten[Position[backbone, #, {1}] & /@ Select[loop, MemberQ[backbone, #] &]], seg,
    rotated, j, pathAB, pathBA},
   If[Length[ix] < 2 || Max[ix] - Min[ix] + 1 != Length[ix], Return[None]];
   seg = Take[backbone, {Min[ix], Max[ix]}];
   If[Complement[backbone, loop] === {}, Return[{First[backbone], Last[backbone]}]];
   rotated = RotateLeft[loop, First[First[Position[loop, First[seg], {1}]]] - 1];
   j = First[First[Position[rotated, Last[seg], {1}]]];
   pathAB = Table[rotated[[i]], {i, 2, j - 1}]; pathBA = Table[rotated[[i]], {i, j + 1, Length[rotated]}];
   If[(Complement[pathAB, seg] === {} || Complement[pathBA, seg] === {}) && Length[loop] > Length[seg], {First[seg], Last[seg]}, None]];

axisPositions[path_, spacing_, minimumHalfWidth_] := With[{L = Length[path] - 1}, With[{X = Max[minimumHalfWidth, L spacing/2]},
    Table[{path[[i]], {If[L == 0, 0., -X + 2. X (i - 1)/L], 0.}}, {i, Length[path]}]]];

(* leaf order of a tree, starting from the first external label (planar placement) *)
treeLeafOrder[ext_, pairs_] := Module[{order = {}, minimumIndex, visit},
   minimumIndex[v_, p_] := Min[Join[If[MemberQ[ext, v], {First[First[Position[ext, v, {1}]]]}, {}],
      minimumIndex[#, v] & /@ DeleteCases[neighboursOf[v, pairs], p]]];
   visit[v_, p_] := (If[MemberQ[ext, v], AppendTo[order, v]];
     Scan[visit[#, v] &, SortBy[DeleteCases[neighboursOf[v, pairs], p], minimumIndex[#, v] &]]);
   visit[First[ext], None];
   Join[order, Complement[ext, order]]];

(* if the graph is a forest, the leaf orders of its trees one after the other (planar placement); else the given order *)
forestLeafOrder[ext_, free_, pairs_] := Module[{components = {}},
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, pairs]]], {v, ext}];
   If[Length[pairs] =!= Length[ext] + Length[free] - Length[components], ext,
    Join @@ Table[treeLeafOrder[Select[ext, MemberQ[c, #] &], Select[pairs, MemberQ[c, #[[1]]] &]], {c, components}]]];

(* ---------------------------------------------------------------- layouts *)
(* Layout of the components that contain external labels. externalNodes: the node of each external label (in order);
   amputated: True for the labels drawn with a leg. Legs end on the virtual nodes leg[label].
   Result: {positions, mode}. *)
layoutWithExternals[externalLabels_, externalNodes_, amputated_, free_, pairs_, propagatorPairs_, externalOrder_] := Module[
   {n = Length[externalLabels], backbone = None, order, R, th, positions, core, stripped, legPairs, fixed, fixedNodes,
    freeNodes, labelDirection, cornerLegNodes = {}, below = {}, paths, pathNodes, arcs},
   legPairs = Join @@ Table[If[amputated[[k]], {{externalNodes[[k]], leg[externalLabels[[k]]]}}, {}], {k, n}];
   If[n == 2 || n == 1,
    backbone = If[n == 1, {externalNodes[[1]]},
      With[{p = shortestPath[externalNodes[[1]], externalNodes[[2]], propagatorPairs]},
       If[p =!= None, p, shortestPath[externalNodes[[1]], externalNodes[[2]], pairs]]]]];
   If[backbone =!= None,
    (* a two-point function: the path joining the external points on the horizontal axis *)
    positions = axisPositions[backbone, If[Length[backbone] <= 3, 1.1, 0.95], If[Length[backbone] == 1, 0., 0.9]];
    positions = layeredLayout[positions, Select[DeleteDuplicates[free], ! MemberQ[backbone, #] &], pairs];
    (* a one-point function whose vertex is joined to a single other vertex by a bundle of lines (a melon, a bubble):
       the bundle along the axis, on the right of the vertex, so that the leg does not leave along one of its arcs *)
    If[n == 1 && amputated[[1]] && With[{nb = neighboursOf[externalNodes[[1]], pairs]},
        Length[nb] == 1 && MemberQ[$multiplePairs, Sort[{externalNodes[[1]], nb[[1]]}]]],
     With[{p0 = Replace[externalNodes[[1]], Rule @@@ positions]},
      positions = Table[{q[[1]], p0 + {q[[2, 2]] - p0[[2]], -(q[[2, 1]] - p0[[1]])}}, {q, positions}]]];
    (* arcs and hanging parts above or below the axis; the parts below are reflected *)
    With[{sides = axisSides[backbone, positions, pairs]},
     below = sides[[1]];
     positions = Table[If[MemberQ[sides[[2]], p[[1]]], {p[[1]], {1, -1} p[[2]]}, p], {p, positions}]];
    positions = Join[positions, Join @@ Table[If[amputated[[k]],
        With[{p = Replace[externalNodes[[k]], Rule @@@ positions]}, {{leg[externalLabels[[k]]], p + If[k == 1, {-$legLength, 0.}, {$legLength, 0.}]}}],
        {}], {k, n}]];
    {positions, {"backbone", backbone, False, below, {0., -0.6}}},
    (* the external points on a circle, counterclockwise from the top left; the node of several external labels
       (joined by delta functions) at their mean position; the legs point away in the directions of their labels *)
    order = Which[ListQ[externalOrder], Join[Select[externalOrder, MemberQ[externalLabels, #] &], Complement[externalLabels, externalOrder]],
      externalOrder === Automatic && n >= 3 && free =!= {} && ! Or @@ amputated,
      forestLeafOrder[externalNodes, free, pairs] /. Thread[externalNodes -> externalLabels],
      True, externalLabels];
    (* one node with two external labels at opposite corners, the other two labels on other nodes, or two nodes each
       with a pair at opposite corners (a bubble in the channel (x1 x3|x2 x4)): the labels at the right corners are
       exchanged, so that the legs of a node are neighbours and nothing crosses *)
    (* an external point at the end of a single line counts as the vertex it hangs from (two points joined to one vertex
       of phi^4 theory are placed like two labels of one node) *)
    If[n == 4 && ! ListQ[externalOrder],
     With[{attach = Table[With[{v = externalNodes[[k]]}, With[{nb = neighboursOf[v, pairs]},
          If[Count[externalNodes, v] == 1 && Length[nb] == 1 && MemberQ[free, First[nb]], First[nb], v]]], {k, n}]},
      With[{corners = Table[Sort[Table[First[First[Position[order, externalLabels[[k]], {1}]]],
            {k, Select[Range[n], attach[[#]] === node &]}]], {node, DeleteDuplicates[attach]}]},
       If[(Count[corners, {1, 3} | {2, 4}] == 1 && Length[DeleteDuplicates[attach]] == 3) ||
         (Count[corners, {1, 3} | {2, 4}] == 2 && Length[DeleteDuplicates[attach]] == 2), order = order[[{1, 2, 4, 3}]]]]]];
    (* three labels on two nodes and nothing else (a bubble of a three-point vertex): the order turned so that the node
       of two labels is on the left, its legs at the corners on the left, and the third leg points to the right *)
    If[n == 3 && ! ListQ[externalOrder] && Length[DeleteDuplicates[externalNodes]] == 2 &&
      Complement[DeleteDuplicates[free], externalNodes] === {},
     With[{single = First[Select[Range[n], Count[externalNodes, externalNodes[[#]]] == 1 &]]},
      order = RotateLeft[order, First[First[Position[order, externalLabels[[single]], {1}]]]]]];
    th = Which[n == 1, {Pi}, n == 2, {Pi, 0.}, True, Table[Pi - Pi/n + 2. Pi (k - 1)/n, {k, n}]];
    fixedNodes = DeleteDuplicates[externalNodes];
    freeNodes = Select[DeleteDuplicates[free], ! MemberQ[fixedNodes, #] &];
    R = If[freeNodes === {}, If[Or @@ amputated, 1.1, 1.], Min[1.7, 1.15 + 0.1 (Length[freeNodes] - 1)]];
    labelDirection[e_] := directionAt[th[[First[First[Position[order, e, {1}]]]]]];
    fixed = Table[{v, R Mean[Table[labelDirection[externalLabels[[k]]], {k, Select[Range[n], externalNodes[[#]] === v &]}]]}, {v, fixedNodes}];
    (* two nodes at the same place (legs 1, 3 on one node and 2, 4 on the other: a crossed channel) are moved apart
       horizontally, the node of the first label on the left; their legs keep the vertical side of their labels and
       point away from the centre, so that the picture is a plain bubble with the labels exchanged on the right *)
    cornerLegNodes = {};
    Do[If[vectorNorm[fixed[[i, 2]] - fixed[[j, 2]]] < 0.3,
      fixed[[i, 2]] = fixed[[i, 2]] + {-0.5 R, 0.}; fixed[[j, 2]] = fixed[[j, 2]] + {0.5 R, 0.};
      cornerLegNodes = Union[cornerLegNodes, {fixed[[i, 1]], fixed[[j, 1]]}]],
     {i, Length[fixed]}, {j, i + 1, Length[fixed]}];
    (* all the legs on two vertices and nothing else (a bubble of a four-point vertex): the vertices as far apart in
       every channel *)
    If[Length[fixed] == 2 && freeNodes === {} && And @@ amputated,
     With[{dd = vectorNorm[fixed[[1, 2]] - fixed[[2, 2]]]}, If[dd > 1.15, fixed = Table[{f[[1]], f[[2]] 1.1/dd}, {f, fixed}]]]];
    (* no free vertices and a bubble (two or more lines) between two of three or more external points: the points are
       brought closer, so that the bubble keeps about the width of a bubble and the picture the size of its neighbours *)
    If[Length[fixed] >= 3 && freeNodes === {},
     With[{dmax = Max[Join[{0.}, Table[vectorNorm[Replace[pr[[1]], Rule @@@ fixed] - Replace[pr[[2]], Rule @@@ fixed]],
          {pr, Select[$multiplePairs, MemberQ[fixedNodes, #[[1]]] && MemberQ[fixedNodes, #[[2]]] &]}]]]},
      If[dmax > 1.15, fixed = Table[{f[[1]], f[[2]] 1.15/dmax}, {f, fixed}]]]];
    (* the open propagator lines (fermion lines) between two external points run straight from one to the other,
       with their vertices evenly spaced; lines joining two vertices of such a line are arcs, outside or inside *)
    (* only the lines between neighbouring external points (sides of the polygon); lines across it are placed with the
       rest *)
    paths = With[{around = SortBy[fixed, angleOf[#[[2]]] &][[All, 1]]},
      Select[openPropagatorPaths[fixedNodes, freeNodes, propagatorPairs], Function[path,
        With[{i = First[First[Position[around, path[[1]], {1}]]], j = First[First[Position[around, Last[path], {1}]]]},
         Mod[i - j, Length[around]] == 1 || Mod[j - i, Length[around]] == 1]]]];
    pathNodes = Union[Flatten[Table[Take[Drop[path, 1], Length[path] - 2], {path, paths}]]];
    (* room for the vertices of long lines: the picture is stretched along them *)
    With[{stretch = Table[With[{d = Replace[Last[path], Rule @@@ fixed] - Replace[path[[1]], Rule @@@ fixed]},
         With[{need = If[vectorNorm[d] < 10.^-6, 1., Max[1., 0.8 (Length[path] - 1)/vectorNorm[d]]]},
          {1. + (need - 1.) Abs[unitVectorOf[d][[1]]], 1. + (need - 1.) Abs[unitVectorOf[d][[2]]]}]], {path, paths}]},
     If[stretch =!= {},
      With[{sx = Min[1.7, Max[stretch[[All, 1]]]], sy = Min[1.7, Max[stretch[[All, 2]]]]},
       fixed = Table[{f[[1]], {sx f[[2, 1]], sy f[[2, 2]]}}, {f, fixed}]]]];
    fixed = Join[fixed, Flatten[Table[With[{pu = Replace[path[[1]], Rule @@@ fixed], pv = Replace[Last[path], Rule @@@ fixed], L = Length[path] - 1},
        Table[{path[[k]], pu + (k - 1)/L (pv - pu)}, {k, 2, L}]], {path, paths}], 1]];
    {core, stripped} = stripPendantVertices[fixed[[All, 1]], Select[freeNodes, ! MemberQ[pathNodes, #] &], pairs];
    (* a vertex on a leg (two lines, one to an external point) with something hanging from it (a tadpole, a chain of
       them): nearer the external point, where the legs are farther apart, and the legs longer, so that the hanging part
       is not wedged between two legs *)
    With[{hanging = Select[core, Length[neighboursOf[#, pairs]] == 2 && Intersection[neighboursOf[#, pairs], fixed[[All, 1]]] =!= {} &&
          (MemberQ[$selfLoopNodes, #] || Intersection[neighboursOf[#, pairs], stripped] =!= {}) &]},
     If[hanging =!= {} && paths === {},
      fixed = Table[{f[[1]], 1.15 f[[2]]}, {f, fixed}]];
     positions = Join[fixed, tutteEmbedding[fixed, core, pairs, hanging]]];
    positions = placePendantVertices[positions, stripped, pairs, {0., 0.}, fixedNodes];
    positions = pushApartVertices[positions, Select[freeNodes, ! MemberQ[pathNodes, #] &], 0.5];
    positions = Join[positions, Join @@ Table[If[amputated[[k]],
        {{leg[externalLabels[[k]]], With[{p = Replace[externalNodes[[k]], Rule @@@ positions], u = labelDirection[externalLabels[[k]]]},
           If[MemberQ[cornerLegNodes, externalNodes[[k]]],
            p + $legLength {If[p[[1]] < 0, -1., 1.], If[u[[2]] < 0, -1., 1.]}/Sqrt[2.],
            p + $legLength u]]}}, {}], {k, n}]];
    arcs = Flatten[Table[pathArcSides[path, pairs, positions, {0., 0.}], {path, paths}], 1];
    positions = refineFreeVertices[positions, Select[freeNodes, ! MemberQ[pathNodes, #] &],
      Join[Select[pairs, ! MemberQ[Table[Sort[a[[1]]], {a, arcs}], Sort[#]] &], legPairs]];
    {positions, {"circle", arcs, {0., 0.}}}]];

(* the open propagator lines between external points: {fixed node, free vertices on two propagator lines each, another
   fixed node} *)
openPropagatorPaths[fixedNodes_, freeNodes_, propagatorPairs_] := Module[{paths = {}, used = {}, path, prev, cur, nb},
   Do[Do[If[MemberQ[freeNodes, w] && ! MemberQ[used, w],
       path = {u, w}; prev = u; cur = w;
       While[MemberQ[freeNodes, cur] && Length[neighboursOf[cur, propagatorPairs]] == 2,
        nb = DeleteCases[neighboursOf[cur, propagatorPairs], prev];
        If[Length[nb] != 1, Break[]];
        prev = cur; cur = First[nb]; AppendTo[path, cur]];
       If[MemberQ[fixedNodes, Last[path]] && Last[path] =!= u && ! MemberQ[Most[path], Last[path]],
        AppendTo[paths, path]; used = Join[used, path]]],
      {w, neighboursOf[u, propagatorPairs]}], {u, fixedNodes}];
   paths];

(* sides of the lines joining non-consecutive vertices of an open propagator line drawn straight: two arcs crossing on
   the same side, or an arc inside the picture over a vertex joined to something inside, conflict; the assignment with
   the fewest conflicts, arcs outside preferred (more than 12 arcs: all outside). Result: {{u, v}, rank, sign} with the
   sign of the sagitta (left of u -> v) *)
pathArcSides[path_, pairs_, positions_, centre_] := Module[{ix, spans, attached, ns, best, point},
   point[v_] := Replace[v, Rule @@@ positions];
   ix[v_] := First[First[Position[path, v, {1}]]];
   spans = SortBy[DeleteDuplicates[Cases[pairs, {u_, v_} /; MemberQ[path, u] && MemberQ[path, v] && Abs[ix[u] - ix[v]] > 1 :>
        Sort[{ix[u], ix[v]}]]], {#[[2]] - #[[1]], #[[1]]} &];
   attached = Select[Range[2, Length[path] - 1], Function[k, AnyTrue[neighboursOf[path[[k]], pairs], ! MemberQ[path, #] &]]];
   ns = Length[spans];
   If[ns == 0, {},
    best = If[ns > 12, Table[0, {ns}], cheapestSides[ns, Function[sides,
        10 (Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
           Count[Table[sides[[i]] == 1 && spanOverQ[spans[[i]], attached], {i, ns}], True]) + Total[sides]]]];
    Table[With[{u = path[[spans[[i, 1]]]], v = path[[spans[[i, 2]]]]},
      With[{out = If[# == 0, 1., #] &[Sign[rotate90[point[v] - point[u]] . ((point[u] + point[v])/2 - centre)]]},
       {{u, v}, Count[Take[best, i], best[[i]]], If[best[[i]] == 1, -out, out]}]], {i, ns}]]];

(* A free vertex lying on a line that does not end at it, or on top of another vertex, is moved a little (by 0.25 or
   0.45, in eight directions) to the nearest place free of these defects where its lines cross no more lines than
   before (lines are taken as straight). *)
refineFreeVertices[positions_, free_, pairs_] := Module[{nodes = positions[[All, 1]], pts = positions[[All, 2]], ix, segs, freeIx,
    incident, othersOf, neighbourOf, bad, crossings, n, b0, c0, best, q},
   n = Length[nodes];
   ix[v_] := First[First[Position[nodes, v, {1}]]];
   segs = Table[{ix[pr[[1]]], ix[pr[[2]]]}, {pr, Select[pairs, MemberQ[nodes, #[[1]]] && MemberQ[nodes, #[[2]]] &]}];
   freeIx = ix /@ Select[free, MemberQ[nodes, #] &];
   incident = Table[Select[segs, MemberQ[#, i] &], {i, n}];
   othersOf = Table[Select[segs, ! MemberQ[#, i] &], {i, n}];
   neighbourOf[sg_, i_] := If[sg[[1]] == i, sg[[2]], sg[[1]]];
   (* the vertex i at q on another line, another vertex on one of its lines (their other ends excepted), vertices on top
      of each other *)
   bad[i_, q_] := With[{rest = Delete[pts, i], mine = Table[{q, pts[[neighbourOf[sg, i]]]}, {sg, incident[[i]]}]},
     countBelow[distancesFromPoint[q, Table[{pts[[sg[[1]]]], pts[[sg[[2]]]]}, {sg, othersOf[[i]]}]], 0.18] +
      Total[Table[countBelow[distancesToSegment[rest, sg], 0.18] - 1, {sg, mine}]] +
      countBelow[Sqrt[(rest[[All, 1]] - q[[1]])^2 + (rest[[All, 2]] - q[[2]])^2], 0.4]];
   crossings[i_, q_] := crossingCount[Table[{q, pts[[neighbourOf[sg, i]]]}, {sg, incident[[i]]}],
     Table[{pts[[sg[[1]]]], pts[[sg[[2]]]]}, {sg, othersOf[[i]]}]];
   Do[b0 = bad[i, pts[[i]]];
    If[b0 > 0,
     c0 = crossings[i, pts[[i]]]; best = None;
     (* vertical moves first: they keep a vertex on the vertical axis of a symmetric picture *)
     Do[If[best === None,
       q = pts[[i]] + dd directionAt[Pi k/4.];
       If[bad[i, q] == 0 && crossings[i, q] <= c0, best = q]], {dd, {0.25, 0.45}}, {k, {2, 6, 0, 4, 1, 3, 5, 7}}];
     If[best =!= None, pts[[i]] = best]],
    {i, freeIx}];
   Table[{nodes[[i]], pts[[i]]}, {i, n}]];

(* A vacuum part made of several fermion loops (and single vertices) joined by interaction lines: every loop is a circle,
   two side by side, more around a circle in the order in which the lines between them cross least; then each loop is
   rotated, and its orientation chosen, so that these lines are short and cross neither each other nor a loop.
   Result: {positions, {"loops", circles, centre}} *)
layoutVacuumLoops[component_, loops_, pairs_] := Module[{units, radii, k, unitOf, links, superEdges, orders, crossings, order, theta,
    bubbleCircles, R, centres, angles, signs, positionsOf, linkData, vertexPoint, segmentsOf, lengthEnergy, penalty, best, e, pts, circles, concentric},
   units = Join[loops, List /@ Select[component, ! MemberQ[Flatten[loops], #] &]];
   radii = Table[Which[Length[u] == 1, 0., Length[u] == 2, 0.5, True, 0.55/Sin[Pi/Length[u]]], {u, units}];
   k = Length[units];
   unitOf[v_] := First[First[Position[units, v, {2}]]];
   links = Select[pairs, unitOf[#[[1]]] =!= unitOf[#[[2]]] &];
   superEdges = DeleteDuplicates[Table[Sort[{unitOf[l[[1]]], unitOf[l[[2]]]}], {l, links}]];
   (* the order of the units around the circle: fewest crossings of the lines between them, then neighbours joined *)
   crossings[o_] := Module[{pos = Table[First[First[Position[o, i, {1}]]], {i, k}], between},
     between[a_, b_, x_] := With[{lo = Min[pos[[a]], pos[[b]]], hi = Max[pos[[a]], pos[[b]]]}, lo < pos[[x]] < hi];
     Count[Flatten[Table[With[{s1 = superEdges[[i]], s2 = superEdges[[j]]},
         Length[Union[s1, s2]] == 4 && Xor[between[s1[[1]], s1[[2]], s2[[1]]], between[s1[[1]], s1[[2]], s2[[2]]]]],
        {i, Length[superEdges]}, {j, i + 1, Length[superEdges]}]], True] +
      0.01 Total[Table[With[{d = Abs[pos[[se[[1]]]] - pos[[se[[2]]]]]}, Min[d, k - d]], {se, superEdges}]]];
   orders = If[k <= 6, Table[Prepend[q, 1], {q, Permutations[Range[2, k]]}], {Range[k]}];
   order = First[SortBy[orders, crossings]];
   R = If[k == 2, (radii[[1]] + radii[[2]] + 1.1)/2,
     Max[Table[(radii[[order[[i]]]] + radii[[order[[Mod[i, k] + 1]]]] + 1.)/(2 Sin[Pi/k]), {i, k}]]];
   centres = Table[{0., 0.}, {k}];
   concentric = k == 2 && Length[links] >= 3 && Min[Length /@ units] >= 3;
   If[concentric,
    (* two loops joined by three or more lines: one inside the other, the lines as spokes *)
    With[{outer = If[Length[units[[2]]] > Length[units[[1]]], 2, 1]},
     radii[[3 - outer]] = 0.45; radii[[outer]] = Max[radii[[outer]], 1.45]],
    Do[centres[[order[[i]]]] = R directionAt[If[k == 2, Pi, Pi/2] + 2. Pi (i - 1)/k], {i, k}]];
   (* each loop: its first vertex at the angle angles[[u]], the others following in the direction signs[[u]] *)
   angles = Table[With[{nb = Select[superEdges, MemberQ[#, u] &]},
      If[nb === {}, 0., angleOf[Mean[Table[centres[[First[DeleteCases[se, u]]]], {se, nb}]] - centres[[u]]]]], {u, k}];
   signs = Table[1., {k}];
   positionsOf[a_, s_] := Flatten[Table[Table[{units[[u, j]], centres[[u]] + radii[[u]] directionAt[a[[u]] + s[[u]] 2. Pi (j - 1)/Length[units[[u]]]]},
       {j, Length[units[[u]]]}], {u, k}], 1];
   (* the lines between units as {unit, index, unit, index} *)
   linkData = Table[{unitOf[l[[1]]], First[First[Position[units[[unitOf[l[[1]]]]], l[[1]], {1}]]],
      unitOf[l[[2]]], First[First[Position[units[[unitOf[l[[2]]]]], l[[2]], {1}]]]}, {l, links}];
   vertexPoint[u_, j_, a_, s_] := centres[[u]] + radii[[u]] directionAt[a[[u]] + s[[u]] 2. Pi (j - 1)/Length[units[[u]]]];
   segmentsOf[a_, s_] := Table[{vertexPoint[d[[1]], d[[2]], a, s], vertexPoint[d[[3]], d[[4]], a, s]}, {d, linkData}];
   lengthEnergy[segs_] := Total[Table[(sg[[1]] - sg[[2]]) . (sg[[1]] - sg[[2]]), {sg, segs}]];
   penalty[segs_] := 8 (mutualCrossingCount[segs] + If[concentric, 0, Count[Flatten[Table[If[Length[units[[u]]] >= 2,
            Table[segmentDistance[centres[[u]], sg] < radii[[u]] - 0.05, {sg, segs}], {}], {u, k}]], True]]);
   Do[Do[If[Length[units[[u]]] >= 2,
      With[{segs0 = segmentsOf[angles, signs]}, best = {lengthEnergy[segs0] + penalty[segs0], angles[[u]], signs[[u]]}];
      Do[With[{a2 = ReplacePart[angles, u -> th], s2 = ReplacePart[signs, u -> sg]},
        With[{segs = segmentsOf[a2, s2]},
         e = lengthEnergy[segs];
         If[e < best[[1]] - 10.^-9, e = e + penalty[segs]; If[e < best[[1]] - 10.^-9, best = {e, th, sg}]]]], {sg, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}];
      angles[[u]] = best[[2]]; signs[[u]] = best[[3]]], {u, k}], {3}];
   (* then the vertices joined to other units slide along their loop towards them (two lines to another unit from
      opposite vertices of a loop would make a wide ring around it), each at least half the even spacing from its
      neighbours on the loop; a move is kept only if it shortens the lines clearly *)
   theta = Table[angles[[u]] + signs[[u]] 2. Pi (j - 1)/Length[units[[u]]], {u, k}, {j, Length[units[[u]]]}];
   If[! concentric,
    With[{linked = DeleteDuplicates[Join[linkData[[All, {1, 2}]], linkData[[All, {3, 4}]]]]},
     Module[{pointOf, energy, n, sg, prev, next, dTot, eBest, e0, thBest, gmin},
      pointOf[th_, u_, j_] := centres[[u]] + radii[[u]] directionAt[th[[u, j]]];
      energy[th_] := With[{segs = Table[{pointOf[th, d[[1]], d[[2]]], pointOf[th, d[[3]], d[[4]]]}, {d, linkData}]},
        lengthEnergy[segs] + penalty[segs]];
      Do[Do[Do[If[Length[units[[u]]] >= 3 && MemberQ[linked, {u, j}],
         n = Length[units[[u]]]; sg = signs[[u]]; gmin = Pi/n;
         prev = theta[[u, Mod[j - 2, n] + 1]]; next = theta[[u, Mod[j, n] + 1]];
         dTot = sg (next - prev); dTot = dTot - 2. Pi Floor[dTot/(2. Pi)];
         If[dTot > 2 gmin + 10.^-6,
          e0 = energy[theta]; eBest = 0.9 e0 - 0.05; thBest = theta[[u, j]];
          Do[With[{th2 = ReplacePart[theta, {u, j} -> prev + sg (gmin + (dTot - 2 gmin) tt)]},
            With[{e2 = energy[th2]}, If[e2 < eBest, eBest = e2; thBest = th2[[u, j]]]]], {tt, 0., 1., 1./12}];
          theta[[u, j]] = thBest]], {j, Length[units[[u]]]}], {u, k}], {2}]]]];
   (* a bubble (a loop of two vertices) joined at each vertex by one line to another bubble (two bubbles side by side,
      the ring of second order): its vertices on the side facing that loop, 50 degrees from the line of the centres,
      so that the lines leave the circle at a wide angle (at the top and the bottom they would be tangent to it); the
      bubble is then drawn as its circle, a short and a long arc *)
   bubbleCircles = {};
   If[! concentric, Do[If[Length[units[[u]]] == 2,
      With[{ls = Table[Select[linkData, (#[[1]] == u && #[[2]] == j) || (#[[3]] == u && #[[4]] == j) &], {j, 2}]},
       If[Length[ls[[1]]] == 1 && Length[ls[[2]]] == 1,
        With[{w = Table[If[ls[[j, 1, 1]] == u, ls[[j, 1, 3]], ls[[j, 1, 1]]], {j, 2}]},
         If[w[[1]] == w[[2]] && w[[1]] != u && Length[units[[w[[1]]]]] == 2,
          With[{dir = unitVectorOf[centres[[w[[1]]]] - centres[[u]]], phi = angleOf[centres[[w[[1]]]] - centres[[u]]]},
           With[{sides = Table[With[{q = directionAt[theta[[u, j]]]}, Sign[dir[[1]] q[[2]] - dir[[2]] q[[1]]]], {j, 2}]},
            With[{sg = If[sides[[1]] != 0 && sides[[1]] != sides[[2]], sides, {1, -1}]},
             theta[[u, 1]] = phi + sg[[1]] 50. Degree; theta[[u, 2]] = phi + sg[[2]] 50. Degree;
             AppendTo[bubbleCircles, u]]]]]]]]], {u, k}]];
   pts = Flatten[Table[Table[{units[[u, j]], centres[[u]] + radii[[u]] directionAt[theta[[u, j]]]}, {j, Length[units[[u]]]}], {u, k}], 1];
   circles = Table[{units[[u]], centres[[u]], radii[[u]]}, {u, Select[Range[k], Length[units[[#]]] >= 3 || MemberQ[bubbleCircles, #] &]}];
   {pts, {"loops", circles, {0., 0.}}}];

(* layout of a vacuum component: several fermion loops, or one cycle with the rest around it *)
layoutVacuumComponent[component_, pairs_, propagatorPairs_, lines_ : {}] := With[{loops = fermionLoops[component, lines, 2],
    scalar = scalarLoops[component, lines]},
   Which[
    Length[loops] >= 2, layoutVacuumLoops[component, loops, pairs],
    (* every vertex on a closed loop of undirected propagators (a theory with a Hubbard-Stratonovich field): the loops
       drawn like fermion loops (two loops joined by three lines one inside the other) *)
    Length[scalar] >= 2 && Complement[component, Flatten[scalar]] === {}, layoutVacuumLoops[component, scalar, pairs],
    True, layoutVacuumCycle[component, pairs, propagatorPairs, lines]]];

(* the cycle, rotated so that the reflection about the vertical axis (through the top vertex, or through the middle of the
   top side) maps the lines between its vertices, with their multiplicities, and the number of line ends at each vertex
   to themselves; unchanged if no rotation does *)
symmetricCycleRotation[cycle_, lines_, topVertex_] := Module[{L = Length[cycle], refl, mult, ends, sym, rots},
   refl = If[topVertex, Table[Mod[1 - k, L] + 1, {k, L}], Table[L + 1 - k, {k, L}]];
   mult[u_, w_] := Count[lines, {a_, b_, _, _} /; Sort[{a, b}] === Sort[{u, w}]];
   ends[v_] := Count[lines, {v, _, _, _}] + Count[lines, {_, v, _, _}];
   sym[c_] := AllTrue[Range[L], ends[c[[#]]] == ends[c[[refl[[#]]]]] &] &&
     AllTrue[Subsets[Range[L], {2}], mult[c[[#[[1]]]], c[[#[[2]]]]] == mult[c[[refl[[#[[1]]]]]], c[[refl[[#[[2]]]]]]] &];
   (* only for a cycle through an insertion on a line (a vertex with two line ends, the mass lambda phibar^2/2), all its
      vertices on lines (no blobs) *)
   If[lines === {} || L < 3 || ! MemberQ[ends /@ cycle, 2] || Min[ends /@ cycle] < 2 || sym[cycle], cycle,
    rots = Select[Table[RotateLeft[cycle, s], {s, L - 1}], sym, 1];
    If[rots === {}, cycle, First[rots]]]];

(* the closed loops of undirected propagators through vertices that end exactly two of them (no self-loop), in order *)
scalarLoops[nodes_, lines_] := Module[{und, vs, loops = {}, comp, ordered, prev, cur},
   und = Cases[lines, {u_, w_, _, st_} /; u =!= w && ! fermionStyleQ[st] && ! interactionStyleQ[st] && MemberQ[nodes, u] && MemberQ[nodes, w] :> {u, w}];
   vs = Select[nodes, Count[Flatten[und], #] == 2 && ! MemberQ[Cases[lines, {x_, x_, _, st_} /; ! interactionStyleQ[st] :> x], #] &];
   und = Select[und, MemberQ[vs, #[[1]]] && MemberQ[vs, #[[2]]] &];
   Do[If[FreeQ[loops, v] && MemberQ[Flatten[und], v],
     comp = componentOf[v, und];
     If[Length[comp] >= 2 && AllTrue[comp, Count[Flatten[und], #] == 2 &],
      ordered = {v}; prev = None; cur = v;
      While[Length[ordered] < Length[comp],
       prev = cur; cur = First[Select[neighboursOf[prev, und], ! MemberQ[ordered, #] &]]; AppendTo[ordered, cur]];
      AppendTo[loops, ordered]]], {v, vs}];
   loops];

layoutVacuumCycle[component_, pairs_, propagatorPairs_, lines_ : {}] := Module[{cycle, propagatorCycle = True, L, r, positions, core, stripped,
    best, p, th0},
   cycle = If[Length[component] == 1, {}, longestCycle[component, propagatorPairs]];
   If[Length[component] > 1 && Length[cycle] < 3, propagatorCycle = False; cycle = longestCycle[component, pairs]];
   Which[
    Length[component] == 1, {{{First[component], {0., 0.}}}, {"free", {0., 0.}}},
    Length[cycle] < 3,
    (* no cycle: the longest shortest path on the axis *)
    best = {First[component]};
    Do[p = shortestPath[component[[i]], component[[j]], pairs]; If[p =!= None && Length[p] > Length[best], best = p],
     {i, Length[component]}, {j, i + 1, Length[component]}];
    {layeredLayout[axisPositions[best, 1., 0.], Select[component, ! MemberQ[best, #] &], pairs], {"backbone", best, True, {}, {0., -0.6}}},
    True,
    L = Length[cycle];
    If[propagatorCycle,
     r = 0.55/Sin[Pi/L]; th0 = Pi/2 - If[EvenQ[L], Pi/L, 0.];
     (* two diameters (two crossing chords): a vertex at the top, so that one diameter is vertical (drawn inside) and,
        in a loop of four, the other horizontal (drawn outside, over the top) *)
     If[EvenQ[L] && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + L/2]]}]], {k, L/2}], True] == 2, th0 = Pi/2];
     (* one diameter crossed by other chords, or three diameters: a diameter horizontal, so that the chords crossing it
        are vertical (a "#", not a slanted tangle); of three diameters the horizontal one is drawn outside, over the top,
        and the other two cross at the centre *)
     With[{dk = If[EvenQ[L], Select[Range[L/2], MemberQ[pairs, Sort[{cycle[[#]], cycle[[# + L/2]]}]] &], {}],
       chordCount = Length[Select[Subsets[Range[L], {2}], MemberQ[pairs, Sort[cycle[[#]]]] && Mod[#[[2]] - #[[1]], L] != 1 && Mod[#[[1]] - #[[2]], L] != 1 &]]},
      If[Length[dk] == 3 || Length[dk] == 1 && chordCount >= 2, th0 = Pi - 2. Pi (Last[dk] - 1)/L]];
     (* a rotation of the cycle whose picture is symmetric about the vertical axis, if the first one is not and the cycle
        goes through an insertion on a line (the basketball with a mass insertion on one line: the insertion at the top) *)
     cycle = symmetricCycleRotation[cycle, lines, OddQ[L] || th0 == Pi/2];
     (* four or more diameters would meet at the centre: the vertices are moved alternately apart, so that the
        diameters become chords crossing in pairs around the centre *)
     With[{shift = If[EvenQ[L] && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + L/2]]}]], {k, L/2}], True] >= 4,
         0.5 Pi/L, 0.]},
      positions = Table[{cycle[[k]], r directionAt[th0 + 2. Pi (k - 1)/L + shift (-1)^k]}, {k, L}];
      (* three diameters of a loop of six: one drawn outside, over the top, from two vertices raised 25 degrees above the
         horizontal, so that its arc leaves them away from the loop, not along it; the other two cross at the centre *)
      If[L == 6 && Count[Table[MemberQ[pairs, Sort[{cycle[[k]], cycle[[k + 3]]}]], {k, 3}], True] == 3,
       With[{k0 = With[{a = (Pi - th0) L/(2. Pi)}, Round[a - L Floor[a/L]]] + 1},
        positions = Table[{cycle[[k]], r directionAt[Pi + {-25., 55., 125., 205., 235., 305.}[[Mod[k - k0, 6] + 1]] Degree]}, {k, L}]]]];
     (* a ring through blobs (Tr ln(1 - G0 Sigma) with Sigma blobs): the blobs evenly around a circle wide enough for
        them, symmetric about the vertical axis (one blob at the bottom, two left and right, three with one at the bottom),
        the other vertices evenly between them *)
     With[{bk = Flatten[Position[cycle, _blob, {1}, Heads -> False]]},
      If[bk =!= {},
       With[{nb = Length[bk], rb = Max[Table[Replace[cycle[[k]], Append[$blobRadii, _ -> $blobRadius]], {k, bk}]]},
        With[{th = If[EvenQ[nb], 0., -Pi/2], gaps = Table[Mod[bk[[Mod[i, nb] + 1]] - bk[[i]], L, 1], {i, nb}]},
         r = Max[0.55/Sin[Pi/L], 1.7 rb, If[nb >= 2, (2 rb + 0.9)/(2 Sin[Pi/nb]), 0.]];
         positions = Flatten[Table[Table[{cycle[[Mod[bk[[i]] + j - 1, L] + 1]],
              r directionAt[th + 2. Pi (i - 1)/nb + (2. Pi/nb) j/gaps[[i]]]}, {j, 0, gaps[[i]] - 1}], {i, nb}], 1]]]]],
     (* sides made of interaction lines only are longer; then the principal axis is made horizontal *)
     With[{wt = Table[If[MemberQ[propagatorPairs, Sort[{cycle[[k]], cycle[[Mod[k, L] + 1]]}]], 1., 1.8], {k, L}]},
      With[{ang = 2. Pi FoldList[Plus, 0., Most[wt]]/Total[wt]},
       r = 0.5/Sin[Pi Min[wt]/Total[wt]];
       positions = Table[{cycle[[k]], r directionAt[ang[[k]]]}, {k, L}];
       With[{sxx = Total[(#[[2, 1]]^2) & /@ positions], syy = Total[(#[[2, 2]]^2) & /@ positions], sxy = Total[(#[[2, 1]] #[[2, 2]]) & /@ positions]},
        With[{phi = If[Abs[sxx - syy] + Abs[sxy] < 10.^-9, 0., ArcTan[sxx - syy, 2 sxy]/2]},
         positions = {#[[1]], {{Cos[phi], Sin[phi]}, {-Sin[phi], Cos[phi]}} . #[[2]]} & /@ positions]]]]];
    {core, stripped} = stripPendantVertices[cycle, Select[component, ! MemberQ[cycle, #] &], pairs];
    positions = Join[positions, tutteEmbedding[positions, core, pairs]];
    positions = placePendantVertices[positions, stripped, pairs, {0., 0.}];
    {pushApartVertices[positions, Select[component, ! MemberQ[cycle, #] &], 0.5], {"cycle", cycle, {0., 0.}, propagatorCycle, r}}]];

(* ---------------------------------------------------------------- fermion lines and units *)
(* In a diagram of fermions with density-type couplings (two-body interactions, phonons, impurities), every vertex lies
   on exactly one fermion line: an open line between external points, or a closed loop (a vertex with a line G(z, z)
   is a loop by itself). Such a diagram is drawn unit by unit. The open lines are straight: the horizontal axis of a
   two-point function, two parallel rails for a four-point function. Every loop is a circle, or a bubble when it has
   two vertices. The interaction lines between the units are straight, and what hangs from a line or a loop points
   away from it, radially; the arcs of a line go to the side where they cross nothing. The spacing along the lines
   leaves room for what hangs from them, so that nothing overlaps. *)

$linkLength = 0.85; $bubbleWidth = 1.; $axisSpacing = 1.; $railSeparation = 1.2;
unitRadius[k_] := If[k >= 3, 0.55/Sin[Pi/k], 0.];

(* {open lines, loops}, or None if some node is not on exactly one fermion line. An open line runs from its incoming
   to its outgoing end; an external node without fermion lines (a vertex of an amputated function with an incoming and
   an outgoing leg, its labels joined by a delta function) is an open line of length zero. Two inner nodes without
   fermion lines joined by two interaction lines (a gluon loop) are a loop too. *)
(* with anomalous lines: chains walked as undirected paths, every inner vertex with one psi end and one bar end (the
   arrows turn inside the anomalous lines); an open chain starts at its bar end (as above, where the arrow leaves), or at
   its first node when both its ends are of one kind (the chains of Delta, psi psi, and Delta-bar, bar bar); a loop is
   walked along the arrow that leaves its first vertex. Pictures without anomalous lines take the definition below. *)
fermionLines[nodes_List, lines_List, externalNodes_List] /; anomalousLinesQ[Select[lines, MemberQ[nodes, #[[1]]] &]] :=
  Catch[Module[{fl = Select[lines, MemberQ[nodes, #[[1]]] &], edges, selfLoops, endsAt, kindsAt, other, paths = {}, loops = {}, seen = {},
     walk, starts, undirected, bubbles, path},
    If[AnyTrue[fl, ! fermionStyleQ[#[[4]]] && ! interactionStyleQ[#[[4]]] &], Throw[None, fermionLines]];
    selfLoops = Cases[fl, {u_, u_, _, st_} /; directedStyleQ[st] :> u];
    If[Length[selfLoops] =!= Length[Union[selfLoops]], Throw[None, fermionLines]];
    edges = Select[fl, fermionStyleQ[#[[4]]] && #[[1]] =!= #[[2]] &];
    endsAt[x_] := Select[Range[Length[edges]], MemberQ[edges[[#, {1, 2}]], x] &];
    kindsAt[x_] := Sort[Table[lineEndKind[edges[[i]], x], {i, endsAt[x]}]];
    other[i_, x_] := If[edges[[i, 1]] === x, edges[[i, 2]], edges[[i, 1]]];
    undirected = Cases[fl, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w :> Sort[{u, w}]];
    bubbles = Select[DeleteDuplicates[undirected], Count[undirected, #] == 2 &&
        AllTrue[#, Function[x, endsAt[x] === {} && ! MemberQ[selfLoops, x] && ! MemberQ[externalNodes, x]]] &];
    If[Length[Flatten[bubbles]] =!= Length[Union[Flatten[bubbles]]], Throw[None, fermionLines]];
    Do[If[! Which[MemberQ[Flatten[bubbles], x], True, MemberQ[selfLoops, x], endsAt[x] === {},
        MemberQ[externalNodes, x], Length[endsAt[x]] <= 1, True, kindsAt[x] === {"bar", "psi"}], Throw[None, fermionLines]], {x, nodes}];
    (* the walk from x along the edge i0, to the end of the chain or back to its start *)
    walk[x_, i0_] := Module[{p = {x}, i = i0, v = other[i0, x], next},
      While[v =!= x,
       If[MemberQ[p, v], Throw[None, fermionLines]]; AppendTo[p, v];
       next = DeleteCases[endsAt[v], i];
       If[next === {}, Break[]]; i = First[next]; v = other[i, v]];
      p];
    starts = Select[nodes, Length[endsAt[#]] == 1 &];
    starts = Join[Select[starts, kindsAt[#] === {"bar"} &], Select[starts, kindsAt[#] =!= {"bar"} &]];
    Do[If[! MemberQ[seen, x], path = walk[x, First[endsAt[x]]]; AppendTo[paths, path]; seen = Join[seen, path]], {x, starts}];
    Do[If[MemberQ[externalNodes, x] && endsAt[x] === {} && ! MemberQ[selfLoops, x], AppendTo[paths, {x}]], {x, nodes}];
    seen = Join[Flatten[paths], Flatten[bubbles]];
    Do[If[! MemberQ[seen, x],
      If[MemberQ[selfLoops, x], AppendTo[loops, {x}]; AppendTo[seen, x],
       path = walk[x, First[Select[endsAt[x], lineEndKind[edges[[#]], x] === "bar" &]]];
       AppendTo[loops, path]; seen = Join[seen, path]]], {x, nodes}];
    {paths, Join[loops, bubbles]}], fermionLines];
fermionLines[nodes_List, lines_List, externalNodes_List] := Catch[Module[{directed, selfLoops, inDeg, outDeg, succ, paths = {},
     loops = {}, seen, path, v, undirected, bubbles},
    If[AnyTrue[lines, MemberQ[nodes, #[[1]]] && ! directedStyleQ[#[[4]]] && ! interactionStyleQ[#[[4]]] &], Throw[None, fermionLines]];
    directed = Cases[lines, {u_, w_, _, st_} /; directedStyleQ[st] && MemberQ[nodes, u] :> {u, w}];
    selfLoops = Cases[directed, {u_, u_} :> u];
    If[Length[selfLoops] =!= Length[Union[selfLoops]], Throw[None, fermionLines]];
    directed = Select[directed, #[[1]] =!= #[[2]] &];
    inDeg[x_] := Count[directed, {_, x}]; outDeg[x_] := Count[directed, {x, _}];
    succ[x_] := Cases[directed, {x, w_} :> w];
    undirected = Cases[lines, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w && MemberQ[nodes, u] :> Sort[{u, w}]];
    bubbles = Select[DeleteDuplicates[undirected], Count[undirected, #] == 2 &&
        AllTrue[#, Function[x, inDeg[x] == 0 && outDeg[x] == 0 && ! MemberQ[selfLoops, x] && ! MemberQ[externalNodes, x]]] &];
    If[Length[Flatten[bubbles]] =!= Length[Union[Flatten[bubbles]]], Throw[None, fermionLines]];
    Do[With[{i = inDeg[x], o = outDeg[x]},
      If[! Which[MemberQ[Flatten[bubbles], x], True, MemberQ[selfLoops, x], i == 0 && o == 0, MemberQ[externalNodes, x], i + o <= 1, True, i == 1 && o == 1],
       Throw[None, fermionLines]]], {x, nodes}];
    Do[If[inDeg[x] == 0 && outDeg[x] == 1,
      path = {x}; v = x;
      While[outDeg[v] == 1, v = First[succ[v]]; If[MemberQ[path, v], Throw[None, fermionLines]]; AppendTo[path, v]];
      AppendTo[paths, path]], {x, nodes}];
    Do[If[MemberQ[externalNodes, x] && inDeg[x] == 0 && outDeg[x] == 0 && ! MemberQ[selfLoops, x], AppendTo[paths, {x}]], {x, nodes}];
    seen = Join[Flatten[paths], Flatten[bubbles]];
    Do[If[! MemberQ[seen, x],
      If[MemberQ[selfLoops, x], AppendTo[loops, {x}]; AppendTo[seen, x],
       path = {x}; v = First[succ[x]];
       While[v =!= x, If[MemberQ[path, v], Throw[None, fermionLines]]; AppendTo[path, v]; v = First[succ[v]]];
       AppendTo[loops, path]; seen = Join[seen, path]]], {x, nodes}];
    {paths, Join[loops, bubbles]}], fermionLines];

(* the interaction lines of the nodes, as pairs (one entry per line) *)
interactionPairsOf[nodes_List, lines_List] := Cases[lines, {u_, w_, _, st_} /; interactionStyleQ[st] && u =!= w && MemberQ[nodes, u] :> {u, w}];

(* blocks: the groups of loops joined to each other by interaction lines (not through the open lines) *)
loopBlocks[loops_List, ipairs_List] := Module[{loopNodes = Flatten[loops], links, blocks = {}, comp},
   links = Join[Select[ipairs, MemberQ[loopNodes, #[[1]]] && MemberQ[loopNodes, #[[2]]] &],
     Flatten[Table[Table[{l[[i]], l[[Mod[i, Length[l]] + 1]]}, {i, Length[l]}], {l, loops}], 1]];
   Do[If[FreeQ[blocks, First[lp]],
     comp = componentOf[First[lp], links];
     AppendTo[blocks, Select[loops, MemberQ[comp, First[#]] &]]], {lp, loops}];
   blocks];

(* the lines from the vertices of a unit to placed nodes: {{vertex, placed node}, ...} *)
attachmentsOf[unit_List, ipairs_List, placedNodes_List] := Flatten[Table[{v, w}, {v, unit},
    {w, Select[neighboursOf[v, ipairs], MemberQ[placedNodes, #] && ! MemberQ[unit, #] &]}], 1];

(* the difference of two angles, in (-Pi, Pi] *)
angleDifference[a_, b_] := With[{d = a - b}, d - 2. Pi Floor[(d + Pi)/(2. Pi)]];

(* Place one unit (a loop in fermion order) joined by the lines att = {{vertex, point}, ...} to points already placed,
   on the side dir of them. Result: {{{vertex, point}, ...}, centre, circle {loop, centre, radius} or None}. A loop of
   three or more vertices is a circle with its vertices evenly spaced, turned so that the joined vertices face their
   points (one joined vertex: exactly opposite to dir); a bubble lies along dir, or across it when both its vertices
   are joined. extra: points the other vertices of the unit are joined to (the unit turns towards them). *)
placeUnit[unit_List, att_List, dir_, extra_List : {}] := Module[{k = Length[unit], P = att[[All, 2]], m, n = unitVectorOf[dir], t0},
   m = Mean[P]; t0 = rotate90[n];
   Which[
    k == 1, With[{q = m + $linkLength n}, {{{unit[[1]], q}}, q, None}],
    k == 2 && Length[att] == 1,
    With[{a = att[[1, 1]], b = First[DeleteCases[unit, att[[1, 1]]]]},
     {{{a, m + $linkLength n}, {b, m + ($linkLength + $bubbleWidth) n}}, m + ($linkLength + $bubbleWidth/2) n, None}],
    k == 2,
    (* a bubble joined at both vertices: across dir, as wide as the points it is joined to are apart if they are close;
       if they are further apart, it keeps its width and goes further out, its lines bent into one round arc with it
       (frameItems) *)
    With[{p1 = Select[att, #[[1]] === unit[[1]] &][[1, 2]], p2 = Select[att, #[[1]] === unit[[2]] &][[1, 2]]},
     With[{sg = If[(p2 - p1) . t0 < 0, -1., 1.], spread = Abs[(p2 - p1) . t0]},
      With[{w = If[spread <= 1.25 $bubbleWidth, Max[$bubbleWidth, spread], $bubbleWidth],
        h = If[spread <= 1.25 $bubbleWidth, $linkLength, Max[$linkLength, Min[spread/2, $linkLength + $bubbleWidth/2]]]},
       {{{unit[[1]], m + h n - sg w/2 t0}, {unit[[2]], m + h n + sg w/2 t0}}, m + h n, None}]]],
    True,
    Module[{r0 = unitRadius[k], idx, best = None, bestScore = Infinity, offsets, mbar, rho, r, th0, c, pts, segs, score, candidates, ideal},
     idx = Table[First[First[Position[unit, a[[1]], {1}]]], {a, att}];
     Do[
      offsets = Table[s 2. Pi (i - 1)/k, {i, idx}];
      rho = vectorNorm[{Mean[Cos[offsets]], Mean[Sin[offsets]]}];
      mbar = angleOf[{Total[Cos[offsets]], Total[Sin[offsets]]}];
      (* two joined vertices: the radius grows (up to 1.25) so that they are as far apart as their points *)
      r = r0;
      If[Length[att] == 2, With[{chord = 2 Abs[Sin[angleDifference[offsets[[1]], offsets[[2]]]/2]], dd = Abs[(P[[2]] - P[[1]]) . t0]},
        If[chord > 0.1, r = Min[1.25, Max[r0, dd/chord]]]]];
      ideal = angleOf[-n] - mbar;
      candidates = If[rho > 0.15, {ideal}, Join[{ideal}, Table[Pi k2/12., {k2, 0, 23}]]];
      Do[
       pts = Table[{unit[[j]], directionAt[th + s 2. Pi (j - 1)/k]}, {j, k}];
       (* the centre that puts the joined vertices nearest to $linkLength from their points along n *)
       c = Mean[Table[P[[i]] + $linkLength n - r Replace[att[[i, 1]], Rule @@@ pts], {i, Length[att]}]];
       pts = Table[{p[[1]], c + r p[[2]]}, {p, pts}];
       segs = Table[{Replace[att[[i, 1]], Rule @@@ pts], P[[i]]}, {i, Length[att]}];
       score = 10 mutualCrossingCount[segs] + Total[Table[(vectorNorm[sg[[1]] - sg[[2]]] - $linkLength)^2, {sg, segs}]] +
         10 Boole[(c - m) . n < 0.5 r] + 0.001 Abs[angleDifference[th, ideal]] +
         (* the other vertices near the points they are joined to *)
         0.01 Total[Table[vectorNorm[Replace[e[[1]], Rule @@@ pts] - e[[2]]], {e, extra}]];
       If[score < bestScore - 10.^-6, bestScore = score; best = {pts, c, {unit, c, r}}],
       {th, candidates}],
      {s, {1., -1.}}];
     best]]];

(* A loop joined at several of its vertices to points on a line (the axis or a rail), on the side n of it: the joined
   vertices on the lower half of the circle, each right above its point, so that the lines are perpendicular to the
   line (if the points are further apart than the loop is wide, spread over its lowest 100 degrees); the other vertices evenly spaced between them, those with something hanging from them preferably on the
   upper half. Result as for placeUnit, or None if the order of the joined vertices around the loop does not match
   their order along the line. *)
placeLoopOnLine[unit_List, att_List, n_, hanging_List] := Module[{k = Length[unit], nn = unitVectorOf[n], t, us, sorted, idx, best = None,
    bestScore = Infinity, cw, joined, angleOf2, free, uc, r, hc, centre, pts, score, frame, v0, ang, prevJ, nextJ, segment, radial},
   t = -rotate90[nn];
   frame[phi_] := Cos[phi] t + Sin[phi] nn;
   us = Table[a[[2]] . t, {a, att}];
   sorted = att[[Ordering[us]]];
   v0 = Mean[Table[a[[2]] . nn, {a, att}]];
   idx[v_] := First[First[Position[unit, v, {1}]]];
   Do[
    (* counterclockwise position around the loop, from the leftmost joined vertex *)
    cw[v_] := Mod[s (idx[v] - idx[sorted[[1, 1]]]), k];
    If[SortBy[sorted[[All, 1]], cw] === sorted[[All, 1]],
     uc = (Min[us] + Max[us])/2;
     (* points further apart than the loop is wide: vertical lines would leave its outer vertices tangentially; the
        joined vertices are spread over the lower 100 degrees of a loop a little narrower than the points, in proportion
        to the positions of their points, and the lines lean inwards *)
     radial = (Max[us] - Min[us])/2 > 0.87 unitRadius[k];
     r = If[radial, Min[1.3, Max[unitRadius[k], 0.85 (Max[us] - Min[us])/2]], unitRadius[k]];
     joined = If[radial,
       Table[{a[[1]], 1.5 Pi + 50. Degree (a[[2]] . t - uc)/((Max[us] - Min[us])/2)}, {a, sorted}],
       Table[{a[[1]], 2. Pi - ArcCos[Max[-1., Min[1., (a[[2]] . t - uc)/r]]]}, {a, sorted}]];
     (* the free vertices evenly between the joined ones around them; the last segment closes over the top *)
     ang = joined;
     Do[If[! MemberQ[joined[[All, 1]], v],
       With[{p = cw[v], js = Table[{cw[j[[1]]], j[[2]]}, {j, joined}]},
        With[{before = Last[Select[js, #[[1]] < p &]], after = Select[js, #[[1]] > p &]},
         With[{aft = If[after === {}, {k + js[[1, 1]], js[[1, 2]] + 2. Pi}, First[after]]},
          AppendTo[ang, {v, before[[2]] + (aft[[2]] - before[[2]]) (p - before[[1]])/(aft[[1]] - before[[1]])}]]]]], {v, unit}];
     hc = v0 + $linkLength + r Max[Table[-Sin[j[[2]]], {j, joined}]];
     centre = uc t + hc nn;
     pts = Table[{a[[1]], centre + r frame[a[[2]]]}, {a, ang}];
     (* something hanging from a vertex between two joined ones (below the circle) is bad *)
     score = Count[Table[MemberQ[hanging, a[[1]]] && cw[a[[1]]] < cw[Last[sorted][[1]]], {a, ang}], True] +
       0.01 Count[Table[cw[a[[1]]] < cw[Last[sorted][[1]]] && ! MemberQ[joined[[All, 1]], a[[1]]], {a, ang}], True];
     If[score < bestScore - 10.^-9, bestScore = score; best = {pts, centre, {unit, centre, r}}]],
    {s, {1, -1}}];
   best];

(* the region of a placed unit, {centre, radius}, to keep units apart *)
unitRegion[res_, dir_] := Which[
   res[[3]] =!= None, {res[[3, 2]], res[[3, 3]]},
   Length[res[[1]]] == 2, {Mean[res[[1]][[All, 2]]], 0.5 vectorNorm[res[[1, 2, 2]] - res[[1, 1, 2]]] + 0.05},
   True, {res[[1, 1, 2]] + 0.28 unitVectorOf[dir], 0.3}];

(* the bisector of the largest free sector around the placed node w (at pw) among its lines to placed nodes: the units
   hanging from a tadpole are spread around it (the first one to the left, the second one to the right, then below) *)
freeDirectionAt[w_, pw_, placed_List, ipairs_List] := With[{angles = Table[angleOf[Replace[u, Rule @@@ placed] - pw],
      {u, Select[neighboursOf[w, ipairs], # =!= w && MemberQ[placed[[All, 1]], #] &]}]},
   If[angles === {}, {-1., 0.}, directionAt[gapDirection[angles, {0., -1.}]]]];

(* Place the units of a block, one after the other, starting from the nodes already placed. placed: {{node, point}, ...};
   centres: {{node, centre}, ...} for the nodes of placed units (what hangs from them points away from the centre);
   railOut: {{node, direction}, ...} for the nodes of open lines (what hangs from them points that way); regions: the
   regions {centre, radius, nodes} of the units placed before: a new unit that overlaps a unit it is joined to moves
   further out.
   Result: {placed, centres, circles, regions}. *)
placeBlock[units0_List, ipairs_List, placed0_List, centres0_List, railOut_List, regions0_List : {}] := Module[{units = units0, placed = placed0,
    centres = centres0, circles = {}, regions = regions0, ready, unit, att, att0, dir, res, nodes, onLine, hanging, region, tries, railNodes},
   While[units =!= {},
    nodes = placed[[All, 1]];
    ready = Select[units, attachmentsOf[#, ipairs, nodes] =!= {} &];
    If[ready === {}, Break[]];
    (* units joined to an open line first, placed from their lines to it only (their other lines are drawn as they
       come); then the units with most lines to placed nodes *)
    railNodes = railOut[[All, 1]];
    unit = First[SortBy[ready, {-Length[Select[attachmentsOf[#, ipairs, nodes], MemberQ[railNodes, #[[2]]] &]],
         -Length[attachmentsOf[#, ipairs, nodes]]} &]];
    att0 = attachmentsOf[unit, ipairs, nodes];
    If[AnyTrue[att0, MemberQ[railNodes, #[[2]]] &], att0 = Select[att0, MemberQ[railNodes, #[[2]]] &]];
    att = Table[{a[[1]], Replace[a[[2]], Rule @@@ placed]}, {a, att0}];
    (* the direction: away from the centres of the units joined to, or as the open lines say; from a tadpole (its centre
       is its vertex), into the largest free sector around its vertex *)
    dir = Total[Table[With[{w = a[[2]]}, With[{c = Select[centres, #[[1]] === w &, 1], o = Select[railOut, #[[1]] === w &, 1],
          pw = Replace[w, Rule @@@ placed]},
         Which[c =!= {}, If[vectorNorm[pw - c[[1, 2]]] > 10.^-6, unitVectorOf[pw - c[[1, 2]]], freeDirectionAt[w, pw, placed, ipairs]],
          o =!= {}, o[[1, 2]], True, {0., 1.}]]],
       {a, att0}]];
    dir = If[vectorNorm[dir] < 10.^-6, {0., 1.}, unitVectorOf[dir]];
    hanging = Select[unit, AnyTrue[neighboursOf[#, ipairs], Function[w, ! MemberQ[nodes, w] && ! MemberQ[unit, w]]] &];
    (* a loop joined to several points of one line *)
    onLine = Length[unit] >= 3 && Length[att] >= 2 && AllTrue[att0, MemberQ[railNodes, #[[2]]] &] &&
      Length[DeleteDuplicates[Table[Select[railOut, #[[1]] === a[[2]] &, 1][[1, 2]], {a, att0}]]] == 1;
    res = If[onLine, placeLoopOnLine[unit, att, dir, hanging], None];
    If[res === None, res = placeUnit[unit, att, dir]];
    (* away from the units it hangs from (other units that it overlaps are moved apart by the caller); a unit joined
       to its parent by two or more lines keeps them visible *)
    region = unitRegion[res, dir]; tries = 0;
    With[{parents = Select[regions, Intersection[#[[3]], att0[[All, 2]]] =!= {} &], margin = If[Length[att0] >= 2, 0.45, 0.12]},
     While[tries < 10 && AnyTrue[parents, vectorNorm[#[[1]] - region[[1]]] < #[[2]] + region[[2]] + margin &],
      res = {Table[{q[[1]], q[[2]] + 0.2 dir}, {q, res[[1]]}], res[[2]] + 0.2 dir, If[res[[3]] === None, None, {res[[3, 1]], res[[3, 2]] + 0.2 dir, res[[3, 3]]}]};
      region = unitRegion[res, dir]; tries++]];
    AppendTo[regions, Append[region, unit]];
    placed = Join[placed, res[[1]]];
    centres = Join[centres, Table[{v, res[[2]]}, {v, unit}]];
    If[res[[3]] =!= None, AppendTo[circles, res[[3]]]];
    units = DeleteCases[units, unit]];
   {placed, centres, circles, regions}];

(* the extent of placed nodes and circles: {{xmin, xmax}, {ymin, ymax}}, with a margin for tadpoles and bubbles *)
unitExtent[points_List, circles_List] := With[{xs = Join[points[[All, 1]] - 0.3, points[[All, 1]] + 0.3, Table[c[[2, 1]] - c[[3]], {c, circles}],
     Table[c[[2, 1]] + c[[3]], {c, circles}]], ys = Join[points[[All, 2]] - 0.3, points[[All, 2]] + 0.3, Table[c[[2, 2]] - c[[3]], {c, circles}],
     Table[c[[2, 2]] + c[[3]], {c, circles}]]},
   {{Min[xs], Max[xs]}, {Min[ys], Max[ys]}}];

(* ---------------------------------------------------------------- two-point functions: the axis *)
(* Sides of the arcs (index spans on the axis) and of the blocks (their lists of attachment indices): {arcs, blocks},
   1 for below. On the same side, two crossing arcs conflict, and so do an arc and a block attached under it, or two
   blocks attached at interleaved vertices; an arc nested under a block, or a block attached at an end of an arc, is
   avoided if possible. Then arcs and blocks above, long arcs above. *)
axisSideChoice[spans_List, attached_List] := Module[{ns = Length[spans], nc = Length[attached], ranges, nestedQ, best},
   ranges = Table[{Min[a], Max[a]}, {a, attached}];
   nestedQ[s_, rg_] := rg[[2]] > rg[[1]] && rg[[1]] <= s[[1]] && s[[2]] <= rg[[2]];
   Which[ns + nc == 0, {{}, {}},
    ns + nc > 12, {Table[0, {ns}], Table[0, {nc}]},
    True,
    best = cheapestSides[ns + nc, Function[sides, 10 axisConflictCount[sides, spans, attached, ranges] +
       3 Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && nestedQ[spans[[i]], ranges[[j]]], {i, ns}, {j, nc}]], True] +
       0.3 Count[Flatten[Table[sides[[i]] == sides[[ns + j]] && (MemberQ[attached[[j]], spans[[i, 1]]] || MemberQ[attached[[j]], spans[[i, 2]]]),
          {i, ns}, {j, nc}]], True] +
       Total[Take[sides, ns]] + 0.5 Total[Drop[sides, ns]] + 0.01 Total[Table[sides[[i]] (spans[[i, 2]] - spans[[i, 1]]), {i, ns}]]]];
    {Take[best, ns], Drop[best, ns]}]];

(* A two-point function whose external points are the ends of one open fermion line: the line on the horizontal axis,
   with the blocks of loops hanging above or below it. Result: {positions, mode, circles}. *)
layoutAxisUnits[axis_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List, coiled_List : {}] := Module[
   {blocks, ix, attached, spans, sides, gaps, u, positions, circles, placedAll, regions, overlaps, pendantExtent, ext, side, out, res,
    railOut, n = Length[axis], below},
   blocks = loopBlocks[loops, ipairs];
   ix[v_] := First[First[Position[axis, v, {1}]]];
   attached = Table[Union[Flatten[Table[ix /@ Select[neighboursOf[v, ipairs], MemberQ[axis, #] &], {v, Flatten[b]}]]], {b, blocks}];
   (* the interaction lines between vertices of the axis: arcs, above or below it (also between neighbours) *)
   spans = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[axis, a] && MemberQ[axis, b] :> Sort[{ix[a], ix[b]}]]];
   sides = axisSideChoice[spans, attached];
   (* the extent, along the axis, of a block hanging from a single vertex *)
   pendantExtent = Table[If[Length[attached[[j]]] == 1,
      With[{v = axis[[attached[[j, 1]]]]}, With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, {0., 1.}}}]},
        unitExtent[Rest[r[[1]]][[All, 2]], r[[3]]][[1]]]], None], {j, Length[blocks]}];
   gaps = Table[$axisSpacing, {Max[n - 1, 0]}];
   (* a bubble joined by coils to two neighbouring vertices: more room between them, so that the bubble (narrower than
      their distance) does not touch the coils; its lines are then bent into one arc with it (bubbleBends) *)
   Do[If[Length[blocks[[j]]] == 1 && Length[blocks[[j, 1]]] == 2 && Length[attached[[j]]] == 2 && attached[[j, 2]] == attached[[j, 1]] + 1 &&
      AllTrue[Select[ipairs, MemberQ[blocks[[j, 1]], #[[1]]] =!= MemberQ[blocks[[j, 1]], #[[2]]] &], MemberQ[coiled, Sort[#]] &],
     gaps[[attached[[j, 1]]]] = Max[gaps[[attached[[j, 1]]]], 1.25 $bubbleWidth + 0.5]], {j, Length[blocks]}];
   Do[If[pendantExtent[[j1]] =!= None && pendantExtent[[j2]] =!= None && sides[[2, j1]] == sides[[2, j2]] &&
      attached[[j2, 1]] == attached[[j1, 1]] + 1,
     gaps[[attached[[j1, 1]]]] = Max[gaps[[attached[[j1, 1]]]], pendantExtent[[j1, 2]] - pendantExtent[[j2, 1]] + 0.25]],
    {j1, Length[blocks]}, {j2, Length[blocks]}];
   Do[
    u = If[n == 0, {}, With[{c = FoldList[Plus, 0., gaps]}, c - Last[c]/2]];
    positions = Table[{axis[[i]], {u[[i]], 0.}}, {i, n}];
    placedAll = positions; circles = {}; regions = {};
    Do[side = If[sides[[2, j]] == 1, -1., 1.]; out = {0., side};
     railOut = Table[{v, out}, {v, axis}];
     res = placeBlock[blocks[[j]], ipairs, positions, {}, railOut];
     placedAll = Join[placedAll, Drop[res[[1]], n]]; circles = Join[circles, res[[3]]]; regions = Join[regions, res[[4]]], {j, Length[blocks]}];
    (* units that overlap: more room on the axis between them *)
    overlaps = 0;
    If[n > 1,
     Do[With[{r1 = regions[[i1]], r2 = regions[[i2]]}, With[{d = vectorNorm[r1[[1]] - r2[[1]]], need = r1[[2]] + r2[[2]] + 0.3},
        If[d < need && Abs[r1[[1, 1]] - r2[[1, 1]]] > 0.05,
         With[{g = Max[1, Min[n - 1, Count[u, x_ /; x < (r1[[1, 1]] + r2[[1, 1]])/2]]]}, overlaps++; gaps[[g]] = gaps[[g]] + Max[0.2, need - d]]]]],
      {i1, Length[regions]}, {i2, i1 + 1, Length[regions]}]];
    If[overlaps == 0, Break[]], {5}];
   (* the legs, along the axis *)
   positions = Join[placedAll, Join @@ Table[If[amputated[[k]],
       {{leg[externalLabels[[k]]], Replace[externalNodes[[k]], Rule @@@ placedAll] + If[k == 1, {-$legLength, 0.}, {$legLength, 0.}]}}, {}],
      {k, Length[externalLabels]}]];
   below = Table[{axis[[spans[[i, 1]]]], axis[[spans[[i, 2]]]]}, {i, Select[Range[Length[spans]], sides[[1, #]] == 1 &]}];
   {positions, {"backbone", axis, False, Sort /@ below, {0., -0.6}}, circles}];

(* ---------------------------------------------------------------- four-point functions: two rails *)
(* Positions along two rails A and B (lists of nodes) with minimal gaps gapsA, gapsB between neighbours, and syncs
   {{i, j}, ...} (increasing in both): A[[i]] and B[[j]] at the same place. The rails start together and end together
   where nothing else holds them (with centred = True and no syncs, the shorter rail keeps its length and is centred:
   crossing lines between the rails then make a symmetric cross); a rail of one node is centred. Result: {uA, uB}. *)
railCoordinates[nA_Integer, nB_Integer, gapsA_List, gapsB_List, syncs_List, centred_ : False] := Module[{uA = Table[0., {nA}], uB = Table[0., {nB}],
    span, fill, s, u0, prev},
   span[gaps_, i_, j_] := If[j <= i, 0., Total[Take[gaps, {i, j - 1}]]];
   (* put the nodes i..j of a rail between the coordinates a and b, spacing them in proportion to their gaps *)
   fill[coords_, gaps_, i_, j_, a_, b_] := Module[{c = coords, tot = span[gaps, i, j]},
     If[j == i, c[[i]] = a,
      Do[c[[k]] = a + (b - a) If[tot < 10.^-9, (k - i)/(j - i), span[gaps, i, k]/tot], {k, i, j}]];
     c];
   If[syncs === {},
    (* without syncs the rails have the same length, or (centred) each its own, the shorter one in the middle *)
    With[{LA = span[gapsA, 1, nA], LB = span[gapsB, 1, nB]}, With[{L = Max[LA, LB]},
      uA = Which[nA == 1, {L/2}, centred, fill[uA, gapsA, 1, nA, (L - LA)/2, (L + LA)/2], True, fill[uA, gapsA, 1, nA, 0., L]];
      uB = Which[nB == 1, {L/2}, centred, fill[uB, gapsB, 1, nB, (L - LB)/2, (L + LB)/2], True, fill[uB, gapsB, 1, nB, 0., L]]]],
    (* before the first sync *)
    s = First[syncs]; u0 = Max[span[gapsA, 1, s[[1]]], span[gapsB, 1, s[[2]]]];
    uA = fill[uA, gapsA, 1, s[[1]], If[s[[1]] == 1, u0, 0.], u0];
    uB = fill[uB, gapsB, 1, s[[2]], If[s[[2]] == 1, u0, 0.], u0];
    prev = s;
    Do[With[{L = Max[span[gapsA, prev[[1]], t[[1]]], span[gapsB, prev[[2]], t[[2]]]], a = uA[[prev[[1]]]]},
      uA = fill[uA, gapsA, prev[[1]], t[[1]], a, a + L];
      uB = fill[uB, gapsB, prev[[2]], t[[2]], a, a + L];
      prev = t], {t, Rest[syncs]}];
    (* after the last sync *)
    With[{L = Max[span[gapsA, prev[[1]], nA], span[gapsB, prev[[2]], nB]], a = uA[[prev[[1]]]]},
     uA = fill[uA, gapsA, prev[[1]], nA, a, If[nA == prev[[1]], a, a + L]];
     uB = fill[uB, gapsB, prev[[2]], nB, a, If[nB == prev[[2]], a, a + L]]]];
   {uA, uB}];

(* the gaps i..j-1 stretched so that they add up to at least L *)
stretchedGaps[gaps_List, i_Integer, j_Integer, L_] := If[j <= i, gaps,
   With[{t = Total[Take[gaps, {i, j - 1}]]}, If[t >= L, gaps, Join[Take[gaps, i - 1], Take[gaps, {i, j - 1}] L/t, Drop[gaps, j - 1]]]]];

(* Sides of the arcs of a rail: 1 inside (towards the other rail), 0 outside. outsideAttached / insideAttached: indices
   of the rail nodes with something hanging outside / joined to the other rail (rungOnly: those joined to it by
   interaction lines alone). Crossing arcs on the same side, an arc over a node with something on its side, conflict;
   an arc inside that only crosses rungs conflicts less (two crossing arcs on one side nearly coincide); arcs inside
   are avoided. *)
railArcSides[spans_List, outsideAttached_List, insideAttached_List, rungOnly_List : {}] := Module[{ns = Length[spans], overQ},
   overQ[i_, nodes_] := spanOverQ[spans[[i]], nodes];
   Which[ns == 0, {},
    ns > 10, Table[0, {ns}],
    True,
    cheapestSides[ns, Function[sides,
      10 (Count[Flatten[Table[sides[[i]] == sides[[j]] && spansCrossQ[spans[[i]], spans[[j]]], {i, ns}, {j, i + 1, ns}]], True] +
          Count[Table[If[sides[[i]] == 1, overQ[i, Complement[insideAttached, rungOnly]], overQ[i, outsideAttached]], {i, ns}], True]) +
       5 Count[Table[sides[[i]] == 1 && ! overQ[i, Complement[insideAttached, rungOnly]] && overQ[i, rungOnly], {i, ns}], True] +
       2 Total[sides]]]]];

(* Place a unit joined to points on both sides of it (a loop between the two rails): the circle, centred at the
   preferred point, turned and oriented so that its joined vertices are near their points and the lines cross nothing *)
placeUnitBetween[unit_List, att_List, preferred_] := Module[{k = Length[unit], r, best = None, bestScore = Infinity, pts, segs, score},
   r = If[k >= 3, unitRadius[k], $bubbleWidth/2];
   Do[pts = Table[{unit[[j]], preferred + r directionAt[th + s 2. Pi (j - 1)/k]}, {j, k}];
    segs = Table[{Replace[a[[1]], Rule @@@ pts], a[[2]]}, {a, att}];
    score = 10 mutualCrossingCount[segs] + Total[Table[(sg[[1]] - sg[[2]]) . (sg[[1]] - sg[[2]]), {sg, segs}]];
    If[score < bestScore - 10.^-6, bestScore = score; best = pts],
    {s, {1., -1.}}, {th, 0., 2. Pi - 0.01, Pi/12.}];
   {best, preferred, If[k >= 3, {unit, preferred, r}, None]}];

(* A four-point function whose external points are the ends of two open fermion lines. The external labels are at the
   corners of a rectangle in the given order, counterclockwise from the top left; each line joins two neighbouring
   corners (if it would join opposite corners, the labels at the two right corners are exchanged), so that the lines
   are two parallel rails, horizontal or vertical. Lines between the rails are straight, perpendicular to them where
   possible; loops joined to both rails lie between them, everything else hangs outside. Result: {positions, mode,
   circles}, or None. *)
layoutRailUnits[paths_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List, order0_List] := Catch[Module[
   {order = order0, cornerOf, labelsOf, pc, horizontal, outerLink, A, B, orient, blocks, side, attA, attB, rungs, syncs, allSyncs, crossQ, spansA, spansB,
    arcSidesA, arcSidesB, ixA, ixB, outsideA, outsideB, insideA, insideB, pendantWidth, gapsA, gapsB, uA, uB, h, bridgeHeight,
    positions, circles = {}, res, railOut, toPlane, arcs, corner, chainQ, centres, legs, labelNode, nA, nB, centre, regions, overlaps},
   cornerOf[e_] := First[First[Position[order, e, {1}]]];
   labelNode[e_] := externalNodes[[First[First[Position[externalLabels, e, {1}]]]]];
   labelsOf[p_] := Select[externalLabels, MemberQ[{First[p], Last[p]}, labelNode[#]] &];
   If[Length[paths] =!= 2 || Sort[Join[labelsOf[paths[[1]]], labelsOf[paths[[2]]]]] =!= Sort[externalLabels] ||
     Length[labelsOf[paths[[1]]]] =!= 2, Throw[None, layoutRailUnits]];
   pc = Sort[cornerOf /@ labelsOf[#]] & /@ paths;
   (* lines joining opposite corners: exchange the labels at the right corners (3 and 4) *)
   If[MemberQ[pc, {1, 3}] || MemberQ[pc, {2, 4}],
    order = ReplacePart[order, {3 -> order[[4]], 4 -> order[[3]]}];
    pc = Sort[cornerOf /@ labelsOf[#]] & /@ paths];
   horizontal = MemberQ[pc, {1, 4}];
   (* the vertical extent of the picture is kept small: along vertical rails the nodes are closer, and between horizontal
      rails the lines to loops are shorter and the bubbles narrower (above and below them the lines keep their length,
      outerLink: a short wavy line to a tadpole is a kink) *)
   outerLink = $linkLength;
   Block[{$axisSpacing = If[horizontal, $axisSpacing, 0.75], $linkLength = If[horizontal, 0.5, $linkLength],
     $bubbleWidth = If[horizontal, 0.7, $bubbleWidth]},
   (* the rails in the frame (u, v): A at v > 0, B at v < 0, u along them. Horizontal: A on top (corners 1 -> 4), B
      below (2 -> 3); vertical: A on the left (2 -> 1), B on the right (3 -> 4), u upwards *)
   orient[p_, from_] := If[MemberQ[labelsOf[p], order[[from]]] && labelNode[order[[from]]] === Last[p] && Length[p] > 1, Reverse[p], p];
   If[horizontal,
    A = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {1, 4} &]], 1];
    B = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {2, 3} &]], 2];
    toPlane[{uu_, vv_}] := {uu, vv},
    A = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {1, 2} &]], 2];
    B = orient[First[Select[paths, Sort[cornerOf /@ labelsOf[#]] === {3, 4} &]], 3];
    toPlane[{uu_, vv_}] := {-vv, uu}];
   nA = Length[A]; nB = Length[B];
   ixA[v_] := First[First[Position[A, v, {1}]]]; ixB[v_] := First[First[Position[B, v, {1}]]];
   blocks = loopBlocks[loops, ipairs];
   attA = Table[Union[Flatten[Table[ixA /@ Select[neighboursOf[v, ipairs], MemberQ[A, #] &], {v, Flatten[b]}]]], {b, blocks}];
   attB = Table[Union[Flatten[Table[ixB /@ Select[neighboursOf[v, ipairs], MemberQ[B, #] &], {v, Flatten[b]}]]], {b, blocks}];
   side = Table[Which[attA[[j]] =!= {} && attB[[j]] =!= {}, "bridge", attA[[j]] =!= {}, "A", True, "B"], {j, Length[blocks]}];
   (* a chain: a bridge joined to each rail at one vertex, every unit of it joined to at most two others *)
   chainQ[j_] := side[[j]] === "bridge" && Length[attA[[j]]] == 1 && Length[attB[[j]]] == 1 &&
     Length[Cases[ipairs, {a_, b_} /; (MemberQ[Flatten[blocks[[j]]], a] && (MemberQ[A, b] || MemberQ[B, b])) ||
         (MemberQ[Flatten[blocks[[j]]], b] && (MemberQ[A, a] || MemberQ[B, a]))]] == 2;
   rungs = Cases[ipairs, {a_, b_} /; (MemberQ[A, a] && MemberQ[B, b]) || (MemberQ[A, b] && MemberQ[B, a]) :>
      If[MemberQ[A, a], {ixA[a], ixB[b]}, {ixA[b], ixB[a]}]];
   (* syncs: rungs and chains that cross no other *)
   crossQ[s_, t_] := (s[[1]] - t[[1]]) (s[[2]] - t[[2]]) < 0;
   allSyncs = Join[rungs, Table[{attA[[j, 1]], attB[[j, 1]]}, {j, Select[Range[Length[blocks]], chainQ]}]];
   syncs = Sort[Select[allSyncs, Function[s, ! AnyTrue[allSyncs, crossQ[s, #] &]]]];
   syncs = DeleteDuplicates[syncs];
   If[! (OrderedQ[syncs[[All, 1]]] && OrderedQ[syncs[[All, 2]]]) || hasDuplicatesQ[syncs[[All, 1]]] || hasDuplicatesQ[syncs[[All, 2]]],
    syncs = {}];
   (* the arcs of each rail, outside or inside *)
   spansA = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[A, a] && MemberQ[A, b] && Abs[ixA[a] - ixA[b]] > 1 :> Sort[{ixA[a], ixA[b]}]]];
   spansB = DeleteDuplicates[Cases[ipairs, {a_, b_} /; MemberQ[B, a] && MemberQ[B, b] && Abs[ixB[a] - ixB[b]] > 1 :> Sort[{ixB[a], ixB[b]}]]];
   outsideA = Union[Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "A" &]]]]];
   outsideB = Union[Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "B" &]]]]];
   insideA = Union[rungs[[All, 1]], Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]];
   insideB = Union[rungs[[All, 2]], Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]];
   arcSidesA = railArcSides[spansA, outsideA, insideA, Complement[insideA, Flatten[attA[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]]];
   arcSidesB = railArcSides[spansB, outsideB, insideB, Complement[insideB, Flatten[attB[[Select[Range[Length[blocks]], side[[#]] === "bridge" &]]]]]];
   (* the room needed by what hangs from neighbouring vertices *)
   pendantWidth[rail_, out_, j_] := With[{v = rail[[First[If[rail === A, attA[[j]], attB[[j]]]]]]},
     With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, out}}]}, unitExtent[Rest[r[[1]]][[All, 2]], r[[3]]][[1]]]];
   gapsA = Table[$axisSpacing, {Max[nA - 1, 0]}]; gapsB = Table[$axisSpacing, {Max[nB - 1, 0]}];
   Do[Do[With[{j1 = j[[1]], j2 = j[[2]]},
       If[side[[j1]] === sd && side[[j2]] === sd && Length[If[sd === "A", attA, attB][[j1]]] == 1 && Length[If[sd === "A", attA, attB][[j2]]] == 1 &&
         If[sd === "A", attA, attB][[j2, 1]] == If[sd === "A", attA, attB][[j1, 1]] + 1,
        With[{e1 = pendantWidth[If[sd === "A", A, B], {0., 1.}, j1], e2 = pendantWidth[If[sd === "A", A, B], {0., 1.}, j2], g = If[sd === "A", attA, attB][[j1, 1]]},
         If[sd === "A", gapsA[[g]] = Max[gapsA[[g]], e1[[2]] - e2[[1]] + 0.25], gapsB[[g]] = Max[gapsB[[g]], e1[[2]] - e2[[1]] + 0.25]]]]],
      {j, Tuples[Range[Length[blocks]], 2]}], {sd, {"A", "B"}}];
   (* neighbouring chains between the rails *)
   Do[If[chainQ[j1] && chainQ[j2] && attA[[j2, 1]] == attA[[j1, 1]] + 1,
     gapsA[[attA[[j1, 1]]]] = Max[gapsA[[attA[[j1, 1]]]], 1.6]];
    If[chainQ[j1] && chainQ[j2] && attB[[j2, 1]] == attB[[j1, 1]] + 1,
     gapsB[[attB[[j1, 1]]]] = Max[gapsB[[attB[[j1, 1]]]], 1.6]], {j1, Length[blocks]}, {j2, Length[blocks]}];
   (* two lines between the rails that cross: room for the cross *)
   Do[If[crossQ[s1, s2] && s1[[1]] < s2[[1]],
     gapsA = stretchedGaps[gapsA, s1[[1]], s2[[1]], 1.6]; gapsB = stretchedGaps[gapsB, s2[[2]], s1[[2]], 1.6]],
    {s1, allSyncs}, {s2, allSyncs}];
   (* the distance of the rails: room for the chains and the loops between them *)
   bridgeHeight[j_] := If[chainQ[j],
     With[{v = A[[attA[[j, 1]]]], w = B[[attB[[j, 1]]]]}, With[{r = placeBlock[blocks[[j]], ipairs, {{v, {0., 0.}}}, {}, {{v, {0., -1.}}}]},
       With[{end = Select[r[[1]], MemberQ[neighboursOf[w, ipairs], #[[1]]] &]}, -end[[1, 2, 2]] + $linkLength]]],
     2 $linkLength + 2 Max[0.5, Max[unitRadius[Length[#]] & /@ blocks[[j]]]] + 0.2];
   h = Max[Join[{$railSeparation}, Table[bridgeHeight[j], {j, Select[Range[Length[blocks]], side[[#]] === "bridge" &]}]]];
   (* inside arcs and crossing lines need room too *)
   If[MemberQ[arcSidesA, 1] || MemberQ[arcSidesB, 1], h = Max[h, 1.7]];
   If[Length[syncs] < Length[DeleteDuplicates[allSyncs]], h = Max[h, 1.5]];
   Do[
    {uA, uB} = railCoordinates[nA, nB, gapsA, gapsB, syncs, syncs === {} && allSyncs =!= {}];
    positions = Join[Table[{A[[i]], {uA[[i]], h/2}}, {i, nA}], Table[{B[[i]], {uB[[i]], -h/2}}, {i, nB}]];
    centres = {}; circles = {}; regions = {};
    (* the blocks outside the rails *)
    Do[If[side[[j]] =!= "bridge",
      railOut = Join[Table[{v, {0., 1.}}, {v, A}], Table[{v, {0., -1.}}, {v, B}]];
      res = Block[{$linkLength = outerLink}, placeBlock[blocks[[j]], ipairs, positions, centres, railOut]];
      positions = res[[1]]; centres = res[[2]]; circles = Join[circles, res[[3]]];
      regions = Join[regions, Table[Append[rg, side[[j]]], {rg, res[[4]]}]]], {j, Length[blocks]}];
    (* the chains between them, from A towards B *)
    Do[If[chainQ[j],
      railOut = Table[{v, {0., -1.}}, {v, A}];
      With[{others = Select[positions, ! MemberQ[B, #[[1]]] &]},
       res = placeBlock[blocks[[j]], ipairs, others, centres, railOut];
       positions = Join[res[[1]], Select[positions, MemberQ[B, #[[1]]] &]]; centres = res[[2]]; circles = Join[circles, res[[3]]]]],
     {j, Length[blocks]}];
    (* other bridges: the loop joined to both rails first, between them, then the rest *)
    Do[If[side[[j]] === "bridge" && ! chainQ[j],
      Module[{units = blocks[[j]], first, att},
       first = First[SortBy[units, -Length[attachmentsOf[#, ipairs, Join[A, B]]] &]];
       att = Table[{a[[1]], Replace[a[[2]], Rule @@@ positions]}, {a, attachmentsOf[first, ipairs, Join[A, B]]}];
       res = placeUnitBetween[first, att, {Mean[att[[All, 2]]][[1]], 0.}];
       positions = Join[positions, res[[1]]]; centres = Join[centres, Table[{v, res[[2]]}, {v, first}]];
       If[res[[3]] =!= None, AppendTo[circles, res[[3]]]];
       If[Length[units] > 1,
        res = placeBlock[DeleteCases[units, first], ipairs, positions, centres, Join[Table[{v, {0., -1.}}, {v, A}], Table[{v, {0., 1.}}, {v, B}]]];
        positions = res[[1]]; centres = res[[2]]; circles = Join[circles, res[[3]]]]]], {j, Length[blocks]}];
    (* units outside a rail that overlap: more room along the rail between them *)
    overlaps = 0;
    Do[With[{r1 = regions[[i1]], r2 = regions[[i2]]}, With[{d = vectorNorm[r1[[1]] - r2[[1]]], need = r1[[2]] + r2[[2]] + 0.3},
       If[r1[[4]] === r2[[4]] && d < need && Abs[r1[[1, 1]] - r2[[1, 1]]] > 0.05,
        With[{rail = If[r1[[4]] === "A", uA, uB], um = (r1[[1, 1]] + r2[[1, 1]])/2},
         With[{g = Max[1, Min[Length[rail] - 1, Count[rail, x_ /; x < um]]]},
          If[Length[rail] > 1, overlaps++;
           If[r1[[4]] === "A", gapsA[[g]] = gapsA[[g]] + Max[0.2, need - d], gapsB[[g]] = gapsB[[g]] + Max[0.2, need - d]]]]]]]],
     {i1, Length[regions]}, {i2, i1 + 1, Length[regions]}];
    If[overlaps == 0, Break[]], {4}];
   (* the arcs, with the sign of their bulge relative to the line from their first to their second node *)
   arcs = Join[
     Table[{{A[[spansA[[i, 1]]]], A[[spansA[[i, 2]]]]}, spansA[[i, 2]] - spansA[[i, 1]], If[arcSidesA[[i]] == 1, -1., 1.], "A"}, {i, Length[spansA]}],
     Table[{{B[[spansB[[i, 1]]]], B[[spansB[[i, 2]]]]}, spansB[[i, 2]] - spansB[[i, 1]], If[arcSidesB[[i]] == 1, 1., -1.], "B"}, {i, Length[spansB]}]];
   (* ranks: 1 for the shortest arc on its side of its rail, higher for longer ones *)
   arcs = Table[{a[[1]], Count[arcs, b_ /; b[[4]] === a[[4]] && b[[3]] == a[[3]] && b[[2]] <= a[[2]]], a[[3]]}, {a, arcs}];
   (* the legs, towards the corners of their labels *)
   corner = {{-1., 1.}, {-1., -1.}, {1., -1.}, {1., 1.}}/Sqrt[2.];
   positions = Table[{p[[1]], toPlane[p[[2]]]}, {p, positions}];
   centre = Mean[positions[[All, 2]]];
   positions = Table[{p[[1]], p[[2]] - centre}, {p, positions}];
   circles = Table[{c[[1]], toPlane[c[[2]]] - centre, c[[3]]}, {c, circles}];
   (* the signs of the arcs refer to the left normal of the line from the first node to the second, in the frame:
      the frames are related by a rotation, which keeps them *)
   (* the legs of a rail of one node (a bare vertex) open wider (110 degrees) and are longer (1.25 times, up to 2.2 times
      beside a longer rail, towards the corners beside the ends of the other rail), so that a bare vertex is a wide V, not
      a narrow one that makes the picture tall; they stay a clear V, never a straight line *)
   With[{railDir = toPlane[{1., 0.}]}, With[{uOther = Table[(Replace[#, Rule @@@ positions] . railDir) & /@ If[rail === A, B, A], {rail, {A, B}}]},
     legs = Join @@ Table[If[amputated[[k]],
         With[{node = externalNodes[[k]], d = corner[[cornerOf[externalLabels[[k]]]]]}, With[{p = Replace[node, Rule @@@ positions],
             single = Which[nA == 1 && node === A[[1]], 1, nB == 1 && node === B[[1]], 2, True, 0]},
           {{leg[externalLabels[[k]]], p + If[single == 0, $legLength d,
               With[{s = Sign[d . railDir], u = p . railDir, nrm = unitVectorOf[d - (d . railDir) railDir]},
                With[{need = Abs[If[s > 0, Max[uOther[[single]]], Min[uOther[[single]]]] - u] + $legLength/Sqrt[2.]},
                 Min[2.2 $legLength, Max[1.25 $legLength, need/Sin[55. Degree]]] (Cos[55. Degree] nrm + Sin[55. Degree] s railDir)]]]}}]], {}],
        {k, Length[externalLabels]}]]];
   {Join[positions, legs], {"circle", arcs, {0., 0.}}, circles}]], layoutRailUnits];


(* Three or more open fermion lines, each with its two ends at external points, joined in a chain by interaction lines
   (the diagrams of Z[eta, etabar] with six or more sources): horizontal rails one above the other, in the order of the
   chain, the lines between neighbouring rails vertical where possible; interaction lines along the first or the last rail
   are arcs outside. The longest rail is placed first and the others are fitted to it, each in the direction that keeps the
   lines between them from crossing. Result: {positions, mode, circles}, or None (loops, lines between rails that are not
   neighbours in the chain, crossing lines). *)
layoutStackedRails[paths_List, loops_List, ipairs_List, externalLabels_List, externalNodes_List, amputated_List] := Catch[Module[
   {k = Length[paths], railOf, edges, deg, order, t, prev, base, ucoord = {}, hasU, uOf, placed, fit, arcs = {}, positions, centre, legs, nbrs, minGap = 0.6},
   If[k < 3 || loops =!= {} || AnyTrue[paths, Length[#] < 2 || ! MemberQ[externalNodes, First[#]] || ! MemberQ[externalNodes, Last[#]] &],
    Throw[None, layoutStackedRails]];
   railOf[v_] := First[First[Position[paths, v, {2}]]];
   hasU[v_] := MemberQ[Cases[ucoord, {w_, _} :> w], v]; uOf[v_] := First[Cases[ucoord, {v, x_} :> x]];
   edges = DeleteDuplicates[Sort /@ Select[Table[{railOf[p[[1]]], railOf[p[[2]]]}, {p, ipairs}], #[[1]] =!= #[[2]] &]];
   deg[i_] := Count[Flatten[edges], i];
   If[Length[edges] =!= k - 1 || AnyTrue[Range[k], deg[#] > 2 || deg[#] == 0 &], Throw[None, layoutStackedRails]];
   (* the rails in the order of the chain *)
   order = {First[Select[Range[k], deg[#] == 1 &]]};
   While[Length[order] < k,
    nbrs = Complement[Flatten[Select[edges, MemberQ[#, Last[order]] &]], order];
    If[nbrs === {}, Throw[None, layoutStackedRails]]; AppendTo[order, First[nbrs]]];
   (* interaction lines along a rail only on the first and the last one *)
   If[AnyTrue[ipairs, railOf[#[[1]]] === railOf[#[[2]]] && ! MemberQ[{First[order], Last[order]}, railOf[#[[1]]]] &], Throw[None, layoutStackedRails]];
   base = First[SortBy[order, -Length[paths[[#]]] &]];
   ucoord = Table[{paths[[base, i]], (i - 1.) $axisSpacing}, {i, Length[paths[[base]]]}];
   placed = {base};
   (* the coordinates of the nodes of rail r along it, from the nodes of the placed rail joined to it *)
   fit[r_] := Module[{best = None, anchors, us, rail, c},
     Do[rail = If[dir == 1, paths[[r]], Reverse[paths[[r]]]];
      anchors = Select[Range[Length[rail]], AnyTrue[neighboursOf[rail[[#]], ipairs], hasU] &];
      us = Table[Mean[uOf /@ Select[neighboursOf[rail[[i]], ipairs], hasU]], {i, anchors}];
      If[best === None && anchors =!= {} && AllTrue[Range[Length[anchors] - 1],
          us[[# + 1]] - us[[#]] >= minGap (anchors[[# + 1]] - anchors[[#]]) - 10.^-9 &],
       c = Table[0., {Length[rail]}];
       Do[c[[anchors[[j]]]] = us[[j]], {j, Length[anchors]}];
       Do[Do[c[[i]] = us[[j]] + (us[[j + 1]] - us[[j]]) (i - anchors[[j]])/(anchors[[j + 1]] - anchors[[j]]), {i, anchors[[j]] + 1, anchors[[j + 1]] - 1}],
        {j, Length[anchors] - 1}];
       Do[c[[i]] = us[[1]] - (anchors[[1]] - i) $axisSpacing, {i, 1, anchors[[1]] - 1}];
       Do[c[[i]] = Last[us] + (i - Last[anchors]) $axisSpacing, {i, Last[anchors] + 1, Length[rail]}];
       best = Table[{rail[[i]], c[[i]]}, {i, Length[rail]}]], {dir, {1, -1}}];
     If[best === None, Throw[None, layoutStackedRails]];
     ucoord = Join[ucoord, best]];
   While[Length[placed] < k,
    t = First[Select[order, ! MemberQ[placed, #] && AnyTrue[edges, Function[e, MemberQ[e, #] && Intersection[e, placed] =!= {}]] &]];
    fit[t]; AppendTo[placed, t]];
   (* the rails aligned: their free ends (the nodes before the first and after the last node joined to another rail) are
      spread out to the common ends, so that the rails are equally long, one above the other *)
   With[{uMin = Min[ucoord[[All, 2]]], uMax = Max[ucoord[[All, 2]]]},
    Do[With[{rail = SortBy[paths[[r]], uOf]}, With[{joined = Select[Range[Length[rail]],
          AnyTrue[neighboursOf[rail[[#]], ipairs], Function[w, railOf[w] =!= r]] &]},
       If[joined =!= {},
        With[{i1 = First[joined], i2 = Last[joined], u1 = uOf[rail[[First[joined]]]], u2 = uOf[rail[[Last[joined]]]]},
         ucoord = Join[Select[ucoord, ! MemberQ[rail, #[[1]]] &],
           Table[{rail[[i]], Which[i < i1, uMin + (u1 - uMin) (i - 1)/(i1 - 1), i > i2, u2 + (uMax - u2) (i - i2)/(Length[rail] - i2), True, uOf[rail[[i]]]]},
            {i, Length[rail]}]]]]]], {r, Range[k]}]];
   positions = Flatten[Table[Table[{v, {uOf[v], -(First[First[Position[order, r]]] - 1.) $railSeparation}}, {v, paths[[r]]}], {r, order}], 1];
   (* the arcs along the first rail above it, along the last one below it, longer ones further out *)
   Do[If[railOf[p[[1]]] === railOf[p[[2]]] && Abs[Subtract @@ (Position[paths[[railOf[p[[1]]]]], #][[1, 1]] & /@ p)] > 1,
     With[{ends = SortBy[p, uOf]}, AppendTo[arcs, {ends, Abs[uOf[ends[[2]]] - uOf[ends[[1]]]], If[railOf[p[[1]]] === First[order], 1., -1.]}]]],
    {p, DeleteDuplicates[Sort /@ ipairs]}];
   arcs = Table[{a[[1]], Count[arcs, b_ /; b[[3]] == a[[3]] && b[[2]] <= a[[2]] + 10.^-9], a[[3]]}, {a, arcs}];
   centre = Mean[positions[[All, 2]]];
   positions = Table[{p[[1]], p[[2]] - centre}, {p, positions}];
   legs = Join @@ Table[If[amputated[[j]],
       With[{v = externalNodes[[j]]}, {{leg[externalLabels[[j]]], Replace[v, Rule @@@ positions] +
           {If[uOf[v] <= Min[uOf /@ paths[[railOf[v]]]] + 10.^-9, -$legLength, $legLength], 0.}}}], {}], {j, Length[externalLabels]}];
   {Join[positions, legs], {"circle", arcs, {0., 0.}}, {}}], layoutStackedRails];

(* ---------------------------------------------------------------- vacuum diagrams: trees of loops *)
(* A vacuum part made of three or more loops joined as a tree (two loops may be joined by several lines): the most
   central loop in the middle, turned so that as many lines as possible leave it horizontally or vertically, and the
   others around it, each pointing away from the loop it hangs from. Result: {positions, mode, circles}, or None. *)
layoutVacuumTree[loops_List, ipairs_List] := Module[{k = Length[loops], unitOf, superEdges, ecc, root, rootLoop, kr, r, best = None,
    bestScore = Infinity, linkVertices, pts, score, res, circles, outerOf, linkCount, kinds, symmetricQ, multiple},
   If[k < 3, Return[None]];
   unitOf[v_] := First[First[Position[loops, v, {2}]]];
   superEdges = DeleteDuplicates[Table[Sort[{unitOf[p[[1]]], unitOf[p[[2]]]}], {p, Select[ipairs, unitOf[#[[1]]] =!= unitOf[#[[2]]] &]}]];
   If[Length[superEdges] =!= k - 1 || Length[componentOf[1, superEdges]] =!= k, Return[None]];
   ecc = Table[Max[graphDistances[{i}, superEdges][[All, 2]]], {i, k}];
   root = First[SortBy[Range[k], {ecc[[#]], -Length[loops[[#]]], #} &]];
   rootLoop = loops[[root]]; kr = Length[rootLoop];
   linkVertices = Select[rootLoop, AnyTrue[neighboursOf[#, ipairs], Function[w, ! MemberQ[rootLoop, w]]] &];
   (* what a line leaving the root loop leads to: the number of lines to that loop and the size of its branch *)
   outerOf[v_] := unitOf /@ Select[neighboursOf[v, ipairs], ! MemberQ[rootLoop, #] &];
   linkCount[w_] := Count[linkVertices, v_ /; MemberQ[outerOf[v], w]];
   kinds = Table[Sort[Table[{linkCount[w], Total[Length[loops[[#]]] & /@ componentOf[w, Select[superEdges, ! MemberQ[#, root] &]]]},
       {w, outerOf[v]}]], {v, linkVertices}];
   (* the loops joined to the root loop by several lines *)
   multiple = Select[DeleteDuplicates[Flatten[outerOf /@ linkVertices]], linkCount[#] >= 2 &];
   symmetricQ[as_, f_] := AllTrue[Range[Length[as]], Function[i, AnyTrue[Range[Length[as]],
        kinds[[#]] === kinds[[i]] && Abs[angleDifference[f[as[[i]]], as[[#]]]] < 0.02 &]]];
   Which[
    kr == 1, pts = {{rootLoop[[1]], {0., 0.}}}; circles = {},
    kr == 2, pts = {{rootLoop[[1]], {-$bubbleWidth/2, 0.}}, {rootLoop[[2]], {$bubbleWidth/2, 0.}}}; circles = {},
    True,
    r = unitRadius[kr];
    (* the loop turned so that the lines leaving it are symmetric about the vertical or the horizontal axis, each mapped
       to a line that leads to an equal branch (hanging down rather than up; a loop joined by several lines beside it,
       like the other pairs of loops), then so that as many as possible leave it horizontally or vertically *)
    Do[With[{p = Table[{rootLoop[[j]], r directionAt[th + 2. Pi (j - 1)/kr]}, {j, kr}]},
       With[{as = Table[angleOf[Replace[v, Rule @@@ p]], {v, linkVertices}]},
         score = -10 Boole[symmetricQ[as, Pi - # &] || symmetricQ[as, -# &]] - Count[Table[Abs[Sin[2 a]] < 0.02, {a, as}], True] +
           0.5 Total[Table[Abs[Sin[angleOf[Total[Table[If[MemberQ[outerOf[linkVertices[[i]]], w], directionAt[as[[i]]], {0., 0.}],
                {i, Length[linkVertices]}]]]]], {w, multiple}]] + 0.01 Total[Sin[as]] + 0.001 th];
       If[score < bestScore - 10.^-9, bestScore = score; best = p]], {th, 0., 2. Pi - 0.01, Pi/12.}];
    pts = best; circles = {{rootLoop, {0., 0.}, r}}];
   res = placeBlock[Delete[loops, root], ipairs, pts, Table[{v, {0., 0.}}, {v, rootLoop}], {},
     {{{0., 0.}, Which[kr >= 3, r, kr == 2, 0.55, True, 0.3], rootLoop}}];
   {res[[1]], {"loops", Join[circles, res[[3]]], {0., 0.}}, Join[circles, res[[3]]]}];

(* ---------------------------------------------------------------- choice of the layout *)
(* the unit layout of the frame with the external labels, or None (then the general layout is used) *)
unitLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, externalOrder_] := Module[
   {frameLines = Select[lines, MemberQ[mainNodes, #[[1]]] &], fl, paths, loops, ipairs, axis},
   fl = fermionLines[mainNodes, frameLines, labelNodes];
   If[fl === None, Return[None]];
   {paths, loops} = fl;
   ipairs = interactionPairsOf[mainNodes, frameLines];
   (* propagators alone (e.g. the terms of Wick's theorem) keep their places *)
   If[ipairs === {}, Return[None]];
   Which[
    Length[labels] == 2 && Length[paths] == 1 && Sort[DeleteDuplicates[labelNodes]] === Sort[DeleteDuplicates[{First[paths[[1]]], Last[paths[[1]]]}]],
    axis = If[First[paths[[1]]] === labelNodes[[1]], paths[[1]], Reverse[paths[[1]]]];
    layoutAxisUnits[axis, loops, ipairs, labels, labelNodes, amputated, Cases[frameLines, {u_, w_, _, "Curly"} :> Sort[{u, w}]]],
    Length[labels] == 4 && Length[paths] == 2,
    layoutRailUnits[paths, loops, ipairs, labels, labelNodes, amputated,
     If[ListQ[externalOrder], Join[Select[externalOrder, MemberQ[labels, #] &], Complement[labels, externalOrder]], labels]],
    Length[paths] >= 3 && Length[labels] == 2 Length[paths],
    layoutStackedRails[paths, loops, ipairs, labels, labelNodes, amputated],
    True, None]];

(* ---------------------------------------------------------------- trees of vertex functions *)
(* A connected function written as a tree of vertex functions (blobs) joined by propagators, with the external points at
   its leaves (the tree expansion of ConnectedFunctionFromVertices). The central blob, or the two central blobs, in the
   middle; a chain of blobs is straight, the legs of a blob at the end of a chain are fanned outwards and those of a blob
   along it are perpendicular to it; a blob with three or more blob neighbours has them all around. The external points
   follow each other counterclockwise, in the order of their labels. {positions, mode, circles}, or None if the part is not such a tree. *)
$treeSpine = 1.; $treeLeg = 0.72;
treeFanOffsets[k_Integer] := Which[k == 1, {0.}, k == 2, {-45., 45.} Degree, k == 3, {-60., 0., 60.} Degree,
   k == 4, {-72., -24., 24., 72.} Degree, True, Table[(j - (k - 1)/2.) 150. Degree/(k - 1), {j, 0, k - 1}]];
treeLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, blobs_List] := Module[
   {treeLines, blobNodes, nbrs, blobNbrs, distances, ecc, centres, pos = {}, place, labelRank, subtreeRank, sortChildren,
    perpendicular},
   If[Or @@ amputated || Length[labels] < 2 || Length[DeleteDuplicates[labelNodes]] < Length[labelNodes], Return[None]];
   treeLines = Select[lines, MemberQ[mainNodes, #[[1]]] &];
   blobNodes = Select[mainNodes, Head[#] === blob &];
   If[blobNodes === {} || Complement[mainNodes, Join[labelNodes, blobNodes]] =!= {} ||
     AnyTrue[treeLines, #[[1]] === #[[2]] || interactionStyleQ[#[[4]]] &] || Length[treeLines] =!= Length[mainNodes] - 1 ||
     AnyTrue[Select[blobs, MemberQ[blobNodes, #[[1]]] &], #[[2]] =!= {} &], Return[None]];
   nbrs[v_] := Join[Cases[treeLines, {v, u_, _, _} :> u], Cases[treeLines, {u_, v, _, _} :> u]];
   If[AnyTrue[labelNodes, Length[nbrs[#]] =!= 1 &], Return[None]];
   blobNbrs[v_] := Select[nbrs[v], Head[#] === blob &];
   (* the distances from the blob s to the other blobs, along the blobs *)
   distances[s_] := Module[{seen = {s}, frontier = {s}, k = 0, next, out = {0}},
     While[frontier =!= {}, k++;
      next = DeleteDuplicates[Select[Flatten[blobNbrs /@ frontier], ! MemberQ[seen, #] &]];
      seen = Join[seen, next]; out = Join[out, Table[k, {Length[next]}]]; frontier = next];
     out];
   ecc = Table[Max[distances[b]], {b, blobNodes}];
   centres = Pick[blobNodes, Thread[ecc == Min[ecc]]];
   (* the children of a node in the order of the first label of their subtrees *)
   labelRank[v_] := First[First[Position[labelNodes, v, {1}]]];
   subtreeRank[v_, parent_] := If[Head[v] =!= blob, labelRank[v],
     Min[Join[{Infinity}, Table[subtreeRank[u, v], {u, DeleteCases[nbrs[v], parent]}]]]];
   sortChildren[cs_, v_] := SortBy[cs, subtreeRank[#, v] &];
   (* directions of m legs on the side a, fanned *)
   perpendicular[a_, m_] := If[m == 0, {}, a + If[m == 1, {0.}, Table[(j - (m - 1)/2.) 40. Degree, {j, 0, m - 1}]]];
   (* place v at p, reached from its parent in the direction phi, and its subtree *)
   place[v_, p_, phi_, parent_] := Module[{children = sortChildren[DeleteCases[nbrs[v], parent], v], bs, ls, angles, k, up},
     AppendTo[pos, {v, p}];
     If[Head[v] === blob,
      bs = Select[children, Head[#] === blob &]; ls = Select[children, Head[#] =!= blob &];
      k = Length[children];
      Which[
       k == 0, angles = {},
       (* the end of a chain: the legs fanned outwards, counterclockwise *)
       bs === {}, angles = phi + treeFanOffsets[k],
       (* along a chain: the next blob straight on, the legs perpendicular, the first ones on the right *)
       Length[bs] == 1, up = Ceiling[Length[ls]/2];
       children = Join[bs, ls]; angles = Join[{phi}, perpendicular[phi - Pi/2, up], perpendicular[phi + Pi/2, Length[ls] - up]],
       (* a branching: all the children fanned outwards *)
       True, angles = phi + Table[(j - (k - 1)/2.) Min[60. Degree, 150. Degree/(k - 1)], {j, 0, k - 1}]];
      Do[With[{c = children[[j]], a = angles[[j]]},
        place[c, p + If[Head[c] === blob, $treeSpine, $treeLeg] directionAt[a], a, v]], {j, k}]]];
   If[Length[centres] == 2,
    (* two central blobs side by side, the one with the first label on the left *)
    With[{cs = SortBy[centres, subtreeRank[#, First[DeleteCases[centres, #]]] &]},
     place[cs[[1]], {-$treeSpine/2, 0.}, Pi, cs[[2]]];
     place[cs[[2]], {$treeSpine/2, 0.}, 0., cs[[1]]]],
    With[{c = First[centres]}, Module[{children = sortChildren[nbrs[c], c], bs, ls, k, angles, up},
      bs = Select[children, Head[#] === blob &]; ls = Select[children, Head[#] =!= blob &]; k = Length[children];
      AppendTo[pos, {c, {0., 0.}}];
      If[Length[bs] == 2,
       (* the middle of a chain: the two blobs left and right, the legs above and below *)
       up = Ceiling[Length[ls]/2]; children = Join[bs, ls];
       angles = Join[{Pi, 0.}, perpendicular[Pi/2, up], perpendicular[-Pi/2, Length[ls] - up]],
       (* a star: all around, counterclockwise from the top (from the top left for an even number) *)
       angles = Table[Pi/2 + If[OddQ[k], 0., Pi/k] + 2. Pi (j - 1)/k, {j, k}]];
      Do[With[{ch = children[[j]], a = angles[[j]]},
        place[ch, If[Head[ch] === blob, $treeSpine, $treeLeg] directionAt[a], a, c]], {j, k}]]]];
   {pos, {"circle", {}, {0., 0.}}, {}}];

(* ---------------------------------------------------------------- declared blobs *)
(* A diagram with blobs declared by DeclareBlob: each blob is a circle or a square of half size $declaredBlobSize (larger
   when its label needs it), or a triangle inscribed in a circle 1.45 times larger, its legs (ports) on its boundary, counterclockwise from the top
   left (the corners of a square for four legs, of the triangle for three). Blobs joined
   by lines are placed side by side, on the side of the ports that join them (Bethe-Salpeter ladders); the other points
   outside the ports they are joined to; the legs of external labels at ports point outwards, or along the side of the
   blob when they would meet a neighbouring blob. {positions, {"blob", blobs, centre}, {}}, or None. *)
$declaredBlobSize = 0.6; $hatchSpacing = 0.1; $blobGap = 0.95; $blobPointDistance = 0.9;
blobHeadOf[v_, blobs_] := First[Cases[blobs, {v, _, h_} :> h], None];
(* the spec {label, shape, fill, size} of a declared blob: its size is $declaredBlobSize, or larger when its label needs it
   (declaredBlobSize) *)
blobSpecOf[h_] := First[Cases[$blobDeclarations, {h, l_, s_, f_, ___} :> {l, s, f, pictureBlobSize[l, s, f]}], None];
(* within one picture the blobs of one shape have one size, the largest their labels need: F beside Gamma_ph is as large
   as it, so that the lines between them are parallel; so do those of all the terms of a sum drawn together, so that a
   blob looks the same in every term (Gamma0 alone and beside Gamma4 in Gamma4 = Gamma0 - Gamma0 L0 Gamma4/2).
   $pictureBlobHeads: the heads of the blobs of the picture drawn, $sumBlobHeads those of the whole sum (geometriesOf) *)
$pictureBlobHeads = {}; $sumBlobHeads = {};
pictureBlobSize[l_, s_, f_] := Max[declaredBlobSize[l, s, f],
   Cases[$blobDeclarations, {hh_, l2_, s, f2_, ___} /; MemberQ[$pictureBlobHeads, hh] :> declaredBlobSize[l2, s, f2]],
   If[s === "Box", $pictureBoxFloor, 0.]];
(* the lines of a triangle leave from its corners: a box joined by two lines to two corners of a triangle (Gamma0 L0
   Lambda) is at least as wide as the side between them, so that the lines are parallel. $pictureBoxFloor: the least
   size of a box in the picture drawn, $sumBoxFloor in the whole sum (geometriesOf), as for $pictureBlobHeads *)
$pictureBoxFloor = 0.; $sumBoxFloor = 0.; $boxTriangleSpacing = 1.;
boxFloorOf[elements_List] := Module[{blobs = elements[[3]], lines = elements[[2]], shapeOf, joins},
   shapeOf[h_] := With[{sp = blobSpecOf[h]}, If[ListQ[sp], sp[[2]], None]];
   (* the legs of the triangle t joined by a line to legs of the box b *)
   joins[b_, t_] := DeleteDuplicates[Join[Cases[lines, {p_, q_, __} /; MemberQ[b[[2]], p] && MemberQ[t[[2]], q] :> q],
      Cases[lines, {q_, p_, __} /; MemberQ[b[[2]], p] && MemberQ[t[[2]], q] :> q]]];
   Max[0., Flatten[Table[If[shapeOf[b[[3]]] === "Box" && MemberQ[{"Triangle", "TriangleUp"}, shapeOf[t[[3]]]] &&
        Length[joins[b, t]] == 2,
       $boxTriangleSpacing Sqrt[3.] 1.45 blobSizeOfHead[t[[3]]]/2, 0.], {b, blobs}, {t, blobs}]]]];
blobAxialPorts[h_] := First[Cases[$blobDeclarations, {h, _, _, _, a_List} :> a], {}];
(* the corners of a triangular blob of size h centred at p: top left, bottom left, right *)
(* "Triangle": the first two corners on the left (top, bottom), the third on the right; "TriangleUp": the first two at the
   bottom (left, right), the third at the top *)
triangleShapeQ[shape_] := MemberQ[{"Triangle", "TriangleUp", "Diamond"}, shape];
(* "Diamond": a square turned by 45 degrees, its corners left, bottom, right, top. Both triangles are equilateral (the
   same triangle turned), so that a vertex looks the same in either orientation and its label has room *)
triangleCorners[p_, h_, shape_ : "Triangle"] := Table[p + If[shape === "Diamond", 1.3, 1.45] h {Cos[a], Sin[a]},
   {a, Switch[shape, "TriangleUp", {7. Pi/6, 11. Pi/6, Pi/2}, "Diamond", {Pi, 3. Pi/2, 0., Pi/2}, _, {2. Pi/3, 4. Pi/3, 0.}]}];
blobPortOffset[shape_, k_, n_, size_ : $declaredBlobSize] := With[{a = Pi/2 + Pi/n + 2. Pi (k - 1)/n},
   Which[
    triangleShapeQ[shape] && n == Length[triangleCorners[{0., 0.}, 1., shape]], triangleCorners[{0., 0.}, size, shape][[k]],
    shape === "Box", size {Cos[a], Sin[a]}/Max[Abs[Cos[a]], Abs[Sin[a]]],
    True, size {Cos[a], Sin[a]}]];
blobSizeOfHead[h_] := With[{sp = blobSpecOf[h]}, If[ListQ[sp], sp[[4]], $declaredBlobSize]];

(* The label of a blob fits it: its box (estimated from its LaTeX by blobLabelExtent, in units of the font size, the font
   taken as $blobLabelEm picture units: \footnotesize at 0.55 cm, \small at the default 0.7 cm and anything smaller fit),
   with the white clearing around it on a hatched blob, lies inside the blob $blobLabelMargin within its outline, so that
   a band of hatching is left all around it. The label sits where it has most room: the centre of a circle, a box or a
   diamond, near the incentre of a triangle (moved towards its widest side); a blob whose label does not fit at
   $declaredBlobSize is made larger, never its label smaller, except in a triangle. *)
$blobLabelEm = 0.6;
(* the label of a triangle is written 0.8 times smaller (TikZ scales its node), so that the triangle keeps the size of the
   other blobs (Lambda-tilde would be 0.73): the lines of a three-point vertex leave from its corners, and a box joined
   to two of them is as wide as the side between them (pictureBlobSize), so that a larger triangle would make it large *)
$triangleLabelScale = 0.8;
blobLabelScale[shape_] := If[MemberQ[{"Triangle", "TriangleUp"}, shape], $triangleLabelScale, 1.];
blobClearingEm[fill_] := If[MemberQ[{"Hatched", "CrossHatched"}, fill], 0.135, 0.07];
blobLabelMargin[fill_] := If[MemberQ[{"Hatched", "CrossHatched"}, fill], 0.1, 0.06];
(* the box {width, height, depth} of a LaTeX label in units of its font size: the glyphs of Palatino and Computer Modern
   (the wider of the two), scripts at 0.7 (0.5 in scripts) shifted as TeX shifts them, accents and bars above. An
   estimate on the safe side, enough to size a blob *)
$labelGreekWidths = <|"Gamma" -> 0.63, "Delta" -> 0.75, "Theta" -> 0.8, "Lambda" -> 0.8, "Xi" -> 0.7, "Pi" -> 0.8, "Sigma" -> 0.7,
   "Upsilon" -> 0.72, "Phi" -> 0.8, "Psi" -> 0.82, "Omega" -> 0.8|>;
$labelAccents = {"tilde", "hat", "bar", "widetilde", "widehat", "vec", "dot", "ddot", "check", "breve", "acute", "grave"};
$labelSpaces = <|"," -> 0.17, ":" -> 0.22, ";" -> 0.28, "!" -> -0.17, "quad" -> 1., "qquad" -> 2.|>;
labelTokens[s_String] := StringCases[s, {"\\" ~~ c : LetterCharacter .. :> {"cs", c}, "\\" ~~ c_ :> {"cs", c},
    c : Except[WhitespaceCharacter] :> {"ch", c}}];
labelCharBox[c_String] := Which[
   MemberQ[CharacterRange["0", "9"], c], {0.5, 0.69, 0.},
   MemberQ[{"(", ")", "[", "]", "|"}, c], {0.45, 0.8, 0.25},
   MemberQ[{"+", "-", "=", "<", ">"}, c], {0.8, 0.58, 0.08},
   MemberQ[{",", ".", ";"}, c], {0.28, 0.12, If[c === ".", 0., 0.2]},
   c === "'", {0.3, 0.8, 0.},
   UpperCaseQ[c], {Switch[c, "W", 1., "M", 0.95, "I", 0.42, "J", 0.55, "L" | "T", 0.65, "F", 0.66, "P", 0.68, _, 0.75], 0.69, If[c === "Q", 0.2, 0.]},
   LowerCaseQ[c], {Switch[c, "i" | "j" | "l", 0.3, "f" | "t" | "r", 0.42, "m", 0.9, "w", 0.8, _, 0.58],
    If[StringContainsQ["bdfhklt", c], 0.72, 0.47], If[StringContainsQ["gjpqy", c], 0.24, 0.]},
   True, {0.6, 0.69, 0.}];
labelGreekBox[name_String] := Which[
   KeyExistsQ[$labelGreekWidths, name], {$labelGreekWidths[name], 0.7, 0.},
   MemberQ[{"alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda", "mu",
     "nu", "xi", "pi", "rho", "varrho", "sigma", "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega"}, name],
   {If[MemberQ[{"iota"}, name], 0.35, If[MemberQ[{"omega", "psi", "phi", "varphi"}, name], 0.72, 0.62]],
    If[MemberQ[{"beta", "delta", "zeta", "theta", "vartheta", "lambda", "xi", "phi", "psi"}, name], 0.72, 0.47],
    If[MemberQ[{"beta", "gamma", "zeta", "eta", "mu", "xi", "rho", "varrho", "phi", "varphi", "chi", "psi"}, name], 0.24, 0.]},
   name === "prime", {0.3, 0.8, 0.},
   True, {0.8, 0.7, 0.}];
blobLabelExtent[label_String] := Module[{toks = labelTokens[label], i = 1, sequence, atom, scripted, size = 1.},
   (* a sequence of atoms up to a closing brace, at the size s (\scriptstyle changes it for the rest of the group) *)
   sequence[s0_] := Module[{s = s0, w = 0., h = 0., d = 0., b},
     While[i <= Length[toks] && toks[[i]] =!= {"ch", "}"},
      Which[
       toks[[i]] === {"cs", "scriptstyle"}, s = 0.7; i++,
       toks[[i]] === {"cs", "scriptscriptstyle"}, s = 0.5; i++,
       MemberQ[{{"cs", "displaystyle"}, {"cs", "textstyle"}}, toks[[i]]], s = 1.; i++,
       MemberQ[{"rm", "it", "bf", "sf", "cal", "left", "right", "big", "Big", "limits", "nolimits"}, toks[[i, 2]]] && toks[[i, 1]] === "cs", i++,
       True, b = scripted[s]; w += b[[1]]; h = Max[h, b[[2]]]; d = Max[d, b[[3]]]]];
     {w, h, d}];
   atom[s_] := Module[{t, b},
     If[i > Length[toks], Return[{0., 0., 0.}]];
     t = toks[[i]]; i++;
     Which[
      t === {"ch", "{"}, b = sequence[s]; If[i <= Length[toks], i++]; b,
      t[[1]] === "ch", s labelCharBox[t[[2]]],
      MemberQ[$labelAccents, t[[2]]], b = atom[s]; {Max[b[[1]], 0.5 s], b[[2]] + 0.2 s, b[[3]]},
      t[[2]] === "overline", b = atom[s]; {b[[1]], b[[2]] + 0.22 s, b[[3]]},
      t[[2]] === "underline", b = atom[s]; {b[[1]], b[[2]], b[[3]] + 0.22 s},
      MemberQ[{"mathrm", "mathit", "mathbf", "mathsf", "mathcal", "text", "textrm", "operatorname", "boldsymbol", "mathbb"}, t[[2]]], atom[s],
      KeyExistsQ[$labelSpaces, t[[2]]], {s $labelSpaces[t[[2]]], 0., 0.},
      True, s labelGreekBox[t[[2]]]]];
   (* an atom with its superscript and subscript *)
   scripted[s_] := Module[{b = atom[s], sup = None, sub = None, ss = If[s >= 1., 0.7, 0.5], u, v},
     While[i <= Length[toks] && MemberQ[{{"ch", "^"}, {"ch", "_"}}, toks[[i]]],
      If[toks[[i]] === {"ch", "^"}, i++; sup = atom[ss], i++; sub = atom[ss]]];
     If[sup === None && sub === None, Return[b]];
     u = 0.41 s; v = If[sub === None, 0., Max[If[sup === None, 0.19, 0.25] s, sub[[2]] - 0.24 s]];
     {b[[1]] + Max[If[sup === None, 0., sup[[1]]], If[sub === None, 0., sub[[1]]]] + 0.05 s,
      Max[b[[2]], If[sup === None, 0., u + sup[[2]]], If[sub === None, 0., sub[[2]] - v]],
      Max[b[[3]], If[sup === None, 0., sup[[3]] - u], If[sub === None, 0., v + sub[[3]]]]}];
   With[{b = sequence[size]}, {1.03 b[[1]], b[[2]], b[[3]]}]];
(* the margin of a box {wd, ht} centred at the offset q in a blob of size h centred at the origin: the least distance
   from its corners to the outline (negative when it sticks out) *)
blobBoxMargin[shape_, h_, {wd_, ht_}, q_] := Which[
   shape === "Box", h - Max[Abs[q[[1]]] + wd/2, Abs[q[[2]]] + ht/2],
   triangleShapeQ[shape], With[{t = triangleCorners[{0., 0.}, h, shape]},
    Min[Table[With[{a = t[[k]], e = unitVectorOf[t[[Mod[k, Length[t]] + 1]] - t[[k]]]},
       Min[Table[e[[1]] (q + {sx wd/2, sy ht/2} - a)[[2]] - e[[2]] (q + {sx wd/2, sy ht/2} - a)[[1]], {sx, {-1, 1}}, {sy, {-1, 1}}]]],
      {k, Length[t]}]]],
   True, h - vectorNorm[Abs[q] + {wd, ht}/2]];
(* the incentre of a triangle (the centre of a diamond) *)
blobIncentre[shape_, h_] := With[{t = triangleCorners[{0., 0.}, h, shape]},
   If[Length[t] == 3, With[{l = Table[vectorNorm[t[[Mod[k, 3] + 1]] - t[[Mod[k + 1, 3] + 1]]], {k, 3}]}, l . t/Total[l]], Mean[t]]];
(* the offset of the label from the centre of a blob of size h, and the margin it has there: in a triangle the point on
   its axis of symmetry, near the incentre, with the largest margin (among the points within 0.005 of it, the nearest to
   the incentre) *)
blobLabelPlacement[shape_, h_, box_] := blobLabelPlacement[shape, h, box] = If[! MemberQ[{"Triangle", "TriangleUp"}, shape],
   {{0., 0.}, blobBoxMargin[shape, h, box, {0., 0.}]},
   Module[{c = blobIncentre[shape, h], cands, best},
    cands = Table[With[{q = c + h If[shape === "Triangle", {a, 0.}, {0., a}]}, {q, blobBoxMargin[shape, h, box, q], vectorNorm[q - c]}],
      {a, -0.4, 0.4, 0.01}];
    best = Max[cands[[All, 2]]];
    First[SortBy[Select[cands, #[[2]] >= best - 0.005 &], Last]][[{1, 2}]]]];
blobLabelBox[label_, fill_, shape_ : None] := With[{e = blobLabelExtent[label], c = blobClearingEm[fill]},
   blobLabelScale[shape] $blobLabelEm {e[[1]] + 2 c, e[[2]] + e[[3]] + 2 c}];
(* the size of a blob: $declaredBlobSize, or the least size (to 0.01) at which its label fits *)
declaredBlobSize[label_String, shape_, fill_] := declaredBlobSize[label, shape, fill] = Module[
   {box = blobLabelBox[label, fill, shape], m = blobLabelMargin[fill], lo = $declaredBlobSize, hi},
   If[blobLabelPlacement[shape, lo, box][[2]] >= m, $declaredBlobSize,
    hi = 2 lo; While[blobLabelPlacement[shape, hi, box][[2]] < m && hi < 5., hi *= 1.5];
    While[hi - lo > 0.005, With[{mid = (lo + hi)/2}, If[blobLabelPlacement[shape, mid, box][[2]] >= m, hi = mid, lo = mid]]];
    N[Ceiling[hi, 0.01]]]];
declaredBlobSize[_, _, _] := $declaredBlobSize;
(* the direction in which the legs and lines leave a port: outwards, but horizontally for the axial ports of the blob
   (option "AxialPorts" of DeclareBlob; the two bottom corners of a triangle with its apex up): the fermion line of a
   self-energy *)
portDirection[h_, shape_, k_, n_] := With[{offset = blobPortOffset[shape, k, n]},
   If[MemberQ[blobAxialPorts[h], k] && Abs[offset[[1]]] > 0.01, {Sign[offset[[1]]], 0.}, unitVectorOf[offset]]];
(* is the point q inside the blob {centre, shape, size} (not on its boundary)? *)
insideBlobQ[q_, {c_, shape_, h_}, margin_ : -0.03] := Which[
   shape === "Box", Max[Abs[q - c]] < h + margin,
   triangleShapeQ[shape], With[{t = triangleCorners[c, h, shape]},
    AllTrue[Range[Length[t]], Function[i, With[{a = t[[i]], b = t[[Mod[i, Length[t]] + 1]]},
       (b - a)[[1]] (q - a)[[2]] - (b - a)[[2]] (q - a)[[1]] > -margin vectorNorm[b - a]]]]],
   True, vectorNorm[q - c] < h + margin];
(* a line from a point to a point or a port that crosses a blob, or passes close to it away from its ends: bent around
   it, away from its centre. A line from a port of a blob that leaves it outwards, 30 degrees or more from its outline
   (both sides at a corner), moves away from it all along (the blobs are convex): it is only tested for crossing that
   blob, so that a line to the near corner of a box stays straight (in the Schwinger-Dyson equation of phi^4, the
   lines from the vertex to the two top corners of Gamma4 would otherwise both be bent, and leave the vertex together) *)
leavesBlobClearQ[e_, o_, d_] := With[{u = unitVectorOf[o - e]},
   insideBlobQ[e, d, 0.03] && ! insideBlobQ[e, d, -0.03] &&
    NoneTrue[{Pi/6., -Pi/6.}, insideBlobQ[e + 0.12 {{Cos[#], -Sin[#]}, {Sin[#], Cos[#]}} . u, d, -0.01] &]];
blobAvoidingKind[pu_, pv_, uv_, discs_] := Module[{n = rotate90[unitVectorOf[pv - pu]], s = 0., t, r},
   Do[If[AnyTrue[Table[pu + k (pv - pu), {k, 0.05, 0.95, 0.05}], insideBlobQ[#, d] &] ||
      ! leavesBlobClearQ[pu, pv, d] && ! leavesBlobClearQ[pv, pu, d] &&
      AnyTrue[Select[Table[pu + k (pv - pu), {k, 0.05, 0.95, 0.05}], vectorNorm[# - pu] > 0.3 && vectorNorm[# - pv] > 0.3 &],
       insideBlobQ[#, d, 0.22] &],
     t = (d[[1]] - pu) . n; r = Switch[d[[2]], "Box", Sqrt[2.] d[[3]], "Triangle" | "TriangleUp", 1.45 d[[3]], "Diamond", 1.3 d[[3]], _, d[[3]]] + 0.45;
     With[{s1 = If[t > 0, t - r, t + r]}, If[Abs[s1] > Abs[s], s = s1]]], {d, discs}];
   If[s == 0., "plain", {"bend", s}]];
(* the ports of a blob whose head is symmetric (DeclareSymmetric) can be taken in any order: the canonical order of its
   arguments would put the external points at arbitrary corners, different from one term to the next. The blobs are
   taken in the order in which they are placed (each joined to one placed before it); the arguments of a symmetric blob
   joined to a blob placed before it, or to a blob whose ports are fixed, take the ports facing that blob (the mirror images of the ports they are joined to),
   the others the remaining ports counterclockwise from the top left, in the order of the external labels they are, or are
   joined to (the corners x1, x2, x3, x4 of a four-point function), the rest in their order *)
$blobSharedGap = 0.4;
orderSymmetricBlobPorts[blobs0_List, labelNodes_List, frameLines_List] := Module[{blobs = blobs0, nb = Length[blobs0], partners, order = {1},
    queue = {1}, i, args, n, shape, offs, back, u, slot, free, k, rank, rest, freeQ},
   (* the arguments of blob i joined to an argument of blob j, by a line or as a common point: {{of i, of j}, ...} *)
   partners[a_, b_] := Join[Table[{x, x}, {x, Intersection[blobs[[a, 2]], blobs[[b, 2]]]}],
     Cases[frameLines, {p_, q_, _, _} /; MemberQ[blobs[[a, 2]], p] && MemberQ[blobs[[b, 2]], q] :> {p, q}],
     Cases[frameLines, {q_, p_, _, _} /; MemberQ[blobs[[a, 2]], p] && MemberQ[blobs[[b, 2]], q] :> {p, q}]];
   While[queue =!= {}, i = First[queue]; queue = Rest[queue];
    Do[If[! MemberQ[order, j] && partners[i, j] =!= {}, AppendTo[order, j]; AppendTo[queue, j]], {j, nb}]];
   rank[x_] := With[{ks = Flatten[Table[Position[labelNodes, w, {1}], {w, Join[{x}, Cases[frameLines, {x, w_, _, _} :> w],
          Cases[frameLines, {w_, x, _, _} :> w]]}]]}, If[ks === {}, 10^6, Min[ks]]];
   freeQ[h_] := (MemberQ[$symmetricHeads, h] || MemberQ[Attributes[h], Orderless]) && ! MemberQ[$antisymmetricHeads, h];
   Do[i = order[[m]]; args = blobs[[i, 2]]; n = Length[args];
    If[n > 1 && freeQ[blobs[[i, 3]]],
     shape = blobSpecOf[blobs[[i, 3]]][[2]];
     offs = Table[blobPortOffset[shape, kk, n, blobSizeOfHead[blobs[[i, 3]]]], {kk, n}];
     (* {argument, offset of the port it is joined to} for the blobs placed before, and those whose ports are fixed *)
     back = Join @@ Table[With[{sj = blobSpecOf[blobs[[j, 3]]][[2]], nj = Length[blobs[[j, 2]]]},
         Table[{pq[[1]], blobPortOffset[sj, First[First[Position[blobs[[j, 2]], pq[[2]], {1}]]], nj, blobSizeOfHead[blobs[[j, 3]]]]}, {pq, partners[i, j]}]],
       {j, Join[Take[order, m - 1], Select[Drop[order, m], ! freeQ[blobs[[#, 3]]] &]]}];
     With[{xs = DeleteDuplicates[back[[All, 1]]]}, back = Table[First[Cases[back, {x, _}]], {x, xs}]];
     slot = Table[None, {n}]; free = Range[n];
     If[back =!= {},
      u = unitVectorOf[Total[back[[All, 2]]]];
      Do[With[{target = b[[2]] - 2 (b[[2]] . u) u},
        k = First[SortBy[free, {vectorNorm[offs[[#]] - target], #} &]];
        slot[[k]] = b[[1]]; free = DeleteCases[free, k]], {b, back}]];
     rest = Select[args, ! MemberQ[slot, #] &];
     rest = SortBy[rest, {rank[#], First[First[Position[args, #, {1}]]]} &];
     Do[slot[[free[[kk]]]] = rest[[kk]], {kk, Length[rest]}];
     blobs[[i, 2]] = slot], {m, Length[order]}];
   blobs];
blobLayoutWithExternals[labels_List, labelNodes_List, amputated_List, mainNodes_List, lines_List, blobs_List] := Module[
   {frameBlobs, ports, portNodes, portIndex, portOffsetIn, frameLines, centres, links, shared, progress, others, pos = {}, placedQ, posOf,
    nb, pn, dir, centroid, legPositions = {}, discs, nbrsOf, axialPoints = {}},
   frameBlobs = Select[blobs, MemberQ[mainNodes, #[[1]]] &];
   If[frameBlobs === {} || ! AllTrue[frameBlobs, declaredBlobHeadQ[#[[3]]] &], Return[None]];
   (* the blob with the first external label first: on the left when the ports do not decide *)
   With[{i1 = First[Select[Range[Length[frameBlobs]], MemberQ[frameBlobs[[#, 2]], First[labelNodes]] &], 1]},
    frameBlobs = Join[{frameBlobs[[i1]]}, Delete[frameBlobs, i1]]];
   frameLines = Select[lines, MemberQ[mainNodes, #[[1]]] && #[[1]] =!= #[[2]] &];
   nbrsOf[v_] := Join[Cases[frameLines, {v, u_, _, _} :> u], Cases[frameLines, {u_, v, _, _} :> u]];
   (* a point that is twice a port of one blob, or a port of three or more blobs: the general layout *)
   If[AnyTrue[frameBlobs, hasDuplicatesQ[#[[2]]] &] || AnyTrue[Tally[Flatten[frameBlobs[[All, 2]]]], #[[2]] > 2 &], Return[None]];
   frameBlobs = orderSymmetricBlobPorts[frameBlobs, labelNodes, frameLines];
   (* ports: {node, blob, offset from the centre of the blob} *)
   ports = Flatten[Table[With[{b = frameBlobs[[i]], n = Length[frameBlobs[[i, 2]]]},
       Table[{b[[2, k]], i, blobPortOffset[blobSpecOf[b[[3]]][[2]], k, n, blobSizeOfHead[b[[3]]]], portDirection[b[[3]], blobSpecOf[b[[3]]][[2]], k, n]}, {k, n}]],
      {i, Length[frameBlobs]}], 1];
   portNodes = ports[[All, 1]];
   portIndex[v_] := First[First[Position[portNodes, v, {1}]]];
   portOffsetIn[v_, i_] := First[Cases[ports, {v, i, o_, _} :> o]];
   (* the blobs: each next to a blob already placed that it is joined to by lines between ports, or that it shares points
      with (two touching blobs, as L0 Gamma0 L: joined by short links, the common point in the middle) *)
   centres = Table[None, {Length[frameBlobs]}]; centres[[1]] = {0., 0.};
   links[i_, j_] := Select[frameLines, MemberQ[portNodes, #[[1]]] && MemberQ[portNodes, #[[2]]] &&
       Sort[{ports[[portIndex[#[[1]]], 2]], ports[[portIndex[#[[2]]], 2]]}] === Sort[{i, j}] &];
   shared[i_, j_] := If[i == j, {}, Intersection[frameBlobs[[i, 2]], frameBlobs[[j, 2]]]];
   (* a circle whose two ports are both joined to two ports of one other blob (the gap function closed on the
      particle-particle vertex, Delta = -1/2 Delta L0 Gamma_pp): its ports, otherwise on opposite sides, turned to face
      that blob, on either side of the line of the centres as the ports they are joined to, at their spacing if the
      circle is wide enough, else 45 degrees off that line (the ports of the other blob then follow, below) *)
   Do[If[i != j && blobSpecOf[frameBlobs[[i, 3]]][[2]] === "Circle" && Length[frameBlobs[[i, 2]]] == 2 && Length[links[i, j]] == 2,
     Module[{ends, cp, op, a, u, w, r, th},
      ends = Table[SortBy[l[[{1, 2}]], ports[[portIndex[#], 2]] == j &], {l, links[i, j]}];
      cp = ends[[All, 1]]; op = ends[[All, 2]];
      If[DuplicateFreeQ[cp] && DuplicateFreeQ[op] && AllTrue[Join[cp, op], Count[portNodes, #] == 1 &] &&
        vectorNorm[Total[Table[ports[[portIndex[q], 3]], {q, op}]]] > 0.1,
       a = Table[ports[[portIndex[q], 3]], {q, op}];
       (* u: from the other blob towards the circle, the side its two ports face; w across it *)
       u = unitVectorOf[Total[a]]; w = rotate90[u]; r = blobSizeOfHead[frameBlobs[[i, 3]]];
       th = ArcSin[Min[vectorNorm[a[[1]] - a[[2]]]/(2 r), Sin[Pi/4.]]];
       Do[With[{o = r (-Cos[th] u + Sign[(a[[k]] - Mean[a]) . w] Sin[th] w)},
         ports[[portIndex[cp[[k]]], 3]] = o; ports[[portIndex[cp[[k]]], 4]] = unitVectorOf[o]], {k, 2}]]]],
    {i, Length[frameBlobs]}, {j, Length[frameBlobs]}];
   (* a box joined by two lines to a circle: its two ports on the side facing the circle are wider apart than the circle's
      ports, so that the lines would spread; they are moved along that side, symmetrically about its middle, to the
      spacing of the circle's ports, and the lines are parallel. (The lines of a triangle always leave from its corners:
      a box joined so to a triangle is as wide as the triangle's side instead, pictureBlobSize) *)
   Do[If[i != j && Length[links[i, j]] == 2 &&
       blobSpecOf[frameBlobs[[j, 3]]][[2]] === "Box" && blobSpecOf[frameBlobs[[i, 3]]][[2]] === "Circle" && Length[frameBlobs[[i, 2]]] == 2,
     Module[{ends, tp, pp, a, b, s},
      ends = Table[SortBy[l[[{1, 2}]], ports[[portIndex[#], 2]] == j &], {l, links[i, j]}];
      pp = ends[[All, 1]]; tp = ends[[All, 2]];
      If[DuplicateFreeQ[pp] && DuplicateFreeQ[tp] && AllTrue[tp, Count[portNodes, #] == 1 &],
       {a, b} = Table[ports[[portIndex[t], 3]], {t, tp}];
       s = vectorNorm[ports[[portIndex[pp[[1]]], 3]] - ports[[portIndex[pp[[2]]], 3]]];
       (* the two ports of a box on one side, not at opposite corners *)
       If[s < vectorNorm[b - a] - 0.05 && Min[Abs[b - a]] < 0.01,
        With[{m = (a + b)/2, e = unitVectorOf[b - a]},
         ports[[portIndex[tp[[1]]], 3]] = m - s e/2; ports[[portIndex[tp[[2]]], 3]] = m + s e/2]]]]],
    {i, Length[frameBlobs]}, {j, Length[frameBlobs]}];
   progress = True;
   While[progress && MemberQ[centres, None], progress = False;
    Do[If[centres[[j]] === None, Do[If[centres[[i]] =!= None && (links[i, j] =!= {} || shared[i, j] =!= {}) && centres[[j]] === None,
        With[{ends = Flatten[links[i, j][[All, {1, 2}]]], gap = If[links[i, j] === {}, $blobSharedGap, $blobGap]},
         With[{d = Total[Join[Table[If[ports[[portIndex[e], 2]] == i, 1, -1] ports[[portIndex[e], 3]], {e, ends}],
              Table[portOffsetIn[s, i] - portOffsetIn[s, j], {s, shared[i, j]}]]]},
          (* ports on opposite corners (the particle-particle ladder): side by side, further apart, as the lines cross *)
          centres[[j]] = centres[[i]] + With[{hh = blobSizeOfHead[frameBlobs[[i, 3]]] + blobSizeOfHead[frameBlobs[[j, 3]]]},
             If[vectorNorm[d] < 0.1, (hh + gap + 0.65) {1., 0.}, (hh + gap) unitVectorOf[d]]];
          progress = True]]], {i, Length[frameBlobs]}]], {j, Length[frameBlobs]}]];
   If[MemberQ[centres, None], Return[None]];
   (* a point shared by two blobs midway between their ports, which are kept (blobPortCorner) as the ends of the links *)
   pos = Join[Table[{frameBlobs[[i, 1]], centres[[i]]}, {i, Length[frameBlobs]}],
     Table[{v, Mean[Cases[ports, {v, i_, o_, _} :> centres[[i]] + o]]}, {v, DeleteDuplicates[portNodes]}],
     Join @@ Table[If[Count[portNodes, p[[1]]] > 1, {{blobPortCorner[frameBlobs[[p[[2]], 1]], p[[1]]], centres[[p[[2]]]] + p[[3]]}}, {}],
       {p, ports}]];
   placedQ[v_] := MemberQ[pos[[All, 1]], v];
   posOf[v_] := First[Cases[pos, {v, p_} :> p]];
   (* the other points: outside the ports they are joined to, or next to a point already placed *)
   others = Select[mainNodes, Head[#] =!= blob && ! MemberQ[portNodes, #] &];
   progress = True;
   While[progress && ! AllTrue[others, placedQ], progress = False;
    centroid = Mean[pos[[All, 2]]];
    Do[If[! placedQ[v],
      nb = Select[nbrsOf[v], placedQ]; pn = Select[nb, MemberQ[portNodes, #] &];
      Which[
       pn =!= {},
       dir = Total[Table[ports[[portIndex[q], 3]], {q, pn}]];
       If[vectorNorm[dir] < 0.1, dir = Mean[posOf /@ pn] - centroid];
       With[{axial = Select[pn, Abs[ports[[portIndex[#], 4, 2]]] < 10.^-6 && ports[[portIndex[#], 4]] =!= unitVectorOf[ports[[portIndex[#], 3]]] &&
            AnyTrue[frameLines, Function[l, Sort[l[[{1, 2}]]] === Sort[{v, #}] && ! interactionStyleQ[l[[4]]]]] &]},
        If[axial =!= {},
         (* joined by a propagator to a bottom corner of a triangle with its apex up: on the line of its base, as the
            fermion line of a self-energy *)
         AppendTo[pos, {v, posOf[First[axial]] + 1.2 $blobPointDistance ports[[portIndex[First[axial]], 4]]}];
         AppendTo[axialPoints, {v, ports[[portIndex[First[axial]], 4]]}],
         (* further from two or more ports, so that its lines do not meet at a sharp angle *)
         AppendTo[pos, {v, Mean[posOf /@ pn] + If[Length[pn] > 1, 1.15, 1.] $blobPointDistance unitVectorOf[dir]}]]]; progress = True,
       nb =!= {},
       AppendTo[pos, {v, Mean[posOf /@ nb] + $blobPointDistance unitVectorOf[Mean[posOf /@ nb] - centroid]}]; progress = True]],
     {v, others}]];
   If[! AllTrue[others, placedQ], Return[None]];
   centroid = Mean[pos[[All, 2]]];
   (* the legs: outwards from a port (along the side of the blob if a neighbouring blob is in the way), else in the
      largest free sector *)
   Do[If[amputated[[k]], With[{q = labelNodes[[k]]},
      If[MemberQ[portNodes, q],
       With[{pr = ports[[portIndex[q]]]}, Module[{u = pr[[4]], c = centres[[pr[[2]]]]},
         Do[If[j =!= pr[[2]], With[{dc = centres[[j]] - c},
            Which[
             Abs[dc[[2]]] < 0.5 $declaredBlobSize && dc[[1]] u[[1]] > 0.01 && Abs[u[[2]]] > 0.01, u = {0., Sign[u[[2]]]},
             Abs[dc[[1]]] < 0.5 $declaredBlobSize && dc[[2]] u[[2]] > 0.01 && Abs[u[[1]]] > 0.01, u = {Sign[u[[1]]], 0.}]]],
          {j, Length[centres]}];
         AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength u}]]],
       If[MemberQ[axialPoints[[All, 1]], q],
        (* a point on the base of a triangle: its leg continues the base *)
        AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength First[Cases[axialPoints, {q, u_} :> u]]}],
        AppendTo[legPositions, {leg[labels[[k]]], posOf[q] + $legLength directionAt[gapDirection[
              Table[angleOf[posOf[w] - posOf[q]], {w, nbrsOf[q]}], preferredDirection[posOf[q], centroid]]]}]]]]],
    {k, Length[labels]}];
   (* the blobs centred on the axis of the formula *)
   With[{m = Mean[centres]},
    discs = Table[{centres[[i]] - m, blobSpecOf[frameBlobs[[i, 3]]][[2]], blobSizeOfHead[frameBlobs[[i, 3]]]}, {i, Length[frameBlobs]}];
    {Table[{q[[1]], q[[2]] - m}, {q, Join[pos, legPositions]}], {"blob", discs, centroid - m}, {}}]];

(* the chords that hatch a blob, at 45 degrees (sense 1) or at -45 degrees (sense -1); for a triangle, the parts of the
   lines t n + s e inside it. A diamond, whose edges are at 45 degrees, is hatched vertically (sense 1) and horizontally
   (sense -1), so that no hatch line runs along an edge and doubles it *)
triangleChord[t_, e_, n_, corners_] := Module[{ss = {}},
   Do[With[{a = corners[[i]], b = corners[[Mod[i, Length[corners]] + 1]]},
     With[{den = (b - a) . n},
      If[Abs[den] > 10.^-9, With[{u = (t - a . n)/den}, If[0 <= u <= 1, AppendTo[ss, (a + u (b - a)) . e]]]]]], {i, Length[corners]}];
   If[Length[ss] < 2, None, {t n + Min[ss] e, t n + Max[ss] e}]];
blobHatchSegments[p_, shape : "Triangle" | "TriangleUp" | "Diamond", h_, sense_] := With[{e = If[shape === "Diamond", If[sense == 1, {0., 1.}, {1., 0.}], {1., sense}/Sqrt[2.]],
    n = If[shape === "Diamond", If[sense == 1, {1., 0.}, {0., 1.}], {1., -sense}/Sqrt[2.]],
    corners = triangleCorners[{0., 0.}, h, shape]},
   Select[Table[With[{c = triangleChord[t, e, n, corners]}, If[c === None, None, p + # & /@ c]],
     {t, $hatchSpacing Range[-Floor[1.5 h/$hatchSpacing], Floor[1.5 h/$hatchSpacing]]}], # =!= None && vectorNorm[#[[2]] - #[[1]]] > 0.02 &]];
blobHatchSegments[p_, shape_, h_, sense_] := Module[{e = {1., sense}/Sqrt[2.], n = {1., -sense}/Sqrt[2.], tmax, m},
   tmax = If[shape === "Box", Sqrt[2.] h, h]; m = Floor[(tmax - 0.02)/$hatchSpacing];
   Table[With[{half = If[shape === "Box", Sqrt[2.] h - Abs[t], Sqrt[Max[0., h^2 - t^2]]]},
     {p + t n - half e, p + t n + half e}], {t, $hatchSpacing Range[-m, m]}]];
blobHatchesOf[p_, {_, shape_, fill_, h_}] := Switch[fill,
   "Hatched", blobHatchSegments[p, shape, h, 1],
   "CrossHatched", Join[blobHatchSegments[p, shape, h, 1], blobHatchSegments[p, shape, h, -1]],
   _, {}];
(* the label of a blob as plain text, for the pictures of Mathematica: Greek letters, without the LaTeX markup; a tilde
   or a hat as a combining accent on the letter that follows it *)
plainBlobLabel[label_String] := StringReplace[StringReplace[StringReplace[label, {"\\overline" -> "", "\\mathrm" -> "", "\\rm" -> "",
      "\\Sigma" -> "\[CapitalSigma]", "\\Gamma" -> "\[CapitalGamma]", "\\Lambda" -> "\[CapitalLambda]", "\\Phi" -> "\[CapitalPhi]",
      "\\Pi" -> "\[CapitalPi]", "\\Delta" -> "\[CapitalDelta]", "\\chi" -> "\[Chi]"}],
    {"{" -> "", "}" -> "", "^" -> "", "_" -> "", " " -> ""}],
   {"\\tilde" ~~ c_ :> c <> "\:0303", "\\hat" ~~ c_ :> c <> "\:0302", "\\" -> ""}];
(* where the label of a blob is written: the centre, or near the incentre of a triangle (blobLabelPlacement) *)
blobLabelPoint[p_, {label_, shape_, fill_, h_}] := p + If[triangleShapeQ[shape] && StringQ[label],
    blobLabelPlacement[shape, h, blobLabelBox[label, fill, shape]][[1]], {0., 0.}];
declaredBlobPrimitives[p_, spec : {label_, shape_, fill_, h_}] := {
   If[fill === "Grey", GrayLevel[0.8], White],
   Switch[shape, "Box", Rectangle[p - {h, h}, p + {h, h}], "Triangle" | "TriangleUp" | "Diamond", Polygon[triangleCorners[p, h, shape]], _, Disk[p, h]],
   Black, {AbsoluteThickness[0.6], Line /@ blobHatchesOf[p, spec]},
   (* on a hatched blob the label sits in a white clearing with rounded corners, the outline drawn over it (as in TikZ) *)
   Text[If[MemberQ[{"Hatched", "CrossHatched"}, fill],
     Framed[Style[plainBlobLabel[label], blobLabelScale[shape] $fontPixels], Background -> White, FrameStyle -> White, RoundingRadius -> 3,
      FrameMargins -> {{2, 2}, {1, 2}}],
     Style[plainBlobLabel[label], blobLabelScale[shape] $fontPixels]], blobLabelPoint[p, spec]],
   Switch[shape, "Box", Line[{p + {-h, -h}, p + {h, -h}, p + {h, h}, p + {-h, h}, p + {-h, -h}}],
    "Triangle" | "TriangleUp" | "Diamond", Line[Append[triangleCorners[p, h, shape], First[triangleCorners[p, h, shape]]]], _, Circle[p, h]]};

(* the unit layout of a vacuum part, or None *)
unitLayoutVacuum[component_List, lines_List] := Module[{frameLines = Select[lines, MemberQ[component, #[[1]]] &], fl},
   fl = fermionLines[component, frameLines, {}];
   If[fl === None || fl[[1]] =!= {}, None, layoutVacuumTree[fl[[2]], interactionPairsOf[component, frameLines]]]];

(* ---------------------------------------------------------------- the elements of a term *)
styleForHead[h_, opts_List] := Module[{style = Which[
      MemberQ[Flatten[{drawingOption[opts, "Directed"]}], h], "Arrow",
      MemberQ[Flatten[{drawingOption[opts, "Interactions"]}], h], "Wavy",
      True, lineStyleOf[h]], override = drawingOption[opts, "InteractionStyle"]},
   If[interactionStyleQ[style] && MemberQ[{"Wavy", "DoubleWavy", "Curly", "Dashed", "Dotted", "Zigzag"}, override], override, style]];

(* {weight, lines, blobs, stubs, classes}: lines {tail, head, factor head, style} (a directed line G[x, y] runs from y
   to x), blobs {id, legs, head}, stubs {label, head}; classes: the points joined by delta functions *)
diagramElements[diagramTerm[c_, o_, d_], ext_List, opts_List] := Module[{factors = expandIntegerPowers[Join[factorList[c], o]], points,
    deltaPairs = {}, weight = 1, lines = {}, blobs = {}, stubs = {}, blobCount = 0, classes, representative, rules},
   points = DeleteDuplicates[Join[ext, d, Flatten[Cases[factors, DeltaFunction[x_, y_] :> {x, y}]]]];
   Do[If[AtomQ[f], weight = weight f,
     With[{labels = Select[List @@ f, MemberQ[points, #] &]},
      Which[
       Head[f] === DeltaFunction && Length[labels] == 2, AppendTo[deltaPairs, labels],
       (* a local vertex: its labels are one point (without labels it is a factor of the weight) *)
       localVertexQ[f] && Length[labels] >= 1, deltaPairs = Join[deltaPairs, Table[{First[labels], l}, {l, Rest[labels]}]],
       labels === {}, weight = weight f,
       (* a declared blob, even with two labels *)
       declaredBlobQ[f] && Length[labels] >= 2, blobCount++; AppendTo[blobs, {blob[blobCount], labels, Head[f]}],
       Length[labels] == 1, AppendTo[stubs, {labels[[1]], Head[f]}],
       Length[labels] == 2 && Length[f] == 2, With[{style = styleForHead[Head[f], opts]},
        AppendTo[lines, If[directedStyleQ[style], {labels[[2]], labels[[1]], Head[f], style}, {labels[[1]], labels[[2]], Head[f], style}]]],
       True, blobCount++; AppendTo[blobs, {blob[blobCount], labels, Head[f]}]]]], {f, factors}];
   classes = mergeOverlapping[Table[{p}, {p, points}], deltaPairs];
   (* the representative of a class: its first external label, else its first point *)
   representative[x_] := With[{cl = First[Select[classes, MemberQ[#, x] &]]},
     With[{e = Select[ext, MemberQ[cl, #] &]}, If[e =!= {}, First[e], First[Select[points, MemberQ[cl, #] &]]]]];
   rules = Table[p -> representative[p], {p, points}];
   lines = Table[{li[[1]] /. rules, li[[2]] /. rules, li[[3]], li[[4]]}, {li, lines}];
   blobs = Table[{bl[[1]], bl[[2]] /. rules, bl[[3]]}, {bl, blobs}];
   stubs = Table[{st[[1]] /. rules, st[[2]]}, {st, stubs}];
   {weight, lines, blobs, stubs, classes, rules}];

(* a point joining one line to one leg of a blob is not drawn: the line ends on the blob. The legs of a declared blob
   stay: they are its ports, on its boundary *)
attachLinesToBlobs[lines0_, blobs0_, stubs_, d_] := Module[{lines = lines0, blobs = blobs0, li, bi},
   Do[li = Select[Range[Length[lines]], MemberQ[lines[[#, {1, 2}]], x] &];
    bi = Select[Range[Length[blobs]], MemberQ[blobs[[#, 2]], x] &];
    If[Length[li] == 1 && Count[lines[[li[[1]], {1, 2}]], x] == 1 && Length[bi] == 1 && Count[blobs[[bi[[1]], 2]], x] == 1 &&
      FreeQ[stubs, x] && ! declaredBlobHeadQ[blobs[[bi[[1]], 3]]],
     lines[[li[[1]]]] = Join[lines[[li[[1]], {1, 2}]] /. x -> blobs[[bi[[1]], 1]], lines[[li[[1]], {3, 4}]]];
     blobs[[bi[[1]], 2]] = DeleteCases[blobs[[bi[[1]], 2]], x]], {x, d}];
   {lines, blobs}];

(* is the external label drawn with a leg (amputated) or as an open circle at the end of a propagator? *)
amputatedLabelQ[e_, lines_, blobs_, stubs_, classes_, opts_List] := Module[{setting = drawingOption[opts, "AmputatedLegs"], ends, cl},
   Which[setting === True, True, setting === False, False,
    True,
    cl = First[Select[classes, MemberQ[#, e] &], {e}];
    ends = Count[Flatten[Cases[lines, {u_, v_, _, _} :> {u, v}]], e] + Count[Flatten[Cases[blobs, {_, l_, _} :> l]], e] +
      Count[Cases[stubs, {u_, _} :> u], e];
    (* a label alone on its point, at the end of a single line (a propagator or an interaction line), is drawn as an
       open circle; otherwise it is a vertex of an amputated function, drawn with a leg *)
    ! (Length[cl] == 1 && ends == 1 && Length[Cases[lines, {e, u_, _, _} /; u =!= e]] + Length[Cases[lines, {u_, e, _, _} /; u =!= e]] == 1)]];

(* directed lines entering minus leaving the node, for the arrows of the legs; an end of -<T psi psi> counts as entering,
   an end of -<T psibar psibar> as leaving *)
fermionImbalance[node_, lines_] := Count[lines, {u_, node, _, s_} /; u =!= node && directedStyleQ[s]] -
   Count[lines, {node, u_, _, s_} /; u =!= node && directedStyleQ[s]] +
   Count[lines, {u_, w_, _, "DivergingArrows"} /; u =!= w && (u === node || w === node)] -
   Count[lines, {u_, w_, _, "ConvergingArrows"} /; u =!= w && (u === node || w === node)];

(* directions ("Out", "In" or None) of the legs of the external labels, from all the terms of an expression (the
   diagramElements of each term) *)
legDirectionsFor[elementsList_List, ext_List, opts_List] := Module[{spec = drawingOption[opts, "LegDirections"], votes = {}, anyDirected = False,
    rules, dir},
   Which[
    ListQ[spec] && Length[spec] == Length[ext], Thread[ext -> spec],
    spec === None, Thread[ext -> None],
    spec === "Alternating", Thread[ext -> Table[If[OddQ[k], "Out", "In"], {k, Length[ext]}]],
    True,
    Do[If[AnyTrue[elements[[2]], fermionStyleQ[#[[4]]] &], anyDirected = True];
     Do[With[{cl = First[Select[elements[[5]], MemberQ[#, e] &], {}]},
       (* a label alone on its point: one more line entering than leaving means an outgoing leg, and vice versa;
          as many entering as leaving (e.g. a density vertex) means a leg without arrow *)
       If[cl =!= {} && Count[cl, x_ /; MemberQ[ext, x]] == 1 && MemberQ[Flatten[Cases[elements[[2]], {u_, v_, _, _} :> {u, v}]], e],
        With[{b = fermionImbalance[e, elements[[2]]]},
         AppendTo[votes, e -> Which[b == 1, "Out", b == -1, "In", True, None]]]]],
      {e, ext}], {elements, elementsList}];
    Table[With[{v = Union[Cases[votes, (ext[[k]] -> x_) :> x]]},
      ext[[k]] -> Which[Length[v] == 1, First[v], Length[v] > 1, None,
        anyDirected || MemberQ[votes, _ -> "Out" | "In"], If[OddQ[k], "Out", "In"], True, None]], {k, Length[ext]}]]];

(* The external labels at which a density (two fields at one point, a composite operator such as psibar psi or phi^2) is
   inserted: in every term the label is alone at its point and its point ends exactly two propagators, one entering and
   one leaving (fermions) or two undirected ones (bosons), and nothing else (no interaction line, blob or source). Such a
   point is the insertion of an external field U(z): it is drawn as the crossed circle of a source insertion, without a
   leg. The same label may be the third corner of a triangle blob (a vertex of the density, DeclareBlob with "Shape" ->
   "Triangle"), alone there, or share its point with exactly two other labels, one with an outgoing and one with an
   incoming leg, and nothing else (the bare vertex, delta(x1, z) delta(x2, z)): the crossed circle is then drawn at the
   corner or at that point, with the label. "AmputatedLegs" -> True or False turns this off. *)
$densityLabels = {}; $densityNodes = {}; $densityNodeLabels = {}; $multiplePairs = {}; $selfLoopNodes = {};
densityLabelsFor[terms_List, elementsList_List, ext_List, opts_List, directions_List : {}] := Module[{t, elements, ok, seen, node, ends, at,
    blobLegs, cl},
   If[drawingOption[opts, "AmputatedLegs"] === True || drawingOption[opts, "AmputatedLegs"] === False, Return[{}]];
   ok = Table[True, {Length[ext]}]; seen = Table[False, {Length[ext]}];
   Do[t = terms[[i]]; elements = elementsList[[i]];
    Do[node = ext[[k]] /. elements[[6]];
     at = Select[elements[[2]], #[[1]] === node || #[[2]] === node &];
     blobLegs = Flatten[Cases[elements[[3]], {_, l_, _} :> l]];
     cl = First[Select[elements[[5]], MemberQ[#, ext[[k]]] &], {ext[[k]]}];
     Which[
      (* the third corner of a triangle blob *)
      at === {} && Count[blobLegs, node] == 1 && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] &&
       Count[cl, x_ /; MemberQ[ext, x]] == 1 &&
       MatchQ[Cases[elements[[3]], {_, l_, h_} /; MemberQ[l, node] :> {l, blobSpecOf[h]}],
        {{l_, sp_}} /; Length[l] == 3 && l[[3]] === node && ListQ[sp] && MemberQ[{"Triangle", "TriangleUp"}, sp[[2]]]],
      seen[[k]] = True,
      (* the bare vertex: three labels at a point without lines, the other two with an outgoing and an incoming leg; for
         bosons (legs without direction) the last label is the density *)
      at === {} && ! MemberQ[blobLegs, node] && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] && Length[cl] == 3 &&
       Count[cl, x_ /; MemberQ[ext, x]] == 3 && MemberQ[{None, ext[[k]]}, ext[[k]] /. directions] &&
       (Sort[(DeleteCases[cl, ext[[k]]] /. directions)] === {"In", "Out"} ||
        k == Length[ext] && AllTrue[DeleteCases[cl, ext[[k]]], MemberQ[{None, #}, # /. directions] &]),
      seen[[k]] = True,
      at =!= {} || MemberQ[blobLegs, node] || MemberQ[Cases[elements[[4]], {u_, _} :> u], node],
      seen[[k]] = True;
      ends = Count[Flatten[at[[All, {1, 2}]]], node];
      If[! (Count[First[Select[elements[[5]], MemberQ[#, ext[[k]]] &], {ext[[k]]}], x_ /; MemberQ[ext, x]] == 1 && ends == 2 &&
           (* a label of a local vertex (DeclareLocalVertex) is a leg of that vertex *)
           FreeQ[t[[1]], h_Symbol[___, ext[[k]], ___] /; MemberQ[$localVertexHeads, h]] &&
           ! MemberQ[Flatten[Cases[elements[[3]], {_, l_, _} :> l]], node] && ! MemberQ[Cases[elements[[4]], {u_, _} :> u], node] &&
           ! AnyTrue[at, interactionStyleQ[#[[4]]] &] &&
           (AllTrue[at, fermionStyleQ[#[[4]]] &] && fermionImbalance[node, at] == 0 || ! AnyTrue[at, fermionStyleQ[#[[4]]] &])),
       ok[[k]] = False]], {k, Length[ext]}], {i, Length[terms]}];
   Pick[ext, Table[ok[[k]] && seen[[k]], {k, Length[ext]}]]];

(* offsets of the lines of a bundle between two nodes at distance dd; kind: "plain", "axis", {"skip", k}, {"cycle", s} *)
bundleOffsets[lines_, dd_, kind_] := Module[{propagators = Select[lines, ! interactionStyleQ[#[[4]]] &],
    interactions = Select[lines, interactionStyleQ[#[[4]]] &], k, h, symmetric, ordered},
   symmetric[m_] := If[m <= 1, Table[0., {m}], Table[0.9 dd ((j - 1)/(m - 1) - 1/2), {j, m}]];
   Which[
    (* only bosonic lines (gluons, phonons) between neighbours on the axis: a bubble, symmetric about the axis *)
    kind === "axis" && propagators === {} && Length[interactions] >= 2,
    {interactions, Table[0.5 dd (2 (j - 1)/(Length[interactions] - 1) - 1), {j, Length[interactions]}]},
    kind === "axis", {Join[propagators, interactions], Join[symmetric[Length[propagators]],
       Table[If[propagators === {}, 0.45 dd j - 0.45 dd, Max[symmetric[Length[propagators]]] + 0.45 dd j], {j, Length[interactions]}]]},
    kind === "axisBelow", {Join[propagators, interactions], Join[symmetric[Length[propagators]],
       Table[If[propagators === {}, -(0.45 dd j - 0.45 dd), -(Max[symmetric[Length[propagators]]] + 0.45 dd j)], {j, Length[interactions]}]]},
    ListQ[kind] && kind[[1]] === "skip", {lines, Table[kind[[3]] (If[Length[kind] >= 4 && NumberQ[kind[[4]]], kind[[4]], 0.26 + 0.1 (kind[[2]] - 1)] +
        0.12 (j - 1)) dd, {j, Length[lines]}]},
    (* along the arc of a round loop: an interaction line between the same two vertices well inside it, clear of the arc *)
    ListQ[kind] && kind[[1]] === "bubbleCircle", {Join[propagators, interactions], kind[[{2, 3}]]},
    ListQ[kind] && kind[[1]] === "circle", ordered = Join[propagators, interactions];
    (* three or more lines (a sunset on the loop): a melon about the chord, the innermost no deeper than the arc is high,
       not a second circle inside the loop *)
    With[{step = If[Length[ordered] >= 3, Max[0.3 dd, Min[0.5 dd, 2 Abs[kind[[2]]]/(Length[ordered] - 1)]], 0.5 dd]},
     {ordered, Table[kind[[2]] - Sign[kind[[2]]] step (j - 1), {j, Length[ordered]}]}],
    ListQ[kind], ordered = Join[propagators, interactions]; {ordered, Table[kind[[2]] - Sign[kind[[2]]] 0.35 dd (j - 1), {j, Length[ordered]}]},
    True,
    k = Length[propagators]; h = Ceiling[k/2];
    ordered = Join[Take[propagators, h], interactions, Drop[propagators, h]];
    {ordered, symmetric[Length[ordered]]}]];

preferredDirection[p_, centre_] := If[vectorNorm[p - centre] < 0.05, {1., 0.}, unitVectorOf[p - centre] + {0., 0.3}];

labelString[DummyVariable[_, k_]] := "z" <> ToString[k];
labelString[x_] := ToString[x];
labelBox[x_] := {0.62 $fontPixels StringLength[labelString[x]], 0.75 $fontPixels}/$pixelsPerUnit;

(* geometry of one frame (the part with the external labels, or one vacuum component). Items:
   {"E", path, style} lines; {"L", node point, end point, direction} legs; {"S", {p, q}} source insertions;
   {"N", p, kind} nodes ("dot", "ext", "blob"); {"T", p, label, a, angle} labels centred at p, or attached at a in the direction angle. *)
(* the circle through three points: {centre, radius}, or None if they are (nearly) on one line *)
circleThrough[p1_, p2_, p3_] := Module[{a = p2 - p1, b = p3 - p1, d},
   d = 2 (a[[1]] b[[2]] - a[[2]] b[[1]]);
   If[Abs[d] < 0.1 vectorNorm[a] vectorNorm[b] + 10.^-12, None,
    With[{c = p1 + {b[[2]] (a . a) - a[[2]] (b . b), a[[1]] (b . b) - b[[1]] (a . a)}/d}, {c, vectorNorm[c - p1]}]]];

(* bubbles (closed loops of two vertices, of two propagators or of two interaction lines) joined by one interaction line at
   each vertex to two points further apart than the bubble is wide (a dressed interaction line hanging from a fermion
   line): {{point node, bubble vertex, centre, radius}, ...}, the circle through the two points and the centre of the
   bubble, along which the lines are bent *)
bubbleBends[lines_, point_, legEnds_List : {}] := Module[{nonSelf = Select[lines, #[[1]] =!= #[[2]] &], out = {}, linesAt, clearQ},
   linesAt[b_, pr_] := Select[nonSelf, MemberQ[#[[{1, 2}]], b] && Sort[#[[{1, 2}]]] =!= pr &];
   (* the bent line leaves its outer point a, towards b, along the circle {c, r}: not within 45 degrees of another line
      or leg there (it would run into the arrow of a leg) *)
   clearQ[a_, b_, c_] := With[{pa = point[a]}, With[{tg = With[{t = rotate90[unitVectorOf[pa - c]]}, If[t . (point[b] - pa) < 0, -t, t]]},
      AllTrue[Join[Table[point[First[DeleteCases[l[[{1, 2}]], a]]] - pa, {l, Select[nonSelf, MemberQ[#[[{1, 2}]], a] && ! MemberQ[#[[{1, 2}]], b] &]}],
        Cases[legEnds, {a, e_} :> e - pa]], vectorNorm[#] < 10.^-6 || Abs[angleDifference[angleOf[#], angleOf[tg]]] > 45. Degree &]]];
   Do[With[{b1 = pr[[1]], b2 = pr[[2]], bw = Select[nonSelf, Sort[#[[{1, 2}]]] === pr &], o1 = linesAt[pr[[1]], pr], o2 = linesAt[pr[[2]], pr]},
     If[Length[bw] == 2 && interactionStyleQ[bw[[1, 4]]] === interactionStyleQ[bw[[2, 4]]] && Length[o1] == 1 && Length[o2] == 1 &&
       interactionStyleQ[o1[[1, 4]]] && interactionStyleQ[o2[[1, 4]]],
      With[{i1 = DeleteCases[o1[[1, {1, 2}]], b1], i2 = DeleteCases[o2[[1, {1, 2}]], b2]},
       If[Length[i1] == 1 && Length[i2] == 1 && i1[[1]] =!= i2[[1]] && ! MemberQ[pr, i1[[1]]] && ! MemberQ[pr, i2[[1]]],
        With[{pa1 = point[i1[[1]]], pa2 = point[i2[[1]]], pb1 = point[b1], pb2 = point[b2]},
         If[(pa2 - pa1) . unitVectorOf[pb2 - pb1] > vectorNorm[pb2 - pb1] + 0.3,
          With[{circ = circleThrough[pa1, (pb1 + pb2)/2, pa2]},
           If[circ =!= None && clearQ[i1[[1]], b1, circ[[1]]] && clearQ[i2[[1]], b2, circ[[1]]],
            out = Join[out, {{i1[[1]], b1, circ[[1]], circ[[2]]}, {i2[[1]], b2, circ[[1]], circ[[2]]}}]]]]]]]]],
    {pr, DeleteDuplicates[Sort /@ nonSelf[[All, {1, 2}]]]}];
   out];

frameItems[frameNodes_, positions_, lines_, blobs_, stubs_, ext_, amputatedExt_, legDirections_, d_, mode_, vertexLabels_] := Module[
   {rules = Rule @@@ positions, point, centre = mode[[-1]], items = {}, placed = {}, frameLines, nonLoops, loops, spokes, groups, skips,
    skipsAbove, skipsBelow, skipBase, diameterQ, graphDiameterQ, threeDiameters, diameterCount, bends, ports, sourceEnds, bubbleGroupQ, lensLimit, outsideChords, loopChordCircles, linePointsForLabels,
    nodeIndex, directions, add, kind, uv, es2, offsets, pu, pv, path, p, th, angles, label, cycle, backbone, legs, loopDiscs},
   If[mode[[1]] === "cycle", centre = mode[[3]]];
   point[v_] := Replace[v, rules];
   nodeIndex[v_] := First[First[Position[frameNodes, v, {1}]]];
   frameLines = Select[lines, MemberQ[frameNodes, #[[1]]] &];
   ports = Flatten[Cases[blobs, {_, l_, h_} /; declaredBlobHeadQ[h] :> l]];
   nonLoops = Select[frameLines, #[[1]] =!= #[[2]] &]; loops = Select[frameLines, #[[1]] === #[[2]] &];
   (* the spokes of a blob to its ports, hidden by it; to a point shared with another blob, from its port (blobPortCorner) *)
   spokes = Flatten[Table[{If[MemberQ[positions[[All, 1]], blobPortCorner[b[[1]], x]], blobPortCorner[b[[1]], x], b[[1]]], x, None, "Spoke"},
      {b, Select[blobs, MemberQ[frameNodes, #[[1]]] &]}, {x, b[[2]]}], 1];
   directions = Table[{v, {}}, {v, frameNodes}];
   add[v_, a_] := With[{k = Position[frameNodes, v, {1}]}, If[k =!= {}, directions[[k[[1, 1]], 2]] = Append[directions[[k[[1, 1]], 2]], a]]];
   (* external legs *)
   legs = Select[amputatedExt, MemberQ[frameNodes, #[[2]]] &];
   Do[With[{q = point[l[[2]]], e = point[leg[l[[1]]]]},
      AppendTo[items, {"L", q, e, l[[1]] /. legDirections}]; add[l[[2]], angleOf[e - q]]], {l, legs}];
   bends = bubbleBends[frameLines, point, Table[{l[[2]], point[leg[l[[1]]]]}, {l, legs}]];
   groups = GatherBy[Join[nonLoops, spokes], Sort[#[[{1, 2}]]] &];
   (* lines joining non-adjacent vertices of the axis: arcs above it, higher for later ones *)
   skips = If[mode[[1]] === "backbone",
     With[{bb0 = mode[[2]]}, With[{ix = First[First[Position[bb0, #, {1}]]] &},
       SortBy[Select[SortBy[#, ix] & /@ Cases[groups[[All, 1, {1, 2}]], {u_, v_} /; MemberQ[bb0, u] && MemberQ[bb0, v]],
         Abs[ix[#[[2]]] - ix[#[[1]]]] > 1 &], {ix[#[[2]]] - ix[#[[1]]], ix[#[[1]]]} &]]], {}];
   (* arcs drawn below the axis (crossing lines of two-point functions), and those above; each set numbered apart *)
   skipsBelow = If[mode[[1]] === "backbone", Select[skips, MemberQ[mode[[4]], Sort[#]] &], {}];
   skipsAbove = Select[skips, ! MemberQ[skipsBelow, #] &];
   (* the bulge of each arc, in units of its chord: higher for later (longer) ones; two arcs on one side that cross over
      less than half of the span of one of them are round, so that they cross high above the axis at a wide angle
      (flat arcs crossed in a knot just above it); an arc over another one stays above it *)
   skipBase = If[mode[[1]] =!= "backbone", {},
     With[{ix = First[First[Position[mode[[2]], #, {1}]]] &},
      Flatten[Table[Module[{base, spans = Table[ix /@ s, {s, set}], dds = Table[vectorNorm[point[s[[2]]] - point[s[[1]]]], {s, set}]},
         base = Table[0.26 + 0.1 (i - 1), {i, Length[set]}];
         Do[If[i != j && spansCrossQ[spans[[i]], spans[[j]]] &&
             Min[spans[[i, 2]], spans[[j, 2]]] - Max[spans[[i, 1]], spans[[j, 1]]] <= (spans[[i, 2]] - spans[[i, 1]])/2,
            base[[i]] = Max[base[[i]], 0.5]], {i, Length[set]}, {j, Length[set]}];
         Do[If[spans[[i, 1]] <= spans[[j, 1]] && spans[[j, 2]] <= spans[[i, 2]] && dds[[i]] > 10.^-6,
            base[[i]] = Max[base[[i]], (base[[j]] dds[[j]] + 0.3)/dds[[i]]]], {i, Length[set]}, {j, i - 1}];
         Table[set[[i]] -> base[[i]], {i, Length[set]}]], {set, {skipsAbove, skipsBelow}}]]]];
   diameterQ[pq_] := mode[[1]] === "cycle" && MemberQ[mode[[2]], pq[[1]]] && MemberQ[mode[[2]], pq[[2]]] &&
     segmentDistance[centre, {point[pq[[1]]], point[pq[[2]]]}] < 0.08;
   diameterCount = Count[Table[diameterQ[es[[1, {1, 2}]]], {es, groups}], True];
   (* two chords of a round loop (single interaction lines between vertices that are not neighbours on it) that cross each
      other: one inside the loop, the other as an arc outside it (the rule of the arcs above and below an axis) *)
   outsideChords = {};
   (* of three diameters (opposite vertices of a loop of six), the horizontal one (layoutVacuumCycle) *)
   graphDiameterQ[pq_] := mode[[1]] === "cycle" && Length[mode[[2]]] == 6 && MemberQ[mode[[2]], pq[[1]]] && MemberQ[mode[[2]], pq[[2]]] &&
     Abs[First[First[Position[mode[[2]], pq[[1]], {1}]]] - First[First[Position[mode[[2]], pq[[2]], {1}]]]] == 3;
   threeDiameters = mode[[1]] === "cycle" && Count[Table[graphDiameterQ[es[[1, {1, 2}]]], {es, groups}], True] == 3;
   If[threeDiameters,
    outsideChords = Select[Table[Sort[es[[1, {1, 2}]]], {es, groups}], graphDiameterQ[#] && Abs[(point[#[[1]]] - point[#[[2]]])[[2]]] < 0.05 &, 1]];
   If[mode[[1]] === "cycle" && diameterCount < 3,
    With[{cyc = mode[[2]]}, With[{ix = First[First[Position[cyc, #, {1}]]] &, L = Length[mode[[2]]]},
      Module[{chords = Select[Sort /@ groups[[All, 1, {1, 2}]], MemberQ[cyc, #[[1]]] && MemberQ[cyc, #[[2]]] &&
           Mod[ix[#[[2]]] - ix[#[[1]]], L] != 1 && Mod[ix[#[[1]]] - ix[#[[2]]], L] != 1 &], inside = {}, interleaved},
       interleaved[c1_, c2_] := With[{a1 = Min[ix /@ c1], b1 = Max[ix /@ c1], a2 = ix[c2[[1]]], b2 = ix[c2[[2]]]},
         Intersection[c1, c2] === {} && Xor[a1 < a2 < b1, a1 < b2 < b1]];
       (* with three or more chords the layout of the cycle has already moved their crossings apart *)
       If[Length[chords] > 2, chords = {}];
       (* the chord drawn outside is the one that bulges most nearly up or down, away from the weight on the left *)
       chords = SortBy[chords, Function[ch, With[{pu = point[ch[[1]]], pv = point[ch[[2]]]}, With[{m = (pu + pv)/2},
            Abs[If[vectorNorm[m - centre] > 0.05, unitVectorOf[m - centre], rotate90[unitVectorOf[pv - pu]]][[2]]]]]]];
       Do[Which[
         ! AnyTrue[inside, interleaved[ch, #] &], AppendTo[inside, ch],
         ! AnyTrue[outsideChords, interleaved[ch, #] &], AppendTo[outsideChords, ch],
         True, AppendTo[inside, ch]], {ch, chords}]]]]];
   (* the same for a round loop hanging from a line or a point (not the cycle of a vacuum diagram): of two crossing chords,
      the one whose middle is farthest from where the loop is joined is drawn outside it *)
   loopChordCircles = {};
   Do[With[{lp = cs[[1]], cc = cs[[2]], rr = cs[[3]]},
     If[! (mode[[1]] === "cycle" && Sort[lp] === Sort[mode[[2]]]),
      Module[{chords, ixl, joinedAt, away, between},
       ixl[v_] := First[First[Position[lp, v, {1}]]];
       chords = Sort /@ Select[groups, Length[#] == 1 && interactionStyleQ[#[[1, 4]]] && #[[1, 1]] =!= #[[1, 2]] &&
            MemberQ[lp, #[[1, 1]]] && MemberQ[lp, #[[1, 2]]] && ! consecutiveOnLoopQ[lp, #[[1, 1]], #[[1, 2]]] &][[All, 1, {1, 2}]];
       between[c1_, c2_] := With[{a1 = Min[ixl /@ c1], b1 = Max[ixl /@ c1]},
         Intersection[c1, c2] === {} && Xor[a1 < ixl[c2[[1]]] < b1, a1 < ixl[c2[[2]]] < b1]];
       If[Length[chords] == 2 && between[chords[[1]], chords[[2]]],
        joinedAt = Select[lp, Function[v, AnyTrue[frameLines, (#[[1]] === v && ! MemberQ[lp, #[[2]]]) || (#[[2]] === v && ! MemberQ[lp, #[[1]]]) &] ||
            AnyTrue[legs, #[[2]] === v &]]];
        away = If[joinedAt === {} || vectorNorm[cc - Mean[point /@ joinedAt]] < 10.^-6, {0., 1.}, unitVectorOf[cc - Mean[point /@ joinedAt]]];
        With[{ch = Last[SortBy[chords, Function[c, With[{m = (point[c[[1]]] + point[c[[2]]])/2},
               If[vectorNorm[m - cc] > 0.05 rr, unitVectorOf[m - cc] . away, 0.]]]]]},
         AppendTo[outsideChords, ch]; AppendTo[loopChordCircles, {ch, cc, rr, away}]]]]]],
    {cs, $loopCircles}];
   (* the largest angle at which the arcs of a bubble {u, v} may leave its vertices: half the angle to another bubble at
      one of them, a little less than the angle to a single line or leg that would run into the bubble or leave along
      its arc (closer to the chord than a right angle, or about at a right angle: a leg beside a semicircle) *)
   bubbleGroupQ[g_] := Count[g, {_, _, _, s_} /; s =!= "Spoke" && ! interactionStyleQ[s]] >= 2;
   lensLimit[uv0_] := Module[{lim = Infinity, single},
     Do[With[{a = ab[[1]], th0 = angleOf[point[ab[[2]]] - point[ab[[1]]]]},
       (* the signed angles from the chord of the single lines and legs at a *)
       single = {};
       Do[If[Sort[g[[1, {1, 2}]]] =!= Sort[uv0] && MemberQ[g[[1, {1, 2}]], a],
         With[{w = First[DeleteCases[g[[1, {1, 2}]], a]]},
          With[{sd = angleDifference[angleOf[point[w] - point[a]], th0]},
           If[bubbleGroupQ[g], lim = Min[lim, Abs[sd]/2 - 0.05], AppendTo[single, sd]]]]],
        {g, groups}];
       Do[If[l[[2]] === a, AppendTo[single, angleDifference[angleOf[point[leg[l[[1]]]] - point[a]], th0]]], {l, legs}];
       (* a line at about a right angle to the chord on one side only (a leg beside a semicircle) limits the lens too; with
          lines on both sides (a loop sitting on a line) the bubble stays round *)
       Do[Which[Abs[sd] < Pi/2 - 0.05, lim = Min[lim, Abs[sd] - 0.15],
         Abs[sd] < Pi/2 + 0.3 && ! AnyTrue[single, # sd < 0 && Abs[Abs[#] - Pi/2] < 0.3 &], lim = Min[lim, Abs[sd] - 0.3]],
        {sd, single}]],
      {ab, {uv0, Reverse[uv0]}}];
     lim];
   Do[
    (* the end that is not a node of the frame (the port of a blob at a shared point) first *)
    uv = Sort[es[[1, {1, 2}]], If[MemberQ[frameNodes, #1], If[MemberQ[frameNodes, #2], nodeIndex[#1] < nodeIndex[#2], False], True] &];
    kind = "plain";
    Switch[mode[[1]],
     "backbone", backbone = mode[[2]];
     If[! mode[[3]] && MemberQ[backbone, uv[[1]]] && MemberQ[backbone, uv[[2]]],
      uv = SortBy[uv, First[First[Position[backbone, #, {1}]]] &];
      kind = Which[
        Abs[First[First[Position[backbone, uv[[1]], {1}]]] - First[First[Position[backbone, uv[[2]], {1}]]]] == 1,
        If[MemberQ[mode[[4]], Sort[uv]], "axisBelow", "axis"],
        MemberQ[skipsBelow, uv], {"skip", First[First[Position[skipsBelow, uv, {1}]]], -1., Replace[uv, skipBase]},
        True, {"skip", First[First[Position[skipsAbove, uv, {1}]]], 1., Replace[uv, skipBase]}]],
     "circle", With[{pa = Select[mode[[2]], Sort[#[[1]]] === Sort[uv] &, 1]},
      If[pa =!= {}, uv = pa[[1, 1]]; kind = {"skip", pa[[1, 2]], pa[[1, 3]]}]],
     "cycle", cycle = mode[[2]];
     If[mode[[4]] && MemberQ[cycle, uv[[1]]] && MemberQ[cycle, uv[[2]]],
      With[{i1 = First[First[Position[cycle, uv[[1]], {1}]]], i2 = First[First[Position[cycle, uv[[2]], {1}]]], L = Length[cycle]},
       Which[
        Mod[i2 - i1, L] == 1, kind = {"cycle", 0.},
        Mod[i1 - i2, L] == 1, uv = Reverse[uv]; kind = {"cycle", 0.}]]]];
    (* four or more diameters of the cycle would meet at its centre: they are bent, all in the same sense *)
    If[kind === "plain" && diameterCount >= 4 && diameterQ[uv],
     If[With[{a = angleOf[point[uv[[2]]] - point[uv[[1]]]]}, a - 2. Pi Floor[a/(2. Pi)]] >= Pi - 10.^-6, uv = Reverse[uv]];
     kind = {"bend", 0.17 vectorNorm[point[uv[[2]]] - point[uv[[1]]]]}];
    pu = point[uv[[1]]]; pv = point[uv[[2]]];
    If[kind === "plain" && MemberQ[outsideChords, Sort[uv]] && Length[es] == 1,
     With[{lc = Select[loopChordCircles, #[[1]] === Sort[uv] &, 1]},
     With[{nrm = rotate90[unitVectorOf[pv - pu]], mid = (pu + pv)/2, rc = If[lc =!= {}, lc[[1, 3]], mode[[5]]],
       cc = If[lc =!= {}, lc[[1, 2]], centre], up = If[lc =!= {}, lc[[1, 4]], {0., 1.}]},
      With[{dm = nrm . (mid - cc)}, With[{side = Which[Abs[dm] > 0.05, Sign[dm], nrm . up < -10.^-6, -1., True, 1.]},
        (* 0.4 outside the circle at its middle, more for a long chord, so that a long arc does not hug the circle *)
        kind = {"bend", side (rc - Abs[dm] + 0.4 + 0.3 Min[1., vectorNorm[pv - pu]/(2 rc)] + If[threeDiameters && lc === {}, 0.2, 0.])}]]]]];
    (* lines between neighbouring vertices of a round fermion loop follow its circle *)
    With[{cs = Select[$loopCircles, consecutiveOnLoopQ[#[[1]], uv[[1]], uv[[2]]] &, 1]},
     If[cs =!= {}, kind = If[Length[cs[[1, 1]]] == 2 && Length[es] == 2, bubbleCircleKind[pu, pv, cs[[1]]],
        circleKind[pu, pv, cs[[1]], Table[point[w], {w, Complement[cs[[1, 1]], uv]}]]]]];
    (* one propagator and interaction lines: the propagator straight, the others bulging away from the centre *)
    If[kind === "plain" && Count[es, {_, _, _, s_} /; ! interactionStyleQ[s]] == 1 && Count[es, {_, _, _, s_} /; interactionStyleQ[s]] >= 1,
     kind = "axis";
     If[rotate90[pv - pu] . ((pu + pv)/2 - centre) < 0, uv = Reverse[uv]; {pu, pv} = {pv, pu}]];
    If[ListQ[kind] && kind[[1]] === "cycle",
     (* the line follows the circle through the cycle vertices, outwards *)
     With[{r = mode[[5]], c = vectorNorm[pv - pu]}, With[{sg = r - Sqrt[Max[r^2 - c^2/4, 0.]]},
       kind = {"cycle", If[vectorNorm[(pu + pv)/2 + sg rotate90[unitVectorOf[pv - pu]] - centre] > vectorNorm[(pu + pv)/2 - centre], sg, -sg]}]]];
    (* the line from a point to a bubble that hangs between two points further apart than its width: bent along the
       circle through the two points and the centre of the bubble *)
    With[{bd = Select[bends, Sort[#[[{1, 2}]]] === Sort[uv] &, 1]},
     If[bd =!= {} && Length[es] == 1,
      uv = bd[[1, {1, 2}]]; pu = point[uv[[1]]]; pv = point[uv[[2]]];
      With[{c = vectorNorm[pv - pu], rr = bd[[1, 4]], cc = bd[[1, 3]]},
       With[{s = rr - Sqrt[Max[0., rr^2 - c^2/4]]},
        kind = {"bend", If[rotate90[unitVectorOf[pv - pu]] . ((pu + pv)/2 - cc) > 0, s, -s]}]]]];
    (* a line that would cross a declared blob is bent around it *)
    If[mode[[1]] === "blob" && kind === "plain" && Length[es] == 1 && es[[1, 4]] =!= "Spoke" &&
      ! (MemberQ[ports, uv[[1]]] && MemberQ[ports, uv[[2]]]),
     kind = blobAvoidingKind[pu, pv, uv, mode[[2]]]];
    (* a line between two ports of one blob (the interaction of a Fock term closed on L), both ends and its middle on
       the blob, would run along its side or across it: bent outwards, away from its centre, 0.35 outside the blob *)
    If[mode[[1]] === "blob" && kind === "plain" && Length[es] == 1 && MemberQ[ports, uv[[1]]] && MemberQ[ports, uv[[2]]],
     With[{own = Select[mode[[2]], insideBlobQ[pu, #, 0.05] && insideBlobQ[pv, #, 0.05] && insideBlobQ[(pu + pv)/2, #, 0.05] &, 1], nrm = rotate90[unitVectorOf[pv - pu]]},
      If[own =!= {}, With[{tc = (own[[1, 1]] - (pu + pv)/2) . nrm, hb = own[[1, 3]]},
        With[{reach = Switch[own[[1, 2]], "Box", hb (Abs[nrm[[1]]] + Abs[nrm[[2]]]), "Triangle" | "TriangleUp", 1.45 hb, "Diamond", 1.3 hb, _, hb]},
         kind = {"bend", If[tc > 10.^-6, -1., 1.] (Max[0., reach - Abs[tc]] + 0.35)}]]]]];
    {es2, offsets} = bundleOffsets[es, vectorNorm[pv - pu], kind];
    (* a bubble next to another bubble, or with other lines leaving its vertices on its sides (the bubble of a triangle):
       made flatter, a lens, so that it overlaps neither *)
    If[(kind === "plain" || MemberQ[{"axis", "axisBelow"}, kind] && ! AnyTrue[es2, interactionStyleQ[#[[4]]] &]) &&
      Length[es2] >= 2 && Max[Abs[offsets]] > 0,
     With[{lim = lensLimit[uv]}, If[lim < Infinity,
       With[{omax = vectorNorm[pv - pu] Tan[Max[lim, 0.25]/2]/2}, If[omax < Max[Abs[offsets]], offsets = offsets omax/Max[Abs[offsets]]]]]]];
    Do[path = arcThrough[pu, pv, offsets[[j]]];
     If[es2[[j, 1]] =!= uv[[1]], path = reversedPath[path]];
     AppendTo[items, {"E", path, es2[[j, 4]]}];
     With[{ea = pathEndAngles[path]}, add[es2[[j, 1]], ea[[1]]]; add[es2[[j, 2]], ea[[2]]]], {j, Length[es2]}],
    {es, groups}];
   (* self-loops and one-label factors, in the largest free sectors that lead outside the loops and bubbles drawn (a
      tadpole on a side of a triangle outside it); a loop that would touch one already drawn (two tadpoles on
      neighbouring vertices of a short leg) goes to another sector nearly as large, if there is one *)
   loopDiscs = {};
   Do[With[{v = l[[1]]},
     p = point[v]; angles = directions[[nodeIndex[v], 2]];
     th = outsideGapDirection[p, angles, preferredDirection[p, centre],
       Flatten[Cases[items, {"E", path_, _} :> Partition[pathPoints[path, N[Range[0, 24]]/24, 0.], 2, 1]], 1], 0.3];
     If[loopDiscs =!= {} && loopClearance[selfLoopPath[p, th, l[[4]], angles], loopDiscs] < 0.2,
      With[{best = Last[SortBy[largeGapDirections[angles, preferredDirection[p, centre], 0.8],
          loopClearance[selfLoopPath[p, #, l[[4]], angles], loopDiscs] &]]},
       If[loopClearance[selfLoopPath[p, best, l[[4]], angles], loopDiscs] > loopClearance[selfLoopPath[p, th, l[[4]], angles], loopDiscs] + 0.05,
        th = best]]];
     AppendTo[items, {"E", selfLoopPath[p, th, l[[4]], angles], l[[4]]}];
     AppendTo[loopDiscs, loopDisc[selfLoopPath[p, th, l[[4]], angles]]];
     directions[[nodeIndex[v], 2]] = Join[angles, {th, th + 0.7, th - 0.7}]], {l, loops}];
   (* a source at the end of a single line (a diagram of Z[J] at a non-zero source) is drawn at the point itself, without
      a stub; any other source insertion on a short stub *)
   sourceEnds = Select[DeleteDuplicates[Cases[Select[stubs, MemberQ[frameNodes, #[[1]]] &], {u_, _} :> u]],
     Count[stubs, {#, _}] == 1 && ! MemberQ[ext, #] && FreeQ[loops, {#, __}] &&
       Count[frameLines, {a_, b_, _, _} /; a =!= b && (a === # || b === #)] == 1 &];
   (* several means on a vertex with lines (the mass insertion lambda phibar^2/2 on a loop) are drawn together, spread
      evenly in the free sector closest to the outward direction (a V on a loop, a cross with a single line), so that none
      is inside a loop; on a vertex without lines they are spread around it *)
   Module[{seen = {}, grouped = {}},
    Do[With[{v = s[[1]], k = Count[stubs, {s[[1]], _}]},
      p = point[v];
      Which[
       MemberQ[sourceEnds, v], AppendTo[items, {"S", {p, p}}],
       MemberQ[grouped, v], Null,
       k >= 2 && ! MemberQ[seen, v] && directions[[nodeIndex[v], 2]] =!= {},
        AppendTo[grouped, v];
        With[{gap = outwardGap[directions[[nodeIndex[v], 2]], preferredDirection[p, centre]]},
         With[{dd = gap[[2]]/(k + 1)},
          Do[th = gap[[1]] + (i - (k + 1)/2) dd;
           AppendTo[items, {"S", {p, p + $stubLength directionAt[th]}}]; add[v, th], {i, k}]]],
       True,
        angles = directions[[nodeIndex[v], 2]];
        (* the largest free sector, unless the stub would end inside a loop or a bubble (the mean on a vertex of a bubble
           on the axis): then the largest sector that leaves it outside *)
        With[{segs = Flatten[Cases[items, {"E", path_, _} :> Partition[pathPoints[path, N[Range[0, 24]]/24, 0.], 2, 1]], 1]},
         th = outsideGapDirection[p, angles, preferredDirection[p, centre], segs, $stubLength]];
        AppendTo[items, {"S", {p, p + $stubLength directionAt[th]}}];
        add[v, th]];
      AppendTo[seen, v]], {s, Select[stubs, MemberQ[frameNodes, #[[1]]] &]}]];
   (* nodes and labels; the labels are kept off the lines *)
   linePointsForLabels = Flatten[Cases[items, {"E", path_, _} :> pathPoints[path, N[Range[0, 24]]/24, 0.]], 1];
   Do[If[Head[v] =!= leg,
     p = point[v];
     Which[
      MemberQ[$densityNodes, v], AppendTo[items, {"S", {p, p}}],
      MemberQ[ext, v] && ! MemberQ[amputatedExt[[All, 2]], v], AppendTo[items, {"N", p, "ext"}],
      Head[v] === blob && declaredBlobHeadQ[blobHeadOf[v, blobs]], AppendTo[items, {"B", p, blobSpecOf[blobHeadOf[v, blobs]]}],
      Head[v] === blob, AppendTo[items, {"N", p, "blob"}],
      (* a port of a declared blob, on its boundary, joined to at most one line: no dot *)
      MemberQ[ports, v] && Count[frameLines, {a_, b_, _, _} /; a =!= b && (a === v || b === v)] <= 1, Null,
      MemberQ[sourceEnds, v], Null,
      True, AppendTo[items, {"N", p, "dot"}]];
     label = Which[
       (* a density point carries its label, also when it is the point of the bare vertex *)
       MemberQ[$densityNodes, v] && vertexLabels =!= None, Replace[v, $densityNodeLabels],
       MemberQ[ext, v] && ! MemberQ[amputatedExt[[All, 2]], v] && vertexLabels =!= None, v,
       ! MemberQ[ext, v] && Head[v] =!= blob && vertexLabels === All, v,
       True, None];
     If[label =!= None,
      (* the preferred free sector whose label box does not overlap the labels already placed *)
      With[{wh = labelBox[label], r0 = Which[MemberQ[$densityNodes, v], 0.1, MemberQ[ext, v], $externalPointRadius, True, $dotRadius] + 0.07 +
          If[MemberQ[Cases[loops, {v0_, __} :> v0], v], 0.12, 0.]},
       With[{ts = gapDirections[directions[[nodeIndex[v], 2]], preferredDirection[p, centre], 0.5]},
        With[{cs = Table[p + (r0 + Abs[Cos[t]] wh[[1]]/2 + Abs[Sin[t]] wh[[2]]/2) directionAt[t], {t, ts}]},
         (* the first candidate that overlaps neither a label nor a line, else one that overlaps no label *)
         With[{k = With[{ok = Select[Range[Length[cs]], Function[j, ! AnyTrue[placed, boxesOverlapQ[{cs[[j]], wh}, #] &]]]},
             With[{clear = Select[ok, Function[j, ! AnyTrue[linePointsForLabels, Abs[#[[1]] - cs[[j, 1]]] < wh[[1]]/2 + 0.07 &&
                     Abs[#[[2]] - cs[[j, 2]]] < wh[[2]]/2 + 0.07 &]], 1]},
              Which[clear =!= {}, First[clear], ok =!= {}, First[ok], True, 1]]]},
          AppendTo[placed, {cs[[k]], wh}];
          (* {"T", centre of the label, label, point the label is attached to, direction in degrees} *)
          AppendTo[items, {"T", cs[[k]], label, p + (r0 + 0.02) directionAt[ts[[k]]], N[ts[[k]]/Degree]}]]]]]]],
    {v, frameNodes}];
   (* labels at the ends of the legs *)
   If[vertexLabels =!= None,
    Do[With[{q = point[l[[2]]], e = point[leg[l[[1]]]]}, With[{wh = labelBox[l[[1]]], u = unitVectorOf[e - q]},
       AppendTo[items, {"T", e + (0.08 + Abs[u[[1]]] wh[[1]]/2 + Abs[u[[2]]] wh[[2]]/2) u, l[[1]], e + 0.09 u, N[ArcTan[u[[1]], u[[2]]]/Degree]}]]],
     {l, legs}]];
   items];

lineCoarsePoints[path_] := With[{k = If[path[[1]] === "segment", 1, Max[4, Ceiling[Abs[path[[5]]]/0.4]]]}, pathPoints[path, N[Range[0, k]]/k, 0.]];
itemPoints[items_] := Join[Flatten[Cases[items, {"E", path_, _} :> lineCoarsePoints[path]], 1], Flatten[Cases[items, {"S", q_} :> q], 1],
   Flatten[Cases[items, {"L", p_, q_, _} :> {p, q}], 1],
   Flatten[Cases[items, {"T", p_, label_, ___} :> With[{b = labelBox[label]/2}, {p - b, p + b}]], 1],
   Flatten[Cases[items, {"N", p_, k_} :> With[{r = If[k === "blob", $blobRadius, $externalPointRadius]}, {p - {r, r}, p + {r, r}}]], 1],
   Flatten[Cases[items, {"B", p_, spec_} :> {p - {spec[[4]], spec[[4]]}, p + {spec[[4]], spec[[4]]}}], 1]];
boundingBox[items_] := With[{pts = itemPoints[items]}, If[pts === {}, {{-0.5, 0.5}, {-0.5, 0.5}},
    {{Min[pts[[All, 1]]], Max[pts[[All, 1]]]}, {Min[pts[[All, 2]]], Max[pts[[All, 2]]]}}]];
shiftItems[items_, v_] := items /. {x_?NumberQ, y_?NumberQ} :> {x + v[[1]], y + v[[2]]};

(* the general layout puts the source points on a circle: one at the end of a single line that is nearly vertical or
   nearly horizontal is moved along the circle's tangent so that the line is exactly vertical or horizontal *)
straightenedSourceLines[positions_, sources_, pairs_] := Module[{pos = positions, rules, nb},
   Do[rules = Rule @@@ pos;
    nb = DeleteDuplicates[Join[Cases[pairs, {s, w_} :> w], Cases[pairs, {w_, s} :> w]]];
    If[Length[nb] == 1 && ! MemberQ[sources, nb[[1]]], With[{ps = Replace[s, rules], pv = Replace[nb[[1]], rules]},
      With[{dd = ps - pv}, Which[
        Abs[dd[[1]]] > 10.^-6 && Abs[dd[[1]]] < 0.27 Abs[dd[[2]]], pos = Replace[pos, {s, _} :> {s, {pv[[1]], ps[[2]]}}, {1}],
        Abs[dd[[2]]] > 10.^-6 && Abs[dd[[2]]] < 0.27 Abs[dd[[1]]], pos = Replace[pos, {s, _} :> {s, {ps[[1]], pv[[2]]}}, {1}]]]]],
    {s, sources}];
   pos];

(* geometry of a canonical term, with its diagramElements: {weight, items, bounding box} *)
diagramGeometry[t_diagramTerm, elements_List, ext0_List, opts_List, legDirections_] := Block[
   {$pictureBlobHeads = Union[$sumBlobHeads, Cases[elements[[3]], {_, _, h_} :> h]], $pictureBoxFloor = $sumBoxFloor},
   $pictureBoxFloor = Max[$sumBoxFloor, boxFloorOf[elements]];
   diagramGeometryOfPicture[t, elements, ext0, opts, legDirections]];
diagramGeometryOfPicture[t_diagramTerm, elements_List, ext0_List, opts_List, legDirections_] := Module[{vertexLabels = drawingOption[opts, "VertexLabels"], weight,
    lines, blobs, stubs, classes, rules, d = t[[3]], ext, present, externalNodes, amputated, amputatedExt, dummies, nodes, pairs, propagatorPairs,
    components, mainComponents, vacuumComponents, frames = {}, positions, mode, items, bb, x0, groups, circles, sourceLayout, unitQ, straighten, multiplePairs,
    axisFrame},
   vertexLabels = Which[vertexLabels === None || vertexLabels === False, None, vertexLabels === All || vertexLabels === True, All, True, Automatic];
   {weight, lines, blobs, stubs, classes, rules} = elements;
   {lines, blobs} = attachLinesToBlobs[lines, blobs, stubs, d];
   (* the points that appear in the picture (lines, blobs, source insertions, or a point made of several labels) *)
   present = Union[Flatten[{Cases[lines, {u_, v_, _, _} :> {u, v}], Cases[blobs, {b_, l_, _} :> Join[{b}, l]], Cases[stubs, {u_, _} :> u],
       Cases[classes, cl_List /; Length[cl] > 1 :> (cl /. rules)]}]];
   ext = Select[ext0, MemberQ[present, # /. rules] &];
   externalNodes = (# /. rules) & /@ ext;
   amputated = Table[amputatedLabelQ[e, lines, blobs, stubs, classes, opts] && ! MemberQ[$densityLabels, e], {e, ext}];
   amputatedExt = Join @@ Table[If[amputated[[k]], {{ext[[k]], externalNodes[[k]]}}, {}], {k, Length[ext]}];
   dummies = Select[DeleteDuplicates[(# /. rules) & /@ d], MemberQ[present, #] && ! MemberQ[externalNodes, #] &];
   nodes = DeleteDuplicates[Join[externalNodes, dummies, Cases[blobs, {b_, _, _} :> b]]];
   propagatorPairs = DeleteDuplicates[Join[Cases[lines, {u_, v_, _, s_} /; u =!= v && ! interactionStyleQ[s] :> Sort[{u, v}]],
      Flatten[Table[Sort[{b[[1]], x}], {b, blobs}, {x, b[[2]]}], 1]]];
   pairs = DeleteDuplicates[Join[propagatorPairs, Cases[lines, {u_, v_, _, _} /; u =!= v :> Sort[{u, v}]]]];
   With[{all = Cases[lines, {u_, v_, _, _} /; u =!= v :> Sort[{u, v}]]}, multiplePairs = Select[DeleteDuplicates[all], Count[all, #] >= 2 &]];
   components = {};
   Do[If[FreeQ[components, v], AppendTo[components, componentOf[v, pairs]]], {v, nodes}];
   components = Table[Select[nodes, MemberQ[c, #] &], {c, components}];
   mainComponents = Select[components, Intersection[#, externalNodes] =!= {} &];
   vacuumComponents = Select[components, Intersection[#, externalNodes] === {} &];
   If[ext =!= {},
    (* the part with the external labels is one picture; a product of two one-point functions (e.g. <phi(x)> <phi(y)>)
       is drawn as two pictures side by side *)
    groups = If[Length[ext] == 2 && Length[mainComponents] == 2,
      Table[{{k}, First[Select[mainComponents, MemberQ[#, externalNodes[[k]]] &]]}, {k, 2}],
      {{Range[Length[ext]], Flatten[mainComponents]}}];
    Do[With[{ks = g[[1]], mainNodes = g[[2]]},
      With[{unitLayout = Replace[blobLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines, blobs],
          None :> Replace[unitLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines,
           drawingOption[opts, "ExternalOrder"]],
          None :> treeLayoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], mainNodes, lines, blobs]]]},
       If[unitLayout =!= None, {positions, mode, circles} = unitLayout,
        {positions, mode} = Block[{$multiplePairs = multiplePairs, $selfLoopNodes = Cases[lines, {u_, u_, _, _} :> u]}, layoutWithExternals[ext[[ks]], externalNodes[[ks]], amputated[[ks]], Select[mainNodes, ! MemberQ[externalNodes, #] &],
          Select[pairs, MemberQ[mainNodes, #[[1]]] &], Select[propagatorPairs, MemberQ[mainNodes, #[[1]]] &], drawingOption[opts, "ExternalOrder"]]];
        {positions, circles} = roundFermionLoops[positions, lines, mode, externalNodes[[ks]], Select[pairs, MemberQ[mainNodes, #[[1]]] &]]]];
      AppendTo[frames, {Block[{$loopCircles = circles, $densityNodes = (# /. rules) & /@ Select[ext[[ks]], MemberQ[$densityLabels, #] &],
         $densityNodeLabels = Table[(e /. rules) -> e, {e, Select[ext[[ks]], MemberQ[$densityLabels, #] &]}]},
         frameItems[Join[Select[nodes, MemberQ[mainNodes, #] &], leg /@ Select[amputatedExt, MemberQ[ext[[ks]], #[[1]]] &][[All, 1]]],
          positions, lines, blobs, stubs, externalNodes, amputatedExt, legDirections, d, mode, vertexLabels]], True}]], {g, groups}]];
   Do[unitQ = False; straighten = {}; axisFrame = False; With[{unitLayout = unitLayoutVacuum[c, lines],
      (* the source insertions at the ends of single lines (the diagrams of Z[J] at a non-zero source): placed like the
         external points of a correlation function, on the axis or at the corners *)
      sourcePoints = Select[DeleteDuplicates[Cases[stubs, {u_, _} :> u]], MemberQ[c, #] && Count[Flatten[pairs], #] == 1 &&
          Count[stubs, {#, _}] == 1 &],
      (* the means on vertices (Gamma2[Psibar, Psi, G]): a fermion line between two means drawn like the open line of a
         self-energy, the means on its axis *)
      meanVertices = Select[DeleteDuplicates[Cases[stubs, {u_, _} :> u]], MemberQ[c, #] && Count[Flatten[pairs], #] > 1 &&
          Count[stubs, {#, _}] == 1 &]},
     Which[
      sourcePoints === {} && 2 <= Length[meanVertices] <= 8 &&
       (sourceLayout = meanUnitLayout[meanVertices, c, lines]) =!= None,
      {positions, mode, circles} = sourceLayout; unitQ = True; axisFrame = True,
      sourcePoints =!= {} && Length[sourcePoints] <= 8 &&
       (sourceLayout = unitLayoutWithExternals[sourcePoints, sourcePoints, Table[False, {Length[sourcePoints]}], c, lines, Automatic]) =!= None,
      {positions, mode, circles} = sourceLayout; unitQ = True; axisFrame = True,
      sourcePoints =!= {} && Length[sourcePoints] <= 8,
      {positions, mode} = layoutWithExternals[sourcePoints, sourcePoints, Table[False, {Length[sourcePoints]}],
        Select[c, ! MemberQ[sourcePoints, #] &], Select[pairs, MemberQ[c, #[[1]]] &], Select[propagatorPairs, MemberQ[c, #[[1]]] &],
        Automatic];
      {positions, circles} = roundFermionLoops[positions, lines, mode, sourcePoints, Select[pairs, MemberQ[c, #[[1]]] &]]; straighten = sourcePoints,
      unitLayout =!= None, {positions, mode, circles} = unitLayout; unitQ = True,
      True,
      {positions, mode} = Block[{$blobRadii = Table[b[[1]] -> If[blobSpecOf[b[[3]]] =!= None, blobSizeOfHead[b[[3]]], $blobRadius], {b, blobs}]},
        layoutVacuumComponent[c, Select[pairs, MemberQ[c, #[[1]]] &], Select[propagatorPairs, MemberQ[c, #[[1]]] &], lines]];
      {positions, circles} = If[mode[[1]] === "loops", {positions, mode[[2]]},
        roundFermionLoops[positions, lines, mode, {}, Select[pairs, MemberQ[c, #[[1]]] &]]]]];
    (* the unit layouts are upright by construction (open lines horizontal, rails parallel): only the general layout is turned *)
    If[! unitQ, {positions, circles} = alignPrincipalAxes[positions, circles, Select[lines, MemberQ[c, #[[1]]] && MemberQ[c, #[[2]]] &]]];
    If[straighten =!= {}, positions = straightenedSourceLines[positions, straighten, Select[pairs, MemberQ[c, #[[1]]] &]]];
    AppendTo[frames, {Block[{$loopCircles = circles}, frameItems[c, positions, lines, blobs, stubs, externalNodes, {}, {}, d, mode, vertexLabels]],
      axisFrame}], {c, vacuumComponents}];
   (* frames side by side: the parts with external labels, and the open lines between sources or means, keep their axis
      (y = 0), the other parts are centred on it, the first one too, so that equal pieces are drawn at the same height *)
   items = {}; x0 = None;
   Do[bb = boundingBox[f[[1]]];
    If[x0 === None,
     items = shiftItems[f[[1]], {0., If[f[[2]] || Length[frames] == 1, 0., -Mean[bb[[2]]]]}]; x0 = bb[[1, 2]],
     items = Join[items, shiftItems[f[[1]], {x0 + 0.5 - bb[[1, 1]], If[f[[2]], 0., -Mean[bb[[2]]]]}]]; x0 = x0 + 0.5 + bb[[1, 2]] - bb[[1, 1]]],
    {f, frames}];
   {weight, items, boundingBox[items]}];

(* the unit layout of a fermion line or of two rails between means (Gamma2[Psibar, Psi, G]); with four means, of the
   orders of the means at the corners the first one whose straight lines cross least (the corners are free here: the
   two rails of a ring of two Fock vertices antiparallel, a box, not an X of two wavy lines) *)
meanUnitLayout[means_, c_, lines_] := Module[{best = None, bestCount = Infinity, res, count, frameLines},
   If[Length[means] =!= 4, Return[unitLayoutWithExternals[means, means, Table[True, {Length[means]}], c, lines, Automatic]]];
   frameLines = Select[lines, MemberQ[c, #[[1]]] && MemberQ[c, #[[2]]] && #[[1]] =!= #[[2]] &];
   Do[res = unitLayoutWithExternals[o, o, Table[True, {4}], c, lines, o];
    If[res =!= None,
     count = With[{pt = Rule @@@ res[[1]]}, mutualCrossingCount[Table[{l[[1]] /. pt, l[[2]] /. pt}, {l, frameLines}]]];
     If[count < bestCount, best = res; bestCount = count]];
    If[bestCount == 0, Break[]], {o, Prepend[DeleteCases[Permutations[means], means], means]}];
   best];

(* a picture without external points turned so that the principal axes of its vertices are horizontal and vertical
   (by less than 45 degrees): a symmetric diagram laid out at an angle is drawn upright; nothing is turned when the
   vertices have no preferred axis *)
alignPrincipalAxes[positions_, circles_, lines_] := Module[{pts = Cases[positions, {v_, q_} /; Head[v] =!= leg :> q], m, dd, cxx, cyy, cxy,
    phi, rot, point, m0},
   If[Length[pts] < 3, Return[{positions, circles}]];
   (* the vertices, the middle of every line, and a point outside every self-loop *)
   point[v_] := Replace[v, Rule @@@ positions]; m0 = Mean[pts];
   pts = Join[pts, Table[If[l[[1]] === l[[2]], With[{q = point[l[[1]]]}, q + 0.5 If[vectorNorm[q - m0] < 0.05, {0., 1.}, unitVectorOf[q - m0]]],
       (point[l[[1]]] + point[l[[2]]])/2], {l, lines}]];
   m = Mean[pts]; dd = Table[q - m, {q, pts}];
   cxx = Total[dd[[All, 1]]^2]; cyy = Total[dd[[All, 2]]^2]; cxy = Total[dd[[All, 1]] dd[[All, 2]]];
   If[Sqrt[(cxx - cyy)^2 + 4 cxy^2] < 0.15 (cxx + cyy) + 10.^-9, Return[{positions, circles}]];
   phi = -ArcTan[cxx - cyy, 2 cxy]/2; phi = phi - Pi/2 Round[phi/(Pi/2)];
   If[Abs[phi] < 0.05, Return[{positions, circles}]];
   rot[q_] := m + {Cos[phi] (q - m)[[1]] - Sin[phi] (q - m)[[2]], Sin[phi] (q - m)[[1]] + Cos[phi] (q - m)[[2]]};
   {Table[{q[[1]], rot[q[[2]]]}, {q, positions}], Table[{q[[1]], rot[q[[2]]], q[[3]]}, {q, circles}]}];

(* ---------------------------------------------------------------- graphics primitives *)
labelPrimitive[label_, p_] := Text[Style[If[Head[label] === DummyVariable, Subscript["z", label[[2]]], label], $fontPixels, Italic], p];

(* parameter of the arrow on a directed line: the middle, unless another arrow is already close to it or another line
   crosses it there *)
arrowParameter[path_, used_, others_ : {}, blobs_ : {}] := Module[{freeQ, c, boxes},
   (* a line whose bounding box is farther than 0.15 from the point, in x or in y, is farther at every segment *)
   boxes = Table[{Min[o[[All, 1]]], Max[o[[All, 1]]], Min[o[[All, 2]]], Max[o[[All, 2]]]}, {o, others}];
   freeQ[t_] := With[{q = First[pathPoints[path, {t}, 0.]]},
     AllTrue[used, vectorNorm[# - q] > 0.25 &] && ! AnyTrue[blobs, insideBlobQ[q, #, 0.12] &] &&
      AllTrue[Range[Length[others]], Function[i, With[{b = boxes[[i]], o = others[[i]]},
        Max[b[[1]] - q[[1]], q[[1]] - b[[2]], b[[3]] - q[[2]], q[[2]] - b[[4]]] > 0.15 + 10.^-9 ||
         Min[Table[segmentDistance[q, {o[[k]], o[[k + 1]]}], {k, Length[o] - 1}]] > 0.15]]]];
   c = Select[{0.5, 0.3, 0.7, 0.4, 0.6}, freeQ, 1];
   If[c === {}, 0.5, First[c]]];
(* blobs: {centre, shape, half size} of the declared blobs, which hide what is under them *)
arrowParameters[items_, blobs_ : {}] := Module[{used = {}, ts = {}, t, path, coarse = Table[lineCoarsePoints[it[[2]]], {it, items}]},
   Do[If[directedStyleQ[items[[i, 3]]], path = items[[i, 2]];
     t = arrowParameter[path, used, Delete[coarse, i], blobs]; AppendTo[used, First[pathPoints[path, {t}, 0.]]];
     AppendTo[ts, t], AppendTo[ts, 0.5]], {i, Length[items]}];
   ts];

(* the half size of the cross on an impurity line *)
$impurityCross = 0.13;
crossAt[q_, size_] := {Line[{q + size {1, 1}, q - size {1, 1}}], Line[{q + size {1, -1}, q - size {1, -1}}]};

linePrimitives[{"E", path_, style_}, t_ : 0.5] := Switch[style,
   "Wavy", Line[pathWavy[path]],
   (* a dressed interaction (the screened W): two parallel waves *)
   "DoubleWavy", {Line[pathWavy[path, 0.03]], Line[pathWavy[path, -0.03]]},
   "Curly", Line[pathCurly[path]],
   "DottedArrow", {AbsolutePointSize[2.2], Point[pathDots[path]], Polygon[pathArrowHead[path, t]]},
   "Dashed", Line[pathDashes[path]],
   "Dotted", {AbsolutePointSize[2.2], Point[pathDots[path]]},
   "Zigzag", Line[pathZigzag[path]],
   "Impurity", {Line[pathDashes[path, 0.13]], crossAt[First[pathPoints[path, {0.5}, 0.]], $impurityCross]},
   "Arrow", {Line[pathPolyline[path]], Polygon[pathArrowHead[path, t]]},
   "DoubleArrow", {Line[pathPolyline[path, 0.035]], Line[pathPolyline[path, -0.035]], Polygon[pathArrowHead[path, t]]},
   "Double", {Line[pathPolyline[path, 0.035]], Line[pathPolyline[path, -0.035]]},
   "DivergingArrows" | "ConvergingArrows", {Line[pathPolyline[path]], Polygon[pathArrowHead[#[[1]], #[[2]]]] & /@ anomalousArrowHeads[path, style]},
   _, Line[pathPolyline[path]]];
(* the two arrowheads of an anomalous line, as {path, parameter}: pointing to the two ends at 0.3 and 0.7 (diverging), or
   to the middle at 0.25 and 0.75 (converging), the tips kept apart *)
anomalousArrowHeads[path_, "DivergingArrows"] := {{path, 0.7}, {reversedPath[path], 0.7}};
anomalousArrowHeads[path_, "ConvergingArrows"] := {{path, 0.25}, {reversedPath[path], 0.25}};

legPrimitives[{"L", q_, e_, direction_}] := With[{path = If[direction === "In", {"segment", e, q}, {"segment", q, e}]},
   If[direction === "In" || direction === "Out", {Line[{q, e}], Polygon[pathArrowHead[path, 0.55]]}, Line[{q, e}]]];

itemPrimitives[items_, opts_List] := {AbsoluteThickness[With[{w = drawingOption[opts, "LineWidth"]}, If[NumberQ[w] && w > 0, w, 1.3]]], Black,
   With[{es = Cases[items, {"E", __}]}, MapThread[linePrimitives[#1, #2] &, {es, arrowParameters[es, Cases[items, {"B", p_, spec_} :> {p, spec[[2]], spec[[4]]}]]}]],
   legPrimitives /@ Cases[items, {"L", __}],
   Cases[items, {"S", {p_, q_}} :> {Line[{p, q}], {EdgeForm[{Black, AbsoluteThickness[1.]}], White, Disk[q, 0.1]},
      AbsoluteThickness[1.], crossAt[q, 0.07]}],
   Cases[items, {"N", p_, "blob"} :> {EdgeForm[{Black, AbsoluteThickness[1.]}], GrayLevel[0.75], Disk[p, $blobRadius]}],
   Cases[items, {"B", p_, spec_} :> declaredBlobPrimitives[p, spec]],
   Cases[items, {"N", p_, "dot"} :> Disk[p, $dotRadius]],
   Cases[items, {"N", p_, "ext"} :> {EdgeForm[{Black, AbsoluteThickness[1.]}], White, Disk[p, $externalPointRadius]}],
   Cases[items, {"T", p_, label_, ___} :> labelPrimitive[label, p]]};

weightPrimitive[w_, p_] := Text[Style[w, $fontPixels + 1], p, {1, 0}];

finishGraphics[primitives_, bb_, opts_List] := Module[{x1 = bb[[1, 1]] - 0.12, x2 = bb[[1, 2]] + 0.12, y1 = bb[[2, 1]] - 0.12,
    y2 = bb[[2, 2]] + 0.12, imageSize = drawingOption[opts, "ImageSize"], pixelsPerUnit},
   pixelsPerUnit = If[NumberQ[imageSize], N[imageSize/(x2 - x1)], $pixelsPerUnit];
   Graphics[primitives, PlotRange -> {{x1, x2}, {y1, y2}}, ImageSize -> Round[pixelsPerUnit (x2 - x1)],
    AspectRatio -> Automatic, PlotRangePadding -> None]];

(* the canonical terms in drawing order: fewer integration variables first *)
termsForDrawing[e_] := With[{ts = canonicalTermsOf[e]}, ts[[Ordering[Length[#[[3]]] & /@ ts]]]];

(* the weight written with a picture: the coefficient of the term divided by the values of its elements, in which an
   interaction line (DeclareInteractionLine, or the option "Interactions") stands for minus its kernel, a local vertex
   for minus its kernel and a vertex for minus its coupling (DeclareCoupling, or the option "Couplings": lambda^n ->
   (-1)^n), the signs of the
   expansion of exp(-S_int); "Weights" -> "Coefficients" gives the coefficient itself *)
interactionLineHeadQ[h_, opts_List] := MemberQ[$interactionLineHeads, h] || MemberQ[Flatten[{drawingOption[opts, "Interactions"]}], h];
displayedWeight[t_diagramTerm, elements_List, opts_List] := If[drawingOption[opts, "Weights"] === "Coefficients", elements[[1]],
   (-1)^(Count[elements[[2]], {_, _, h_, _} /; interactionLineHeadQ[h, opts]] +
       Count[expandIntegerPowers[Join[factorList[t[[1]]], t[[2]]]], f_ /; localVertexQ[f]]) (elements[[1]] /.
      Table[c -> -1, {c, Union[$couplingSymbols, Flatten[{drawingOption[opts, "Couplings"]}]]}])];

(* an anomalous line (-<T psi psi>, -<T psibar psibar>) or an antisymmetric blob of two labels (the gap function) shows
   no orientation, so the weight takes it read along the fermion line it lies on: the chains of fermion lines (directed
   or anomalous) and of blobs of two labels, a loop in either sense (it holds an even number of such elements, so the
   sense does not matter), an open line from its end at the first external label (else from its first end; the sense
   matters only when both ends are of one kind, psi psi or psibar psibar). The canonical term orients them by the order
   of its labels: the sign converts one reading into the other. Chains that branch are left as they are. *)
anomalousOrientationSign[elements_List, ext_List] := Module[{lines = elements[[2]], blobs = elements[[3]], rules = elements[[6]],
    edges, extNodes, sign = 1, done = {}, compNodes, compEdges, degree, ends, start, v, prev, next, k, grow},
   edges = Join[
     Cases[lines, {u_, w_, h_, st_} /; u =!= w && fermionStyleQ[st] :> {u, w, anomalousStyleQ[st] && MemberQ[$antisymmetricHeads, h]}],
     Cases[blobs, {_, {u_, w_}, h_} /; u =!= w :> {u, w, MemberQ[$antisymmetricHeads, h]}]];
   If[FreeQ[edges, {_, _, True}], 1,
    extNodes = (# /. rules) & /@ ext;
    degree[x_] := Count[Flatten[edges[[All, {1, 2}]]], x];
    Do[If[edges[[i, 3]] && ! MemberQ[done, i],
      (* the component of the edge i *)
      compNodes = {edges[[i, 1]], edges[[i, 2]]}; grow = True;
      While[grow, With[{more = Union[Flatten[Select[edges[[All, {1, 2}]], Intersection[#, compNodes] =!= {} &]]]},
        grow = Length[more] > Length[compNodes]; compNodes = more]];
      compEdges = Select[Range[Length[edges]], MemberQ[compNodes, edges[[#, 1]]] &];
      done = Join[done, compEdges];
      If[AllTrue[compNodes, degree[#] <= 2 &],
       ends = Select[compNodes, degree[#] == 1 &];
       start = Which[ends === {}, edges[[First[compEdges], 1]],
         Intersection[ends, extNodes] =!= {}, First[Select[extNodes, MemberQ[ends, #] &]],
         True, First[Select[Flatten[edges[[compEdges, {1, 2}]]], MemberQ[ends, #] &]]];
       (* the walk from start: at every node the edge not yet taken *)
       v = start; prev = {};
       While[(next = Select[compEdges, ! MemberQ[prev, #] && MemberQ[edges[[#, {1, 2}]], v] &]) =!= {},
        k = First[next]; AppendTo[prev, k];
        If[edges[[k, 3]] && edges[[k, 1]] =!= v, sign = -sign];
        v = If[edges[[k, 1]] === v, edges[[k, 2]], edges[[k, 1]]]]]], {i, Length[edges]}];
    sign]];

(* a kernel OperatorKernel[h^-1] in a term that also holds lines h (a derivative of a Tr ln or of a 1PI action before
   ExpandTrace): it is drawn as a plain line, like h, and the products h . h^-1, not contracted, look like chains of h *)
FunctionalDiagrams::inversekernel = "OperatorKernel[`1`] is drawn as a line like `2`, and its products with the lines `2` are not contracted: the picture is misleading. Contract them with ExpandTrace first.";
inverseKernelHeads[t_] := Select[Union[Cases[t, OperatorKernel[Power[h_Symbol, -1]] :> h, Infinity, Heads -> True]],
   ! FreeQ[t, #[_, _]] &];
geometriesOf[e_, ext_List, opts_List] := Module[{ts = termsForDrawing[e], elements, directions},
   checkLabels[e, ext];
   Scan[Message[FunctionalDiagrams::inversekernel, ToString[#] <> "^-1", #] &, Union @@ Append[inverseKernelHeads /@ ts, {}]];
   (* the elements of every term, computed once for the three steps that need them *)
   elements = Table[diagramElements[t, ext, opts], {t, ts}];
   directions = legDirectionsFor[elements, ext, opts];
   Block[{$densityLabels = densityLabelsFor[ts, elements, ext, opts, directions],
     $sumBlobHeads = Union @@ Append[Table[Cases[el[[3]], {_, _, h_} :> h], {el, elements}], {}], $sumBoxFloor = 0.},
    $sumBoxFloor = Block[{$pictureBlobHeads = $sumBlobHeads}, Max[0., boxFloorOf /@ elements]];
    Table[ReplacePart[diagramGeometry[ts[[i]], elements[[i]], ext, opts, directions], 1 ->
       anomalousOrientationSign[elements[[i]], ext] displayedWeight[ts[[i]], elements[[i]], opts]], {i, Length[ts]}]]];

DrawDiagram[t_, ext_List : {}, opts___] := Module[{ts = termsForDrawing[t], g, allOpts = drawingOptions[DrawDiagram, {opts}]},
   Which[
    ts === {}, Graphics[{}],
    Length[ts] > 1, DiagramPlot[t, ext, Sequence @@ allOpts],
    True,
    g = First[geometriesOf[t, ext, allOpts]];
    finishGraphics[itemPrimitives[g[[2]], allOpts], g[[3]], allOpts]]];

DiagramPlot[e_, ext_List : {}, opts___] := Module[{allOpts = drawingOptions[DiagramPlot, {opts}], gs, n, ww, dw, dh, cw, ch, columns,
    cells, bb},
   gs = geometriesOf[e, ext, allOpts]; n = Length[gs];
   Which[
    n == 0, Graphics[{}],
    drawingOption[allOpts, "Output"] === "Grid",
    Grid[Table[{g[[1]], finishGraphics[itemPrimitives[g[[2]], allOpts], g[[3]], allOpts]}, {g, gs}], Frame -> All,
     Alignment -> {Left, Center}],
    True,
    ww = If[TrueQ[drawingOption[allOpts, "Weights"]] || drawingOption[allOpts, "Weights"] === Automatic,
      Max[0.62 $fontPixels StringLength[ToString[#[[1]], InputForm]]/$pixelsPerUnit & /@ gs] + 0.3, 0.];
    dw = Max[(#[[3, 1, 2]] - #[[3, 1, 1]]) & /@ gs]; dh = Max[(#[[3, 2, 2]] - #[[3, 2, 1]]) & /@ gs];
    cw = ww + dw + 0.4; ch = dh + 0.4;
    columns = drawingOption[allOpts, "Columns"];
    (* about 16 units wide, or more for many pictures, so that the grid is not much taller than wide *)
    If[! (IntegerQ[columns] && columns >= 1), columns = Max[1, Min[n, Max[Floor[16./cw], Round[Sqrt[1.5 n ch/cw]]]]]];
    cells = Table[With[{r = Quotient[i - 1, columns], c = Mod[i - 1, columns], g = gs[[i]]},
       With[{x0 = c cw, y0 = -r ch},
        {shiftItems[g[[2]], {x0 + ww + 0.2 + dw/2 - Mean[g[[3, 1]]], y0 - ch/2 - Mean[g[[3, 2]]]}], g[[1]], {x0 + ww, y0 - ch/2},
         {{x0, y0}, {x0 + cw, y0}, {x0 + cw, y0 - ch}, {x0, y0 - ch}, {x0, y0}}}]], {i, n}];
    bb = {{0., Min[n, columns] cw}, {-Ceiling[n/columns] ch, 0.}};
    finishGraphics[{
      {GrayLevel[0.85], AbsoluteThickness[0.6], Line[#[[4]]] & /@ cells},
      If[ww > 0, weightPrimitive[#[[2]], #[[3]]] & /@ cells, {}],
      itemPrimitives[#[[1]], allOpts] & /@ cells}, bb, allOpts]]];
