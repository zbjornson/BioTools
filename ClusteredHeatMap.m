Options[ClusteredHeatMap] = {
  ColorFunction -> Automatic, 
  "LeftDendrogramRatio" -> 0.4, "TopDendrogramRatio" -> 0.4, 
  "BottomLabels" -> None, "RightLabels" -> None}~Join~Options[Graphics];

ClusteredHeatMap[data_, hClustering_, vClustering_, opts : OptionsPattern[]] :=
Module[{
    hClustOrder = ClusterFlatten[hClustering],
    vClustOrder = ClusterFlatten[vClustering],
    orderedData,
    left = DendrogramPlot[vClustering, Orientation -> Left],
    leftXcoords,
    leftDomain,
    top = DendrogramPlot[hClustering],
    topYcoords,
    topDomain,
    xLength = Length@data,
    yLength = Length@data[[1]],
    $defaultColorFunction,
    $colorFunction,
    $leftDendrogramRatio = OptionValue["LeftDendrogramRatio"],
    $topDendrogramRatio = OptionValue["TopDendrogramRatio"],
    rightLabels = OptionValue["RightLabels"],
    bottomLabels = OptionValue["BottomLabels"],
    $xTicks, $xTickLocations, $xTickLabels, $yTicks
  },
  
  orderedData = data[[hClustOrder, vClustOrder]];
  
  $defaultColorFunction[x_] := Blend[
    {
      {-1, Blue},
      {0, Black},
      {1, Yellow}
    }, x];
  
  $colorFunction = OptionValue[ColorFunction] /. Automatic -> $defaultColorFunction;
  
  leftXcoords = Cases[left, Line[{x : {_, _} ..}] :> x, Infinity][[All, 1]];
  leftDomain = {Min[leftXcoords], Max[leftXcoords]};
  topYcoords = Cases[top, Line[{x : {_, _} ..}] :> x, Infinity][[All, 2]];
  topDomain = {Min[topYcoords], Max[topYcoords]};
  
  (* X ticks are normally drawn below the axis, which interferes with the dendrogram. Draw manually. *)
  $xTickLocations = 
    Table[{i, 
      Rescale[i, leftDomain, {-$leftDendrogramRatio*xLength, 0}]}, {i, 
      Select[FindDivisions[leftDomain, 3], leftDomain[[1]] < # < 0 &]}];
  $xTicks = Table[{i[[2]], ""}, {i, $xTickLocations}];
  $xTickLabels = 
    Table[{GrayLevel[0.4], 
      Text[i[[1]], Offset[{0, 10}, {i[[2]], 0}]]}, {i, 
      If[OptionValue[Axes] == True, $xTickLocations, {}]}];

  $yTicks = 
    Table[{Rescale[i, topDomain, {0, $topDendrogramRatio*yLength}], 
      i}, {i, FindDivisions[topDomain, 5]}];
  
  Show[
    (* Dendrograms *)
    (* Shift the left dendrogram to go from {0, 0} to {0, negativeValue}. *)
    (* Rescale both dendrograms to be half the width (left) or height (top) of the heatmap. *)
    left /. (Line[x : {{_, _} ..}] :> 
      Line[({Rescale[#[[1]], leftDomain, {-$leftDendrogramRatio*xLength, 0}], #[[2]] - yLength - 0.5}) & /@ x]),
    top /. (Line[x : {{_, _} ..}] :>
      Line[({#[[1]] - 0.5, Rescale[#[[2]], topDomain, {0, $topDendrogramRatio*yLength}]}) & /@ x]),
    (* Heatmap *)
    Graphics[
      Table[{$colorFunction[orderedData[[x, y]]], 
        Rectangle[{x - 1, y - yLength}, {x, y - yLength - 1}]}, {x, xLength}, {y, 1, yLength}]],
    (* Bottom labels *)
    Graphics[
      Table[Text[Rotate[bottomLabels[[hClustOrder[[i]]]], 90 Degree], 
        Offset[{0, -5}, {i - 0.5, -yLength}], {0, 1}], {i, 1, 
        Length@bottomLabels, 4(*, optional skipping increment*)}]],
    (* Right labels *)
    Graphics[
      Table[Text[rightLabels[[vClustOrder[[i]]]], 
        Offset[{5, 0}, {xLength, i - yLength - 0.5}], {-1, 0}], {i, 1, 
        Length@rightLabels(*, optional skipping increment*)}]],
    (* X tick labels *)
    Graphics[$xTickLabels],
    Ticks -> {$xTicks, $yTicks}, PlotRangePadding -> 0,
    FilterRules[{opts}, Options[Graphics]]
  ]
]
