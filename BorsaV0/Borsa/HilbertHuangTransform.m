(* Wolfram Language Package *)

BeginPackage["HilbertHuangTransform`"]
(* Exported symbols added here with SymbolName::usage *)  
emd::usage = "emd[ts] computes the Empirical Mode Decomposition of ts."

Begin["`Private`"] (* Begin Private Context *) 

isMonotonic[x_List] :=
    Length[findPeaks[x]] Length[findPeaks[-x]] == 0;

isIMF[x_List] :=
    Module[ {u1, u2},
        u1 = Count[Most[x] Rest[x], _?Negative];
        u2 = Length[findPeaks[x]] + Length[findPeaks[-x]];
        Abs[u1 - u2] <= 1
    ]

Off[Interpolation::inhr];

getSpline[x_List] :=
    Module[ {n, p},
        n = Length[x];
        p = findPeaks[x];
        Interpolation[
          Transpose[{Flatten[{1, p, n }], Flatten[{x[[1]], x[[p]],x[[n]]}]}],Method->"Spline"
          ][Range[n]]
    ]

findPeaks[x_List] :=
    Module[ {n, u},
        n = Flatten@
          Position[
           Differences[Boole[# > 0] & /@ Differences[x]], _?(# < 0 &)];
        u = Flatten@Position[x[[n + 1]] - x[[n]], _?(# > 0 &)];
        n[[u]] + 1
    ]
    
emd[x_List] :=
    Module[ {xt = x, imf = {}, x1, x2, s1, s2, sd},
        While[! isMonotonic[xt], x1 = xt;
                                 sd = Infinity;
                                 While[(sd > 0.1) || ! isIMF[x1], s1 = getSpline[x1];
                                                                  s2 = -getSpline[-x1];
                                                                  x2 = x1 - (s1 + s2)/2;
                                                                  sd = Total[(x1 - x2)^2]/Total[x1^2];
                                                                  x1 = x2;];
                                 AppendTo[imf, x1];
                                 xt = xt - x1;];
        Append[imf, xt]
    ]

End[] (* End Private Context *)

EndPackage[]