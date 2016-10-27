(* Mathematica Package *)

(* Created by the Wolfram Workbench 05/09/2014 *)

BeginPackage["Borsa`",{"StockData`","BackTest`","EmbeddingAnalysis`","Strategy`"}]
 

(* ::Section:: *)
(* Financial Data (FD) prices from Yahoo *)
FDOpen::usage = "FDOpen[FD] Returns the temporal series of open prices. Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}. Output: Open prices, {{date,open},...}"
FDClose::usage = "FDClose[FD] Returns the temporal series of close prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Close prices, {{date,close},...}"
FDHigh::usage = "FDHigh[FD] Returns the temporal series of high prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: High prices, {{date,high},...}"
FDLow::usage = "FDLow[FD] Returns the temporal series of low prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Low prices, {{date,low},...}"
FDVolume::usage = "FDVolume[FD] Returns the temporal series of volumes
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Volumens, {{date,volume},...}"
FDDates::usage = "FDDates[FD] Returns the dates of the temporal series of financial data
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Dates, {{date},...}"   
FDIntradayRange::usage = "" 
FDInterdayRange::usage = ""
FDMedianPrice::usage = "FDMedianPrice[FD] Returns the temporal series of median prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Median prices = (H+L)/2, {{date,median},...}" 
FDVolumePrice::usage = "FDVolumePrice[FD] Returns the temporal series of median prices x volumes
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Median prices x volumes = (H+L)/2 x V, {{date,medianvol},...}"
FDVolumeIntradayRange::usage = ""
depureData::usage = "depureData[FD] Validation of imported financial data
	Input:Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output:Depured data: in temporal order, H>=O,C>=L and volume>0"
	
(* ::Section:: *)

kNNForecast::usage = "kNNForecast[ts,m,dt,k:1,opts]
	Input: step series, embedding dimension, number of steps to forecast, number of NN
	opts: Weighting->{Mean,Median,Max,Min, ...}, Options[Nearest] 
	Output: step series joined to the dt steps forecasted"
MPForecast::usage = "MPForecast[ts,m,dt,opts]
	Input: step series, embedding dimension, number of steps to forecast
	opts: Options[Predict],Options[PredictorFunction]
	Output: step series joined to the dt steps forecasted and prediction function used for the forecasting"
MPPredictions::usage = "MPPredictions[p,ts,m,opts]
	Input: prediction function, step series, embedding dimension
	opts: Options[PredictorFunction]
	Output: predictor measurements"
smoothDCT::usage = "smoothDCT[fd,n]
	Input: financial data {{date,price},...} and smoothing days number
	Output: smoothed DCT step series"

emd::usage = "emd[ts] computes the Empirical Mode Decomposition of ts."


Begin["`Private`"]
(* To eliminate in definitive version*)
<<"StockData`"
<<"BackTest`"
<<"Strategy`"
<<"EmbeddingAnalysis`"

(* ::Section:: *)
(* Financial Data (FD) prices from Yahoo *)
FDOpen[data_] :=
    Map[{#[[1]], #[[2, 1]]} &, data]
FDClose[data_] :=
    Map[{#[[1]], #[[2, 4]]} &, data]
FDHigh[data_] :=
    Map[{#[[1]], #[[2, 2]]} &, data]
FDLow[data_] :=
    Map[{#[[1]], #[[2, 3]]} &, data]
FDVolume[data_] :=
    Map[{#[[1]], #[[2, 5]]} &, data]
FDDates[data_] :=
    Map[#[[1]] &, data]
FDIntradayRange[data_] :=
    Map[{#[[1]],
    If[ #[[2, 1]]< #[[2, 4]],
        #[[2, 2]]-#[[2, 3]],
        #[[2, 3]]-#[[2, 2]]
    ]} &, data]
FDInterdayRange[data_] :=
    Transpose@
    {FDDates[data][[2;;]],FDOpen[data][[2;;,2]]-FDClose[data][[;;-2,2]]}
FDMedianPrice[data_] :=
    Transpose@
    {FDDates[data],(FDHigh[data][[All,2]]+FDLow[data][[All,2]])/2}
FDVolumePrice[data_] :=
    Transpose@
    {FDDates[data],FDVolume[data][[All,2]] FDMedianPrice[data][[All,2]]}
FDVolumeIntradayRange[data_] :=
    Transpose@
    {FDDates[data],Abs@(FDVolume[data][[All,2]] FDIntradayRange[data][[All,2]])}

validateData[histValues_] :=
    Module[ {initialDate = AbsoluteTime[histValues[[1, 1]]],
             finalDate = AbsoluteTime[histValues[[Length[histValues], 1]]]},
        Select[
         MapIndexed[
          If[
            (* Date between initial and final dates *) AbsoluteTime[#[[1]]] > initialDate || 
             AbsoluteTime[#[[1]]] < finalDate ||
             (* High>=Low High>=Open High>=Close *)
             #[[2, 2]] < #[[2, 1]] || #[[2, 2]] < #[[2, 3]] || #[[2, 
                2]] < #[[2, 4]] ||
             (* Low<=Open, Low<=Close *)
             #[[2, 3]] > #[[2, 1]] || #[[2, 3]] > #[[2, 4]] ||
             (* Volume>0 *)
             #[[2, 5]] <= 0,
              First[#2],
              Null
          ] &, histValues
          ],
         IntegerQ
        ]
    ];

depureData[histValues_] :=
    Delete[histValues, Map[List, validateData[histValues]]];
    
(* ::Section:: *)


Options[kNNForecast] = {Weighting->Mean,Options[Nearest]};

kNNForecast[s_List,m_Integer,dt_Integer,k_Integer:1,opts:OptionsPattern[]] :=
    Module[ {s1 = s,embN,punts,valors,posicions,x0,x0N,p},
        embN = Table[Take[s1,{i,i+m}]/s1[[i]],{i, Length[s1]-m}];
        punts = embN[[All,2;;-2]];
        valors = embN[[All,-1]];
        posicions = Range[Length[punts]];
        Do[
            x0 = s1[[-m;;]];
            x0N = Rest[x0/x0[[1]] ];
            p = Nearest[punts-> posicions,x0N,k,FilterRules[{opts},Options[Nearest]]];
            s1 = Append[s1,OptionValue[Weighting][x0[[1]] valors[[#]]&/@p]],
            dt
        ];
        s1
    ]

Options[MPForecast] :=
    {Options[Predict],Options[PredictorFunction]};

MPForecast[s_List,m_Integer,dt_Integer,opts:OptionsPattern[]] :=
    Module[ {s1 = s,embN,punts,valors,x0,x0N,p},
        embN = Table[Take[s1,{i,i+m}]/s1[[i]],{i, Length[s1]-m}];
        punts = embN[[All,2;;-2]];
        valors = embN[[All,-1]];
        p = Predict[punts-> valors,FilterRules[{opts},Options[Predict]]];
        Print[PredictorInformation[p]];
        Do[
        x0 = s1[[-m;;]];
        x0N = Rest[x0/x0[[1]] ];
        (*Print[{x0[[1]],p[x0N],p[x0N] x0[[1]]}];*)
        s1 = Append[s1,x0[[1]] p[x0N,FilterRules[{opts},Options[PredictorFunction]]]],
        dt];
        {s1,p}
    ]

Options[MPPredictions] :=
    {Options[PredictorFunction]};

MPPredictions[p_PredictorFunction,s_List,m_Integer,opts:OptionsPattern[]] :=
    Module[ {embN,punts,valors,pm},
        embN = Table[Take[s,{i,i+m}]/s[[i]],{i, Length[s]-m}];
        punts = embN[[All,2;;-2]];
        valors = embN[[All,-1]];
        (*Do[Print[p[punts[[i]]],"     ",valors[[i]]],{i,20}];*)
        pm = PredictorMeasurements[p,punts-> valors,FilterRules[{opts},Options[PredictorFunction]]]
    ]
    
smoothDCT1[data_,n_] :=
    FourierDCT[
        PadRight[
            Take[
                FourierDCT[data],
                Ceiling[Length[data]/n]
            ],
            Length[data],
            0.
        ],
        3
    ]

smoothDCT[fd_,n_] :=
    Transpose[MapAt[smoothDCT1[#,n]&,Transpose[FDClose[fd]],2]];
    
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

End[]

EndPackage[]

