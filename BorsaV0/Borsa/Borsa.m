(* Mathematica Package *)

(* Created by the Wolfram Workbench 05/09/2014 *)

BeginPackage["Borsa`",{"Stock`","BackTest`","EmbeddingAnalysis`","Strategy`"}]
 

(* ::Section:: *)
(* Financial Data (FD) prices from Yahoo *)

depureData::usage = "depureData[FD] Validation of imported financial data
	Input:Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output:Depured data: in temporal order, H>=O,C>=L and volume>0"
	
IDateListPlot::usage ="\!\(\*
StyleBox[\"interactiveDateListPlot\",\nFontFamily->\"Courier New Bold\
\",\nFontSize->11,\nFontWeight->\"Plain\"]\)[\!\(\*
StyleBox[\"data\",\nFontSlant->\"Italic\"]\)] generates a \!\(\*
StyleBox[\"DateListPlot\",\nFontFamily->\"Courier New Bold\",\n\
FontSize->11,\nFontWeight->\"Plain\"]\) of the data with interactive \
sliders below the plot to enable the user to dynamically change the \
plot range. Options are the same as for \!\(\*
StyleBox[\"DateListPlot\",\nFontFamily->\"Courier New Bold\",\n\
FontSize->11,\nFontWeight->\"Plain\"]\).";

SignalTradingChart::usage = "SignalTradingChart[Stock,List[Signal],options]"
ISTradingChart::usage = "ISTradingChart[Stock,List[Signal],options]"
	
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
smoothDCT::usage = "smoothDCT[s_Stock,n_,opts:OptionsPattern[]]
	Input: financial data and smoothing days number
	Output: smoothed DCT step series"
smoothDCT1::usage = "smoothDCT1[data_List,n_Integer]"

emd::usage = "emd[ts] computes the Empirical Mode Decomposition of ts."


Begin["`Private`"]
(* To eliminate in definitive version*)
<<"Stock`"
<<"BackTest`"
<<"Strategy`"
<<"EmbeddingAnalysis`"

(* ::Section:: *)
(* Financial Data (FD) prices from Yahoo *)


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


Options[IDateListPlot] = Options[DateListPlot];
SetOptions[IDateListPlot, ImageSize -> 600];

SyntaxInformation[IDateListPlot] = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

IDateListPlot[data_List, opts : OptionsPattern[]] := 
  DynamicModule[{x = 0.1, y = 0.9, z = 0.5, tmp = 0.8, d1,
    d2, g, handle, opts1, temp, userPlotRange, yRange = Automatic, 
    xAxisLength, imagesize = OptionValue[ImageSize], data1 = data},
   (* we're working with only the x axis length. 
   control the y axis length via AspectRatio option *)
   xAxisLength = If[ListQ[imagesize], First[imagesize], imagesize];
   (* make sure only DateListPlot options are entered *)
   opts1 = FilterRules[Flatten[{opts}], Options[DateListPlot]];
   (* we ideally want to be able to extract user defined y axis ranges *)
   userPlotRange = PlotRange /. opts1;
   (* so if the user enters a specific y axis range then let's extract that *)
   Which[
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     Length[userPlotRange[[2]]] == 2, yRange = userPlotRange[[2]], 
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     userPlotRange[[2]] === All, yRange = All,
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     userPlotRange[[2]] === Automatic, yRange = Automatic,
    ! ListQ[userPlotRange] && userPlotRange === All, yRange = All,
    ! ListQ[userPlotRange] && userPlotRange === Automatic, 
    yRange = Automatic
    ];
   (* make a little Locator object *)
   handle = 
    Graphics[{EdgeForm[Directive[Thin, Gray]], GrayLevel[0.8], 
      Rectangle[{0, 0}, {1, 4}], EdgeForm[None], White, 
      Rectangle[{0.3, 0.3}, {.7, 3.7}]},
     AspectRatio -> 4, ImageSize -> 7];
   (* make the small image of the plot to be used as the slider background and also for extracting the plot range *)
   g = DateListPlot[data1,
     AspectRatio -> 40/xAxisLength,
     Axes -> False,
     Frame -> False,
     (* if Joined\[Rule]False AbsoluteOptions fails however PlotRange\[Rule]Full works regardless *)
     Joined -> True,
     ImageSize -> {xAxisLength, 40},
     ImagePadding -> {{0, 0}, {0, 0}},
     PlotRange -> All];
   (* extract the x axis plot ranges *)
   {{d1, d2}, temp} = PlotRange /. AbsoluteOptions[g, PlotRange];
   (* Render the plot and sliders *)
   Deploy@Column[{
      (* main plot *)
      Dynamic[
       DateListPlot[data1,
        ImageSize -> xAxisLength,
        ImagePadding -> {{50, 10}, {20, 10}},
        PlotRange -> {{d1 + x*(d2 - d1), d1 + y*(d2 - d1)}, yRange},
        opts1],
       TrackedSymbols :> {d1, d2, x, y}],
      (* "Locator slider" *)
      Row[{Spacer[{50, 0}],
        Graphics[{White, EdgeForm[], Rectangle[{0, 0}, {1, 0.1}], 
          Inset[g, Scaled[{0.5, 0.5}], Scaled[{0.5, 0.5}], 1], 
          Opacity[0.15],
          RGBColor[0, 0.5, 1], 
          EdgeForm[Directive[Thin, GrayLevel[0.5]]], 
          Rectangle[Dynamic[{x, 0}], Dynamic[{y, 0.1}]], 
          Locator[Dynamic[{y, 
              0.05}, (y = Min[1, Max[0.05 + x, First@#1]]; 
              z = 0.5*(x + y)) &], handle], 
          Locator[Dynamic[{x, 
             0.05}, (x = Max[0, Min[y - 0.05, First@#1]]; 
              z = 0.5*(x + y)) &], handle]},
         AspectRatio -> 40/xAxisLength,
         Frame -> False,
         ImageSize -> xAxisLength - 60,
         ImagePadding -> {{0, 0}, {1, 1}},
         PlotRangePadding -> 0], Spacer[{10, 0}]}],
      (* slider *)
      Row[{Spacer[{50, 0}], 
        Slider[Dynamic[
          z, (tmp = y - x; z = #; 
            y = Min[1, Max[0.05 + x, z + 0.5*tmp]]; 
            x = Max[0, Min[y - 0.05, z - 0.5*tmp]]) &], 
         Appearance -> "UpArrow", ImageSize -> xAxisLength - 60], 
        Spacer[{10, 0}]}]
      },
     Alignment -> Right,
     Spacings -> 0]
   ];

SetAttributes[IDateListPlot, ReadProtected];

Options[SignalTradingChart] = Options[TradingChart];

SignalTradingChart[stock_Stock, signals_List, 
  opts : OptionsPattern[]] :=
 Module[{tchartx, n = Length[signals],lines, 
   color = {Red, Green, Blue, Black, Gray, Cyan, Magenta, Yellow, 
     Brown, Orange}},
  tchartx[val_, limits_: stock["OHLCV"][[{1, -1}, 1]]] := 
   Rescale[AbsoluteTime[val], 
    AbsoluteTime /@ limits, {1, Length[stock["OHLCV"]]}];
  lines = Line /@ Map[{tchartx[#1[[1]]], #1[[2]]} &, signals, {2}];
  TradingChart[stock["OHLCV"],
   Prolog -> {Riffle[color[[1 ;; n]], lines]
     },
   PlotLabel -> stock["Name"], opts]
  ]

Options[ISTradingChart] = Options[TradingChart];
SetOptions[ISTradingChart, ImageSize -> 600];

SyntaxInformation[
   ISTradingChart] = {"ArgumentsPattern" -> {_, OptionsPattern[]}};

ISTradingChart[stock_Stock, signals_List, opts : OptionsPattern[]] := 
  DynamicModule[{x = 0.1, y = 0.9, z = 0.5, tmp = 0.8, d1, d2, g, 
    handle, opts1, temp, userPlotRange, yRange = Automatic, 
    xAxisLength, imagesize = OptionValue[ImageSize], 
    data1 = stock},(*we're working with only the x axis length.control \
the y axis length via AspectRatio option*)
   xAxisLength = If[ListQ[imagesize], First[imagesize], imagesize];
   (*make sure only TradingChart options are entered*)
   opts1 = FilterRules[Flatten[{opts}], Options[TradingChart]];
   (*we ideally want to be able to extract user defined y axis ranges*)
   userPlotRange = PlotRange /. opts1;
   (*so if the user enters a specific y axis range then let's extract \
that*)Which[
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     Length[userPlotRange[[2]]] == 2, yRange = userPlotRange[[2]], 
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     userPlotRange[[2]] === All, yRange = All, 
    ListQ[userPlotRange] && Length[userPlotRange] == 2 && 
     userPlotRange[[2]] === Automatic, 
    yRange = Automatic, ! ListQ[userPlotRange] && 
     userPlotRange === All, 
    yRange = All, ! ListQ[userPlotRange] && 
     userPlotRange === Automatic, yRange = Automatic];
   (*make a little Locator object*)
   handle = 
    Graphics[{EdgeForm[Directive[Thin, Gray]], GrayLevel[0.8], 
      Rectangle[{0, 0}, {1, 4}], EdgeForm[None], White, 
      Rectangle[{0.3, 0.3}, {.7, 3.7}]}, AspectRatio -> 4, 
     ImageSize -> 7];
   (*make the small image of the plot to be used as the slider \
background and also for extracting the plot range*)
   g = DateListPlot[{#[[1]], #[[2]]} & /@ data1["Close"], 
     AspectRatio -> 40/xAxisLength, Axes -> False, 
     Frame -> 
      True,(*if Joined\[Rule]False AbsoluteOptions fails however \
PlotRange\[Rule]Full works regardless*)ImageSize -> {xAxisLength, 40},
      ImagePadding -> {{0, 0}, {0, 0}}, PlotRange -> All];
   (*extract the x axis plot ranges*){{d1, d2}, temp} = 
    PlotRange /. AbsoluteOptions[g, PlotRange];
   (*Render the plot and sliders*)
   Deploy@Column[{(*main plot*)
      Dynamic[SignalTradingChart[data1, signals, 
        ImageSize -> xAxisLength, 
        ImagePadding -> {{50, 50}, {20, 10}}, 
        PlotRange -> {{d1 + x*(d2 - d1), d1 + y*(d2 - d1)}, yRange}, 
        opts1], TrackedSymbols :> {d1, d2, x, y}],(*"Locator slider"*)
      Row[{Spacer[{15, 0}], 
        Graphics[{White, EdgeForm[], Rectangle[{0, 0}, {1, 0.1}], 
          Inset[g, Scaled[{0.5, 0.5}], Scaled[{0.5, 0.5}], 1], 
          Opacity[0.15], RGBColor[0, 0.5, 1], 
          EdgeForm[Directive[Thin, GrayLevel[0.5]]], 
          Rectangle[Dynamic[{x, 0}], Dynamic[{y, 0.1}]], 
          Locator[Dynamic[{y, 
             0.05}, (y = Min[1, Max[0.05 + x, First@#1]];
              z = 0.5*(x + y)) &], handle], 
          Locator[Dynamic[{x, 
             0.05}, (x = Max[0, Min[y - 0.05, First@#1]];
              z = 0.5*(x + y)) &], handle]}, 
         AspectRatio -> 40/xAxisLength, Frame -> False, 
         ImageSize -> xAxisLength - 60, 
         ImagePadding -> {{0, 0}, {1, 1}}, PlotRangePadding -> 0], 
        Spacer[{10, 0}]}],(*slider*)
      Row[{Spacer[{15, 0}], Slider[Dynamic[z, (tmp = y - x; z = #;
            y = Min[1, Max[0.05 + x, z + 0.5*tmp]];
            x = Max[0, Min[y - 0.05, z - 0.5*tmp]]) &], 
         Appearance -> "UpArrow", ImageSize -> xAxisLength - 60], 
        Spacer[{10, 0}]}]}, Alignment -> Left, Spacings -> 0]];

SetAttributes[ISTradingChart, ReadProtected];


Options[kNNForecast] = {"Weighting"->Mean,Options[Nearest]};

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
            s1 = Append[s1,OptionValue["Weighting"][x0[[1]] valors[[#]]&/@p]],
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

Options[smoothDCT]:={"Property"->"Close"};

smoothDCT[s_Stock,n_,opts:OptionsPattern[]] :=
    Transpose[MapAt[smoothDCT1[#,n]&,Transpose[s[OptionValue["Property"]]],2]];
    
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

