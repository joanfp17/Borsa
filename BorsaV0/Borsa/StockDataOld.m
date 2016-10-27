(* Mathematica Package *)

BeginPackage["StockData`",{"RLink`"}]
(* Exported symbols added here with SymbolName::usage *)  

connectDB::usage = "connectDB[] runs R to connect to yahoo"
disConnectDB::usage = "disConnectDB[] stops R and disconnects from yahoo"
getSymbol::usage = "getSymbol[s_String] obtains financial information from 2000-1-1 to today of ticker s in yahoo. {s,OHLCV,Adj,Dividends,Splits}"
getSymbol::usage = "getSymbol[s_String, from_List] obtains financial information from {year,month,day} to today of ticker s in yahoo. {s,OHLCV,Adj,Dividends,Splits}"
getSymbol::usage = "getSymbol[s_String, from_List, to_List] obtains financial information from {year,month,day} to {year,month,day} of ticker s in yahoo. {s,OHLCV,Adj,Dividends,Splits}"
stockO::usage = "stockO[s_String] Returns the temporal series of open prices. Input: Financial Data obtained from Yahoo,{s,OHLCV,Adj,Dividends,Splits}. Output: Open prices, {{date,open},...}"
stockC::usage = "stockC[s_String] Returns the temporal series of close prices. Input: Financial Data obtained from Yahoo,{s,OHLCV,Adj,Dividends,Splits}. Output: Close prices, {{date,close},...}"
stockH::usage = "FDHigh[FD] Returns the temporal series of high prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: High prices, {{date,high},...}"
stockL::usage = "FDLow[FD] Returns the temporal series of low prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Low prices, {{date,low},...}"
stockV::usage = "FDVolume[FD] Returns the temporal series of volumes
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Volumens, {{date,volume},...}"
stockDates::usage = "stockDates[FD] Returns the dates of some temporal series of financial data. Input: {date,{...},...}. Output: Dates, {{date},...}" 
stockOHLCV::usage = "stockOHLCV[s_List] Returns the temporal series of OHLCV." 
stockOHLC::usage = "stockOHLC[s_List] Returns the temporal series of OHLC." 
stockIntradayRange::usage = "" 
stockInterdayRange::usage = ""
stockMedianPrice::usage = "FDMedianPrice[FD] Returns the temporal series of median prices
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Median prices = (H+L)/2, {{date,median},...}" 
stockVolumePrice::usage = "FDVolumePrice[FD] Returns the temporal series of median prices x volumes
	Input: Financial Data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Median prices x volumes = (H+L)/2 x V, {{date,medianvol},...}"
stockVolumeIntradayRange::usage = ""
weekPrices::usage = "WeekPrices[FD] Returns the temporal series of week prices
	Input: Diary financial data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Weekly financial data,  {last date week,{open,high,low,close,volume},...}"

Begin["`Private`"] (* Begin Private Context *) 

connectDB[] :=
    (
    InstallR[];
    REvaluate["library(\"quantmod\")"]
    );

disConnectDB[]:=UninstallR[];

dateList2String[d_List] :=
    (ToString /@ d)[[1]] <> "-" <> (ToString /@ d)[[2]] <> 
     "-" <> (ToString /@ d)[[3]];

getSymbol[s_String] := Module[{symbol, dividends, splits},
   REvaluate["getSymbols(\"" <> s <> "\")"];
   (*Vigilar Llistes buides*)
   symbol = REvaluate[s];
   Quiet[dividends = REvaluate["getDividends(\"" <> s <> "\")"]];
   If[Head@dividends =!= RObject, dividends = {}];
   Quiet[splits = REvaluate["getSplits(\"" <> s <> "\")"]];
   If[Head@splits =!= RObject, splits = {}];
   {s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
      symbol[[1, All, {1, 2, 3, 4, 5}]]}, 
    Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), symbol[[1, All, 6]]},
    If[dividends === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (dividends[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
       dividends[[1, All, 1]]}],
    If[splits === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (splits[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), splits[[1, All, 1]]}]
    }
   ];

getSymbol[s_String, from_List] := Module[{symbol, dividends, splits},
   REvaluate[
    "getSymbols(\"" <> s <> "\",from=\"" <> dateList2String[from] <> 
     "\")"];
   symbol = REvaluate[s];
   Quiet[dividends = 
     REvaluate[
      "getDividends(\"" <> s <> "\",from=\"" <> 
       dateList2String[from] <> "\")"]];
   If[Head@dividends =!= RObject, dividends = {}];
   Quiet[splits = 
     REvaluate[
      "getSplits(\"" <> s <> "\",from=\"" <> dateList2String[from] <> 
       "\")"]];
   If[Head@splits =!= RObject, splits = {}];
   {s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
      symbol[[1, All, {1, 2, 3, 4, 5}]]}, 
    Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), symbol[[1, All, 6]]},
    If[dividends === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (dividends[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
       dividends[[1, All, 1]]}],
    If[splits === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (splits[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), splits[[1, All, 1]]}]
    }
   ];

getSymbol[s_String, from_List, to_List] := 
  Module[{symbol, dividends, splits},
   REvaluate[
    "getSymbols(\"" <> s <> "\",from=\"" <> dateList2String[from] <> 
     "\",to=\"" <> dateList2String[to] <> "\")"];
   symbol = REvaluate[s];
   Quiet[dividends = 
     REvaluate[
      "getDividends(\"" <> s <> "\",from=\"" <> 
       dateList2String[from] <> "\",to=\"" <> dateList2String[to] <> 
       "\")"]];
   If[Head@dividends =!= RObject, dividends = {}];
   Quiet[splits = 
     REvaluate[
      "getSplits(\"" <> s <> "\",from=\"" <> dateList2String[from] <> 
       "\",to=\"" <> dateList2String[to] <> "\")"]];
   If[Head@splits =!= RObject, splits = {}];
   {s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
      symbol[[1, All, {1, 2, 3, 4, 5}]]}, 
    Transpose@{Delete[{{4}, {5}, {6}}] /@ 
       DateList /@ (symbol[[2, 2, 2, 1]] + 
          AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), symbol[[1, All, 6]]},
    If[dividends === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (dividends[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), 
       dividends[[1, All, 1]]}],
    If[splits === {}, {}, 
     Transpose@{Delete[{{4}, {5}, {6}}] /@ 
        DateList /@ (splits[[2, 2, 2, 1]] + 
           AbsoluteTime[{1970, 1, 1, 0, 0, 0}]), splits[[1, All, 1]]}]
    }
   ];

stockO[s_List] :=
    Map[{#[[1]], #[[2, 1]]} &, s[[2]]];
stockC[s_List] :=
    Map[{#[[1]], #[[2, 4]]} &, s[[2]]];
stockH[s_List] :=
    Map[{#[[1]], #[[2, 2]]} &, s[[2]]];
stockL[s_List] :=
    Map[{#[[1]], #[[2, 3]]} &, s[[2]]];
stockV[s_List] :=
    Map[{#[[1]], #[[2, 5]]} &, s[[2]]];
stockOHLCV[s_List]:=s[[2]];
stockOHLC[s_List]:=
	Map[{#[[1]], #[[2,{1,2,3,4}]]} &, s[[2]]];
stockDates[s_List] := If[
	Head[s[[1]]]===List,
	Map[#[[1]] &, s],
	Map[#[[1]] &, s[[2]]]
]
stockIntradayRange[s_List] :=
    Map[{#[[1]],
    If[ #[[2, 1]]< #[[2, 4]],
        #[[2, 2]]-#[[2, 3]],
        #[[2, 3]]-#[[2, 2]]
    ]} &, s[[2]]]
stockInterdayRange[s_List] :=
    Transpose@
    {stockDates[s][[2;;]],stockO[s][[2;;,2]]-stockC[s][[;;-2,2]]}
stockMedianPrice[s_List] :=
    Transpose@
    {stockDates[s],(stockH[s][[All,2]]+stockL[s][[All,2]])/2}
stockVolumePrice[s_List] :=
    Transpose@
    {stockDates[s[[2]]],stockV[s][[All,2]] stockMedianPrice[s][[All,2]]}
stockVolumeIntradayRange[s_List] :=
    Transpose@
    {stockDates[s],Abs@(stockV[s][[All,2]] stockIntradayRange[s][[All,2]])}

dayNumber[data_] :=
    Switch[
        DayName[data],
        Monday, 1,
        Tuesday, 2,
        Wednesday, 3,
        Thursday, 4,
        Friday, 5,
        Saturday, 6,
        Sunday, 7
    ]

oneWeekPrices[datelist_] :=
    Module[ {firstday, weekdays = {}, prices = {}, i, ii},
        firstday = dayNumber[datelist[[1, 1]]];
        i = 1;
        While[(i <= Length[datelist] && dayNumber[datelist[[i, 1]]] >= 1 && 
           dayNumber[datelist[[i, 1]]] <= firstday && Length[weekdays] < 5),
                 weekdays = Append[weekdays, datelist[[i]]];
                 i++];
        prices = Append[prices, weekdays[[Length[weekdays]]][[2, 1]]];
        prices = Append[prices, Max[Map[#[[2, 2]] &, weekdays]]];
        prices = Append[prices, Min[Map[#[[2, 3]] &, weekdays]]];
        prices = Append[prices, weekdays[[1, 2, 4]]];
        prices = Append[prices, Sum[weekdays[[ii, 2, 5]], {ii, Length[weekdays]}]];
        {Length[weekdays], {weekdays[[Length[weekdays], 1]], prices}}
    ]

weekPrices[dateprices_] :=
    Module[ {oneweekprices = {}, weekprices = {},dateprices1 = dateprices},
        While[Length[dateprices1] > 0,
             oneweekprices = oneWeekPrices[dateprices1];
             weekprices = Append[weekprices, oneweekprices[[2]]];
             dateprices1 = Drop[dateprices1, oneweekprices[[1]]]
        ];
        weekprices
    ]
End[] (* End Private Context *)

EndPackage[]