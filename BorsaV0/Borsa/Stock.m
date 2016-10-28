(* Mathematica Package *)

BeginPackage["StockData`",{"RLink`"}]
(* Exported symbols added here with SymbolName::usage *)  

connectDB::usage = "connectDB[] runs R to connect to yahoo"
disConnectDB::usage = "disConnectDB[] stops R and disconnects from yahoo"
Stock::usage = "Stock[s_String] obtains an Stock object, Stock[s,OHLCV,Adj,Dividends,Splits], with the financial information from 2007-1-1 to today of ticker s in yahoo.
	Stock[s_String, from_List] obtains an Stock object with the financial information from {year,month,day} to today of ticker s in yahoo.
	Stock[s_String, from_List, to_List] obtains an Stock object with the financial information from {year,month,day} to {year,month,day} of ticker s in yahoo.
	Stock[][Properties]"
Stock::arg = "The valid properties are `1`"
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

Stock[s_String] := Module[{symbol, dividends, splits},
   REvaluate["getSymbols(\"" <> s <> "\")"];
   (*Vigilar Llistes buides*)
   symbol = REvaluate[s];
   Quiet[dividends = REvaluate["getDividends(\"" <> s <> "\")"]];
   If[Head@dividends =!= RObject, dividends = {}];
   Quiet[splits = REvaluate["getSplits(\"" <> s <> "\")"]];
   If[Head@splits =!= RObject, splits = {}];
   Stock[s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
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
   ]
   ];

Stock[s_String, from_List] := Module[{symbol, dividends, splits},
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
   Stock[s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
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
   ]
   ];

Stock[s_String, from_List, to_List] := 
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
   Stock[s, Transpose@{Delete[{{4}, {5}, {6}}] /@ 
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
   ]
   ];

Stock[s_String,ohlcv_List,adjp_List,div_List,split_List][p_String] :=
    Which[
     p === "Properties",
     {"Name","Date","Open","Close","High","Low","Volume","OHLCV","Median Price","Intraday Range","Interday Range","Dividends","Splits","Adjusted"},
     p === "Name",
     s,
     p === "Date",
     Map[#[[1]] &, ohlcv],
     p === "Open",
     Map[{#[[1]], #[[2, 1]]} &, ohlcv],
     p === "Close",
     Map[{#[[1]], #[[2, 4]]} &, ohlcv],
     p === "High",
     Map[{#[[1]], #[[2, 2]]} &, ohlcv],
     p === "Low",
     Map[{#[[1]], #[[2, 3]]} &, ohlcv],
     p === "Volume",
     Map[{#[[1]], #[[2, 5]]} &, ohlcv],
     p === "OHLCV",
     ohlcv,
     p === "Median Price",
     Transpose@{Map[{#[[1]],(#[[2, 2]]+#[[2, 3]])/2}&,ohlcv]},
     p === "Intraday Range",
     Map[{#[[1]],
	    If[ #[[2, 1]]< #[[2, 4]],
	        #[[2, 2]]-#[[2, 3]],
	        #[[2, 3]]-#[[2, 2]]
	    ]} &, ohlcv],
	 p === "Interday Range",
	 Transpose@{Map[{#[[1]],#[[2, 1]]-#[[2, 4]]}&, ohlcv]},
	 p === "Dividends",
	 div,
	 p === "Splits",
	 split,
	 p === "Adjusted",
	 adjp,
     True,
     Message[Stock::arg,{"Name","Date","Open","Close","High","Low","Volume","OHLCV","Median Price","Intraday Range","Interday Range","Dividends","Splits","Adjusted"}]
    ]

Format[Stock[s_String,ohlcv_List,adjp_List,div_List,split_List]] := Panel[Column[{s,Row[{ "from  ", DateObject[ohlcv[[1,1]]], "  to  ", DateObject[ohlcv[[Length[ohlcv],1]]]}], Length[ohlcv] " bars"}],Style["Stock Object",16]]

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