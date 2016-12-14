(* Mathematica Package *)

BeginPackage["Stock`",{"RLink`"}]
(* Exported symbols added here with SymbolName::usage *)  

connectDB::usage = "connectDB[] runs R to connect to yahoo"
disConnectDB::usage = "disConnectDB[] stops R and disconnects from yahoo"
Stock::usage = "Stock[s_String] obtains an Stock object, Stock[s,OHLCV,Adj,Dividends,Splits], with the financial information from 2007-1-1 to today of ticker s in yahoo.
	Stock[s_String, from_List] obtains an Stock object with the financial information from {year,month,day} to today of ticker s in yahoo.
	Stock[s_String, from_List, to_List] obtains an Stock object with the financial information from {year,month,day} to {year,month,day} of ticker s in yahoo.
	Stock[][Properties]"
Stock::arg = "The valid properties are `1`"
(*weekPrices::usage = "WeekPrices[FD] Returns the temporal series of week prices
	Input: Diary financial data obtained from Yahoo, {date,{open,high,low,close,volume},...}
	Output: Weekly financial data,  {last date week,{open,high,low,close,volume},...}"*)

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
     {"Name","Date","Open","Close","High","Low","Volume","OHLCV","Median Price","Intraday Range","Interday Range","Dividends","Splits","Adjusted","Weekly Prices","Monthly Prices"},
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
	 p === "Monthly Prices",
	 Module[ {d = Map[#[[1]] &, ohlcv], c,  mprices = {},i,j,pos,x},
         For[i = 1, i <= (d[[-1, 1]] - d[[1, 1]]) + 1, i++,
          For[j = If[ i == 1,
                      d[[1, 2]],
                      1
                  ], 
           j <= If[ i == (d[[-1, 1]] - d[[1, 1]]) + 1,
                    d[[-1, 2]],
                    12
                ], j++,
           pos = Flatten@Position[d, {d[[1, 1]] + i - 1, j, x_}];
           c = ohlcv[[pos, 2]];
           mprices = Append[mprices, {d[[pos[[1]]]], {c[[1, 1]], Max[c[[All, 2]]], 
               Min[c[[All, 3]]], c[[-1, 4]], Plus @@ c[[All, 5]]}}]
           ]
          ];
         mprices
     ],
     p === "Quarterly Prices",
     Module[{d = Map[#[[1]] &, ohlcv], c, mprices = {}, i, pos1, pos2,pos3, pos4},
 		For[i = 1, i <= (d[[-1, 1]] - d[[1, 1]]) + 1, i++,(* Recorro anys *)
  			pos1 = Flatten@
    Position[d, {d[[1, 1]] + i - 1, m_, x_} /; (m == 1 || m == 2 || m == 3)];
  pos2 = Flatten@
    Position[d, {d[[1, 1]] + i - 1, m_, x_} /; (m == 4 || m == 5 || m == 6)];
  pos3 = Flatten@
    Position[d, {d[[1, 1]] + i - 1, m_, x_} /; (m == 7 || m == 8 || m == 9)];
  pos4 = Flatten@
    Position[d, {d[[1, 1]] + i - 1, m_, x_} /; (m == 10 || m == 11 || m == 12)];
  If[Length[pos1] != 0, c = ohlcv[[pos1, 2]];
   mprices = 
    Append[mprices, {d[[pos1[[1]]]], {c[[1, 1]], Max[c[[All, 2]]], 
       Min[c[[All, 3]]], c[[-1, 4]], Plus @@ c[[All, 5]]}}]];
  If[Length[pos2] != 0, c = ohlcv[[pos2, 2]];
   mprices = 
    Append[mprices, {d[[pos2[[1]]]], {c[[1, 1]], Max[c[[All, 2]]], 
       Min[c[[All, 3]]], c[[-1, 4]], Plus @@ c[[All, 5]]}}]];
  If[Length[pos3] != 0, c = ohlcv[[pos3, 2]];
   mprices = 
    Append[mprices, {d[[pos3[[1]]]], {c[[1, 1]], Max[c[[All, 2]]], 
       Min[c[[All, 3]]], c[[-1, 4]], Plus @@ c[[All, 5]]}}]];
  If[Length[pos4] != 0, c = ohlcv[[pos4, 2]];
   mprices = 
    Append[mprices, {d[[pos4[[1]]]], {c[[1, 1]], Max[c[[All, 2]]], 
       Min[c[[All, 3]]], c[[-1, 4]], Plus @@ c[[All, 5]]}}]]
  ];
 mprices],
     p === "Weekly Prices",
     weekPrices[ohlcv],
     True,
     Message[Stock::arg,{"Name","Date","Open","Close","High","Low","Volume","OHLCV","Median Price","Intraday Range","Interday Range","Dividends","Splits","Adjusted","Weekly Prices","Monthly Prices"}]
    ]

Stock[s_String,ohlcv_List,adjp_List,div_List,split_List][fd_List,ld_List] :=
    Module[ {fdat = AbsoluteTime[fd],ldat = AbsoluteTime[ld]},
        Stock[s,
        If[ AbsoluteTime[#[[1]]] >= fdat && AbsoluteTime[#[[1]]] <= ldat,
            #,
            Nothing
        ] & /@ ohlcv,
        If[ AbsoluteTime[#[[1]]] >= fdat && AbsoluteTime[#[[1]]] <= ldat,
            #,
            Nothing
        ] & /@ adjp,
        If[ AbsoluteTime[#[[1]]] >= fdat && AbsoluteTime[#[[1]]] <= ldat,
            #,
            Nothing
        ] & /@ div,
        If[ AbsoluteTime[#[[1]]] >= fdat && AbsoluteTime[#[[1]]] <= ldat,
            #,
            Nothing
        ] & /@ split
        ]
    ]

Format[Stock[s_String,ohlcv_List,adjp_List,div_List,split_List],StandardForm] := Panel[Column[{s,Row[{ "from  ", DateObject[ohlcv[[1,1]]], "  to  ", DateObject[ohlcv[[Length[ohlcv],1]]]}], Length[ohlcv] " bars"}],Style["Stock Object",16]]

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
    Module[ {actualday, weekdays = {}, prices, i, ii},
        actualday = dayNumber[datelist[[1, 1]]];
        i = 1;
        While[(i <= Length[datelist] && 
           dayNumber[datelist[[i, 1]]] == actualday && Length[weekdays] <= 5),
         actualday++;
         weekdays = Append[weekdays, datelist[[i]]];
         i++];
        prices = {weekdays[[1, 2, 1]], Max[Map[#[[2, 2]] &, weekdays]], 
          Min[Map[#[[2, 3]] &, weekdays]], 
          weekdays[[Length[weekdays], 2, 4]], 
          Sum[weekdays[[ii, 2, 5]], {ii, Length[weekdays]}]};
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