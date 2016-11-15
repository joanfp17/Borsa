(* Mathematica Package *)

BeginPackage["Strategy`" , { "Stock`"} ]
(* Exported symbols added here with SymbolName::usage *) 

Strategy::usage="Strategy[strategyName,strategyParameters] returns the Strategy Object"
Strategy::arg = "Strategy `1` not defined"

Begin["`Private`"] (* Begin Private Context *) 

Strategy[strategyName_String,strategyParameters_List][p_String]:=
	Which[
     p === "Properties",
     {"Name","Parameters"},
     p === "Name",
     strategyName,
     p === "Parameters",
     strategyParameters,
     True,
     Message[Strategy::arg,{"StrategyName","StrategyParameters"}]
	];
	
Format[Strategy[strategyName_String,strategyParameters_List]]:=
	Panel[Column[{Row[{strategyName,"  ",strategyParameters}]," "," "}],Style["Strategy Object",16]]

Strategy[strategyName_String,strategyParameters_List][s_Stock] :=
    Which[
        strategyName==="Ema Up Down",
        emaUpDown[s,strategyParameters],
        strategyName==="ParabolicStopAndReversal",
        stochasticK[s,strategyParameters],
        True,
        Message[Strategy::arg,strategyName]
    ]

emaUpDown[s_Stock, p_List] :=
    Module[ 
    {ema = (Transpose@FinancialIndicator["ExponentialMovingAverage", p[[1]]][s["OHLCV"]]["Path"])[[2]], 
    lows = s["Low"][[All,2]],
    closes = s["Close"][[All, 2]],
    fi},
    fi[l_, c_, e_] :=
    	If[ l > e,
            1,
            If[ c < e,
               -1,
                0
            ]
    	];
    {s["Date"][[2;;]],Delete[MapThread[fi, {lows, closes, ema}], -1]}
    (* Desplaso al dia seguent *)
    ]	

stochasticK[s_Stock, p_List] := Module[
  {PSAR = (Transpose@
       FinancialIndicator["ParabolicStopAndReversal",p[[1]],p[[2]]][s["OHLCV"]]
        ["Path"])[[2]],
   opens = s["Open"][[All,2]],
   closes = s["Close"][[All, 2]],
   fi
  },
  fi[o_, c_, e_] := If[e<c, 1, If[e>o,-1, 0]];
  {s["Date"][[2;;]],
  	Delete[MapThread[fi, {opens, closes, PSAR}], -1]}
   (* Desplaso al p[[1]] dies *)
  ]	

End[] (* End Private Context *)

EndPackage[]