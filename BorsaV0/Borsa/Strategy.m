(* Mathematica Package *)

BeginPackage["Strategy`" , { "Stock`"} ]
(* Exported symbols added here with SymbolName::usage *) 

Strategy::usage="Strategy[strategyName,strategyParameters] returns the Strategy Object"
Strategy::arg = "Strategy `1` not defined"

Begin["`Private`"] (* Begin Private Context *) 

strategies = {"Ema Up Down","ParabolicStopAndReversal","maSF"};
	
Strategy[strategyName_String,strategyParameters_List][p_String] :=
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
	
Format[Strategy[strategyName_String,strategyParameters_List]] :=
    If[ MemberQ[strategies,strategyName],
        Panel[Column[{Row[{strategyName,"  ",strategyParameters}]," "," "}],Style["Strategy Object",16]],
        Message[Strategy::arg,strategyName]
    ]

Strategy[strategyName_String,strategyParameters_List][s_Stock] :=
    Which[
        strategyName==="Ema Up Down",
        emaUpDown[s,strategyParameters],
        strategyName==="ParabolicStopAndReversal",
        psr[s,strategyParameters],
        strategyName==="maSF",
        maSF[s,strategyParameters],
        True,
        Message[Strategy::arg,strategyName]
    ]

emaUpDown[s_Stock, p_List] :=
    Module[ {
    ema = (Transpose@
    FinancialIndicator["ExponentialMovingAverage", p[[1]]][
    s["OHLCV"]]["Path"])[[2]], lows = s["Low"][[All, 2]],
    closes = s["Close"][[All, 2]],
    fi, flag = 0
    },
        fi[l_, c_, e_] :=
            If[ l > e &&  flag != 1,
                flag = 1;
                1,
                If[ c < e && flag == 1,
                    flag = -1;
                    -1,
                    0
                ]
            ];
        (* Desplaso al dia seguent i retallo el primer per entrar/
        sortir al dia seguent al senyal, 
        elimino els p[[1]] primers pel lag *)
        Drop[Transpose@{s["Date"][[2 ;;]], 
           Delete[MapThread[fi, {lows, closes, ema}], -1]}, p[[1]]]
 ]	

psr[s_Stock, p_List] := Module[
  {PSAR = (Transpose@
       FinancialIndicator["ParabolicStopAndReversal",p[[1]],p[[2]]][s["OHLCV"]]
        ["Path"])[[2]],
   opens = s["Open"][[All,2]],
   closes = s["Close"][[All, 2]],
   fi
  },
  fi[o_, c_, e_] := If[e<c, 1, If[e>o,-1, 0]];
  Transpose@{s["Date"][[2;;]],
  	Delete[MapThread[fi, {opens, closes, PSAR}], -1]}
   (* Desplaso al p[[1]] dies *)
  ]
  
maSF[s_Stock, p_List] :=
    Module[ 
    {maS = (Transpose@FinancialIndicator["SimpleMovingAverage", p[[1]]][s["OHLCV"]]["Path"])[[2]],
     maF = (Transpose@FinancialIndicator["SimpleMovingAverage", p[[2]]][s["OHLCV"]]["Path"])[[2]],
     fi},
    fi[slow_, fast_] :=
    	If[fast >= slow,
            1,
            If[ fast < slow,
               -1,
                0
            ]
    	];
   Drop[Transpose@{s["Date"][[2;;]],Delete[MapThread[fi, {maS,maF}], -1]},p[[1]]]
]		

End[] (* End Private Context *)

EndPackage[]