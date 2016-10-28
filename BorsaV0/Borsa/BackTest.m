(* Mathematica Package *)

BeginPackage["BackTest`",{"Stock`","Strategy`"}]
(* Exported symbols added here with SymbolName::usage *)  

BackTest::usage = "BackTest[Stock, Strategy, Options] returns a BackTest Object with a list of all the trades done with the strategy
	{EntryDay,{EntryPrices},ExitDay,{ExitPrices}...}"
BackTest::usage=" BackTest[\"Properties\"]"
BackTest::arg= "The valid properties are `1`"

Begin["`Private`"] (* Begin Private Context *) 

Options[BackTest] = {
      "EntryPrices" -> {"Open", "High"}(* {BuyPrice, SlippagePrice}:
   		Open, Median = (H+L)/2, High, Medium = (High+BuyPrice)/2 *),
      "ExitPrices" -> {"Open" , "Low"}(* {SellPrice, SlippagePrice}:
   		Open, Close, Median = (H+L)/2, Low, Medium = (Low+SellPrice)/2 *)
      };

BackTest[stock_Stock, strategy_Strategy, o : OptionsPattern[]] :=
    Module[ {dateStock, signal, dateSignal,
    	open=stock["Open"][[All,2]], high=stock["High"][[All,2]], 
    	low=stock["Low"][[All,2]], close=stock["Close"][[All,2]],div=stock["Dividends"],
    	initialDate, finalDate, stockIndices, signalIndices, buyDate, buyPrice, buyPriceSlip, sellDate, sellPrice, 
    	sellPriceSlip, positions = {}, actualPosition, position = "Out",initialIndex,finalIndex,dividend,
    	entryPrices=OptionValue["EntryPrices"], exitPrices=OptionValue["ExitPrices"],period
    	},
    	signal=strategy[stock];
    	dateStock = AbsoluteTime/@stock["Date"];
    	dateSignal=AbsoluteTime/@signal[[1]];
    	(* Adjust temporal coincidence between stock and signal *)
        initialDate=Max[dateStock[[1]],dateSignal[[1]]];
        finalDate=Min[dateStock[[-1]],dateSignal[[-1]]];
        stockIndices=Flatten[Position[dateStock,#]&/@{initialDate,finalDate}];
        signalIndices=Flatten[Position[dateSignal,#]&/@{initialDate,finalDate}];
        dateStock=stock["Date"][[stockIndices[[1]];;stockIndices[[2]]]];
        open=open[[stockIndices[[1]];;stockIndices[[2]]]];
        high=high[[stockIndices[[1]];;stockIndices[[2]]]];
        low=low[[stockIndices[[1]];;stockIndices[[2]]]];
        close=close[[stockIndices[[1]];;stockIndices[[2]]]];
        (* First and last days of the period traded *)
        buyPrice = Which[
                 OptionValue["EntryPrices"][[1]] == "Open", open[[1]],
                 OptionValue["EntryPrices"][[1]] == "Median", (high[[1]] + low[[1]])/2
                 ];
        buyPriceSlip = Which[
                 OptionValue["EntryPrices"][[2]] == "High", high[[1]],
                 OptionValue["EntryPrices"][[2]] == "Medium", (high[[1]] + buyPrice)/2
                 ];
        sellPrice = Which[
                 OptionValue["ExitPrices"][[1]] == "Open", open[[-1]],
                 OptionValue["ExitPrices"][[1]] == "Median", (high[[-1]] + low[[-1]])/2,
                 OptionValue["ExitPrices"][[1]] == "Close", close[[-1]]
                 ];
        sellPriceSlip = Which[
                 OptionValue["ExitPrices"][[2]] == "Low", low[[-1]],
                 OptionValue["ExitPrices"][[2]] == "Medium", (low[[-1]] + sellPrice)/2
                 ];
        dividend=Plus @@ (If[AbsoluteTime[#[[1]]] > AbsoluteTime[dateStock[[1]]] &&
        	AbsoluteTime[#[[1]]] < AbsoluteTime[dateStock[[-1]]], #[[2]],0] & /@ div);
        period={dateStock[[1]],buyPrice,buyPriceSlip,dateStock[[-1]],sellPrice,sellPriceSlip,Length[dateStock],
        	dividend,Max[high],Min[low]}; (* TODO modify period, like positions *)
        (* Compute the trades with the signals, prices and options *)    
        MapIndexed[
         Which[
           position == "Out",
           If[ #1 == 1,
               initialIndex = First[#2];
               buyDate = dateStock[[First[#2]]];
               buyPrice = Which[
                 OptionValue["EntryPrices"][[1]] == "Open", open[[First[#2]]],
                 OptionValue["EntryPrices"][[1]] == 
                  "Median", (high[[First[#2]]] + low[[First[#2]]])/2
                 ];
               buyPriceSlip = Which[
                 OptionValue["EntryPrices"][[2]] == "High", high[[First[#2]]],
                 OptionValue["EntryPrices"][[2]] == 
                  "Medium", (high[[First[#2]]] + buyPrice)/2
                 ];
               position = "Long";
               actualPosition = {buyDate, buyPrice, buyPriceSlip, Null, Null, 
                 Null,Null,Null,Null};
               positions = Append[positions, actualPosition]
           ],
           position == "Long",
           If[ #1 == -1,
           		finalIndex = First[#2];
               sellDate = dateStock[[First[#2]]];
               sellPrice = Which[
                 OptionValue["ExitPrices"][[1]] == "Open", open[[First[#2]]],
                 OptionValue["ExitPrices"][[1]] == 
                  "Median", (high[[First[#2]]] + low[[First[#2]]])/2,
                 OptionValue["ExitPrices"][[1]] == "Close", close[[First[#2]]]
                 ];
               sellPriceSlip = Which[
                 OptionValue["ExitPrices"][[2]] == "Low", low[[First[#2]]],
                 OptionValue["ExitPrices"][[2]] == 
                  "Medium", (low[[First[#2]]] + sellPrice)/2
                 ];
               position = "Out";
               dividend=Plus @@ (If[AbsoluteTime[#[[1]]] > AbsoluteTime[buyDate] && 
     			 AbsoluteTime[#[[1]]] < AbsoluteTime[sellDate], #[[2]],0] & /@ div);
               actualPosition = {buyDate, buyPrice, buyPriceSlip, sellDate, 
                 sellPrice, sellPriceSlip,finalIndex-initialIndex+1,dividend,Max[high[[initialIndex;;finalIndex]]],Min[low[[initialIndex;;finalIndex]]]};
               positions = Append[Delete[positions, -1], actualPosition]
           ]
           ] &, (signal[[2]][[signalIndices[[1]];;signalIndices[[2]]]])
         ];
        BackTest[stock,strategy,entryPrices,exitPrices,period,positions]
    ]

BackTest[stock_Stock,strategy_Strategy,entryPrices_List,exitPrices_List,period_List,positions_List][p_String,o:OptionsPattern[]]:=
	Which[
     p === "Properties",
     {"Stock","Strategy","Entry Prices","Exit Prices","Period","Positions","Performance","Report"},
     p === "Stock",
     stock,
     p === "Strategy",
     strategy,
     p === "Entry Prices",
     entryPrices,
     p === "Exit Prices",
     exitPrices,
     p === "Period",
     period,
     p === "Positions",
     positions,
     p == "Performance",
     Performance[period,positions,o],
     p == "Report",
     Report[period,positions,o],
     True,
     Message[BackTest::arg,{"Stock","Strategy","Entry Prices","Exit Prices","Period","Positions","Performance","Report"}]
	];

Format[BackTest[stock_Stock,strategy_Strategy,entryPrices_List,exitPrices_List,period_List,positionList_List]] := 
	Panel[Column[{Row[{stock["Name"],"  with  ",strategy["Name"]}],Row[{ "from  ", DateObject[period[[1]]], "  to  ", DateObject[period[[4]]]}], Length[positionList] " trades"}],Style["BackTest Object",16]];

Options[Performance] = {
  "Slippage" -> False, (* True or False *)
  "Brokerage" -> 0, (* % comission *)
  "Deposit" -> "Simple", (* "Simple"/"Compound" *)
  "RiscFreeRate" -> 0} 

Performance[period_List,positions_List, o : OptionsPattern[]] :=
(* calculated for a fixed investment on each trade, simple interest rate *)
    Module[ {
    (* --- Local variables --- *)
    slippage = OptionValue["Slippage"],
    commission = OptionValue["Brokerage"]/100,
    deposit = OptionValue["Deposit"],
    riscFree = OptionValue["RiscFreeRate"],
    numberOfTrades, totalNumberDays, daysInMarket,
    numberDaysTrade, avgNumberOfDaysPerTrade, 
	netTradeReturns, percentWinTrades, netReturn, 
    annualizedNetReturn,
    avgTrade, profitFactor, recoveryFactor,
    drawDownsList, maxDrawDown,trades,
    netBH, winTrades, lossTrades, avrgProfitTrade, avrgLossTrade,
    payoffRatio, buyHoldIndex, calmarRatio, indexBH, sharpeRatio
    },
    (* Eliminate the last trade if not closed *)
        If[ positions[[-1]][[4]] === Null,
            trades = Delete[positions, -1],
            trades = positions
        ];
        (* ---- Begin common calculations --- *)
        numberOfTrades = Length[trades];
        (* List with the number of days of each trade *)
        numberDaysTrade = 
          Map[(DayCount[#[[1]], #[[4]]]+1) &, trades];
        totalNumberDays = DayCount[period[[1]], 
            trades[[numberOfTrades, 4]]]+1;
        daysInMarket = N@(Plus@@numberDaysTrade/totalNumberDays*100);
        If[slippage,
        	netBH = (period[[6]] (1-commission)+ period[[8]])/(period[[3]] (1+commission))-1,
        	netBH = (period[[5]] (1-commission)+ period[[8]])/(period[[2]] (1+commission))-1
        ];
        If[slippage,
        	netTradeReturns = Map[((#[[6]] (1-commission)+#[[8]])/(#[[3]] (1+commission))-1) &, trades],
        	netTradeReturns = Map[((#[[5]] (1-commission)+#[[8]])/(#[[2]] (1+commission))-1) &, trades]
        ];
        percentWinTrades = N@(Count[netTradeReturns, x_ /; x > 0]/numberOfTrades*100);
        winTrades = Count[netTradeReturns, x_ /; x > 0];
        lossTrades = Count[netTradeReturns, x_ /; x < 0];
        avgNumberOfDaysPerTrade = N[Mean[numberDaysTrade]];
        Which[
	    	deposit === "Simple",(* TODO repasar i contar correctament *)
		        netReturn = Plus@@netTradeReturns;
		        annualizedNetReturn = 100 netReturn 365/totalNumberDays;
		        avgTrade = netReturn/numberOfTrades;
		        avrgProfitTrade = Plus@@Select[netTradeReturns, # > 0 &]/winTrades;
		        avrgLossTrade = -Plus@@Select[netTradeReturns,# < 0 &]/lossTrades;
		        payoffRatio = avrgProfitTrade/avrgLossTrade;
		        buyHoldIndex = netReturn/netBH;
		        profitFactor = (Plus@@Select[netTradeReturns, # > 0 &])/(-Plus@@Select[netTradeReturns,# < 0 &]);
		        drawDownsList = drawDowns[netTradeReturns];
		        maxDrawDown = Min[drawDownsList];
		        recoveryFactor = netReturn/(-maxDrawDown);
		        indexBH = netReturn/netBH;
		        (*weights = 
		        variance = *)
		        sharpeRatio = (annualizedNetReturn - riscFree);
		        calmarRatio = annualizedNetReturn/(-100 maxDrawDown);
                {{"Trades/year",N[numberOfTrades 365/totalNumberDays],StringForm["(`1`,`2`)", numberOfTrades,totalNumberDays/365.]}, 
                {"Exposure (% in Market)",daysInMarket},
                {"Return Average Trade",100 avgTrade}, 
                {"% Winning Trades",percentWinTrades},
                {"Annualized Return",annualizedNetReturn},
                {"Maximum percent Drawdown",-100 maxDrawDown}, 
                {"Payoff Ratio",payoffRatio}, 
                {"Profit Factor",profitFactor},
                {"Recovery Factor",recoveryFactor},
                {"B&H Index", indexBH,StringForm["(`1`,`2`)",netReturn,netBH]},
                {"Sharpe Ratio", sharpeRatio},
                {"Calmar Ratio",calmarRatio} 
                },
   			deposit === "Compound", (* TODO repasar i contar correctament *)
   				netReturn = Times@@(netTradeReturns+1)-1;
   				annualizedNetReturn = 100 ((netReturn+1)^(365/totalNumberDays)-1) ;
   				avgTrade = netReturn/numberOfTrades;
		        profitFactor = (Plus@@Select[netTradeReturns, # > 0 &])/(-Plus@@Select[netTradeReturns,# < 0 &]);
		        drawDownsList = drawDowns[netTradeReturns];
		        maxDrawDown = Min[drawDownsList];
		        recoveryFactor = netReturn/(-maxDrawDown);
                {numberOfTrades,daysInMarket,percentWinTrades,avgNumberOfDaysPerTrade,
                	avgTrade,annualizedNetReturn,profitFactor,maxDrawDown,recoveryFactor}
        ]
    ]
    
drawDowns[ratios_List] :=
    Module[ {i, logics, down, downs},
        logics = Prepend[Map[# < 0 &, ratios], False];
        downs = {};
        MapIndexed[If[ #1 && ! logics[[First[#2]]],
                       i = First[#2];
                       down = 0;
                       While[i + 1 <= Length[logics] && logics[[i + 1]], 
                        down = down + ratios[[i]];
                        i++];
                       downs = Append[downs, down]
                   ] &, Rest[logics]];
        downs
    ]

maxDrawDown[equity_List] :=
    Module[ {maxPath, ddPath, posMax},
        maxPath = Drop[FoldList[Max, 0, equity], 1];
        ddPath = maxPath - equity;
        posMax = First@First@Position[ddPath, Max[ddPath]];
        {Max[ddPath],posMax - First@First@Position[maxPath, maxPath[[posMax]]]}
    ]

Options[Report] = {
  "Brokerage" -> {0, 0.00}, (* minimum commission, % comission *)
  "Slippage" -> False, (* True or False *)
  "MoneyManagement" -> {"SimpleDeposit",100} (*{"CompoundDeposit",100},{"FixedShares",1}*)
  }

Report[period_List,positions_List, o : OptionsPattern[]] :=
    Module[ {
    (* --- Local variables --- *)
    moneyManagement = OptionValue["MoneyManagement"],
    numberOfTrades, totalNumberDays,
    numberDaysTrade, avgNumberOfDaysPerTrade, largestTrade, 
    smallestTrade,
    ratiosTrade, annualizedRatiosTrade,
    numberOfWinTrades, numberOfLosTrades, largestWinTrade, 
    largestLosTrade,
    equity, equityDays, listPositions = trades,
    avgTrade, stdTrade, interval, informationRatio,
    grossProfit, grossLoss, profitFactor,
    drawDownsList, maxDrawDown, avrgDrawDown, stdDrawDown, 
    intervalDD, calmarRatio,
    initialInvest = OptionValue["InitialInvestment"],
    commission = OptionValue["Brokerage"][[2]],
    (*rateOfReference = OptionValue["rateOfReference"],*)
    slippage = OptionValue["Slippage"]
    },
    (* If the last trade is not closed *)
        If[ listPositions[[-1]][[4]] === Null,
        	Which[
        		moneyManagement[[1]]= "SimpleDeposit",
        		equityOpenTrades = (period[[5]]-listPositions[[-1]][[2]])*moneyManagement[[2]]/listPositions[[-1,2]],
            listPositions[[-1]] = Delete[listPositions, -1]
        	]
        ];
        (* ---- Begin common calculations --- *)
        numberOfTrades = Length[listPositions];
        (* List with the number of days of each trade *)
        SetSystemOptions["DataOptions" -> "ReturnQuantities" -> False];
        numberDaysTrade = 
          Map[DateDifference[#[[1]], #[[4]]] &, listPositions];
        totalNumberDays = 
          DateDifference[listPositions[[1, 1]], 
            listPositions[[numberOfTrades, 4]]];
        SetSystemOptions["DataOptions" -> "ReturnQuantities" -> True];
        avgNumberOfDaysPerTrade = N[Mean[numberDaysTrade]];
        largestTrade = Max[numberDaysTrade];
        smallestTrade = Min[numberDaysTrade];
        If[ ! slippage,
          (* List with {{Date},SellPrice/BuyPrice (1-
          commission)^2} for each trade *)
          (* --- Initiating calculations without slippage --- *)
            ratiosTrade = 
              Map[{#[[4]], #[[5]]/#[[2]] (1 - commission)^2} &, 
            listPositions],
            (* --- Initiating calculations with slippage --- *)
            ratiosTrade = 
              Map[{#[[4]], #[[6]]/#[[3]] (1 - commission)^2} &, 
            listPositions]
        ];
        (* --- Computing statistics --- *)
        numberOfWinTrades = Count[ratiosTrade[[All, 2]], x_ /; x > 1];
        numberOfLosTrades = Count[ratiosTrade[[All, 2]], x_ /; x <= 1];
        largestWinTrade = Max[ratiosTrade[[All, 2]]];
        largestLosTrade = Min[ratiosTrade[[All, 2]]];
        avgTrade = Apply[Times, ratiosTrade[[All, 2]]]^(1/numberOfTrades);
        stdTrade = 
          Exp[Sqrt[
              Apply[Plus, Log[ratiosTrade[[All, 2]]/avgTrade]^2]/
                numberOfTrades]];
        interval = {avgTrade/stdTrade, avgTrade stdTrade};
        informationRatio = avgTrade/(interval[[2]] - interval[[1]]);
        grossProfit = 
          Apply[Times, Cases[ratiosTrade[[All, 2]], x_ /; x > 1]];
        grossLoss = 
        Apply[Times, Cases[ratiosTrade[[All, 2]], x_ /; x < 1]];
        profitFactor = grossProfit grossLoss;
        drawDownsList = drawDowns[ratiosTrade[[All, 2]]];
        maxDrawDown = Min[drawDownsList];
        avrgDrawDown = 
        Apply[Times, drawDownsList]^(1/Length[drawDownsList]);
        stdDrawDown = 
          Exp[Sqrt[
              Apply[Plus, Log[drawDownsList/avrgDrawDown]^2]/
                Length[drawDownsList]]];
        intervalDD = {avrgDrawDown/stdDrawDown, avrgDrawDown stdDrawDown};
        calmarRatio = (profitFactor^(365/totalNumberDays) - 1)/
        Abs[maxDrawDown - 1];
        equity = initialInvest FoldList[Times, 1, ratiosTrade[[All, 2]]];

             (* --- Generating report --- *)
        Print[Style["Statistics", 20]];
        Print[
             "Number of Trades: ", numberOfTrades,
             "   Number of days: ", totalNumberDays,
             "   In-market days: ", 
        100 N[Apply[Plus, numberDaysTrade]/totalNumberDays], " %"
         ];
        Print[
             "Average number of days per trade: ", avgNumberOfDaysPerTrade,
             "   Days of the largest trade: ", largestTrade, 
          "   Days of the smallest trade: ", smallestTrade 
         ];
        Print[
             "Winning trades: ", N[100 numberOfWinTrades/numberOfTrades], 
        " %",
          "   Largest winning trade: ", 100 (largestWinTrade - 1), " %",
          "   Largest lossing trade: ", 100 (largestLosTrade - 1), " %"
         ];
        Print[
             "Gross profit: ", grossProfit, 
             "   Gross loss: ", grossLoss, 
          "   Profit factor: ", profitFactor
         ];
        Print[
             Style["Annualized profit factor: ", Bold], 
        Style[100 (profitFactor^(365/totalNumberDays) - 1), Bold], 
        Style[" %", Bold]
         ];
        Print["Average trade: ", avgTrade,
             "   Sigma interval: ", interval,
             "   Information ratio: ", informationRatio
         ];
        Print[
             "Average drawdown: ", avrgDrawDown, 
          "   Sigma interval: ", intervalDD
         ];
        Print[
             Style["Maximum drawdown: ", Bold], 
        Style[100 (maxDrawDown - 1), Bold], Style[" %", Bold]
         ];
        Print[
             Style["Calmar ratio: ", Bold], Style[calmarRatio, Bold]
         ];
        Print[Histogram[numberDaysTrade, 20, LabelingFunction -> Above]];
        annualizedRatiosTrade = 
        ratiosTrade[[All, 2]]^(365/numberDaysTrade);
        Print[ListPlot[
            Transpose[{numberDaysTrade, annualizedRatiosTrade}], 
        PlotRange -> All]];
        equityDays = 
          Prepend[ratiosTrade[[All, 1]], listPositions[[1]][[1]]];
        Print[DateListPlot[Transpose[{equityDays, equity}]]]
    ]


(*drawDowns[ratios_List] :=
    Module[ {i, logics, down, downs},
        logics = Prepend[Map[# < 1 &, ratios(*+1*)], False];
        downs = {};
        MapIndexed[
          If[ #1 && ! logics[[First[#2]]],
              i = First[#2];
              down = 1;
              While[ i + 1 <= Length[logics] && logics[[i + 1]], 
                down = down  (ratios[[i]](*+1*));
                i++];
              downs = Append[downs, down]
          ] &,
          Rest[logics]
          ];
        downs
    ]*)

End[] (* End Private Context *)

EndPackage[]