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
        	dividend,Max[high],Min[low]};
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
     p == "Report",
     Report[stock,strategy,period,positions,o],
     p == "Performance",
     Performance[period,positions,o],
     p == "Bootstrap",
     Bootstrap[positions,o],
     True,
     Message[BackTest::arg,{"Stock","Strategy","Entry Prices","Exit Prices","Period","Positions","Performance","Report","Bootstrap"}]
	];

Format[BackTest[stock_Stock,strategy_Strategy,entryPrices_List,exitPrices_List,period_List,positionList_List]] := 
	Panel[Column[{Row[{stock["Name"],"  with  ",strategy["Name"]}],Row[{ "from  ", DateObject[period[[1]]], "  to  ", DateObject[period[[4]]]}], Length[positionList] " trades"}],Style["BackTest Object",16]];

Options[Bootstrap] = {
  "Brokerage" -> 0
  } 
  
Bootstrap[positions_List, o : OptionsPattern[]] :=
    Module[ {trades},
        If[ positions[[-1]][[4]] === Null,
            trades = Delete[positions, -1],
            trades = positions
        ];
        GenerateDocument["BTBootstrap.nb",{OptionValue["Brokerage"]/100,trades}]
    ]

Options[Report] = {
  "Brokerage" -> 0, (* % commission *)
  "Deposit" -> {"Compound",5000} (* "Simple"/"Compound" *)
  } 

Report[stock_Stock,strategy_Strategy,period_List,positions_List, o : OptionsPattern[]] :=
(* calculated for a fixed investment on each trade, simple interest rate *)
    Module[ {
    (* --- Local variables --- *)
    commission = OptionValue["Brokerage"]/100,
    deposit = OptionValue["Deposit"][[1]],
    principal = OptionValue["Deposit"][[2]],
    numberOfTrades, totalNumberOfDays, daysInMarket,
    numberOfDaysTrade, avgNumberOfDaysPerTrade, 
	netTradeReturns, netReturn, 
    annualizedNetReturn,ddPath,totalCommissions,
    tradesPerformance,winningTrades, lossingTrades,avgTradeReturn,avgProfitTradeReturn,avgLossTradeReturn,
    avgTrade, profitFactor, recoveryFactor,
    mDD, mDDp,trades,netReturnSlip,equitySlip,netProfitSlip,mDDSlip, mDDpSlip,ddPathSlip,grossProfitSlip,grossLossSlip,profitFactorSlip,recoveryFactorSlip,
    netBHReturn, BHProfit,  avgProfitTrade, avgLossTrade,avgDaysTrade,avgDaysProfitTrade,avgDaysLossTrade,
    avgMFE,avgProfitMFE,avgLossMFE,avgMAE,avgProfitMAE,avgLossMAE,avgEficiency,avgEficiencySlip,
    payoffRatio, marRatio, sharpeTrades,mcwt,mclt,
    daysTradeReturns,commissionTrade,netTradeReturnsSlip,
    netProfit,grossProfit,grossLoss,performance, equity, daysEquity,
    slippage,winningTradesSlip,avgTradeSlip,avgTradeReturnSlip,lossingTradesSlip,payoffRatioSlip,
    mfe,mae,win,
    numForm = ToString@NumberForm[#,{10,2},ExponentFunction -> (If[-10 < # < 10, Null, #] &)]&
    },
    (* Eliminate the last trade if not closed *)
    If[ positions[[-1]][[4]] === Null,
        trades = Delete[positions, -1],
        trades = positions
    ];
    (* ---- Begin common calculations --- *)
    numberOfTrades = Length[trades];
    numberOfDaysTrade = Map[(DayCount[#[[1]], #[[4]]]+1) &, trades];
    totalNumberOfDays = DayCount[period[[1]],trades[[numberOfTrades, 4]]]+1;
    daysInMarket = N@(Plus@@numberOfDaysTrade/totalNumberOfDays*100);
   	netBHReturn = (period[[5]] (1-commission)+ period[[8]])/(period[[2]] (1+commission))-1;
    netTradeReturns = Map[((#[[5]] (1-commission)+#[[8]])/(#[[2]] (1+commission))-1) &, trades];
    daysTradeReturns = #[[4]]&/@trades;
    commissionTrade = Map[commission/(commission+1)(1+#[[5]]/#[[2]])&,trades];
    winningTrades = Count[netTradeReturns, x_ /; x > 0];
    lossingTrades = Count[netTradeReturns, x_ /; x < 0];
    avgNumberOfDaysPerTrade = N[Mean[numberOfDaysTrade]];
	
    Which[
    	deposit === "Simple",(* TODO repasar i contar correctament *)
    		netReturn = Plus@@netTradeReturns;
    		equity = Prepend[principal Accumulate[netTradeReturns],0];
   		    totalCommissions = Plus@@(principal commissionTrade);
   		    equity = Delete[Riffle[equity,equity],-1]; (* Dates entry and exit trades *)
   		    daysEquity = Join[{period[[1]]},Flatten[{#[[1]],#[[4]]}&/@trades,1]];
   		    netProfit = principal netReturn;
    		grossProfit = Plus@@Select[Differences[equity], # > 0 &];
    		grossLoss = -Plus@@Select[Differences[equity],# < 0 &];
	        {mDD, mDDp,ddPath}= drawDown[equity];
	        profitFactor = grossProfit/grossLoss;
	        recoveryFactor = netProfit/mDD;
	        annualizedNetReturn = 100 (principal netReturn)/(principal-Min[equity]) 365/totalNumberOfDays;
	        marRatio = annualizedNetReturn/mDDp;
	        BHProfit = principal netBHReturn;
    		
    		performance = {
    			{
	    			{"Net Profit",Row[{numForm[netProfit],"(",numForm[100 netReturn],"%)"}]},
	    			{"Gross Profit",Row[{numForm[grossProfit],"(",numForm[100 grossProfit/principal],"%)"}]},
	    			{"Gross Loss",Row[{numForm[grossLoss],"(",numForm[100 grossLoss/principal],"%)"}]},
	    			{"Maximum Drawdown",Row[{numForm[mDD],"(",numForm[mDDp],"%)"}]},
	    			{"Profit Factor",numForm[profitFactor]},
	    			{"Recovery Factor",numForm[recoveryFactor]}
    			},
    			{
    				{"Annual Average Return",Row[{numForm[annualizedNetReturn],"%"}]},
    				{"Days in Market",Row[{numForm[daysInMarket],"%"}]},
    				{"Risc Adjusted Return",Row[{numForm[100 annualizedNetReturn/daysInMarket],"%"}]},
    				{"Mar Ratio",numForm[marRatio]},
    				{"Total commissions", numForm[totalCommissions]},
    				{"Commission Index", numForm[totalCommissions/(totalCommissions+netProfit)]}
    			},
    			{
    				{"Buy & Hold Profit", Row[{numForm[BHProfit],"(",numForm[100 netBHReturn],"%)"}]},
    				{"Annual Average BH Return", Row[{numForm[100 netBHReturn 365/totalNumberOfDays],"%"}]},
    				{"Buy & Hold Index", numForm[(annualizedNetReturn-100 netBHReturn 365/totalNumberOfDays)/Abs[100 netBHReturn 365/totalNumberOfDays]]}
    			}
    		};

    		avgTradeReturn = Mean[netTradeReturns];
		    avgTrade = netProfit/numberOfTrades;
		    avgProfitTradeReturn = Plus@@Select[netTradeReturns, # > 0 &]/winningTrades;
			avgProfitTrade = grossProfit/winningTrades;
			avgLossTradeReturn = Plus@@Select[netTradeReturns, # < 0 &]/lossingTrades;
			avgLossTrade = grossLoss/lossingTrades;
			payoffRatio = avgProfitTrade/avgLossTrade;
			avgDaysTrade = N[Mean[numberOfDaysTrade]];
			avgDaysProfitTrade = N@Mean[Extract[numberOfDaysTrade, Position[netTradeReturns, x_ /; x > 0]]];
			avgDaysLossTrade = N@Mean[Extract[numberOfDaysTrade, Position[netTradeReturns, x_ /; x < 0]]];
			avgMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@trades];
			avgProfitMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x > 0]]];
			avgLossMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x < 0]]];
			avgMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@trades];
			avgProfitMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x > 0]]];
			avgLossMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x < 0]]];
			avgEficiency = 100 Mean[(#[[5]]-#[[2]])/(#[[9]]-#[[10]])&/@trades];
			sharpeTrades = Mean[netTradeReturns]/StandardDeviation[netTradeReturns];
			mcwt = Max@Differences@Flatten@Position[If[# <= 0, 1, 0] & /@ netTradeReturns, 1] - 1;
			mclt = Max@Differences@Flatten@Position[If[# >= 0, 1, 0] & /@ netTradeReturns, 1] - 1;
    		tradesPerformance = {
    			{
	    			{"Number of Trades",numberOfTrades},
	    			{"Winning Trades",Row[{winningTrades,"(",numForm[100. winningTrades/numberOfTrades],"%)"}]},
	    			{"Lossing Trades",Row[{lossingTrades,"(",numForm[100. lossingTrades/numberOfTrades],"%)"}]},
	    			{"Average Trades/Year",numForm[365. numberOfTrades/totalNumberOfDays]},
	    			{"Sharpe Ratio of Trades",numForm[sharpeTrades]}
    			},
    			{
	    			{"Average Trade", Row[{numForm[avgTrade],"(",numForm[100 avgTradeReturn],"%)"}]},
	    			{"Average Profit Trade",Row[{avgProfitTrade,"(",numForm[100. avgProfitTradeReturn],"%)"}]},
	    			{"Largest Profit Trade Return",Row[{numForm[100. Max[netTradeReturns]],"%"}]},
	    			{"Average Loss Trade",Row[{avgLossTrade,"(",numForm[- 100. avgLossTradeReturn],"%)"}]},
	    			{"Largest Loss Trade Return",Row[{numForm[-100. Min[netTradeReturns]],"%"}]},
	    			{"Payoff Ratio",numForm[payoffRatio]}
    			},
    			{
    				{"Average Days Trade",numForm[avgDaysTrade]},
    				{"Average Days Profit Trade",numForm[avgDaysProfitTrade]},
    				{"Average Days Loss Trade",numForm[avgDaysLossTrade]},
    				{"Average Eficiency",Row[{numForm[avgEficiency],"%"}]},
    				{"Max. consecutive win trades",mcwt},
    				{"Max. consecutive loss trades",mclt}
    			},
    			{
    				{"Average MFE",Row[{numForm[avgMFE],"%"}]},
    				{"Average Profit MFE",Row[{numForm[avgProfitMFE],"%"}]},
    				{"Average Loss MFE",Row[{numForm[avgLossMFE],"%"}]},
    				{"Average MAE",Row[{numForm[avgMAE],"%"}]},
    				{"Average Profit MAE",Row[{numForm[avgProfitMAE],"%"}]},
    				{"Average Loss MAE",Row[{numForm[avgLossMAE],"%"}]}
    			}
    		};	        

    		netTradeReturnsSlip = Map[((#[[6]] (1-commission)+#[[8]])/(#[[3]] (1+commission))-1) &, trades];
    		netReturnSlip = Plus@@netTradeReturnsSlip;
	        equitySlip = Prepend[principal Accumulate[netTradeReturnsSlip],0];
	        equitySlip = Delete[Riffle[equitySlip,equitySlip],-1];
	        netProfitSlip = principal netReturnSlip;
	        {mDDSlip, mDDpSlip,ddPathSlip}= drawDown[equitySlip];
	        grossProfitSlip = Plus@@Select[Differences[equitySlip], # > 0 &];
    		grossLossSlip = -Plus@@Select[Differences[equitySlip],# < 0 &];
    		profitFactorSlip = grossProfitSlip/grossLossSlip;
	        recoveryFactorSlip = netProfitSlip/mDDSlip;
    		winningTradesSlip = N@(Count[netTradeReturnsSlip, x_ /; x > 0]);
    		lossingTradesSlip = N@(Count[netTradeReturnsSlip, x_ /; x < 0]);
    		avgTradeSlip = netProfitSlip/numberOfTrades;
    		avgTradeReturnSlip = Mean[netTradeReturnsSlip];
    		payoffRatioSlip = (grossProfitSlip/winningTradesSlip)/(grossLossSlip/lossingTradesSlip);
    		avgEficiencySlip = 100 Mean[(#[[6]]-#[[3]])/(#[[9]]-#[[10]])&/@trades];
    		slippage = {
    			{
    				{"Net profit",Row[{numForm[netProfitSlip],"(",numForm[100 netReturnSlip],"%)"}]},
    				{"Maximum Drawdown",Row[{numForm[mDDSlip],"(",numForm[mDDpSlip],"%)"}]},
	    			{"Profit Factor",numForm[profitFactorSlip]},
	    			{"Recovery Factor",numForm[recoveryFactorSlip]},
	    			{"Average Trade",  Row[{numForm[avgTradeSlip],"(",numForm[100 avgTradeReturnSlip],"%)"}]},
	    			{"Winning Trades",Row[{winningTradesSlip,"(",numForm[100. winningTradesSlip/numberOfTrades],"%)"}]},
	    			{"Payoff Ratio",numForm[payoffRatioSlip]},
	    			{"Average Eficiency",Row[{numForm[avgEficiencySlip],"%"}]}
    			}
    		};
    		
    		mfe = (#[[9]]/#[[2]]-1)&/@trades; 
    		mae = (#[[10]]/#[[2]]-1)&/@trades; 
    		win = ReplaceAll[netTradeReturns,{x_ /; x >= 0 -> Blue, x_ /; x < 0 -> Red}];
    		
    		GenerateDocument["BTTemplate.nb",{{stock["Name"],strategy["Name"],strategy["Parameters"],period[[1]],period[[4]],commission,deposit,principal},{performance,tradesPerformance,slippage},
    			{Transpose@{daysEquity,equity},Transpose@{daysEquity,equitySlip},Transpose@{daysEquity,ddPath}},{numberOfDaysTrade,netTradeReturns,mfe,mae,win},stock["OHLCV"],trades}],

   		deposit === "Compound", (* TODO repasar i contar correctament *)
   		    netReturn = Times@@(netTradeReturns+1)-1;
   		    equity = principal FoldList[Times,1,netTradeReturns+1]; (* Dates exit trades*)
   		    totalCommissions = Plus@@(equity[[2;;]] commissionTrade);
   		    equity = Delete[Riffle[equity,equity],-1]; (* Dates entry and exit trades *)
   		    daysEquity = Join[{period[[1]]},Flatten[{#[[1]],#[[4]]}&/@trades,1]];
   		    netProfit = principal netReturn;
    		grossProfit = Plus@@Select[Differences[equity], # > 0 &];
    		grossLoss = -Plus@@Select[Differences[equity],# < 0 &];
	        {mDD, mDDp,ddPath}= drawDown[equity];
	        profitFactor = grossProfit/grossLoss;
	        recoveryFactor = netProfit/mDD;
	        annualizedNetReturn = 100 ((netReturn+1)^(365/totalNumberOfDays)-1) ;
	        marRatio = annualizedNetReturn/mDDp;
	        BHProfit = principal netBHReturn;
	        	        
    		performance = {
    			{
	    			{"Net Profit",Row[{numForm[netProfit],"(",numForm[100 netReturn],"%)"}]},
	    			{"Gross Profit",Row[{numForm[grossProfit],"(",numForm[100 grossProfit/principal],"%)"}]},
	    			{"Gross Loss",Row[{numForm[grossLoss],"(",numForm[100 grossLoss/principal],"%)"}]},
	    			{"Maximum Drawdown",Row[{numForm[mDD],"(",numForm[mDDp],"%)"}]},
	    			{"Profit Factor",numForm[profitFactor]},
	    			{"Recovery Factor",numForm[recoveryFactor]}
    			},
    			{
    				{"Annual Average Return",Row[{numForm[annualizedNetReturn],"%"}]},
    				{"Days in Market",Row[{numForm[daysInMarket],"%"}]},
    				{"Risc Adjusted Return",Row[{numForm[100 annualizedNetReturn/daysInMarket],"%"}]},
    				{"Mar Ratio",numForm[marRatio]},
    				{"Total commissions", numForm[totalCommissions]},
    				{"Commission Index", numForm[totalCommissions/(totalCommissions+netProfit)]}
    			},
    			{
    				{"Buy & Hold Profit", Row[{numForm[BHProfit],"(",numForm[100 netBHReturn],"%)"}]},
    				{"Annual Average BH Return", Row[{numForm[100 ((netBHReturn+1)^(365/totalNumberOfDays)-1)],"%"}]},
    				{"Buy & Hold Index", numForm[(annualizedNetReturn-100 ((netBHReturn+1)^(365/totalNumberOfDays)-1))/Abs[100 ((netBHReturn+1)^(365/totalNumberOfDays)-1)]]}
    			}
    		};
    		
    		avgTradeReturn = (netReturn+1)^(1/numberOfTrades)-1;
		    avgTrade = netProfit/numberOfTrades;
		    avgProfitTradeReturn = (Times@@(Select[netTradeReturns, # > 0 &]+1))^(1/winningTrades)-1;
			avgProfitTrade = grossProfit/winningTrades;
			avgLossTradeReturn = (Times@@(Select[netTradeReturns, # < 0 &]+1))^(1/lossingTrades)-1;
			avgLossTrade = grossLoss/lossingTrades;
			payoffRatio = avgProfitTrade/avgLossTrade;
			avgDaysTrade = N[Mean[numberOfDaysTrade]];
			avgDaysProfitTrade = N@Mean[Extract[numberOfDaysTrade, Position[netTradeReturns, x_ /; x > 0]]];
			avgDaysLossTrade = N@Mean[Extract[numberOfDaysTrade, Position[netTradeReturns, x_ /; x < 0]]];
			avgMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@trades];
			avgProfitMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x > 0]]];
			avgLossMFE = 100 Mean[(#[[9]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x < 0]]];
			avgMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@trades];
			avgProfitMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x > 0]]];
			avgLossMAE = 100 Mean[(#[[10]]/#[[2]]-1)&/@Extract[trades, Position[netTradeReturns, x_ /; x < 0]]];
			avgEficiency = 100 Mean[(#[[5]]-#[[2]])/(#[[9]]-#[[10]])&/@trades];
			sharpeTrades = Mean[netTradeReturns]/StandardDeviation[netTradeReturns];
			mcwt = Max@Differences@Flatten@Position[If[# <= 0, 1, 0] & /@ netTradeReturns, 1] - 1;
			mclt = Max@Differences@Flatten@Position[If[# >= 0, 1, 0] & /@ netTradeReturns, 1] - 1;
    		tradesPerformance = {
    			{
	    			{"Number of Trades",numberOfTrades},
	    			{"Winning Trades",Row[{winningTrades,"(",numForm[100. winningTrades/numberOfTrades],"%)"}]},
	    			{"Lossing Trades",Row[{lossingTrades,"(",numForm[100. lossingTrades/numberOfTrades],"%)"}]},
	    			{"Average Trades/Year",numForm[365. numberOfTrades/totalNumberOfDays]},
	    			{"Sharpe Ratio of Trades",numForm[sharpeTrades]}
    			},
    			{
	    			{"Average Trade", Row[{numForm[avgTrade],"(",numForm[100 avgTradeReturn],"%)"}]},
	    			{"Average Profit Trade",Row[{avgProfitTrade,"(",numForm[100. avgProfitTradeReturn],"%)"}]},
	    			{"Largest Profit Trade Return",Row[{numForm[100. Max[netTradeReturns]],"%"}]},
	    			{"Average Loss Trade",Row[{avgLossTrade,"(",numForm[- 100. avgLossTradeReturn],"%)"}]},
	    			{"Largest Loss Trade Return",Row[{numForm[-100. Min[netTradeReturns]],"%"}]},
	    			{"Payoff Ratio",numForm[payoffRatio]}
    			},
    			{
    				{"Average Days Trade",numForm[avgDaysTrade]},
    				{"Average Days Profit Trade",numForm[avgDaysProfitTrade]},
    				{"Average Days Loss Trade",numForm[avgDaysLossTrade]},
    				{"Average Eficiency",Row[{numForm[avgEficiency],"%"}]},
    				{"Max. consecutive win trades",mcwt},
    				{"Max. consecutive loss trades",mclt}
    			},
    			{
    				{"Average MFE",Row[{numForm[avgMFE],"%"}]},
    				{"Average Profit MFE",Row[{numForm[avgProfitMFE],"%"}]},
    				{"Average Loss MFE",Row[{numForm[avgLossMFE],"%"}]},
    				{"Average MAE",Row[{numForm[avgMAE],"%"}]},
    				{"Average Profit MAE",Row[{numForm[avgProfitMAE],"%"}]},
    				{"Average Loss MAE",Row[{numForm[avgLossMAE],"%"}]}
    			}
    		};
    		
    		netTradeReturnsSlip = Map[((#[[6]] (1-commission)+#[[8]])/(#[[3]] (1+commission))-1) &, trades];
    		netReturnSlip = Times@@(netTradeReturnsSlip+1)-1;
	        equitySlip = principal FoldList[Times,1,netTradeReturnsSlip+1];
	        equitySlip = Delete[Riffle[equitySlip,equitySlip],-1];
	        netProfitSlip = principal netReturnSlip;
	        {mDDSlip, mDDpSlip,ddPathSlip}= drawDown[equitySlip];
	        grossProfitSlip = Plus@@Select[Differences[equitySlip], # > 0 &];
    		grossLossSlip = -Plus@@Select[Differences[equitySlip],# < 0 &];
    		profitFactorSlip = grossProfitSlip/grossLossSlip;
	        recoveryFactorSlip = netProfitSlip/mDDSlip;
    		winningTradesSlip = N@(Count[netTradeReturnsSlip, x_ /; x > 0]);
    		lossingTradesSlip = N@(Count[netTradeReturnsSlip, x_ /; x < 0]);
    		avgTradeSlip = netProfitSlip/numberOfTrades;
    		avgTradeReturnSlip = (netReturnSlip+1)^(1/numberOfTrades)-1;
    		payoffRatioSlip = (grossProfitSlip/winningTradesSlip)/(grossLossSlip/lossingTradesSlip);
    		avgEficiencySlip = 100 Mean[(#[[6]]-#[[3]])/(#[[9]]-#[[10]])&/@trades];
    		slippage = {
    			{
    				{"Net profit",Row[{numForm[netProfitSlip],"(",numForm[100 netReturnSlip],"%)"}]},
    				{"Maximum Drawdown",Row[{numForm[mDDSlip],"(",numForm[mDDpSlip],"%)"}]},
	    			{"Profit Factor",numForm[profitFactorSlip]},
	    			{"Recovery Factor",numForm[recoveryFactorSlip]},
	    			{"Average Trade",  Row[{numForm[avgTradeSlip],"(",numForm[100 avgTradeReturnSlip],"%)"}]},
	    			{"Winning Trades",Row[{winningTradesSlip,"(",numForm[100. winningTradesSlip/numberOfTrades],"%)"}]},
	    			{"Payoff Ratio",numForm[payoffRatioSlip]},
	    			{"Average Eficiency",Row[{numForm[avgEficiencySlip],"%"}]}
    			}
    		};
    		
    		mfe = (#[[9]]/#[[2]]-1)&/@trades; 
    		mae = (#[[10]]/#[[2]]-1)&/@trades; 
    		win = ReplaceAll[netTradeReturns,{x_ /; x >= 0 -> Blue, x_ /; x < 0 -> Red}];
    		
    		GenerateDocument["BTTemplate.nb",{{stock["Name"],strategy["Name"],strategy["Parameters"],period[[1]],period[[4]],commission,deposit,principal},{performance,tradesPerformance,slippage},
    			{Transpose@{daysEquity,equity},Transpose@{daysEquity,equitySlip},Transpose@{daysEquity,ddPath}},{numberOfDaysTrade,netTradeReturns,mfe,mae,win},stock["OHLCV"],trades}]

        ]
    ]
    
drawDown[equity_List]:=Module[{maxPath,ddPath,maxdd,pos,maxddpc},
	maxPath = Drop[FoldList[Max, 0, equity], 1];
	ddPath = maxPath - equity;
	maxdd = Max[ddPath];
	pos = Flatten@Position[ddPath, maxdd];
	maxddpc = 100 Max[(maxdd/maxPath[[#]]) & /@ pos];
	{maxdd,maxddpc,ddPath}
	]


Options[Performance] = {
  "Output" -> "List"
  } 

Performance[period_List,positions_List, o : OptionsPattern[]] :=
(* netReturn, mDDp, profitFactor, recoveryFactor, avgTradeReturn, winningTrades, payoffRatio, sharpeRatio, avgEficiency *)
    Module[ {
    (* --- Local variables --- *)
    numberOfTrades, netTradeReturns, netReturn, equity, mDD,mDDp,ddPath,
    recoveryFactor,sharpeRatio,
      avgTradeReturn,avgEficiency,
    trades,winningTrades,payoffRatio,
    grossProfit, grossLoss, profitFactor
    
    },
    (* If the last trade is not closed *)
        If[ positions[[-1]][[4]] === Null,
            trades = Delete[positions, -1],
            trades = positions
        ];
        numberOfTrades = Length[trades];
        netTradeReturns = Map[((#[[5]]+#[[8]])/#[[2]] - 1) &, trades];
        netReturn = Times@@(netTradeReturns+1)-1;
        equity = FoldList[Times,1,netTradeReturns+1];
        {mDD, mDDp,ddPath} = drawDown[equity];
        grossProfit = Plus@@Select[Differences[equity], # > 0 &];
        grossLoss = -Plus@@Select[Differences[equity],# < 0 &];
        profitFactor = grossProfit/grossLoss;
        recoveryFactor = netReturn/mDD;
        avgTradeReturn = (netReturn+1)^(1/numberOfTrades)-1;
        winningTrades = Count[netTradeReturns, x_ /; x > 0];
        sharpeRatio = Mean[netTradeReturns]/StandardDeviation[netTradeReturns];
        avgEficiency = 100 Mean[(#[[5]]-#[[2]])/(#[[9]]-#[[10]])&/@trades];
        payoffRatio = (grossProfit/winningTrades)/(grossLoss/(numberOfTrades-winningTrades));
        If[ OptionValue["Output"]==="Table",
            TableForm[{{"Net Return","MDD","Profit Factor","Recovery Factor","Average Trade Return","Winning Trades","Payoff Ratio","Sharpe Ratio","Average Eficiency"},
            {netReturn, mDDp, profitFactor, recoveryFactor, avgTradeReturn,N@winningTrades/numberOfTrades, payoffRatio, sharpeRatio, avgEficiency}}],
            {netReturn, mDDp, profitFactor, recoveryFactor, avgTradeReturn, N@winningTrades/numberOfTrades, payoffRatio, sharpeRatio, avgEficiency}
        ]
    ]


End[] (* End Private Context *)

EndPackage[]