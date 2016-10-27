(* Mathematica Package *)

BeginPackage["EmbeddingAnalysis`"]
(* Exported symbols added here with SymbolName::usage *)
  
phasePortraitManipulation::usage = "phasePortraitManipulation[data] computes and represents the bidimensional phase portrait
of the time series data for different time delays
Input: Time series to represent
Output: Phase portrait for a defined time delay"

phasePortrait3DManipulation::usage = "phasePortrait3DManipulation[data] computes the tridimensional phase 
portrait of the time series data for different time delays."

falseNearestNeighbors::usage = "falseNearestNeighbors[data,\[Tau],mMax,T1,T2] computes for a given time series data 
and a given time delay \[Tau] the fraction of false nearest neighbors for each embedding dimensions 1, 2,...,mMax.
T1 and T2 are thresholds. Normally 10<=T1<=30 (T1=15 is a good starting point) and T2=2 is the other threshold."

averageMutualInformation::usage = "averageMutualInformation[ts,tauMax, binWidth] calculates the average mutual information between Subscript[ts, t] and Subscript[ts, t+\[Tau]] of data when \[Tau] gets values from 0 to \[Tau]Max. In calculating the frequencies, the width of the bins is binWidth. "

entropyH::usage = "entropyH[x_List] computes entropy of time series x by the histogram method
entropyH[x_List,y_List] computes de conjoint entropy of the time series x and y by the histogram method"

mutualInformationH::usage = "mutualInformationH[x_List,y_List] computes the mutual information between time series x and y by the histogram method"

distanceMutualInformationH::usage = "distanceMutualInformationH[x_List,y_List] computes the normalized distance between time series x and y by the histogram method"

TStepPrediction::usage = "TStepPrediction[data_,T_,\[Tau]_,tmk_]."

TStepPredictionList::usage = "TStepPredictionList[data_,newData_,T_,\[Tau]_,tmk_]."

singularSpectrumAnalysis::usage = "singularSpectrumAnalysis[x_List,n_Integer] computes the ssa of time series x for an embedding dimension n
singularSpectrumAnalysis[x_List,n_Integer,r_Integer] computes the first r components of the ssa of time series x for an embedding dimension n"

ssaPlotEigenvalues::usage = "ssaPlotEigenvalues[ssa_List,n_Integer] takes the output of ssa and plots the first n Log[eigenvalues]"

ssaPlotComponents::usage = "ssaPlotComponents[ssa_List,nc_Integer]"

ssaPlotResiduals::usage = "ssaPlotResiduals[ssa_List,ts_List,n_Integer] takes the output of ssa and the original time series ts and plots the residuals of the n first components of the ssa"

ssaPlotPairVectors::usage = "ssaPlotPairVectors[ssa_List,n_Integer] takes the output of ssa and plots the pairs of relations between the first n vector components"

ssaSmoothing::usage = "ssaSmoothing[ssa_List,n_Integer] takes the output of ssa and computes the time series of the first n components"

ssaRForecasting::usage = "ssaRForecasting[ssa_List,n_Integer,t_Integer] takes the output of ssa and computes the time series of the first n components forecasted t time steps"

ssaVForecasting::usage = "ssaVForecasting[ssa_List,n_Integer,t_Integer] takes the output of ssa and computes the time series of the first n components forecasted t time steps"

ssawCorrelation::usage = "ssawCorrelation[ssa_List,m_Integer] takes the output of ssa and plots the correlation matrix between the first m components"

recurrenceMatrix::usage = "recurrenceMatrix[ts,tau,m,eps,n] computes recurrence matrix"



Begin["`Private`"] (* Begin Private Context *) 

phasePortraitManipulation[data_] :=
    Manipulate[
        ListPlot[{Drop[data,-\[Tau]],Drop[data,\[Tau]]}\[Transpose],
            AspectRatio->Automatic,
            PlotStyle->AbsolutePointSize[ps]
        ],
        {{\[Tau],1},0,20,1,Appearance->"Labeled"},
        {{ps,1,"point size"},{0.5,1,2}},
        SaveDefinitions->True
    ]

phasePortrait3DManipulation[data_] :=
    Manipulate[
        Module[ {emb},
            emb = Table[Take[data,{i,i+2\[Tau],\[Tau]}],{i,Length[data]-2\[Tau]}];
            ListPointPlot3D[emb,BoxRatios->Automatic,PlotStyle->AbsolutePointSize[ps]]
        ],
        {{\[Tau],1},1,20,1,Appearance->"Labeled"},
        {{ps,1.6,"point size"},{1,1.6,2,2.4}},
        SaveDefinitions->True
    ]
    
entropyH[x_List] :=
    Module[ {p},
        p = Last@HistogramList[x,Automatic,"Probability"];
        - Plus@@(p Log[p]/.Indeterminate-> 0)
    ]

entropyH[x_List,y_List] :=
    Module[ {p},
        p = Last@HistogramList[Transpose[{x,y}],Automatic,"Probability"];
        -Plus@@(Apply[Plus,#]&/@(p (Log[p]/.Indeterminate->0)))
    ]

mutualInformationH[x_List,y_List] :=
    Module[ {p,exy,px,ex,py,ey},
        p = Last@HistogramList[Transpose[{x,y}],Automatic,"Probability"];
        exy = -Plus@@(Apply[Plus,#]&/@(p (Log[p]/.Indeterminate->0)));
        px = (Plus@@#)&/@p;
        ex = - Plus@@(px Log[px]/.Indeterminate-> 0);
        py = (Plus@@#)&/@(Transpose@p);
        ey = - Plus@@(py Log[py]/.Indeterminate-> 0);
        ex+ey-exy
    ]

distanceMutualInformationH[x_List,y_List] :=
    Module[ {p,exy,px,ex,py,ey},
        p = Last@HistogramList[Transpose[{x,y}],Automatic,"Probability"];
        exy = -Plus@@(Apply[Plus,#]&/@(p (Log[p]/.Indeterminate->0)));
        px = (Plus@@#)&/@p;
        ex = - Plus@@(px Log[px]/.Indeterminate-> 0);
        py = (Plus@@#)&/@(Transpose@p);
        ey = - Plus@@(py Log[py]/.Indeterminate-> 0);
        2-(ex+ey)/exy
    ]



averageMutualInformation[data_, \[Tau]Max_, binWidth_] :=
    Module[ {iter, n = Length[data], pi, pij},
        Table[iter = {Min[data], Max[data] + binWidth, binWidth};(* TODO test ParallelTable *)
              pi = N[BinCounts[data, iter]/n];
              pij = N[
                BinCounts[{Drop[data, -\[Tau]], Drop[data, \[Tau]]}\[Transpose], 
                  iter, iter]/(n - \[Tau])];
              {\[Tau], 
               Total[DeleteCases[Flatten[pij Log2[pij/KroneckerProduct[pi, pi]]],
                  Indeterminate | \[Infinity]]]}, {\[Tau], 0, \[Tau]Max}]
    ]
  
falseNearestNeighbors[data_, t_, mMax_, T1_, T2_]:=
	Module[{emb, m, n, s, count, nf, x, yPos, y, Rm, incr},
		emb=Table[Table[Take[data,{i,i+t*(m-1),t}],{i, Length[data]-t*(m-1)}],{m, mMax+1}];
		n=Length/@emb;
		s=N@StandardDeviation[data];
		Table[ (* TODO test ParallelTable *)
			count=0;
			nf=Nearest[emb[[m]]->Range[n[[m]]]];
			Do[
				x=emb[[m,i]];
				yPos=Last[nf[x,2]];
				If[yPos<=n[[m]]-t,y=emb[[m,yPos]],Continue[]];
				Rm=EuclideanDistance[x,y];
				incr=Abs[Last[emb[[m+1,i]]]-Last[emb[[m+1,yPos]]]];
				Quiet@If[incr/Rm>T1||incr/s>T2,count=count+1],
				{i,n[[m+1]]}
			];
			{m,N[count/n[[m+1]]]},
			{m,mMax}
		]
	]

recurrenceMatrix[data_List,t_Integer,m_Integer,eps_Real,n_Integer]:=
	Module[{emb,i,j},
		emb=Table[Take[data,{i,i+t*(m-1),t}],{i, Length[data]-t*(m-1)}];
		Table[1 - UnitStep[eps - Norm[emb[[i]] - emb[[j]]]],{i, 1, n},{j, 1, n}]
	]

oneStepPrediction[emb_,startingValues_,\[Tau]_,{m_,k_}]:=
	Module[{mEmb=Most[emb],startingPoint,neighborIndices,neigh,futureValues,fitData,xx,x,fit},
		startingPoint=Take[startingValues,{1,\[Tau](m-1)+1,\[Tau]}];
		neighborIndices=Nearest[mEmb->Range[Length[mEmb]],startingPoint,k];
		neigh=mEmb[[neighborIndices]];
		futureValues=emb[[neighborIndices+1]];
		fitData=Join[neigh\[Transpose],{futureValues[[All,m]]}]\[Transpose];
		xx=Array[x,m];
		fit=Fit[fitData,Join[{1},xx],xx];
		fit/.Thread[xx->startingPoint]
	]

TStepPrediction[data_,T_,\[Tau]_,tmk_]:=
	Module[{s,m,k,emb,startingValues,pred={},nextPred,dat=data},
		Do[
			{s,m,k}=tmk[[t]];
			emb=Table[Take[dat,{i,i+\[Tau](m-1),\[Tau]}],{i,Length[dat]-\[Tau](m-1)}];
			startingValues=Take[dat,-(\[Tau](m-1)+1)];
			nextPred=oneStepPrediction[emb,startingValues,\[Tau],{m,k}];
			pred={pred,nextPred};
			dat=Append[dat,nextPred],
			{t,1,T}
		];
		Flatten@pred
	]

TStepPredictionList[data_,newData_,T_,\[Tau]_,tmk_]:=
	Module[{comb=Join[data,newData],nd=Length[data],nnd=Length[newData]},
		Table[Last@TStepPrediction[Take[comb,j],T,\[Tau],tmk],{j,nd,nd+nnd-T}]
	]

NRMSE1[predictions_,newData_,T_]:=
	With[{nnd=Length[newData]},
		Sqrt[(nnd-T+1)/(nnd-T)] RootMeanSquare[Drop[newData,T-1]-predictions]/StandardDeviation[Drop[newData,T-1]]
	]

NRMSE2[data_,newData_,T_,\[Tau]_,tmk_]:=
	Module[{comb=Join[data,newData],pr,nd=Length[data],nnd=Length[newData]},
		pr=ParallelTable[Last[TStepPrediction[Take[comb,j],T,\[Tau],tmk]],{j,nd,nd+nnd-T}];
		NRMSE1[pr,newData,T]
	]

showNRMSE[nrmse_,mk_,opts___]:=
	Module[{mm,pp,m,k,p1,p2},
		mm=Min[#[[2]]&/@nrmse];
		pp=Position[nrmse,mm][[1]];
		If[mk!={},
			m=mk[[1]];
			k=mk[[2]];
			ListLinePlot[#[[2]]&/@nrmse,Mesh->All,AxesLabel->{Style["k",Italic],"NRMSE"},
				Epilog->{Red,PointSize[Medium],Point[nrmse[[Sequence@@Most[pp]]]],Blue,Point[nrmse[[p1=Position[nrmse,m,2][[1,1]],2,p2=Position[nrmse[[p1,2]],k][[1,1]]]]]},
				PlotLabel->Row[{"The smallest NRMSE is ",mm," with ",Style["m",Italic]," = ",nrmse[[pp[[1]],1]]," and ",Style["k",Italic]," = ",pp[[3]]+nrmse[[pp[[1]],2,1,1]]-1,"\nThe NRMSE is ",nrmse[[p1,2,p2,2]]," with ",Style["m",Italic]," = ",m," and ",Style["k",Italic]," = ",k}],
				opts],
			ListLinePlot[#[[2]]&/@nrmse,Mesh->All,AxesLabel->{"k","NRMSE"},
				Epilog->{Red,PointSize[Medium],Point[nrmse[[Sequence@@Most[pp]]]]},PlotLabel->Column[{Row[{"The smallest NRMSE is ",mm," with ",Style["m",Italic]," = ",nrmse[[pp[[1]],1]]," and ",Style["k",Italic]," = ",pp[[3]]+nrmse[[pp[[1]],2,1,1]]-1}]}],
				opts]
		]
	]

showPredictions[data_,start_,steps_,\[Tau]_,tmk_]:=
	Module[{predicted,observed},
		predicted=TStepPrediction[Take[data,start],steps,\[Tau],tmk];
		observed=Take[data,{start+1,start+steps}];
		Show[
			ListLinePlot[predicted,Mesh->All,AxesOrigin->{0,0},PlotStyle->{Red,Thickness[Large]},MeshStyle->{Red,PointSize[Medium]},PlotRange->All],
			ListLinePlot[observed,Mesh->All,PlotStyle->Black]
		]
	]

scatterPlot[newData_,predictions_,T_,opts___]:=ListPlot[{Drop[newData,T-1],predictions}\[Transpose],PlotStyle->PointSize[Tiny],AspectRatio->Automatic,PlotLabel->Row[{T,"-steps-ahead predictions"}],opts]

Hankelize[M_]:=Mean[Diagonal[Reverse[M],#]]&/@Range[-Dimensions[M][[1]]+1,Dimensions[M][[2]]-1]

relativeMSD[x_List,xr_List]:=N@Sqrt[Plus@@((x-xr)^2)]/Sqrt[Plus@@xr^2]

singularSpectrumAnalysis[x_List,n_Integer]:=Module[{X,XXt,U,D,V,y},
	X=Transpose@MovingMap[Identity,x,n-1];
	XXt=X.Transpose[X];
	{U,D}=SingularValueDecomposition[XXt][[1;;2]];
	V=Transpose[X].U.DiagonalMatrix[Sqrt[Diagonal[D]]^-1];
	X=Sqrt[D][[#,#]] Outer[Times,U[[All,#]],V[[All,#]]]&/@Range[1,n];
	y= Hankelize/@X;
	{Diagonal[D],y,U,X}
]

singularSpectrumAnalysis[x_List,n_Integer,r_Integer]:=Module[{X,XXt,U,D,V,y},
	X=Transpose@MovingMap[Identity,x,n-1];
	XXt=X.Transpose[X];
	{U,D}=SingularValueDecomposition[XXt,r][[1;;2]];
	V=Transpose[X].U.DiagonalMatrix[Sqrt[Diagonal[D]]^-1];
	X=Sqrt[D][[#,#]] Outer[Times,U[[All,#]],V[[All,#]]]&/@Range[1,r];
	y= Hankelize/@X;
	{Diagonal[D],y,U,X}
]

ssaPlotEigenvalues[ssa_List,n_Integer]:=ListPlot[Log[ssa[[1]]],Joined->True,PlotRange->{{1,n},All}]

ssaPlotComponents[ssa_List,nc_Integer]:=
	GraphicsGrid[Table[
		{ListPlot[ssa[[2,n]],PlotLabel->{n},Joined->True],
			ListPlot[ssa[[2,n+1]],PlotLabel->{n+1},Joined->True]},
		{n,1,nc-1,2}]
	]

ssaPlotResiduals[ssa_List,ts_List,n_Integer]:=ListPlot[ts-Plus@@ssa[[2,1;;n]],PlotRange->All,Joined->True]

ssaPlotPairVectors[ssa_List,n_Integer]:=
GraphicsGrid[
	Partition[
		MapThread[
			ListPlot[Transpose[{ssa[[3]][[All,#1]],ssa[[3]][[All,#2]]}],PlotLabel->{#1,#2},Joined->True]&,
			Transpose[Partition[Range[n],2,1]]
		],
		2
	]
]

ssaPlotPairVectors[ssa_List,modes_List]:=
	GraphicsGrid[
		Partition[
			MapThread[
				ListPlot[Transpose[{ssa[[3]][[All,#1]],ssa[[3]][[All,#2]]}],PlotLabel->{#1,#2},Joined->True]&,
				Transpose[modes]
			],
		2
	]
]

ssaSmoothing[ssa_List,n_Integer]:=Plus@@ssa[[2,1;;n]];

ssaSmoothing[ssa_List,modes_List]:=Plus@@ssa[[2,modes]];

ssaRForecasting[ssa_List,n_Integer,t_Integer]:=
	Module[{U=ssa[[3]],alfa,y,i},
		alfa=1/(1-Plus@@(U[[-1,1;;n]]^2)) U[[;;-2,1;;n]]. U[[-1,1;;n]];
		y=Plus@@ssa[[2,1;;n]];
		For[i=1,i<t+1,i++,
			y=Flatten[Append[y,y[[-Length[alfa];;]].alfa]]
		];
		y
	]

ssaRForecasting[ssa_List,n_Integer,t_Integer,m_Integer]:=Module[{U=ssa[[3]],alfa,y,i},
	alfa=1/(1-Plus@@(U[[-1,1;;n]]^2)) U[[;;-2,1;;n]]. U[[-1,1;;n]];
	y=(Plus@@ssa[[2,1;;n]])[[1;;m]];
	For[i=1,i<t+1,i++,
		y=Flatten[Append[y,y[[-Length[alfa];;]].alfa]]
	];
	y
]

ssaVForecasting[ssa_List,n_Integer,t_Integer]:=Module[{U=ssa[[3]],X,W,alfa,y,i},
	X=Transpose@(Plus@@ssa[[4]][[1;;n]]);
	alfa=1/(1-Plus@@(U[[-1,1;;n]]^2)) U[[;;-2,1;;n]]. U[[-1,1;;n]];
	W=U[[;;-2,1;;n]].Transpose[U[[;;-2,1;;n]]]+(1-Plus@@(U[[-1,1;;n]]^2)) Outer[Times,alfa,alfa];
	For[i=1,i<t+Length[alfa],i++,
		X=Append[X,Flatten@{W.X[[-1,2;;]],alfa.X[[-1,2;;]]}]
	];
	y=(Hankelize@Transpose[X])[[;;-Length[alfa]]];
	y
]

ssaRForecasting[ssa_List,modes_List,t_Integer]:=Module[{U=ssa[[3]],alfa,y,i},
	alfa=1/(1-Plus@@(U[[-1,modes]]^2)) U[[;;-2,modes]]. U[[-1,modes]];
	y=Plus@@ssa[[2,modes]];
	For[i=1,i<t+1,i++,
		y=Flatten[Append[y,y[[-Length[alfa];;]].alfa]]
	];
	y
]

ssaVForecasting[ssa_List,modes_List,t_Integer]:=Module[{U=ssa[[3]],X,W,alfa,y,i},
	X=Transpose@(Plus@@ssa[[4]][[modes]]);
	alfa=1/(1-Plus@@(U[[-1,modes]]^2)) U[[;;-2,modes]]. U[[-1,modes]];
	W=U[[;;-2,modes]].Transpose[U[[;;-2,modes]]]+(1-Plus@@(U[[-1,modes]]^2)) Outer[Times,alfa,alfa];
	For[i=1,i<t+Length[alfa],i++,
		X=Append[X,Flatten@{W.X[[-1,2;;]],alfa.X[[-1,2;;]]}]
	];
	y=(Hankelize@Transpose[X])[[;;-Length[alfa]]];
	y
]

ssawCorrelation[ssa_List,m_Integer]:=
	Module[{l,n,k,le,ke,w,i,j,wc},
		{l,n}=(Dimensions/@ssa)[[2]];
		k=n-l+1;
		le=Min[l,k];
		ke=Max[l,k];
		w=Join[Table[i,{i,1,le-1}],Table[le,{i,le,ke}],Table[n-i+1,{i,ke+1,n}]];
		wc=Table[0,{i,1,m},{j,1,m}];
		For[i=1,i<=m,i++,
			For[j=1,j<=m, j++,
				wc[[i,j]]=(w ssa[[2,i]]).ssa[[2,j]]/(Sqrt[(w ssa[[2,i]]).ssa[[2,i]]] Sqrt[(w ssa[[2,j]]).ssa[[2,j]]])
			]
		];
		ArrayPlot[Reverse[wc],PlotLegends->Automatic]
	]

End[] (* End Private Context *)

EndPackage[]