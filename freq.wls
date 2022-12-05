(*

Note, that results are dumpsaved into the subfolder "exports", in order to save RAM and in order to not lose results if Mathematica crashes which it tends to do.

*)

(*INIT*)

ClearSystemCache[];
Off[FrontEndObject::notavail];

highestcoeffresp = ReadList["params.txt"][[1]];
formthreshhold = ReadList["formconf.txt"][[1]];
formprintbool = ToString[ReadList["formconf.txt"][[2]]];
formcmd = If[formprintbool=="y","tform -w"<>ToString[$ProcessorCount]<>" expandfast","tform -f -w"<>ToString[$ProcessorCount]<>" expandfast"];

ascipic = ReadString["ascipic.txt"];
If[Not[DirectoryQ["exports"]],
  Print["Creating exports directory."];CreateDirectory["exports"];SetDirectory["exports"];CreateDirectory["logs"];,
  SetDirectory["exports"];];

		
(*MISCDEF*)

logfilename = "freq_"<>DateString["ISODateTime"]<>".log";
AppendTo[$Messages,OpenWrite[FileNameJoin[{"logs",logfilename}]]];

LogPrint[x_,type_] := Module[{},
	Print[x];
	outp = "["<>DateString["ISODateTime"]<>"]\t["<>type<>"]\t[Maximum RAM: "<>ToString[Round[N[MaxMemoryUsed[]/(10^9) *1.0],0.1]]<>" GB ("<>ToString[Round[N[MaxMemoryUsed[]/MemoryAvailable[] *100.0],0.01]]<>"%)]\t"<>x;
	WriteLine[FileNameJoin[{"logs",logfilename}],outp];];
PD[]:=Print["---------------------------------------------------------------------"];

TransToFORM[exp_]:= Module[{tos,expor,file},
	tos = ToString[exp/.{f[n_][t_]:>f[n,t]},InputForm];
	expor = "L expr = "<>StringReplace[tos,{"["->"(","]"->")","I"->"i_"}]<>";";
	file=OpenWrite["expr.frm"];WriteString[file,expor];Close[file];
];

ContainsPlusQ[eexp_] := StringContainsQ[ToString[eexp], {"+", "-"}];

EXP[exp_] := Module[{formtime,ttformtime,bc},
	bc = LeafCount[exp];
	If[bc<formthreshhold,
		Expand[exp],
	LogPrint["Starting FORM (LC="<>ToString[bc]<>").","EXP:FORM"];
	formtime = AbsoluteTime[];
	TransToFORM[exp];
	ttformtime = AbsoluteTime[];
	Run[formcmd];
	LogPrint["FORM is done (Ts="<>ToString[Round[ttformtime-formtime,0.1]]<>"s, "<>"Tc="<>ToString[Round[AbsoluteTime[]-formtime,0.1]]<>"s).","EXP:FORM"];
	ToExpression[StringReplace[Import["expr-expanded.m"],{"i_"->"I"}]]
	]
];

ExpandFORM[exp_] := Module[{sumlist,other,res},
	If[LeafCount[exp]<formthreshhold,
		If[ToString[Head[exp]]=="Plus",
			sumlist = List@@exp;
			sumlist = Map[Expand[#]&,sumlist];
			Return[Plus@@sumlist],
		Return[Expand[exp]];
		],
		If[ToString[Head[exp]]=="Plus",
			other = DeleteCases[exp,_?ContainsPlusQ];
			sumlist = Cases[exp,_?ContainsPlusQ];
			If[ToString[Head[other]]=="Plus",LogPrint["Stripping away "<>ToString[Length@(List@@other)]<>" summands.","EXP:SUM"]];
			res = EXP[Plus@@sumlist];
			Return[res + other];
		,
		Return[EXP[exp]];
		]
	]
];

BetterD[exp_,t_,n_] := Module[{sumlist,derlist},
	If[ToString[Head[exp]]=="Plus",
		sumlist = List@@exp;
		If[Length@sumlist>3000,
			LogPrint["Derivative of sum decomposed into "<>ToString[Length@sumlist]<>" terms.","DER:DEC"];
			derlist = ParallelMap[D[#,{t,n}]&,sumlist];,
		derlist = Map[D[#,{t,n}]&,sumlist];];
		Plus@@derlist,
	D[exp,{t,n}]
	]
];

(*INIT PRINTOUT*)

PD[];
Print[ascipic];
PD[];
Pause[1.5];
If[$VersionNumber!=12.1,LogPrint["Version error: Script was developed on and for version 12.1.","ERROR"];Quit[],
LogPrint["### KAPITZA FREQUENCY SCRIPT ###","RUNNING"];];

timebeg = AbsoluteTime[];

LogPrint["Generating FORM code.","RUNNING"];
formcode = "On Threads;
Auto S t;
Auto S r;
Auto S x;
Auto CF y;
Auto CF ph;
Auto CF PH;
Auto CF a;


#include expr.frm

.sort

Format Mathematica;

#write  <expr-expanded.m> \"\\\"(%E)\\\"\",expr

.end";

formfile = OpenWrite["expandfast.frm"];
WriteString[formfile,formcode];
Close[formfile];
LogPrint["Acquiring parameters from param.txt","RUNNING"];
highestcoeff = If[IntegerQ[highestcoeffresp],highestcoeffresp,LogPrint["Wrong input in params.txt file!","ERROR"];Quit[]];

PD[];
userinfo = "Script has been executed on "<>ToString[$OperatingSystem]<>" by the user " <>ToString[$UserName]<>".";
versioninfo = "Running on Mathematica "<>ToString[$VersionNumber]<>".";
processorinfo = ToString[$ProcessorCount]<>" processors of type "<>ToString[$ProcessorType]<>" available.";
memoryinfo = ToString[N[DecimalForm[MemoryAvailable[]/10^6]]]<>" MB of memory available for allocation.";
paraminfo = "Calculating up to non-trivial correction "<>ToString[highestcoeff]<>".";
forminfo = "Using TFORM ("<>ToString[$ProcessorCount]<>" worker threads) for LC>"<>ToString[formthreshhold]<>" ("<>If[formprintbool=="y","with ","without "]<>"printouts).";
esttimeinfo = "Estimate resource usage from logfiles by running paramest.wls.";
exportinfo = "Results are dumpsaved into exports.";
loginfo = "Log is saved under exports/logs/"<>logfilename<>".";
LogPrint["### SYSTEM INFORMATION ###","RUNNING"];
LogPrint[userinfo,"RUN:SYS"];LogPrint[versioninfo,"RUN:SYS"];LogPrint[processorinfo,"RUN:SYS"];LogPrint[memoryinfo,"RUN:SYS"];PD[];
LogPrint["### RUN INFORMATION ###","RUNNING"];LogPrint[paraminfo,"RUN:PRM"];LogPrint[forminfo,"RUN:PRM"];LogPrint[esttimeinfo,"INFORM"];LogPrint[loginfo,"INFORM"];LogPrint[exportinfo,"INFORM"];PD[];
LogPrint["### NOW RUNNING SCRIPT ###","RUNNING"];PD[];

(*SCRIPT*)

LogPrint["Initiating.","PROGRESS"];
\[Epsilon]max = 2 * highestcoeff + 2; (*Max collected \[Epsilon]*)
highestpowercorr=highestcoeff; (*Highest power correction*)
e[n_,t]:=Exp[I n t];
\[CapitalPhi][n_] := ep^(Abs[n])*Piecewise[{{Sum[PH[n,m,t]ep^m,{m,0,\[Epsilon]max-n}],n>=0}},Sum[(-1)^(n+m)PH[-n,m,t] ep^m,{m,0,\[Epsilon]max-n}]];
\[Delta]\[Theta]= Sum[If[n==0,0,e[n,t]\[CapitalPhi][n]],{n,-\[Epsilon]max,\[Epsilon]max}]; (*\[Delta]\[Theta] Ansatz*)
\[Theta]= \[CapitalPhi][0] +\[Delta]\[Theta];
oddphi0repl =Flatten[Table[D[PH[0,2i+1,t],{t,k}]->0,{i,0,highestcoeff+2},{k,0,\[Epsilon]max}]];
newnot = Flatten[Table[D[PH[0,2j,t],{t,k}]->ph[2 j,k,t],{j,0,highestcoeff+2},{k,0,\[Epsilon]max}],2];
enrepl = Table[Exp[I n t]->x^n,{n,-\[Epsilon]max,\[Epsilon]max}];
PD[];

\[Theta]sercoeff = Series[\[Theta]/.oddphi0repl/.newnot,{ep,0,\[Epsilon]max}][[3]];
\[Omega]sercoeff = D[\[Theta]sercoeff,t]/.enrepl;
\[Theta]sercoeff = \[Theta]sercoeff/.enrepl;
dercoeff = Series[ep^2 D[\[CapitalPhi][0],{t,2}] + Sum[If[n==0,0,e[n,t](ep^2 D[\[CapitalPhi][n],{t,2}] + 2 I n ep D[\[CapitalPhi][n],{t,1}]- n^2 \[CapitalPhi][n])],{n,-\[Epsilon]max,\[Epsilon]max}]/.oddphi0repl /.newnot/.enrepl,{ep,0,\[Epsilon]max}][[3]];
sinser = Series[Sin[\[CurlyPhi][0][t]+\[Delta]],{\[Delta],0,\[Epsilon]max-1}][[3]]/.{Sin[\[CurlyPhi][0][t]]->1/(2 I) y[t] - 1/(2 I) 1/y[t],Cos[\[CurlyPhi][0][t]]->1/2 y[t]+1/2*1/y[t]};
bellargument = Table[Table[Factorial[i]\[Theta]sercoeff[[i+1]],{i,1,n}],{n,1,\[Epsilon]max}];
sinexpfunc[epsord_] := If[epsord<0,0,If[epsord==0,1/(2 I) y[t]-1/(2 I)*1/y[t],1/Factorial[epsord] Sum[Factorial[n]sinser[[n+1]]BellY[epsord,n,bellargument[[epsord-n+1]]],{n,1,epsord}]]];

y'[t] := I ph[0,1,t]y[t];
For[j=2,j<\[Epsilon]max+2,j++,\!\(\*SuperscriptBox[\(y\), 
TagBox[
RowBox[{"(", "j", ")"}],
Derivative],
MultilineFunction->None]\)[t] := D[y'[t],{t,j-1}]];
For[i1=0,i1<highestcoeff+1,i1++,
	For[j1=0,j1<\[Epsilon]max+1,j1++,
		For[k1=0,k1<\[Epsilon]max+1,k1++,
			\!\(\*SuperscriptBox[\(ph\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "0", ",", "k1"}], ")"}],
Derivative],
MultilineFunction->None]\)[2 i1,j1,t]= ph[2 i1,j1+k1,t];
		]
	];
];

DumpSave["ini.mx",{LogPrint,logfilename,\[Theta],\[Delta]\[Theta],\[CapitalPhi],\[Epsilon]max,highestcoeff,formthreshhold,formprintbool,formcmd,PD,newnot,oddphi0repl,ContainsPlusQ,EXP,ExpandFORM,TransToFORM,BetterD}]; (*Dumping basic definitions for later usage.*)

(*Sin treatment*)

LogPrint["Acquiring sin(theta) expansion.","PRG:INIT"];
sinexplist = Table[sinexpfunc[ee],{ee,0,\[Epsilon]max-1}]; (*Need only up to \[Epsilon]max-1, since one \[Epsilon] already on lhs.*)
sinexp[\[Epsilon]ord_] := If[\[Epsilon]ord<0,0,sinexplist[[\[Epsilon]ord+1]]];

(*Declare \[Phi][n\[NotEqual]0,m,t] solutions*)

PD[];LogPrint["Acquiring algebraic solutions up to eps^"<>ToString[\[Epsilon]max]<>".","PROGRESS"];

acc0[ord_] :=  ExpandFORM[Coefficient[Expand[sinexp[ord-2]- r * 1/(2 I) * (x-1/x)sinexp[ord-1],x],x,0]];
acc0list = Table[acc0[2 ee+2],{ee,0,highestpowercorr}];
DumpSave["acclistbef.mx",{acc0list}];
ClearAll[acc0list];

LogPrint["Finding relevant coefficients.","PRG:ALG"];
For[i=1,i<\[Epsilon]max+1,i++,
	PD[];
	LogPrint["Saving coeffs at order eps^"<>ToString[i]<>".","PRG:ALG"];
	exbef[i] = ExpandFORM[sinexp[i-2]- r * 1/(2 I) * (x-x^-1)sinexp[i-1]];
	For[j=0,j<i,j++,
		LogPrint["Saving coeffs of e["<>ToString[i-j]<>"].","PRG:ALG"];
		cf = Coefficient[exbef[i],x,i-j];
		cefbef[i,j] = ExpandFORM[-1/((i-j)^2)*cf]; 
		DumpSave["cefbef.mx",cefbef];
		ClearAll[cefbef];
		<<cefbef.mx;
	];
	DumpSave["exbef.mx",exbef];
	ClearAll[exbef];
	<<exbef.mx;
];
ClearAll[exbef];

For[i=1,i<\[Epsilon]max+1,i++,
	PD[];LogPrint["Declaring algebraic solutions at order eps^"<>ToString[i]<>".","PRG:ALG"];
	(*
	If[i>11,
		For[ll=i,ll<\[Epsilon]max+1,ll++,
			For[kk=0,kk<i,kk++,
				LogPrint["Expanding equations","PRG:ALG"];
				cefbef[i,kk] = ExpandFORM[cefbef[i,kk]];
				DumpSave["cefbef.mx",cefbef];
				ClearAll[cefbef];
				<<cefbef.mx;
			];
		];
	];
	*)
	For[j=0,j<i,j++,
		LogPrint["Declaring phi["<>ToString[i-j]<>","<>ToString[j]<>"][t].","PRG:ALG"];
		exaa = ExpandFORM[cefbef[i,j]];
		If[j==0,
			PH[i-j,j,t_] = exaa,
			deraa = 2/(i-j) I BetterD[PH[i-j,j-1,t],t,1];
			If[j==1,
			PH[i-j,j,t_] = ExpandFORM[exaa + deraa],
			sdera = 1/(i-j)^2 BetterD[PH[i-j,j-2,t],t,2];
			PH[i-j,j,t_] = ExpandFORM[exaa + deraa + sdera];
			];
		];
		DumpSave["phi.mx",PH];
		Clear[PH];
		<<phi.mx;
	];
];
ClearAll[exaa,cefbef];
timeacc = AbsoluteTime[];
PD[];LogPrint["All phi[n!=0,m,t] declared in "<>ToString[Round[timeacc-timebeg,0.1]]<>"s.","INFORM"];PD[];

(*Effective potential treatment*)

LogPrint["Acquiring acceleration expressions.","PROGRESS"];
LogPrint["Declaring phi[0,2m]''[t] up to m = "<>ToString[highestcoeff]<>".","PRG:ACC"];

<<acclistbef.mx;
acc0list = Map[ExpandFORM[#]&,acc0list];

For[m=0,m<highestcoeff+1,m++,
	For[n=2,n<\[Epsilon]max-1,n++,
		ph[2 m,n,t_] = a[2m,n-2,t];
	];
];

For[m=0,m<highestcoeff,m++,
	LogPrint["Declaring phi[0,"<>ToString[2 m]<>"]''[t].","PRG:ACC"];
	a[2 m,0,t_] = acc0list[[m+1]];
	acc0list = Map[Evaluate[ExpandFORM[#]]&,acc0list];
	DumpSave["a.mx",{a,ph,acc0list}];
	ClearAll[a,ph,acc0list];
	<<a.mx;
];

PD[];
LogPrint["Declaring higher derivatives.","PRG:ACC"];

For[nn=0,nn<highestcoeff+1,nn++,
	For[mm=1,mm<\[Epsilon]max-1-2 nn,mm++,
		LogPrint["Declaring derivative "<>ToString[mm]<>" of phi[0,"<>ToString[2 nn]<>"][t].","PRG:ACC"];
		a[2 nn,mm,t] = ExpandFORM[BetterD[a[2 nn,mm-1,t],t,1]];
		DumpSave["a.mx",{a,ph,acc0list}];
		ClearAll[a,ph,acc0list];
		<<a.mx;
	];
];

PD[];LogPrint["Setting phi[0,2m][t] and phi[0,2m]'[t] to zero.","PRG:ACC"];
ph[n_,0,t] = 0;
ph[n_,1,t] = 0;
acc0list = Map[ExpandFORM[#]&,acc0list];
DumpSave["acc0listend.mx",acc0list];
timeend = AbsoluteTime[];
PD[];LogPrint["Accelerations acquired up to correction "<>ToString[highestpowercorr]<>" in "<>ToString[Round[timeend-timeacc,0.1]]<>"s.","INFORM"];

LogPrint["Acquiring frequency expressions.","PROGRESS"];
For[n=0,n<highestcoeff+1,n++,
	\[Omega][2n] = Collect[-acc0list[[n+1]]/.{y[_]^n_:>I n}/.{y[_]:>I},r,Expand];
];

reference = -1 + \[Rho]^2/
  2 + \[Epsilon]^2 (-2 \[Rho]^2 + (25 \[Rho]^4)/
     32) + \[Epsilon]^4 (8 \[Rho]^2 - (273 \[Rho]^4)/32 + (
     1169 \[Rho]^6)/576) + \[Epsilon]^6 (-32 \[Rho]^2 + (
     2049 \[Rho]^4)/32 - (94831 \[Rho]^6)/2592 + (16824665 \[Rho]^8)/
     2654208) + \[Epsilon]^8 (128 \[Rho]^2 - (13057 \[Rho]^4)/32 + (
     18943067 \[Rho]^6)/46656 - (15301534105 \[Rho]^8)/95551488 + (
     104550461873 \[Rho]^10)/
     4777574400) + \[Epsilon]^10 (-512 \[Rho]^2 + (75777 \[Rho]^4)/
     32 - (187198453 \[Rho]^6)/52488 + (2704847264795 \[Rho]^8)/
     1146617856 - (510599414365027 \[Rho]^10)/716636160000 + (
     1383860829361699 \[Rho]^12)/
     17199267840000) + \[Epsilon]^12 (2048 \[Rho]^2 - (
     413697 \[Rho]^4)/32 + (102745082885 \[Rho]^6)/3779136 - (
     3275894575046105 \[Rho]^8)/123834728448 + (
     16897194776220398401 \[Rho]^10)/1289945088000000 - (
     12419023504555535053 \[Rho]^12)/3869835264000000 + (
     29289023958538918009 \[Rho]^14)/
     94810963968000000) + \[Epsilon]^14 (-8192 \[Rho]^2 + (
     2162689 \[Rho]^4)/32 - (3200742128665 \[Rho]^6)/17006112 + (
     1114567661360939425 \[Rho]^8)/4458050224128 - (
     155733295202139312167933 \[Rho]^10)/870712934400000000 + (
     985874975154709612644967 \[Rho]^12)/13931406950400000000 - (
     488045135705765455513091071 \[Rho]^14)/
     33449308087910400000000 + (
     83857909128685125912504026393 \[Rho]^16)/
     68504182964040499200000000);
     
difmax = Min[2 highestcoeff,14];
difference = Normal[Series[Sum[\[Epsilon]^(2 n) \[Omega][2n]/.r->\[Rho],{n,0,highestcoeff}]-reference,{\[Epsilon],0,difmax}]];

DumpSave["om.mx",{\[Omega],reference,difmax,difference}];

PD[];
If[ToString[difference]=="0",LogPrint["### RESULTS AGREE WITH KNOWN RESULTS UP TO CORRECTION 7 ###","INFORM"];,LogPrint["### RESULTS ARE INCORRECT UP TO 7TH CORRECTION ###","ERROR"];];

(*CLEANUP*)

PD[];LogPrint["Script run completed. It took "<>ToString[Round[timeend-timebeg,0.1]]<>"s.","INFORM"];
PD[];
LogPrint["### FREQUENCY CALCULATION DONE ###","RUNNING"];
PD[];
