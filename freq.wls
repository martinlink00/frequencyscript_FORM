(*

Note, that results are dumpsaved into the subfolder "exports", in order to save RAM and in order to not lose results if Mathematica crashes which it tends to do.

*)

(*-----------------------------------------------------------------------------------------------------*)

(*FORM SOURCECODE*)

formcode = "S r,x,y;
Auto S p,P,a;
On Threads;
On NoSpacesInNumbers;
On ThreadStats;
ThreadBucketSize 700;

#include expr.frm

.sort

Format NoSpaces;
Format O4;
Format Mathematica;

#write  <expr-expanded.m> \"\\\"(%E)\\\"\",expr

.end";

formcodelist ="S r,x,y;
Auto S p,P,a;
On Threads;
On NoSpacesInNumbers;
On ThreadStats;
ThreadBucketSize 700;
inparallel;

#include exprlist.frm

.sort

Format NoSpaces;
Format O4;
Format Mathematica;

#write  <expr-expanded-list.m> \"\\\"(%E)\\\"\",res

.end";

(*-----------------------------------------------------------------------------------------------------*)

(*INIT*)

ClearSystemCache[];
Off[FrontEndObject::notavail];

highestcoeffresp = ReadList["params.txt"][[1]];
formthreshhold = ReadList["formconf.txt"][[1]];
blocksize = ReadList["formconf.txt"][[2]];
formlogbool = ToString[ReadList["formconf.txt"][[3]]];

formcmd = "tform -f -w"<>ToString[$ProcessorCount]<>" expandfast";
formcmdlist = "tform -f -w"<>ToString[$ProcessorCount]<>" expandfastlist";
(*formcmd = "form -f expandfast";
formcmdlist = "form -f expandfastlist";*)

ascipic = ReadString["ascipic.txt"];
If[Not[DirectoryQ["exports"]],
  Print["Creating exports directory."];
  CreateDirectory["exports"];SetDirectory["exports"];
  CreateDirectory["logs"];
  CreateDirectory["formlogs"];,
  SetDirectory["exports"];];

(*-----------------------------------------------------------------------------------------------------*)
		
(*MISCDEF*)

logfilename = "freq_"<>DateString["ISODateTime"]<>".log";
AppendTo[$Messages,OpenWrite[FileNameJoin[{"logs",logfilename}]]];

LogPrint[x_,type_] := Module[{},
	Print[x];
	outp = "["<>DateString["ISODateTime"]<>"]\t["<>type<>"]\t[Maximum RAM: "<>ToString[Round[N[MaxMemoryUsed[]/(10^9) *1.0],0.1]]<>" GB ("<>ToString[Round[N[MaxMemoryUsed[]/MemoryAvailable[] *100.00],0.1]]<>"%)]\t"<>x;
	WriteLine[FileNameJoin[{"logs",logfilename}],outp];];
PD[]:=Print["---------------------------------------------------------------------"];

TransToFORM[exp_]:= Module[{tos,expor,file},
	tos = ToString[exp,InputForm];
	expor = "L expr = "<>StringReplace[tos,{"y[t]"->"y","["->"L",", t]"->"R",", "->"D","I"->"i_"}]<>";";
	file=OpenWrite["expr.frm"];WriteString[file,expor];Close[file];
];

LISTTransToFORM[list_]:= Module[{string,file},
	string = "";
	For[iiiiiii=1,iiiiiii<=Length@list,iiiiiii++,
		string = string<>"L ex"<>ToString[iiiiiii]<>" = "
			<>StringReplace[ToString[list[[iiiiiii]],InputForm],
			{"y[t]"->"y","["->"L", ", t]"->"R",", "->"D","I"->"i_"}]
			<>";\n\n";
	];
	string = string<>".sort \n";
	string = If[Length@list==1,string<>"L res = ex1;",string<>"L res = ex1+...+ex"<>ToString[Length@list]<>";"];
	file = OpenWrite["exprlist.frm"];WriteString[file,string];Close[file];
];

EXPList[explist_] := Module[{formtime,ttformtime,bc,logfilename},
	bc = LeafCount[explist];
	If[bc<formthreshhold,
		Return[Plus@@Map[Expand[#]&,explist]];,
	LogPrint["Starting FORM list (LC="<>ToString[bc]<>").","EXP:FORM"];
	formtime = AbsoluteTime[];
	LISTTransToFORM[explist];
	ttformtime = AbsoluteTime[];
	Run[formcmdlist];
	LogPrint["FORM is done (Ts="<>ToString[Round[ttformtime-formtime,0.1]]<>"s, "<>"Tc="<>ToString[Round[AbsoluteTime[]-formtime,0.1]]<>"s).","EXP:FORM"];
	logfilename = "EXPList_"<>DateString["ISODateTime"]<>".log";
	If[formlogbool=="y",CopyFile["expandfastlist.log",FileNameJoin[{"formlogs",logfilename}]]];
	Return[ToExpression[StringReplace[Import["expr-expanded-list.m"],{"y"->"y[t]","i_"->"I","L"->"[","R"->",t]","D"->","}]]];
	];
];

ContainsPlusQ[eexp_] := StringContainsQ[ToString[FullForm[eexp]], {"Plus"}];

EXP[exp_] := Module[{formtime,ttformtime,bc,logfilename},
	bc = LeafCount[exp];
	If[bc<formthreshhold,
		Return[Expand[exp]];
	,
	LogPrint["Starting FORM (LC="<>ToString[bc]<>").","EXP:FORM"];
	formtime = AbsoluteTime[];
	TransToFORM[exp];
	ttformtime = AbsoluteTime[];
	Run[formcmd];
	LogPrint["FORM is done (Ts="<>ToString[Round[ttformtime-formtime,0.1]]<>"s, "<>"Tc="<>ToString[Round[AbsoluteTime[]-formtime,0.1]]<>"s).","EXP:FORM"];
	logfilename = "EXP"<>DateString["ISODateTime"]<>".log";
	CopyFile["expandfast.log",FileNameJoin[{"formlogs",logfilename}]];
	Return[ToExpression[StringReplace[Import["expr-expanded.m"],{"y"->"y[t]","i_"->"I","L"->"[","R"->",t]","D"->","}]]];
	];
];

ExpandFORM[exp_] := Module[{sumlist,explist,res},
	If[ToString[Head[exp]]=="Plus",
		explist = SortBy[List@@exp,ByteCount];
		sumlist = Partition[explist,UpTo[blocksize]];
		res = 0;
		For[iii=1,iii<Length@sumlist+1,iii++,
			If[Length@sumlist!=1,LogPrint["Expanding Block "<>ToString[iii]<>"/"<>ToString[Length@sumlist]<>".","EXP:DEC"];];
			res += EXPList[sumlist[[iii]]];
		];
		Return[res];
		
	,
	Return[EXP[exp]];
	];
];

BetterD[exp_,t_,n_] := Module[{sumlist,derlist},
	If[ToString[Head[exp]]=="Plus",
		sumlist = List@@exp;
		If[Length@sumlist>blocksize*$ProcessorCount,
			LogPrint["D decomposed into "<>ToString[Length@sumlist]<>" terms.","DER:DEC"];
			derlist = Map[D[#,{t,n}]&,sumlist];,
		derlist = Map[D[#,{t,n}]&,sumlist];];
		Return[Plus@@derlist],
	Return[D[exp,{t,n}]]
	]
];

PartSum[listexp_] := Module[{par},
	par = Partition[listexp,UpTo[Max[Round[(Length@listexp)*1/($ProcessorCount)],1]]];
	Return[Plus@@@par];
];

(*-----------------------------------------------------------------------------------------------------*)

(*INIT PRINTOUT*)

PD[];
Print[ascipic];
PD[];
Pause[0.5];
If[$VersionNumber!=12.1,LogPrint["Version error: Script was developed on and for version 12.1.","ERROR"];Quit[],
LogPrint["### KAPITZA FREQUENCY SCRIPT ###","RUNNING"];];PD[];

timebeg = AbsoluteTime[];

LogPrint["Generating FORM code.","RUNNING"];
formfile = OpenWrite["expandfast.frm"];
WriteString[formfile,formcode];
Close[formfile];
formfilelist = OpenWrite["expandfastlist.frm"];
WriteString[formfilelist,formcodelist];
Close[formfilelist];

LogPrint["Acquiring parameters from param.txt","RUNNING"];
highestcoeff = If[IntegerQ[highestcoeffresp],highestcoeffresp,LogPrint["Wrong input in params.txt file!","ERROR"];Quit[]];

PD[];
userinfo = "Script has been executed on "<>ToString[$OperatingSystem]<>" by the user " <>ToString[$UserName]<>".";
versioninfo = "Running on Mathematica "<>ToString[$VersionNumber]<>".";
processorinfo = ToString[$ProcessorCount]<>" processors of type "<>ToString[$ProcessorType]<>" available.";
memoryinfo = ToString[N[DecimalForm[MemoryAvailable[]/10^6]]]<>" MB of memory available for allocation.";
paraminfo = "Calculating up to non-trivial correction "<>ToString[highestcoeff]<>".";
forminfo = "Using TFORM ("<>ToString[$ProcessorCount]<>" worker threads) for LC>"<>ToString[formthreshhold]<>If[formlogbool=="y"," (logs in /exports/formlogs/).","."];
blockinfo = "Expansions are done in blocks of "<>ToString[blocksize]<>".";
esttimeinfo = "Estimate resource usage from logfiles by running paramest.wls.";
exportinfo = "Results are dumpsaved into exports.";
loginfo = "Log is saved under exports/logs/"<>logfilename<>".";
LogPrint["### SYSTEM INFORMATION ###","RUNNING"];PD[];
LogPrint[userinfo,"INFORM"];LogPrint[versioninfo,"INFORM"];LogPrint[processorinfo,"INFORM"];LogPrint[memoryinfo,"INFORM"];
PD[];LogPrint["### RUN INFORMATION ###","RUNNING"];PD[];
LogPrint[paraminfo,"INFORM"];LogPrint[forminfo,"INFORM"];LogPrint[blockinfo,"INFORM"];
PD[];LogPrint["### MISC INFORMATION ###","RUNNING"];PD[];
LogPrint[esttimeinfo,"INFORM"];LogPrint[loginfo,"INFORM"];LogPrint[exportinfo,"INFORM"];

(*-----------------------------------------------------------------------------------------------------*)

(*SCRIPT*)
PD[];LogPrint["### INIT ###","PROGRESS"];PD[];
LogPrint["Initiating and preparing.","PRG:INI"];
\[Epsilon]max = 2 * highestcoeff + 2; (*Max collected \[Epsilon]*)
highestpowercorr=highestcoeff; (*Highest power correction*)
e[n_,t]:=Exp[I n t];
\[CapitalPhi][n_] := ep^(Abs[n])*Piecewise[{{Sum[P[n,m,t]ep^m,{m,0,\[Epsilon]max-n}],n>=0}},Sum[(-1)^(n+m)P[-n,m,t] ep^m,{m,0,\[Epsilon]max-n}]];
\[Delta]\[Theta]= Sum[If[n==0,0,e[n,t]\[CapitalPhi][n]],{n,-\[Epsilon]max,\[Epsilon]max}]; (*\[Delta]\[Theta] Ansatz*)
\[Theta]= \[CapitalPhi][0] +\[Delta]\[Theta];
oddphi0repl =Flatten[Table[D[P[0,2i+1,t],{t,k}]->0,{i,0,highestcoeff+2},{k,0,\[Epsilon]max}]];
newnot = Flatten[Table[D[P[0,2j,t],{t,k}]->p[2 j,k,t],{j,0,highestcoeff+2},{k,0,\[Epsilon]max}],2];
enrepl = Table[Exp[I n t]->x^n,{n,-\[Epsilon]max,\[Epsilon]max}];

\[Theta]sercoeff = Series[\[Theta]/.oddphi0repl/.newnot,{ep,0,\[Epsilon]max}][[3]];
\[Omega]sercoeff = D[\[Theta]sercoeff,t]/.enrepl;
\[Theta]sercoeff = \[Theta]sercoeff/.enrepl;
dercoeff = Series[ep^2 D[\[CapitalPhi][0],{t,2}] + Sum[If[n==0,0,e[n,t](ep^2 D[\[CapitalPhi][n],{t,2}] + 2 I n ep D[\[CapitalPhi][n],{t,1}]- n^2 \[CapitalPhi][n])],{n,-\[Epsilon]max,\[Epsilon]max}]/.oddphi0repl /.newnot/.enrepl,{ep,0,\[Epsilon]max}][[3]];
sinser = Series[Sin[\[CurlyPhi][0][t]+\[Delta]],{\[Delta],0,\[Epsilon]max-1}][[3]]/.{Sin[\[CurlyPhi][0][t]]->1/(2 I) y[t] - 1/(2 I) 1/y[t],Cos[\[CurlyPhi][0][t]]->1/2 y[t]+1/2*1/y[t]};
bellargument = Table[Table[Factorial[i]\[Theta]sercoeff[[i+1]],{i,1,n}],{n,1,\[Epsilon]max}];
sinexpfunc[epsord_] := If[epsord<0,0,If[epsord==0,1/(2 I) y[t]-1/(2 I)*1/y[t],1/Factorial[epsord] Sum[Factorial[n]sinser[[n+1]]BellY[epsord,n,bellargument[[epsord-n+1]]],{n,1,epsord}]]];

y'[t] := I p[0,1,t]y[t];
For[j=2,j<\[Epsilon]max+2,j++,\!\(\*SuperscriptBox[\(y\), 
TagBox[
RowBox[{"(", "j", ")"}],
Derivative],
MultilineFunction->None]\)[t] := D[y'[t],{t,j-1}]];

For[i1=0,i1<highestcoeff+1,i1++,
	For[j1=0,j1<\[Epsilon]max+1,j1++,
		For[k1=0,k1<\[Epsilon]max+1,k1++,
			\!\(\*SuperscriptBox[\(p\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "0", ",", "k1"}], ")"}],
Derivative],
MultilineFunction->None]\)[2 i1,j1,t]= p[2 i1,j1+k1,t];
		]
	];
];

For[m=0,m<highestcoeff+1,m++,
	For[n=2,n<\[Epsilon]max-1,n++,
		p[2 m,n,t_] = a[2m,n-2,t];
	];
];

DumpSave["ini.mx",{LogPrint,logfilename,\[Theta],\[Delta]\[Theta],\[CapitalPhi],\[Epsilon]max,highestcoeff,formthreshhold,formprintbool,formcmd,PD,newnot,oddphi0repl,ContainsPlusQ,EXP,ExpandFORM,TransToFORM,LISTTransToFORM,EXPList,formcmd,formcmdlist,BetterD}]; (*Dumping basic definitions for later usage.*)

(*Sin treatment*)

sinexplist = Table[sinexpfunc[ee],{ee,0,\[Epsilon]max-1}]; (*Need only up to \[Epsilon]max-1, since one \[Epsilon] already on lhs.*)
sinexp[\[Epsilon]ord_] := If[\[Epsilon]ord<0,0,sinexplist[[\[Epsilon]ord+1]]];

(**)

LogPrint["Finding relevant coefficients.","PRG:COE"];
acc0[ord_] :=  ExpandFORM[Coefficient[Expand[sinexp[ord-2]- r * 1/(2 I) * (x-1/x)sinexp[ord-1],x],x,0]];
For[asd=1,asd<=highestpowercorr+1,asd++,
	LogPrint["Saving accelerations at eps^"<>ToString[2*asd]<>".","PRG:COE"];
	acc0bef[asd] = acc0[2 asd];
];

DumpSave["acclistbef.mx",acc0bef];
ClearAll[acc0bef];
<<acclistbef.mx;

For[i=1,i<\[Epsilon]max+1,i++,
	LogPrint["Saving algebraic eqs at eps^"<>ToString[i]<>".","PRG:COE"];
	exbef[i] = ExpandFORM[sinexp[i-2]- r * 1/(2 I) * (x-x^-1)sinexp[i-1]];
	For[j=0,j<i,j++,
		LogPrint["Saving coeffs of e["<>ToString[i-j]<>"].","PRG:COE"];
		cf = Coefficient[exbef[i],x,i-j];
		cefbef[i,j] = ExpandFORM[-1/((i-j)^2)*cf]; 
	];
	(*DumpSave["exbef.mx",exbef];
	ClearAll[exbef];
	<<exbef.mx;*)
];
DumpSave["cefbef.mx",cefbef];
ClearAll[cefbef,exbef];
<<cefbef.mx;

For[i=1,i<\[Epsilon]max+1,i++,
	PD[];LogPrint["### SOLVING AT EPS^"<>ToString[i]<>" ###","PROGRESS"];PD[];
	If[i!=1,
		If[EvenQ[i],
			For[asdf=i/2,asdf<=highestpowercorr+1,asdf++,
				LogPrint["Pre-expanding accelerations at eps^"<>ToString[2*asdf]<>".","PRG:ACC"];
				acc0bef[asdf] = ExpandFORM[acc0bef[asdf]];
				(*DumpSave["acc0bef.mx",acc0bef];
				ClearAll[acc0bef];
				<<acc0bef.mx;*)
			];
			LogPrint["Declaring phi[0,"<>ToString[i-2]<>"]''[t].","PRG:ACC"];
			a[i-2,0,t] = ExpandFORM[acc0bef[i/2]];
			(*For[mm=1,mm<\[Epsilon]max-1-2 (i-2),mm++,
				LogPrint["Declaring deriv "<>ToString[mm]<>" of phi[0,"<>ToString[i-2]<>"]''[t].","PRG:ACC"];
				a[i-2,mm,t] = ExpandFORM[BetterD[a[i-2,mm-1,t],t,1]];
				DumpSave["a.mx",a];
				ClearAll[a];
				<<a.mx;
			];*)
		];
		For[ll=i,ll<\[Epsilon]max+1,ll++,
			LogPrint["Pre-expanding algebraic eqs at eps^"<>ToString[ll]<>".","PRG:ALG"];
			For[kk=0,kk<ll,kk++,
				LogPrint["Pre-expanding coeff e["<>ToString[ll-kk]<>"].","PRG:ALG"];		
				cefbef[ll,kk] = ExpandFORM[cefbef[ll,kk]];
				(*DumpSave["cefbef.mx",cefbef];
				ClearAll[cefbef];
				<<cefbef.mx;*)
			];
		];	
	];
	For[j=0,j<i,j++,
		LogPrint["Declaring phi["<>ToString[i-j]<>","<>ToString[j]<>"][t].","PRG:ALG"];
		exaa = ExpandFORM[cefbef[i,j]];
		If[j==0,
			P[i-j,j,t_] = exaa,
			deraa = 2/(i-j) I BetterD[P[i-j,j-1,t],t,1];
			If[j==1,
			P[i-j,j,t_] = ExpandFORM[exaa + deraa],
			sdera = 1/(i-j)^2 BetterD[P[i-j,j-2,t],t,2];
			P[i-j,j,t_] = ExpandFORM[exaa + deraa + sdera];
			];
		];
		(*DumpSave["phi.mx",P];
		Clear[P];
		<<phi.mx;*)
	];
];
DumpSave["phi.mx",P];
ClearAll[exaa,cefbef,P];
DumpSave["acc0end.mx",acc0bef];
PD[];LogPrint["### EVALUATING ZERO ENERGY LIMIT ###","PROGRESS"];PD[];
p[n_,0,t] = 0;
p[n_,1,t] = 0;

For[asdf=1,asdf<=highestpowercorr+1,asdf++,
		LogPrint["Setting phi=0 at eps^"<>ToString[2*asdf]<>".","PRG:ZER"];
		acc0bef[asdf] = ExpandFORM[acc0bef[asdf]];
		(*DumpSave["acc0bef.mx",acc0bef];
		ClearAll[acc0bef];
		<<acc0bef.mx;*)
];

PD[];LogPrint["### ACQUIRING FREQUENCIES ###","PROGRESS"];
For[n=0,n<highestcoeff+1,n++,
	\[Omega][2n] = Collect[-acc0bef[n+1]/.{y[_]^n_:>I n}/.{y[_]:>I},r,Expand];
];

reference = -1 + \[Rho]^2/2 + \[Epsilon]^2 (-2 \[Rho]^2 + (25 \[Rho]^4)/
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
    488045135705765455513091071 \[Rho]^14)/33449308087910400000000 + (
    83857909128685125912504026393 \[Rho]^16)/
    68504182964040499200000000) + \[Epsilon]^16 (32768 \[Rho]^2 - (
    10944513 \[Rho]^4)/32 + (371789779789295 \[Rho]^6)/306110016 - (
    112207057270371335395 \[Rho]^8)/53496602689536 + (
    2108768571265773123745227511 \[Rho]^10)/1044855521280000000000 - (
    3571425412964887484418187991 \[Rho]^12)/3134566563840000000000 + (
    92279593282170513811138202650217 \[Rho]^14)/
    245852414446141440000000000 - (
    269168508120402223082153608059967051 \[Rho]^16)/
    4028045958285581352960000000000 + (
    120089176174908625267123147897106657 \[Rho]^18)/
    24168275749713488117760000000000) + \[Epsilon]^18 (-131072 \
\[Rho]^2 + (54001665 \[Rho]^4)/32 - (5116060734204095 \[Rho]^6)/
    688747536 + (93058155781360081771505 \[Rho]^8)/
    5777633090469888 - (9355356107413400058873181771417 \[Rho]^10)/
    470184984576000000000000 + (
    33739061859510881881535523290359 \[Rho]^12)/
    2256887925964800000000000 - (
    45296196240036131201719335763017237187 \[Rho]^14)/
    6505254886244902502400000000000 + (
    279232015345030240850013025618323421714447 \[Rho]^16)/
    142109461408315310132428800000000000 - (
    131243335963394451160758386540543920310989 \[Rho]^18)/
    426328384224945930397286400000000000 + (
    175263403904227897478594054729227794909043 \[Rho]^20)/
    8526567684498918607945728000000000000);
     
difmax = Min[2 highestcoeff,18];
difference = Normal[Series[Sum[\[Epsilon]^(2 n) \[Omega][2n]/.r->\[Rho],{n,0,highestcoeff}]-reference,{\[Epsilon],0,difmax}]];

DumpSave["om.mx",{\[Omega],reference,difmax,difference}];
timeend = AbsoluteTime[];
PD[];
LogPrint["### SCRIPT RUN AT CORRECTION "<>ToString[highestcoeff]<>" COMPLETED IN "<>ToString[Round[timeend-timebeg,0.1]]<>"s ###","INFORM"];
PD[];
If[ToString[difference]=="0",LogPrint["### RESULTS AGREE WITH KNOWN RESULTS ###","INFORM"];,LogPrint["### RESULTS ARE INCORRECT ###","ERROR"];];
PD[];
LogPrint["### FREQUENCY CALCULATION DONE ###","RUNNING"];
PD[];
