#!/usr/bin/env wolframscript
(* ::Package:: *)

Off[FrontEndObject::notavail];

SetDirectory["exports"]; SetDirectory["logs"];

freqloglist = FileNames["freq_*.log"];

extracttime[file_] := Module[ {found},
   found = Find[file, "Script run completed. It took"]; Close[file];
   ToExpression[
    StringCases[found, 
      RegularExpression[
       "([+-]?(?=\\.\\d|\\d)(?:\\d+)?(?:\\.?\\d*))(?:[eE]([+-]?\\d+))?\
"]][[-1]]]
   ];

extractorder[file_] := Module[ {found},
   found = Find[file, "non-trivial correction"]; Close[file];
   ToExpression[
    StringCases[found, 
      RegularExpression[
       "[0-9]+"]][[-1]]]
   ];

extractmaxram[file_] := Module[ {filetab,ramtab},
   filetab = Import[file, "Table"];
   ramtab = filetab[[All, 5]];
   Max[ramtab]
   ];

Print["Found " <> ToString[Length@freqloglist] <> 
   " frequency calculation logfiles to extract data from."];
Print["Extracting data."];


freqdata = {};
For[i = 1, i <= Length@freqloglist, i++,
  file = ToString[freqloglist[[i]]];
  AppendTo[
   freqdata, {extractorder[file], extracttime[file], 
    extractmaxram[file]}];
  ];

Print["Fitting data."]

upperbound = Max[freqdata[[All,1]]] + 5;

timefitfreq[
   z_] = (a*Exp[b*z] /. 
    FindFit[freqdata[[All, 1 ;; 2]], a*Exp[b*x], {a, b}, x]);
timeplotfitf = 
  LogPlot[{timefitfreq[x]}, {x, 0, upperbound}, 
   AxesLabel -> {"Non-trivial correction", "Computation time (s)"}, 
   GridLines -> Automatic];
timeplotdat = 
  ListLogPlot[{freqdata[[All, 1 ;; 2]]}, 
   PlotRange -> {{0, upperbound}, All}, 
   AxesLabel -> {"Non-trivial correction", "Computation time (s)"}];
exptimeplot = Show[timeplotfitf, timeplotdat];

ramfitfreq[z_] = 
  a*Exp[b*z] /. 
   FindFit[freqdata[[All, 1 ;; 3 ;; 2]], a*Exp[b*x], {a, b}, x];
ramplotfitf = 
  LogPlot[{ramfitfreq[x]}, {x, 0, upperbound}, 
   AxesLabel -> {"Non-trivial correction", "Max used RAM (GB)"}, 
   GridLines -> Automatic];
ramplotdat = 
  ListLogPlot[{freqdata[[All, 1 ;; 3 ;; 2]]}, PlotRange -> {{0, upperbound}, All}, 
   AxesLabel -> {"Non-trivial correction", "Max used RAM (GB)"}];
expramplot = Show[ramplotfitf, ramplotdat];

SetDirectory[ParentDirectory[]];
SetDirectory[ParentDirectory[]];

Export["comptime.pdf", exptimeplot]; Export["ram.pdf", expramplot];

hc = ReadList["params.txt"][[1]];


Print["Computation time and RAM usage of all available logfiles has \
been fitted and plotted into comptime.pdf and ram.pdf."];
Print["For the current inserted highest correction (" <> 
   ToString[hc] <> ") the following parameters are to be expected:"];
Print["Running freq.wls is expected to take " <> 
   ToString[N[timefitfreq[hc], 2]] <> " s and use up " <> 
   ToString[N[ramfitfreq[hc], 2]] <> " GB of RAM."];
