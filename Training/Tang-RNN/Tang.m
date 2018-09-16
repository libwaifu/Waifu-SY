(* ::Package:: *)
(* ::Title:: *)
(*Tang(Tang)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2018-08-28*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(**)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
BeginPackage["Tang`", {"OpenCLLink`"}];
(* ::Section:: *)
(*函数说明*)
Tang7::usage = "";
Tang7Line::usage = "";
Tang7LineRandom::usage = "";
PrepareDataTang::usage = "PrepareData[char_, f_ : 3]";
jsonNetPlot::usage = "";
GetReport::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`RNN`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$Tang = "V1.0";
Updated$Tang = "2018-08-28";
(* ::Subsubsection:: *)
(*功能块 1*)

PrepareDataTang[char_, f_ : 3] := Block[
	{s, sp, ipt, ips, rare, chars},
	s = ToString[char];
	sp = Print@Export[#1, #2, PerformanceGoal -> "Size"]&;
	ipt = Select[
		Flatten@Import["Data-Tang.csv"],
		StringLength[#] == (char + 1) * 2&
	] // Core$Galaster`TT;
	sp["count" <> s <> ".WXF", count = Tally[Characters@StringJoin[ipt]]];
	Echo[Length@ipt, Text["语料数: "]];
	sp["ipt" <> s <> ".WXF", ipt];
	rare = First /@ Select[SortBy[count, Last], Last@# < f&];
	ips = Select[ipt, !StringContainsQ[#, rare]&] // Core$Galaster`TT;
	Echo[Length[chars = DeleteDuplicates@Flatten[Characters /@ ips]], "符号数: "];
	sp["chars" <> s <> ".WXF", chars ];
	Now
];

inspectPredictions[string_] := Block[
	{obj, chars, pred, predItems, charItems},
	obj = NetStateObject[generateNet];
	chars = Characters[string];
	pred = Map[obj[#, {"TopProbabilities", 9}]&, chars];
	predItems = Map[Item[First[#], Background -> Opacity[Last[#], Darker[Green]]]&, pred, {2}];
	predItems = Prepend[Most[predItems], Table[Item["", Background -> Gray], 9]];
	charItems = Item[#, Background -> LightBlue]& /@ chars;
	Grid[Prepend[Transpose[predItems], charItems], Spacings -> {0.6, 0.2}, Dividers -> All, FrameStyle -> Gray]
]



(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2] = "我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
End[];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[]