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
(* ::Section:: *)
(*函数说明*)
Tang7::usage = "";
Tang7Line::usage = "";
Tang7LineRandom::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExNumber::usage = "程序包的说明,这里抄一遍";
Begin["`Tang`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$Tang = "V1.0";
Updated$Tang = "2018-08-28";
(* ::Subsubsection:: *)
(*功能块 1*)
$TangShiComplex = 10;
WaifuTang7 = Import["WaifuSY-Tang-7.WMLF"]

Tang7Line[in_List] := TableForm[Text@*Tang7Line /@ in];
Tang7Line[char_String] := (
	If[StringLength@char >= 8, Return[]];
	Nest[# <> WaifuTang7@#&, char, 16 - StringLength@char]
);
Tang7LineRandom[in_List] := TableForm[Text@*Tang7LineRandom /@ in];
Tang7LineRandom[char_String] := Block[
	{state, next, head, top},
	If[StringLength@char >= 8, Return[]];
	state = NetStateObject[WaifuTang7];
	next[str_, r_ : 50] := (
		top = Association @@ state[str, {"TopProbabilities", r}];
		top = KeyDrop[top, Characters["，。？兮" <> StringTake[str, -1]]];
		str <> RandomChoice[Values@top -> Keys@top]
	);
	head = Nest[next[#, 5$TangShiComplex]&, char, 7 - StringLength@char];
	Nest[next[#, $TangShiComplex]&, head <> "，", 7] <> "。"
];
Options[Tang7] = {Line -> 8, Riffle -> True};
Tang7[char_, OptionsPattern[]] := Block[
	{next, shi, line},
	line = OptionValue[Line];
	next = RandomChoice@Rest[Characters@StringDelete[#, {"，", "。"}]]&;
	shi = NestList[Tang7LineRandom@*next, Tang7LineRandom[char], line - 1];
	TableForm[Text /@ Riffle[shi, "", 5]]
];



(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2] = "我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
]
End[]