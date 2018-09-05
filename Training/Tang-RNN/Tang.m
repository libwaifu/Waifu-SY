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
BeginPackage["Tang`"];
(* ::Section:: *)
(*函数说明*)
Tang7::usage = "";
Tang7Line::usage = "";
Tang7LineRandom::usage = "";
PrepareDataTang::usage = "PrepareData[char_, f_ : 3]";
jsonNetPlot::usage = "";
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

Options[jsonNetPlot] = {
	"ShowTensors" -> True,
	"VertexLabels" -> Placed["ID", Above],
	"VertexOrder" -> Automatic,
	"EdgeBundling" -> False,
	"OutputTensors" -> None,
	"InternalDimensions" -> None,
	Rotate -> False,
	GraphLayout -> "LayeredDigraphDrawing"
};
jsonNetPlot[file_File, OptionsPattern[]] := Block[
	{
		showTensors, vertexLabels, vertexOrder, edgeBundling, outputTensors, plot,
		internalDimensions, nodes, argnodes, heads, MXNetLink`Visualization`PackagePrivate`$oldids, nameStrings, typeStrings,
		edges, nodeOps, longRange, opTypes, opNames, nullType, blank, maxIndex,
		name, nodeDims, edgeTooltips, nops, opStyles, opSizes, vertexTypeData, labels, infoGrids, nnodes
	},
	{
		showTensors, vertexLabels, vertexOrder, edgeBundling, outputTensors, internalDimensions
	} = OptionValue @ {
		"ShowTensors", "VertexLabels", "VertexOrder", "EdgeBundling", "OutputTensors", "InternalDimensions"
	};
	expr = MXNetLink`MXSymbolToJSON@MXNetLink`MXSymbolFromJSON@file;
	{nodes, argnodes, heads} = Lookup[expr, {"nodes", "arg_nodes", "heads"}, GeneralUtilities`Panic[]];
	MXNetLink`Visualization`PackagePrivate`$oldids = If[ListQ[vertexOrder],
		Map[Function[GeneralUtilities`IndexOf[vertexOrder, #name] - 1], nodes],
		Range[Length[nodes]] - 1
	];
	nodes = Map[Append[#, "inputs1" -> (Part[#inputs, All, 1] + 1)]&, nodes];
	nameStrings = Map[MXNetLink`Visualization`PackagePrivate`toVertexLabelString[#name]&, nodes];
	typeStrings = Map[MXNetLink`Visualization`PackagePrivate`toVertexTypeString[#op]&, nodes];
	AddTo[argnodes, 1];
	AddTo[heads, 1];
	edges = Apply[Join,
		MapIndexed[
			If[
				SameQ[#op, "null"],
				Nothing,
				If[showTensors,
					Thread @ Prepend[#2, #inputs1],
					Thread @ Prepend[#2, Complement[#inputs1, argnodes]]
				]
			]&,
			nodes
		]
	];
	edges = DeleteDuplicates @ edges;
	nodeOps = nodes[[All, "op"]];
	If[And[edgeBundling, !FreeQ[nodeOps, "Concat" | "SliceChannel"]],
		longRange = MXNetLink`Visualization`PackagePrivate`pickLongRangeEdges[edges, nodes],
		longRange = None;
	];
	{opTypes, opNames} = GeneralUtilities`Labelling @ nodeOps;
	nullType = GeneralUtilities`IndexOf[opNames, "null"];
	nodes = MapIndexed[Function[Append[#, "id" -> (First[#2] - 1)]],
		nodes
	];
	If[showTensors && ListQ[outputTensors],
		opTypes = Join[opTypes, ConstantArray[nullType, Length @ outputTensors]];
		argnodes = Join[argnodes, Range[Length[outputTensors]] + Max[edges]];
		nameStrings = Join[nameStrings, outputTensors];
		blank = ConstantArray["", Length @ outputTensors];
		MXNetLink`Visualization`PackagePrivate`$oldids = Join[MXNetLink`Visualization`PackagePrivate`$oldids, blank];
		typeStrings = Join[typeStrings, blank];
		nodes = Join[nodes, blank];
		maxIndex = Max @ edges;
		MapIndexed[AppendTo[edges, {First @ #, First[#2] + maxIndex}]&, heads]
	];
	edgeTooltips = If[
		SameQ[internalDimensions, None],
		None,
		nodeDims = Table[
			name = Internal`UnsafeQuietCheck[nodes[[i, "name"]], None];
			MXNetLink`Visualization`PackagePrivate`toDimLabel @ Lookup[
				internalDimensions, name, If[StringQ[name],
					Lookup[internalDimensions, StringJoin[name, "_output"], None],
					None
				]
			],
			{i, Length @ nodes}
		];
		((Part[nodeDims, #]&) @@@ edges) /. BatchSize -> "b"
	];
	nops = Length @ opNames;
	opStyles = Map[MXNetLink`Visualization`PackagePrivate`opColor, opNames];
	opSizes = ReplacePart[ConstantArray[6, nops], nullType -> 4];
	opStyles = ReplacePart[opStyles, nullType -> Gray];
	opNames = opNames /. "null" -> "Tensor";
	vertexTypeData = <|"VertexStyles" -> opStyles|>;
	If[showTensors,
		vertexTypeData = Join[vertexTypeData, <|"VertexSizes" -> opSizes|>]
	];
	labels = ReplaceAll[vertexLabels,
		{"Name" :> nameStrings, "ID" :> MXNetLink`Visualization`PackagePrivate`$oldids, "Type" :> typeStrings}
	];
	infoGrids = Map[MXNetLink`Visualization`PackagePrivate`nodeInfoGrid, nodes];
	nnodes = Length @ nodes;
	plot = GeneralUtilities`LayerPlot[edges,
		"VertexLabels" -> labels, "HiddenVertices" -> If[showTensors, None, argnodes],
		"VertexTypeData" -> vertexTypeData, "VertexTypeLabels" -> opNames, "MaximumImageSize" -> None,
		"VertexSizes" -> 4, "EdgeTooltips" -> edgeTooltips,
		"BaseLabelStyle" -> {FontSize -> 7}, "LayoutMethod" -> OptionValue[GraphLayout],
		"DuplicateInputVertices" -> True, If[showTensors, "VertexTypes" -> opTypes, "VertexStyles" -> opTypes],
		"LongRangeEdges" -> longRange, "Rotated" -> OptionValue[Rotate], "ArrowShape" -> "Chevron", "LegendLabelStyle" -> 8
	];
	Magnify[plot, 1.5]
]







$TangShiComplex = 10;

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
End[];
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
EndPackage[]