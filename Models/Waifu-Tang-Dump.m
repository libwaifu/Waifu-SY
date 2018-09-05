BeginPackage["Waifu`"]
$TangShiComplex::usage = "生成古诗用字的生僻程度";
WaifuTang::usage = "生成古诗";
Begin["`WaifuSY`"];
$TangShiComplex = 10;
WaifuTang4 = Import["Waifu-Tang-4.WMLF"];
WaifuTang5 = Import["Waifu-Tang-5.WMLF"];
WaifuTang7 = Import["Waifu-Tang-7.WMLF"];
WaifuTang[l_Integer : 5, len_Integer : 4] := Block[
	{net, choose, next, one, head, shi},
	net = NetStateObject@Switch[l, 7, WaifuTang7, 5, WaifuTang5, _, WaifuTang4];
	choose[asc_, str_] := Block[
		{keys = Rest@KeyDrop[asc, Append[{"，", "。", "？", _}, StringTake[str, -1]]]},
		RandomChoice[Sqrt[Values@keys] -> Keys@keys]
	];
	next[str_] := choose[net[str, {"TopProbabilities", $TangShiComplex + 4}], str];
	one[char_] := Nest[StringJoin[#, next[#]]&, Nest[StringJoin[#, next[#]]&, char, l - 1] <> "，", l] <> "。";
	head = choose[net["_", {"TopProbabilities", $TangShiComplex + 1000}], "_"];
	shi = NestList[one@next[StringTake[#, {-1 - l, -2}] <> "，"]&, one@head, len - 1];
	TableForm[Text /@ shi]
];
WaifuTang[l_Integer : 5, len_String] := Block[
	{net, choose, next, one, shi},
	net = NetStateObject@Switch[l, 7, WaifuTang7, 5, WaifuTang5, _, WaifuTang4];
	choose[asc_, str_] := Block[
		{keys = Rest@KeyDrop[asc, Append[{"，", "。", "？", _}, StringTake[str, -1]]]},
		RandomChoice[Sqrt[Values@keys] -> Keys@keys]
	];
	next[str_] := choose[net[str, {"TopProbabilities", $TangShiComplex + 4}], str];
	one[char_] := Nest[StringJoin[#, next[#]]&, Nest[StringJoin[#, next[#]]&, char, l - 1] <> "，", l] <> "。";
	shi = one /@ Characters@len;
	TableForm[Text /@ shi]
];
End[];
SetAttributes[{WaifuTang}, {Protected, ReadProtected}];
EndPackage[];
DumpSave["WaifuTang.mx", {Waifu`WaifuTang, Waifu`$TangShiComplex, Waifu`WaifuSY`WaifuTang4, Waifu`WaifuSY`WaifuTang5, Waifu`WaifuSY`WaifuTang7}]