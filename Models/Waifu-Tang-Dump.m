(* ::Package:: *)

System`SerializeDefinitions = ResourceFunction["BinarySerializeWithDefinitions"];
SetDirectory@NotebookDirectory[];
<< DeepMath`;


app = DeepMath`NetApplication@<|
	"Name" -> "Tang Poetry Generator trained on Custom",
	"Input" -> "Normal Distribution Vector<512>",
	"Example" -> Inactive[RandomVariate][NormalDistribution[], 512],
	"Date" -> DateString[],
	"Handler" -> FunctionRepository`WaifuSY`Tang`handlerFunction,
	"Models" -> <|
		"Tang4" -> Import["Waifu-Tang-4.WMLF"],
		"Tang5" -> Import["Waifu-Tang-5.WMLF"],
		"Tang7" -> Import["Waifu-Tang-7.WMLF"]
	|>,
	MetaInformation -> <|
		"$TangShiComplex" -> 10
	|>
|>



FunctionRepository`PGGANTrainedOnCelebA`handlerFunction[dict_Association, other___] := Scope[
	WaifuTang4 = dict["Models", "Tang4"];
	WaifuTang5 = dict["Models", "Tang5"];
	WaifuTang7 = dict["Models", "Tang7"];
	
	
	Options[WaifuTang] = {"$TangShiComplex" -> 10};
	WaifuTang[l_Integer : 5, len_Integer : 4, OptionsPattern[]] := (
		$TangShiComplex = OptionValue["$TangShiComplex"];
		net = NetStateObject@Switch[l, 7, WaifuTang7, 5, WaifuTang5, _, WaifuTang4];
		choose[asc_, str_] := Block[
			{keys = Rest@KeyDrop[asc, Append[{"\:ff0c", "\:3002", "\:ff1f", _}, StringTake[str, -1]]]},
			RandomChoice[Sqrt[Values@keys] -> Keys@keys]
		];
		next[str_] := choose[net[str, {"TopProbabilities", $TangShiComplex + 4}], str];
		one[char_] := Nest[StringJoin[#, next[#]]&, Nest[StringJoin[#, next[#]]&, char, l - 1] <> "\:ff0c", l] <> "\:3002";
		head = choose[net["_", {"TopProbabilities", $TangShiComplex + 1000}], "_"];
		shi = NestList[one@next[StringTake[#, {-1 - l, -2}] <> "\:ff0c"]&, one@head, len - 1];
		TableForm[Text /@ shi]
	);
	WaifuTangHead[l_Integer : 5, len_String, OptionsPattern[]] := (
		$TangShiComplex = OptionValue["$TangShiComplex"];
		net = NetStateObject@Switch[l, 7, WaifuTang7, 5, WaifuTang5, _, WaifuTang4];
		choose[asc_, str_] := Block[
			{keys = Rest@KeyDrop[asc, Append[{"\:ff0c", "\:3002", "\:ff1f", _}, StringTake[str, -1]]]},
			RandomChoice[Sqrt[Values@keys] -> Keys@keys]
		];
		next[str_] := choose[net[str, {"TopProbabilities", $TangShiComplex + 4}], str];
		one[char_] := Nest[StringJoin[#, next[#]]&, Nest[StringJoin[#, next[#]]&, char, l - 1] <> "\:ff0c", l] <> "\:3002";
		shi = one /@ Characters@len;
		TableForm[Text /@ shi]
	);
	
	
	main[args_]:=(
		
		
		
		
		Message[DeepMath`NetApplication::illInput]
	);
	
	
]


Export[
FileBaseName@NotebookFileName[] <>".app", 
Unevaluated[BinaryDeserialize@System`SerializeDefinitions[app]], 
"WXF"
]
