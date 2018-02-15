(* ::Package:: *)

Part1:=Quiet@Block[
	{path,valueQ,filelist,imp,paras,compress,save},
	path=FileNameJoin@{NotebookDirectory[],"raw","chinese-poetry-zhCN","poetry"};
	valueQ=Total[ToExpression[StringSplit[#,"."][[{-3,-2}]]]/.{tang->-10!,song->10!}]&;
	filelist=SortBy[FileNames["*",path],valueQ][[1;;-3]];
	imp=StringSplit[Flatten["paragraphs"/.Import[#,"RawJSON"]],{"\:ff0c","\:3002","\:ff1f","\:ff01"}]&;
	paras=imp/@filelist//Flatten;
	(*持久化储存*)
	compress=BinarySerialize[paras,PerformanceGoal->"Size"];
	SetDirectory[NotebookDirectory[]];
	save=OpenWrite["Tableaux_A.data", BinaryFormat->True];
	BinaryWrite[save,compress];Close[save];
];
data=ByteArray[BinaryReadList[FileNameJoin@{NotebookDirectory[],"Tableaux_A.data"}, "Byte"]];
gat=GatherBy[DeleteCases[BinaryDeserialize@data,Removed["$$Failure"]],StringLength];
ass=Association[Rule@@@Transpose@{#,Range@Length@#}&[StringLength/@(gat[[All,1]])]];
att[long_]:=Association[Function[strlist,StringTake[strlist[[1]],1]->strlist]/@GatherBy[gat[[ass@long]],StringTake[#,1]&]];
UtahimeTableaux[long_:7,str_]:=Flatten[RandomSample[#,1]&/@(att[long]/@StringPartition[str,1])];
