#!/usr/bin/env wolframscript
Part1:=Quiet@Block[
	{path,valueQ,filelist,imp,paras,compress,save},
	path=FileNameJoin@{NotebookDirectory[],"raw","chinese-poetry-zhCN","poetry"};
	valueQ=Total[ToExpression[StringSplit[#,"."][[{-3,-2}]]]/.{tang->-10!,song->10!}]&;
	filelist=SortBy[FileNames["*",path],valueQ][[1;;-3]];
	imp=StringSplit[Flatten["paragraphs"/.Import[#,"RawJSON"]],{"，","。","？","！"}]&;
	paras=imp/@filelist//Flatten;
	(*持久化储存*)
	compress=BinarySerialize[paras,PerformanceGoal->"Size"];
	SetDirectory[NotebookDirectory[]];
	save=OpenWrite["Tableaux_A.data", BinaryFormat->True];
	BinaryWrite[save,compress];Close[save];
];