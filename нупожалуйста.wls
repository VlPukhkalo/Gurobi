#!/usr/bin/env wolframscript
(* ::Package:: *)

(*\:0434\:0435\:043a\:043e\:0440\:0430\:0442\:043e\:0440*)
throwError[f_] := Throw[$Failed, error[f]];
deferror[f_Symbol] := f[___] := throwError[f];


forThreeElement[c_,m_,b_/;MatchQ[b,{{_,_}...}]]:=
{c,m,b,ConstantArray[{0,\[Infinity]},Length@c],Reals}

forThreeElement[c_,m_,b_/;MatchQ[b,{(_?NumberQ) ...}]]:=
{c,m,{#,1}&/@b,ConstantArray[{0,\[Infinity]},Length@c],Reals}

deferror[forThreeElement]


forFourElement[c_,m_,b_,l_Integer|l_Real]:=
forThreeElement[c,m,b][[;;3]]~Join~{ConstantArray[{l,\[Infinity]},Length@c],Reals}

forFourElement[c_,m_,b_,l_/;MatchQ[l,{{_,_}...}]]:=
forThreeElement[c,m,b][[;;3]]~Join~{l,Reals}

forFourElement[c_,m_,b_,l_/;MatchQ[l,{(_?NumberQ) ...}]]:=
forThreeElement[c,m,b][[;;3]]~Join~{{#,\[Infinity]}&/@l,Reals}

deferror[forFourElement]


(*\:043e\:0431\:0440\:0430\:0431\:043e\:0442\:043a\:0430 \:043f\:043e\:0434\:0430\:043d\:043d\:044b\:0445 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 \:043a\:0430\:043a \:0432 LinearProgramming*)

inputProcessing[args__/;Length[{args}]==3]:=
forThreeElement[args]

inputProcessing[args__/;Length[{args}]==4]:=
forFourElement[args]

inputProcessing[args__/;Length[{args}]==5]:=
Most@forFourElement[Sequence@@Most@{args}]~Join~{Last@{args}}

deferror[inputProcessing]


times[0.,_]:=0
times[x_,y_]:=Times[x,y]
deferror[times]


strInner[coef_,vars_]:=ToString@Inner[times,coef,vars,Plus]
deferror[strInner]


Options[MultiObjOptProc]={missionOpt->"Minimize",MultiObjOpt->{PriorityOpt->0,WeightOpt->1,AbsTolOpt->0,RelTolOpt->0}};
MultiObjOptProc[nof_(*\:043a\:043e\:043b-\:0432\:043e \:0446\:0444*),OptionsPattern[]]:=
Module[{curVal,optNames={PriorityOpt,WeightOpt,AbsTolOpt,RelTolOpt},res={}},
If[curVal=OptionValue[MultiObjOpt->#];
AtomQ[curVal],
	AppendTo[res,ConstantArray[curVal,nof]],
(*else*)
AppendTo[res,curVal]
]
&/@optNames; res\[Transpose]]


validGenCons[False]:=True
validGenCons[lst_]:=And@@
{AllTrue[lst[[All,1]],MemberQ[{"MIN","MAX","OR","AND","ABS"},#]&],
AllTrue[lst[[All,2]],Count[#,1]==1\[And]Total@#==1&],
AllTrue[lst,Length@#[[2]]==Length@#[[3]]&],
AllTrue[lst,#[[3,FirstPosition[#[[2]],1][[1]]]]==0&],
AllTrue[Select[lst,#[[1]]=="ABS"&][[All,3]],Count[#,1]==1\[And]Total@#==1&]
}


GenCons[False,vars_]:=""
GenCons[lst_,vars_]:=
StringJoin["GENCONS\n",
	StringJoin[
		Riffle[
			ToString/@Flatten[
				MapAt[DeleteCases[times[#,vars],0]&,#,{{2},{3}}]]
		,"\n    "],
	"\n"]
	&/@lst]


Options[objFun]={missionOpt->"Minimize",MultiObjOpt->{PriorityOpt->0,WeightOpt->1,AbsTolOpt->0,RelTolOpt->0}};
objFun[of_,vars_,opts:OptionsPattern[]]:=
With[
{optResLst=MultiObjOptProc[Length@of,opts]},
If[Depth[of]==2,

TemplateApply[
		StringTemplate[
			"``\n``\n"],
			{OptionValue[missionOpt],
			strInner[of,vars]}],

(*else multi-obj*)
OptionValue[missionOpt]<>" multi-objectives\n"<>
StringJoin@@MapIndexed[
	StringJoin[
		"OBJ",ToString[#2[[1]]],": ",
		#1[[1]],"\n",strInner[#1[[2]],vars],"\n"]&,
{TemplateApply[
		StringTemplate[
			"Priority=`` Weight=`` AbsTol=`` RelTol=``"],
			#]&/@optResLst,of}\[Transpose]]
]]


conditions[A_,RS_,vars_]:=
With[
{signAssoc=<|-1. -> " <= ", 0. -> " = ", 1. -> " >= "|>},
MapIndexed[
	StringJoin[
		"con",ToString[#2[[1]]],": ",
		strInner[#1[[1]],vars],
		signAssoc[#1[[2,2]]],
		ToString@#1[[2,1]],"\n"]&,
{A,RS}\[Transpose]]]
deferror[conditions]


borders[bord_,vars_]:=
StringJoin[
	ToString@#[[2,1]]," <= ",#[[1]]," <= ",ToString@#[[2,2]],"\n"]&/@
({vars,bord}\[Transpose])
deferror[borders]


domain[dom_,vars_]:=Which[dom===Reals," ",
dom===Integers,StringJoin@Riffle[vars," "],
Head[dom]===List,StringJoin@Riffle[Pick[vars,dom,Integers]," "]]
deferror[domain]


variables[OF_]:=
If[Depth[OF]==2,
Table["x"<>IntegerString[i,10,IntegerLength[Length@OF]],{i,Length@OF}],
Table["x"<>IntegerString[i,10,IntegerLength[Length@OF[[1]]]],{i,Length@OF[[1]]}]]
deferror[variables]


(*\:0438\:043d\:0444\:043e\:0440\:043c\:0430\:0446\:0438\:044f, \:043a\:043e\:0442\:043e\:0440\:0430\:044f \:0437\:0430\:043f\:0438\:0441\:044b\:0432\:0430\:0435\:0442\:0441\:044f \:0432 lp*)
Options[rewrite]:=Options[objFun]~Join~{GenConsOpt->False};
rewrite[gm_,opts:OptionsPattern[]]:=
Module[
	{OF,A,RS,bord,dom,il,vars},
	{OF,A,RS,bord,dom}=N@gm;
	vars=variables[OF];
	TemplateApply[
		StringTemplate[
			"``Subject To\n``Bounds\n``\n``Integers\n``\nEnd"],
			{objFun[OF,vars,missionOpt->OptionValue[missionOpt],MultiObjOpt->OptionValue[MultiObjOpt]],
			StringJoin@@conditions[A,RS,vars],
			StringJoin@@borders[bord,vars],
			GenCons[OptionValue[GenConsOpt],vars],
			domain[dom,vars]
	}]
]

deferror[rewrite]


Options[createLPfile]=Options[rewrite]~Join~{lpNameopt->"model.lp"};

(*createLPfile[gm_/;!invalidInputQ[gm],
	directory_String,
	lpName_String:"model.lp",
	opts:OptionsPattern[]]/;Message[CreateLPFile::invinput]:=Null*)

createLPfile[
	gm_,
	directory_String,
	OptionsPattern[]
]:=
With[
	{name="model.txt",lpName=OptionValue[lpNameopt]},
	SetDirectory[directory];
	Export[name,
	rewrite[gm,
		missionOpt -> OptionValue[missionOpt], 
		MultiObjOpt->OptionValue[MultiObjOpt],
		GenConsOpt->OptionValue[GenConsOpt]
		]];
	If[FileExistsQ[lpName],DeleteFile[lpName]];
	RenameFile[name,lpName];
	]
	
deferror[createLPfile]


Options[CreateLPFile]=Options[createLPfile];
CreateLPFile::invinp="Invalid input";

CreateLPFile[args__,opts:OptionsPattern[]]:=
Module[{
res=createLPfile[
inputProcessing[Sequence@@Most@{args}],Last@{args},opts]}, 
If[res===Null,Null,Message[CreateLPFile::invinp]]](*\:043d\:0435 \:0442\:043e*)

deferror[CreateLPFile]


(*\:0434\:043e\:0431\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:043e\:0432*)
Options[setParameter]=
{ResultFileOpt->"model.sol",
MethodOpt->"-1",
TimeLimitOpt-> "Infinity", 
MIPGapOpt->"1e-4", 
BestObjStopOpt->"-Infinity", 
BestBdStopOpt->"Infinity",
FeasRelaxBigMOpt->"1e6",
IterationLimitOpt->"Infinity",
MIPFocusOpt->"0",
SetFullStringOpt->False};

setParameter[lpName_String,OptionsPattern[]]:=
Module[{sfso},
If[sfso=OptionValue[SetFullStringOpt];sfso,Nothing,
(*else*)
TemplateApply[
		StringTemplate[
			"gurobi_cl.exe ResultFile=`` Method=`` TimeLimit=`` MIPGap=`` BestObjStop=`` BestBdStop=`` FeasRelaxBig=`` IterationLimit=`` MIPFocus=`` ``"],
			{OptionValue[ResultFileOpt],
			OptionValue[MethodOpt],
			OptionValue[TimeLimitOpt],
			OptionValue[MIPGapOpt],
			OptionValue[BestObjStopOpt],
			OptionValue[BestBdStopOpt],
			OptionValue[FeasRelaxBigMOpt],
			OptionValue[IterationLimitOpt],
			OptionValue[MIPFocusOpt], 
			lpName
	}],
(*else*)
sfso]]


Options[getSolution]=Options[setParameter];

getSolution[lpName_String,opts:OptionsPattern[]]:=
			Run[setParameter[lpName,opts]]


StringToDoubleKernel[str_]:=Module[{fpRegex,s,sign,mantissa,exponent,int,frac,decimals,a,scale,n,d,sd,r},
(*check the formatting*)fpRegex=RegularExpression["(?i)^[-+]?((((\\d+\\.?)|(\\d*\\.\\d+))(e[-+]?\\d+)?)|(inf(inity)?)|(nan(\\([^)]*\\))?))$"];
If[Length@StringCases[str,fpRegex]==0,Throw["Invalid floating point string format."]];
s=ToLowerCase[str];
(*extract the sign*)sign=1;
If[StringTake[s,1]=="-",sign=-1;s=StringDrop[s,1]];
If[StringTake[s,1]=="+",s=StringDrop[s,1]];
(*check for NaN*)If[StringContainsQ[s,"nan"],Return[{sign,Indeterminate,0}]];
(*check for infinity*)If[StringContainsQ[s,"inf"],Return[{sign,Infinity,0}]];
(*determine the exact value*)If[StringContainsQ[s,"e"],{mantissa,exponent}=StringSplit[s,"e"],{mantissa,exponent}={s,"0"}];
If[StringContainsQ[mantissa,"."],If[StringTake[mantissa,1]==".",{int,frac}={"0",StringDrop[mantissa,1]},If[StringTake[mantissa,-1]==".",
{int,frac}={StringDrop[mantissa,-1],"0"},{int,frac}=StringSplit[mantissa,"."]]],{int,frac}={mantissa,"0"}];
decimals=StringLength[frac];
{int,frac,exponent}=ToExpression[{int,frac,exponent}];
(*the exact value of the input string*)a=(int*10^decimals+frac)*10^(exponent-decimals);
(*underflow if at or below halfway between 0 and 2^-1074*)If[a<=2^-1075,Return[{sign,0,0}]];
(*overflow if at or over 1/2 ULP past 2^971(2^53-1)*)If[a>=2^971*(2^53-1+1/2),Return[{sign,Infinity,0}]];
(*scale'a' into[2^52,2^53) interval*)(*scaling approximaton step based on the fast approx for Log2[a]*)scale=Round[3.3*(IntegerLength[int]-1+exponent)]-52;
If[scale<-1074,scale=-1074];
a*=2^-scale;
(*scaling correction loops*)(*if the exact value is below the interval*)While[a<2^52&&scale>-1074,a*=2;scale-=1];
(*if the exact value is above the interval*)While[a>=2^53,a/=2;scale+=1];
(*significand integer part and reminder*)n=Numerator[a];
d=Denominator[a];
sd=IntegerPart[a];
r=n-sd*d;
(*significand rounding according to IEEE-754 round-half-to-even rule*)If[(2*r>d)||(2*r==d&&OddQ[sd]),sd+=1];
(*if rounding got significand out of[2^52,2^53) interval*)If[sd==2^53,sd=2^52;scale+=1];
(*sign,significand,power of 2 scale bias*)Return[{sign,sd,scale}];]
deferror[StringToDoubleKernel]


StringToDouble[str_]:=N[#1*#2*2^#3]&@@StringToDoubleKernel@str
deferror[StringToDouble]


StringToDoubleBin[str_]:=Module[{sign,sd,scale,signBin,exponentBin,mantissaBin},{sign,sd,scale}=StringToDoubleKernel[str];
signBin=If[sign==1,{0},{1}];
Which[SameQ[sd,Infinity],exponentBin=IntegerDigits[2047,2,11];mantissaBin=IntegerDigits[0,2,52],SameQ[sd,Indeterminate],
exponentBin=IntegerDigits[2047,2,11];mantissaBin=IntegerDigits[1,2,52],sd>=2^52,(*normal*)exponentBin=IntegerDigits[scale+1075,2,11];
mantissaBin=Drop[IntegerDigits[sd,2,53],1],True,(*subnormal*)exponentBin=IntegerDigits[0,2,11];mantissaBin=IntegerDigits[sd,2,52]];
Return[IntegerString[FromDigits[Join[signBin,exponentBin,mantissaBin],2],2,64]];]
deferror[StringToDoubleBin]


StringToDoubleHex[str_]:=IntegerString[FromDigits[StringToDoubleBin[str],2],16,16]
deferror[StringToDoubleHex]


StringToDoubleInt[str_]:=(*\:043e\:0431\:0440\:0430\:0431\:043e\:0442\:043a\:0430 \:0432\:0435\:043d\:0433\:0435\:0440\:0441\:043a\:043e\:0439 \:0437\:0430\:043f\:0438\:0441\:0438 \:0447\:0438\:0441\:0435\:043b*)
Module[{sign,sd,scale,signStr,sdStr,exponentStr},{sign,sd,scale}=StringToDoubleKernel[str];
signStr=If[sign==1,"","-"];
Which[SameQ[sd,Infinity],sdStr="inf";exponentStr="",SameQ[sd,Indeterminate],sdStr="nan";exponentStr="",True,sdStr=ToString[sd];
exponentStr=If[scale!=0,StringJoin["*2^",ToString[scale]],""]];
Return[StringJoin[signStr,sdStr,exponentStr]];]
deferror[StringToDoubleInt]


importProcess[x_]:=With[{xnum=ToExpression@x},xnum/;NumberQ@xnum]
importProcess[x_]:=N@ToExpression@StringToDoubleInt[x]

deferror[importProcess]


importResults::grsnf="No solution can be found that satisfies the constraints.";


Options[importResults]:={getOFValue->False};

importResults[directory_String, solName_String,OptionsPattern[]]:=
	Module[{import=Import[FileNameJoin[{directory,solName}],"Data"],res},
		Which[Head@import===List \[And] import=!={},
				res=importProcess/@
					(StringSplit[#," "][[-1]]&/@
						Select[
							Sort[Rest@import],
							StringPart[#,1]=="x"&]);
				If[OptionValue[getOFValue],
					{importProcess@StringSplit[import[[1]]," = "][[2]],res},res],
			import==={},
				Message[importResults::grsnf],
			True,
				$Failed
				]
		]


invalidInputQ[gm_]:=
With[{ngm=Normal@gm,d=Depth[gm[[1]]]},
(And@@
{MatchQ[ngm ,
	{{{_?NumberQ ...}...}|{_?NumberQ ...}, 
	{{_?NumberQ ...}...},
	{{_?NumberQ,1|-1|0}...},
	{{_?NumberQ|-\[Infinity],_?NumberQ|\[Infinity]}...},
	_List|_}], (*\:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0438\:0435 \:043f\:043e \:0448\:0430\:0431\:043b\:043e\:043d\:0443*)
	(*\:043f\:0440\:043e\:0432\:0435\:0440\:043a\:0430 \:0434\:043b\:0438\:043d*)
If[d==3 (*\:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:043e \:0446\:0444*),
AllTrue[ngm[[1]],Length@#==Length@ngm[[4]]&],
(*1 \:0446\:0444*)
Length@ngm[[1]]==Length@ngm[[4]]
], 
Length@ngm[[2]]==Length@ngm[[3]],
MatchQ[ngm[[2]],{_List ...}],
If[d==3,(*\:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:043e \:0446\:0444*)
AllTrue[ngm[[2]],Length@#==Length@ngm[[1,1]]&],
(*else*)
AllTrue[ngm[[2]],Length@#==Length@ngm[[1]]&]]
})]


Options[CalculateGurobiForList]=Options[createLPfile]~Join~Options[importResults]~Join~Options[setParameter];

CalculateGurobiForList[gm_List, directory_String, opts:OptionsPattern[]]:=
With[{
	spopts=Sequence@@Select[{opts},MemberQ[Options[setParameter][[All,1]],#[[1]]]&],
	lpName=OptionValue[lpNameopt]
		},
		createLPfile[gm,directory,missionOpt->OptionValue[missionOpt],lpNameopt->lpName];
		Run[setParameter[lpName,spopts]];
		importResults[directory,OptionValue[ResultFileOpt],getOFValue->OptionValue[getOFValue]]/;

(*\:043f\:0440\:043e\:0432\:0435\:0440\:043a\:0430*)
(StringMatchQ[lpName,"*.lp"]\[And]
invalidInputQ[gm]\[And]
AnyTrue[{"Minimize","Maximize"},#==OptionValue[missionOpt]&]\[And]
AnyTrue[{True,False},#==OptionValue[getOFValue]&]\[And]
AllTrue[OptionValue[MultiObjOpt->#]&/@{PriorityOpt,WeightOpt,AbsTolOpt,RelTolOpt},NumberQ@#\[Or]Length@#==Length[gm[[1]]]&]\[And]
validGenCons[OptionValue[GenConsOpt]]
)]


Options[CalculateGurobi]=Options[CalculateGurobiForList];

CalculateGurobi[args__,opts:OptionsPattern[]]:=
CalculateGurobiForList[inputProcessing[Sequence@@Most@{args}],Last@{args},opts]


DeleteLP[]:=Quiet@DeleteFile[FileNames["*.lp"]]
DeleteLP[directory_String]:=Quiet@DeleteFile[FileNames["*.lp",directory]]


DeleteSOL[]:=Quiet@DeleteFile[FileNames["*.sol"]]
DeleteSOL[directory_String]:=Quiet@DeleteFile[FileNames["*.sol",directory]]


ClearGurobiDirectory[]:=(DeleteLP[];DeleteSOL[])
ClearGurobiDirectory[directory_String]:=(DeleteLP[directory];DeleteSOL[directory])


GetLastLogging[directory_String]:=
Module[
{path=FileNameJoin[{directory,"gurobi.log"}],log,startstr},
log=Import[path,"List"];
startstr=Quiet@Position[log,_?(StringContainsQ[#,"Gurobi"~~___~~"logging started"]&)][[-1]];
StringJoin@Riffle[log[[startstr[[1]];;]],"\n"]/;
FileExistsQ[path]
]
