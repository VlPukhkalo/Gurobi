#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Section:: *)
(*Installing *)


(* ::Input:: *)
(*directory = "";*)


(* ::Input:: *)
(*<<""*)


(* ::Section:: *)
(*Usage*)


(* ::ExampleText:: *)
(*Minimize x+y, subject to constraint x+2 y>=3 and implicit non-negative constraints:*)


(* ::Input:: *)
(*GurobiOptimization[{1,1},{{1,2}},{{3,1}},{{0,\[Infinity]},{0,\[Infinity]}},Reals,directory]*)


(* ::ExampleText:: *)
(*Maximize x+2y, subject to constraints x+2 y>=3,  1.5x-2 y=10, -x+20 y<=30 and lower bounds x>=-1, y>=-1:*)


(* ::Input:: *)
(*GurobiOptimization[{1,2},{{1,2},{1.5,-2},{-1,20}},{{3,1},{10,0},{30,-1}},{{-1,\[Infinity]},{-1,\[Infinity]}},Reals,directory,missionOpt->"Maximize"]*)


(* ::ExampleText:: *)
(*Solve the same problem, but with the first variable an integer:*)


(* ::Input:: *)
(*GurobiOptimization[{1,2},{{1,2},{1.5,-2},{-1,20}},{{3,1},{10,0},{30,-1}},{{-1,\[Infinity]},{-1,\[Infinity]}},Integers,directory]*)


(* ::ExampleText:: *)
(*Minimize x^2+2y, subject to constraints x+ y>=-3,  2.7x- y<=10 and  bounds 1<=x<=10, -2<=y<=5:*)


(* ::Input:: *)
(*GurobiOptimization[{{1,{2,0}},{2,{0,1}}},{{1,1},{2.7,-1}},{{-3,1},{10,-1}},{{1,10},{-2,5}},Reals,directory]*)


(* ::ExampleText:: *)
(*Maximize -5x + y^2 , subject to constraints x+ y=10  and upper bounds x<=20, y<=100 with getting OF value:*)


(* ::Input:: *)
(*GurobiOptimization[{{-5,{1,0}},{1,{0,2}}},{{1,1}},{{10,1}},{{-\[Infinity],20},{-\[Infinity],100}},Reals,directory,missionOpt->"Maximize",getOFValue->True]*)


(* ::ExampleText:: *)
(*Minimize 5x-y, subject to constraints x^2+ y>=3,   \!\(TraditionalForm\`x + \ y >= 15\)  and  and implicit non-negative integers constraints. Note that if the problem is Non-Convex, then the NonConvexOpt should be set to "2":*)


(* ::Input:: *)
(*GurobiOptimization[{5,1},{{1,1}},{{15,1}},{{0,\[Infinity]},{0,\[Infinity]}},Integers,directory,*)
(*QuadrConsOpt->{{{{1,{2,0}},{-1,{0,1}}}},{{15,1}}}]*)


(* ::Input:: *)
(*GurobiOptimization[{5,1},{{1,1}},{{15,1}},{{0,\[Infinity]},{0,\[Infinity]}},Integers,directory,*)
(*QuadrConsOpt->{{{{1,{2,0}},{-1,{0,1}}}},{{15,1}}},NonConvexOpt->"2" ]*)


(* ::ExampleText:: *)
(*Minimize 5x-y and y+3x , subject to constraints x+2 y>=6,   \!\(TraditionalForm\`y - \ x <= 15\)  and  and implicit non-negative integers constraints (the weights of the objective functions are by default equal to one):*)


(* ::Input:: *)
(*GurobiOptimization[{{5,-1},{1,3}},{{1,2},{1,-1}},{{6,1},{15,-1}},{{0,\[Infinity]},{0,\[Infinity]}},Integers,directory ]*)


(* ::ExampleText:: *)
(*Solve the same problem, but with weights equal to 5 and 1, respectively:*)


(* ::Input:: *)
(*GurobiOptimization[{{5,-1},{1,3}},{{1,2},{1,-1}},{{6,1},{15,-1}},{{0,\[Infinity]},{0,\[Infinity]}},Integers,directory ,MultiObjOpt->{WeightOpt->{5,1}}]*)
