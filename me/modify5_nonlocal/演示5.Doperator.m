(* ::Package:: *)

Clear["coe1","coe2","coe3","equ1","equ2","equ3","i","j","k","equ","X","T","A","u","v"]
Print[Style["Package \"\:6f14\:793a5.operator\" is successfully loaded!",Orange,14]];


(* ::Section:: *)
(*\:6c42\:602a\:6ce2\:89e3*)


(* ::Subsection:: *)
(*\:53d8\:91cf\:53d8\:6362*)


(*
	\:7531coe1\:548citem1\:7ec4\:6210\:7684\:65b9\:7a0b1\:53d8\:6362\:4e3a\:7531coe2\:548citem1\:7ec4\:6210\:7684\:65b9\:7a0b2
	coe1\:ff1a\:65b9\:7a0b1\:7684\:5404\:9879\:7cfb\:6570\:7ec4\:6210\:7684\:5217\:8868
	coe2\:ff1a\:65b9\:7a0b2\:7684\:5404\:9879\:7cfb\:6570\:7ec4\:6210\:7684\:5217\:8868
	item1\:ff1a\:65b9\:7a0b1\:548c\:65b9\:7a0b2\:5404\:9879\:7684\:51fd\:6570\:90e8\:5206\:7ec4\:6210\:7684\:5217\:8868
	item2\:ff1aitem1\:53d8\:91cf\:66ff\:6362\:540e\:7684\:5f62\:5f0f
*)
DeReplace[coe1_,coe2_,item1_,item2_]:=Module[{equ1,equ2,coe3,constrants,i,j,k},
equ1=0;
equ2=0;
coe3=List[];
constrants=List[];
For[i=1,i<Length[coe1]+1,i++,
equ1=equ1+coe1[[i]]*item1[[i]];
];
Print["equ1\:ff1a"];
Print[equ1];
u[x,t]=A U[X[x,t],T[t]];
v[x,t]=A V[X[x,t],T[t]];
X[x,t]=a x + b ;
a2[t]=m a5[t];


\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[u[x,t],x];


\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[u[x,t],t];


\!\(\*SuperscriptBox[\(v\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[v[x,t],x];


\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"2", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[u[x,t],{x,2}];


\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"3", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[u[x,t],{x,3}];
equ3=Expand[equ1];
For[j=1,j<Length[item2]+1,j++,
coe3=Append[coe3,Coefficient[equ3,item2[[j]]]];
]
Print["\:53d8\:91cf\:53d8\:6362\:540e\:7684\:7cfb\:6570\:ff1a"];
Print[coe3];
flag=coe3[[1]]/coe2[[1]];
For[k=2,k<Length[coe2]+1,k++,
constrants=Append[constrants,coe3[[k]]==coe2[[k]]*flag]
]
Print["\:7ea6\:675f\:6761\:4ef6\:ff1a"];
Print[constrants];
NSolve[constrants,{Derivative[1][T][t],A,a,b}]
]



(* ::Subsection:: *)
(*\:9a8c\:8bc1\:602a\:6ce2\:89e3*)


(*
	\:5c06\:65b9\:7a0b1\:5230\:65b9\:7a0b2\:7684\:5909\:91cf\:5909\:6362\:7684\:7ed3\:679c\:5e94\:7528\:5230\:65b9\:7a0b2\:7684\:602a\:6ce2\:89e3\:4e0a\:53ef\:4ee5\:5f97\:5230\:65b9\:7a0b1\:7684\:602a\:6ce2\:89e3
	X T A\:5206\:522b\:662f\:4e0a\:9762\:5909\:91cf\:5909\:6362\:6c42\:5f97\:7684\:7ed3\:679c
	equ\:4e3a\:65b9\:7a0b1\:ff0c\:5373\:6700\:540e\:6211\:4eec\:8981\:9a8c\:8bc1\:602a\:6ce2\:89e3\:7684\:65b9\:7a0b
*)
DeRogue[X_,T_,A_,equ_]:=Module[{c,k},
d=1;
c=1;
k=1;
a2[t]=m a5[t];
K=1+3k;
u0[x,t]=-A c/(2d) Exp[-I/(2d) (k X-w/(4d) T)];
v0[x,t]=-A c/(2d) Exp[I/(2d) (k X-w/(4d) T)];
w=2c^2 (1+3k)-k^2-k^3;
G=(d^2 (9c^2+2K^2))/(2c^2 K^2) (a+(4k1^2 k2^2)/(9c^2+2K^2) T)^2+(8d^2 k1^2 k2^2 (k1^2-k2^2))/(c^2 (9c^2+2K^2)) T^2+(162d^6 (9c^2+2K^2)^3)/(c^2 k1^2 k2^2 (k1^2+k2^2)^2);
H=1/(2K) (a^2/4+k1^2 k2^2 T^2-(243d^4 (9c^2+2K^2)^2)/(k1^2 k2^2 (18c^2+K^2)))(a+(4k1^2 k2^2)/(9c^2) T)+(16d^4 k1^2 k2^2 (27c^2+2K^2))/(c^4 K(18c^2+K^2)) T;
B=1/(36d^2) (a^2/4+k1^2 k2^2 T^2+(18^2 d^4 (k1^4-k1^2 k2^2-k2^4))/(k1^2 k2^2 (k1^2+k2^2)))^2+(9d^2 k2^2)/(k1^2+k2^2)^2 (a+2k1^2 T)^2+(108^2 d^6)/(k2^2 (k1^2+k2^2));
a=(K^2-1-27c^2)T+12d X;
k1=Sqrt[2]/2 (Sqrt[K^2 (18c^2+K^2)]-9c^2+K^2)^(1/2);
k2=Sqrt[2]/2 (Sqrt[K^2 (18c^2+K^2)]+9c^2-K^2)^(1/2);
p[x,t]=u0[x,t](1-(G+I H)/B);
q[x,t]=v0[x,t](1-(G-I H)/B);


\!\(\*SuperscriptBox[\(p\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[p[x,t],x];


\!\(\*SuperscriptBox[\(p\), 
TagBox[
RowBox[{"(", 
RowBox[{"0", ",", "1"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[p[x,t],t];


\!\(\*SuperscriptBox[\(q\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[q[x,t],x];


\!\(\*SuperscriptBox[\(p\), 
TagBox[
RowBox[{"(", 
RowBox[{"2", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[p[x,t],{x,2}];


\!\(\*SuperscriptBox[\(p\), 
TagBox[
RowBox[{"(", 
RowBox[{"3", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]=D[p[x,t],{x,3}];
Simplify[equ]
]
