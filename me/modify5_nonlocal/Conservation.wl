(* ::Package:: *)

Clear["coe1","coe2","coe3","equ1","equ2","equ3","i","j","k","equ","X","T","A","u","v","c0","d0","n"]
Print[Style["Package \"\:6f14\:793a conservation\" is successfully loaded!",Orange,14]]


(* ::Section:: *)
(*\:6c42\:5b88\:6052\:5f8b*)


(* ::Subsection:: *)
(*\:5b88\:6052\:5f8b\:6c42\:89e3*)


(*
	\:8f93\:51fa ss \:65b9\:7a0b\:7b2c n \:7ec4\:7684\:5b88\:6052\:5f8b
	c0\:ff1a\:7cfb\:6570 cn \:7684\:7b2c\:4e00\:9879
	d0\:ff1a\:7cfb\:6570 dn \:7684\:7b2c\:4e00\:9879
	n\:ff1a\:7b2c n \:7ec4\:7684\:5b88\:6052\:5f8b\:ff0c\:8981\:6c42 n \:5fc5\:987b\:5927\:4e8e 0
*)
Conservation[c0_, d0_,  n_]:=Module[{equ1, equ2, clist, dlist},
	If[n <= 0, Print["n\:8f93\:5165\:4e0d\:5408\:6cd5"],
	
	clist = List[];
	dlist = List[];
	clist = Append[clist, 0];
	dlist = Append[dlist, 0];
	clist = Append[clist, c0];
	dlist = Append[dlist, d0];
	Subscript[k, 11][t]=0;
	Subscript[k, 12][t]=Subscript[k, 12];
	Subscript[k, 21][t]=0;
	Subscript[k, 22][t]=\[Integral](Subscript[\[Alpha], 6][t])\[DifferentialD]t;
	k[x,t]= Exp[ I Subscript[k, 11][t]x+Subscript[k, 12][t]x+I Subscript[k, 21][t]+Subscript[k, 22][t]];
	SuperStar[k][-x,t]= Exp[ I Subscript[k, 11][t]x-Subscript[k, 12][t]x-I Subscript[k, 21][t]+Subscript[k, 22][t]];
	(*Print[n];*)
	For[i=2, i <= n+3, i++,
	(*Print[clist[[i]]];*)
	equ1 = 1/(2a I) (D[clist[[i]],x]+a \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(m = 0\), \(i - 1\)]\((clist[\([m + 1]\)] clist[\([i - m]\)] k[x, t]\ 
\(\*SuperscriptBox[\(u\), \(*\)]\)[\(-x\), t] + clist[\([m + 1]\)] dlist[\([i - m]\)] 
\(\*SuperscriptBox[\(k\), \(*\)]\)[\(-x\), t]\ u[x, t])\)\));
	equ2 = 1/(2a I) (D[dlist[[i]],x]+a \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(m = 0\), \(i - 1\)]\((dlist[\([m + 1]\)] dlist[\([i - m]\)] 
\(\*SuperscriptBox[\(k\), \(*\)]\)[\(-x\), t]\ u[x, t] + dlist[\([m + 1]\)] clist[\([i - m]\)] k[x, t]\ 
\(\*SuperscriptBox[\(u\), \(*\)]\)[\(-x\), t])\)\));
	clist = Append[clist, equ1];
	dlist = Append[dlist, equ2];
	]
	Print["\:7b2c ".n." \:9879 c \:662f\:ff1a"]
	Print[Simplify[clist[[n+1]]]];
	Print["\:7b2c ".n." \:9879 d \:662f\:ff1a"]
	Print[Simplify[dlist[[n+1]]]];

	Subscript[A, 2]=(I (Subscript[k, 12] SuperStar[u][-x,t]-
\!\(\*SuperscriptBox[\((
\*SuperscriptBox[\(u\), \(*\)])\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[-x,t]))/(3 a);
	SuperStar[Subscript[A, 2]]=(I (u[x,t] Subscript[k, 12] -
\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]))/(3 a);
	Subscript[A, 4]=-(1/(6 a^2))(9 a^2 C2 SuperStar[u][-x,t]-Subscript[k, 11][t]^2 SuperStar[u][-x,t]+2 I Subscript[k, 11][t] Subscript[k, 12][t] SuperStar[u][-x,t]+Subscript[k, 12][t]^2 SuperStar[u][-x,t]+4 a^2 E^(2 I x Subscript[k, 11][t]+2 Subscript[k, 22][t]) u[x,t] SuperStar[u][-x,t]^2-2 I Subscript[k, 11][t] 
\!\(\*SuperscriptBox[\((
\*SuperscriptBox[\(u\), \(*\)])\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[-x,t]-2 Subscript[k, 12][t] 
\!\(\*SuperscriptBox[\((
\*SuperscriptBox[\(u\), \(*\)])\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[-x,t]+
\!\(\*SuperscriptBox[\((
\*SuperscriptBox[\(u\), \(*\)])\), 
TagBox[
RowBox[{"(", 
RowBox[{"2", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[-x,t]);
	SuperStar[Subscript[A, 4]]=-(1/(6 a^2))(-9 a^2 C2 u[x,t]+u[x,t] Subscript[k, 11][t]^2-u[x,t] Subscript[k, 12][t]^2-4 a^2 E^(2 I x Subscript[k, 11][t]+2 Subscript[k, 22][t]) u[x,t]^2 SuperStar[u][-x,t]+2 Subscript[k, 12][t] 
\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]+2 I (u[x,t] Subscript[k, 11][t] Subscript[k, 12][t]-Subscript[k, 11][t] 
\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"1", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t])-
\!\(\*SuperscriptBox[\(u\), 
TagBox[
RowBox[{"(", 
RowBox[{"2", ",", "0"}], ")"}],
Derivative],
MultilineFunction->None]\)[x,t]);
	Dn = a(k[x,t] SuperStar[u][-x,t] clist[[n+1]] + SuperStar[k][-x,t] u[x,t] dlist[[n+1]]);
	Fn = b[t](Subscript[A, 2] k[x,t] clist[[n+2]] + Subscript[A, 4] k[x,t] clist[[n+1]] + 2/3 k[x,t] SuperStar[u][-x,t]clist[[n+3]]+ 2/3 u[x,t]SuperStar[k][-x,t] dlist[[n+3]]-SuperStar[Subscript[A, 2]] SuperStar[k][-x,t] dlist[[n+2]]+SuperStar[Subscript[A, 4]] SuperStar[k][-x,t] dlist[[n+1]]);
	Print["\:7b2c ".n." \:9879 D \:662f\:ff1a"]
	Print[Expand[Dn]];
	Print["\:7b2c ".n." \:9879 F \:662f\:ff1a"]
	Print[Expand[Fn]];
]
]



(* ::Subsection::Closed:: *)
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
