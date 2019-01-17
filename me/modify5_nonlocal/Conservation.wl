(* ::Package:: *)

Clear["equ1", "equ2", "d", "f", "n", "c", "c1", "d1", "coe1", "coe2", "clist", "dlist", "u", "v"]
Print[Style["Package \"\:6f14\:793a conservation\" is successfully loaded!",Orange,14]]


(* ::Section:: *)
(*\:5b88\:6052\:5f8b\:76f8\:5173*)


(* ::Subsection:: *)
(*\:5b88\:6052\:5f8b\:6c42\:89e3*)


(*
	c1: \:901a\:9879\:516c\:5f0f cn \:7684\:9996\:9879\:5185\:5bb9
	d1: \:901a\:9879\:516c\:5f0f dn \:7684\:9996\:9879\:5185\:5bb9
	coe1: \:5bf9\:5e94\:7cfb\:6570\:7684\:51fd\:6570\:5217\:8868
	coe2: \:5bf9\:5e94\:7cfb\:6570\:7684\:51fd\:6570\:5176\:5b83\:5f62\:5f0f(\:4f8b\:5982\:5171\:8f6d)\:5217\:8868
	n: \:8868\:793a\:7b2c n \:7ec4\:7684\:7ed3\:679c
*)
getGeneralTerm[c1_, d1_, coe1_, coe2_, n_]:=Module[{temp1, temp2, clist, dlist},
	If[n <= 0, Print["n\:8f93\:5165\:4e0d\:5408\:6cd5"],
	If[Length[coe1]!= Length[coe2], Print["\:7cfb\:6570\:5217\:8868\:957f\:5ea6\:4e0d\:7b26\:5408\:8981\:6c42"],
	
	(*\:901a\:9879\:516c\:5f0f C \:7684\:5217\:8868*)
	clist = List[]; 
	(*\:901a\:9879\:516c\:5f0f d \:7684\:5217\:8868*)
	dlist = List[];
	(*\:5217\:8868 clist, dlist \:7684\:586b\:5145\:9879\:ff0c\:4e3a\:4fdd\:6301\:957f\:5ea6\:7edf\:4e00*)
	clist = Append[clist, 0];
	dlist = Append[dlist, 0];
	(*\:5217\:8868 clist, dlist \:7684\:9996\:9879\:5185\:5bb9*)
	clist = Append[clist, c1];
	dlist = Append[dlist, d1];

	For[i=2, i <= n+3, i++,
	temp1 = 1/(2a I) (D[clist[[i]],x]+a \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(m = 0\), \(i - 1\)]\((clist[\([m + 1]\)] clist[\([i - m]\)] coe1[\([1]\)] coe2[\([2]\)] + clist[\([m + 1]\)] dlist[\([i - m]\)] coe2[\([1]\)] coe1[\([2]\)])\)\));
	temp2 = 1/(2a I) (D[dlist[[i]],x]+a \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(m = 0\), \(i - 1\)]\((dlist[\([m + 1]\)] dlist[\([i - m]\)] coe2[\([1]\)] coe1[\([2]\)] + dlist[\([m + 1]\)] clist[\([i - m]\)] coe1[\([1]\)] coe2[\([2]\)])\)\));
	clist = Append[clist, temp1];
	dlist = Append[dlist, temp2];
	]
	Print["\:7b2c ".n." \:9879 c \:662f\:ff1a"]
	Print[Simplify[clist[[n+1]]]];
	Print["\:7b2c ".n." \:9879 d \:662f\:ff1a"]
	Print[Simplify[dlist[[n+1]]]];

	Conservation[clist, dlist, coe1, coe2, n];
]]]

(*
	clist: \:901a\:9879\:516c\:5f0f cn \:7684\:524d n \:9879\:5217\:8868
	dlist: \:901a\:9879\:516c\:5f0f dn \:7684\:524d n \:9879\:5217\:8868
	coe1: \:5bf9\:5e94\:7cfb\:6570\:7684\:51fd\:6570\:5217\:8868
	coe2: \:5bf9\:5e94\:7cfb\:6570\:7684\:51fd\:6570\:5176\:5b83\:5f62\:5f0f(\:4f8b\:5982\:5171\:8f6d)\:5217\:8868
	n: \:8868\:793a\:7b2c n \:7ec4\:7684\:7ed3\:679c
*)
Conservation[clist_, dlist_, coe1_, coe2_, n_]:=Module[{Dlist, Flist, dn, fn},
	If[n <= 0, Print["n \:8f93\:5165\:4e0d\:5408\:6cd5"],
	If[Length[coe1]!= Length[coe2] || Length[clist] != Length[dlist], 
	Print["\:7cfb\:6570\:5217\:8868\:957f\:5ea6\:4e0d\:7b26\:5408\:8981\:6c42"],
	
	(*\:5b88\:6052\:5f8b Dn \:7684\:5217\:8868*)
	Dlist = List[]; 
	(*\:5b88\:6052\:5f8b Fn \:7684\:5217\:8868*)
	Flist = List[];
	(*\:5217\:8868 Dlist, Flist \:7684\:9996\:9879\:5185\:5bb9*)
	Dlist = Append[Dlist, 0];
	Flist = Append[Flist, 0];

	For[i=1, i <= n, i++,
	dn = a(coe1[[1]]coe2[[2]] clist[[n+1]] + coe2[[1]]coe1[[2]] dlist[[n+1]]);
	fn = b[t](coe1[[3]] coe1[[1]] clist[[n+2]] + coe1[[4]] coe1[[1]] clist[[n+1]] +  2/3 coe1[[1]]coe2[[2]] clist[[n+3]]+  2/3 coe1[[2]]coe2[[1]] dlist[[n+3]]-coe2[[3]] coe2[[1]] dlist[[n+2]]+coe2[[4]] coe2[[1]]dlist[[n+1]]);
	Dlist = Append[Dlist, dn];
	Flist = Append[Flist, fn];
	]
	Print["\:7b2c ".n." \:9879 D \:662f\:ff1a"]
	Print[Expand[Dlist[[n+1]]]];
	Print["\:7b2c ".n." \:9879 F \:662f\:ff1a"]
	Print[Expand[Flist[[n+1]]]];
]
]
]





(* ::Subsubsection:: *)
(*\:9a8c\:8bc1\:5b88\:6052\:5f8b*)


(*
	equ1: \:8868\:793a\:591a\:7814\:7a76\:7684\:76ee\:6807\:65b9\:7a0b
	equ2: \:8868\:793a\:76ee\:6807\:65b9\:7a0b\:7684\:5171\:8f6d\:5f62\:5f0f
	d, f: \:8868\:793a\:5b88\:6052\:5f8b\:7684\:8868\:8fbe\:5f0f
	u, v: \:8868\:793a\:65b9\:7a0b\:4e2d\:7684\:539f\:5b50\:51fd\:6570\:548c\:5176\:5171\:8f6d\:51fd\:6570
	n: \:8868\:793a\:7b2c\:51e0\:7ec4\:5b88\:6052\:5f8b
	c: \:8868\:793a\:4e00\:4e2a\:5e38\:6570\:9879
*)
validateConservation[equ1_, equ2_, u_, v_, d_, f_, n_, c_]:=Module[{result},
	If[n != 1 && n != 2, Print["n \:8f93\:5165\:4e0d\:5408\:6cd5"],
	
	result = 0;

	If[n == 1,
		result = Simplify[(D[d,t] - D[f, x]) / (v * equ1 - u * equ2) / c];
	];
	If[n == 2,
		result = Simplify[(D[d,t] - D[f, x]) / ((D[equ2, x] * u - D[equ1, x] * v - D[v, x] * equ1 + D[u, x] * equ2)) / c];
	];
	Print["\:7ed3\:679c\:4e3a". Expand[result]];
	(*Print[Expand[(-D[v, x] * equ1 + D[u, x] * equ2)*c]];*)
	If[result == 1,
		Print["\:9a8c\:8bc1\:6210\:529f"],
		Print["\:9a8c\:8bc1\:5931\:8d25"]
	];
	]
];
