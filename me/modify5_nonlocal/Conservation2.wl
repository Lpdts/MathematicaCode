(* ::Package:: *)

Clear["equ1", "equ2", "d", "f", "n", "c"]
Print[Style["Package \"\:6f14\:793a conservation\" is successfully loaded!",Orange,14]]


(* ::Section:: *)
(*\:5b88\:6052\:5f8b\:76f8\:5173*)


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
validateConservation[equ1_, equ2_, u_, v_, d_, f_, n_, c_]:=Module[{result, m},
	If[n != 1, Print["n \:8f93\:5165\:4e0d\:5408\:6cd5"],
	
	result = 0;

	If[n == 1,
		result = Simplify[(D[d,t] - D[f, x]) / (v * equ1 - u * equ2) / c];
	];
	If[n == 2,
		result = Simplify[(D[d,t] - D[f, x])/(D[equ2, x] * u -D[equ1, x] * v + D[v, x] * equ1 + D[u, x] * equ2) / c];
	];
	Print["\:7ed3\:679c\:4e3a". result];
	If[result == 1,
		Print["\:9a8c\:8bc1\:6210\:529f"],
		Print["\:9a8c\:8bc1\:5931\:8d25"]
	];
	]
];
