(* ::Package:: *)

(* *********************************************************************** *)

(* Code updated: Sunday, January 25, 2009 at 22:10 in Boulder by Hereman *)
(* Previously updated on Friday, January 2 at 10:00AM by Baldwin in Arvada *)

(* : Title: PainleveTest.m (Version 2) *)
(* : Authors: Douglas Baldwin and Willy Hereman *)

(* : Paper: Symbolic Software for the Painleve Test of Nonlinear Ordinary
             and Partial Differential Equations
   : Published:
     Journal of Nonlinear Mathematical Physics 13(1) (2006) 90-110.       *)

(* : Based on Master of Science Project of Douglas Baldwin *)
(* : Supervisor: Prof. Willy Hereman
     Department of Mathematical and Computer Sciences
     Colorado School of Mines, Golden, Colorado, U.S.A. *)

(* : Code tested for Mathematica 3.0, 4.0, 4.1, 5.0, 5.1, and 6.0 *)

(* : Summary:
     This software package performs the Painleve test of nonlinear ODEs
     and PDEs.
     The code implements the three part ARS (Ablowitz, Ramani and Segur)
     algorithm to determine if a system of nonlinear ordinary or partial
     differential equations has the Painleve property.

     The Painleve test is broken into three main parts:

      i.   Find the dominant behavior;
      ii.  Find the resonances;
      iii. Find the constants of integration (coefficients in the
           Laurent series) and determine the compatibility conditions.   *)

(* The Backus-Naur form of the function is:
  <Main Function> -> PainleveTest[<Equations>, <Functions>,
        <Variables>, <Options>]
  <Equations> -> <Equation> | {<Equation>,...}
  <Functions> -> <Function> | {<Function>,...}
  <Variables> -> <Variable> | {<Variable>,...}
  <Options> -> Verbose -> <Boolean> |
        KruskalSimplification -> <Variable> |
        DominantBehaviorMin -> <Negative Integer> |
        DominantBehaviorMax -> <Integer> |
        DominantBehavior -> <List of Rules> |
        DominantBehaviorConstraints -> <List of Constraints> |
        DominantBehaviorVerbose -> <Range> |
        ResonancesVerbose -> <Range> |
        ConstantsOfIntegrationVerbose -> <Range>
  <Boolean> -> True | False
  <Range> -> 0 | 1 | 2 | 3
  <List of Rules> ->
        {{alpha[1] -> <Integer>,alpha[2] -> <Integer>,...},...}
  <List of Constraints> -> {alpha[1] == alpha[2],...}
*)
(* The output of the function is:
  {{{Dominant behavior}, {Resonances},
        {{Laurent series coefficients},{Compatibility conditions}}}, ... }
*)

BeginPackage["Calculus`PainleveTest`"]

Unprotect[PainleveTest];

PainleveTest::usage =
"PainleveTest[eqn, u[x], x, opts] performs the standard Painleve test of a "<>
"single nonlinear ordinary differential equation.
PainleveTest[eqn, u[x1, x2, ... ], {x1, x2, ... }, opts] "<>
"performs the standard Painleve test of a single nonlinear partial "<>
"differential equation.
PainleveTest[{eqn1, eqn2, ... }, {u1[x], u2[x], ... }, x, opts] "<>
"performs the standard Painleve test of a system of nonlinear ordinary "<>
"differential equations.
PainleveTest[{eqn1, eqn2, ... }, {u1[x,t], u2[x,t], ... }, {x,t}, opts] "<>
"performs the standard Painleve test of a system of nonlinear partial "<>
"differential equations.
PainleveTest has a number of options:
Verbose -> True gives a detailed trace of the steps of the algorithm;
KruskalSimplification -> x2 will use g[x1, x2, ... ] = x2 - h[x1, x3, ... ] "<>
"instead of g[x1, x2, ... ];
Parameters -> {\[Beta]} specifies that \[Beta] is a non-zero parameter;
DominantBehaviorMax -> i specifies that any free dominant exponents, "<>
" say \!\(\[Alpha]\_1\), will be incremented in integer steps from "<>
"-3 up to i, where i is an Integer given by the user (the default is -1);
DominantBehaviorMin -> k specifies that if a minimum value for the free "<>
"dominant exponent, "<>
"say \!\(\[Alpha]\_1\), cannot be determined, then \!\(\[Alpha]\_1\) will "<>
"be incremented by integer steps starting from k up to DominantBehaviorMax. "<>
"k is the minimum of all fixed dominant exponents and the user-specified "<>
" value of i, which must be non-positive (the default is -3);
DominantBehavior -> {{alpha[1] -> -2, alpha[2] -> -1}, ... } "<>
"forces the code to use the specified dominant behavior;
DominantBehaviorVerbose -> j, ResonancesVerbose -> j, "<>
"and ConstantsOfIntegrationVerbose -> j will give additional information "<>
"about the steps of the computation in increasing detail as j is increased "<>
"(j = 0, 1, 2 or 3), default is j = 0."

PainleveTest`dominantBehavior::freevalues =
"The solution(s) `1` do not fix all the \!\(\[Alpha]\_i\). "<>
"The algorithm will now generate all possible values for the free "<>
"\!\(\[Alpha]\_i\) up to the user specified DominantBehaviorMax "<>
"which is set to -1 by default, but DominantBehaviorMax can be any "<>
"non-negative integer. "

PainleveTest`dominantBehavior::underdetermined =
"There is too much freedom in choosing the values of \!\(\[Alpha]\_i\). "<>
"The algorithm will now generate all possible values for the free "<>
"\!\(\[Alpha]\_i\) starting from the minimum of the list of all fixed "<>
"values \!\(\[Alpha]\_i\) but the list is augmented with the "<>
"user-specified DominantBehaviorMin (with default -3) "<>
"up to the user-specified DominantBehaviorMax (with default -1). "<>
"DominantBehaviorMin can be any non-positive integer and "<>
"DominantBehaviorMax can be any non-negative integer."

PainleveTest::FaileddominantBehavior =
"The algorithm could not find a valid dominant behavior for the system. "<>
"Please check this result by hand."

PainleveTest::FailedInitialConstantsOfIntegration =
"The algorithm could not find valid initial constants of integration."<>
"This is likely the result of finding a dominant behavior that was negative,"<>
" but required one (or more) of the u[i,0] to be zero."

PainleveTest::FailedResonances =
"The algorithm could not find a valid set of resonances for the system. "<>
"Please check this result by hand."

PainleveTest::overdetermined =
"There is no freedom at level(s) `1`, even though the algorithm requires "<>
"freedom at this level. Please check the system by hand at that level."

PainleveTest::KruskalSimplificationFalse =
"The value of the option KruskalSimplification must be one of the "<>
"independent variables."

PainleveTest::KruskalSimplificationNonpolynomial =
"Kruskal's simplification can only be used if the coefficients are at "<>
"worst polynomial in `1`."

PainleveTest::KruskalSimplificationVariable =
"It is strongly recommended that you use Kruskal's simplification with `1` "<>
"instead of `2`."

Options[PainleveTest] =
  { KruskalSimplification -> {},
    Verbose -> False,
    Parameters -> {},
    DominantBehaviorMin -> -3,
    DominantBehaviorMax -> -1,
    DominantBehavior -> {},
    DominantBehaviorConstraints -> {},
    DominantBehaviorVerbose -> 0,
    ResonancesVerbose -> 0,
    ConstantsOfIntegrationVerbose -> 0
  };

Begin["`Private`"]

If[$VersionNumber < 4,
   Attributes[Internal`DeactivateMessages] = {HoldAll};
   Internal`DeactivateMessages[body_, msgs___MessageName] :=
      Module[{wasOn = Select[Hold[msgs], Head[#] =!= $Off &], result},
         CheckAbort[
            Scan[Off, wasOn];
            result = body;
            Scan[On, wasOn];
            result,
            Scan[On, wasOn];
            Abort[]
         ]
      ] (* end module for version 4 *)
];

(* ------------------------------------------------------------------------- *)

(* Extra debug statement. *)
debugwilly1 = False;
debugwilly2 = False;

(* The code first wraps equations, functions and variables into lists. *)
PainleveTest[equations_, functions_, variables_, options___] :=
  PainleveTest[
    If[ Head[equations]===List, equations, {equations}],
    If[ Head[functions]===List, functions, {functions}],
    If[ Head[variables]===List, variables, {variables}],
    options
  ] /; ( FreeQ[equations, Power[__, _Symbol]] &&
         FreeQ[equations, Power[E, __]] );

(* Now that everything is in the correct form, we start the main function. *)
PainleveTest[
    eqns_List,
    functions_List,
    variables_List,
    options___? OptionQ ] /;
    ( FreeQ[eqns, Power[__, _Symbol]] && FreeQ[eqns, Power[E, __]] ) :=
  Module[{ painleveTestDebug = False, (* The debug Boolean. *)
           (* DB:09/22/2006 Applying PainleveTest`Simplify to fix bug. *)
           equations = PainleveTest`Simplify[eqns /. Equal[a_,b_] :> a-b],
           myTrackingVariableMax, (* The number of tracking variables. *)
           thedominantBehavior,   (* The result of step 1. *)
           theinitialConstantsOfIntegration, (* Used in step 2. *)
           theResonances, (* The result of step 2. *)
           theConstantsOfIntegration (* The result of step 3. *)
         }, (* Protected Local Variables *)

    (* WH:10/02/2006 begin print equations (before and after simplification) *)
    If[painleveTestDebug || debugwilly1,
      Print["The equations before simplification:"];
      Print[ CleanUpFunction[ eqns ] ];
      Print["The equations after simplification:"];
      Print[ CleanUpFunction[ (# == 0)& /@ equations ] ]
    ];

    If[ painleveTestDebug || (Verbose /. {options} /. Options[PainleveTest]),
      (* WH:10/02/2006 print the common denominators that are cancelled *)
      Module[{denominatoreqns}, (* Protected Local Variables *)
        denominatoreqns =
          Table[Factor[equations[[k]]/(eqns[[k]] /. Equal[a_,b_] :> a-b)],
            {k,1,Length[eqns]}];
        If[FreeQ[Union[Map[NumberQ,denominatoreqns]], False] === False,
          Print["The equations were multiplied by the denominators: "];
          Print[ CleanUpFunction[ denominatoreqns ] ]
        ];
      ]; (* WH:10/02/2006 end print statements *)
      Print["The Painleve test is carried out for the following system: "];
      Print[
       TableForm[
         pdeForm[ # == 0 & /@ equations ]
       ]
      ];
    ];

    (* If the equations are ODEs, then the Laurent series is in *)
    (* g[x] = x - h[], where x is the independent variable. *)
    (* Thus, we must substitute any x by g[x] + h[]. *)
    (* start if 100 *)
    If[Length[variables] === 1,
      equations =
        Module[{x},
          (* Define x as the independent variable of the ODE. *)
          x = First[variables];
          (* Replace every instance of x by g[x] + h[] since g[x] = x - h[].*)
          equations =
            equations /. x -> g[x] + h[]; (* DB:07/15/2008 Changed - to +. *)
          (* Any function which is u[g[x] + h[]] replace with u[x]. *)
          equations =
            Expand[equations] /.
              { u_[g[x] + h[]] :> u[x],
                Derivative[n_][u_][g[x] + h[]] :> Derivative[n][u][x]
              }
        ]; (* end small module under if statement *)
      (* start if 101 *)
      If[painleveTestDebug ||
          (Verbose /. {options} /. Options[PainleveTest]),
        Print["The given system is an ODE."];
        (* WH:01/03/2009, BugFix *)
        (* g = x - h[t] = z - z0, --> replaced a " + " by " - " below *)
        Print["The Laurent series is expressed in powers of "<>
          "g(",First[variables],") = ",First[variables]," - h[t] "<>
          "= ",First[variables]," - ",First[variables],"0. "];
        Print["The Laurent series has constant coefficients."];
        Print["If ",First[variables]," appears explicitly in the ODE,"<>
          " we replace ",First[variables],
          " with ",CleanUpFunction[ g[First[variables]] + h[] ],
          " in the equation to get:"];
        Print[
          TableForm[
            pdeForm[ # == 0 & /@ CleanUpFunction[equations] ]
          ]
        ]
      ]; (* end of if 101 *)
    ]; (* end of if 100 *)

    (* DB:01/02/2009 Added error messages for KruskalSimplification. *)
    If[debugwilly2,
      Print["At point AAAA1, before checking, the equations are: "];
      Print[ CleanUpFunction[ equations ] ]
    ];
    If[{} =!= (KruskalSimplification /. {options} /. Options[PainleveTest]),
      Module[{x = (KruskalSimplification /. {options} /. Options[PainleveTest])
          }, (* Protected Local Variables *)
        (* Check that x is one of the independent variables of the DE. *)
        If[FreeQ[ variables, x],
          Message[PainleveTest::KruskalSimplificationFalse];
          Abort[];
        ];
        (* Check that the coefficients are at most polynomials in x. *)
        If[
          ! FreeQ[ (* Replaces the functions, x, x^k with 1 and checks that *)
              Replace[ (* what remains is free of x.  *)
                Flatten[
                  ( Flatten[equations /. {Plus -> List, Times -> List}] /.
                    ((# -> 1)& /@ functions) ) /.
                      (Derivative[__][Head[#]][__] -> 1& /@ functions)
                ], {x^_?IntegerQ -> 1, x -> 1}, 1],
            x],
          Message[PainleveTest::KruskalSimplificationNonpolynomial,x];
          Abort[];
        ];
        (* Checks that the highest order derivative is in x. *)
        Module[{temp = {}}, (* Protected Local Variables *)
          equations /. Derivative[a__][_][b__] :>
            (temp = Append[temp, Transpose[{{a},{b}}]];1);
          temp = Sort[Partition[Flatten[temp],2]][[-1,2]];
          If[ temp =!= x,
            Message[PainleveTest::KruskalSimplificationVariable,temp,x];
          ];
        ];
      ]; (* end small module under if statement *)
    ];

    (* DB:12/31/2008 If KruskalSimplification -> x, then any x explicitly *)
    (* appearing in the coefficients of the PDE is replaced by *)
    (* g[x,y,z,...,t] + h[y,z,...,t] --- just as in the ODE case. *)
    If[debugwilly2,
      Print["At point AAAA2, after checking, before possible modification,"];
      Print[" the equations are: "];
      Print[ CleanUpFunction[ equations ] ]
    ];
    If[{} =!= (KruskalSimplification /. {options} /. Options[PainleveTest]),
      equations =
        Module[{x,temp,temp2},
          (* Sets x as the independent variable used in KruskalSimplification. *)
          x = (KruskalSimplification /. {options} /. Options[PainleveTest]);
          If[painleveTestDebug ||
              (Verbose /. {options} /. Options[PainleveTest]),
            Print["With the KruskalSimplification, the characteristic "<>
              "manifold is ",
              CleanUpFunction[ g[Sequence @@ variables] ]," = ",
              CleanUpFunction[
                x - h[Sequence @@ (Complement[variables,{x}])] ] ]
          ];
          (* Define temp to be g[x,y,z,...,t] + h[y,z,...,t] *)
          temp = g[Sequence @@ variables]
            + h[Sequence @@ (Complement[variables,{x}])];
          If[painleveTestDebug ||
              (Verbose /. {options} /. Options[PainleveTest]),
            Print["Since the KruskalSimplification is being used, any ",x,
              " explicitly appearing in the coefficients of any equation "<>
              "is replaced with ",
              CleanUpFunction[ temp ]," to give: "]
          ];
          (* Sets temp2 to be {g[x,y,z,...,t] + h[y,z,...,t],y,z,...,t} *)
          temp2 = variables /. x -> temp;
          (* Replaces all x by g[x,y,z,...,t] + h[y,z,...,t]. *)
          equations =
            equations /. x -> temp;
          (* Replaces any u[g[x,y,z,...,t] + h[y,z,...,t],y,z,...,t] *)
          (* by u[x,y,z,...,t]. *)
          equations =
            Expand[equations] /.
              { u_[Sequence @@ temp2] :> u[Sequence @@ variables],
                Derivative[n_][u_][Sequence @@ temp2] :>
                  Derivative[n][u][Sequence @@ variables]
              }
        ]; (* end small module for equation modification *)
      If[debugwilly2,
        Print["At point AAAA3, after modification, the equations are: "];
        Print[ CleanUpFunction[ equations ] ]
      ];
      If[painleveTestDebug ||
          (Verbose /. {options} /. Options[PainleveTest]),
        Print[ TableForm[ pdeForm[ # == 0 & /@
          CleanUpFunction[ equations ]]]]
      ];
    ];

    (* Compute the dominant behavior. *)
    thedominantBehavior =
      Internal`DeactivateMessages[
        dominantBehavior[
          equations,
          functions,
          variables,
          options
        ],
        Solve::svars
      ];

    If[ Length[ thedominantBehavior ] === 0,
      Message[PainleveTest::FaileddominantBehavior];
      Abort[];
    ];

    If[painleveTestDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["The dominant behaviors (including minimal degrees in g) are: "];
      Print[ CleanUpFunction[ thedominantBehavior ] ];
    ];

    theinitialConstantsOfIntegration =
      Internal`DeactivateMessages[
        ( Sequence @@
            initialConstantsOfIntegration[
              equations,
              functions,
              variables,
              #,
              options
            ]
        ) & /@ thedominantBehavior,
        Solve::svars
      ];

    If[ Length[ theinitialConstantsOfIntegration ] === 0,
      Message[PainleveTest::FailedInitialConstantsOfIntegration];
      Abort[];
    ];

    If[painleveTestDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["FIRST SUMMARY: Dominant behaviors and first constants of "<>
            "integration (u[i,0]): "];
      Print[
        pdeForm[
          CleanUpFunction[ theinitialConstantsOfIntegration ]
        ]
      ];
      Print["*************************************************************"]
    ];

    theResonances =
      Release[
        Internal`DeactivateMessages[
          resonances[
            equations,
            functions,
            variables,
            #[[1]],
            #[[2]],
            options
          ]& /@ theinitialConstantsOfIntegration,
          Solve::svars
        ]
      ];

    If[ Length[ theResonances ] === 0,
      Message[PainleveTest::FailedResonances];
      Abort[];
    ];

    If[painleveTestDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["SECOND SUMMARY: leading order behaviors and corresponding "<>
            "resonances: "];
      Print[
        pdeForm[
          CleanUpFunction[ theResonances ]
        ]
      ];
      Print["*************************************************************"]
    ];

    theConstantsOfIntegration =
      Internal`DeactivateMessages[
        constantsOfIntegration[
          equations,
          functions,
          variables,
          #[[1]],
          #[[2]],
          #[[3]],
          options
        ]& /@ theResonances,
        Solve::svars
      ];

(* WH:05/21/2008, added a MapAll Factor *)
(* to simplify the solutions (and put them on common denominators) *)
If[debugwilly1,
   Print["At point YYY1, before applying MapAll Factor, "<>
         "theConstantsOfIntegration: "];
   Print[ CleanUpFunction[ theConstantsOfIntegration ] ];
   theConstantsOfIntegration = MapAll[Factor, theConstantsOfIntegration];
   Print["At point YYY2, after applying MapAll Factor,"<>
         " theConstantsOfIntegration: "];
   Print[ CleanUpFunction[ theConstantsOfIntegration ] ]
  ];

    If[Length[variables] === 1,
      theConstantsOfIntegration =
        theConstantsOfIntegration /.
          { g :> Function[{z}, z],
            h[] -> ToExpression[ToString[First[variables]] <> "0"] }
    ];

    If[painleveTestDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["*************************************************************"];
      Print["FINAL SUMMARY: dominant behaviors, resonances, constants of "<>
            "integration, and (if applicable) compatibility conditions: "];
      Print["Each sublist will have a list of alphas (and alphaMins), "<>
            " followed by a list with resonances. In turn, followed by "<>
            " a nested list of coefficients u[i,k]. The final list "<>
            " contains the compatibility condition(s).  That list "<>
            " will be empty ({}) if there are no compatibility conditions. "];
      Print[
        pdeForm[
          CleanUpFunction[ theConstantsOfIntegration ]
        ]
      ];
      Print["End of FINAL SUMMARY."];
      Print["*************************************************************"]
    ];
If[debugwilly1,
   Print["Leaving the function PainleveTest."]
  ];
    Return[
      CleanUpFunction[
        theConstantsOfIntegration /. (alphaMin[_] -> _) -> Sequence[]
      ]
    ]
  ]; (* end of module PainleveTest *)

(* ------------------------------------------------------------------------- *)

(* The input of the dominantBehavior function is:

    - System of equations (Form: List of polynomial equations)
    - Functions (Form: List of dependent variables, e.g. {u[x,t], v[x,t]})
    - Variables (Form: List of independent variables, e.g. {x,t})
    - Options (Form: Options)
*)
(* The output of the dominantBehavior function is:
   { List of dominant exponents (e.g., {alpha[1] -> -1, alpha[2] -> -3}), ...}
*)
(* The primary functions called by dominantBehavior are:

   - dominantBehavior`GenerateSystem
   - dominantBehavior`ListFormation
   - dominantBehavior`Simplification
   - dominantBehavior`RulesSolver
   - dominantBehavior`PowerSolver
   - dominantBehavior`SystemCleanUp
   - dominantBehavior`FixFreeAlpha
*)

dominantBehavior[ _List, _List, _List, options___? OptionQ ] :=
  ToExpression[
    StringReplace[
      ToString[ DominantBehavior /. {options}],
      "alpha" -> "Calculus`PainleveTest`Private`alpha"
    ]
  ] /; ( DominantBehavior /. {options} /. Options[PainleveTest] ) =!= {};

dominantBehavior[
    equations_List,
    functions_List,
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ dominantBehaviorDebug = (* The debug Boolean. *)
           (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 0,
           theSystem, (* The equations after substitution of the ansatz. *)
           myTrackingVariableMax, (* Largest tracking variable index. *)
           alphaList0,  (* The exponents of g[x,t] (before simplification). *)
           alphaList,   (* The exponents of g[x,t] (after simplification). *)
           myAlphaList, (* List of alpha to be solved. *)
           alphaRules, (* List of potential dominant exponent relationships. *)
           alphaSoln0, (* List of possible dominant exponents (pre-prune). *)
           alphaSoln   (* List of possible dominant exponents (post-prune). *)
         }, (* Protected Local Variables *)
If[debugwilly1,
   Print["Entering the function dominantBehavior."]
  ];
    (* Adds tacking variables which will be used in the dominantBehavior *)
    (* function to check for false balances coming from a single term *)
    (* instead of from (at least) two different terms *)
    {theSystem, myTrackingVariableMax} =
      attachTrackingVariables[equations];

    If[dominantBehaviorDebug,
      Print[
        "After introducing the tracking variables, the system is: "];
      Print[
        pdeForm[
          CleanUpFunction[
            theSystem
          ]
        ]
      ];
    ];

    theSystem =
      dominantBehavior`GenerateSystem[
        theSystem /. Equal[ a_, b_ ] :> a-b,
        functions,
        variables,
        options
      ];

    If[dominantBehaviorDebug,
      Print[
      "After setting all tracking variables to 1, that system is: "];
      Print[
        pdeForm[
          CleanUpFunction[
            theSystem /. myTrackingVariable[_] -> 1
          ]
        ]
      ];
    ];

    alphaList0 =
      dominantBehavior`ListFormation[
        theSystem,
        functions,
        variables,
        myTrackingVariableMax,
        options
      ];

    If[dominantBehaviorDebug,
      Print["Before simplification, the exponents of g are: "];
      Print[ CleanUpFunction[ alphaList0 ] ];
    ];

    (* Removes expressions which cannot be the highest power *)
    (* by myTrackingVariable. *)
    alphaList =
      Flatten[
        dominantBehavior`Simplification[#, options]& /@ #
      ]& /@ alphaList0;

    If[dominantBehaviorDebug,
      Print["After simplification, the exponents of g are: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];

    (* Forms a list of all alpha_i. *)
    myAlphaList = {};
    alphaList /.
      alpha[i_Integer] :>
        (myAlphaList = Append[myAlphaList, alpha[i]]; alpha[i]);
    myAlphaList = Union[myAlphaList];

    If[dominantBehaviorDebug,
      Print["The list of \!\(\[Alpha]\_i\) (to be solved for): "];
      Print[ CleanUpFunction[ myAlphaList ] ];
    ];

    (* Solves the expressions (involving alpha_i) for alpha_i *)
    alphaRules =
      dominantBehavior`RulesSolver[#, myAlphaList, options]& /@ alphaList;

    If[dominantBehaviorDebug,
      Print["The iterative solver for \!\(\[Alpha]\_i\) finds: "];
      Print[ CleanUpFunction[ alphaRules ] ];
    ];

    (* Uses the previous results to determine *)
    (* explicit solutions for alpha_i. *)
    alphaSoln0 =
      dominantBehavior`PowerSolver[alphaRules, myAlphaList, options];

    If[dominantBehaviorDebug,
      Print["Before pruning, the possible solutions for \!\(\[Alpha]\_i\) "<>
            "are: "];
      Print[ CleanUpFunction[ alphaSoln0 ] ];
    ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
 Print["AT POINT 01, alphaSoln0 before Join: "];
 Print[ CleanUpFunction[ alphaSoln0 ] ]
];

    (* DB:05/15/2004 Forgot to add Join. *)
    (* WH:06/13/2006 Added Union to remove duplicates. *)
    alphaSoln0 =
      Union[Join[
        alphaSoln0,
        dominantBehavior`FixFreeAlpha[
          alphaSoln0,
          myAlphaList,
          options
         ]
       ]
      ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
  Print["AT POINT 02, alphaSoln0 after Join and Union, "];
  Print["but before remove bad solutions: "];
  Print[ CleanUpFunction[ alphaSoln0 ] ]
];

    (* Remove bad solutions. *)
    alphaSoln  =
      dominantBehavior`SystemCleanUp[
        alphaList,
        alphaSoln0,
        myAlphaList,
        options
      ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
  Print["AT POINT 03, alphaSoln after remove bad solutions: "];
  Print[ CleanUpFunction[ alphaSoln ] ]
];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
 Print["AT POINT 04, this Complement[Union[Sort /@ alphaSoln0], alphaSoln]: "];
 Print[ CleanUpFunction[ Complement[Union[Sort /@ alphaSoln0], alphaSoln] ] ]
];

    (* Warn the user when potential solutions are removed. *)
    If[Length[Complement[Union[Sort /@ alphaSoln0], alphaSoln] ] > 0,
      StylePrint[
        "The potential solutions "<>
        ToString[
          InputForm[
            CleanUpFunction[
              Complement[Union[Sort /@ alphaSoln0], alphaSoln]
            ]
          ]
        ] <>
  " are being removed because there still is freedom, or they are greater "<>
  "than DominantBehaviorMax, or fail to balance highest exponent terms from "<>
  "(at least) two different terms in the given system. If \!\(\[Alpha]\_i\) "<>
  "> 0, then transformations like u -> 1/v, 1/(v^2), etc., could result in "<>
  "a system that might pass the Painleve test. ",
  "Message"
      ];
    ];

    If[dominantBehaviorDebug,
      Print["After pruning, the solutions for \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
   Print["AT POINT 05, this is alphaSoln: "];
   Print[ CleanUpFunction[ alphaSoln ] ]
];

    (* If the algorithm does not find any solutions, it quits. *)
    If[Length[alphaSoln] === 0,
      StylePrint[
        "The algorithm failed while attempting to find negative values "<>
        "of \!\(\[Alpha]\_i\). The list of rules constraining the system are "
        <> ToString[ InputForm[ CleanUpFunction[alphaRules] ] ] <>
        ". The original exponents in \!\(\[Alpha]\_i\) are " <>
        ToString[ InputForm[ CleanUpFunction[alphaList] ] ] <>
        ". The code will now test all possible solutions in the "<>
        "range DominantBehaviorMin (with default -3) to "<>
        "DominantBehaviorMax (with default -1). "
        "IMPORTANT: The code may miss branches! It is prudent to "<>
        "experiment with other integer choices for DominantBehaviorMin "<>
        "and DominantBehaviorMax!", "Message"
      ];

      (*
         Generate all possible solutions from given DominantBehaviorMin
         (with default value -3) to given DominantBehaviorMax
         (with default value -1).
      *)
      alphaSoln =
        dominantBehavior`SystemCleanUp[
          alphaList,
          dominantBehavior`GenerateAlternativeSolutions[
            myAlphaList,
            options
          ],
          myAlphaList,
          options
        ];

    ];

    If[ Length[alphaSoln] === 0,
      Abort[ ]
    ];

    If[dominantBehaviorDebug,
      Print[
        "The final solutions for \!\(\[Alpha]\_i\) (to be returned) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];

    (* Return the alphaSoln along with the alphaMin values. *)
    alphaSoln =
      Join[#,
        Table[alphaMin[i] -> Min[alphaList[[i]] /. #],
          {i, Length[equations]}
        ]
      ]& /@ alphaSoln;

    (* Returns the solutions. *)
    If[debugwilly1,
      Print["Leaving the function dominantBehavior."]
      ];
    Return[alphaSoln]
  ]; (* end of module dominantBehavior *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* Attaches tracking variables of the form myTrackingVariable to each *)
(* term in the given system of equations.  The output is in the form: *)
(* { system, maximum tracking variable} *)

attachTrackingVariables[
    eqns_List,
    options___? OptionQ
  ] :=
  Module[{ equations, (* Local version of equations. *)
           i = 0 (* Local iterator. *)
         }, (* Protected Local Variables. *)
  If[debugwilly1,
      Print["Entering the function attachTrackingVariables."]
     ];
    equations =
      If[Head[#] === Plus,
        Plus @@ ((myTrackingVariable[++i]*#) & /@ List @@ #),
        myTrackingVariable[++i]*#
      ] & /@ Expand[eqns];
   If[debugwilly1,
      Print["Leaving the function attachTrackingVariables."]
     ];
   Return[{equations, i}]
  ]; (* end of module attachTrackingVariables *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* GenerateSystem substitutes the ansatz for the dominant behavior into the *)
(* given system of equations. *)

dominantBehavior`GenerateSystem[
    equations_List,
    functions_List,
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ systemGenerationDebug = (* The debug Boolean. *)
           (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
           ansatzRules, (* Rules that define the ansatz. *)
           theSystem    (* Local version of the equations. *)
         }, (* Protected Local Variables *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`GenerateSystem."]
    ];
    ansatzRules =
      RuleDelayed @@ #& /@
        Table[
          { Head[ functions[[n]] ],
            Function[variables,
              Evaluate[ u[n,0]*g[Sequence @@ variables]^alpha[n] ]
            ]
          },
          {n, Length[ functions ] }
        ];

    If[ Verbose /. {options} /. Options[PainleveTest],
      Print["**************************************************************"];
      Print["Computation of THE DOMINANT BEHAVIORS."]
    ];

    If[systemGenerationDebug,
      Print["The ansatz rules (for determining the dominant behavior) are: "];
      Print[ CleanUpFunction[ ansatzRules ] ];
    ];

    theSystem =
      MapAll[ Expand, equations /. ansatzRules ];

    If[systemGenerationDebug,
     Print[
     "After substituting the ansatz (for dominant behavior), the system is: "];
     Print[ CleanUpFunction[ theSystem ] ];
    ];
    If[debugwilly1,
       Print["Leaving the function dominantBehavior`GenerateSystem."]
      ];
    Return[ theSystem ]
  ]; (* end of module dominantBehavior`GenerateSystem *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* ListFormation pulls off the exponents of g and puts them into a list. *)

dominantBehavior`ListFormation[
    equations_List,
    functions_List,
    variables_List,
    myTrackingVariableMax_Integer,
    options___? OptionQ
  ] :=
  Module[{ listFormationDebug = (* The debug Boolean. *)
           (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
           theSystem
         }, (* Protected Local Variables *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`ListFormation."]
    ];
    theSystem =
      ( Table[Coefficient[#, myTrackingVariable[i] ],
          {i, 1, myTrackingVariableMax}
        ]& /@ equations) /. 0:>Sequence[];

    If[listFormationDebug,
      Print["After splitting by tracking variable, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Breaking up expressions into lists of terms. *)
    theSystem =
      If[Head[#]===Plus, List @@ #, {#}]& /@ #& /@
        MapAll[Expand, theSystem];

    If[listFormationDebug,
      Print["After Plus is replaced by {}, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Pulls off the exponents of g and forms a list *)
    (* of expressions of the form {{{1+m[1]},{..}..}..}  *)
    (* DB:04/18/2004 moved Union. *)
    alphaList =
      ( Union[
          Exponent[#,
            g[Sequence @@ variables]
          ]
        ]& /@
      #)& /@ theSystem;

    (* DB:02/15/2004 *)
    alphaList = alphaList /. {0} :> Sequence[];

    If[listFormationDebug,
      Print["The exponents of g (of the various terms in the system) are: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];
    If[debugwilly1,
       Print["Leaving the function dominantBehavior`ListFormation."]
      ];
   (* Returns the number of equations and the list of exponents. *)
   Return[alphaList]
  ]; (* end of module dominantBehavior`ListFormation *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

dominantBehavior`Simplification[
    alphaList0_List,
    options___? OptionQ
  ]:=
  Module[{alphaListSimplificationDebug = (* The debug Boolean. *)
          (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
          alphaList, (* The list of exponents. *)
          alphaListStructure (* Structure of exponents. *)
         }, (* Protected Local Variables. *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`Simplification."]
    ];
    (* The following lines breaks up the list of exponents of g *)
    (* and then removes invalid cases. *)

    (* Breaking up expressions of a + b*alphi_i + c*alpha_j +... *)
    (* where a,b,c,...,i,j,k,...\in\mathbb{R} into lists. *)
    alphaList =
      If[Head[#]===Plus,
        List @@ #,
        If[Head[#]===alpha || Head[#]===Times,
          {#},
          {#,0} (* DB:11/14/2005 # -> {0,#} *)
        ]
      ]& /@ alphaList0;

    If[alphaListSimplificationDebug,
      Print["Splitting expressions of the form "<>
            "a + b*\!\(\[Alpha]\_i\) + c*\!\(\[Alpha]\_j\) + ... "<>
            "into lists {a, b*\!\(\[Alpha]\_i\), ... }, yields: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];

    (* The following routine strips off the constant (a) in the above *)
    (* expression and leaves only the underlying structure. *)
    alphaListStructure = Union[alphaList /. {a_Integer, b__}:>{b}];

    If[alphaListSimplificationDebug,
      Print["After removing the constant pieces (a), the list becomes: "];
      Print[ CleanUpFunction[ alphaListStructure ] ];
    ];

    (* Re-organizes the list of exponents of g by the structure *)
    (* listed above. *)
    alphaList =
      Cases[alphaList, {_, Sequence @@ #} | #]& /@ alphaListStructure;

    If[alphaListSimplificationDebug,
      Print["Reorganized by structure, the list of exponents is: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];

    (* Determines the maximum a in each power of g *)
    alphaList =
      {Min[# /. {a_, ___}:>If[IntegerQ[a], a, 0]& /@ #]}& /@ alphaList;

    If[alphaListSimplificationDebug,
      Print["After removing constants which cannot lead to highest "<>
            "exponents, the list of exponents is: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];

    (* Creates a list of the maximum exponents of g, *)
    (* such that all the members of the list are of the form *)
    (* a_{max} + b*alpha_i + c*alpha_j + ... + d*alpha_i*alpha_j *)
    alphaList =
      (Plus @@ Flatten[#])& /@
        Transpose[{alphaList, alphaListStructure}];

    If[alphaListSimplificationDebug,
      Print["After reassembling the exponents in the correct form, "<>
            "the list of exponents is: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];
  If[debugwilly1,
     Print["Leaving the function dominantBehavior`Simplification."]
    ];
    (* Returns the simplified list. *)
    Return[alphaList]
 ]; (* end of module dominantBehavior`Simplification *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

dominantBehavior`RulesSolver[
    alphaList0_,
    myAlphaList_,
    options___? OptionQ
  ]:=
  Module[ {rulesSolverDebug = (* The debug Boolean. *)
          (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
          alphaList, eqnList, alphaRules (* List of rules from first solve. *)
         }, (* Protected local variables. *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`RulesSolver."]
    ];
    (* Makes sure we are working with the simplest list possible *)
    (* for combinatorial purposes. *)
    alphaList =
      Union[dominantBehavior`Simplification[alphaList0] ];

    If[rulesSolverDebug,
      Print["The exponents involving \!\(\[Alpha]\_i\) "<>
            "(entering the Exponent Solver) is: "];
      Print[ CleanUpFunction[ alphaList ] ];
    ];

    (* Forms a list of equations to be solved for alpha_i. *)
    eqnList =
      Flatten[
        Map[
          Thread[
            Table[#[[1]], {Length[#] -1} ] == Drop[#, 1]
          ]&,
          Table[Drop[alphaList, i],
            {i, 0, Length[alphaList] - 2}
          ]
        ],
        1
      ];

    If[rulesSolverDebug,
      Print["The equations (to be solved first for \!\(\[Alpha]\_i\)) are: "];
      Print[ CleanUpFunction[ eqnList ] ];
    ];

    (* Does the first run of solving. *)
    alphaRules =
      Union[
        Flatten[
          Solve[#, myAlphaList]& /@ eqnList
        ]
      ];

    If[rulesSolverDebug,
      Print["The first set of solutions (for \!\(\[Alpha]\_i\)) is: "];
      Print[ CleanUpFunction[ alphaRules ] ];
    ];
  If[debugwilly1,
     Print["Leaving the function dominantBehavior`RulesSolver."]
    ];
    Return[alphaRules]
  ]; (* end of module dominantBehavior`RulesSolver *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

dominantBehavior`PowerSolver[
    alphaRules0_,
    myAlphaList_,
    options___? OptionQ
  ]:=
  Module[{powerSolverDebug = (* The debug Boolean. *)
          (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
          alphaRules = Sort[alphaRules0, (Length[#1] < Length[#2])&],
          eqnList,
          numberOfEquations = Length[myAlphaList]
         }, (* Protected local variables. *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`PowerSolver."]
    ];
    If[powerSolverDebug,
      Print["List of alphaRules for \!\(\[Alpha]\_i\) (used in Exponent "<>
            "Solver and sorted by length) is: "];
      Print[ CleanUpFunction[ alphaRules ] ];
    ];

    (* Forms a list of equations to be solved for alpha_i. *)
    eqnList =
      Outer[List, Sequence @@ (alphaRules /. Rule->Equal)];

    (* Since Outer creates a list numberOfEquations deep, *)
    (* we drop it down into the correct form. DB:05/20/2003 *)
    eqnList =
      Partition[ Flatten[ eqnList ], numberOfEquations ];

    If[powerSolverDebug,
      Print["The next set of equations (to be solved for "<>
      "\!\(\[Alpha]\_i\)) are: "];
      Print[ CleanUpFunction[ eqnList ] ];
    ];

    (* Takes these solutions and uses them to find *)
    (* actual integer solutions for alpha_i *)
    alphaSoln =
      Union[
        Sequence @@
          Solve[
            Join[
              #,
              ToExpression[
                StringReplace[
                  ToString[ DominantBehaviorConstraints /.
                    {options} /. Options[PainleveTest]
                  ],
                  "alpha" -> "Calculus`PainleveTest`Private`alpha"
                ]
              ]
            ],
            myAlphaList
          ]& /@ eqnList
      ];

    If[powerSolverDebug,
      Print["The solutions for \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];
  If[debugwilly1,
     Print["Leaving the function dominantBehavior`PowerSolver."]
    ];
   (* Returns the solutions *)
   Return[alphaSoln]
  ]; (* end of module dominantBehavior`PowerSolver *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

dominantBehavior`FixFreeAlpha[
    alphaSoln_List,
    myAlphaList_List,
    options___? OptionQ
  ] :=
  Module[{ fixFreeAlphaDebug = (DominantBehaviorVerbose /.
           {options} /. Options[PainleveTest]) > 1,
           alphaSoln0,
           alphaFree,
           alphaFreeValues, alphaFixedValues,
           alphaValues
         }, (* Protected Local Variables *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`FixFreeAlpha."]
    ];
    (* Remove symbolic solutions, based on the code in SystemCleanUp. *)
    alphaSoln0 =
      ( {Rest /@ (# /. Rule -> List), #}& /@ alphaSoln ) /.
          {a_List, {(_Rule)..}} :> Sequence[] /;
            Not[ And @@ (FreeQ[ a, #] & /@ myAlphaList  ) ];
    alphaSoln0 = #[[2]]& /@ alphaSoln0;

    (* Warn the user when potential solutions for alpha_i are removed. *)
    If[Length[Complement[Union[Sort /@ alphaSoln], alphaSoln0] ] > 0,
      StylePrint[
        "The potential solutions for \!\(\[Alpha]\_i\): "<>
        ToString[
          InputForm[
           CleanUpFunction[ Complement[Union[Sort /@ alphaSoln], alphaSoln0] ]
          ]
        ] <>
        " are being removed because they are under-determined. "<>
        "IMPORTANT: The code may miss branches! It is prudent to "<>
        "experiment with other integer choices for DominantBehaviorMin "<>
        "and DominantBehaviorMax!", "Message"
      ];
    ];

    If[fixFreeAlphaDebug,
      Print["After the under-determined systems are removed, "<>
            "the solutions for \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln0 ] ];
    ];

    (* Pick out the solutions with freedom. *)
    alphaFree =
      Select[alphaSoln0, Length[#] < Length[myAlphaList]&];

    If[fixFreeAlphaDebug,
     Print["The dominant behaviors with one or more free \!\(\[Alpha]\_i\): "];
     Print[ CleanUpFunction[ alphaFree ] ];
    ];

    If[Length[alphaFree] >= 1,

      Message[PainleveTest`dominantBehavior::freevalues,
        CleanUpFunction[ alphaFree ]
      ];

      (* Substitutes in the values that are fixed, leaving the free values *)
      (* unaltered. *)
      alphaFreeValues =
        (myAlphaList /. #)& /@ alphaFree;
      alphaFixedValues =
        (myAlphaList /. #)& /@
          Complement[alphaSoln0, alphaFree];

      (* DB:02/08/2004 *)
      If[Length[alphaFixedValues] === 0,
        Message[PainleveTest`dominantBehavior::underdetermined];
        alphaFixedValues =
          alphaFreeValues /.
            alpha[_] :>
              (DominantBehaviorMin /. {options} /. Options[PainleveTest]);
      ];

      If[fixFreeAlphaDebug,
        Print["The arbitrary (free) \!\(\[Alpha]\_i\) are: "];
        Print[ CleanUpFunction[ alphaFreeValues ] ];
        Print["The fixed \!\(\[Alpha]\_i\) are: "];
        Print[ CleanUpFunction[ alphaFixedValues ] ];
      ];

      alphaValues =
        Transpose[
          { #,
            Sequence @@
            Cases[
              alphaFixedValues,
              # /. (alpha[i_] :> _)
            ]
          }
        ]& /@ alphaFreeValues;

      If[fixFreeAlphaDebug,
        Print["Split by free variables, the fixed values of "<>
              "\!\(\[Alpha]\_i\) are: "];
        Print[ CleanUpFunction[ alphaValues ] ];
      ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
  Print["AT POINT 10, alphaValues before Threading over Range: "];
  Print[ CleanUpFunction[ alphaValues ] ]
];

      alphaValues =
        Sequence @@
          Thread[
            If[Head[First[#]] === Integer,
              First[#],
                Range[
(* WH:06/08/2006, bug fixed *)
(* DB had as lower bound:     Min[Rest[#]],   *)
(* WH has replaced this with *)
              Min[
                 Rest[#],
                 {(DominantBehaviorMin /. {options} /. Options[PainleveTest])}
                 ],
              (DominantBehaviorMax /. {options} /. Options[PainleveTest])
              ]
            ]& /@ #
          ]& /@ alphaValues;

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
  Print["AT POINT 11, alphaValues after Threading over Range: "];
  Print[ CleanUpFunction[ alphaValues ] ]
];

(* WH:06/13/2006, made the text a bit more explicit *)
      If[fixFreeAlphaDebug,
        Print["Fixing the free values for \!\(\[Alpha]\_i\) with values in "<>
            "the range starting from the minimum of all fixed alpha values "<>
            "(including DominantBehaviorMin) to DominantBehaviorMax: "];
        Print[ CleanUpFunction[ alphaValues ] ];
      ];

      alphaValues =
        ( Rule @@ #& /@
          Transpose[
            { myAlphaList,
              #
            }
          ]
        )& /@ alphaValues;

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
   Print["AT POINT 12, alphaValues after fixing free values: "];
   Print[ CleanUpFunction[ alphaValues ] ]
];

      If[fixFreeAlphaDebug,
        Print["After reformatting, the fixed values for \!\(\[Alpha]\_i\) "<>
              "are: "];
        Print[ CleanUpFunction[ alphaValues ] ];
      ];

      Return[ alphaValues ];

    ];

(* WH:06/13/2006 debug *)
If[debugDominantBehaviorMax,
  Print["AT POINT 13, alphaSoln0 being returned: "];
  Print[ CleanUpFunction[ alphaSoln0 ] ]
];
  If[debugwilly1,
     Print["Leaving the function dominantBehavior`FixFreeAlpha."]
    ];
    Return[alphaSoln0]
  ]; (* end of module dominantBehavior`FixFreeAlpha *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

dominantBehavior`SystemCleanUp[
    alphaList_List,
    alphaSoln0_List,
    myAlphaList_List,
    options___? OptionQ
  ] :=
  Module[{systemCleanUpDebug = (* The debug Boolean. *)
          (DominantBehaviorVerbose /. {options} /. Options[PainleveTest]) > 1,
          alphaSoln = Union[Sort /@ alphaSoln0],
                      (* A local version of alphaSoln0. *)
          chiList = {}, (* The list of chi constraints. *)
          numberOfEquations = Length[eqnList0] (* The number of equations. *)
         }, (* Protected local variables. *)
  If[debugwilly1,
     Print["Entering the function dominantBehavior`SystemCleanUp."]
    ];
     (* Applies the solutions to the expressions of alpha_i. *)
     alphaSoln =
      Transpose[{(alphaList //. #)& /@ alphaSoln, alphaSoln}];

     If[systemCleanUpDebug,
       Print["After possible substitution of solutions for "<>
             "\!\(\[Alpha]\_i\), the minimal exponents (in g) of the "<>
             "various terms in the given system are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];

    alphaSoln =
      alphaSoln /.
        {a_List, {(_Rule)..}} :> Sequence[] /;
          Not[ And @@ (FreeQ[ a, #] & /@ myAlphaList  ) ];

    If[systemCleanUpDebug,
      Print["After removal of the under-determined systems, "<>
            "the minimal exponents and the \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];

    (* Removes solutions that do not balance at least two independent *)
    (* terms in the original system. *)
    alphaSoln =
      alphaSoln /.
        { {a_List, b : {(_Rule) ..}} :>
            b  /; And @@ ((Length[ Cases[#, Min[#]] ] >= 2)& /@ a),
          {a_List, b : {(_Rule) ..}} :>
            Sequence[]
        };

    If[systemCleanUpDebug,
      Print["After checking exponent balances, the \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];

    (* Removes solutions with any of the alpha_i = 0. *)
    (* Removed by DB:12/20/2003
    alphaSoln =
      alphaSoln /.
        a : {(_Rule) ..} :>
          Sequence[] /; Or @@ ( #[[2]] === 0 & /@ a );

    If[systemCleanUpDebug,
      Print["After removing zero solutions, the \!\(\[Alpha]\_i\) are: "];
      Print[ CleanUpFunction[ alphaSoln ] ];
    ];
    *) (* end of removed code *)

    (* Removes positive and rational solutions for the dominant exponents. *)
    (* Modified by DB:12/21/2003 *)
    alphaSoln =
      alphaSoln //.
        {
          a : {(_Rule) ..} :>
              Sequence[] /;
                Or @@ (! IntegerQ[ #[[2]] ]& /@ a ),
          a : {(_Rule) ..} :>
              Sequence[] /;
                Or @@ ( #[[2]] > ( DominantBehaviorMax /.
                  {options} /. Options[PainleveTest] )& /@ a ),
          a : {(_Rule) ..} :>
              Sequence[] /;
                And @@ ( Positive[#[[2]] ]& /@ a )
        };
     If[systemCleanUpDebug,
       Print["After removing positive and non-integer solutions (in the "<>
             "case of systems), the \!\(\[Alpha]\_i\) are: "];
       Print[ CleanUpFunction[ alphaSoln ] ];
     ];
    If[debugwilly1,
       Print["Leaving the function dominantBehavior`SystemCleanUp."]
      ];
    (* Returns the good solutions. *)
    Return[alphaSoln]
  ]; (* end of module dominantBehavior`SystemCleanUp *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* Finds free solutions that result from inequalities. *)
(* Added direct from old code on 11/20/2002 by DB. *)
dominantBehavior`GenerateAlternativeSolutions[
    alphaList_List,
    options___? OptionQ
  ]:=
  Module[{ n (* The number of alpha[i] *)
         },  (* Protected Local Variables *)
  If[debugwilly1,
  Print["Entering the function dominantBehavior`GenerateAlternativeSolutions."]
  ];
    n = Length[alphaList];
  If[debugwilly1,
   Print["Leaving the function dominantBehavior`GenerateAlternativeSolutions."]
    ];
    Return[
      Thread[
        alphaList -> #
      ] & /@
        Flatten[
          Outer[
            List,
            Sequence @@
              Table[
                Range[
                 ( DominantBehaviorMin /. {options} /. Options[PainleveTest] ),
                 ( DominantBehaviorMax /. {options} /. Options[PainleveTest] )
                ],
                {n}
              ]
          ],
          n - 1
        ]
      ]
    ]; (* end of module dominantBehavior`GenerateAlternativeSolutions *)

(* ------------------------------------------------------------------------- *)

(* This is a function that solves for the first constants of integration
   used to find the resonances. *)

(* The input of the initialConstantsOfIntegration function is:

   - System of equations (Form: List of polynomial equations)
   - Functions (Form: List of dependent variables, e.g. {u[x,t], v[x,t]})
   - Variables (Form: List of independent variables, e.g. {x,t})
   - thedominantBehavior (Form: The output of dominantBehavior)
*)
(* The output of the dominantBehavior function is:
  { { List of dominant exponents (e.g., {alpha[1] -> -1, alpha[2] -> -3}),
      List of dominant exponents' constants of integration
    }, ...
  }
*)

initialConstantsOfIntegration[
    equations_List,
    functions_List,
    variables_List,
    alphaSoln_List,
    options___? OptionQ
  ]:=
(* WH:01/31/2006, Block replaced by Module *)
  Module[{ initialConstantsOfIntegrationDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
                /. Options[PainleveTest] ) > 0,
          theSystem,  (* Local version of the equations. *)
          theSystem0, (* Before modification. *)
          uList, (* List of u[i,0] to be solved for. *)
          i = 0, (* iterator *)
          ui0variables,
          uSoln0 (* List of solutions. *)
        },
  If[debugwilly1,
     Print["Entering the function initialConstantsOfIntegration."]
     ];
    (* DB:12/22/2005 Sets the value of ui0variables. *)
    ui0variables =
      constantsOfIntegration`uijVariables[
        variables, options];

    (* Substitutes the ansatz for the dependent variables. *)
    theSystem =
      initialConstantsOfIntegration`GenerateSystem[
        equations, functions,
        variables, alphaSoln,
        options
      ];

    If[initialConstantsOfIntegrationDebug,
      Print["After substituting the ansatz for the dependent variables, "<>
            "the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Divides through by the greatest common denominator. *)
    theSystem =
      Table[
        g[Sequence @@ variables]^(-alphaMin[i])*theSystem[[i]] /. alphaSoln,
        {i, Length[equations]}
      ];

    If[initialConstantsOfIntegrationDebug,
      Print["After simplification, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Pulls off the terms which are independent of g[x,t] *)
    theSystem =
      Coefficient[
        #,
        g[Sequence @@ variables],
        0
      ]& /@ theSystem;

    If[initialConstantsOfIntegrationDebug,
      Print["After stripping of the coefficients, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* DB:11/17/2005 for an ODE, code uses the transformation g(z) = z - z0. *)
    theSystem =
      theSystem /.
      constantsOfIntegration`KruskalSimplification[
        variables,
        If[Length[variables] === 1,
          Prepend[{options},
            KruskalSimplification -> First[variables]
          ],
          {options}
        ]
      ];

    (* Applying PainleveTest`Simplify. *)
    (* Divides through by the greatest common denominator and sets the *)
    (* expressions equal to zero so as to solve in the next step. *)
    theSystem =
      (# == 0)& /@ PainleveTest`Simplify[theSystem];

    If[initialConstantsOfIntegrationDebug,
      Print["After simplification, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* The list of u[i,0] to be solved for is: *)
    uList =
      Sort[
        Table[
          u[i,0][Sequence @@ ui0variables],
          {i, Length[equations]}
        ],
        Greater
      ];

    (* Solves the system for u[i,0]. *)
    uSoln0 =
      Solve[
        Join[
          theSystem,
          (# != 0)& /@ uList
        ],
        uList
      ];

(* WH:05/21/2008, added a MapAll Factor *)
(* to simplify the solutions (and put them on common denominators) *)
If[debugwilly1,
   Print["At point XXX1, before applying MapAll Factor, uSoln0: "];
   Print[ CleanUpFunction[ uSoln0 ] ]
   ];

   uSoln0 = MapAll[Factor, uSoln0];

If[debugwilly1,
   Print["At point XXX2, after applying MapAll Factor, uSoln0: "];
   Print[ CleanUpFunction[ uSoln0 ] ]
   ];

    (* DB:11/17/2005 if after simplification 0 == 0, *)
    (* then assume a constant solution for an ODE. *)
    If[Length[variables] === 1 && uSoln0 === {{}},
      uSoln0 =
        {(# -> Evaluate[# /. Flatten[uSoln0] ])& /@
          uList}
    ];

    If[initialConstantsOfIntegrationDebug,
      Print["The solutions (for the coefficients u[i,0]) are: "];
      Print[ CleanUpFunction[ uSoln0 ] ];
    ];

    If[( Verbose /. {options} /. Options[PainleveTest] ),
      Print["The system (within initialConstantsOfIntegration) "<>
        "(determining the first constant of integration) "<>
        "for the dominant behavior, "<>
        ToString[ CleanUpFunction[ alphaSoln ] ] <>
        " , is: "];
      Print[ CleanUpFunction[ theSystem ] ];
      Print["The solutions for u[i,0] are: "];
      Print[ CleanUpFunction[ uSoln0 ] ];
    ];
  If[debugwilly1,
     Print["Leaving the function initialConstantsOfIntegration."]
     ];
    (* If there are no solutions for this case, return nothing. *)
    If[Length[ uSoln0 ] === 0,
      StylePrint[
        "The dominant behavior, "<>
         ToString[ CleanUpFunction[ alphaSoln ] ] <>
        ", does not appear to lead to a solution for the initial constant "<>
        "of integration and will be ignored. "<>
        "The system (for finding the \!\(u\_\(i,0\)\)) is:",
        "Message"
      ];
      StylePrint[
        CleanUpFunction[ theSystem ],
        "Message"
      ];
      Return[ Hold[ Sequence[] ] ]
    ];
    Return[{alphaSoln, #}& /@ uSoln0]
  ]; (* end of module initialConstantsOfIntegration *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

initialConstantsOfIntegration`GenerateSystem[
    equations_List,
    functions_List,
    variables_List,
    alphaSoln_List,
    options___? OptionQ
  ]:=
  Module[{ systemGenerationDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
               /. Options[PainleveTest] ) > 1,
           ansatzRules, (* Rules that define the ansatz. *)
           un0variables
         }, (* Protected Local Variables *)
  If[debugwilly1,
  Print["Entering the function initialConstantsOfIntegration'GenerateSystem."]
  ];
    (* If working with an ODE or the Kruskal simplification is being used, *)
    (* the u[n,0] should be constant or depend on the remaining variables. *)
    (* DB:12/22/2005 *)
    un0variables =
      constantsOfIntegration`uijVariables[
        variables, options];

    ansatzRules =
      RuleDelayed @@ #& /@
        Table[
          { Head[ functions[[n]] ],
            Function[variables,
              Evaluate[
                u[n,0][Sequence @@ un0variables]*
                  g[Sequence @@ variables]^alpha[n]
              ]
            ]
          },
          {n, Length[ functions ] }
        ];

    If[systemGenerationDebug,
      Print["The ansatz rules (for computing the u[i,0]) are: "];
      Print[ CleanUpFunction[ ansatzRules ] ];
    ];
  If[debugwilly1,
    Print["Leaving the function initialConstantsOfIntegration'GenerateSystem."]
    ];
    Return[
      MapAll[
        Expand,
        (equations /. ansatzRules) /. alphaSoln
      ]
    ]
 ]; (* end of module initialConstantsOfIntegration`GenerateSystem *)

(* ------------------------------------------------------------------------- *)

resonances[
    equations_List,
    functions_List,
    variables_List,
    thedominantBehavior_List,
    initialConstantsOfIntegration_List,
    options___? OptionQ
  ] :=
  Module[{ resonancesDebug = (* The debug Boolean. *)
           (ResonancesVerbose /. {options} /. Options[PainleveTest]) > 0,
           theSystem, (* The system after substitution of ansatz. *)
           matrixQ,   (* The Q-matrix. *)
           detQ,      (* The determinant of the Q matrix. *)
           uirVariables,
           resSoln  (* Solutions to the characteristic equation (detQ == 0). *)
         }, (* Protected Local Variables *)
    If[debugwilly1,
       Print["Entering the function resonances."]
       ];
    (* Sets what the variables are for uir *)
    uirVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    theSystem =
      resonances`GenerateSystem[
        equations, functions,
        variables, thedominantBehavior,
        initialConstantsOfIntegration,
        options
      ];

    If[resonancesDebug,
      Print[
         "After substituting the ansatz (for computing the resonances), "<>
         "the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Divides the system by the greatest common denominator. *)
    theSystem =
      Table[
        Expand[
          g[Sequence @@ variables]^(-alphaMin[i]) *
            theSystem[[i]] /. thedominantBehavior],
        {i, Length[equations]}
      ];
    (* Applying PainleveTest`Simplify. *)
    theSystem =
      PainleveTest`Simplify[theSystem];

    If[resonancesDebug,
      Print["After simplification, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* DB:03/27/2005 Added myCoefficient function based on problems *)
    (* encountered in the SpecialSolutions package. *)
    myCoefficient[p_Plus, q_, 0] := Plus @@ Select[p, FreeQ[#, q] &];
    myCoefficient[p_, q_, 0] := Plus @@ Select[{p}, FreeQ[#, q] &];
    myCoefficient[p_Plus, q_, r_:1] := Plus @@ Cases[p, z_. q^r -> z];
    myCoefficient[p_, q_, r_:1] := Plus @@ Cases[{p}, z_. q^r -> z];

    (* Generates the matrix used to find the resonances. *)
    matrixQ =
      Table[
        Table[
          myCoefficient[
            myCoefficient[
              myCoefficient[
                theSystem[[i]],
                epsilon[j]
              ],
              u[j,r][Sequence @@ uirVariables]
            ],
            g[Sequence @@ variables],
            r
          ],
          (* Old version below. *)
          (* Coefficient[
             Coefficient[
              theSystem[[i]],
              u[j,r][Sequence @@ variables]*g[Sequence @@ variables]^r
            ],
            g[Sequence @@ variables],
            0
          ],
          *)
          {i, Length[theSystem]}
        ],
        {j, Length[theSystem]}
      ];

    (* DB:11/17/2005 for an ODE, code uses the transformation g(z) = z - z0. *)
    matrixQ =
      matrixQ /.
      constantsOfIntegration`KruskalSimplification[
        variables,
        If[Length[variables] === 1,
          Prepend[{options},
            KruskalSimplification -> First[variables]
          ],
          {options}
        ]
      ];

    If[resonancesDebug,
      Print["The matrix for the resonances is: "];
      Print[MatrixForm[ CleanUpFunction[ matrixQ ] ]];
    ];

    (* Computes the determinant of the matrix Q. *)
    detQ = Factor[MapAll[Expand,Det[matrixQ]]];

(* WH:01/03/2009 always print the factored form of characteristic equation *)
    If[resonancesDebug || Verbose /. {options} /. Options[PainleveTest],
      Print["The characteristic equation for the resonances is: "];
      Print[ CleanUpFunction[ detQ == 0 ] ]
      ];

    (* Solves the characteristic equation. *)
    resSoln =
      Flatten[ Solve[detQ == 0, r] ];

If[debugwilly1,
   Print["At point AAA1, before applying MapAll Factor, resSoln: "];
   Print[ CleanUpFunction[ resSoln ] ]
];

(* WH:05/21/2008, added the following line *)
   resSoln = MapAll[Factor, resSoln];

If[debugwilly1,
   Print["At point AAA2, after applying MapAll Factor, resSoln: "];
   Print[ CleanUpFunction[ resSoln ] ]
   ];

If[ Verbose /. {options} /. Options[PainleveTest],
   Print["The solutions (resonances r) of the characteristic equation are: "];
   Print[ CleanUpFunction[ resSoln ] ]
];

    (* Tests if the solution is valid. *)
    If[ (* start if x1 *)
      Or[
        !FreeQ[resSoln,
          (u[_,0][Sequence @@ uirVariables]|Derivative[__][g][__])],
        Or @@
          ( ( ! IntegerQ[ #[[2]] ] )& /@ resSoln ),
        Max[ Cases[ ( r /. # )& /@ resSoln, _Integer ] ] < 0
      ],
      StylePrint[
       "The resonances (r) below are being removed because they are either "<>
       "non-integer, all negative, symbolic, depend on u[i,0] or depend "<>
       "on some derivative of g.",
       "Message"
      ];

(* WH:05/21/2008, Reason for change: result was not factored *)
(* was:
   StylePrint[
        pdeForm[CleanUpFunction[Factor[MapAll[Expand,resSoln]]]],"Message"];
*)
(* WH:05/21/2008, replaced by *)
      StylePrint[ pdeForm[ CleanUpFunction[
                MapAll[Factor, MapAll[Expand,resSoln]] ] ],"Message"
                ]; (* closes style print *)
      Return[ Hold[ Sequence[] ]]
    ]; (* closes if x1 *)

    If[ Length[ Flatten[ resSoln /. (r -> a_) :> Sequence[] /; a >= -1 ] ] > 0,
      StylePrint[
        "The resonances "<>
        ToString[
          InputForm[
            CleanUpFunction[
              Flatten[ resSoln /. (r -> a_) :> Sequence[] /; a >= -1 ]
            ]
          ]
        ] <>
        " will be ignored because they are < -1.",
        "Message"
      ]
    ];
    If[debugwilly1,
       Print["Leaving the function resonances."]
       ];
    Return[
      { thedominantBehavior,
        initialConstantsOfIntegration,
        resSoln
      }
    ]
  ]; (* end of module resonances *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

resonances`GenerateSystem[
    equations_List,
    functions_List,
    variables_List,
    alphaSoln_List,
    initialConstantsOfIntegration_List,
    options___? OptionQ
  ]:=
  Module[{ systemGenerationDebug = (* The debug Boolean. *)
           (ResonancesVerbose /. {options} /. Options[PainleveTest]) > 1,
           uirVariables,
           ansatzRules (* Rules that define the ansatz. *)
         }, (* Protected Local Variables *)
    If[debugwilly1,
       Print["Entering the function resonances`GenerateSystem."]
       ];
    If[ Verbose /. {options} /. Options[PainleveTest],
      Print["Computation of THE RESONANCES for VARIOUS DOMINANT BEHAVIORS"];
      Print[ "(one at the time)."];
      Print["**************************************************************"];
      Print["START OF A NEW BRANCH OF THE COMPUTATIONS."];
      Print[
      "The system (entering the function resonances`GenerateSystem) is: "];
      Print[ CleanUpFunction[ equations ]  ];
(* WH:05/23/2008, added details about the branch considered *)
      Print["with dominant behavior (leading orders): "];
      Print[ CleanUpFunction[ alphaSoln ] ];
      Print["and corresponding coefficients: "];
      Print[ CleanUpFunction[ initialConstantsOfIntegration ] ]
    ];

    (* Sets what the variables are for uir *)
    uirVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    (* DB:03/27/2005 Added epsilon[n] to match paper. *)
    ansatzRules =
      RuleDelayed @@ #& /@
        Table[
          { Head[ functions[[n]] ],
            Function[variables,
              Evaluate[
                u[n,0][Sequence @@ uirVariables]*
                  g[Sequence @@ variables]^alpha[n] +
                epsilon[n]*u[n,r][Sequence @@ uirVariables]*
                  g[Sequence @@ variables]^(alpha[n]+r)
              ]
            ]
          },
          {n, Length[ functions ] }
        ];

    If[systemGenerationDebug,
      Print["The ansatz rules (for computing the resonances) are: "];
      Print[ CleanUpFunction[ ansatzRules ] ];
    ];
    If[debugwilly1,
       Print["Leaving the function resonances`GenerateSystem."]
      ];
    Return[
      MapAll[
        Expand,
        ( (equations /. ansatzRules) /. alphaSoln
        ) //. initialConstantsOfIntegration
      ]
    ]
  ]; (* end of module resonances`GenerateSystem *)

(* ------------------------------------------------------------------------- *)

constantsOfIntegration[
    equations_List,
    functions_List,
    variables_List,
    thedominantBehavior_List,
    theInitialConstantsOfIntegration_List,
    theResonances_List,
    options___? OptionQ
  ] :=
  Module[{ constantsOfIntegrationDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
                /. Options[PainleveTest] ) > 0,
           resonanceList, theMaximumResonance,
           theSystem, (* Equations after applying series expansion. *)
           KruskalSimplificationRules,
                   (* Rules that relate to Kruskal's simplification *)
           uijVariables, (* The variables that uij depend on. *)
           theConstantsOfIntegration,  (* The constants of integration. *)
           theCompatibilityConditions, (* The compatibility conditions. *)
           i = 0,    (* Iterator for nest function. *)
           pureRules (* To replace u[i,j] on right hand side of rules. *)
         }, (* Protected Local Variables *)
    If[debugwilly1,
       Print["Entering the function constantsOfIntegration."]
       ];
    If[ Verbose /. {options} /. Options[PainleveTest],
       Print["Computation of THE CONSTANTS OF INTEGRATION and COMPATIBILITY "<>
           "CONDITIONS (if applicable) "<>
           "for the various dominant behaviors"];
       Print["(one at the time)."];
       Print["**************************************************************"];
       Print["START OF A NEW BRANCH OF THE COMPUTATIONS."];
  (* WH:05/23/2008, added details about the branch considered *)
      Print[
      "The system (entering the function constantsOfIntegration) is: "];
      Print[ CleanUpFunction[ equations ]  ];
   If[debugwilly1,
      Print["with dominant behavior (leading orders): "];
      Print[ CleanUpFunction[ thedominantBehavior ] ];
      Print["and corresponding coefficients: "];
      Print[ CleanUpFunction[ theInitialConstantsOfIntegration ] ];
      Print["and resonances: "];
      Print[ CleanUpFunction[ theResonances ]]
      ]
    ];

    (* The maximum resonance for this particular case is: *)
    resonanceList =
      ( r /. # /. _? Negative -> Sequence[] )& /@ theResonances;

    theMaximumResonance = Max[ resonanceList ];

    If[constantsOfIntegrationDebug,
      Print["The positive resonances are: "];
      Print[ CleanUpFunction[ resonanceList ] ];
      Print["The maximum resonance is: ",
            CleanUpFunction[ theMaximumResonance ]];
    ];

    (* Applies the series expansion. *)
    theSystem =
      constantsOfIntegration`GenerateSystem[
        equations,
        functions,
        variables,
        thedominantBehavior,
        theMaximumResonance,
        options
      ];

    If[constantsOfIntegrationDebug,
      Print["After substituting the Laurent series, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* The rules for the Kruskal simplification are: *)
    KruskalSimplificationRules =
      constantsOfIntegration`KruskalSimplification[
        variables,
        If[Length[variables] === 1,
          Prepend[{options},
            KruskalSimplification -> First[variables]
          ],
          {options}
        ]
      ];

    If[constantsOfIntegrationDebug,
      Print["The KruskalSimplification rules are: "];
      Print[ CleanUpFunction[ KruskalSimplificationRules ] ];
    ];

    (* Sets what the variables are for uir *)
    uijVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    (* Add the solution previously solved for to list of solutions. *)
    theConstantsOfIntegration =
      toPure[
        Internal`DeactivateMessages[
          theInitialConstantsOfIntegration /. KruskalSimplificationRules,
          Power::infy, General::indet
        ]
      ];

    If[constantsOfIntegrationDebug,
      Print["The initial solution (in pure function form) is: "];
      Print[ CleanUpFunction[ theConstantsOfIntegration ] ];
    ];

    (* Solve for the constants of integration (at non-resonant level). *)
    { theConstantsOfIntegration, theCompatibilityConditions }  =
      Fold[
        (
        If[(Verbose /. {options} /. Options[PainleveTest] ),
            Print["Working with leading order behavior and resonances: "];
            Print[
              CleanUpFunction[
                { thedominantBehavior,
                  theInitialConstantsOfIntegration,
                  theResonances
                }
              ]
            ];
          ];

          constantsOfIntegration`NthResonance[
            #2,
            theSystem,
            variables,
            #1,
            KruskalSimplificationRules,
            options
          ]
        )&,
        { theConstantsOfIntegration, {} },
        Range[ theMaximumResonance ]
      ];

    theConstantsOfIntegration =
      Flatten[
        Join[
          # -> Evaluate[ # /. theConstantsOfIntegration ]& /@
            ( Parameters /. {options} /. Options[PainleveTest] ),
          Table[
            u[j,i] ->
              Evaluate[
                u[j,i][Sequence @@ uijVariables ] /. theConstantsOfIntegration
              ],
            {i,0, theMaximumResonance},
            {j, Length[ equations] }
          ]
        ]
      ];

    (* Takes the above and sets the u[i,j] -> u[i,j] to *)
    (* u[i,j] -> C[k][x,t] if it is one of the resonances, or to *)
    (* u[i,j] -> $Undetermined if it is not a resonance. *)
    theConstantsOfIntegration =
      theConstantsOfIntegration //.
        { ( u[a_, b_] -> u[c_,d_][Sequence @@ uijVariables ] ) :>
            ( resonanceList = Complement[ resonanceList, {b} ];
              u[a,b] ->
                If[Length[variables] === 1, (* DB:11/17/2005 *)
                  C[++i],
                  C[++i][Sequence @@ uijVariables ]
                ]
            ) /; ( MemberQ[ resonanceList, b] && ( a === c && b === d ) ),
          ( u[a_, b_] -> u[c_,_d][Sequence @@ uijVariables ] ) :>
            ( u[a,b] -> $Undetermined
            ) /; ( ( !MemberQ[ resonanceList, b] ) && ( a === c && b === d ) )
        };

    (* Re-apply, so as to remove u[i,j][], etc. *)
    pureRules =
      theConstantsOfIntegration /.
        Rule[c_,d_] :>
          Rule[c, Function[Evaluate[uijVariables],d] ];

    If[constantsOfIntegrationDebug,
      Print["The solution (again in pure function form) is: "];
      Print[ CleanUpFunction[ pureRules ] ];
    ];

    theConstantsOfIntegration =
      theConstantsOfIntegration /.
        Rule[a_,b_] :> Rule[a, b /. pureRules];

    If[Length[ resonanceList ] > 0,
      Message[PainleveTest::overdetermined, resonanceList ];
    ];
    If[debugwilly1,
       Print["Leaving the function constantsOfIntegration."]
      ];
    Return[
      { thedominantBehavior,
        theResonances,
        { theConstantsOfIntegration,
          ( ( # == 0 )& /@ (* DB:12/22/2005 Applying pureRules. *)
              (theCompatibilityConditions /. pureRules )
          ) /. True -> Sequence[]
        }
      }
    ]
  ]; (* end of module constantsOfIntegration *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

(* Formulates rules for the series and then applies them to the equations. *)
constantsOfIntegration`GenerateSystem[
    equations_List,
    functions_List,
    variables_List,
    thedominantBehavior_List,
    theMaximumResonance_Integer,
    options___? OptionQ
  ] :=
  Module[{ GenerateSystemDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
              /. Options[PainleveTest] ) > 1,
           uijVariables, (* The variable which uij depend on. *)
           seriesRules,  (* Rules for the series expansion. *)
           theSystem     (* The system of equations to be returned. *)
         }, (* Protected Local Variables *)
    If[debugwilly1,
       Print["Entering the function constantsOfIntegration`GenerateSystem."]
       ];
    (* Sets what the variables are for uir *)
    uijVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    seriesRules =
      Table[
        functions[[n]] ->
          Sum[
            u[n,m][Sequence @@ uijVariables] *
              g[Sequence @@ variables]^(m+alpha[n]),
            {m, 0, theMaximumResonance}
          ] /. thedominantBehavior,
        {n, Length[ functions ] }
      ];

    seriesRules =
      seriesRules /.
        ( u_[var__]->temp__) :>
          ( u :> Function[{var}, temp] );

    (* Generates Laurent series from -alpha_i up to the maximum resonance *)
    (* It puts this into the pure function format. *)
    (* DB:12/22/2005
    seriesRules =
      RuleDelayed @@ #& /@
        Table[
          { Head[ functions[[n]] ],
            Function[variables,
              Evaluate[
                Sum[
                  u[n,m][Sequence @@ uijVariables] *
                    g[Sequence @@ variables]^(m+alpha[n]),
                  {m, 0, theMaximumResonance}
                ] /. thedominantBehavior
              ]
            ]
          },
          {n, Length[ functions ] }
        ];

    If[GenerateSystemDebug,
      Print["The rules for the truncated Laurent series are: "];
      Print[ CleanUpFunction[ seriesRules ] ];
    ];
    *)

    (* Applies the Laurent series to the equations. *)
    theSystem =
      MapAll[ Expand, equations /. seriesRules ];
   (* Removed DB:12/23/2005
      PainleveTest`Simplify[
        MapAll[
          Expand,
          equations /. seriesRules
        ]
      ];
   *)

    (* Clears g from the denominator. *)
    theSystem =
      Table[
        Expand[
          g[Sequence @@ variables]^(-alphaMin[i]) *
            theSystem[[i]] /. thedominantBehavior],
        {i, Length[equations]}
      ];
    (* Applying PainleveTest`Simplify. *)
    theSystem =
      PainleveTest`Simplify[theSystem];
(*
    If[GenerateSystemDebug,
      Print[
        "After substituting the Laurent series, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];
*)
    If[debugwilly1,
       Print["Leaving the function constantsOfIntegration`GenerateSystem."]
      ];
    Return[theSystem]
  ]; (* end of module constantsOfIntegration`GenerateSystem *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`KruskalSimplification[
    variables_List,
    options_List
  ] :=
  Module[{ KruskalSimplificationDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. options
               /. Options[PainleveTest] ) > 1,
           theVariable, (* The variable that g and u are assumed to be *)
                        (* constant with respect to. *)
           theRule
         }, (* Protected Local Variables *)
   If[debugwilly1,
   Print["Entering the function constantsOfIntegration`KruskalSimplification."]
   ];
    If[KruskalSimplificationDebug ,
      Print["The input "<>
      "(for the function constantsOfIntegration`KruskalSimplification) is: "];
      Print[ CleanUpFunction[ {variables, options} ] ];
    ];

    theVariable =
      KruskalSimplification /. options /. Options[PainleveTest];
    (* start if 50 *)
    If[ ( Head[theVariable] != Symbol ) ||
        ( FreeQ[ theVariable, Alternatives @@ variables ] ),

      Return[{}],

      theRule =
        g[Sequence @@ variables] ->
          Evaluate[
            theVariable -
            h[Sequence @@
              ( Complement[
                  variables,
                  { theVariable }
                ]
              )
            ]
          ] /. ( g[var__] -> temp__) :>
            ( g :> Function[{var}, temp] ) ;
          (* DB:12/22/2005 Removed since uij only depend on uijVariables.
          ,
          Derivative[var__][u[_,_]][Sequence @@ variables] :>
            0 /;
              Part[{var},
                Position[variables,
                  (KruskalSimplification /. options /. Options[PainleveTest])
                ][[1,1]]
              ] != 0
          *)
   If[debugwilly1,
    Print["Leaving the function constantsOfIntegration`KruskalSimplification."]
     ];
   Return[ {theRule} ]
   ] (* end of if 50 *)
  ]; (* end of module constantsOfIntegration`KruskalSimplification *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`uijVariables[
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ uijVariablesDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
               /. Options[PainleveTest] ) > 2,
           theVariable (* The variable that g and u are assumed to be *)
                       (* constant with respect to. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function constantsOfIntegration`uijVariables."]
     ];
    If[uijVariablesDebug ,
      Print["The input" <>
            "(for the function constantsOfIntegration`uijVariables) is: "];
      Print[ CleanUpFunction[ {variables, {options}} ] ];
    ];
   If[debugwilly1,
      Print["Leaving the function constantsOfIntegration`uijVariables."]
     ];
    (* If it is an ODE, return {} so uij is a constant. *)
    If[ Length[variables] === 1, Return[{}] ];

    (* Sets the variable to the value of option set by the user. *)
    theVariable = KruskalSimplification /. {options} /. Options[PainleveTest];

    If[ ( Head[theVariable] != Symbol ) ||
        ( FreeQ[ theVariable, Alternatives @@ variables ] ),

      (* If the Kruskal simplification is not being used, *)
      (* then uij can depend on all the variables. *)
      Return[variables],

      (* If the Kruskal simplification is being used, *)
      (* then we return the variables without `theVariable'. *)
      Return[ variables /. theVariable -> Sequence[] ]
    ]
  ]; (* end of module constantsOfIntegration`uijVariables *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`NthResonance[
    n_Integer,
    equations_List,
    variables_List,
    { theConstantsOfIntegration0_List,
      theCompatibilityCondition0_List
    },
    KruskalSimplificationRules_List,
    options___? OptionQ
  ] :=
  Module[{ NthResonanceDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
               /. Options[PainleveTest] ) > 1,
           theSystem, (* The local copy of the equations. *)
           uList,     (* List of u_{i,n} to be solved for. *)
           uijVariables, (* The variables which uij depend on. *)
           theConstantsOfIntegration, (* The solutions for the constants. *)
           compatibilityConditions    (* Compatibility conditions. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function constantsOfIntegration`NthResonance."]
     ];
    (* Pulls off the coefficients of g^n(x,t). *)
    theSystem =
      Coefficient[
        #,
        g[Sequence @@ variables],
        n
      ]& /@ equations;

    If[NthResonanceDebug,
      Print["The equations (coming from the coefficient of g^n) are: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    theSystem =
      (* DB:11/01/2006 Added Factor *)
      Factor[
        MapAll[Expand,
          theSystem /. theConstantsOfIntegration0
        ]
      ];

    If[NthResonanceDebug,
      Print["After substitution of the integration constants (obtained "<>
            "previously), the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Sets what the variables are for uir *)
    uijVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    (* Builds the matrices that will be solved for u_ij *)
    { rightHandSideOfMatrix, leftHandSideOfMatrix } =
      constantsOfIntegration`NthResonance`BuildMatrices[
        n,
        theSystem,
        variables,
        KruskalSimplificationRules,
        options
      ];

If[debugwilly1,
  Print["At pt. MMM1, before solving for theConstantsOfIntegration."]
  ];

    (* Solves theSystem for the u[i,n] *)
    { theConstantsOfIntegration,
      compatibilityConditions
    } =
      constantsOfIntegration`NthResonance`Solve[
        n,
        rightHandSideOfMatrix,
        leftHandSideOfMatrix,
        variables,
        options
      ];

If[debugwilly1,
  Print["At pt. MMM2, "<>
        "{theConstantsOfIntegration, compatibilityConditions} :"
       ];
  Print[ { CleanUpFunction[ theConstantsOfIntegration ],
           CleanUpFunction[ compatibilityConditions ] }
       ];
  Print["At pt. MMM3, after solving for theConstantsOfIntegration."]
  ];

(* WH:05/21/2008, inserted a Map Factor below, just before Evaluate *)
(* NOTE: worked fine, solutions are factored and in PDE format!!! *)

    If[NthResonanceDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["The solutions for \!\(u\_\(i," <> ToString[n] <> "\)\) are: "];
      Print[
        pdeForm[
          CleanUpFunction[
            Join[
              Table[
                u[i,n][Sequence @@ uijVariables] ->
                  Map[Factor,Evaluate[
                    u[i,n][Sequence @@ uijVariables] /.
                                       theConstantsOfIntegration
                  ]],
                {i, Length[ equations ] }
              ],
              # -> Evaluate[ # /. theConstantsOfIntegration ]& /@
                ( Parameters /. {options} /. Options[PainleveTest] )
            ]
          ]
        ]
      ];
    ];

(* WH:05/31/2007, fix in output added == 0, so it reads expr == 0 *)
    If[ Length[ compatibilityConditions ] > 0,
      StylePrint[
        "The compatibility condition AT LEVEL " <> ToString[n] <> " is:",
        "Message"
      ];
      StylePrint[
        pdeForm[
          CleanUpFunction[
            compatibilityConditions
          ]
        ],
        "Message"
      ];
     StylePrint[" == 0", "Message"];
    ];

If[debugwilly1,
  Print["At pt. MMM4 in constantsOfIntegration`NthResonance function."]
  ];
   If[debugwilly1,
      Print["Leaving the function constantsOfIntegration`NthResonance."]
     ];
    Return[
      {
        Flatten[
          Join[
            theConstantsOfIntegration0,
            theConstantsOfIntegration
          ]
        ],
        Flatten[
          Join[
            theCompatibilityCondition0,
            compatibilityConditions
          ]
        ]
      }
    ]
  ]; (* end of module constantsOfIntegration`NthResonance *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`NthResonance`BuildMatrices[
    n_Integer,
    theSystem_List,
    variables_List,
    KruskalSimplificationRules_List,
    options___? OptionQ
  ] :=
  Module[{ buildMatricesDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
              /. Options[PainleveTest] ) > 1,
           uijVariables,
           rightHandSide, leftHandSide
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`BuildMatrices."]
     ];
    (* Sets what the variables are for uir *)
    uijVariables =
      constantsOfIntegration`uijVariables[
        variables, options];

    (* Builds the right hand side of the matrix problem. *)
    (* The matrix is made up of the coefficients of: *)
    (* [ u[1,n] u[2,n] . . . ] <- from equation 1 *)
    (* [ u[1,n] u[2,n] . . . ] <- from equation 2 *)
    (* [ u[1,n] u[2,n] . . . ] <- from equation 3 *)
    rightHandSide =
      Table[
        Table[
          Expand[
            Coefficient[
              theSystem[[i]],
              u[j, n][Sequence @@ uijVariables]
            ]
          ],
          {j, Length[theSystem] }
        ],
        {i, Length[theSystem] }
      ] /. KruskalSimplificationRules;

    If[buildMatricesDebug,
      Print["The right hand side of the matrix equation (to be solved) is: "];
      Print[ CleanUpFunction[ rightHandSide ] ];
    ];

    leftHandSide = - ( MapAll[ Expand, theSystem ] /.
                      u[_,n] :> Function[Evaluate[uijVariables], 0]
                      ) /. KruskalSimplificationRules;

    If[buildMatricesDebug,
      Print["The coefficient matrix (left hand side) of the equation "<>
            "(to be solved) is: "];
      Print[ CleanUpFunction[ leftHandSide ] ];
    ];
   If[debugwilly1,
      Print["Leaving the function "<>
      "constantsOfIntegration`NthResonance`BuildMatrices."]
     ];
    Return[
      { rightHandSide, leftHandSide }
    ]
  ]; (* end of module constantsOfIntegration`NthResonance`BuildMatrices *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`NthResonance`Solve[
    n_Integer,
    rightHandSideOfMatrix_List,
    leftHandSideOfMatrix_List,
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ solveDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
             /. Options[PainleveTest] ) > 1,
           theSystem,  (* The re-composed system to be solved. *)
           uList,      (* The list of coefficients to solve for. *)
           theSolution (* The solution of the system. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
  "constantsOfIntegration`NthResonance`Solve (for determinant non-zero)."]
     ];
If[debugwilly1,
   Print["At pt. KKK1a, "<>
   "inside the first constantsOfIntegration`NthResonance`Solve function"];
   Print["(for determinant non-zero)."]
  ];

    { theSystem, uList } =
      constantsOfIntegration`NthResonance`Solve`BuildSystem[
        n,
        rightHandSideOfMatrix,
        leftHandSideOfMatrix,
        variables,
        options
      ];

(* WH:05/21/2008, suppress the printout of these often long systems *)
(* was
    If[solveDebug || (Verbose /. {options} /. Options[PainleveTest]),
      Print["The system (determining the integration constants) "<>
            "AT LEVEL ", n, " is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];
*)
(* replaced by *)
If[ (Verbose /. {options} /. Options[PainleveTest]) || debugwilly1,
    Print["Determining the integration constants AT LEVEL ", n, "."];
    Print["Solving the system "<>
           "(within constantsOfIntegration`Solve)."];
    If[solveDebug,
      Print["The system (determining the integration constants) "<>
            "AT LEVEL ", n, " is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ]
];

(* Looks like the condition is ignored. Needs further investigation !!!!!! *)
If[debugwilly1,
   Print["At pt. BBB1, det of righhandsideofmatrix: "];
   Print[ CleanUpFunction[ MapAll[Expand, Det[ rightHandSideOfMatrix ] ] ] ]
  ];

If[debugwilly1,
   Print["At marker YYY1, "<>
   "inside the constantsOfIntegration`NthResonance`Solve function."]
  ];

    theSolution =
      Solve[
        theSystem,
        uList
      ];

(*
If[debugwilly1,
   Print["At point ZZZ1, before applying MapAll Factor, theSolution: "];
   Print[ CleanUpFunction[ theSolution ] ]
  ];
*)

(* WH:05/21/2008, added MapAll Factor *)
   theSolution = MapAll[Factor, theSolution];

(*
If[debugwilly1,
   Print["At point ZZZ2, after applying MapAll Factor, theSolution: "];
   Print[ CleanUpFunction[ theSolution ] ]
   ];
*)

If[debugwilly1,
   Print["At pt. DDD2, theSolution: "];
   Print[ CleanUpFunction[ theSolution ] ];
   Print["At marker EEE2, "<>
         "inside constantsOfIntegration`NthResonance`Solve function."]
  ];

 If[debugwilly1,
    Print["Leaving the function constantsOfIntegration`NthResonance`Solve "<>
    "(for determinant non-zero)."]
    ];
(* WH:05/21/2008, Expand replaced by Factor *)
    Return[
      { toPure[
          MapAll[Factor,
            theSolution
          ]
        ],
        {}
      }
    ]
  ] /; MapAll[Expand, Det[ rightHandSideOfMatrix ] ] =!= 0;
  (* end of module constantsOfIntegration`NthResonance`Solve *)

(* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - *)

constantsOfIntegration`NthResonance`Solve[
    n_Integer,
    rightHandSideOfMatrix_List,
    leftHandSideOfMatrix_List,
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ solveDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
               /. Options[PainleveTest] ) > 2,
           theSystem,   (* The system re-composed system to be solved. *)
           uList,       (* The list of coefficients to solve for. *)
           theSolution, (* The solution of the system. *)
           theCondition (* The conditions on the system. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`Solve (for determinant = 0)."]
     ];
If[debugwilly1,
   Print["At pt. KKK1b, "<>
         "inside second constantsOfIntegration`NthResonance`Solve function"];
   Print["(for determinant = 0)."]
  ];
    { theSystem, uList } =
      constantsOfIntegration`NthResonance`Solve`BuildSystem[
        n,
        rightHandSideOfMatrix,
        leftHandSideOfMatrix,
        variables,
        options
      ];

(* WH:05/21/2008, suppress the printout of these often long systems *)
(* was
    If[solveDebug || ( Verbose /. {options} /. Options[PainleveTest] ),
      Print["The system (within constantsOfIntegration`NthResonance`Solve) "<>
            "(determining the integration constants) "<>
            "AT LEVEL ", n, " is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];
*)
(* replaced by *)
If[ (Verbose /. {options} /. Options[PainleveTest]) || debugwilly1,
    Print["Determining the integration constants AT LEVEL ", n, "."];
    Print["Solving the system "<>
           "(within constantsOfIntegration`NthResonance`Solve)."];
    If[solveDebug,
      Print["The system (within constantsOfIntegration`NthResonance`Solve) "<>
            "(determining the integration constants) "<>
            "AT LEVEL ", n, " is: "];
      Print[ CleanUpFunction[ theSystem ] ];
      If[ CleanUpFunction[ theSystem ] === True,
          Print["where True means that there is no compatibility condition."]
        ]
    ]
];

    If[theSystem === True,
      Return[ {{},{}} ]
    ];

(* WH:05/31/2007, BugFix. *)
(* If the compatibility condition is always false *)
(* the false is added to the list of compatibility conditions *)
   If[theSystem === False,
     StylePrint["Inconsistent equation encountered.", "Message"];
     StylePrint["The compatibility condition is not satisfied!", "Message"];
     StylePrint[ CleanUpFunction[ {-1} ], "Message"];
     Return[ {{},{-1}} ]
    ];

    (* Does a very basic simplification. *)
    theSystem =
      constantsOfIntegration`NthResonance`Solve`Simplify[
        theSystem,
        options
      ];

If[debugwilly1,
  Print["At pt XXX3a, uList going into IterativelySolve function, uList: "];
  Print[ CleanUpFunction[ uList ] ];
  Print["At pt XXX3b, theSystem going into the IterativelySolve function, "<>
        "theSystem: "];
  Print[ CleanUpFunction[ theSystem ] ];
  Print["At marker YYY3, "<>
        "inside constantsOfIntegration`NthResonance`Solve function."]
  ];

    { theSolution, theCondition } =
      constantsOfIntegration`NthResonance`Solve`IterativelySolve[
        theSystem,
        uList,
        {},
        options
      ];

If[debugwilly1,
  Print["At marker YYY4, "<>
        "inside constantsOfIntegration`NthResonance`Solve function."]
  ];
      If[solveDebug,
        Print["The solution (AT LEVEL ",n,") is: "];
        Print[ CleanUpFunction[ theSolution ] ];
        Print["The compatibility condition (AT LEVEL ",n,") is: "];
        Print[ CleanUpFunction[ theCondition ] ];
        ];

conditiontestvalue = Replace[
          theCondition,
          0 -> Sequence[],
          1
        ];

If[debugwilly1,
  Print["At KKK1, theSolution: "];
  Print[ CleanUpFunction[ theSolution ] ];
  Print["At KKK2, condition based on zero replaced by sequence, testvalue: "];
  Print[ CleanUpFunction[ conditiontestvalue ] ]
  ];
   If[debugwilly1,
      Print["Leaving the function "<>
      "constantsOfIntegration`NthResonance`Solve (for determinant = 0)."]
     ];
    (* DB:07/28/2003 *)
    Return[
      { theSolution,
        Replace[
          theCondition,
          0 -> Sequence[],
          1
        ]
      }
    ]
  ] /; MapAll[Expand, Det[ rightHandSideOfMatrix ] ] === 0;
  (* end of module constantsOfIntegration`NthResonance`Solve *)

(* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - *)

constantsOfIntegration`NthResonance`Solve`Simplify[
    theSystem_Equal,
    options___? OptionQ
  ] :=
  Module[{ theNewSystem (* The simplified system. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`Solve`Simplify."]
     ];
        (* Changes from an equation to an expression equal to zero. *)
    theNewSystem  =
      ( theSystem /. True -> Sequence[] ) /. Equal[a_,b_] :> a - b;

    theNewSystem  =
      Factor[
        MapAll[Expand, theNewSystem ]
      ];
   If[debugwilly1,
      Print["Leaving the function "<>
      "constantsOfIntegration`NthResonance`Solve`Simplify."]
     ];
    Return[ theNewSystem ]
  ]; (* end of module constantsOfIntegration`NthResonance`Solve`Simplify *)

(* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - *)

(* HEREMAN CRM Trouble is in this function *)
constantsOfIntegration`NthResonance`Solve`IterativelySolve[
    equations_List,
    unknowns_List,
    solutions_List,
    options___? OptionQ
  ] :=
  Module[{ iterativelySolveDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
                /. Options[PainleveTest] ) > 2,
           theSystem = equations //. solutions,
                        (* A local version of the equations. *)
           uList,       (* List of u_in in the system. *)
           theEquation, (* The equation to be solved. *)
           solveFor,    (* The independent variables to solve for. *)
           theSolution  (* The solution for the equation. *)
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`Solve`IterativelySolve."]
     ];
    (* Takes the given unknowns and adds the option parameters. *)
    uList =
      Flatten[
        Join[ unknowns,
          ( Parameters /. {options} /. Options[PainleveTest] )
        ]
      ];

If[debugwilly1,
   Print["At pt. ZZZ1, uList: "];
   Print[ CleanUpFunction[ uList ] ]
  ];

    (* Pulls off the u_in that the are in the system. *)
    uList =
      Select[
        uList,
        ! FreeQ[ equations, # ]&
      ];

If[debugwilly1,
   Print["At pt. ZZZ2, uList: "];
   Print[ CleanUpFunction[ uList ] ]
  ];

    If[iterativelySolveDebug,
      Print["The \!\(u\_\(i," <> CleanUpFunction[ToString[n]] <>
            "\)\) (in the system) are: "];
      Print[ CleanUpFunction[ uList ] ];
    ];

    (* If there are no u_in left in the system, what remains in equations *)
    (* must be the constraint to the system. *)
    If[ Length[uList] === 0, (* begin if 1 *)
      If[debugwilly1,
         Print["At pt. ZZZ3, because length of uList is zero, uList: "];
         Print[ CleanUpFunction[ uList ] ]
        ];
      Return[
        { solutions, (* The solutions. *)
          Factor[
            MapAll[Expand, (* Simplifies the conditions. *)
              equations
            ]
          ]
        }
      ]
    ]; (* end if 1 *)

    (* Sorts the equations by complexity. *)
    theSystem =
     Sort[
      theSystem,
      ( constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity[
            #1, uList ] <=
      constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity[
            #2, uList ]
      )&
     ];

If[debugwilly1,
   Print["At pt. ZZZ4, after sorting by complexity, theSystem: "];
   Print[ CleanUpFunction[ theSystem ] ]
  ];

    If[iterativelySolveDebug,
      Print["Sorted by complexity, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];

    (* Takes the first equation as the one to be solved in this iteration. *)
    theEquation = theSystem[[1]];

If[debugwilly1,
   Print["At pt. XXX4, after taking the first equation in the system, "<>
          "theEquation to be solved: "];
   Print[ CleanUpFunction[ theEquation ] ]
  ];

    (* Takes the u_ij that the equation is polynomial in. *)
    solveFor =
      Select[ uList, PolynomialQ[ theEquation , # ]& ];

If[debugwilly1,
   Print["At pt. ZZZ5, solveFor: "];
   Print[ CleanUpFunction[ solveFor ] ]
  ];

    If[Length[ solveFor ] != 0, (* start if 2 *)
       If[debugwilly1,
         Print["At pt. ZZZ6, length solveFor is nonzero, length solveFor: "];
         Print[ CleanUpFunction[ Length[solveFor] ] ]
         ];
      (* If there is a u_ij that the equation is polynomial in, *)
      (* we sorts it by power. *)
      solveFor =
        Sort[solveFor,
          Exponent[theEquation, #1] <= Exponent[theEquation, #2]&
        ];
       If[debugwilly1,
          Print["At pt. ZZZ7, length solveFor is nonzero, new solveFor: "];
          Print[ CleanUpFunction[ solveFor ] ]
         ];
      solveFor =
        Flatten[{solveFor, Complement[uList, solveFor]}];
        If[debugwilly1,
          Print["At pt. ZZZ8, length solveFor is nonzero, new solveFor: "];
          Print[ CleanUpFunction[ solveFor ] ]
          ], (* else if 2 *)
      (* If there is not a u_ij that the equation is polynomial in, *)
      (* then we continue with the original uList. *)
       If[debugwilly1,
         Print["At pt. ZZZ9, since there was no u_ij in equation, solveFor: "];
         Print[ CleanUpFunction[ solveFor ] ]
         ];
      solveFor = uList
    ]; (* end if 2 *)

    If[iterativelySolveDebug,
      Print["The first equation (of the system above) will be solved "<>
            "for variable "];
      Print[ CleanUpFunction[ solveFor ] ];
    ];

If[debugwilly1,
    Print["At pt. XXX5, theEquation to be solved: "];
    Print[ CleanUpFunction[ theEquation ] ];
    Print["At marker YYY5, "<>
     "in constantsOfIntegration`NthResonance`Solve`IterativelySolve function."]
  ];

    (*  Solves the equation for the polynomial u_ij first. *)
    theSolution =
      Solve[
        theEquation == 0,
        solveFor
      ];

If[debugwilly1,
    Print["At pt. LLL1, before applying MapAll Factor, theSolution: "];
    Print[ CleanUpFunction[ theSolution ] ]
  ];

(* WH:05/21/2008, added MapAll Factor *)
theSolution = MapAll[Factor, theSolution];

If[debugwilly1,
  Print["At marker YYY6, "<>
    "in constantsOfIntegration`NthResonance`Solve`IterativelySolve function."];
    Print["At pt. LLL2, after applying MapAll Factor, theSolution: "];
    Print[ CleanUpFunction[ theSolution ] ]
  ];

    If[iterativelySolveDebug,
      Print["The solution of this equation is: "];
      Print[ CleanUpFunction[ theSolution ] ];
    ];

If[debugwilly1,
  Print["At pt. XXX7, theSolution: "];
  Print[ CleanUpFunction[ theSolution ] ];
   If[Head[ CleanUpFunction[ theSolution ] ] =!= List,
      Print["At pt. XXX8, ALARM ALARM ALARM: Solution does not exist!"]
     ];
  Print["At marker YYY7, "<>
    "in constantsOfIntegration`NthResonance`Solve`IterativelySolve function."];
  Print["At pt. LLL3, before the joining starts, theSolution: "];
  Print[ CleanUpFunction[ theSolution ] ];
  ];

(* WH:05/21/2008, replaced Expand by MapAll Factor below *)
    (* Joins the new solution to the previously found solutions. *)
    theSolution =
      Flatten[
        Join[
          solutions,
          toPure[
            MapAll[Factor,
              theSolution
            ]
          ]
        ]
      ];

If[debugwilly1,
   Print["At marker YYY8, "<>
    "in constantsOfIntegration`NthResonance`Solve`IterativelySolve function."]
  ];

    If[iterativelySolveDebug,
      Print["Joining the new solution with previous solutions, yields: "];
      Print[ CleanUpFunction[ theSolution ] ];
    ];

    (* Applies the solutions to the system and removes any equations that
       are satisfied. *)
    theSystem =
      Replace[
        Factor[
          MapAll[Expand,
            theSystem /. theSolution
          ]
        ],
        0 -> Sequence[],
        1
      ];

    If[iterativelySolveDebug,
      Print["After applying the above solutions, the system is: "];
      Print[ CleanUpFunction[ theSystem ] ];
    ];
   If[debugwilly1,
      Print["Leaving the function "<>
      "constantsOfIntegration`NthResonance`Solve`IterativelySolve."]
     ];
    Return[
      constantsOfIntegration`NthResonance`Solve`IterativelySolve[
        theSystem,
        unknowns,
        theSolution,
        options
      ]
    ]
 ]; (* end module constantsOfIntegration`NthResonance`Solve`IterativelySolve *)

(* -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - *)

constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity[
    expr_,
    unknowns_
  ]:=
  Module[{ complexity,
           exprLength
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity."]
     ];
    (* The length of the expression. *)
    exprLength =
      If[Head[Expand[expr]] === Plus,
        Length[Expand[expr]],
        1
      ];

    (* Sets complexity to the unknowns in the expression. *)
    complexity = Select[unknowns, ! FreeQ[ expr, # ]& ];

    (* Sets complexity to the exponent of the expression in the present *)
    (* unknowns. *)
    complexity = Exponent[ expr, complexity ];
    If[debugwilly1,
      Print["Leaving the function "<>
      "constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity."]
      ];
    (* Returns the minimum exponent plus the length of the expression. *)
    Return[
      Min[ complexity ] + exprLength
    ]
  ]; (* end of module
     constantsOfIntegration`NthResonance`Solve`IterativelySolve`Complexity *)

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

constantsOfIntegration`NthResonance`Solve`BuildSystem[
    n_Integer,
    rightHandSideOfMatrix_List,
    leftHandSideOfMatrix_List,
    variables_List,
    options___? OptionQ
  ] :=
  Module[{ buildSystemDebug = (* The debug Boolean. *)
           ( ConstantsOfIntegrationVerbose /. {options}
             /. Options[PainleveTest] ) > 2,
           theSystem,
           uijVariables,
           uList
         }, (* Protected Local Variables *)
   If[debugwilly1,
      Print["Entering the function "<>
      "constantsOfIntegration`NthResonance`Solve`BuildSystem."]
     ];
    (* Sets what the variables are for uir *)
    uijVariables =
      constantsOfIntegration`uijVariables[ variables, options];

    (* The [ right hand ] . [u[1,n], u[2,n], ...] == [ left hand ]. *)
    theSystem =
      rightHandSideOfMatrix .
        Table[
          u[i,n][Sequence @@ uijVariables],
          {i, Length[ rightHandSideOfMatrix ] }
        ] ==
          leftHandSideOfMatrix;

If[debugwilly1,
   Print["At XXX9, this is theSystem: "];
   Print[ CleanUpFunction[ theSystem ] ];
   If[ CleanUpFunction[ theSystem ] === True,
      Print["where True means that there is no compatibility condition"]
     ]
  ];

If[debugwilly1,
  If[theSystem === False,
     Print["Inconsistent equation encountered!"];
     Print["The compatibility condition is not satisfied!"];
     ]
  ];

If[debugwilly1,
       righthandside = rightHandSideOfMatrix .
         Table[
           u[i,n][Sequence @@ uijVariables],
           {i, Length[ rightHandSideOfMatrix ] }
         ];
         lefthandside = leftHandSideOfMatrix;
(* WH:05/21/2008, reversed the names lefthandside and righthandside *)
(* and changed the wording, because the system could be consistent, or *)
(* lead to a compatibility condition *)
  Print["At pt. CCC1, the system has righthandside: "];
  Print[ CleanUpFunction[ lefthandside ] ];
  Print["At pt. CCC2, the system has lefthandside: "];
  Print[ CleanUpFunction[ righthandside ] ]
  ];

    (* The list of u_{i,n} to be solved for. *)
    uList =
      Reverse[
        Table[
          u[i,n][Sequence @@ uijVariables],
          {i, Length[rightHandSideOfMatrix] }
        ]
      ];

If[debugwilly1,
   Print["At CCC3, this is uList: "];
   Print[ CleanUpFunction[ uList ] ]
  ];

 If[buildSystemDebug,
 Print["The system" <>
 "(within the constantsOfIntegrationNthResonance`Solve`BuildSystem function)"<>
 " is: "];
      Print[ CleanUpFunction[ theSystem ] ];
      Print["The \!\(u\_\(i," <> ToString[n] <> "\)\) "<>
            "(multiplying the coefficient matrix) are: "];
      Print[ CleanUpFunction[ uList ] ];
 ];
    If[debugwilly1,
       Print["Leaving the function "<>
       "constantsOfIntegration`NthResonance`Solve`BuildSystem."]
      ];
    Return[ { theSystem, uList } ]
  ]; (* end of module constantsOfIntegration`NthResonance`Solve`BuildSystem *)

(* ------------------------------------------------------------------------- *)

(* Function PainleveTest`Simplify. *)
PainleveTest`Simplify[theSystem_List] :=
  Expand[
    Numerator[
      Together[ # ]
    ]
  ] & /@ theSystem;

PainleveTest`Simplify[theSystem_List, eliminateTerm_] :=
  Times @@ (First[#]^Last[#]&) /@ #& /@
    ( ( FactorList /@ PainleveTest`Simplify[theSystem]) /.
        {eliminateTerm, _Integer} :> Sequence[]
    );

(* ------------------------------------------------------------------------- *)

(* : Title: toPure *)
(* : Author: Douglas Baldwin *)
(* : Summary: Converts u[i,j][x,y,...] -> expr into
               u[i,j] :> Function[{x,y,z,t,...}, expr]. *)

toPure =
  ( # /.
    ( u_[a_,b_][var__]->temp__) :>
      ( u[a,b]:>Function[{var}, temp] )
    )&;

(* ------------------------------------------------------------------------- *)

(* : Title: CleanUpFunction *)
(* : Author: Douglas Baldwin *)
(* : Summary: Remove "PDESpecialSolutionsPrivate`" from output. *)

CleanUpFunction =
  ToExpression[
    StringReplace[ToString[InputForm[#]],
      "Calculus`PainleveTest`Private`"->""
    ]
  ]&

(* ------------------------------------------------------------------------- *)

(* : Title: pdeForm *)
(* : Summary: Displays expressions in standard mathematical notation. *)
(* : Authors: Initially written by Tracy Otto and Anthony Miller (1995),
              later updated by Unal Goktas, then by Mike Colagrosso, and
              finally re-written by Douglas Baldwin *)

pdeForm[expr_] :=
  expr /.
    { Derivative[n__][u_[a_,b_]][x__] :>
       SequenceForm[
         Subscript[u,
           SequenceForm["(",a,",",b,")"]
         ],
         Subscript[
           SequenceForm @@
             Flatten[
               Table[#[[1]],
                 {#[[2]]}
               ]& /@
                 Transpose[{{x}, {n}}]
             ]
         ]
       ],
    Derivative[n__][F[a_]][x__] :>
      SequenceForm[
        Subscript[u,a],
        Subscript[
          SequenceForm @@
            Flatten[
              Table[#[[1]], {#[[2]]}]& /@
                Transpose[{{x}, {n}}]
            ]
        ]
      ],
    Derivative[n__][F_[a_]][x__] :>
      SequenceForm[
        Subscript[F,a],
        Subscript[
          SequenceForm @@
            Flatten[
              Table[#[[1]], {#[[2]]}]& /@
                Transpose[{{x}, {n}}]
           ]
        ]
      ],
    Derivative[n__][g_][x__] :>
      SequenceForm[g,
        Subscript[
          SequenceForm @@
            Flatten[
              Table[#[[1]], {#[[2]]}]& /@
                Transpose[{{x}, {n}}]
            ]
          ]
        ],
    F[a_][__] :> Subscript[u,a],
(* WH:01/29/2006, did not work u[__] :> u, *)
    g[__] :> g,
    u[a_,b_][__] :>
      Subscript[u,
        SequenceForm["(",a,",",b,")"]
      ],
    u[a_,b_] :>
      Subscript[u,
        SequenceForm["(",a,",",b,")"]
      ]
    };

(* ------------------------------------------------------------------------- *)

End[] (* `Private` context *)

Attributes[PainleveTest] = {Protected, ReadProtected}

EndPackage[]

Print["Package PainleveTest.m was successfully loaded."];
Print["Last updated by Hereman on Sunday, January 25, 2009."];
Print["Previously updated by Baldwin on Friday, January 2, 2009."];
Print["Version 2 first released: February 1, 2006."];

(* ------------------------------------------------------------------------- *)

(* ***************************** end of all ******************************** *)

