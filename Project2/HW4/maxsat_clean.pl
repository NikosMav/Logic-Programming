:- lib(ic).
:- lib(branch_and_bound).

create_formula(NVars, NClauses, Density, Formula) :-
   formula(NVars, 1, NClauses, Density, Formula).

formula(_, C, NClauses, _, []) :-
   C > NClauses.
formula(NVars, C, NClauses, Density, [Clause|Formula]) :-
   C =< NClauses,
   one_clause(1, NVars, Density, Clause),
   C1 is C + 1,
   formula(NVars, C1, NClauses, Density, Formula).

one_clause(V, NVars, _, []) :-
   V > NVars.
one_clause(V, NVars, Density, Clause) :-
   V =< NVars,
   rand(1, 100, Rand1),
   (Rand1 < Density ->
      (rand(1, 100, Rand2),
       (Rand2 < 50 ->
        Literal is V ;
        Literal is -V),
       Clause = [Literal|NewClause]) ;
      Clause = NewClause),
   V1 is V + 1,
   one_clause(V1, NVars, Density, NewClause).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.


/*creates an enhanced version of the formula that instead of keeping the number
of the variable, keeps a reference to the actual CSP variable in order to avoid
lookups.*/
pack_formula([], _, []).

pack_formula([Disjunction|Rest_formula], S, [Packed_disjunction|Rest_packed_formula]) :-
  pack_disjunction(Disjunction, S, Packed_disjunction),
  pack_formula(Rest_formula, S, Rest_packed_formula).


/*Does the job for a single disjunction term.*/
pack_disjunction([], _, []).

pack_disjunction([Literal|Rest_disjunction], S, [Packed_literal|Rest_packed_disjunction]) :-
  abs(Literal, Index),
  find(S, Index, CSP_var),
  sign(Literal, Sign),
  Packed_literal = pack(Sign, CSP_var),
  pack_disjunction(Rest_disjunction, S, Rest_packed_disjunction).


/*creates a new CSP variable Eval that represents the entire disjunction's truth.*/
evaluate_literals([], []).

evaluate_literals([Packed_literal|Rest_packed_disjunction], [Eval|Rest_evals]) :-
  Packed_literal = pack(Sign, CSP_var),
  Eval #:: [0, 1],
  Sign is 1,
  !,
  Eval #= CSP_var,
  evaluate_literals(Rest_packed_disjunction, Rest_evals).

evaluate_literals([Packed_literal|Rest_packed_disjunction], [Eval|Rest_evals]) :-
  Packed_literal = pack(Sign, CSP_var),
  Eval #:: [0, 1],
  Eval #= 1 - CSP_var,
  evaluate_literals(Rest_packed_disjunction, Rest_evals).


/*creates the variables that represents the truth of all disjunctions.*/
evaluate_disjunctions([], []).

/*this is the case of an empty disjunction, always false*/
evaluate_disjunctions([Packed_disjunction|Rest_packed_formula], [Eval|Rest_evals]) :-
  Packed_disjunction = [],
  !,
  Eval #= 0,
  evaluate_disjunctions(Rest_packed_formula, Rest_evals).


evaluate_disjunctions([Packed_disjunction|Rest_packed_formula], [Eval|Rest_evals]) :-
  evaluate_literals(Packed_disjunction, Literals_evals),
  Eval #:: [0, 1],
  Eval #= max(Literals_evals), /*if at least one is true, the disjunction is true.*/
  evaluate_disjunctions(Rest_packed_formula, Rest_evals).


/*the cost is the number of false disjunctions.*/
cost(Packed_formula, NC, Cost) :-
  evaluate_disjunctions(Packed_formula, Evals),
  Cost #:: [0..NC],
  Cost #= NC - sum(Evals).


sign(X, 1) :-
  X >= 0.

sign(X, -1).


/*finds the element at index Index of a list.*/
find([X|List], Index, X) :-
  Index is 1.

find([_|List], Index, Y) :-
  New_index is Index - 1,
  find(List, New_index, Y).


maxsat(NV, NC, D, F, S, M) :-
  create_formula(NV, NC, D, F),
  length(S, NV),
  S #:: [0, 1],
  pack_formula(F, S, Packed_formula),
  cost(Packed_formula, NC, Cost),
  !,
  bb_min(search(S, 0, most_constrained, indomain_max, complete, []), Cost, bb_options{strategy:dichotomic}),
  M is NC - Cost.
