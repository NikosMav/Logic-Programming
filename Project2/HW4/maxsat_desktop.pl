/*README:

The solution to the problem is given as follows. For every clause we create a
mathematical inequality that is augmented with a boolean variable that shows if
the clause is false. For example, for the clause (x1 or x2' or x3) we create
x1 + (1 - x2) + x3 + fc >= 1. All these MUST hold true.
If any of the variables gets the value that makes the clause true, then it is
ok to assign fc = 0. We assign fc = 1 if and only if the clause is not satisfied
with the literal's current assignments. Therefore, we need to minimize the sum
of the newly introduced variables, since they count the number of false clauses.
This is much more expressive than other, naive methods which leads to faster
traversal through the search tree.

DISCLAIMER: This method works super fast for the given inputs because we
assign values to the variables in a way that we consider low costs first,
which means we reach the (relatively low) goals fast. This does not mean that
the problem cannot find solutions with high optimal costs, it just takes longer.
I chose to use this method because most maxsat problems have a relatively low
minimum cost.*/


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


/*computes the contribution of a clause's variables as in README*/
compute_literals_sum([], 0).

compute_literals_sum([Packed_literal|Rest_packed_disjunction], Eval) :-
  Packed_literal = pack(Sign, CSP_var),
  Sign is 1,
  !,
  compute_literals_sum(Rest_packed_disjunction, Rest_eval),
  Eval #= CSP_var + Rest_eval.

compute_literals_sum([Packed_literal|Rest_packed_disjunction], Eval) :-
  Packed_literal = pack(Sign, CSP_var),
  compute_literals_sum(Rest_packed_disjunction, Rest_eval),
  Eval #= 1 - CSP_var + Rest_eval.


/*computes and constrains FC as in README*/
compute_clause_variables([], []).

/*this is the case of an empty disjunction, always false*/
compute_clause_variables([Packed_disjunction|Rest_packed_formula], [FC|Rest_FCs]) :-
  Packed_disjunction = [],
  !,
  FC #= 1,
  compute_clause_variables(Rest_packed_formula, Rest_FCs).


compute_clause_variables([Packed_disjunction|Rest_packed_formula], [FC|Rest_FCs]) :-
  compute_literals_sum(Packed_disjunction, Literals_eval),
  FC #:: [0, 1],
  FC + Literals_eval #>= 1, /*constrain as in README*/
  compute_clause_variables(Rest_packed_formula, Rest_FCs).


cost(Packed_formula, NC, Cost, FCs) :-
  compute_clause_variables(Packed_formula, FCs),
  Cost #:: [0..NC],
  Cost #= sum(FCs).


sign(X, 1) :-
  X >= 0.

sign(X, -1).


/*finds the element at index Index of a list.*/
find([X|List], Index, X) :-
  Index is 1.

find([_|List], Index, Y) :-
  New_index is Index - 1,
  find(List, New_index, Y).


/*list comprehension like 1 - x for x in list*/
inverse([], []).

inverse([X|FCs], [Y|Inv_FCs]):-
  X #= 1 - Y,
  inverse(FCs, Inv_FCs).


maxsat(NV, NC, D, F, S, M) :-
  create_formula(NV, NC, D, F),
  length(S, NV),
  S #:: [0, 1],
  pack_formula(F, S, Packed_formula),
  cost(Packed_formula, NC, Cost, FCs),
  inverse(FCs, Inv_FCs), /*using inverse so that FCs start with 0's but S with 1's*/
  !,
  bb_min(search([Inv_FCs|S], 0, input_order, indomain_max, complete, []), Cost, bb_options{strategy:dichotomic}),
  M is NC - Cost.
