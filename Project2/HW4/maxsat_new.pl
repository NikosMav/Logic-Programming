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
  Eval #= CSP_var * Sign + max(0, -Sign),
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
  writeln("making graph"),
  make_graph(F, V, E),
  % writeln(V),
  % writeln(E),
  length(S, NV),
  S #:: [0, 1],
  pack_formula(F, S, Packed_formula),
  cost(Packed_formula, NC, Cost),
  !,
  bb_min(search(S, 0, first_fail, indomain, complete, []), Cost, bb_options{strategy:dichotomic}),
  M is NC - Cost.



make_graph(Formula, Vertices, Edges) :-
  length(Formula, NC),
  make_list(NC, Vertices),
  writeln("made vertices"),
  writeln("making first egdes"),
  get_edges(Formula, Temp_edges, 1),
  writeln("made first egdes"),
  reverse(Formula, Reversed_Formula),
  writeln("making second egdes"),
  get_edges(Reversed_Formula, Temp_mirrored_edges, 1),
  writeln("made second egdes"),
  flip_inside(Temp_mirrored_edges, NC, Reverse_mirrored_edges),
  reverse(Reverse_mirrored_edges, Mirrored_edges),
  merge_edges(Temp_edges, Mirrored_edges, Edges1),
  sort_inside(Edges1, Edges).

flip_inside([], _, []).

flip_inside([X|Temp_edges], NC, [Y|Mirrored_edges]):-
  flip(X, NC, Y),
  flip_inside(Temp_edges, NC, Mirrored_edges).

flip([], _, []).

flip([X|L1], NC, [Y|L2]):-
  Y is NC + 1 - X,
  flip(L1, NC, L2).


merge_edges([], [], []).

merge_edges([X|Temp_edges], [Y|Mirrored_edges], [Z|Edges]) :-
  append(X, Y, Z),
  merge_edges(Temp_edges, Mirrored_edges, Edges).


sort_inside([], []).

sort_inside([X|TE], [Y|E]):-
  sort(X, Y),
  sort_inside(TE, E).

get_edges([], [], _).

get_edges([Disjunction|Rest_formula], [Dis_edges|Rest_edges], Dis_number) :-
  get_dis_edges(Disjunction, Rest_formula, Dis_number, Dis_edges),
  Next_dis_number is Dis_number + 1,
  get_edges(Rest_formula, Rest_edges, Next_dis_number).


get_dis_edges([], _, _, []).

get_dis_edges([Literal|Rest_disjunction], Rest_formula, Dis_number, Dis_edges) :-
  Other_dis_number is Dis_number + 1,
  get_dis_edges_by_literal(Literal, Rest_formula, Other_dis_number, Dis_edges_by_literal),
  get_dis_edges(Rest_disjunction, Rest_formula, Dis_number, Rest_dis_edges),
  append(Dis_edges_by_literal, Rest_dis_edges, Dis_edges).

get_dis_edges_by_literal(_, [], _, []).

get_dis_edges_by_literal(Literal, [Other_dis|Rest_formula], Other_dis_number, Dis_edges_by_literal):-
  New_other_dis_number is Other_dis_number + 1,
  get_dis_edges_by_literal(Literal, Rest_formula, New_other_dis_number, Rest_dis_edges_by_literal),
  dis_has_lit(Literal, Other_dis),
  !,
  append([Other_dis_number], Rest_dis_edges_by_literal, Dis_edges_by_literal).

get_dis_edges_by_literal(Literal, [Other_dis|Rest_formula], Other_dis_number, Dis_edges_by_literal):-
  New_other_dis_number is Other_dis_number + 1,
  get_dis_edges_by_literal(Literal, Rest_formula, New_other_dis_number, Dis_edges_by_literal).


dis_has_lit(Literal, [Other_lit|Rest_dis]):-
  abs(Literal, Abslit),
  abs(Other_lit, Other_abs_lit),
  Abslit == Other_abs_lit.

dis_has_lit(Literal, [Other_lit|Rest_dis]):-
  abs(Literal, Abslit),
  abs(Other_lit, Other_abs_lit),
  Abslit =\= Other_abs_lit,
  dis_has_lit(Literal, Rest_dis).

between(I, J, I) :-
  I =< J.
between(I, J, X) :-
  I < J,
  I1 is I+1,
  between(I1, J, X).


% makes a list from 1 to N
make_list(N, L):-
  findall(Num, between(1, N, Num), L).
