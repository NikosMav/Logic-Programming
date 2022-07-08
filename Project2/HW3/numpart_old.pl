:- lib(ic). % Φορτώστε τη βιβλιοθήκη ic

% tells us if a number is even or not
even(N) :-
  N1 is N // 2,
  N is N1 * 2.

%tells us if an array is ordered
order([X]).

order([X,Y|Tail]):-
  X #=< Y,
  order([Y|Tail]).

% creates domain variables that consist of the sums of each element in the sets
sum1([X], [Y], X, Y).

sum1([X|L1], [Y|L2], S1, S2) :-
  sum1(L1, L2, RS1, RS2),
  S1 #= RS1 + X,
  S2 #= RS2 + Y.

% same but with squares
sum2([X], [Y], X * X, Y * Y).

sum2([X|L1], [Y|L2], S1, S2) :-
  sum2(L1, L2, RS1, RS2),
  S1 #= RS1 + X * X,
  S2 #= RS2 + Y * Y.




numpart(N, L1, L2) :-
  even(N),
  N_half is N // 2,
  length(L1, N_half),
  length(L2, N_half),
  L1 #:: 1..N,
  L2 #:: 1..N,
  append(L1, L2, List), % used for search

  alldifferent(List),

  %these commands eliminate duplicates
  append([1], _, L1), %solution L1, L2 is same as L2, L1, so force 1 into L1
  order(L1), %we make sets so order doesn't matter, so we keep ordered version
  order(L2), %same

  % commands below apply sum constraints
  sum1(L1, L2, S1, S2),
  S1 #= S2,
  sum2(L1, L2, SS1, SS2),
  SS1 #= SS2,

  % finally, find all solutions
  search(List, 0, first_fail, indomain, complete, []),
  writeln(L1),
  writeln(L2).
