:- lib(ic).

% tells us if a number is even or not
even(0).

even(N) :-
  N1 is N - 2,
  N > 0,
  even(N1).

%tells us if an array is ordered
order([X]).

order([X,Y|Tail]):-
  X #=< Y,
  order([Y|Tail]).

% creates domain variable that consist of the sum of each element in set
sum1([X], Y) :-
  Y #= X.

sum1([X|L1], S1) :-
  sum1(L1, RS1),
  S1 #= RS1 + X.

% same but with squares
sum2([X], Y) :-
  Y #= X * X.

sum2([X|L1], S1) :-
  sum2(L1, RS1),
  S1 #= RS1 + X * X.


/*The problem can be thought of as follows. Define SI(X) to be the sum of the
elements of set X, and SS(X) the sum of their squares. We need to find two sets
such that SI(L1) = SI(L2) (1) and SS(L1) = SS(L2) (2). Because these two sets' union
is the set L = {1, 2, ..., N} we know that SI(L) = SI(L1) + SI(L2) (3) and
SS(L) = SS(L1) + SS(L2) (4). We also know that SI(L) = n(n+1)/2 (5) and
SS(L) = n(n+1)(2n+1)/6 (6). Equations 1, 3 and 5 give SI(L1) = n(n+1)/4
while equations 2, 4 and 6 give SS(L1) = n(n+1)(2n+1)/12. Therefore, we only
need to find such an L1, and from then on it is easy to calculate L2 by taking
L1's complement with respect to L. This realization halves the depth of the
search tree, as we only need to assign values to n/2 variables instead of n
in a naive approach.*/


numpart(N, L1, L2) :-
  even(N),
  N_half is N // 2,
  length(L1, N_half),
  make_list(N, L), % used to compute L2
  L1 #:: 1..N,
  S1Goal is N * (N + 1) // 4, % sum of first n integers over 2
  S2Goal is N * (N + 1) * (2 * N  + 1) // 12, % same for squares
  alldifferent(L1),

  %these commands eliminate duplicates
  append([1], _, L1), %solution L1, L2 is same as L2, L1, so force 1 into L1
  order(L1), %we make sets so order doesn't matter, so we keep ordered version

  % commands below apply sum constraints
  sum1(L1, S1),
  sum2(L1, S2),
  S1 #= S1Goal,
  S2 #= S2Goal,

  % finally, find all solutions
  search(L1, 0, first_fail, indomain, complete, []),
  complement(L1, L2, L). % compute L2 for complete solution

between(I, J, I) :-
  I =< J.
between(I, J, X) :-
  I < J,
  I1 is I+1,
  between(I1, J, X).


% makes a list from 1 to N
make_list(N, L):-
  findall(Num, between(1, N, Num), L).


%calculates the set L - L1 (L2)
complement([], [], []).

complement([X|L1], L2, [X|L]) :-
  complement(L1, L2, L).

complement(L1, [X|L2], [X|L]) :-
  complement(L1, L2, L).
