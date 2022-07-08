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
  X #< Y,
  order([Y|Tail]).

% creates domain variable that consists of the sum of each element in set
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


% Helper predicates that calculate largest and smallest possible sums of I
% numbers in range(1, N). Helps with pruning.
min_sum(I, MS) :-
  MS is I * (I + 1) // 2.

min_sum_sq(I, MSS) :-
  MSS is I * (I + 1) * (2 * I + 1) // 6.

max_sum(I, N, MS) :-
  MS is N * (N + 1) // 2 - (N - I) * ((N - I) + 1) // 2.

max_sum_sq(I, N, MSS) :-
  MSS is N * (N + 1) * (2 * N + 1) // 6 - (N - I) * ((N - I) + 1) * (2 * (N - I) + 1) // 6.

/* Constraints below are based on the following logic. In order to be able to
reach S1Goal when we complete L1's assignment, at each step we must make sure
we haven't gone too low or too high. For example, let's assume N = 8. We need
to find the values of 4 variables that sum up to S1Goal = 36 / 2 = 18.
Let's say we have assigned:
X_1 = 5 and X_2 = 3. Our partial sum up until now is 5 + 3 = 8. We'd
like to add 2 numbers to that so we reach 18. The two biggest numbers we can
add to that are 8 and 7. Therefore the biggest sum we can reach is 8 + 8 + 7
which equals 23 >= 18. Therefore we are safe. But what if we can't keep the sum
low enough? We MUST add two more numbers so we need to stay below 18 even if we
add the two smallest numbers. In this case, there is no problem because
8 + 1 + 2 = 11 <= 18. An example that would not fit is if we had assigned
X_1 = 8, X_2 = 7 and X_3 = 3. In this case, we have already reached 18 and
cannot possibly stay below it, so we would backtrack. Same logic is applied for
partial sums of the squares.*/

make_and_constrain_partial_sums([X|L1], [X|SL1], N, I, S1Goal) :-
  New_I is I + 1,
  make_and_constrain_partial_sums(L1, SL1, X, N, New_I, S1Goal).

make_and_constrain_partial_sums([X], [], _, _, _, _).

make_and_constrain_partial_sums([X|L1], [Z|SL1], Y, N, I, S1Goal) :-
  Z #= X + Y,
  K is N // 2 - I,
  I =< N // 2 - 1,
  max_sum(K, N, Max_S),
  min_sum(K, Min_S),
  Upper_lim is S1Goal - Min_S,
  Lower_lim is S1Goal - Max_S,
  Z #=< Upper_lim,
  Z #>= Lower_lim,
  New_I is I + 1,
  make_and_constrain_partial_sums(L1, SL1, Z, N, New_I, S1Goal).


make_and_constrain_partial_sums_sq([X|L1], [(X * X)|SSL1], N, I, S2Goal) :-
  New_I is I + 1,
  make_and_constrain_partial_sums_sq(L1, SSL1, (X * X), N, New_I, S2Goal).

make_and_constrain_partial_sums_sq([X], [], _, _, _, _).


make_and_constrain_partial_sums_sq([X|L1], [Z|SSL1], Y, N, I, S2Goal) :-
  Z #= X * X + Y,
  K is N // 2 - I,
  I =< N // 2 - 1,
  max_sum_sq(K, N, Max_SS),
  min_sum_sq(K, Min_SS),
  Upper_lim is S2Goal - Min_SS,
  Lower_lim is S2Goal - Max_SS,
  Z #=< Upper_lim,
  Z #>= Lower_lim,
  New_I is I + 1,
  make_and_constrain_partial_sums_sq(L1, SSL1, Z, N, New_I, S2Goal).


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

  %these commands eliminate duplicates
  append([1], _, L1), %solution L1, L2 is same as L2, L1, so force 1 into L1
  %order below also implies alldifferent
  order(L1), %we make sets so order doesn't matter, so we keep ordered version

  % commands below apply sum constraints
  sum1(L1, S1),
  sum2(L1, S2),
  S1 #= S1Goal,
  S2 #= S2Goal,
  make_and_constrain_partial_sums(L1, SL1, N, 1, S1Goal),
  % make_and_constrain_partial_sums_sq(L1, SSL1, N, 1, S2Goal),
  writeln(SL1),
  writeln(SSL1),

  % finally, find all solutions
  search(L1, 0, input_order, indomain, complete, []),
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
