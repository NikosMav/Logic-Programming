% creates a new list with the length of the input list
new_list([], []).

new_list([X|L], NL) :-
  new_list(L, NL1),
  append([_], NL1, NL).

% creates a 2D array made out of the input lists
table(_, [], []).

table(W, [_|H], [NW|N]) :-
  table(W, H, N),
  new_list(W, NW).

% checks if a list has an even number of items
even([]).
even([_|L]) :-
  odd(L).

% checks if a list has an odd number of items
odd([_]).

odd([_|L]) :-
  even(L).

% takes a resting snake (table T) and unfolds it into a straight line (list L)
unfold_snake(T, L) :-
  even(T),
  unfold_snake_even1(T, L).

unfold_snake(T, L) :-
  odd(T),
  unfold_snake_odd1(T, L).

/* when the height is even, reversing a list on one edge means
   we dont reverse the list on the other edge */
unfold_snake_even1([], []).

unfold_snake_even1(T, L) :-
  append([X|L1], [Y], T), % X and Y are the lists on the edges
  unfold_snake_even2(L1, RL1),
  reverse(Y, RY), % last list needs to be reversed as it is on an even index
  append(X, RL1, L2),
  append(L2, RY, L). % append commands combine the solved subproblems

unfold_snake_even2([], []).

unfold_snake_even2(T, L) :-
  append([X|L1], [Y], T),
  unfold_snake_even1(L1, RL1),
  reverse(X, RX), /* now we reverse the first list and not the last,
                  because of its index */
  append(RX, RL1, L2),
  append(L2, Y, L). % append commands combine the solved subproblems

% logic is pretty much the same below, but we either reverse both lists or none.
unfold_snake_odd1([X], X).

unfold_snake_odd1(T, L) :-
  append([X|L1], [Y], T),
  unfold_snake_odd2(L1, RL1),
  append(X, RL1, L2),
  append(L2, Y, L).

unfold_snake_odd2([X], Y) :-
  reverse(X, Y).

unfold_snake_odd2(T, L) :-
  append([X|L1], [Y], T),
  unfold_snake_odd1(L1, RL1),
  reverse(X, RX),
  reverse(Y, RY),
  append(RX, RL1, L2),
  append(L2, RY, L).

% checks if a list (pattern) is repeated in another list (unfolded snake)
repeats(_, []).

repeats([X|P], [X|L]) :-
  append(P, [X], NP), /* we put the element we checked against
                      at the end of the list for future use*/
  repeats(NP, L).

disnake(Pattern, Width, Height) :-
  table(Width, Height, T),
  unfold_snake(T, L),
  !, /* cut needed or else we get the same answer twice
      (for some reason unfold_snake works with 2 possible ways)*/
  repeats(Pattern, L),
  pretty_print(T).

pretty_print([]).

pretty_print([X|T]) :-
  pretty_write(X),
  pretty_print(T).

pretty_write([]) :-
  nl.

pretty_write([X|L]) :-
  write(X),
  pretty_write(L).
