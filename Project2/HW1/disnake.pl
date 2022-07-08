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



% takes a resting snake (table T) and unfolds it into a straight line (list L)
unfold_snake(T, L) :-
  unfold_snake1(T, L).

% current list is not reversed
unfold_snake1([], []).


unfold_snake1(T, L) :-
  append([X], T1, T),
  unfold_snake2(T1, L1),
  append(X, L1, L).


% current list is reversed
unfold_snake2([], []).

unfold_snake2(T, L) :-
  append([X], T1, T),
  unfold_snake1(T1, L1),
  reverse(X, RX),
  append(RX, L1, L).


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
