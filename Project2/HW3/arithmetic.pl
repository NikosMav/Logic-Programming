min_sum(I, MS) :-
  MS is I * (I + 1) / 2.

min_sum_sq(I, MSS) :-
  MSS is I * (I + 1) * (2 * I + 1) / 6.

max_sum(I, N, MS) :-
  MS is N * (N + 1) / 2 - (N - I) * ((N - I) + 1) / 2.

max_sum_sq(I, N, MSS) :-
  MSS is N * (N + 1) * (2 * N + 1) / 6 - (N - I) * ((N - I) + 1) * (2 * (N - I) + 1) / 6.
