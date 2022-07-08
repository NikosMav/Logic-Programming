/*merges activities, start times and end times into one list*/
merge_lists([], [], [], []).

merge_lists([X|L1], [Y|L2], [Z|L3], FL) :-
  CA = compact_activity(X, Y, Z),
  merge_lists(L1, L2, L3, RL),
  append([CA], RL, FL).

assignment(NP, ST, ASP, ASA) :-
  findall(A, activity(A, _), Activity_list),
  findall(S, activity(_, act(S, _)), Start_time_list),
  findall(E, activity(_, act(_, E)), End_time_list),
  merge_lists(Activity_list, Start_time_list, End_time_list, Compact_activity_list),
  assign_tasks(Compact_activity_list, NP, ST, Assignments),
  convert_to_ASP(Assignments, ASP, 1),
  convert_ASP_to_ASA(ASP, ASA, Activity_list).

  convert_to_ASP([], [], _).

  convert_to_ASP([First_cluster|Rest_assignments], [X|Rest_of_ASP], Index) :-
    First_cluster = cluster(Time, List),
    New_index is Index + 1,
    labelize_list(List, Final_list),
    X = Index - Final_list - Time,
    convert_to_ASP(Rest_assignments, Rest_of_ASP, New_index).

convert_ASP_to_ASA(ASP, ASA, Activity_list) :-
  bunch_up(ASP, Helper_list),
  sort_activities(Helper_list, Activity_list, ASA).

sort_activities(_, [], []).

sort_activities(Helper_list, [Current_activity|Rest_activity_list], ASA) :-
  find_activity(Helper_list, Current_activity, Found_activity),
  sort_activities(Helper_list, Rest_activity_list, RASA),
  append([Found_activity], RASA, ASA).

find_activity([X|_], Activity, X) :-
  X = Activity - _,
  !.

find_activity([_|Rest_helper_list], Activity, Y) :-
  find_activity(Rest_helper_list, Activity, Y).

/*makes unsorted list of Activity - Worker_number*/
bunch_up([], []).

bunch_up([X|RASP], Helper_list) :-
  X = Worker_number - Task_list - _,
  fill(Task_list, Helper_list_1, Worker_number),
  bunch_up(RASP, Helper_list_2),
  append(Helper_list_1, Helper_list_2, Helper_list).

/*puts the activities of one worker into the unsorted list of activities*/
fill([], [], _).

fill([Task1|Task_list], [Y|Helper_list], Worker_number) :-
  fill(Task_list, Helper_list, Worker_number),
  Y = Task1 - Worker_number.

/*keeps only the activity labels of a compact activity list*/
labelize_list([], []).

labelize_list([X|RCList], [Y|RFList]) :-
  X = compact_activity(Y, _, _),
  labelize_list(RCList, RFList).


/*Main search routine, works as follows. We assign each and every activity
either to an already hired worker or hiring another one for the job. In terms
of discrete mathematics, the problem boils down to grouping n elements to  at
most k groups, since workers cannot be distinguished from each other.
We can solve this problem recursively. We keep the first of the n elements.
From there on we have 2 choices. Either group the remaining n - 1 elements to
at most k groups and add the first element in one of them, or group the
remaining n - 1 elements to at most k - 1 groups and make the first element
a group on its own. This produces all possible solutions to our problem only
once, since we impose a strict ordering on the groups based on their creation
timestamp and also assign the activities with a strict ordering (the order
in which they are given to us).*/

assign_tasks([], Max_people, _, []) :-
  Max_people >= 0.

/*Creation of new group*/

assign_tasks([First_task|Rest_tasks], Max_people, Max_time, Assignments) :-
  New_max_people is Max_people - 1,
  New_max_people >= 0,
  assign_tasks(Rest_tasks, New_max_people, Max_time, Rest_assignments),
  create_cluster(First_task, New_cluster),
  New_cluster = cluster(Time, _),
  Max_time >= Time,
  append([New_cluster], Rest_assignments, Assignments).

/*Merging to already existing group*/

assign_tasks([First_task|Rest_tasks], Max_people, Max_time, Assignments) :-
  assign_tasks(Rest_tasks, Max_people, Max_time, Rest_assignments),
  add_task(First_task, Max_time, Rest_assignments, Assignments).


add_task(Task, Max_time, [First_starting_cluster|Rest_clusters], [First_ending_cluster|Rest_clusters]) :-
  Task = compact_activity(_, S, E),
  Time_diff is E - S,
  First_starting_cluster = cluster(Starting_total_time, Starting_activity_list),
  First_ending_cluster = cluster(Ending_total_time, Ending_activity_list),
  Ending_total_time is Starting_total_time + Time_diff,
  Max_time >= Ending_total_time,
  legal_combination(Task, Starting_activity_list),
  append([Task], Starting_activity_list, Ending_activity_list).

add_task(Task, Max_time, [Same_cluster|Rest_starting_clusters], [Same_cluster|Rest_ending_clusters]) :-
  add_task(Task, Max_time, Rest_starting_clusters, Rest_ending_clusters).

/*checks for time related constraints, helps with pruning, just like
checking against Max_time does*/

legal_combination(_, []).

legal_combination(Task, [Other_task|Rest_tasks]) :-
  legal_combination(Task, Rest_tasks),
  Task = compact_activity(_, _, E1),
  Other_task = compact_activity(_, S2, E2),
  E1 < E2,
  Real_end is E1 + 1,
  S2 >= Real_end,
  !.

legal_combination(Task, [Other_task|Rest_tasks]) :-
  legal_combination(Task, Rest_tasks),
  Task = compact_activity(_, S1, _),
  Other_task = compact_activity(_, _, E2),
  Real_end is E2 + 1,
  S1 >= Real_end.


create_cluster(CA, C) :-
  CA = compact_activity(_, S, E),
  Time_diff is E - S,
  C = cluster(Time_diff, [CA]).
