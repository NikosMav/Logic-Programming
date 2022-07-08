:- lib(ic).
:- lib(branch_and_bound).
activity(a001, act(41,49)).
activity(a002, act(72,73)).
activity(a003, act(80,85)).
activity(a004, act(65,74)).
activity(a005, act(96,101)).
activity(a006, act(49,55)).
activity(a007, act(51,59)).
activity(a008, act(63,65)).
activity(a009, act(66,69)).
activity(a010, act(80,87)).
activity(a011, act(71,76)).
activity(a012, act(64,68)).
activity(a013, act(90,93)).
activity(a014, act(49,56)).
activity(a015, act(23,29)).
activity(a016, act(94,101)).
activity(a017, act(25,34)).
activity(a018, act(51,54)).
activity(a019, act(13,23)).
activity(a020, act(67,72)).
activity(a021, act(19,21)).
activity(a022, act(12,16)).
activity(a023, act(99,104)).
activity(a024, act(92,94)).
activity(a025, act(74,83)).
activity(a026, act(95,100)).
activity(a027, act(39,47)).
activity(a028, act(39,49)).
activity(a029, act(37,39)).
activity(a030, act(57,66)).
activity(a031, act(95,101)).
activity(a032, act(71,74)).
activity(a033, act(86,93)).
activity(a034, act(51,54)).
activity(a035, act(74,83)).
activity(a036, act(75,81)).
activity(a037, act(33,43)).
activity(a038, act(29,30)).
activity(a039, act(58,60)).
activity(a040, act(52,61)).
activity(a041, act(35,39)).
activity(a042, act(46,51)).
activity(a043, act(71,72)).
activity(a044, act(17,24)).
activity(a045, act(94,103)).
activity(a046, act(77,87)).
activity(a047, act(83,87)).
activity(a048, act(83,92)).
activity(a049, act(59,62)).
activity(a050, act(2,4)).
activity(a051, act(86,92)).
activity(a052, act(94,103)).
activity(a053, act(80,81)).
activity(a054, act(39,46)).
activity(a055, act(60,67)).
activity(a056, act(72,78)).
activity(a057, act(58,61)).
activity(a058, act(8,18)).
activity(a059, act(12,16)).
activity(a060, act(47,50)).
activity(a061, act(49,50)).
activity(a062, act(71,78)).
activity(a063, act(34,42)).
activity(a064, act(21,26)).
activity(a065, act(92,95)).
activity(a066, act(80,81)).
activity(a067, act(74,79)).
activity(a068, act(28,29)).
activity(a069, act(100,102)).
activity(a070, act(29,37)).
activity(a071, act(4,12)).
activity(a072, act(79,83)).
activity(a073, act(98,108)).
activity(a074, act(91,100)).
activity(a075, act(82,91)).
activity(a076, act(59,66)).
activity(a077, act(34,35)).
activity(a078, act(51,60)).
activity(a079, act(92,94)).
activity(a080, act(77,83)).
activity(a081, act(38,48)).
activity(a082, act(51,59)).
activity(a083, act(35,39)).
activity(a084, act(22,24)).
activity(a085, act(67,68)).
activity(a086, act(90,97)).
activity(a087, act(82,83)).
activity(a088, act(51,53)).
activity(a089, act(78,88)).
activity(a090, act(74,79)).
activity(a091, act(100,105)).
activity(a092, act(53,63)).
activity(a093, act(57,66)).
activity(a094, act(32,41)).
activity(a095, act(48,56)).
activity(a096, act(92,96)).
activity(a097, act(4,8)).
activity(a098, act(31,33)).
activity(a099, act(69,77)).
activity(a100, act(88,93)).


/*finds the element at index Index of a list.*/
find([X|_], Index, X) :-
  Index is 1.

find([_|List], Index, Y) :-
  New_index is Index - 1,
  find(List, New_index, Y).

%calculates time cost for each activity
make_costs([], [], []).

make_costs([Start_time|Rest_start_time_list], [End_time|Rest_end_time_list], [Cost|Rest_cost_list]):-
  Cost is End_time - Start_time,
  make_costs(Rest_start_time_list, Rest_end_time_list, Rest_cost_list).

%makes a task set, namely a bit array that represents which activities this
%person takes on. For example, a value of [1, 0, 0, 1] means that this person
%has to do activities 1 and 4.
make_task_set(0, []).

make_task_set(Task_count, [Task_bit|Rest_task_set]):-
  Task_bit #:: [0, 1],
  New_task_count is Task_count - 1,
  make_task_set(New_task_count, Rest_task_set).

%makes a task set for every person.
make_task_sets(0, [], _).

make_task_sets(NP, [Task_set|Rest_task_sets], Task_count):-
  make_task_set(Task_count, Task_set),
  New_NP is NP - 1,
  make_task_sets(New_NP, Rest_task_sets, Task_count).

%ensures that if an activity is assigned to a person during the search, said
%person's task set is updated accordingly.
constrain_set_against_tasks([], [], _).

constrain_set_against_tasks([Task|Rest_tasks], [Task_bit|Rest_task_set], Task_set_number):-
  Task_bit #= (Task =:= Task_set_number),
  constrain_set_against_tasks(Rest_tasks, Rest_task_set, Task_set_number).


%does the above for every person.
constrain_against_tasks(_, [], _).

constrain_against_tasks(Tasks, [Task_set|Rest_task_sets], Task_set_number):-
  constrain_set_against_tasks(Tasks, Task_set, Task_set_number),
  New_task_set_number is Task_set_number + 1,
  constrain_against_tasks(Tasks, Rest_task_sets, New_task_set_number).

%returns a domain variable that represents the time a person has to work.
compute_sum_of_costs([], [], 0).

compute_sum_of_costs([Cost|Rest_cost_list], [Task_bit|Rest_task_set], Total_work):-
  compute_sum_of_costs(Rest_cost_list, Rest_task_set, Rest_work),
  Current_work #= Cost * Task_bit,
  Total_work #= Current_work + Rest_work.

%makes sure no person get assigned too many tasks.
constrain_set_against_sum_of_work(Cost_list, Task_set, ST, Total_work):-
  compute_sum_of_costs(Cost_list, Task_set, Total_work),
  Total_work #=< ST.

%does the above for every person.
constrain_against_sum_of_work(_, [], _, []).

constrain_against_sum_of_work(Cost_list, [Task_set|Rest_task_sets], ST, Total_works):-
  constrain_set_against_sum_of_work(Cost_list, Task_set, ST, Total_work),
  constrain_against_sum_of_work(Cost_list, Rest_task_sets, ST, Rest_total_works),
  append([Total_work], Rest_total_works, Total_works).

%Imposes a constraint on pairs of bits whose activities conflict each other.
%For every pair of activities that cannot be carried out by the same person,
%the expression not (task_bit_1 and task_bit_2) MUST hold true.
%With some boolean algebra manipulation, we encode this constraint as shown below.
constrain_set_against_conflict(Conflict, Task_set):-
  Conflict = conflict(Task_index_1, Task_index_2),
  find(Task_set, Task_index_1, Task_bit_1),
  find(Task_set, Task_index_2, Task_bit_2),
  Safety_check #= (1 - Task_bit_1 + 1 - Task_bit_2 >= 1),
  Safety_check #= 1.

%imposes constraints for all task sets for a single conflict
constrain_against_conflict(_, []).

constrain_against_conflict(Conflict, [Task_set|Rest_task_sets]):-
  constrain_set_against_conflict(Conflict, Task_set),
  constrain_against_conflict(Conflict, Rest_task_sets).

%imposes the constraint for all conflicts
constrain_against_conflicts([], _).

constrain_against_conflicts([Conflict|Rest_conflicts], Task_sets):-
  constrain_against_conflict(Conflict, Task_sets),
  constrain_against_conflicts(Rest_conflicts, Task_sets).


%inititates the various constraints.
constrain_task_sets(Tasks, Conflicts, Cost_list, Task_sets, ST, Total_works):-
  constrain_against_tasks(Tasks, Task_sets, 1),
  constrain_against_sum_of_work(Cost_list, Task_sets, ST, Total_works),
  constrain_against_conflicts(Conflicts, Task_sets).


%decides if two activities conflict each other.
conflicting(_, End_time, Other_start_time, Other_end_time):-
  End_time < Other_end_time,
  !,
  First_end is End_time + 1,
  Other_start_time < First_end.

conflicting(Start_time, _, _, Other_end_time):-
  First_end = Other_end_time + 1,
  Start_time < First_end.

%gathers the conflicts in which Task is the first member of the conflicting pair.

identify_task_conflicts(_, _, _, _, [], _, [], [], []).

identify_task_conflicts(Task, Task_index, Start_time, End_time, [_|Rest_tasks], Other_task_index, [Other_start_time|Rest_start_time_list], [Other_end_time|Rest_end_time_list], Task_conflicts):-
  conflicting(Start_time, End_time, Other_start_time, Other_end_time),
  !, %if conflicting, add the duo to conflicts
  Conflict = conflict(Task_index, Other_task_index),
  New_other_task_index is Other_task_index + 1,
  identify_task_conflicts(Task, Task_index, Start_time, End_time, Rest_tasks, New_other_task_index, Rest_start_time_list, Rest_end_time_list, Rest_task_conflicts),
  append([Conflict], Rest_task_conflicts, Task_conflicts).


identify_task_conflicts(Task, Task_index, Start_time, End_time, [_|Rest_tasks], Other_task_index, [_|Rest_start_time_list], [_|Rest_end_time_list], Task_conflicts):-
  New_other_task_index is Other_task_index + 1,
  identify_task_conflicts(Task, Task_index, Start_time, End_time, Rest_tasks, New_other_task_index, Rest_start_time_list, Rest_end_time_list, Task_conflicts).

%gathers the conflicts for all pairs of tasks.
identify_conflicts([_], _, [_], [_], []).

identify_conflicts([Task|Rest_tasks], Task_index, [Start_time|Rest_start_time_list], [End_time|Rest_end_time_list], Conflicts):-
  New_task_index is Task_index + 1,
  identify_task_conflicts(Task, Task_index, Start_time, End_time, Rest_tasks, New_task_index, Rest_start_time_list, Rest_end_time_list, Task_conflicts),
  identify_conflicts(Rest_tasks, New_task_index, Rest_start_time_list, Rest_end_time_list, Rest_conflicts),
  append(Task_conflicts, Rest_conflicts, Conflicts).


%We use this constraint in order to avoid receiving duplicate solutions.
%By straight up mirroring the logic with which I ensured the above in exercise 2,
%we can see that we only need to disallow tasks being assigned to a person that is
%too far "ahead". For example, if we have assigned task 1 to person 1, we cannot
%assign task 2 to person k if k is greater than 2. To start this chain of constraints,
%we hard code task 1 to person 1 (it has to be assigned to someone, and every person
%is indistiguishable so we might as well assign it to person 1).
% constrain_for_search_tree([_]).
%
% constrain_for_search_tree(Tasks):-
%   append(Rest_tasks, [Task], Tasks),
%   constrain_for_search_tree(Rest_tasks),
%   Max_predecessor #= max(Rest_tasks),
%   Task #=< Max_predecessor + 1.

convert_to_ASA([], [], []).

convert_to_ASA([Activity|Rest_activity_list], [Task|Rest_tasks], ASA):-
  convert_to_ASA(Rest_activity_list, Rest_tasks, Rest_ASA),
  X = Activity - Task,
  append([X], Rest_ASA, ASA).



convert_ASA_to_cluster([], [], [], _, [], 0).

convert_ASA_to_cluster([Entry|Rest_ASA], [Activity_name|Rest_activity_list], [Cost|Rest_cost_list], Index, Cluster, Time):-
  Entry = Activity_name - Task,
  Task is Index,
  !,
  convert_ASA_to_cluster(Rest_ASA, Rest_activity_list, Rest_cost_list, Index, Rest_cluster, Rest_time),
  append([Activity_name], Rest_cluster, Cluster),
  Time is Cost + Rest_time.

convert_ASA_to_cluster([_|Rest_ASA], [_|Rest_activity_list], [_|Rest_cost_list], Index, Cluster, Time):-
  convert_ASA_to_cluster(Rest_ASA, Rest_activity_list, Rest_cost_list, Index, Cluster, Time).


convert_ASA_to_ASP(_, _, _, Index, [], NP):-
  Index > NP,
  !.

convert_ASA_to_ASP(ASA, Activity_list, Cost_list, Index, [Current_bunch|Rest_ASP], NP):-
  convert_ASA_to_cluster(ASA, Activity_list, Cost_list, Index, Cluster, Time),
  Current_bunch = Index - Cluster - Time,
  New_index is Index + 1,
  convert_ASA_to_ASP(ASA, Activity_list, Cost_list, New_index, Rest_ASP, NP).


%stolen from stack overflow, takes first N elements of a list.
take(N, _, Xs) :-
  N =< 0,
  !,
  N =:= 0,
  Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
  M is N-1,
  take(M, Xs, Ys).

%basically a wrapper for take in order to handle NF == 0
limit_activities(0, X, Y, Z, X, Y, Z).

limit_activities(NF, Complete_activity_list, Complete_start_time_list, Complete_end_time_list, Activity_list, Start_time_list, End_time_list):-
  NF =\= 0,
  !,
  take(NF, Complete_activity_list, Activity_list),
  take(NF, Complete_start_time_list, Start_time_list),
  take(NF, Complete_end_time_list, End_time_list).


%does all the work of formulating the csp
formulate_csp(NP, ST, NF, Tasks, Activity_list, Cost_list, Total_works):-
  findall(A, activity(A, _), Complete_activity_list),
  findall(S, activity(_, act(S, _)), Complete_start_time_list),
  findall(E, activity(_, act(_, E)), Complete_end_time_list),
  limit_activities(NF, Complete_activity_list, Complete_start_time_list, Complete_end_time_list, Activity_list, Start_time_list, End_time_list),

  length(Activity_list, Len),
  length(Tasks, Len),
  Tasks #:: [1..NP],

  % %hard code task 1 to person 1, as stated above (in constrain_for_search_tree)
  % find(Tasks, 1, First_task),
  % First_task #= 1,

  make_costs(Start_time_list, End_time_list, Cost_list),
  make_task_sets(NP, Task_sets, Len),
  identify_conflicts(Tasks, 1, Start_time_list, End_time_list, Conflicts),
  constrain_task_sets(Tasks, Conflicts, Cost_list, Task_sets, ST, Total_works).
  % constrain_for_search_tree(Tasks).


%stolen from stack overflow
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

%calculates A
optimal_workload(Cost_list, NP, A):-
  sum_list(Cost_list, Total_cost),
  Real_optimal_workload is Total_cost / NP,
  Float_A is round(Real_optimal_workload),
  A is integer(Float_A).


%calculates the variance of the workloads
add_sq_diffs(_, [], 0).

add_sq_diffs(A, [Total_work|Rest_total_works], Cost):-
  add_sq_diffs(A, Rest_total_works, Rest_cost),
  Diff #= A - Total_work,
  Current_cost #= Diff * Diff ,
  Cost #= Rest_cost + Current_cost.

%handles calculating the cost function to be minimized
make_cost_function(Cost_list, NP, Total_works, Cost):-
  optimal_workload(Cost_list, NP, A),
  add_sq_diffs(A, Total_works, Cost).


available_works([], _, _, [], []).

available_works([Total_work|Rest_total_works], Domain, Worker_index, [Actual_work|Rest_actual_works], [Worker_index|Rest_unsorted_candidates]):-
  member(Worker_index, Domain),
  !,
  get_min(Total_work, Actual_work),
  New_worker_index is Worker_index + 1,
  available_works(Rest_total_works, Domain, New_worker_index, Rest_actual_works, Rest_unsorted_candidates).

available_works([_|Rest_total_works], Domain, Worker_index, Rest_actual_works, Rest_unsorted_candidates):-
  New_worker_index is Worker_index + 1,
  available_works(Rest_total_works, Domain, New_worker_index, Rest_actual_works, Rest_unsorted_candidates).

merge_works_with_candidates([], [], []).

merge_works_with_candidates([Actual_work|Rest_actual_works], [Unsorted_candidate|Rest_unsorted_candidates], [Merge|Rest_merges]):-
  Merge = Actual_work - Unsorted_candidate,
  merge_works_with_candidates(Rest_actual_works, Rest_unsorted_candidates, Rest_merges).

unmerge([], []).

unmerge([Sorted_merge|Rest_sorted_merges], [Candidate|Rest_candidates]):-
  Sorted_merge = _ - Candidate,
  unmerge(Rest_sorted_merges, Rest_candidates).

available_workers_by_laziness(Total_works, Domain, Candidates):-
  available_works(Total_works, Domain, 1, Actual_works, Unsorted_candidates),
  merge_works_with_candidates(Actual_works, Unsorted_candidates, Merged),
  sort(Merged, Sorted_merge),
  unmerge(Sorted_merge, Candidates).



laziest_worker(New_max_index, _, Previous_max_index, New_max_index):-
  New_max_index is Previous_max_index + 1.

laziest_worker(Task, Total_works, Max_index, Max_index):-
  Task #=< Max_index,
  get_domain_as_list(Task, Domain),
  available_workers_by_laziness(Total_works, Domain, Candidates),
  member(Task, Candidates).


assignment_csp(NP, ST, ASP, ASA):-
  formulate_csp(NP, ST, 0, Tasks, Activity_list, Cost_list, Total_works),
  !,
  search(Tasks, 0, first_fail, laziest_worker(Total_works, 0, _), complete, []),
  convert_to_ASA(Activity_list, Tasks, ASA),
  convert_ASA_to_ASP(ASA, Activity_list, Cost_list, 1, ASP, NP).


assignment_opt(NF, NP, ST, F, T, ASP, ASA, Cost):-
  formulate_csp(NP, ST, NF, Tasks, Activity_list, Cost_list, Total_works),
  make_cost_function(Cost_list, NP, Total_works, Cost),
  !,
  bb_min(search(Tasks, 0, first_fail, laziest_worker(Total_works, 0, _), complete, []), Cost, bb_options{strategy:continue, timeout:T, factor:F}),
  convert_to_ASA(Activity_list, Tasks, ASA),
  convert_ASA_to_ASP(ASA, Activity_list, Cost_list, 1, ASP, NP).
