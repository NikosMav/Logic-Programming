:- lib(ic).
:- lib(branch_and_bound).

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


  make_costs(Start_time_list, End_time_list, Cost_list),
  make_task_sets(NP, Task_sets, Len),
  identify_conflicts(Tasks, 1, Start_time_list, End_time_list, Conflicts),
  constrain_task_sets(Tasks, Conflicts, Cost_list, Task_sets, ST, Total_works).


%stolen from stack overflow
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

%calculates A
optimal_workload(Cost_list, NP, A, Total_cost):-
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
%also calculates the minimum possible cost as follows.
%in order to achieve the most even distribution we will most definitely use
%all NP workers.
%we notice that if Total_cost is an integer multiple of NP, we can achieve
%distribution with 0 cost, theoretically. However, if there is a division residue,
%this will impact the least cost in the following ways.
%the best way to distribute the work is to have N_1 people do W_1 work each and
%N_2 people do W_2 work such that N_1 + N_2 = NP, N_1 * W_1 + N_2 * W_2 = Total_cost
%and W2 = W1 + 1. it is obvious that N_2 equals the residue of the abovementioned division.
%the (minimum) cost is either N_2 itself or NP - N2 because of the rounding of A.
make_cost_function(Cost_list, NP, Total_works, Least_cost, Cost):-
  optimal_workload(Cost_list, NP, A, Total_cost),
  add_sq_diffs(A, Total_works, Cost),
  Offset is Total_cost mod NP,
  Other_offset is NP - Offset,
  Least_cost is min(Offset, Other_offset).

%returns an array with the current workloads of the workers that can take on
%a task based on its domain.
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

%used for sorting parallel arrays
merge_works_with_candidates([], [], []).

merge_works_with_candidates([Actual_work|Rest_actual_works], [Unsorted_candidate|Rest_unsorted_candidates], [Merge|Rest_merges]):-
  Merge = Actual_work - Unsorted_candidate,
  merge_works_with_candidates(Rest_actual_works, Rest_unsorted_candidates, Rest_merges).

%extracts the workers
unmerge([], []).

unmerge([Sorted_merge|Rest_sorted_merges], [Candidate|Rest_candidates]):-
  Sorted_merge = _ - Candidate,
  unmerge(Rest_sorted_merges, Rest_candidates).

%returns an array of worker indices sorted based on their current workloads
available_workers_by_laziness(Total_works, Domain, Candidates):-
  available_works(Total_works, Domain, 1, Actual_works, Unsorted_candidates),
  merge_works_with_candidates(Actual_works, Unsorted_candidates, Merged),
  sort(Merged, Sorted_merge),
  unmerge(Sorted_merge, Candidates).


%tries workers in ascending order of their current workloads
%this is a heuristic that tries to keep the workload between the workers
%as balanced as possible.
%the two different versions ensure that no symmetric solutions are tried.
%i stole this method from the examples in the search/6 documentation.
%my previous implementation had a_k <= max{a_i} + 1 for all i < k.
%it worked, but i could not use first_fail with it (i had to assign the tasks
%in the order that they were given).
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
  make_cost_function(Cost_list, NP, Total_works, Least_cost, Cost),
  !,
  %Least_cost is explained in make_cost_function
  bb_min(search(Tasks, 0, first_fail, laziest_worker(Total_works, 0, _), complete, []), Cost, bb_options{strategy:continue, timeout:T, factor:F, from:Least_cost}),
  convert_to_ASA(Activity_list, Tasks, ASA),
  convert_ASA_to_ASP(ASA, Activity_list, Cost_list, 1, ASP, NP).
