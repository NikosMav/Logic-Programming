job(j1,[t11,t12]).
job(j2,[t21,t22,t23]).
job(j3,[t31]).
job(j4,[t41,t42]).
task(t11,m1,2).
task(t12,m2,6).
task(t21,m2,5).
task(t22,m1,3).
task(t23,m2,3).
task(t31,m2,4).
task(t41,m1,5).
task(t42,m2,2).
machine(m1,1).
machine(m2,2).
deadline(14).

jobshop(Schedule) :-
  deadline(Deadline),
  getMachines(Deadline, AvailMachs),
  findall(Tasks, job(_, Tasks), TaskList),
  timetable(TaskList, AvailMachs),   %Choose The correct Machine for the Tasks
  %writeln(AvailMachs),
  schedule(AvailMachs, Schedule).

%%%%%%%%%%%%%%%%%%%% Machines %%%%%%%%%%%%%%%%%%%%%%
getMachines(Deadline, AvailMachs) :-
  findall(m(M, N), machine(M, N), L),
  expand(L, Deadline, [], AvailMachs).

expand([], _, AvailMachs, AvailMachs).
expand([m(M, N)|Rest], Deadline, AvailMachs1, AvailMachs4) :-
  expand_one(M, N, Deadline, AvailMachs2),
  append(AvailMachs1, AvailMachs2, AvailMachs3),
  expand(Rest, Deadline, AvailMachs3, AvailMachs4).

expand_one(_, 0, _, []).
expand_one(M, N, Deadline, [m(M, Avails)|AvailMachs]) :-
  N > 0,
  N1 is N-1,
  length(Avails, Deadline),
  expand_one(M, N1, Deadline, AvailMachs).

%%%%%%%%%%%%%%%%%%%% Timetable %%%%%%%%%%%%%%%%%%%%%%%
timetable(TaskList, AvailMachs) :-
  machineSchedAll(TaskList, AvailMachs).

machineSchedAll([], _).
machineSchedAll([TL|Rest], AvailMachs) :-   %For every TaskList (Job)
  machineSched(TL, AvailMachs, 0),   %Initial Time Limitation is 0 for every Job
  machineSchedAll(Rest, AvailMachs).

machineSched([], _, _).
machineSched([Task|RestT], AvailMachs, TimeLimitation) :-  %For every Task
  task(Task, Machine, _),
  insertInMachine(Task, Machine, AvailMachs, TimeLimitation),
  findEndOfTask(Task, Machine, AvailMachs, End),
  NewTimeLimitation = End,       %Update the Time Limitation
  machineSched(RestT, AvailMachs, NewTimeLimitation).

insertInMachine(Task, Machine, [m(M, Avails)|RestM], TimeLimitation) :-
  Machine == M,
  task(Task, _, Dur),
  expand(Task, Dur, TL),
  createNewAvails(TimeLimitation, NewAvails),   %Create a List of (Length(Avails) - TimeLimitation) for the new Task to be inserted
  sublist(TL, NewAvails),    %Insert The Task
  my_merge(Avails, NewAvails, TimeLimitation, 0, Avails)    %Then merge the two lists (Initial Avails and the newly created NewAvails) into Avails
;
  insertInMachine(Task, Machine, RestM, TimeLimitation).

findEndOfTask(Task, Machine, [m(M, Avails)|RestM], End) :-
  Machine == M,
  findStart(Task, Avails, 0, Start),
  task(Task, _, Dur),
  End is Start + Dur
;
  findEndOfTask(Task, Machine, RestM, End).

createNewAvails(Limit, NewAvails) :-
  deadline(Length),
  NewLength is Length - Limit,
  length(NewAvails, NewLength).

my_merge([], _, _, _, []).
my_merge(_, [], _, _, []).
my_merge([H|T], [NH|NT], Limit, Counter, [NNH|NNT]) :-
  Limit > 0,
  Counter < Limit,
  NNH = H,
  NewCounter is Counter + 1,
  my_merge(T, [NH|NT], Limit, NewCounter, NNT)
;
  NNH = NH,
  NewCounter is Counter + 1,
  my_merge([H|T], NT, Limit, NewCounter, NNT).

expand(_, 0, []).
expand(T, D, [T|Ts]) :-
  D > 0,
  D1 is D-1,
  expand(T, D1, Ts).

sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).
%%%%%%%%%%%%%%%%%%%%%% Schedule %%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule(AvailMachs, Schedule) :-
  findall(execs(M, N), machine(M, N), L),
  expandMachine(L, [], Schedule),
  schedule2(Schedule, AvailMachs).

expandMachine([], NewList, NewList).
expandMachine([execs(M, N)|Rest], NewList1, NewList4) :-
  expandOneMachine(M, N, NewList2),
  append(NewList1, NewList2, NewList3),
  expandMachine(Rest, NewList3, NewList4).

expandOneMachine(_, 0, []).
expandOneMachine(M, N, [execs(M, _)|RestExecs]) :-
  N > 0,
  N1 is N-1,
  expandOneMachine(M, N1, RestExecs).

schedule2([], []).
schedule2([execs(M, FixedNewTemp)|RestExecs], [m(M, Avails)|RestMach]) :-
  include(atom, Avails, FixedAvails),      %To keep only the tasks and exclude the "dont care" variables
  findall(t(T, D), (member(T, FixedAvails), task(T, M, D)), Temp),
  remove_duplicates(Temp, NewTemp), !,
  fixNewTemp(NewTemp, Avails, FixedNewTemp),   %In order to find the Start and Finish of every Task
  schedule2(RestExecs, RestMach).

%include predicate filters the elements of a list for which a goal succeeds (in our case the element has to be of type "atom")
include(_, [], []).
include(P, [H|T], L) :- ','(=..(P, A), ','(append(A, [H], B), ','(=..(F, B), ','(;(','(call(F), ','(=(L, [H|S]), !)), =(L, S)), include(P, T, S))))).

remove_duplicates([],[]).
remove_duplicates([H | T], List) :-
  member(H, T),
  remove_duplicates( T, List).
remove_duplicates([H | T], [H|T1]) :-
  \+member(H, T),
  remove_duplicates( T, T1).

fixNewTemp([], _, []).
fixNewTemp([t(T, D)|RestT1], Avails, [t(T, Start, End)|RestT2]) :-
  findStart(T, Avails, 0, Start),
  End is Start + D,
  fixNewTemp(RestT1, Avails, RestT2).

findStart(V, [H|T], Counter, I) :-
  atom(H), V == H, Counter = I, !
;
  NewCounter is Counter + 1,
  findStart(V, T, NewCounter, I).
