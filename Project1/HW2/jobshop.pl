jobshop(Schedule) :-
  deadline(Deadline),
  getMachines(Deadline, AvailMachs),
  findall(Tasks, job(_, Tasks), TaskList),
  timetable(TaskList, AvailMachs),   %Choose The correct Machine for the Tasks
  schedule(AvailMachs, Schedule).

jobshop_with_manpower(Schedule) :-
  deadline(Deadline),
  getMachines(Deadline, AvailMachs),
  findall(Tasks, job(_, Tasks), TaskList),
  timetable_with_manpower(TaskList, AvailMachs),  %Choose The correct Machine for the Tasks
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
%%%%%%%%
findEndOfTask(Task, Machine, [m(M, Avails)|RestM], End) :-
  Machine == M,
  findStart(Task, Avails, 0, Start),
  task(Task, _, Dur),
  End is Start + Dur
;
  findEndOfTask(Task, Machine, RestM, End).
%%%%%
timetable_with_manpower(TaskList, AvailMachs) :-
  staff(Staff),
  deadline(Deadline),
  build(Staff, Deadline, StaffList),
  machineSchedAll_with_manpower(TaskList, AvailMachs, StaffList).

build(_,0,[]).
build(X,N1,[X|L]) :-
  N1 > 0,
  N is N1 - 1,
  build(X,N,L).

machineSchedAll_with_manpower([], _, _).
machineSchedAll_with_manpower([TL|Rest], AvailMachs, StaffList) :-   %For every TaskList (Job)
  machineSched_with_manpower(TL, AvailMachs, 0, StaffList, NewStaffList),  %Initial Time Limitation is 0 for every Job
  machineSchedAll_with_manpower(Rest, AvailMachs, NewStaffList).

machineSched_with_manpower([], _, _, NewStaffList, NewStaffList).
machineSched_with_manpower([Task|RestT], AvailMachs, TimeLimitation, StaffList, TempStaffList) :-  %For every Task
  task(Task, Machine, _),
  deadline(D),
  length(NewStaffList, D),
  insertInMachine(Task, Machine, AvailMachs, TimeLimitation),
  staffConstraint(Task, StaffList, Machine, AvailMachs, NewStaffList),
  findEndOfTask_with_manpower(Task, Machine, AvailMachs, End, _),
  NewTimeLimitation = End,       %Update the Time Limitation
  machineSched_with_manpower(RestT, AvailMachs, NewTimeLimitation, NewStaffList, TempStaffList).

staffConstraint(Task, StaffList, Machine, AvailMachs, NewStaffList) :-
  task(Task, _, _, WanS),
  findEndOfTask_with_manpower(Task, Machine, AvailMachs, Finish, Start),
  fixStaffList(StaffList, Start, Finish, 0, FixedStaffList),
  findall(Staff, (member(Staff, FixedStaffList), integer(Staff)), SplittedStaffList),
  checkIfAvailStaff(SplittedStaffList, WanS, NewSplittedStaffList),
  my_replace(StaffList, NewSplittedStaffList, 0, Start, Finish, NewStaffList)
;
  false.
%%%%%%%
fixStaffList([], _, _, _, []).
fixStaffList([H|T], S, F, Counter, [NH|NT]) :-
  Counter < F,
  S =< Counter,!,
  NH = H,
  NewCounter is Counter + 1,
  fixStaffList(T, S, F, NewCounter, NT)
;
  NewCounter is Counter + 1,
  fixStaffList(T, S, F, NewCounter, NT).

checkIfAvailStaff([], _, []).
checkIfAvailStaff([H|T], WanS, [NH|NT]) :-
  NH is H - WanS,
  NH >= 0,
  checkIfAvailStaff(T, WanS, NT)
;
  false.

findEndOfTask_with_manpower(Task, Machine, [m(M, Avails)|RestM], End, Start) :-
  Machine == M,
  findStart(Task, Avails, 0, Start),
  task(Task, _, Dur),
  End is Start + Dur
;
  findEndOfTask_with_manpower(Task, Machine, RestM, End, Start).

createNewAvails(Limit, NewAvails) :-
  deadline(Length),
  NewLength is Length - Limit,
  length(NewAvails, NewLength).

my_replace(Rest, [], _, _, _, Rest).
my_replace([_|T1], [NH|T2], Counter, S, F, [NH|PT]) :-
  Counter >= S,
  Counter < F,
  % PH = NH, !,
  NewCounter is Counter + 1,
  my_replace(T1, T2, NewCounter, S, F, PT).
my_replace([OrH|T1], [NH|T2], Counter, S, F, [OrH|PT]) :-
  Counter < S,
  %PH = OrH,
  NewCounter is Counter + 1,
  my_replace(T1, [NH|T2], NewCounter, S, F, PT).
my_replace([OrH|T1], [NH|T2], Counter, S, F, [OrH|PT]) :-
  Counter >= F,
  %PH = OrH,
  NewCounter is Counter + 1,
  my_replace(T1, [NH|T2], NewCounter, S, F, PT).
%%%%%%
insertInMachine(Task, Machine, [m(M, Avails)|RestM], TimeLimitation) :-
  Machine == M,
  task(Task, _, Dur),
  expand(Task, Dur, TL),
  createNewAvails(TimeLimitation, NewAvails),   %Create a List of (Length(Avails) - TimeLimitation) for the new Task to be inserted
  sublist(TL, NewAvails),    %Insert The Task
  my_merge(Avails, NewAvails, TimeLimitation, 0, Avails)    %Then merge the two lists (Initial Avails and the newly created NewAvails) into Avails
;
  insertInMachine(Task, Machine, RestM, TimeLimitation).
%%%%
my_merge([], _, _, _, []).
my_merge(_, [], _, _, []).
my_merge([H|T], [NH|NT], Limit, Counter, [NNH|NNT]) :-
  Limit > 0,
  Counter < Limit,
  NNH = H, !,
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
