:- set_flag(print_depth,1000).
:- lib(ic).
:- lib(branch_and_bound).

job(j1, [t11,t12,t13,t14]).
job(j2, [t21,t22,t23,t24]).
job(j3, [t31,t32,t33]).
job(j4, [t41,t42,t43]).
job(j5, [t51]).
job(j6, [t61]).
job(j7, [t71]).

task(t11, m1, 3, 3).
task(t12, m2, 2, 3).
task(t13, m1, 2, 3).
task(t14, m2, 3, 1).
task(t21, m1, 2, 2).
task(t22, m2, 3, 2).
task(t23, m1, 3, 1).
task(t24, m1, 4, 2).
task(t31, m1, 3, 1).
task(t32, m2, 1, 3).
task(t33, m2, 4, 3).
task(t41, m1, 1, 1).
task(t42, m1, 3, 2).
task(t43, m1, 2, 3).
task(t51, m2, 3, 2).
machine(m1, 2).
machine(m2, 2).

jobshop_opt(Jobs, Staff, Schedule, Cost, Delta, Timeout) :-
  createTaskListOfJobs(Jobs, TaskListOfJobs),
  getMachineNum(MachineNum),
  findall(Machine, machine(Machine, _), AllTypesOfMachines),
  getUpperBound(UpperBound, TaskListOfJobs),
  %Variables
  findlen(AllTypesOfMachines, NumOfTypes),
  createMachineList(UpperBound, NumOfTypes, MachineListOfLists, 0),
  createStaffList(Staff, StaffList, UpperBound),
  createVariables(TaskListOfJobs, MachineNum, UpperBound, [], VariableList, AllTypesOfMachines),
  %Constraints
  stateConstraintsTaskSequence(TaskListOfJobs, VariableList, UpperBound),
  stateConstraintsStaff(StaffList, VariableList, TaskListOfJobs),
  stateConstraintsMachineAvail(MachineListOfLists, VariableList, TaskListOfJobs, AllTypesOfMachines),
  stateConstraintsMachineOverLap(VariableList, TaskListOfJobs, AllTypesOfMachines),
  %Cost
  flatten(VariableList, FlatVariableList),
  fixVariableList(FlatVariableList, Final),     %Final contains only Start and End times
  Cost #= max(Final),
  bb_min(search(FlatVariableList, 0, input_order, indomain, complete, []), Cost, bb_options{strategy:continue, delta:Delta ,timeout:Timeout, solutions:all}), !,
  Schedule = FlatVariableList.

fixVariableList([], []).
fixVariableList([_, S, F|Rest1], [S, F|Rest2]):-
  fixVariableList(Rest1, Rest2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Variables%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createTaskListOfJobs([], []).
createTaskListOfJobs([Job|Jobs], [FlatJobsTaskList|Rest]) :-
  findall(Tasks, job(Job, Tasks), JobsTaskList),
  flatten(JobsTaskList, FlatJobsTaskList),
  createTaskListOfJobs(Jobs, Rest).

getMachineNum(MachineNum) :-
  findall(N, machine(_, N), Ns),
  sumList2(Ns, 0, MachineNum).

getUpperBound(UpperBound, TaskListOfJobs) :-
  flatten(TaskListOfJobs, FlatTaskListOfJobs),
  sumList1(FlatTaskListOfJobs, TLs),
  sumList2(TLs, 0, UpperBound).

sumList1([], []).
sumList1([Task|Tasks], [Dur|Rest]) :-
  task(Task, _, Dur, _),
  sumList1(Tasks, Rest).

sumList2([], Num, Num).
sumList2([N|Ns], Sum1, Sum2) :-
  NewSum is Sum1 + N,
  sumList2(Ns, NewSum, Sum2).

createVariables([], _, _, VariableList, VariableList, _).
createVariables([TaskList|TaskLists], MachineNum, UpperBound, VariableList1, VariableList3, AllTypesOfMachines) :-
  createVariables2(TaskList, MachineNum, UpperBound, [], TempVariableList, AllTypesOfMachines),
  append(VariableList1, [TempVariableList], VariableList2),
  createVariables(TaskLists, MachineNum, UpperBound, VariableList2, VariableList3, AllTypesOfMachines).

%Creates a list of variables for each Task. For each one 3 variables are added to the list.
%Indicating the Task's Machine, its Start and End.
createVariables2([], _, _, VariableList, VariableList, _).
createVariables2([Task|Tasks], MachineNum, UpperBound, VariableList1, VariableList3, AllTypesOfMachines) :-
  S #:: 0..UpperBound,
  E #:: 0..UpperBound,
  task(Task, TasksMachine, _, _),
  indexOf(AllTypesOfMachines, TasksMachine, Type),    %Find the type of the appropriate machine
  machine(TasksMachine, CopiesOfMachine),
  UB is CopiesOfMachine * Type,
  LB is UB - (CopiesOfMachine - 1),
  M #:: LB..UB,
  append(VariableList1, [M, S, E], VariableList2),
  createVariables2(Tasks, MachineNum, UpperBound, VariableList2, VariableList3, AllTypesOfMachines).

createStaffList(Staff, StaffList, UpperBound) :-
  length(StaffList, UpperBound),
  StaffList #:: 0..Staff.

createMachineListOfLists(_, NumOfTypes, [], NumOfTypes).
createMachineListOfLists(UB, NumOfTypes, [List|Rest], Counter) :-
  length(List, UB),
  List #:: 0..NumOfTypes,
  NewCounter is Counter + 1,
  createMachineListOfLists(UB, NumOfTypes, Rest, NewCounter).

findlen([],X):-
  X=0.
findlen([_|Tail],Count):-
  findlen(Tail,Prev),
  Count is Prev + 1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Variables%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Constraints%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Task Sequence Constraint
stateConstraintsTaskSequence([], [], _).
stateConstraintsTaskSequence([TaskList|TaskLists], [VariableList|VariableLists], UpperBound) :-
  Last #:: 0..UpperBound,
  taskSequence(TaskList, VariableList, Last),
  stateConstraintsTaskSequence(TaskLists, VariableLists, UpperBound).

taskSequence([], [], _).
taskSequence([Task|Tasks], [_, Start, End|RestVars], Last) :-
  task(Task, _, Dur, _),
  Start #=< End,
  Start #>= Last,
  End #= Start + Dur,
  taskSequence(Tasks, RestVars, End).

%Staff Constraint
stateConstraintsStaff(StaffList, VariableList, TaskListOfJobs) :-
  flatten(VariableList, FlatVariableList),
  flatten(TaskListOfJobs, FlatTaskList),
  checkAvailStaff(StaffList, FlatVariableList, FlatTaskList).

checkAvailStaff(_, [], []).
checkAvailStaff(StaffList, [_, Start, End|RestVars], [Task|Tasks]) :-
  task(Task, _, _, TaskStaff),
  buildStaffList(StaffList, Start, End, TaskStaff, 0),
  checkAvailStaff(StaffList, RestVars, Tasks).

buildStaffList([], _, _, _, _).
buildStaffList([CurStaff|RestStaff], Start, End, TaskStaff, CurrentTime) :-
  StaffBit #= ((CurrentTime >= Start) and (CurrentTime < End)),
  get_min(CurStaff, CurrentMin),
  CurStaff #>= CurrentMin + TaskStaff * StaffBit,
  NewCurrentTime is CurrentTime + 1,
  buildStaffList(RestStaff, Start, End, TaskStaff, NewCurrentTime).

%Machine Availability Constraint
stateConstraintsMachineAvail(MachineListOfLists, VariableList, TaskList, AllTypesOfMachines) :-
  flatten(VariableList, FlatVariableList),
  flatten(TaskList, FlatTaskList),
  machineAvailability(MachineListOfLists, FlatVariableList, FlatTaskList, AllTypesOfMachines).

machineAvailability(_, [], [], _).
machineAvailability(MachineList, [_, Start, End|RestVars], [Task|Tasks], AllTypesOfMachines) :-
  task(Task, TasksMachine, _, _),
  indexOf(AllTypesOfMachines, TasksMachine, Type),    %Find the type of the appropriate machine
  indexOf(MachineList, SubMachineList, Type),         %Given its type take the appropriate list
  buildSubMachineList(SubMachineList, Start, End, 0),
  machineAvailability(MachineList, RestVars, Tasks, AllTypesOfMachines).

indexOf([Element|_], Element, 1):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1 + 1.

buildSubMachineList([], _, _, _).
buildSubMachineList([CurMach|RestMachs], Start, End, CurrentTime) :-
  get_min(CurMach, CurrentMin),
  MachineBit #= ((CurrentTime >= Start) and (CurrentTime < End)),   %Checks whether Current time is inside our wanted Time span of the Task.
  CurMach #>= CurrentMin + MachineBit,
  NewCurrentTime is CurrentTime + 1,
  buildSubMachineList(RestMachs, Start, End, NewCurrentTime).

%Machine Overlaps Constraint
stateConstraintsMachineOverLap(VariableList, TaskList, AllTypesOfMachines) :-
  flatten(VariableList, FlatVariableList),
  flatten(TaskList, FlatTaskList),
  checkForMachineOverLap1(FlatVariableList, FlatTaskList, AllTypesOfMachines).

checkForMachineOverLap1([], [], _).
checkForMachineOverLap1([Machine, Start, End|RestVars], [Task|Tasks], AllTypesOfMachines) :-
  task(Task, TasksMachine, _, _),
  indexOf(AllTypesOfMachines, TasksMachine, Type),    %Find the type of the appropriate machine
  checkForMachineOverLap2(RestVars, Tasks, Machine, Start, End, Type, AllTypesOfMachines),
  checkForMachineOverLap1(RestVars, Tasks, AllTypesOfMachines).

checkForMachineOverLap2([], [], _, _, _, _, _).
checkForMachineOverLap2([NextMachine, NextStart, NextEnd|RestVars], [NextTask|Tasks], PrevMachine, PrevStart, PrevEnd, PrevType, AllTypesOfMachines) :-
  task(NextTask, NextTasksMachine, _, _),
  indexOf(AllTypesOfMachines, NextTasksMachine, NextType),
  NextType == PrevType,
  StartInside #= ((PrevStart =< NextStart) and (NextStart < PrevEnd)),
  EndInside #= ((PrevStart < NextEnd) and (NextEnd =< PrevEnd)),
  AtLeastOneInside #= (StartInside or EndInside),
  MachinesDifferent #= (PrevMachine =\= NextMachine),
  AtLeastOneInside => MachinesDifferent,
  % ((max(PrevEnd, NextEnd) - min(PrevStart, NextStart)) < ((PrevEnd - PrevStart) + (NextEnd - NextStart))) => (PrevMachine #\= NextMachine),
  checkForMachineOverLap2(RestVars, Tasks, PrevMachine, PrevStart, PrevEnd, PrevType, AllTypesOfMachines).
checkForMachineOverLap2([_, _, _|RestVars], [NextTask|Tasks], PrevMachine, PrevStart, PrevEnd, PrevType, AllTypesOfMachines) :-
  task(NextTask, NextTasksMachine, _, _),
  indexOf(AllTypesOfMachines, NextTasksMachine, NextType),
  NextType \= PrevType,
  checkForMachineOverLap2(RestVars, Tasks, PrevMachine, PrevStart, PrevEnd, PrevType, AllTypesOfMachines).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Constraints%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
