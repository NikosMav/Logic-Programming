:- lib(fd).

stable(Marriage) :-
  createVariables(MenVars, WomenVars),
  stateConstraints(MenVars, WomenVars),
  labeling(MenVars),
  men(ListOfMen),
  showResults(MenVars, ListOfMen, Marriage).

createVariables(MenVars, WomenVars) :-
  men(ListOfMen),
  women(ListOfWomen),
  findlen(ListOfMen, NumOfMen),
  length(MenVars, NumOfMen),
  MenVars #:: ListOfWomen,
  findlen(ListOfWomen, NumOfWomen),
  length(WomenVars, NumOfWomen),
  WomenVars #:: ListOfMen.

stateConstraints(MenVars, WomenVars) :-
  men(ListOfMen),
  consForMen(MenVars, ListOfMen, WomenVars), %Stating the #=> Constraint.
  finalCons(MenVars, ListOfMen, WomenVars).  %Stating the #<=> Constraint.

consForMen([], [], _).
consForMen([ManVar|MenVars], [Man|Men], WomenVars) :-
  prefers(Man, PrefList),                    %Getting the Man's preferred women.
  element(WomansPrefNum, PrefList, ManVar),  %Finding the Man's woman ranking according to his prefer list.
  women(ListOfWomen),
  consForWomen(WomansPrefNum, PrefList, Man, ManVar, WomenVars, ListOfWomen),  %For every Woman and for every WomanVar...
  consForMen(MenVars, Men, WomenVars).

consForWomen(_, _, _, _, [], []).
consForWomen(WomansPrefNum, PrefList, CurrentMan, CurrentManVar, [WomanVar|WomenVars], [Woman|Women]) :-
  element(OtherWomansPrefNum, PrefList, Woman), %Getting each Other Woman's ranking according to the current Man's prefer list.
  prefers(Woman, PrefList2),                    %Getting each Woman's prefer List.
  element(MansPrefNum, PrefList2, WomanVar),    %Getting each Woman's Man rank according to her prefer list.
  element(OtherMansPrefNum, PrefList2, CurrentMan),    %Getting for every other Woman the prefer rank of the current Man.
  (WomansPrefNum #> OtherWomansPrefNum) #=> (MansPrefNum #< OtherMansPrefNum),   %If current Man prefers the other Woman mor than his wife then that Woman must like her husband more than the current Man.
  consForWomen(WomansPrefNum, PrefList, CurrentMan, CurrentManVar, WomenVars, Women).

finalCons([], [], _).
finalCons([ManVar|MenVars], [Man|Men], WomenVars) :-
  women(ListOfWomen),
  finalCons2(ManVar, Man, WomenVars, ListOfWomen),
  finalCons(MenVars, Men, WomenVars).

finalCons2(_, _, [], []).
finalCons2(CurrentManVar, CurrentMan, [WomanVar|WomenVars], [Woman|Women]) :-
  (CurrentManVar #= Woman) #<=> (WomanVar #= CurrentMan),
  finalCons2(CurrentManVar, CurrentMan, WomenVars, Women).

showResults([], [], []).
showResults([ManVar|MenVars], [Man|Men], [Man-ManVar|RestPairs]) :-
  showResults(MenVars, Men, RestPairs).

findlen([],X):-
  X=0.
findlen([_|Tail],Count):-
  findlen(Tail,Prev),
  Count is Prev + 1.
