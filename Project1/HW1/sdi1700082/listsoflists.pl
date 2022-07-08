%First Query
:- set_flag(print_depth,1000).

cart_prod([], [[]]). %Base Case for the initial List
cart_prod([Fset|Rest], P) :- cart_prod_set(Fset, List, P, []), cart_prod(Rest, List).

cart_prod_set([], _, P, P). %Base Case for every Set
cart_prod_set([Felement|Rest], List, P0, P) :- cart_prod_element(List, Felement, P0, P1), cart_prod_set(Rest, List, P1, P).

cart_prod_element([], _, P, P). %Base Case for every Element
cart_prod_element([X|Xs], Element, [[Element|X]|P1], P) :- cart_prod_element(Xs, Element, P1, P).

%Second Query
matr_transp([Frow|Rest], Transp):- transpose2([Frow|Rest], Transp).

transpose2([[]|_], []). %Base Case, traversed whole matrix
%getting the heads of all rows creating a new row
transpose2([Frow|Rest], [TrRow|RestRows]):- getNextRow([Frow|Rest], Matr, TrRow), transpose2(Matr, RestRows).

getNextRow([], [], []).
getNextRow([Frow|RestRows1], [RestFrow|RestRows2], [Element|Rest]):-
  append([Element], RestFrow, Frow),
  getNextRow(RestRows1, RestRows2, Rest).

%Third Query
matr_mult([],[],[]).
matr_mult(Matr1,Matr2,Res):- matr_transp(Matr2,Tran), matr_mult1(Matr1,Tran,Res).

matr_mult1([],_,[]). %For every row of Matrix1
matr_mult1([Flist1|Rest1],Matr2,[FlistRes|RestRes]):- matr_mult2(Flist1,Matr2,FlistRes), matr_mult1(Rest1,Matr2,RestRes).

matr_mult2(_,[],[]). %For every row of the Transposed Matrix2
matr_mult2(List1,[Flist2|Rest2],[FlistRes|RestRes]):- product(List1,Flist2,FlistRes), matr_mult2(List1,Rest2,RestRes).

%Mutliplying rows and creating each time the elements of the Newly Produced Matrix
product([],[],0).  %When the lists become empty return and add with 0, indicating the end of the sum
product([Elm1|RestElm1],[Elm2|RestElm2],P):- product(RestElm1,RestElm2,NextP), P is Elm1*Elm2 + NextP.

%Fourth Query
matr_det([[A,B],[C,D]],Det):-!,
    Det is A*D-B*C. %Determinant of 2x2 matrix

matr_det([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],Det):-
    matr_det([[A22,A23],[A32,A33]],D1),
    matr_det([[A21,A23],[A31,A33]],D2),
    matr_det([[A21,A22],[A31,A32]],D3),
    Det is D1*A11 - D2*A12 + D3*A13. %Determinant of 3x3 matrix

matr_det([[A11,A12,A13,A14],[A21,A22,A23,A24],[A31,A32,A33,A34],[A41,A42,A43,A44]],Det):-
    matr_det([[A22,A23,A24],[A32,A33,A34],[A42,A43,A44]],D1),
    matr_det([[A12,A13,A14],[A32,A33,A34],[A42,A43,A44]],D2),
    matr_det([[A12,A13,A14],[A22,A23,A24],[A42,A43,A44]],D3),
    matr_det([[A12,A13,A14],[A22,A23,A24],[A32,A33,A34]],D4),
    Det is D1*A11 - D2*A21 + D3*A31 - D4*A41. %Determinant of 4x4 matrix
