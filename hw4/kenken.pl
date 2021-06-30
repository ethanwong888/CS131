% KenKen Solver - GNU Prolog Finite Domain Solver

% Helper functions for KenKen

% createColumns - (helper function for createMatrix)
createColumns([], _).
createColumns([H|T], Num) :-
  length(H, Num),
  createColumns(T, Num).

% createMatrix - make the 2D matrix 
createMatrix(L, W) :-
  length(L, W),
  createColumns(L, W).

% accessBlock - access the number stored at board[i][j]
accessBlock(B, [I|J], Num) :- 
  nth(I, B, R),
  nth(J, R, Num).

% setDomain - set the domain of numbers that can be allowed for row or column
setDomain(F, D) :- 
  fd_domain(D, 1, F).

% transposeOneColumn - (helper function for transposeMatrix)
transposeOneColumn([], [], []).
transposeOneColumn([[First|Second] | Tail], [First|Column], [Second|Row]) :- 
  transposeOneColumn(Tail, Column, Row).

% transposeMatrix - transpose the matrix by turning each column into a row, one at a time
transposeMatrix([], []).
transposeMatrix([[X|Y] | YTail], [[X|Z] | ZTail]) :- 
  transposeOneColumn(YTail, Z, Rem), 
  transposeMatrix(Rem, ZRem), 
  transposeOneColumn(ZTail, Y, ZRem).

% Subtraction constraint - need to check subtraction both ways (helper function for constraints)
subtract(B, X, Y, Difference) :-
  accessBlock(B, X, First), 
  accessBlock(B, Y, Second),
  (Difference #= First - Second; 
    Difference #= Second - First).

% Addition constraint - (helper function for constraints)
add( _, [], 0).
add(B, [H|T], Ans) :-
  accessBlock(B, H, Temp),
  add(B, T, Sum),
  Ans #= Temp + Sum.

% Quotient constraint- need to check division both ways (helper function for constraints)
division(B, X, Y, Quotient) :-
  accessBlock(B, X, First), 
  accessBlock(B, Y, Second),
  (Quotient #= First / Second; 
    Quotient #= Second / First).

% Multiplication constraint - (helper function for constraints)
multiply( _, [], 1).
multiply(B, [H|T], Ans) :-
  accessBlock(B, H, Temp),
  multiply(B, T, Product),
  Ans #= Temp * Product.

% constraints - (helper function for setConstraints)
constraints(B, -(Ans, X, Y)) :-
  subtract(B, X, Y, Ans).
constraints(B, +(Ans, V)) :-
  add(B, V, Ans).
constraints(B, /(Ans, X, Y)) :-
  division(B, X, Y, Ans).
constraints(B, *(Ans, V)) :-
  multiply(B, V, Ans).

% setConstraints - make sure the provided constraints are feasible, apply them
setConstraints(X, Y) :- 
  maplist(constraints(X), Y).

% distinctBoard - make sure that each row and column has no repeating numbers from the domain
distinctBoard([]).
distinctBoard([H|T]) :-
  fd_all_different(H),
  distinctBoard(T).

% kenken - creates board and solves accordingly
kenken(N, C, T) :-
  createMatrix(T, N),
  distinctBoard(T),
  transposeMatrix(T, TransposeBoard),
  distinctBoard(TransposeBoard),
  maplist(setDomain(N), T),
  setConstraints(T, C),
  maplist(fd_labeling, T).




% KenKen Solver - NO GNU Prolog Finite Domain Solver

% Helper functions for Plain KenKen

% need to include 'H > 0' to prevent accidentally going into negative values (TA Slides)
translate(0, []).
translate(H, [H|T]) :- 
  H > 0, 
  Temp is H - 1, 
  translate(Temp, T).

permute(L, Num) :-
  translate(Num, Temp), 
  permutation(Temp, L).

% need to include 'X > 0' to prevent accidentally going into negative values (TA Slides)
compute([], 0, _).
compute([H|T], X, Num) :- 
  X > 0, 
  Temp is X - 1, 
  permute(H, Num),
  compute(T, Temp, Num).

plain_kenken(N, C, T) :- 
  compute(T, N, N), 
  transposeMatrix(T, TransposeBoard), 
  compute(TransposeBoard, N, N), 
  setConstraints(T, C).



% test case from the spec
kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).