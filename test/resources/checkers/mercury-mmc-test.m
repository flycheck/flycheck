:- interface.

:- import_module int.
:- import_module array.

:- pred testme(int::in, int::out) is semidet.

:- func foldtest(array(int), int, int) = int.

:- implementation.

testme(N, X) :- N < X.

foldtest(Arr, X, Y) = X + Y + array.foldl((func(X, Y) = X + Y), Arr, 0).
