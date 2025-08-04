value(X, V) :-
    V is -X*X + 10*X.


neighbor(X, X1) :-
    X1 is X + 1.
neighbor(X, X1) :-
    X1 is X - 1.


hill_climb(X, X) :-
    value(X, V),
    \+ (neighbor(X, N), value(N, VN), VN > V), !.

hill_climb(X, Best) :-
    value(X, V),
    neighbor(X, N),
    value(N, VN),
    VN > V,
    hill_climb(N, Best).
