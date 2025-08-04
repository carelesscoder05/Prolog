stochastic_hill_climbing(FinalState) :-
    initial_state(InitialState),
    heuristic(InitialState, InitialValue),
    write('Initial State: X = '), write(InitialState), write(' -> f(X) = '), writeln(InitialValue),
    shc_loop(InitialState, InitialValue, FinalState).

shc_loop(CurrentState, CurrentValue, FinalState) :-
    get_neighbors(CurrentState, AllNeighbors),
    find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves),
    UphillMoves \= [],
    !,
    random_member(NextState, UphillMoves),
    heuristic(NextState, NextValue),
    write('Climbing to:   X = '), write(NextState), write(' -> f(X) = '), writeln(NextValue),
    shc_loop(NextState, NextValue, FinalState).

shc_loop(CurrentState, CurrentValue, CurrentState) :-
    write('Local maximum found. Halting at f(X) = '), writeln(CurrentValue).

find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves) :-
    findall(Neighbor,
            (   member(Neighbor, AllNeighbors),
                heuristic(Neighbor, NeighborValue),
                NeighborValue > CurrentValue
            ),
            UphillMoves).

initial_state(X) :-
    random_between(-100, 100, X).

heuristic(X, Value) :-
    Value is -X*X + 10.

get_neighbors(X, [Neighbor1, Neighbor2]) :-
    Neighbor1 is X - 1,
    Neighbor2 is X + 1.
