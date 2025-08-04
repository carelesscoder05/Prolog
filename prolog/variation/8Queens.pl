stochastic_hill_climbing(FinalState) :-
    initial_state(InitialState),
    heuristic(InitialState, InitialValue),
    write('Initial State: '), write(InitialState), nl,
    write('Initial Heuristic (non-attacking pairs): '), writeln(InitialValue),
    shc_loop(InitialState, InitialValue, FinalState).

shc_loop(CurrentState, CurrentValue, FinalState) :-
    get_neighbors(CurrentState, AllNeighbors),
    find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves),
    UphillMoves \= [],
    !,
    random_member(NextState, UphillMoves),
    heuristic(NextState, NextValue),
    write('Climbing to value: '), write(NextValue), write(' with state '), write(NextState), nl,
    shc_loop(NextState, NextValue, FinalState).

shc_loop(CurrentState, 28, CurrentState) :-
    !,
    writeln('Success! Global maximum found.'), nl.

shc_loop(CurrentState, CurrentValue, CurrentState) :-
    write('Stuck at local maximum. Heuristic value: '), writeln(CurrentValue), nl.

find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves) :-
    findall(Neighbor,
            (   member(Neighbor, AllNeighbors),
                heuristic(Neighbor, NeighborValue),
                NeighborValue > CurrentValue
            ),
            UphillMoves).

initial_state(State) :-
    findall(Row, (between(1, 8, _), random_between(1, 8, Row)), State).

heuristic(State, Value) :-
    count_attacking_pairs(State, AttackingCount),
    Value is 28 - AttackingCount.

get_neighbors(State, Neighbors) :-
    findall(Neighbor, is_neighbor(State, Neighbor), Neighbors).

is_neighbor(State, Neighbor) :-
    between(1, 8, ColumnIndex),
    nth1(ColumnIndex, State, CurrentRow),
    between(1, 8, NewRow),
    CurrentRow \= NewRow,
    replace_nth(State, ColumnIndex, NewRow, Neighbor).

count_attacking_pairs(State, Count) :-
    findall([C1, C2],
            (   between(1, 7, C1),
                C2 is C1 + 1,
                between(C2, 8, C2),
                nth1(C1, State, R1),
                nth1(C2, State, R2),
                q_attacks(C1, R1, C2, R2)
            ),
            Pairs),
    length(Pairs, Count).

q_attacks(_, R1, _, R2) :- R1 == R2, !.
q_attacks(C1, R1, C2, R2) :- abs(C1 - C2) =:= abs(R1 - R2).

replace_nth([_|T], 1, X, [X|T]).
replace_nth([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace_nth(T, I1, X, R).
