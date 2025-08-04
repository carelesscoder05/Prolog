stochastic_hill_climbing(FinalState) :-
    initial_state(InitialState),
    heuristic(InitialState, InitialValue),
    write('Initial State: '), writeln(InitialState),
    write('Initial Heuristic (Neg. Manhattan Distance): '), writeln(InitialValue),
    shc_loop(InitialState, InitialValue, FinalState).

shc_loop(CurrentState, CurrentValue, FinalState) :-
    get_neighbors(CurrentState, AllNeighbors),
    find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves),
    UphillMoves \= [],
    !,
    random_member(NextState, UphillMoves),
    heuristic(NextState, NextValue),
    write('Climbing to value: '), write(NextValue), nl,
    shc_loop(NextState, NextValue, FinalState).

shc_loop(CurrentState, 0, CurrentState) :-
    !,
    writeln('Success! Goal state found.'),
    write('Final State: '), writeln(CurrentState), nl.

shc_loop(CurrentState, CurrentValue, CurrentState) :-
    write('Stuck at local maximum. Heuristic value: '), writeln(CurrentValue),
    write('Final State: '), writeln(CurrentState), nl.

find_uphill_moves(AllNeighbors, CurrentValue, UphillMoves) :-
    findall(Neighbor,
            (   member(Neighbor, AllNeighbors),
                heuristic(Neighbor, NeighborValue),
                NeighborValue > CurrentValue
            ),
            UphillMoves).

initial_state(FinalShuffledState) :-
    GoalState = [1, 2, 3, 4, 5, 6, 7, 8, 0],
    shuffle(GoalState, 50, FinalShuffledState).

shuffle(State, 0, State).
shuffle(State, N, FinalState) :-
    N > 0,
    get_neighbors(State, Neighbors),
    random_member(NextState, Neighbors),
    N1 is N - 1,
    shuffle(NextState, N1, FinalState).

heuristic(State, Value) :-
    total_manhattan_distance(State, MD),
    Value is -MD.

get_neighbors(State, Neighbors) :-
    nth0(BlankIndex, State, 0),
    findall(Neighbor,
            (   valid_swap(BlankIndex, SwapIndex),
                swap(State, BlankIndex, SwapIndex, Neighbor)
            ),
            Neighbors).

total_manhattan_distance(Board, TotalDist) :-
    total_manhattan_distance(Board, 0, TotalDist).

total_manhattan_distance([], _, 0).
total_manhattan_distance([Tile|Rest], Index, TotalDist) :-
    ( Tile == 0 ->
        TileDist = 0
    ;
        goal_pos(Tile, GoalIndex),
        current_row(Index, CRow), current_col(Index, CCol),
        goal_row(GoalIndex, GRow), goal_col(GoalIndex, GCol),
        TileDist is abs(CRow - GRow) + abs(CCol - GCol)
    ),
    NextIndex is Index + 1,
    total_manhattan_distance(Rest, NextIndex, RestDist),
    TotalDist is TileDist + RestDist.

valid_swap(BlankIndex, SwapIndex) :- BlankIndex mod 3 > 0, SwapIndex is BlankIndex - 1.
valid_swap(BlankIndex, SwapIndex) :- BlankIndex mod 3 < 2, SwapIndex is BlankIndex + 1.
valid_swap(BlankIndex, SwapIndex) :- BlankIndex // 3 > 0, SwapIndex is BlankIndex - 3.
valid_swap(BlankIndex, SwapIndex) :- BlankIndex // 3 < 2, SwapIndex is BlankIndex + 3.

goal_pos(1, 0). goal_pos(2, 1). goal_pos(3, 2).
goal_pos(4, 3). goal_pos(5, 4). goal_pos(6, 5).
goal_pos(7, 6). goal_pos(8, 7). goal_pos(0, 8).

current_row(Index, Row) :- Row is Index // 3.
current_col(Index, Col) :- Col is Index mod 3.
goal_row(Index, Row) :- Row is Index // 3.
goal_col(Index, Col) :- Col is Index mod 3.

replace_nth0([_|T], 0, Elem, [Elem|T]).
replace_nth0([H|T], I, Elem, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace_nth0(T, I1, Elem, R).

swap(List, Index1, Index2, Swapped) :-
    nth0(Index1, List, Elem1),
    nth0(Index2, List, Elem2),
    replace_nth0(List, Index1, Elem2, TempList),
    replace_nth0(TempList, Index2, Elem1, Swapped).
