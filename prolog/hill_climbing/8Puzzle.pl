:- use_module(library(lists)).

% --- GOAL STATE ---
goal([1,2,3,4,5,6,7,8,0]).

% --- HEURISTIC FUNCTION (Misplaced Tiles) ---
heuristic(State, H) :-
    goal(Goal),
    heuristic(State, Goal, H).

heuristic([], [], 0).
heuristic([0|T1], [_|T2], H) :-  % ignore blank
    heuristic(T1, T2, H).
heuristic([X|T1], [X|T2], H) :-
    heuristic(T1, T2, H).
heuristic([_|T1], [_|T2], H) :-
    heuristic(T1, T2, H1),
    H is H1 + 1.

% --- SWAP HELPER ---
swap(List, I, J, Swapped) :-
    nth0(I, List, ElemI),
    nth0(J, List, ElemJ),
    set_nth(List, I, ElemJ, Temp),
    set_nth(Temp, J, ElemI, Swapped).

set_nth([_|T], 0, X, [X|T]).
set_nth([H|T], I, X, [H|R]) :-
    I > 0, I1 is I - 1,
    set_nth(T, I1, X, R).

% --- VALID MOVES FOR BLANK (0) ---
move_index(Zero, Swap) :- Zero >= 3,    Swap is Zero - 3.      % Up
move_index(Zero, Swap) :- Zero =< 5,    Swap is Zero + 3.      % Down
move_index(Zero, Swap) :- Zero mod 3 =\= 0, Swap is Zero - 1.  % Left
move_index(Zero, Swap) :- Zero mod 3 =\= 2, Swap is Zero + 1.  % Right

move(State, Next) :-
    nth0(ZeroIndex, State, 0),
    move_index(ZeroIndex, SwapIndex),
    swap(State, ZeroIndex, SwapIndex, Next).

% --- GENERATE NEIGHBORS ---
neighbors(State, Neighbors) :-
    findall(N, move(State, N), Neighbors).

% --- BEST NEIGHBOR (Strictly Better) ---
best_neighbor(State, Best) :-
    neighbors(State, Neighbors),
    findall((H, N), (member(N, Neighbors), heuristic(N, H)), HNList),
    sort(HNList, [(BestH, Best)|_]),
    heuristic(State, CurrH),
    BestH < CurrH.  % Strict improvement only

% --- HILL CLIMBING WITH PATH TRACE ---
hill_climb_path(State, Path) :-
    hill_climb_path(State, [], [], RevPath),
    reverse(RevPath, Path).

hill_climb_path(State, _, PathSoFar, [State|PathSoFar]) :-
    goal(State), !.

hill_climb_path(State, Visited, PathSoFar, Path) :-
    best_neighbor(State, Next),
    \+ member(Next, Visited),  % avoid cycles
    hill_climb_path(Next, [State|Visited], [State|PathSoFar], Path).

% --- MAIN SOLVER ---
solve_8_puzzle(Start, Path) :-
    hill_climb_path(Start, Path).


% ?- solve_8_puzzle([1,2,3,4,5,6,0,7,8], Path), maplist(writeln, Path).