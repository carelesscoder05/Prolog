:- use_module(library(random)).

% Count number of attacking pairs
attacking_pairs([], 0).
attacking_pairs([Q|Others], Count) :-
    attacking_pairs(Others, Q, 1, C1),
    attacking_pairs(Others, C2),
    Count is C1 + C2.

attacking_pairs([], _, _, 0).
attacking_pairs([Q1|Others], Q, D, Count) :-
    (Q =:= Q1 ; abs(Q - Q1) =:= D) ->
        (attacking_pairs(Others, Q, D+1, C), Count is C + 1)
    ;
        attacking_pairs(Others, Q, D+1, Count).

% Move a queen to a new row in a given column
move_queen(State, Col, NewRow, NewState) :-
    nth1(Col, State, _, Rest),
    nth1(Col, NewState, NewRow, Rest).

% Generate neighbors by moving each queen in its column
neighbors(State, Neighbors) :-
    findall(Neighbor,
        (between(1, 8, Col),
         between(1, 8, Row),
         nth1(Col, State, CurrentRow),
         Row \= CurrentRow,
         move_queen(State, Col, Row, Neighbor)),
        Neighbors).

% Find best neighbor (min attacking pairs)
best_neighbor(Current, Best) :-
    neighbors(Current, Neighbors),
    findall((H, N), (member(N, Neighbors), attacking_pairs(N, H)), HNList),
    sort(HNList, [(BestH, Best)|_]),
    attacking_pairs(Current, CurrH),
    BestH < CurrH.

% Hill climbing with restarts
hill_climb(State, State) :-
    attacking_pairs(State, 0), !.

hill_climb(State, Solution) :-
    best_neighbor(State, Next),
    hill_climb(Next, Solution).

% Random state: 1 queen per column, random row
random_state(State) :-
    findall(Row, (between(1, 8, _), random_between(1, 8, Row)), State).

% Final solve predicate with restart limit
solve_8_queens(Solution) :-
    solve_with_restarts(100, Solution).

solve_with_restarts(0, _) :- !, fail.
solve_with_restarts(N, Solution) :-
    random_state(Start),
    (hill_climb(Start, Solution) ->
        true
    ;
        N1 is N - 1,
        solve_with_restarts(N1, Solution)).
