%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Prolog Implementations of Minimax and Alpha-Beta Pruning
%
%   - Defines a sample game tree.
%   - Implements the classic Minimax algorithm.
%   - Implements the optimized Alpha-Beta Pruning algorithm.
%
%   To run:
%   ?- choose_move_minimax(a, Move).
%   ?- choose_move_alphabeta(a, Move).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%==============================================================================
%   Game Tree Definition
%==============================================================================
% A sample game tree is defined for demonstration. The structure assumes a
% root 'a' where the 'max' player makes a move.
%
%                 a (max)
%              /     \
%             b (min) c (min)
%            / \     / \
%           d   e   f   g (max)
%          / \ / \ / \ / \
%          h i j k l m n o  (terminal values)
% Values:  3 5 6 9 1 2 0 1

% Level 3: Terminal nodes with their static evaluation scores
% terminal(State, Value).
terminal(h, 3).
terminal(i, 5).
terminal(j, 6).
terminal(k, 9).
terminal(l, 1).
terminal(m, 2).
terminal(n, 0).
terminal(o, 1).

% Branching nodes (non-terminal)
% branch(State, [ListOfChildren]).
branch(d, [h, i]).
branch(e, [j, k]).
branch(f, [l, m]).
branch(g, [n, o]).
branch(b, [d, e]).
branch(c, [f, g]).
branch(a, [b, c]).


%==============================================================================
%   Minimax Implementation
%==============================================================================
% This algorithm explores the entire game tree to determine the optimal move.

% minimax(+Position, -Value, +Player)
% Base Case: If the position is terminal, its value is its static score.
minimax(Pos, Val, _) :-
    terminal(Pos, Val), !.

% Recursive Step (Maximizing Player): Finds the max value among children.
minimax(Pos, Val, max) :-
    branch(Pos, Children),
    findall(V, (member(Child, Children), minimax(Child, V, min)), Values),
    max_list(Values, Val), !.

% Recursive Step (Minimizing Player): Finds the min value among children.
minimax(Pos, Val, min) :-
    branch(Pos, Children),
    findall(V, (member(Child, Children), minimax(Child, V, max)), Values),
    min_list(Values, Val), !.

% --- Minimax Helpers ---

% max_list(+List, -Max)
max_list([H|T], Max) :- max_list(T, H, Max).
max_list([], Max, Max).
max_list([H|T], Acc, Max) :- H > Acc, max_list(T, H, Max).
max_list([H|T], Acc, Max) :- H =< Acc, max_list(T, Acc, Max).

% min_list(+List, -Min)
min_list([H|T], Min) :- min_list(T, H, Min).
min_list([], Min, Min).
min_list([H|T], Acc, Min) :- H < Acc, min_list(T, H, Min).
min_list([H|T], Acc, Min) :- H >= Acc, min_list(T, Acc, Min).

% --- Top-level predicate to choose a move using Minimax ---

% choose_move_minimax(+Position, -BestMove)
choose_move_minimax(Pos, BestMove) :-
    branch(Pos, Children),
    findall(Val-Move, (member(Move, Children), minimax(Move, Val, min)), Moves),
    sort(1, @>=, Moves, [BestValue-BestMove|_]),
    write('Minimax chooses move to '), write(BestMove),
    write(' with value '), write(BestValue), nl.


%==============================================================================
%   Alpha-Beta Pruning Implementation
%==============================================================================
% This is an optimized version of Minimax that prunes branches of the search
% tree that cannot influence the final decision.

% alphabeta(+Position, +Alpha, +Beta, -Value, +Player)
% Base Case: Terminal node's value is its static score.
alphabeta(Pos, _, _, Val, _) :-
    terminal(Pos, Val), !.

% Recursive Step (Maximizing Player):
alphabeta(Pos, Alpha, Beta, Val, max) :-
    branch(Pos, Children),
    best_value_max(Children, Alpha, Beta, Val), !.

% Recursive Step (Minimizing Player):
alphabeta(Pos, Alpha, Beta, Val, min) :-
    branch(Pos, Children),
    best_value_min(Children, Alpha, Beta, Val), !.

% --- Alpha-Beta Helpers ---

% best_value_max(+Children, +Alpha, +Beta, -BestVal)
% Iterates through children for the 'max' player.
best_value_max([], Alpha, _, Alpha). % No children left, return final Alpha.
best_value_max([Child|Rest], Alpha, Beta, BestVal) :-
    alphabeta(Child, Alpha, Beta, Val, min),
    NewAlpha is max(Alpha, Val),
    ( NewAlpha >= Beta ->
        BestVal = NewAlpha      % Beta Cutoff: Prune remaining children.
    ;
        best_value_max(Rest, NewAlpha, Beta, BestVal) % Continue search.
    ).

% best_value_min(+Children, +Alpha, +Beta, -BestVal)
% Iterates through children for the 'min' player.
best_value_min([], _, Beta, Beta). % No children left, return final Beta.
best_value_min([Child|Rest], Alpha, Beta, BestVal) :-
    alphabeta(Child, Alpha, Beta, Val, max),
    NewBeta is min(Beta, Val),
    ( NewBeta =< Alpha ->
        BestVal = NewBeta       % Alpha Cutoff: Prune remaining children.
    ;
        best_value_min(Rest, Alpha, NewBeta, BestVal) % Continue search.
    ).

% --- Top-level predicate to choose a move using Alpha-Beta ---

% choose_move_alphabeta(+Position, -BestMove)
choose_move_alphabeta(Pos, BestMove) :-
    branch(Pos, Children),
    % Initial call with Alpha = -inf, Beta = +inf
    findall(Val-Move,
            (member(Move, Children), alphabeta(Move, -1000, 1000, Val, min)),
            Moves),
    sort(1, @>=, Moves, [BestValue-BestMove|_]),
    write('Alpha-Beta chooses move to '), write(BestMove),
    write(' with value '), write(BestValue), nl.