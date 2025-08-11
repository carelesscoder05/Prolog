%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Tic-Tac-Toe AI in Prolog (with Minimax and Alpha-Beta)
%
%   - 'x' is the maximizing player (value = +1 for a win).
%   - 'o' is the minimizing player (value = -1 for a win).
%   - 'e' represents an empty cell.
%   - A board is a flat list of 9 atoms, e.g., [e,e,e,e,e,e,e,e,e].
%
%   HOW TO USE:
%   For a fast and efficient move, always use the Alpha-Beta predicate:
%   ?- find_best_move_ab([e,e,e,e,x,e,e,e,e], o, Move).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%==============================================================================
%   1. Game Rules and Logic
%==============================================================================

% win(+Board, +Player)
% Checks if a player has won by checking all 8 winning lines.
win(Board, Player) :- member([I1,I2,I3], [[1,2,3],[4,5,6],[7,8,9], % Rows
                                           [1,4,7],[2,5,8],[3,6,9], % Cols
                                           [1,5,9],[3,5,7]]),      % Diagonals
                       nth1(I1, Board, Player),
                       nth1(I2, Board, Player),
                       nth1(I3, Board, Player).

% full(+Board)
% Checks if the board is full (no 'e'mpty cells).
full(Board) :- \+ member(e, Board).

% game_over(+Board, -Winner)
% Succeeds if the game has a result. Winner is 'x', 'o', or 'draw'.
game_over(Board, x)    :- win(Board, x), !.
game_over(Board, o)    :- win(Board, o), !.
game_over(Board, draw) :- full(Board), !.

% move(+Board, +Index, +Player, -NewBoard)
% Places Players mark at Index (1-9) to produce NewBoard.
move(Board, Index, Player, NewBoard) :-
    nth1(Index, Board, e),
    nth1(Index, NewBoard, Player, Board).

% get_opponent(+Player, -Opponent)
get_opponent(x, o).
get_opponent(o, x).


%==============================================================================
%   2. Minimax Algorithm (Slow, for demonstration)
%==============================================================================
% NOTE: This version is too slow for an empty board due to combinatorial explosion.

% minimax(+Board, +Player, -Value)
% Determines the minimax value of a board state for the player whose turn it is.
minimax(Board, _, 1)  :- game_over(Board, x), !.
minimax(Board, _, -1) :- game_over(Board, o), !.
minimax(Board, _, 0)  :- game_over(Board, draw), !.
minimax(Board, Player, Value) :-
    findall(MoveValue,
            (   between(1, 9, Index),
                move(Board, Index, Player, NewBoard),
                get_opponent(Player, Opponent),
                minimax(NewBoard, Opponent, MoveValue)
            ),
            MoveValues),
    best_value(Player, MoveValues, Value).

% best_value(+Player, +ValueList, -BestValue)
best_value(x, MoveValues, BestValue) :- max_list(MoveValues, BestValue).
best_value(o, MoveValues, BestValue) :- min_list(MoveValues, BestValue).


%==============================================================================
%   3. Alpha-Beta Pruning Algorithm (Efficient)
%==============================================================================

% alphabeta(+Board, +Player, +Alpha, +Beta, -Value)
% Base cases: If game is over, return the static value.
alphabeta(Board, _, _, _, 1)  :- game_over(Board, x), !.
alphabeta(Board, _, _, _, -1) :- game_over(Board, o), !.
alphabeta(Board, _, _, _, 0)  :- game_over(Board, draw), !.
% Recursive cases for 'x' (max) and 'o' (min)
alphabeta(Board, x, Alpha, Beta, Value) :- !, alphabeta_max(Board, x, Alpha, Beta, Value).
alphabeta(Board, o, Alpha, Beta, Value) :- !, alphabeta_min(Board, o, Alpha, Beta, Value).

% alphabeta_max(+Board, +Player, +Alpha, +Beta, -Value)
alphabeta_max(Board, Player, Alpha, Beta, Value) :-
    findall(Index, between(1, 9, Index), Indices),
    alphabeta_max_loop(Indices, Board, Player, Alpha, Beta, Alpha, Value).

alphabeta_max_loop([], _, _, _, _, CurrentBest, CurrentBest).
alphabeta_max_loop([Index|Rest], Board, Player, Alpha, Beta, CurrentBest, Value) :-
    (   \+ move(Board, Index, Player, _) -> % If move is invalid, skip
        alphabeta_max_loop(Rest, Board, Player, Alpha, Beta, CurrentBest, Value)
    ;   move(Board, Index, Player, NewBoard),
        get_opponent(Player, Opponent),
        alphabeta(NewBoard, Opponent, Alpha, Beta, ChildValue),
        NewBest is max(CurrentBest, ChildValue),
        (   NewBest >= Beta -> Value = NewBest % Prune
        ;   NewAlpha is max(Alpha, NewBest),
            alphabeta_max_loop(Rest, Board, Player, NewAlpha, Beta, NewBest, Value)
        )
    ).

% alphabeta_min(+Board, +Player, +Alpha, +Beta, -Value)
alphabeta_min(Board, Player, Alpha, Beta, Value) :-
    findall(Index, between(1, 9, Index), Indices),
    alphabeta_min_loop(Indices, Board, Player, Alpha, Beta, Beta, Value).

alphabeta_min_loop([], _, _, _, _, CurrentBest, CurrentBest).
alphabeta_min_loop([Index|Rest], Board, Player, Alpha, Beta, CurrentBest, Value) :-
    (   \+ move(Board, Index, Player, _) -> % If move is invalid, skip
        alphabeta_min_loop(Rest, Board, Player, Alpha, Beta, CurrentBest, Value)
    ;   move(Board, Index, Player, NewBoard),
        get_opponent(Player, Opponent),
        alphabeta(NewBoard, Opponent, Alpha, Beta, ChildValue),
        NewBest is min(CurrentBest, ChildValue),
        (   NewBest =< Alpha -> Value = NewBest % Prune
        ;   NewBeta is min(Beta, NewBest),
            alphabeta_min_loop(Rest, Board, Player, Alpha, NewBeta, NewBest, Value)
        )
    ).


%==============================================================================
%   4. Top-Level Predicates (User Interface)
%==============================================================================

% The slow way (will hang on empty board)
find_best_move(Board, Player, BestMove) :-
    findall(Value-Index,
            (   between(1, 9, Index),
                move(Board, Index, Player, NewBoard),
                get_opponent(Player, Opponent),
                minimax(NewBoard, Opponent, Value)
            ),
            Moves),
    select_best(Player, Moves, _-BestMove),
    print_board(Board),
    format('~nMinimax AI (~w) chooses move: ~w~n', [Player, BestMove]).

% The fast and recommended way
find_best_move_ab(Board, Player, BestMove) :-
    findall(Value-Index,
            (   between(1, 9, Index),
                move(Board, Index, Player, NewBoard),
                get_opponent(Player, Opponent),
                alphabeta(NewBoard, Opponent, -1000, 1000, Value) % Initial alpha, beta
            ),
            Moves),
    select_best(Player, Moves, _-BestMove),
    print_board(Board),
    format('~nAlpha-Beta AI (~w) chooses move: ~w~n', [Player, BestMove]).

% select_best(+Player, +MovesList, -BestMovePair)
% Sorts the moves to find the best one.
select_best(x, Moves, BestMove) :- sort(1, @>=, Moves, [BestMove|_]).
select_best(o, Moves, BestMove) :- sort(1, @=<, Moves, [BestMove|_]).


%==============================================================================
%   5. Helper and Utility Predicates
%==============================================================================

% print_board(+Board)
% Displays the board in a user-friendly 3x3 format.
print_board([C1,C2,C3,C4,C5,C6,C7,C8,C9]) :-
    nl,
    maplist(char_print, [C1,C2,C3,C4,C5,C6,C7,C8,C9], Chars),
    [P1,P2,P3,P4,P5,P6,P7,P8,P9] = Chars,
    format(' ~w | ~w | ~w ~n', [P1, P2, P3]),
    format('---|---|---~n'),
    format(' ~w | ~w | ~w ~n', [P4, P5, P6]),
    format('---|---|---~n'),
    format(' ~w | ~w | ~w ~n', [P7, P8, P9]).

char_print(e, ' ') :- !.
char_print(X, X).

% List helpers
max_list([H|T], Max) :- max_list(T, H, Max).
max_list([], Max, Max).
max_list([H|T], Acc, Max) :- H > Acc, max_list(T, H, Max).
max_list([H|T], Acc, Max) :- H =< Acc, max_list(T, Acc, Max).

min_list([H|T], Min) :- min_list(T, H, Min).
min_list([], Min, Min).
min_list([H|T], Acc, Min) :- H < Acc, min_list(T, H, Min).
min_list([H|T], Acc, Min) :- H >= Acc, min_list(T, Acc, Min).