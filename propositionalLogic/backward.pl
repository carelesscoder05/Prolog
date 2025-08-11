/*
 * CORRECTED Backward-Chaining Inference for the Wumpus World
 *
 * This version includes logic for finding the gold.
 * To use, run the query:
 * ?- check_for_gold.
 */

% --- World Definition ---
% Defines the valid coordinates for a 4x4 grid.
coord(1). coord(2). coord(3). coord(4).

% ===================================================================
%   Wumpus World Facts (The Agents Perceptions)
% ===================================================================
stench(1, 2).
breeze(2, 1).

% NEW PERCEPTION: The agent finds a glitter at [2,3]!
glitter(2, 3).

% Knowledge: Which squares the agent has visited.
visited(1, 1).
visited(1, 2).
visited(2, 1).
visited(2, 3). % The agent had to visit [2,3] to perceive the glitter.


% ===================================================================
%   Wumpus World Rules (The Logic of the World)
% ===================================================================

%% --- User-Friendly Query for Gold ---
check_for_gold :-
    (   is_gold_at(X, Y)
    ->  format('Success! Gold found at [~w, ~w]. Consider using the Grab action.~n', [X, Y])
    ;   write('No glitter perceived. The location of the gold remains unknown.'), nl
    ).

%% --- NEW: Rule for Inferring Gold Location ---
is_gold_at(X, Y) :-
    glitter(X, Y).

%% --- Defining Adjacency (Corrected Version) ---
adjacent(X, Y, X, Y2) :- Y2 is Y - 1, coord(Y2).
adjacent(X, Y, X, Y2) :- Y2 is Y + 1, coord(Y2).
adjacent(X, Y, X2, Y) :- X2 is X - 1, coord(X2).
adjacent(X, Y, X2, Y) :- X2 is X + 1, coord(X2).

%% --- Defining Safety ---
is_safe(X, Y) :-
    pit_is_not_at(X, Y),
    wumpus_is_not_at(X, Y).

%% --- Rules for Inferring Wumpus Location ---
is_wumpus_at(X, Y) :-
    coord(X), coord(Y),
    stench(Sx, Sy),
    adjacent(Sx, Sy, X, Y),
    forall(
        (adjacent(Sx, Sy, OtherX, OtherY), (OtherX, OtherY) \= (X, Y)),
        wumpus_is_not_at(OtherX, OtherY)
    ).

wumpus_is_not_at(X, Y) :- visited(X, Y).
wumpus_is_not_at(X, Y) :-
    coord(X), coord(Y),
    adjacent(X, Y, Ax, Ay),
    visited(Ax, Ay),
    \+ stench(Ax, Ay).

%% --- Rules for Inferring Pit Location ---
is_pit_at(X, Y) :-
    coord(X), coord(Y),
    breeze(Bx, By),
    adjacent(Bx, By, X, Y),
    forall(
        (adjacent(Bx, By, OtherX, OtherY), (OtherX, OtherY) \= (X, Y)),
        pit_is_not_at(OtherX, OtherY)
    ).

pit_is_not_at(X, Y) :- visited(X, Y).
pit_is_not_at(X, Y) :-
    coord(X), coord(Y),
    adjacent(X, Y, Ax, Ay),
    visited(Ax, Ay),
    \+ breeze(Ax, Ay).


% ?- check_for_gold.
% Success! Gold found at [2, 3]. Consider using the Grab action.
% true.

% ?- is_wumpus_at(1, 3).
% true.

% ?- is_safe(2, 2).
% true.

% ?- is_pit_at(3, 1).
% true.
