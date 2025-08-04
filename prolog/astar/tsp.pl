% A* for Traveling Salesperson Problem (TSP)
%
% --- How to use ---
% 1. Define your graph using city/1 and edge/3 facts.
% 2. Run the query: tsp(start_city, Path, Cost).
%    e.g., tsp(a, Path, Cost).

:- op(400, yfx, f). % For our state representation f(F, G, H, Path)

% --- Graph Definition ---
% Define the cities in the graph.
city(a).
city(b).
city(c).
city(d).

% Define the edges and their costs (undirected graph).
edge(a, b, 10).
edge(b, a, 10).
edge(a, c, 15).
edge(c, a, 15).
edge(a, d, 20).
edge(d, a, 20).
edge(b, c, 35).
edge(c, b, 35).
edge(b, d, 25).
edge(d, b, 25).
edge(c, d, 30).
edge(d, c, 30).

% Helper to get distance, handling the symmetric nature of the graph.
distance(City1, City2, Cost) :- edge(City1, City2, Cost).


% --- Main TSP Predicate ---
% tsp(StartCity, FinalPath, FinalCost)
% This is the main entry point for the user.
tsp(Start, Path, Cost) :-
    % Find all cities and count them
    findall(C, city(C), Cities),
    length(Cities, NumCities),

    % Initial state: f(F, G, H, Path)
    % G is cost so far (0), H is heuristic (0), F = G + H.
    % Path is a list starting with the Start city.
    heuristic([], NumCities, Start, H), % Heuristic for the start node
    G is 0,
    F is G + H,
    InitialState = f(F, G, H, [Start]),

    % Start the A* search
    astar([InitialState], [], NumCities, Start, Path, Cost).

% --- A* Search Algorithm ---

% Base Case: GOAL FOUND
% The path in the head of the OpenList has visited all cities.
astar([f(_, G, _, Path) | _], _, NumCities, Start, FinalPath, FinalCost) :-
    length(Path, NumCities),
    Path = [LastCity | _], % The path is stored reversed
    distance(LastCity, Start, ReturnCost), !, % Check if a path back to start exists
    FinalCost is G + ReturnCost,
    reverse(Path, FwdPath),
    append(FwdPath, [Start], FinalPath). % Append start city to complete the tour

% Recursive Step: EXPAND NODE
% Pop the best node, generate its successors, and continue the search.
astar([CurrentState | RestOpen], Closed, NumCities, Start, FinalPath, FinalCost) :-
    expand(CurrentState, NumCities, Start, Successors),
    append(Successors, RestOpen, NewOpenUnsorted),
    sort(NewOpenUnsorted, NewOpen), % Sorts by F-score, maintaining the priority queue
    astar(NewOpen, [CurrentState | Closed], NumCities, Start, FinalPath, FinalCost).

% Base Case: FAILURE
% The OpenList is empty, but no solution was found.
astar([], _, _, _, _, _) :-
    write('No solution found.'), nl,
    fail.


% --- Helper Predicates ---

% expand(+CurrentState, +NumCities, +Start, -Successors)
% Generates all valid successor states from the current state.
expand(f(_, G, _, Path), NumCities, Start, Successors) :-
    Path = [CurrentCity | _],
    findall(
        f(F, NewG, H, [NextCity | Path]),
        (
            distance(CurrentCity, NextCity, EdgeCost),
            \+ member(NextCity, Path), % The crucial check to avoid cycles
            NewG is G + EdgeCost,
            heuristic([NextCity | Path], NumCities, Start, H),
            F is NewG + H
        ),
        Successors
    ).

% heuristic(+Path, +NumCities, +Start, -H)
% Calculates the heuristic value.
% This is an admissible heuristic h(n) = 0.
% This turns A* into Uniform Cost Search, which is optimal but less efficient.
% For a better heuristic, you would implement something like MST of unvisited nodes.
heuristic(_, _, _, 0).