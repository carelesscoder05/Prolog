% Greedy Best-First Search for TSP (Nearest Neighbor Heuristic)
%
% --- How to use ---
% 1. Define your graph using city/1 and edge/3 facts.
% 2. Run the query: tsp_greedy(start_city, Path, Cost).
%    e.g., tsp_greedy(a, Path, Cost).

% --- Graph Definition (same as before) ---
city(a).
city(b).
city(c).
city(d).

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

% Helper to get distance.
distance(City1, City2, Cost) :- edge(City1, City2, Cost).


% --- Main Greedy TSP Predicate ---
% tsp_greedy(StartCity, FinalPath, FinalCost)
% This is the main entry point for the user.
tsp_greedy(Start, Path, Cost) :-
    % Find all cities that need to be visited
    findall(C, city(C), AllCities),
    select(Start, AllCities, Unvisited), % Unvisited is AllCities without Start

    % Start the greedy search from the Start city.
    % The accumulator for the path starts with just the Start city.
    % The accumulator for the cost starts at 0.
    greedy_loop(Start, Unvisited, [Start], 0, Start, Path, Cost).


% --- Greedy Search Loop ---

% Base Case: All cities have been visited.
% The Unvisited list is now empty. Complete the tour by returning to the start.
greedy_loop(LastCity, [], RevPath, AccCost, Start, FinalPath, FinalCost) :- !,
    distance(LastCity, Start, ReturnCost),
    FinalCost is AccCost + ReturnCost,
    reverse(RevPath, FwdPath),
    append(FwdPath, [Start], FinalPath).

% Recursive Step: Find the nearest neighbor and move to it.
greedy_loop(CurrentCity, Unvisited, RevPath, AccCost, Start, FinalPath, FinalCost) :-
    % Find the nearest unvisited city from the current one.
    find_nearest(CurrentCity, Unvisited, NextCity, EdgeCost),

    % Update the state for the next recursive call
    NewCost is AccCost + EdgeCost,
    select(NextCity, Unvisited, RemainingUnvisited), % Remove the chosen city from Unvisited
    greedy_loop(NextCity, RemainingUnvisited, [NextCity | RevPath], NewCost, Start, FinalPath, FinalCost).


% --- Helper Predicates ---

% find_nearest(+From, +Candidates, -Nearest, -MinCost)
% Finds the city in the Candidates list that is nearest to the From city.
find_nearest(From, Candidates, Nearest, MinCost) :-
    % Create a list of pairs: [Cost-City, ...]
    findall(Cost-City, (member(City, Candidates), distance(From, City, Cost)), Edges),
    % Sort the list by cost (the key)
    keysort(Edges, [MinCost-Nearest | _]). % The head of the sorted list is the nearest