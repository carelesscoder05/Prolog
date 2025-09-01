% =================================================================
%      FOL Resolution with Length of Clause Heuristic
% =================================================================

% prove_with_length_heuristic(Clauses)
% Main entry point for the heuristic search.
prove_with_length_heuristic(Clauses) :-
    write('Starting Resolution with Length Heuristic...'), nl,
    % Convert the initial clauses into Length-Clause pairs.
    clauses_to_pairs(Clauses, PairedClauses),
    % Sort the pairs to create the initial priority queue.
    keysort(PairedClauses, InitialPQueue),
    % The initial set of known clauses is just the input clauses.
    heuristic_resolve(InitialPQueue, Clauses).


% heuristic_resolve(PriorityQueue, KnownClauses)
% The core recursive engine for the heuristic search.

% Base Case: The priority queue is empty, so no proof was found.
heuristic_resolve([], _) :-
    write('Proof not found. Priority queue is empty.'), nl,
    fail.

% Base Case: The highest priority clause is the empty clause! Proof succeeded.
heuristic_resolve([0-[] | _], _) :-
    write('Proof found! Empty clause derived.'), nl,
    true.

% Recursive Step: Process the shortest clause from the priority queue.
heuristic_resolve([_-CurrentClause | RestPQueue], Known) :-
    % 1. Generate all resolvents by resolving the shortest clause against all known clauses.
    findall(R, (member(Other, Known), resolve(CurrentClause, Other, R)), RawResolvents),

    % 2. Prune resolvents that have already been discovered.
    subtract(RawResolvents, Known, NewUniqueResolvents),

    % 3. Convert new unique clauses into Length-Clause pairs for the priority queue.
    clauses_to_pairs(NewUniqueResolvents, NewPairedClauses),

    % 4. Add the new pairs to the priority queue and re-sort it.
    append(RestPQueue, NewPairedClauses, MergedPQueue),
    keysort(MergedPQueue, NextPQueue),

    % 5. Add the new clauses to the set of all known clauses.
    append(NewUniqueResolvents, Known, NextKnown),

    % 6. Recurse with the new priority queue and known set.
    heuristic_resolve(NextPQueue, NextKnown).


% =================================================================
%                      Helper Predicates
% =================================================================

% clauses_to_pairs(+Clauses, -PairedClauses)
% Converts a list of clauses to a list of Length-Clause pairs.
clauses_to_pairs([], []).
clauses_to_pairs([Clause | Cs], [Len-Clause | Pairs]) :-
    length(Clause, Len),
    clauses_to_pairs(Cs, Pairs).

% resolve/3 and complementary/2 are the same as in the previous corrected versions.

% resolve(Clause1, Clause2, Resolvent)
resolve(C1, C2, Resolvent) :-
    copy_term(C2, C2_fresh),
    select(L1, C1, RestC1),
    select(L2, C2_fresh, RestC2_fresh),
    complementary(L1, L2),
    append(RestC1, RestC2_fresh, TempResolvent),
    sort(TempResolvent, Resolvent).

% complementary(Literal1, Literal2)
complementary(L, neg(L)).
complementary(neg(L), L).


# Clauses = [
#        [animal(f(X)), loves(g(X), X)],
#        [neg(loves(X, f(X))), loves(g(X), X)],
#        [neg(animal(Y)), neg(kills(X, Y)), neg(loves(Z, X))],
#        [neg(animal(W)), loves(jack, W)],
#        [kills(jack, tuna), kills(curiosity, tuna)],
#        [animal(tuna)],
#        [neg(kills(curiosity, tuna))]
#    ],
#    prove_with_length_heuristic(Clauses).


# This strategy is a form of "best-first search." Instead of processing clauses in the order they are generated (like BFS), it prioritizes processing shorter clauses first. The heuristic is that shorter clauses are simpler and more likely to lead to the ultimate shortest clause: the empty clause [] (length 0).
# How It Works 🧠
# The core of this strategy is a priority queue instead of a standard FIFO queue.

# Prioritization: Each clause is assigned a priority based on its length (the number of literals it contains). Shorter clauses get higher priority.

# Selection: At each step of the proof, we select the clause with the highest priority (the shortest one) from the queue to resolve next.

# Insertion: When new clauses (resolvents) are generated, they are added to the priority queue according to their length.

# This approach guides the search toward simpler clauses, often finding a proof more quickly than an uninformed strategy like Breadth-First Search, which would explore all clauses of a certain depth equally.