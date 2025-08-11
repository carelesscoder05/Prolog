/*
 * Resolution Prover for the Wumpus World
 *
 * To run, consult this file in SWI-Prolog and execute the goal:
 * ?- run_wumpus_inference.
 */

:- op(100, fx, neg). % Define 'neg' as a prefix operator for readability

% ===================================================================
%   Main Entry Point
% ===================================================================

run_wumpus_inference :-
    % We want to prove that the Wumpus is at [1,3]
    Query = w(1,3),
    write('===================================================='), nl,
    write('       Wumpus World Resolution Prover'), nl,
    write('===================================================='), nl,
    write('Attempting to prove by refutation: '), writeln(Query), nl,
    prove(Query).

% ===================================================================
%   Resolution Engine
% ===================================================================

/**
 * prove(+Query)
 *
 * Tries to prove a query by refutation. It negates the query,
 * adds it to the knowledge base, and starts the resolution process.
 */
prove(Query) :-
    kb(KB),
    negate_query(Query, NegatedQueryClauses),
    append(KB, NegatedQueryClauses, TempClauses),
    maplist(sort, TempClauses, SortedClauses), % Use a canonical representation
    list_to_set(SortedClauses, ClauseSet), % Remove any duplicate clauses
    
    write('--- Initial Clauses (KB + Negated Query) ---'), nl,
    print_clauses(ClauseSet), nl,
    
    % The resolution loop is seeded with all clauses, but subsequent
    % iterations will only use newly derived clauses as the set of support.
    resolution([ClauseSet, ClauseSet]).

/**
 * resolution(+[KnownClauses, SetOfSupport])
 *
 * The core resolution loop. It repeatedly resolves clauses from the
 * SetOfSupport against all KnownClauses to derive new clauses.
 * It halts when it derives the empty clause (success) or when no
 * new clauses can be derived (failure).
 */
resolution([Known, _]) :-
    member([], Known), !,
    write('--- Proof Successful! Contradiction (empty clause) found. ---'), nl.

resolution([Known, SoS]) :-
    findall(Resolvent,
            (   select(C1, SoS, _),      % Select a clause from the set of support
                member(C2, Known),      % Select any clause from the known set
                resolve(C1, C2, R),     % Resolve them
                sort(R, Resolvent)      % Sort for canonical representation
            ),
            Resolvents),
    
    list_to_set(Resolvents, UniqueResolvents),
    subtract(UniqueResolvents, Known, NewlyDerived), % Find genuinely new clauses
    
    ( NewlyDerived == [] ->
        write('--- Proof Failed. No new clauses can be derived. ---'), nl, fail
    ;
        write('--- New Clauses Derived in this step ---'), nl,
        print_clauses(NewlyDerived), nl,
        append(Known, NewlyDerived, NextKnown),
        resolution([NextKnown, NewlyDerived]) % Recur with the new clauses as the next SoS
    ).


% ===================================================================
%   Core Resolution and Utility Predicates
% ===================================================================

/**
 * resolve(+Clause1, +Clause2, -Resolvent)
 *
 * Resolves two clauses if they contain complementary literals.
 * For example, [p, q] and [neg(p), r] resolve to [q, r].
 */
resolve(C1, C2, Resolvent) :-
    member(Literal, C1),                % Select a literal from C1
    complement(Literal, CompLiteral),   % Find its complement
    member(CompLiteral, C2),            % Check if the complement is in C2
    
    select(Literal, C1, Rem1),          % Remove the literals
    select(CompLiteral, C2, Rem2),
    
    union(Rem1, Rem2, TempResolvent),   % Merge the remaining literals
    \+ has_complementary_pair(TempResolvent), % Ensure its not a tautology
    Resolvent = TempResolvent.

% Helper to negate the initial query for proof by refutation.
negate_query(neg(Term), [[Term]]).
negate_query(Term, [[neg(Term)]]) :- Term \= neg(_).

% Helper to find the complement of a literal.
complement(neg(P), P).
complement(P, neg(P)) :- P \= neg(_).

% Helper to check if a clause is a tautology (contains both P and neg(P)).
has_complementary_pair(Clause) :-
    member(L, Clause),
    complement(L, CompL),
    member(CompL, Clause), !.

% Utility to print clauses nicely.
print_clauses([]).
print_clauses([H|T]) :-
    write('  '), writeln(H),
    print_clauses(T).


% ===================================================================
%   Wumpus World Knowledge Base (KB)
% ===================================================================

/**
 * kb(-Clauses)
 *
 * Defines the static knowledge base for the specific scenario.
 * It includes fundamental rules of the world and the agent's perceptions.
 */
kb(KB) :-
    KB = [
        % --- Percepts ---
        % Agent perceived no stench at [1,1] and [2,1], and no breeze at [1,1].
        [neg(s(1,1))],
        [neg(s(2,1))],
        [neg(b(1,1))],
        % Agent perceived a stench at [1,2] and a breeze at [2,1].
        [s(1,2)],
        [b(2,1)],

        % --- Initial Knowledge ---
        % The agent knows its starting square [1,1] is safe.
        [neg(p(1,1))],
        [neg(w(1,1))],

        % --- Rules of the World (converted to CNF) ---
        % B(1,1) <=> P(1,2) v P(2,1)
        [neg(b(1,1)), p(1,2), p(2,1)],
        [b(1,1), neg(p(1,2))],
        [b(1,1), neg(p(2,1))],

        % S(1,2) <=> W(1,1) v W(2,2) v W(1,3)
        [neg(s(1,2)), w(1,1), w(2,2), w(1,3)],
        [s(1,2), neg(w(1,1))],
        [s(1,2), neg(w(2,2))],
        [s(1,2), neg(w(1,3))],

        % W(adj) => S(2,1)
        [neg(w(1,1)), s(2,1)],
        [neg(w(2,2)), s(2,1)],
        [neg(w(3,1)), s(2,1)]
    ].



% ?- run_wumpus_inference.