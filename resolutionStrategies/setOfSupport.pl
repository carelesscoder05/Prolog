% =================================================================
%      FOL Resolution with Set of Support (SoS) Strategy
% =================================================================

% prove_with_sos(GoalClauses, KB_Clauses)
% Main entry point.
% GoalClauses: A list of clauses from the negated goal. This is the initial SoS.
% KB_Clauses: A list of axiom/knowledge base clauses.
prove_with_sos(GoalClauses, KB_Clauses) :-
    write('Starting Resolution with Set of Support...'), nl,
    % The SoS queue starts with the goal clauses.
    % The set of all known SoS clauses also starts with the goal clauses.
    sos_resolve(GoalClauses, KB_Clauses, GoalClauses).


% sos_resolve(SoS_Queue, KB_Clauses, All_Known_SoS_Clauses)
% The core recursive engine for the SoS strategy.

% Base Case: The SoS queue is empty, so no more work can be done. Proof fails.
sos_resolve([], _, _) :-
    write('Proof not found. Set of Support is empty.'), nl,
    fail.

% Recursive Step: Process the first clause from the SoS queue.
sos_resolve([CurrentSoS | RestSoSQueue], KB, AllSoS) :-
    % 1. Resolve the current SoS clause against all KB clauses.
    findall(R1, (member(KBC, KB), resolve(CurrentSoS, KBC, R1)), Resolvents_from_KB),

    % 2. Resolve the current SoS clause against all previously found SoS clauses.
    findall(R2, (member(SoSC, AllSoS), resolve(CurrentSoS, SoSC, R2)), Resolvents_from_SoS),

    % 3. Combine the new resolvents from both sources.
    append(Resolvents_from_KB, Resolvents_from_SoS, RawResolvents),

    % 4. Check if the empty clause has been derived for success.
    (   member([], RawResolvents)
    ->  write('Proof found! Empty clause derived.'), nl,
        true
    ;
        % 5. Prune clauses that we already know.
        % A clause is known if its in the KB or already in the SoS.
        union(AllSoS, KB, AllKnownClauses),
        subtract(RawResolvents, AllKnownClauses, NewUniqueResolvents),

        % 6. Update the SoS queue and the set of all SoS clauses.
        append(RestSoSQueue, NewUniqueResolvents, NextSoSQueue),
        append(NewUniqueResolvents, AllSoS, NextAllSoS),

        % 7. Recurse.
        sos_resolve(NextSoSQueue, KB, NextAllSoS)
    ).

% =================================================================
%                      Helper Predicates
% =================================================================

% resolve/3 and complementary/2 are the same as in the previous corrected version.

% resolve(Clause1, Clause2, Resolvent)
% Uses copy_term/2 to prevent variable pollution during unification.
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



# ?- KB_Clauses = [
#        [animal(f(X)), loves(g(X), X)],
#        [neg(loves(X, f(X))), loves(g(X), X)],
#        [neg(animal(Y)), neg(kills(X, Y)), neg(loves(Z, X))],
#        [neg(animal(W)), loves(jack, W)],
#        [kills(jack, tuna), kills(curiosity, tuna)],
#        [animal(tuna)]
#    ],
#    GoalClauses = [[neg(kills(curiosity, tuna))]],
#    prove_with_sos(GoalClauses, KB_Clauses).




# The core idea is to prevent the prover from doing useless work by resolving axioms against each other. Instead, every resolution must involve the goal you're trying to prove (or a clause derived from it).
# How It Works 🧠
# The Set of Support (SoS) strategy works by partitioning your initial clauses into two groups:

# The Knowledge Base (KB): These are your trusted axioms and facts. We assume this set of clauses is internally consistent (satisfiable).

# The Set of Support (SoS): This set initially contains only the clauses that come from the negated goal.

# The resolution process is then restricted by one simple rule: you cannot resolve two clauses that are both from the KB. At least one of the parent clauses in any resolution step must originate from the Set of Support.

# Any new clause (a resolvent) that is generated is added to the SoS. This way, the prover stays "focused" on the goal, as every new piece of information is directly related to the initial query. It's like a detective only following leads that are connected to the specific crime, rather than interviewing random people in the city.