/*
 * Forward-Chaining Prover for the Wumpus World
 *
 * To run, consult this file in SWI-Prolog and execute the goal:
 * ?- run_wumpus_inference.
 */

:- op(100, fx, neg). % Define 'neg' as a prefix operator for readability
:- dynamic fact/1.   % Declare 'fact' as a dynamic predicate

% ===================================================================
%   Main Entry Point
% ===================================================================

run_wumpus_inference :-
    write('===================================================='), nl,
    write('      Wumpus World Forward-Chaining Prover'), nl,
    write('===================================================='), nl, nl,
    
    % Clean up any facts from previous runs
    retractall(fact(_)),
    
    % Assert the initial facts from the scenario
    write('--- Initial Facts ---'), nl,
    (   initial_fact(F),
        write('  '), writeln(F),
        assertz(fact(F)),
        fail % Loop through all initial_fact predicates
    ;   true
    ),
    nl,
    
    % Start the inference process
    write('--- Inference Process ---'), nl,
    infer,
    nl,
    
    % Display the final results
    write('--- Final Deduced Facts ---'), nl,
    list_all_facts.

% ===================================================================
%   Forward-Chaining Engine
% ===================================================================

/**
 * infer
 *
 * The core forward-chaining loop. It tries to find a rule whose
 * premises are satisfied and whose conclusion is a new fact.
 * If successful, it asserts the new fact and restarts the process.
 * If it completes a full pass without deriving anything new, it stops.
 */
infer :-
    % Find a rule whose premises are met but conclusion is not yet a fact
    rule(ID, Premises, Conclusion),
    all_premises_true(Premises),
    \+ fact(Conclusion),
    
    !, % Cut to prevent backtracking on rule selection
    
    % Announce the new deduction and assert it
    write('  [Rule '), write(ID), write(']: '),
    write(Premises), write(' -> '), writeln(Conclusion),
    assertz(fact(Conclusion)),
    
    % Restart the inference process with the new fact
    infer.

% Base case: If no new rules can be fired, the process is complete.
infer :-
    write('  (No more facts can be derived.)'), nl.

/**
 * all_premises_true(+ListOfPremises)
 *
 * Succeeds if every premise in the list is currently a known fact.
 */
all_premises_true([]). % Base case: an empty list of premises is always true.
all_premises_true([H | T]) :-
    fact(H),
    all_premises_true(T).

% Utility to print all known facts at the end.
list_all_facts :-
    fact(F),
    write('  - '), writeln(F),
    fail.
list_all_facts.

% ===================================================================
%   Wumpus World Knowledge Base (Facts and Rules)
% ===================================================================

% --- Initial Facts ---
% Based on the scenario: agent perceives nothing at [1,1], a breeze
% at [2,1], and a stench at [1,2].

initial_fact(neg(stench(1,1))).
initial_fact(neg(breeze(1,1))).
initial_fact(breeze(2,1)).
initial_fact(stench(1,2)).
% The agent also knows [1,1] is safe because it started there.
initial_fact(neg(wumpus(1,1))).
initial_fact(neg(pit(1,1))).
% The agent moved to [2,1] and [1,2], so it knows theres no pit there.
% It also didnt perceive a stench at [2,1].
initial_fact(neg(pit(2,1))).
initial_fact(neg(pit(1,2))).
initial_fact(neg(stench(2,1))).


% --- Inference Rules ---
% These are definite clauses (IF P1 and P2... THEN C).

% If theres no stench in a square, adjacent squares are Wumpus-free.
rule(r_ns1, [neg(stench(1,1))], neg(wumpus(1,2))).
rule(r_ns2, [neg(stench(1,1))], neg(wumpus(2,1))).
rule(r_ns3, [neg(stench(2,1))], neg(wumpus(2,2))).
rule(r_ns4, [neg(stench(2,1))], neg(wumpus(3,1))).

% If theres no breeze in a square, adjacent squares are pit-free.
% (These are already known, but included for completeness)
rule(r_nb1, [neg(breeze(1,1))], neg(pit(1,2))). % fact(neg(pit(1,2))) is asserted initially
rule(r_nb2, [neg(breeze(1,1))], neg(pit(2,1))). % fact(neg(pit(2,1))) is asserted initially

% The key inference rule for finding the Wumpus.
% This captures the logic: S(1,2) => W(1,1) v W(2,2) v W(1,3).
% If we know S(1,2) and weve proven ¬W(1,1) and ¬W(2,2), we can conclude W(1,3).
rule(r_find_wumpus, [stench(1,2), neg(wumpus(1,1)), neg(wumpus(2,2))], wumpus(1,3)).




% ?- run_wumpus_inference.