% Required for list operations
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 1: FORWARD CHAINING ENGINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% forward_chain
% Main entry point to start the forward chaining process.
forward_chain :-
    write('--- Starting FOL Forward Chaining ---'), nl,
    findall(Fact, fact(Fact), InitialFacts),
    fc_loop(InitialFacts).

% fc_loop(KnownFacts)
% The main loop of the engine. It continues until no new facts are derived.
fc_loop(KnownFacts) :-
    % Find all new, derivable facts.
    findall(InstantiatedConclusion,
            (   % Get a rule (Prolog provides a fresh copy with unbound variables).
                rule(Premises, Conclusion),
                % Satisfy all the rules premises against the known facts.
                % This will bind the variables in both Premises and Conclusion.
                satisfy(Premises, KnownFacts),
                % The Conclusion is now instantiated with the bindings.
                InstantiatedConclusion = Conclusion,
                % Check that the newly derived fact is not already known.
                \+ member(InstantiatedConclusion, KnownFacts)
            ),
            NewFacts),
    
    % Remove duplicates from the list of new facts found in this pass.
    list_to_set(NewFacts, UniqueNewFacts),

    ( UniqueNewFacts = [] ->
        % If no new facts were found, the process is complete.
        write('--- No new facts derived. Inference complete. ---'), nl, nl,
        write('Final derived facts:'), nl,
        print_facts(KnownFacts)
    ;
        % If new facts were found, print them and recurse.
        write('New facts derived in this pass:'), nl,
        print_facts(UniqueNewFacts), nl,
        append(UniqueNewFacts, KnownFacts, UpdatedFacts),
        fc_loop(UpdatedFacts)
    ).

% satisfy(ListOfPremises, KnownFacts)
% Recursively tries to prove a list of premises against the known facts.
satisfy([], _KnownFacts). % Base case: An empty list of premises is always satisfied.
satisfy([Premise|RestPremises], KnownFacts) :-
    member(Fact, KnownFacts), % Find a fact in our knowledge base.
    Premise = Fact,           % Try to unify the premise with that fact.
    satisfy(RestPremises, KnownFacts). % Continue to satisfy the rest.

% print_facts(FactList)
% Helper predicate to print the list of facts neatly.
print_facts([]).
print_facts([Fact|Rest]) :-
    write('  - '), writeln(Fact),
    print_facts(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 2: EXAMPLE KNOWLEDGE BASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- Initial Facts ---
fact(american(west)).
fact(hostile(nono)).
fact(owns(nono, m1)).
fact(missile(m1)).

% --- Rules ---
% If X is a missile, then X is a weapon.
rule([missile(X)], weapon(X)).

% If an American (X) sells a weapon (Y) to a hostile nation (Z), then X is a criminal.
rule([american(X), weapon(Y), sells(X, Y, Z), hostile(Z)], criminal(X)).

% If enemy nation Nono owns a missile M1, it is inferred that West sold it to them.
rule([owns(nono, m1), missile(m1)], sells(west, m1, nono)).