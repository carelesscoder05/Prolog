% =================================================================
%      FOL Resolution with Breadth-First Strategy (Corrected)
% =================================================================

% prove(Clauses)
prove(Clauses) :-
    (   member([], Clauses)
    ->  write('Proof found: Empty clause was in the initial set.'), nl, true
    ;   write('Starting BFS Resolution...'), nl,
        bfs_resolve(Clauses, Clauses)
    ).

% bfs_resolve(Queue, KnownClauses)
bfs_resolve([], _) :-
    write('Proof not found. No more clauses to resolve.'), nl,
    fail.

bfs_resolve([CurrentClause | RestQueue], Known) :-
    findall(Resolvent,
            (member(OtherClause, Known), resolve(CurrentClause, OtherClause, Resolvent)),
            RawResolvents),
    (   member([], RawResolvents)
    ->  write('Proof found! Empty clause derived.'), nl,
        true
    ;
        subtract(RawResolvents, Known, NewUniqueResolvents),
        append(RestQueue, NewUniqueResolvents, NextQueue),
        append(NewUniqueResolvents, Known, NextKnown),
        bfs_resolve(NextQueue, NextKnown)
    ).

% resolve(Clause1, Clause2, Resolvent)
% CORRECTED VERSION with copy_term/2 to ensure variables are fresh.
resolve(C1, C2, Resolvent) :-
    copy_term(C2, C2_fresh), % Prevents variable binding pollution.
    select(L1, C1, RestC1),
    select(L2, C2_fresh, RestC2_fresh),
    complementary(L1, L2),
    append(RestC1, RestC2_fresh, TempResolvent),
    sort(TempResolvent, Resolvent).

% complementary(Literal1, Literal2)
complementary(L, neg(L)).
complementary(neg(L), L).



% ?- prove([
%       [neg(man(X)), mortal(X)],  % All men are mortal
%       [man(socrates)],           % Socrates is a man
%       [neg(mortal(socrates))]    % Negation of the goal "Socrates is mortal"
%   ]).


% ?- prove([
%    [neg(dragon(X)), breathes_fire(X)],
%    [neg(dragon(Y)), mythical(Y)],
%    [dragon(spyro)],
%    [neg(mythical(Z)), neg(real_animal(Z))],
%    [real_animal(spyro)]
% ]).




# The core idea of a BFS strategy is to generate all possible resolvents at a given "depth" before moving on to the next depth. This is accomplished by using a queue (or "agenda") of clauses. We process clauses from the front of the queue and add any new, unique resolvents to the back.

# Assumptions
# Input is in Clausal Form: The program assumes the knowledge base and the negated goal are already converted into Conjunctive Normal Form (CNF), represented as a list of clauses.

# Clause Representation: A clause is a list of literals. For example, the clause P(x)
# lor
# negQ(a) is represented as [p(X), neg(q(a))].

# Negation: A negative literal like 
# negP(x) is represented using a functor, e.g., neg(p(X)).

# Empty Clause: The empty clause, which signifies a successful proof, is represented by an empty list [].

# How to Use: An Example
# Lets use the classic "Socrates is mortal" argument.

# Knowledge Base (KB):

# All men are mortal: 
# forallx(
# textMan(x)
# rightarrow
# textMortal(x))

# Socrates is a man: 
# textMan(
# textsocrates)

# Goal:

# Socrates is mortal: 
# textMortal(
# textsocrates)

# Conversion to Clausal Form:

# forallx(
# neg
# textMan(x)
# lor
# textMortal(x))
# implies[
# textneg(
# textman(X)),
# textmortal(X)]

# textMan(
# textsocrates)
# implies[
# textman(
# textsocrates)]

# Negated Goal: 
# neg
# textMortal(
# textsocrates)
# implies[
# textneg(
# textmortal(
# textsocrates))]

# Now, we combine these into a single list for our prover.