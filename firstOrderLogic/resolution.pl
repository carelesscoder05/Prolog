% Required for list operations like union/3 and select/3.
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 1: FOL to CNF CONVERTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_cnf(F, CNF) :-
    eliminate_iff(F, F1),
    eliminate_imp(F1, F2),
    move_neg_in(F2, F3),
    skolemize(F3, [], 0, F4, _),
    drop_foralls(F4, F5),
    distribute(F5, F6),
    collect_clauses(F6, CNF).

eliminate_iff(iff(A,B), and(imp(A,B), imp(B,A))) :- !.
eliminate_iff(forall(X,F), forall(X,F1)) :- !, eliminate_iff(F, F1).
eliminate_iff(exists(X,F), exists(X,F1)) :- !, eliminate_iff(F, F1).
eliminate_iff(and(A,B), and(A1,B1)) :- !, eliminate_iff(A,A1), eliminate_iff(B,B1).
eliminate_iff(or(A,B), or(A1,B1)) :- !, eliminate_iff(A,A1), eliminate_iff(B,B1).
eliminate_iff(not(F), not(F1)) :- !, eliminate_iff(F, F1).
eliminate_iff(F, F).

eliminate_imp(imp(A,B), or(not(A1), B1)) :- !, eliminate_imp(A,A1), eliminate_imp(B,B1).
eliminate_imp(forall(X,F), forall(X,F1)) :- !, eliminate_imp(F, F1).
eliminate_imp(exists(X,F), exists(X,F1)) :- !, eliminate_imp(F, F1).
eliminate_imp(and(A,B), and(A1,B1)) :- !, eliminate_imp(A,A1), eliminate_imp(B,B1).
eliminate_imp(or(A,B), or(A1,B1)) :- !, eliminate_imp(A,A1), eliminate_imp(B,B1).
eliminate_imp(not(F), not(F1)) :- !, eliminate_imp(F, F1).
eliminate_imp(F, F).

move_neg_in(not(not(F)), F1) :- !, move_neg_in(F, F1).
move_neg_in(not(and(A,B)), or(A1,B1)) :- !, move_neg_in(not(A),A1), move_neg_in(not(B),B1).
move_neg_in(not(or(A,B)), and(A1,B1)) :- !, move_neg_in(not(A),A1), move_neg_in(not(B),B1).
move_neg_in(not(forall(X,F)), exists(X,F1)) :- !, move_neg_in(not(F), F1).
move_neg_in(not(exists(X,F)), forall(X,F1)) :- !, move_neg_in(not(F), F1).
move_neg_in(forall(X,F), forall(X,F1)) :- !, move_neg_in(F, F1).
move_neg_in(exists(X,F), exists(X,F1)) :- !, move_neg_in(F, F1).
move_neg_in(and(A,B), and(A1,B1)) :- !, move_neg_in(A,A1), move_neg_in(B,B1).
move_neg_in(or(A,B), or(A1,B1)) :- !, move_neg_in(A,A1), move_neg_in(B,B1).
move_neg_in(F, F).

skolemize(exists(X,F), UVars, C_in, F_out, C_out) :- !,
    ( UVars = [] ->
        atomic_list_concat([skc, C_in], SkolemTerm)
    ;
        atomic_list_concat([skf, C_in], SkolemFunc),
        SkolemTerm =.. [SkolemFunc | UVars]
    ),
    subst(F, X, SkolemTerm, F_sub),
    C_next is C_in + 1,
    skolemize(F_sub, UVars, C_next, F_out, C_out).
skolemize(forall(X,F), UVars, C_in, forall(X,F_out), C_out) :- !,
    skolemize(F, [X|UVars], C_in, F_out, C_out).
skolemize(and(A,B), UVars, C_in, and(A_out, B_out), C_out) :- !,
    skolemize(A, UVars, C_in, A_out, C_mid),
    skolemize(B, UVars, C_mid, B_out, C_out).
skolemize(or(A,B), UVars, C_in, or(A_out, B_out), C_out) :- !,
    skolemize(A, UVars, C_in, A_out, C_mid),
    skolemize(B, UVars, C_mid, B_out, C_out).
skolemize(F, _, C, F, C).

subst(Term, X, _Replacement, Term) :- var(Term), Term \== X, !.
subst(Term, X, Replacement, Replacement) :- var(Term), Term == X, !.
subst(Atomic, _, _, Atomic) :- atomic(Atomic), !.
subst(Compound, X, Replacement, Result) :-
    Compound =.. [Func|Args],
    maplist(subst_mapper(X, Replacement), Args, NewArgs),
    Result =.. [Func|NewArgs].
subst_mapper(X, R, Term, NewTerm) :- subst(Term, X, R, NewTerm).

drop_foralls(forall(_,F), F_out) :- !, drop_foralls(F, F_out).
drop_foralls(and(A,B), and(A_out, B_out)) :- !, drop_foralls(A, A_out), drop_foralls(B, B_out).
drop_foralls(or(A,B), or(A_out, B_out)) :- !, drop_foralls(A, A_out), drop_foralls(B, B_out).
drop_foralls(F, F).

distribute(or(A, and(B,C)), and(D1, D2)) :- !,
    distribute(or(A,B), D1),
    distribute(or(A,C), D2).
distribute(or(and(A,B), C), and(D1, D2)) :- !,
    distribute(or(A,C), D1),
    distribute(or(B,C), D2).
distribute(or(A,B), or(A1,B1)) :- !, distribute(A,A1), distribute(B,B1).
distribute(and(A,B), and(A1,B1)) :- !, distribute(A,A1), distribute(B,B1).
distribute(F, F).

collect_clauses(and(A,B), Clauses) :- !,
    collect_clauses(A, C1),
    collect_clauses(B, C2),
    append(C1, C2, Clauses).
collect_clauses(Clause, [Lits]) :- !,
    collect_lits(Clause, Lits).
collect_lits(or(A,B), Lits) :- !,
    collect_lits(A, L1),
    collect_lits(B, L2),
    append(L1, L2, Lits).
collect_lits(not(F), [neg(F)]) :- !.
collect_lits(F, [F]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 2: RESOLUTION ENGINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variant(A, B) :- subsumes_term(A, B), subsumes_term(B, A).
member_variant(X, [Y|_]) :- variant(X, Y), !.
member_variant(X, [_|T]) :- member_variant(X, T).
filter_new([], _, []).
filter_new([H|T], Known, Result) :- member_variant(H, Known), !, filter_new(T, Known, Result).
filter_new([H|T], Known, [H|Result]) :- filter_new(T, Known, Result).
resolve(C1, C2, Resolvent) :-
    copy_term(C2, C2_fresh), select(L1, C1, C1_rest), select(L2, C2_fresh, C2_rest),
    ( L1 = neg(Atom1), L2 = Atom2, Atom1 = Atom2 ; L2 = neg(Atom2), L1 = Atom1, Atom1 = Atom2 ),
    union(C1_rest, C2_rest, TempResolvent), sort(TempResolvent, Resolvent).
is_tautology(Clause) :- member(L, Clause), ( L = neg(A) -> member(A, Clause) ; member(neg(L), Clause) ), !.
resolution_loop(_, _, D, MaxD) :- D > MaxD, !, write('--- Proof Not Found (Max depth exceeded). ---'), nl, fail.
resolution_loop(SoS, _, _, _) :- member([], SoS), !, write('--- Proof Found (Contradiction derived). ---'), nl.
resolution_loop([], _, _, _) :- !, write('--- Proof Not Found (Set of Support is empty). ---'), nl, fail.
resolution_loop([GC|SoS_r], KCs, D, MaxD) :-
    findall(R, ( append(KCs, [GC|SoS_r], AllC), member(RW, AllC), resolve(GC, RW, R), \+ is_tautology(R) ), NRs),
    append(KCs, [GC|SoS_r], CKB), filter_new(NRs, CKB, UNCs),
    append(SoS_r, UNCs, NewSoS), append(UNCs, KCs, NewKCs), D_new is D + 1,
    resolution_loop(NewSoS, NewKCs, D_new, MaxD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 3: PROOF GLUE AND USER-FACING PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prove(QueryFOL, KBFOL) :- prove(QueryFOL, KBFOL, 50).
prove(QueryFOL, KBFOL, MaxDepth) :-
    maplist(to_cnf, KBFOL, ListOfClauseLists),
    append(ListOfClauseLists, KB_CNF), % <-- THE FINAL FIX IS HERE
    to_cnf(not(QueryFOL), SoS_Clauses),

    write('--- Converted Knowledge Base (CNF) ---'), nl, print_kb(KB_CNF),
    write('--- Negated Query Clauses (CNF) ---'), nl, print_kb(SoS_Clauses), nl,

    write('--- Starting Resolution (Max Depth: '), write(MaxDepth), write(') ---'), nl,
    resolution_loop(SoS_Clauses, KB_CNF, 0, MaxDepth).

print_clause([]) :- write('[] (Contradiction)').
print_clause([L]) :- write(L).
print_clause([L|Ls]) :- write(L), write(' v '), print_clause(Ls).
print_kb([]).
print_kb([C|Cs]) :- write('  '), print_clause(C), nl, print_kb(Cs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 4: EXAMPLE PROBLEM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kb_fol(forall(X, imp( forall(Y, imp(animal(Y), loves(X,Y))), exists(Z, loves(Z,X)) ))).
kb_fol(forall(X, imp( exists(Y, and(animal(Y), kills(X,Y))), forall(Z, not(loves(Z,X))) ))).
kb_fol(forall(X, imp(animal(X), loves(jack, X)))).
kb_fol(or(kills(jack, tuna), kills(curiosity, tuna))).
kb_fol(cat(tuna)).
kb_fol(forall(X, imp(cat(X), animal(X)))).

query_fol(kills(curiosity, tuna)).

run :-
    findall(F, kb_fol(F), KBFOL),
    query_fol(QueryFOL),
    prove(QueryFOL, KBFOL).