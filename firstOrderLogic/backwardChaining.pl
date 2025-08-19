% --- Facts ---
american(west).
hostile(nono).
owns(nono, m1).
missile(m1).

% --- Rules ---
% If X is a missile, then X is a weapon.
weapon(X) :- missile(X).

% If an American (X) sells a weapon (Y) to a hostile nation (Z), then X is a criminal.
criminal(X) :-
    american(X),
    weapon(Y),
    sells(X, Y, Z),
    hostile(Z).

% If Nono owns missile m1, then West sold it to them.
sells(west, m1, nono) :-
    owns(nono, m1),
    missile(m1).

% ?- criminal(west).