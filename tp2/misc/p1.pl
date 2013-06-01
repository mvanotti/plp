padre(john, sue). 
padre(bob, nancy). 
padre(bill, ron).
padre(bob, jeff). 
padre(bill, anne).
padre(john, bill). 

madre(mary, bill). 
madre(mary, sue). 
madre(sue, nancy). 
madre(sue, jeff). 
madre(jane, ron). 
madre(carol, anne).

hijo(X, Y) :- padre(Y, X).
hijo(X, Y) :- madre(Y, X).

descendiente(X,Y) :- hijo(X, Y).
descendiente(X,Y) :- hijo(X, P), descendiente(P,Y).

hermano(X, Y) :- hijo(X, P), hijo(Y, P), X \= Y.

long([], 0).
long([_|XS], N) :- long(XS, T), N is T + 1.

sonIguales(1, 1).
sonIguales(0, 0).

noSonIguales(1, 0).
noSonIguales(0, 1).

distHam([], [], 0).
distHam([X|XS], [Y|YS], D) :- sonIguales(X, Y), distHam(XS, YS, D).
distHam([X|XS], [Y|YS], D) :- noSonIguales(X, Y), distHam(XS, YS, T),  D is T + 1.


distPref([], XS, X) :- long(XS, X).
distPref(XS, [], X) :- long(XS, X).
distPref([X|XS], [Y|YS], D) :- noSonIguales(X, Y), long(XS, A), long(YS, B), D is A + B + 2.
distPref([X|XS], [Y|YS], D) :- sonIguales(X, Y), distPref(XS, YS, D).

