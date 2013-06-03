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

sonDistintos(X, Y) :- esBin(X), esBin(Y), X \= Y.

noSonIguales(1, 0).
noSonIguales(0, 1).

esBin(0).
esBin(1).
esBinL([]).
esBinL([X|XS]):- esBin(X), esBinL(XS).

distHam([], [], 0).
distHam([X|XS], [X|YS], D) :- esBin(X), distHam(XS, YS, D).
distHam([X|XS], [Y|YS], D) :- noSonIguales(X, Y), distHam(XS, YS, T),  D is T + 1.

desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).


distPref([], XS, D) :- length(XS, D), esBinL(XS).
distPref([X|XS], [], D) :- esBinL([X|XS]), length([X|XS], D).
distPref([X|XS], [Y|YS], D) :- 	ground(D), noSonIguales(X, Y),
								esBinL(XS), length(XS, A),
								B is D - A - 2,
								length(YS, B), esBinL(YS).
distPref([X|XS], [Y|YS], D) :- 	not(ground(D)), not(ground([Y|YS])),
                                desde(0,D), distPref([X|XS], [Y|YS], D).

distPref([X|XS], [X|YS], D) :-  ground(D), esBin(X), distPref(XS, YS, D).
