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

%atMostDiffLength(+S,?T,+N) % devuelve true si S y T difieren en longitud en a lo sumo en N y ademas son solo ceros y unos
atMostDiffLength([],[],N) :- N >= 0.
atMostDiffLength([],[0|X],N) :- N > 0, atMostDiffLength([],X,N-1).
atMostDiffLength([],[1|X],N) :- N > 0, atMostDiffLength([],X,N-1).
atMostDiffLength([0|X],[],N) :- N > 0, atMostDiffLength(X,[],N-1).
atMostDiffLength([1|X],[],N) :- N > 0, atMostDiffLength(X,[],N-1).
atMostDiffLength([0|X],[0|Y],N) :- atMostDiffLength(X,Y,N).
atMostDiffLength([0|X],[1|Y],N) :- atMostDiffLength(X,Y,N).
atMostDiffLength([1|X],[0|Y],N) :- atMostDiffLength(X,Y,N).
atMostDiffLength([1|X],[1|Y],N) :- atMostDiffLength(X,Y,N).

%distEd(+S,?T,?N)
distEd([],X,N) :- ground(N), N >= 0, length(X,N), esBinL(X). % si N esta instanciado chequeo que sea mayor o igual a cero, que la longitud de X sea N y que X tenga solo ceros y unos
distEd(X,[],N) :- ground(N), N > 0, length(X,N), esBinL(X). % lo mismo que en el caso anterior pero con N > 0 para evitar repeticiones
distEd([],X,N) :- not(ground(N)), length(X,N), esBinL(X). % si N no esta instanciado devuelvo true si X tiene longitud N y esta compuesto por ceros y unos
distEd(X,[],N) :- not(ground(N)), not(length(X,0)), length(X,N), esBinL(X). % igual que en el caso anterior pero con not(length(X,0)) para evitar repetidos

distEd([F|X],[G|Y],N) :- ground(N), atMostDiffLength(X,Y,N), distEd(X,Y,A), distEd(X,[G|Y],B), distEd([F|X],Y,C), diffBit(F,G,D), N is min(A+D,min(B+1,C+1)).
% cuando N esta instanciado chequeo que la diferencia de longitud entre X e Y sea a lo sumo N (para restringir el dominio de Y) y luego para cada
% Y que genero testeo con la dinamica del enunciado
distEd([F|X],[G|Y],N) :- not(ground(N)), distEd(X,Y,A), distEd(X,[G|Y],B), distEd([F|X],Y,C), diffBit(F,G,D), N is min(A+D,min(B+1,C+1)).
% cuadno N no esta instanciado hago lo mismo que en el caso anterior pero sin generar los Y posibles primero ya que hay infinitos Y posibles
% en el caso de que Y no este instanciado
