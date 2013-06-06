%noSonIguales(?X, ?Y).
% No son iguales vale si ambos parametros son digitos binarios y no son el mismo.
noSonIguales(1, 0).
noSonIguales(0, 1).

% esBin(?X)
% esBin vale si el parámetro es un dígito binario
esBin(0).
esBin(1).

% esBinL(?L)
% esBinL vale si la lista es una lista compuesta por dígitos binarios.
esBinL([]).
esBinL([X|XS]):- esBin(X), esBinL(XS).

desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

%------------------------------------------------------------------------------%
%                      Distancia de Hamming                                    %
%------------------------------------------------------------------------------%

distHam([], [], 0).
distHam([X|XS], [X|YS], D) :- esBin(X), distHam(XS, YS, D).
distHam([X|XS], [Y|YS], D) :- noSonIguales(X, Y), distHam(XS, YS, T),  D is T + 1.




%------------------------------------------------------------------------------%
%                      Distancia de Prefijos                                   %
%------------------------------------------------------------------------------%

% Si L1 es vacía, y la longitud de L2 es D, y L2 es una lista binaria,
% entonces vale distPref(L1, L2, D).
distPref([], XS, D) :- length(XS, D), esBinL(XS).

% Si L2 es vacía, y L1 es binaria y la longitud de L1 es D,
% entonces vale distPref(L1, L2, D).
distPref([X|XS], [], D) :- esBinL([X|XS]), length([X|XS], D).

% Si tengo instanciada D, las cabezas de L1 y L2 son digitos binarios distintos, y L1 es BIN
% Si A es la longitud de L1 - 1, y B es D - A - 2, y la longitud de YS es B e YS es una lista binaria,
% entonces vale distPref(L1, L2, D).

% La idea es que como ya tengo L1 y D instanciados, puedo saber cuál es la longitud de B,
% y por ende, toda lista binaria de esa longitud me sirve.
distPref([X|XS], [Y|YS], D) :- 	ground(D), noSonIguales(X, Y),
								esBinL(XS), length(XS, A),
								B is D - A - 2,
								length(YS, B), esBinL(YS).


% Si tengo instanciado D, y la cabeza de L1 y L2 es el mismo digito binario, y
% la distancia de prefijos entre el resto de la lista es D,
% entonces vale distPref(L1, L2, D).

% Si las cabezas son iguales es un prefijo,
% entonces D tiene que ser el mismo D que ignorandolas
distPref([X|XS], [X|YS], D) :-  ground(D), esBin(X), distPref(XS, YS, D).

% Si no tengo instanciado ni D ni L2, uso desde para instanciar todos los posibles D, y luego busco
% los L2 que puedan satisfacer distPref(L1, L2, D)
distPref([X|XS], [Y|YS], D) :- 	not(ground(D)), not(ground([Y|YS])),
                                desde(0,D), distPref([X|XS], [Y|YS], D).

% Si no tengo instanciado D pero SI L2, se que D tiene que estar entre
% 0 y la longitud de L1 + la longitud de l2, uso between para instanciar D
distPref([X|XS], [Y|YS], D) :-  not(ground(D)), ground([Y|YS]),
                                length([X|XS], L1), length([Y|YS], L2),
                                K is L1 + L2,
                                between(0, K, D), distPref([X|XS], [Y|YS], D).


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

%diffBit(A, B, K) devuelve true si K es |A-B|, y ademas A y B son binarios
diffBit(A, B, K) :- esBin(A), esBin(B), noSonIguales(A, B), K is 1.
diffBit(A, B, K) :- esBin(A), esBin(B), A=B, K is 0.

%distEd(+S,?T,?N)
distEd([],X,N) :- ground(N), N >= 0, length(X,N), esBinL(X). % si N esta instanciado chequeo que sea mayor o igual a cero, que la longitud de X sea N y que X tenga solo ceros y unos
distEd(X,[],N) :- ground(N), N > 0, length(X,N), esBinL(X). % lo mismo que en el caso anterior pero con N > 0 para evitar repeticiones
distEd([],X,N) :- not(ground(N)), length(X,N), esBinL(X). % si N no esta instanciado devuelvo true si X tiene longitud N y esta compuesto por ceros y unos
distEd(X,[],N) :- not(ground(N)), not(length(X,0)), length(X,N), esBinL(X). % igual que en el caso anterior pero con not(length(X,0)) para evitar repetidos

distEd([F|X],[G|Y],N) :- ground(N), esBin(F), esBin(G), atMostDiffLength(X,Y,N), distEd(X,Y,A), distEd(X,[G|Y],B), distEd([F|X],Y,C), diffBit(F,G,D), N is min(A+D,min(B+1,C+1)).
% cuando N esta instanciado chequeo que la diferencia de longitud entre X e Y sea a lo sumo N (para restringir el dominio de Y) y luego para cada
% Y que genero testeo con la dinamica del enunciado
distEd([F|X],[G|Y],N) :- not(ground(N)), esBin(F), esBin(G),distEd(X,Y,A), distEd(X,[G|Y],B), distEd([F|X],Y,C), diffBit(F,G,D), N is min(A+D,min(B+1,C+1)).
% cuadno N no esta instanciado hago lo mismo que en el caso anterior pero sin generar los Y posibles primero ya que hay infinitos Y posibles
% en el caso de que Y no este instanciado
