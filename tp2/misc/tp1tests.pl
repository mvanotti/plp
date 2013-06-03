:- begin_tests(tp1).


# Tests!
# Test de distHam distancias no instanciadas


distHam([1], [1], 0).
distHam([0], [1], 1).
distHam([1], [0], 1).

\+ distHam([], [0], N).
\+ distHam([plp], [plp], N).
distHam([0,1,0], [0,0,1], N), N is 2.
bagof(C, distHam([0,1,0], C, 2), Cs), sort(Cs, X), sort([[0,0,1], [1,1,1], [1, 0, 0]], Y), X = Y.

:- end_tests(tp1).