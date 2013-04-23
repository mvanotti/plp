import Prelude

-- El traductor se representa por un conjunto de estados "q",
--   - una funcion de transicion (primer parametro),
--   - una funcion de output (segundo parametro) y
--   - un estado inicial.
type Traductor q = (q -> Char -> q, q -> Char -> String, q)

-- Traductor que cambia las "a"s por "e"s y viceversa.
cambiarAE :: Traductor ()
cambiarAE = (const, g, ())
  where
    g :: () -> Char -> String
      g () 'a' = "e"
      g () 'e' = "a"
      g ()  x  = [x]

-- Traductor que intercambia caracteres consecutivos.
-- EJERCICIO

-- Como lo que se quiere es intercambiar las posiciones de a pares, nos 
-- interesa saber en que posición (par o impar) se encuentra el caracter
-- leido. Si está en una posición impar, guardamos en el estado el 
-- caracter leido para ser escrito más tarde, y no escribimos nada. Si 
-- el caracter leido está en una posición par, escribimos el caracter
-- actual y luego el caracter  que fue almacenado en el estado.

intercambiarConsecutivos :: Traductor (Maybe Char)
intercambiarConsecutivos = (f, g, Nothing) 
  where
	f :: Maybe Char -> Char -> Maybe Char
	f  Nothing c = Just c
    f (Just k) c = Nothing
    --
    g :: Maybe Char -> Char -> String
    g Nothing c = ""
    g (Just k) c = [c, k]

-- Traductor que sea la identidad, salvo que nunca genera
-- salida output de las "a"s y, cuando aparece una "z",
-- muestra la "z" y luego todas las "a"s que se acumularon
-- juntas.
acumularAes :: Traductor Int
acumularAes = (f, g, 0)
  where
    f :: Int -> Char -> Int
    f x 'a' = x + 1
    f x 'z' = 0
    f x  c  = x
    --
    g :: Int -> Char -> String
    g x 'z' = 'z':(replicate x 'a')
    g x 'a' = [ ]
    g x  c  = [c]

-- Traductor que de vuelta (ie. espeje) todo lo que esta
-- entre dos "a"s consecutivas.
-- EJERCICIO

-- Usamos es el estado para ir guardando los caracteres que se van 
-- leyendo, que son distinto de 'a', y los guardamos en el estado de 
-- manera tal que queden ya espejados. Entonces, cada vez que se lee un 
-- caracter, si es distitno de 'a', no se escribe nada. Si es 'a', 
-- escribimos lo que estaba almacenado en el estado, seguido de 'a'. De 
-- esta forma, el resultado obtenido es el espejado de caracteres que 
-- están entre 'a'es.
espejarEntreAs :: Traductor String
espejarEntreAs = (f, g, [])
  where
	f :: String -> Char -> String
	f xs 'a' = []
	f xs  c  = c:xs
	--
	g :: String -> Char -> String
	g xs 'a' = xs ++ 'a'
	g xs  c  = []


-- Calcular la clausura de Kleene de la funcion de
-- transicion pasada como parametro
-- (version recursiva explicita).
fAst' :: (q -> Char -> q) -> q -> String -> q
fAst' f q0    ""  = q0
fAst' f q0 (c:cs) = fAst' f (f q0 c) cs

-- Calcular la clausura de Kleene de la funcion de
-- transicion pasada como parametro
-- (version con esquemas de recursion).
-- EJERCICIO

-- Lo que queremos es aplicar sucesivas veces la f que recibe un
-- traductor, es decir, queremos obtener el estado final luego de haber
-- procesado el string. Ademas, queremos que la f sea aplicada en el 
-- sentido en el que se lee el string (de izquierda a derecha). Esto 
-- mismo es lo que hace la funcion foldl 
fAst :: (q -> Char -> q) -> q -> String -> q
fAst = foldl

-- Calcular la clausura de Kleene de la funcion de
-- salida pasada como parametro junto con la funcion
-- de transicion pasada como parametro
-- (version recursiva explicita).
gAst' :: (q -> Char -> q) -> (q -> Char -> String) ->
         q -> String -> String
gAst' f g q0    ""  = ""
gAst' f g q0 (c:cs) = (g q0 c) ++ gAst' f g (f q0 c) cs

-- Calcular la clausura de Kleene de la funcion de salida
-- pasada como parametro junto con la funcion de
-- transicion pasada como parametro
-- (version con esquemas de recursion).
-- EJERCICIO

-- La idea es: con scanl obtenemos la lista de los estados al procesar 
-- cada caracter. Luego con zipWith armamos uan lista que contiene el 
-- resultado de aplicar g a cada estado con el caracter correspondiente, 
-- obteniendo asi una lista con la traduccion parcial de cada caracter.
-- Es decir, nos queda la lista de todos los strings que queremos tener.
-- Finalmente hacemos un concat y unificamos todo.

gAst :: (q -> Char -> q) -> (q -> Char -> String) ->
        q -> String -> String
gAst f g q0 xs = let estadosParciales = scanl f q0 xs in
		concat $ zipWith g estadosParciales xs

-- Dado un traductor, retornar la funcion String -> String
-- que resulta al aplicarlo a cualquier entrada
aplicando :: Traductor q -> String -> String
aplicando (f, g, q) = gAst f g q

-- Dados dos traductores, dar un traductor tal que la
-- funcion String -> String que denota el resultado, sea
-- justo la composicion de las funciones de cada
-- uno de los parametros.
-- EJERCICIO


comp :: Traductor a -> Traductor b -> Traductor (a, b)
comp (f, g, e) (f', g', e') = (fc, gc, ec) 
  where
	ec = (e, e')
    fc (q, q') c = (f q c, fAst f' q' (g q c))
    gc (q, q') c = gAst f' g' q' (g q c)


-- Dado un traductor, dar la cadena infinita que resulta de
-- procesar una lista infinita de "a"s (si se pide
-- "take n (salidaAes t)" no puede procesar infinitamente
-- para ningun "n")
-- EJERCICIO

salidaAes :: Traductor q -> String
salidaAes t = aplicando t xs
    where
        xs = ['a', 'a'..]

-- Decidir si es posible que el traductor dado de la salida
-- dada como segundo parametro

-- EJERCICIO
salidaPosible :: Traductor q -> String -> Bool
salidaPosible t1 w = any (\x -> aplicando t1 x == w) (takeUntil (\x -> length x > length w) (kleeneStar "0123456789")))
    where
        kleeneStar :: [a] -> [[a]]
        kleeneStar ls = concat $ iterate agregarElementoATodas [[]] 
            where
                agregarElementoATodas = \xs -> concat [map (l:) xs | l <- ls]
                
--Otra forma de resolver el ejercicio poría ser usando backtracking 
--comparando que los prefijos coincidan con los prefijos de la 
--lista que nos pasan por parámetro (W)