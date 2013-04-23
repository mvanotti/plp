module TP1 where

import Prelude
import Data.List

{-|
    El traductor se representa por:

     * un conjunto de estados

     * una funcion de transicion (primer parametro)

     * una funcion de output (segundo parametro)

     * un estado inicial.
-}
type Traductor q = (q -> Char -> q, q -> Char -> String, q)

{-| Traductor que cambia las letras a por e y viceversa.

    >>> aplicando cambiarAE "abcdefa123"
    "ebcdafe123"
-}
cambiarAE :: Traductor ()
cambiarAE = (const, g, ())
    where
        g :: () -> Char -> String
        g () 'a' = "e"
        g () 'e' = "a"
        g ()  x  = [x]

{-| Traductor que intercambia caracteres consecutivos.

    Como lo que se quiere es intercambiar las posiciones de a pares, nos
    interesa saber en que posición (par o impar) se encuentra el caracter
    leido.

    Si está en una posición impar, guardamos en el estado el
    caracter leido para ser escrito más tarde, y no escribimos nada. Si
    el caracter leido está en una posición par, escribimos el caracter
    actual y luego el caracter  que fue almacenado en el estado.

    >>> aplicando intercambiarConsecutivos "paradigmas"
    "aparidmgsa"
-}
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

{-| Traductor que sea la identidad, salvo que nunca genera
    salida output de las "a"s y, cuando aparece una "z",
    muestra la "z" y luego todas las "a"s que se acumularon
    juntas.

    >>> aplicando acumularAes "Esto acumula todas las aes hasta la z."
    "Esto cumul tods ls es hst l zaaaaaaaa."

    >>> aplicando acumularAes "Si pongo una a despues de la z no aparece."
    "Si pongo un  despues de l zaaa no prece."
-}
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

{- | Traductor que de vuelta (ie. espeje) todo lo que esta
     entre dos "a"s consecutivas.

    Usamos es el estado para ir guardando los caracteres que se van
    leyendo, que son distinto de 'a', y los guardamos en el estado de
    manera tal que queden ya espejados. Entonces, cada vez que se lee un
    caracter, si es distitno de 'a', no se escribe nada. Si es 'a',
    escribimos lo que estaba almacenado en el estado, seguido de 'a'. De
    esta forma, el resultado obtenido es el espejado de caracteres que
    están entre 'a'es.

    >>> aplicando espejarEntreAs "aeioua"
    "auoiea"

    >>> aplicando espejarEntreAs "espejo"
    ""

    >>> aplicando espejarEntreAs "espejoaeso"
    "ojepsea"

    >>> aplicando espejarEntreAs "aderecho"
    "a"
-}

espejarEntreAs :: Traductor String
espejarEntreAs = (f, g, [])
    where
        f :: String -> Char -> String
        f xs 'a' = []
        f xs  c  = c:xs
        --
        g :: String -> Char -> String
        g xs 'a' = xs ++ "a"
        g xs  c  = []


{- | Calcular la clausura reflexo-transitiva de la funcion de
    transicion pasada como parametro
    (version recursiva explicita).
-}
fAst' :: (q -> Char -> q) -> q -> String -> q
fAst' f q0    ""  = q0
fAst' f q0 (c:cs) = fAst' f (f q0 c) cs

{- | Calcular la clausura reflexo-transitiva de la funcion de
    transicion pasada como parametro
    (version con esquemas de recursion).

    La función "fAst" recibe una función de transición de estados como parámetro
    (función usada por un "Traductor"), un estado inicial (que coincide con el
    tipo de entrada de la función de transición), y un "String".

    "fAst" aplica sucesivamente la función recibida de izquierda a derecha sobre
    el "String" comenzando por el estado recibido como argumento. "fAst" va tomando
    elemento por elemento del "String" y realizando la transición de estados. Al finalizar
    obtenemos el último estado de la transición.

    Por lo descripto anteriormente, la función es equivalente a "foldl".

    >>> fAst (flip (:)) "" "1234"
    "4321"
    >>> foldl (flip (:)) "" "1234"
    "4321"

-}
fAst :: (q -> Char -> q) -> q -> String -> q
fAst = foldl

{- | Calcular la clausura de Kleene de la funcion de
    salida pasada como parametro junto con la funcion
    de transicion pasada como parametro
    (version recursiva explicita).
-}
gAst' :: (q -> Char -> q) -> (q -> Char -> String) ->
         q -> String -> String
gAst' f g q0    ""  = ""
gAst' f g q0 (c:cs) = (g q0 c) ++ gAst' f g (f q0 c) cs

{- | Calcular la clausura de Kleene de la funcion de salida
    pasada como parametro junto con la funcion de
    transicion pasada como parametro
    (version con esquemas de recursion).

    La función "gAst" recibe como parámetros:

    * Una función de transición de estados

    * Una función de traducción

    * Un estado inicial (que coincide con los estados recibidos por las funciones anteriores)

    * Un "String"

    Aplica la función de traducción de izquierda a derecha por todo el String, teniendo
    en cuenta los estados obtenidos tras aplicar la función de transición de estado.

    Cada caracter procesado retorna un nuevo string, que es concatenado con el resto.
-}

{-  La idea es: con scanl obtenemos la lista de los estados parciales al procesar
    cada caracter. Luego con zipWith armamos una lista que contiene el
    resultado de aplicar g a cada estado con el caracter correspondiente,
    obteniendo asi una lista con la traduccion parcial de cada caracter.
    Es decir, nos queda la lista de todos los strings que queremos tener.
    Finalmente hacemos un concat y unificamos todo.

    scanl deja todos los estados parciales que serían de aplicar la función fAst
    sobre cada prefijo del String.
    (Se puede ver como aplicar foldl a todos los prefijos de la lista).
-}
gAst :: (q -> Char -> q) -> (q -> Char -> String) -> q -> String -> String
gAst f g q0 xs = let estadosParciales = scanl f q0 xs in
		concat $ zipWith g estadosParciales xs

-- | Dado un traductor, retornar la funcion String -> String
-- que resulta al aplicarlo a cualquier entrada
aplicando :: Traductor q -> String -> String
aplicando (f, g, q) = gAst f g q

{- | Dados dos traductores, dar un traductor tal que la
   funcion String -> String que denota el resultado, sea
   justo la composicion de las funciones de cada
   uno de los parametros.

   >>> let t1 = comp acumularAes cambiarAE in aplicando t1 "maravavillaza!"
   "mrvvillzeeee!"

   >>> let t2 = comp cambiarAE acumularAes in aplicando t2 "maravillenze!"
   "merevillnza!"

   >>> let t3 = comp cambiarAE cambiarAE in aplicando t3 "esta es la identida"
   "esta es la identida"
-}

comp :: Traductor a -> Traductor b -> Traductor (a, b)
comp (f, g, e) (f', g', e') = (fc, gc, ec)
    where
        ec = (e, e')
        fc (q, q') c = (f q c, fAst f' q' (g q c))
        gc (q, q') c = gAst f' g' q' (g q c)


{- | Dado un traductor, dar la cadena infinita que resulta de
     procesar una lista infinita de "a"s (si se pide
     "take n (salidaAes t)" no puede procesar infinitamente
     para ningun "n")

     >>> take 10 (salidaAes cambiarAE)
     "eeeeeeeeee"

-}
salidaAes :: Traductor q -> String
salidaAes t = aplicando t $ repeat 'a'

-- | Decidir si es posible que el traductor dado de la salida
-- dada como segundo parametro
salidaPosible :: Traductor q -> String -> Bool
salidaPosible t1 w = any (\x -> aplicando t1 x == w) (takeWhile (\x -> length x <= length w) (kleeneStar "0123456789"))
    where
        kleeneStar :: [a] -> [[a]]
        kleeneStar ls = concat $ iterate agregarElementoATodas [[]]
            where
                agregarElementoATodas = \xs -> concat [map (l:) xs | l <- ls]



filtrarPalabras :: [(String, String)] -> Traductor String
filtrarPalabras xs = (f, g, "") where
                    f q c = if (elem c " .,!") then "" else q ++ [c]
                    g q c = if (elem c " .,!") then
                                if (elem q [fst s | s <- xs]) then
                                    head [snd s | s <- xs, fst s == q] ++ [c]
                                else
                                    q ++ [c]
                            else
                                ""

--Otra forma de resolver el ejercicio poría ser usando backtracking
--comparando que los prefijos coincidan con los prefijos de la
--lista que nos pasan por parámetro (W)
gen :: Traductor q -> String -> Bool
gen (f, g, q) w = if null w then
                        True
                  else
                    let prefijos = filter ((flip isPrefixOf w) . fst) (map (\x -> (g q x, x)) "0123456789") in
                        if null prefijos then
                            False
                        else
                            or (map (\x -> gen (f, g, f q (snd x) ) (w \\ (fst x))) prefijos)
-- Amor y Paz


-- Traduce español a un lenguaje secreto
ftoger :: () -> Char -> ()
ftoger _ _ = ()

gtoger :: () -> Char -> String
gtoger _ 'a' = "apa"
gtoger _ 'e' = "epe"
gtoger _ 'i' = "ipi"
gtoger _ 'o' = "opo"
gtoger _ 'u' = "upu"
gtoger _ 'A' = "Apa"
gtoger _ 'E' = "Epe"
gtoger _ 'I' = "Ipi"
gtoger _ 'O' = "Opo"
gtoger _ 'U' = "Upu"
gtoger _  c = c:[]

toger = (ftoger, gtoger, ())
