module TP1 where

import Prelude
import Data.List
import Data.Maybe

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

    >>> aplicando espejarEntreAs "espejoa"
    "ojepsea"

    >>> aplicando espejarEntreAs "espejoaesoa"
    "ojepseaosea"

    >>> aplicando espejarEntreAs "aderechoa"
    "aohcereda"
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

    >>> let fst (a, b, c) = a in fAst (fst espejarEntreAs) "" "asd"
    "ds"

    >>> let fst (a, b, c) = a in fAst (fst espejarEntreAs) "" "asda"
    ""

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

    >>> let fst (a, b, c) = a in let snd (a, b, c) = b in gAst (fst espejarEntreAs) (snd espejarEntreAs) "" "aaabaabcacddda"
    "aaabaacbadddca"

    >>> let fst (a, b, c) = a in let snd (a, b, c) = b in gAst (fst espejarEntreAs) (snd espejarEntreAs) "amipalabra" "aabca"
    "amipalabraaacba"
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
        --fc  es el estado resultante de aplicar los dos traductores.
        --Es una tupla que en la primer componente devuelve el estado
        --despues de aplicar el Traductor 'a' a un caracter c, y en la segunda componente 
        --devuelve el estado reusltante de aplicar el Traductor 'b', al string resultante de 
        --aplicar el Traductor a, al caracter c
        fc (q, q') c = (f q c, fAst f' q' (g q c))
        --gc es el string resultante de haber aplicado los traductores al caracter c.
        --Aplica el Traductor 'a' al caracter c, y luego obtiene, a partir de 
        --gAst y la funcion de transicion de estados del Traductor 'b', la
        --traduccion completa del string resultante de haber aplicado el 
        --traductor 'a' al caracter c, es decir la traduccion de la composicion.
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

{- | La función "salisaPosible" recibe como parámetros:

    * Un traductor

    * Una "String" de tipo numerico

    Compara si existe una cadena tal queal traducirla se obtenga el string
    pasado como parametro.

    Genera las cadenasde string numericos en orden creciente en la longitud
    de la cadena y se fija si la traduccion obtenida es prefijo del string
    recibido por parametro.

    >>> salidaPosible cambiarAE "456"
    True

    >>> salidaPosible cambiarAE "456e7"
    False

    Como "cambiarAE" no genera ningun cambio en un srting numerico, "salidaPosible"
    siempre devolvera "True"
-}

{-  la idea general es la siguiente: probar aplicale al traductor
	todas las cadenas de numeros posibles y fijarnos si el resultado de
	alguna es igual al string pasado como segundo parametro (w).
	Para genera todas las cadenas posibles de numeros usamos la
	clausura de kleene de 0.9.
	Como sabemos la longitud de w, solo miramos los elemenos que al
	traducirlos tienen longitud menor o igual a la longitud de w.
 -}
salidaPosible :: Traductor q -> String -> Bool
--De todas las cadenas que se pueden generar a partir de 0..9, se toman las que tienen longitud menor o igual a la longitude de w.
--Si la traduccion de alguna de ellas es igual a w, retorna True, sino False.
salidaPosible t1 w = any (\x -> aplicando t1 x == w) (takeWhile (\x -> length x <= length w) (kleeneStar "0123456789"))
    where
    	--dada una lista, genera todas las listas posibles con los elementos de la lista
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

{-	Otra forma de resolver el ejercicio poría ser usando backtracking
	comparando que los prefijos coincidan con los prefijos de la
	lista que nos pasan por parámetro (W)
-}
gen :: Traductor q -> String -> Bool
gen (f, g, q) w = if null w then
                        True
                  else
                    let prefijos = filter ((flip isPrefixOf w) . fst) (map (\x -> (g q x, x)) "0123456789") in
                        if null prefijos then
                            False
                        else
                            or (map (\x -> gen (f, g, f q (snd x) ) (w \\ (fst x))) prefijos)

{-| pasarEntreBases toma dos Strings que representen sistemas de numeración
    posicionales de enteros y retorna un traductor que convierte cualquier substring de la base
    origen en un String de la base destino.

    >>> let t = pasarEntreBases "0123456789" "01" in aplicando t "5."
    "101."

    >>> let t = pasarEntreBases "0123456789" "abcdefghij" in aplicando t "101."
    "bab."
-}
pasarEntreBases :: String -> String -> Traductor String
pasarEntreBases base destino = (f, g, "") where
                            f q c = if not (elem c base) then
                                        ""
                                    else
                                        q ++ [c]
                            g q c = if not (elem c base) then
                                        pasarABase destino (pasarAEntero base q) ++ [c]
                                    else
                                        ""
{- | Dada un sistema de numeracion posicional de enteros y un entero, retorna un
    String que consiste en representar ese entero en la base dada.

    >>> pasarABase "01" 5
    "101"
-}
pasarABase :: String -> Integer -> String
pasarABase base numero =
    -- divisionesParciales tiene la lista de los resultados de dividir al numero por la base.
    let divisionesParciales = takeWhile (>0) $ iterate (flip div (toInteger $ length base)) numero in
        -- restos tiene los restos de ir dividiendo el número por la base.
        let restos = map (flip mod (toInteger $ length base)) $ divisionesParciales in
            reverse [ base!!(fromInteger x) | x <- restos]

{- | Dado a una base y un número expresado en esa base, se lo convierte a enter

    >>> pasarAEntero "0123456789abcdef" "fe0"
    4064
-}
pasarAEntero :: String -> String -> Integer
pasarAEntero base numero =
    -- indices son los indices dentro de la base de cada caracter
    let indices = [toInteger $ fromJust $ elemIndex c base | c <- numero] in
        -- potencias es la base elevada a la posición de cada elemento del numero
        let potencias base k = [(toInteger base) ^ x | x <- reverse.(take k) $ [0..]] in
            sum $ zipWith (*) indices (potencias (length base) (length numero))

-- Amor y Paz


-- | Funcion que cambia de estados secretamente
ftoger :: () -> Char -> ()
ftoger _ _ = ()

-- | Función que traduce secretamente
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
gtoger _ '0' = "0p0"
gtoger _ '1' = "1p1"
gtoger _ '2' = "2p2"
gtoger _ '3' = "3p3"
gtoger _ '4' = "4p4"
gtoger _ '5' = "5p5"
gtoger _ '6' = "6p6"
gtoger _ '7' = "7p8"
gtoger _ '8' = "8p8"
gtoger _ '9' = "9p9"
gtoger _  c = c:[]

-- | Traduce español a un lenguaje secreto
toger = (ftoger, gtoger, ())
