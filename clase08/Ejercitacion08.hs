module Ejercitacion08
where
  import Clase08
  import Conjuntos

  {-
    este modulo son los ejercicios de la clase 8
  -}

  {- funciones principales: -}

  -- |Documentacion de bolitasEnCajas -- Ejercicio 1
  -- bolitasEnCajas devuelve formas de ubicar n bolitas numeradas en k cajas
  bolitasEnCajas :: (Eq n, Enum n, Integral n) => n -> n -> Set (Set n)
  bolitasEnCajas n k = variaciones [1..k] n

  -- |Documentacion de bolitasEnCajasPrimeraCajaNoVacia -- Ejercicio 2
  -- bolitasEnCajasPrimeraCajaNoVacia devuelve formas de ubicar n bolitas numeradas en k cajas con la primera caja no vacia
  bolitasEnCajasPrimeraCajaNoVacia :: (Eq n, Enum n, Integral n) => n -> n -> Set (Set n)
  bolitasEnCajasPrimeraCajaNoVacia n k = bolitasEnCajasPrimeraCajaNoVacia' (bolitasEnCajas n k) 1

  -- |Documentacion de listasOrdenadasDeElementosNoRepetidos -- Ejercicio 3
  -- listasOrdenadasDeElementosNoRepetidos devuelve todas las listas ordenadas de k numeros distintos tomados del conjunto {1, . . . , n}.
  listasOrdenadasDeElementosNoRepetidos :: (Eq n, Enum n, Integral n) => n -> Set n -> Set (Set n)
  listasOrdenadasDeElementosNoRepetidos k conjunto = filtrarRepetidos ( permutarTodas (variaciones conjunto k) )

  -- |Documentacion de sucesionesAB -- Ejercicio 4
  -- sucesionesAB devuelve todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m respectivamente
  sucesionesAB :: Char -> Char -> Int -> Int -> [[Char]]
  sucesionesAB a b n m = anagramasDe ( generaCadena a n ++ generaCadena b m )

  -- |Documentacion de sucesionesABC -- Ejercicio 5
  -- sucesionesABC devuelve todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m respectivamente
  sucesionesABC :: Char -> Char -> Char -> Int -> Int -> Int -> [[Char]]
  sucesionesABC a b c n m k = anagramasDe ( generaCadena a n ++ generaCadena b m ++ generaCadena c k )

  -- |Documentacion de subconjuntos -- Ejercicio 6
  -- subconjuntos  dados un conjunto de enteros y un entero k, genera todos los subconjuntos de k elementos del conjunto pasado por parametro
  subconjuntos :: (Eq n, Enum n, Integral n) => Set n -> n -> Set (Set n) -- puede ser mas general
  subconjuntos conjunto n = filtrarPorCardinal (partes conjunto) n

  {- funciones auxiliares: -}

  -- |Documentacion de bolitasEnCajasPrimeraCajaNoVacia'
  -- bolitasEnCajasPrimeraCajaNoVacia' es auxiliar de bolitasEnCajasPrimeraCajaNoVacia. Quita los conjuntos que no contiene a k
  bolitasEnCajasPrimeraCajaNoVacia' :: (Eq n, Enum n, Integral n) => Set (Set n) -> n -> Set (Set n)
  bolitasEnCajasPrimeraCajaNoVacia' [] _ = []
  bolitasEnCajasPrimeraCajaNoVacia' (xs:xss) cajaNoVacia  | pertenece cajaNoVacia xs = xs : bolitasEnCajasPrimeraCajaNoVacia' xss cajaNoVacia
                                                          | otherwise = bolitasEnCajasPrimeraCajaNoVacia' xss cajaNoVacia

  -- |Documentacion de permutarTodas
  -- permutarTodas dado un Set de Set, retorna la union de todas las permutaciones de cada Set
  permutarTodas :: (Eq n, Enum n, Integral n) => Set (Set n) -> Set (Set n)
  permutarTodas [] = []
  permutarTodas (xs:xss) = union (permutaciones xs) (permutarTodas xss)

  -- |Documentacion de filtrarRepetidos
  -- filtrarRepetidos dado un Set de Set, retorna la union de todos aquellos Set que no tiene elementos repetidos
  filtrarRepetidos :: (Eq n, Enum n, Integral n) => Set (Set n) -> Set (Set n)
  filtrarRepetidos [] = []
  filtrarRepetidos (xs:xss) | listaDeElementosUnicos xs = union [xs] (filtrarRepetidos xss)
                            | otherwise = filtrarRepetidos xss

  -- |Documentacion de listaDeElementosUnicos
  -- listaDeElementosUnicos dado un Set indica si cada elemento es unico
  listaDeElementosUnicos :: (Eq n, Enum n, Integral n) => Set n -> Bool
  listaDeElementosUnicos [] = True
  listaDeElementosUnicos (x:xs) = not (pertenece x xs) && listaDeElementosUnicos xs

  -- |Documentacion de generaCadena
  -- generaCadena una cadena formada con los dos carcteres pasados como parametro, tantas veces como m y n indiquen.
  generaCadena :: Char -> Int -> String
  generaCadena _ 0 = []
  generaCadena c n = c : generaCadena c (pred n)

  -- |Documentacion de filtrarPorCardinal
  -- filtrarPorCardinal dado un Set de Set retorna la union de los Set que tiene el carnidal n indicado.
  filtrarPorCardinal :: (Eq n, Enum n, Integral n) => Set (Set n) -> n -> Set (Set n)
  filtrarPorCardinal [] _ = []
  filtrarPorCardinal (xs:xss) n   | cardinal xs == n = union [xs] (filtrarPorCardinal xss n)
                                  | otherwise = filtrarPorCardinal xss n

  -- |Documentacion de agregarCaracterEnCadaPosicion
  -- agregarCaracterEnCadaPosicion añade un caracter en cada posicion de la cadena
  agregarCaracterEnCadaPosicion :: String -> Char -> Int -> Set String
  agregarCaracterEnCadaPosicion cadena caracter 0   = [insertarCaracterEn cadena caracter 1]
  agregarCaracterEnCadaPosicion cadena caracter len = union [insertarCaracterEn cadena caracter (succ len)] (agregarCaracterEnCadaPosicion cadena caracter (pred len))

  -- |Documentacion de insertarCaracterEn
  -- insertarCaracterEn añade un caracter en cada posicion de la cadena
  insertarCaracterEn :: Set Char -> Char -> Int -> Set Char
  insertarCaracterEn conjunto n p | p == 1 = n : conjunto
                                  | otherwise = (head conjunto) : insertarCaracterEn (tail conjunto) n (pred p)

  -- |Documentacion de agregarCaracterEnCadaPosicionDeCadaConjunto
  -- agregarCaracterEnCadaPosicionDeCadaConjunto añade un caracter en cada posicion de la cadena de cada conjunto
  agregarCaracterEnCadaPosicionDeCadaConjunto :: Set String -> Char -> Set String
  agregarCaracterEnCadaPosicionDeCadaConjunto [] _ = []
  agregarCaracterEnCadaPosicionDeCadaConjunto (xs:xss) c = union (agregarCaracterEnCadaPosicion xs c (length xs)) (agregarCaracterEnCadaPosicionDeCadaConjunto xss c)

  -- |Documentacion de anagramasDe
  --anagramasDe genera un Set con todos los posibles anagramas del String dado
  anagramasDe :: String -> Set (String)
  anagramasDe cadena = generaAnagramas ([[head cadena]]) (tail cadena)

  -- |Documentacion de generaAnagramas
  -- generaAnagramas implemtacion auxiliar para la funcion anagramasDe
  generaAnagramas :: Set String -> String -> Set String
  generaAnagramas conjunto [] = conjunto
  generaAnagramas conjunto cadena  = generaAnagramas (agregarCaracterEnCadaPosicionDeCadaConjunto conjunto (head cadena)) (tail cadena)

















--
