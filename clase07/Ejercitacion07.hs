module Ejercitacion07
where
  import Clase07
  {-
    este modulo son los ejercicios de la clase 7
  -}

  {- funciones principales: -}

  -- |Documentacion de cardinal
  -- cardinal devuelve el cardinal del conjunto
  cardinal :: (Num a) => Set a -> a
  cardinal [] = 0
  cardinal (x:xs) = 1 + cardinal xs

  -- |Documentacion de union
  -- union devuelve la union de los conjuntos
  union :: (Eq a) => Set a -> Set a -> Set a
  union [] conjunto = conjunto
  union (x:xs) conjunto = agregar x (union xs conjunto)

  -- |Documentacion de interseccion
  -- interseccion dado dos conjuntos, devuelve la interseccion entre ellos
  interseccion :: (Eq a) => Set a -> Set a -> Set a
  interseccion [] _ = []
  interseccion (x:xs) conjunto  | pertenece x conjunto = agregar x (interseccion xs conjunto)
                                | otherwise = interseccion xs conjunto

  -- |Documentacion de diferencia
  -- diferencia dado los conjuntos A y B, devuelve A \ B
  diferencia :: (Eq a) => Set a -> Set a -> Set a
  diferencia [] _ = []
  diferencia (x:xs) conjunto  | not (pertenece x conjunto) = agregar x (diferencia xs conjunto)
                              | otherwise = diferencia xs conjunto

  -- |Documentacion de diferenciaSimetrica
  -- diferenciaSimetrica dado los conjuntos A y B, devuelve la diferencia simétrica entre ellos.
  diferenciaSimetrica :: (Eq a) => Set a -> Set a -> Set a
  diferenciaSimetrica conjuntoA conjuntoB = diferencia (union conjuntoA conjuntoB) (interseccion conjuntoA conjuntoB)

  -- |Documentacion de partes
  -- partes dado un conjunto retorna el conjunto de partes del mismo.
  partes :: (Eq a) => Set a -> Set (Set a)
  partes [] = [[]]
  partes (x:xs) = union (partes xs) (agregarATodos x (partes xs))

  -- |Documentacion de partesN
  -- partesN genera los subconjuntos del conjunto {1, 2, 3, . . . , n}.
  partesN :: (Eq a, Enum a, Num a) => a -> Set (Set a)
  partesN n = partes (generaConjuntoN n)

  -- |Documentacion de productoCartesiano
  -- productoCartesiano dados dos conjuntos genera todos los pares posibles
  productoCartesiano :: (Eq a, Enum a, Num a) => Set a -> Set a -> Set (a, a)
  -- productoCartesiano :: (Eq a, Enum a, Num a) => Set a -> Set a -> Set (Set a)
  productoCartesiano [] _ = []
  productoCartesiano (x:xs) (y:ys) = union (darPares x (y:ys)) (productoCartesiano xs (y:ys))


  {- funciones auxiliares: -}

  -- |Documentacion de generaConjuntoN
  -- generaConjuntoN dado un N, genera el conjunto {1, 2, 3, . . . , n}.
  generaConjuntoN :: (Eq a, Enum a, Num a) => a -> Set a
  generaConjuntoN 0 = []
  generaConjuntoN n = agregar n (generaConjuntoN (pred n) )

  -- |Documentacion de darPares
  -- darPares genera un conjunto de pares ordenados con un conjunto de pares pasados como parametro
  darPares :: (Eq a, Enum a, Num a) => a -> Set a -> Set (a,a)
  darPares n [] = []
  darPares n (x:xs) = union [(n,x)] (darPares n xs)
