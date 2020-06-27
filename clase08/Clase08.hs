module Clase08
where
  import Conjuntos

  {-
    este modulo corresponde a la clase 8
  -}

  {- funciones principales: -}

  -- |Documentacion de combinatorio
  -- combinatorio devuelve el numero nCk combinatorio
  combinatorio :: (Eq n, Enum n, Integral n) => n -> n -> n
  combinatorio n k = div (factorial n) ( (factorial k) * (factorial (n - k) ) )

  -- |Documentacion de combinatorio'
  -- combinatorio devuelve el numero nCk combinatorio
  combinatorio' :: (Eq n, Enum n, Integral n) => n -> n -> n
  combinatorio' _ 0 = 1
  combinatorio' n k | n == k = 1
                    | otherwise = combinatorio' (pred n) k + combinatorio (pred n) (pred k)

  -- |Documentacion de variaciones
  -- variaciones dado un conjunto c y una longitud k genera todas las posibles listas de longitud k a partir de elementos de c. Con repeticion
  variaciones :: (Eq k, Enum k, Integral k) => Set k -> k -> Set (Set k)
  variaciones conjunto 0 = [[]]
  variaciones conjunto k = agregarTodosEnLasListas conjunto (variaciones conjunto (pred k))

  -- |Documentacion de insertarEn
  -- insertarEn dados una lista l, un numero n y una posicion i (contando desde 1) devuelva una lista en donde se inserto n en la posicion i de l y los elementos siguientes corridos en una posicion
  insertarEn :: (Eq n, Enum n, Integral n) => Set n -> n -> n -> Set n
  insertarEn conjunto n p | p == 1 = n : conjunto
                          | otherwise = (head conjunto) : insertarEn (tail conjunto) n (pred p)

  -- |Documentacion de permutaciones
  -- permutaciones devuelve un Set de Set que contienen las posibles permutaciones
  permutaciones :: (Eq n, Enum n, Integral n) => Set n -> Set (Set n)
  -- permutaciones :: (Eq n, Enum n) => Set n -> Set (Set n)
  permutaciones [] = [[]]
  permutaciones (x:xs) = agregarEnCadaListaEnCadaPosicion (permutaciones xs) x

  {- funciones auxiliares: -}

  -- |Documentacion de factorial
  -- factorial devuelve el factorial
  factorial :: (Eq n, Enum n, Integral n) => n -> n
  factorial 0 = 1
  factorial n = n * factorial (pred n)

  -- |Documentacion de agregarTodosEnLasListas
  -- agregarTodosEnLasListas añade todos los elementos del Set al Set de Set
  agregarTodosEnLasListas :: (Eq a) => Set a -> Set (Set a) -> Set (Set a)
  agregarTodosEnLasListas [] _ = []
  agregarTodosEnLasListas (x:xs) conjunto = union (agregarEnTodos conjunto x) (agregarTodosEnLasListas xs conjunto)

  -- |Documentacion de agregarEnTodos
  -- agregarEnTodos agrega a cada integrante del set, el elemeto indicado
  agregarEnTodos :: (Eq a) => Set (Set a) -> a -> Set (Set a)
  agregarEnTodos [] _ = []
  agregarEnTodos (xs:xss) k = (k : xs) : agregarEnTodos xss k

  -- | Documentacion agregarEnCadaListaEnCadaPosicion
  -- agregarEnCadaListaEnCadaPosicion agrega un valor numero dentro de cada conjunto dentro de un set, en cada posicion
  agregarEnCadaListaEnCadaPosicion :: (Eq a, Integral a) => Set (Set a) -> a -> Set (Set a)
  agregarEnCadaListaEnCadaPosicion [] _ = []
  agregarEnCadaListaEnCadaPosicion (xs:xss) n = union (agregarEnCadaPosicion xs n (cardinal xs + 1)) (agregarEnCadaListaEnCadaPosicion xss n)

  -- | Documentacion agregarEnCadaPosicion
  -- agregarEnCadaPosicion agrega un valor en cada posicion de un set
  agregarEnCadaPosicion :: (Eq a, Num a, Integral a) => Set a -> a -> a -> Set (Set a)
  agregarEnCadaPosicion xs n 1 = (insertarEn xs n 1) : vacio
  agregarEnCadaPosicion xs n p = (insertarEn xs n p) : (agregarEnCadaPosicion xs n (pred p))
