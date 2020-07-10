module ClaseAdicional
where
  import Clase05
  {-
    este modulo son los ejercicios de la clase adicional
  -}

  {- funciones principales: -}

  -- |Documentacion de longitud -- Ej 1
  -- longitud dado un numero natural n, devuelve la longitud de la lista codificada por n
  longitud :: Integer -> Integer
  longitud n = longitudDesde n 2

  -- |Documentacion de iesimo -- Ej 2
  -- iesimo dados dos numeros naturales n e i, devuelve el iesimo elemento de la lista que codifica n
  iesimo :: Integer -> Integer -> Integer
  iesimo n i = darPotencia n (nEsimoPrimo i)

  -- |Documentacion de  headN -- Ej 3
  --  headN dado un numero natural n, devuelve la cabeza de la lista codificada por n
  headN :: Integer -> Integer
  headN n = darPotencia n (nEsimoPrimo 1)

  -- |Documentacion de  tailN -- Ej 4
  -- tailN dado un numero natural n, devuelve el numero que codifica a la cola de la lista que codifica n
  -- Esta resuelta con implementacion de funciones posteriores.
  tailN :: Integer -> Integer
  tailN n = godel (tail (codificarALista n))

  -- |Documentacion de  codificarALista -- Ej 5
  --  codificarALista dado un numero natural n, devuelve la lista codificada por n
  codificarALista :: Integer -> [Integer]
  codificarALista n = codificarALista' n (longitud n) 1

  -- |Documentacion de  godel -- Ej 6
  -- godel que dada una lista , devuelve el numero de Gôdel de la misma
  godel :: [Integer] -> Integer
  godel lista = godel' lista 1

  {- funciones auxiliares: -}

  -- |Documentacion de longitudDesde
  -- longitudDesde añade un parametro a longitud, que es un "contador de primos"
  longitudDesde :: Integer -> Integer -> Integer
  longitudDesde 1 _ = 1
  longitudDesde n k | (mod n k) == 0 = longitudDesde (div n k) k
                    | otherwise = 1 + longitudDesde n (proximoPrimo k)

  -- |Documentacion de darPotencia
  -- darPotencia indica la potencia maxima del argumento i que divide al argumento n
  darPotencia :: Integer -> Integer -> Integer
  darPotencia n i | (mod n i) == 0 = 1 + darPotencia (div n i) i
                  | otherwise = 0


  -- |Documentacion de  codificarALista'
  --  codificarALista' añade un "contador de primos" y la longitud del entero para formar la lista
  codificarALista' :: Integer -> Integer -> Integer -> [Integer]
  codificarALista' n l k  | l == 0 = []
                          | otherwise = (darPotencia n primo) : codificarALista' n (pred l) (succ k)
                          where primo = nEsimoPrimo k

  -- |Documentacion de  godel'
  -- godel' añade un "contador de primos"
  godel' :: [Integer] -> Integer -> Integer
  godel' [] _ = 1 -- neutro multiplicativo
  godel' (x:xs) k = ( (nEsimoPrimo k) ^ x ) * (godel' xs (succ k))
