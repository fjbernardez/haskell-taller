module Clase
where
  {-
    este modulo son los ejercicios de la clase 5
  -}

  -- |Documentacion de sumaDivisoresHasta
  -- sumaDivisoresHasta devuelve la suma de los divisores de un numero hasta cierto punto
  sumaDivisoresHasta :: Int -> Int -> Int
  sumaDivisoresHasta n k  | k == 1 = k
                          | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                          | otherwise = sumaDivisoresHasta n (k-1)

  -- |Documentacion de sumaDivisores
  -- sumaDivisores calcula la suma de los divisores un entero positivo
  sumaDivisores :: Int -> Int
  sumaDivisores n = sumaDivisoresHasta n n


  -- |Documentacion de menorDivisorDesde
  -- menorDivisorDesde devuelve el menor divisor de un numero entero n, comenzando desde un natural k
  menorDivisorDesde :: Int -> Int -> Int
  menorDivisorDesde n k   | k > n = 0
                          | k == 1 || k == n  = k
                          | mod n k == 0 = k
                          | otherwise = menorDivisorDesde n (k+1)

  -- |Documentacion de menorDivisor
  -- menorDivisor calcula el menor divisor (mayor que 1) de un natural n
  menorDivisor :: Int -> Int
  menorDivisor n = menorDivisorDesde n 2

  -- |Documentacion de esPrimo
  -- esPrimo indica si el numero natural pasado como argumento es primo
  esPrimo :: Int -> Bool
  esPrimo n = menorDivisor n == n

  -- |Documentacion de nEsimoPrimo
  -- nEsimoPrimo devuelve el n-esimo
  -- "El nEsimo primo es el primer primo DESDE el numero primo anterior primo anterior"
  nEsimoPrimo :: Int -> Int
  nEsimoPrimo 1 = 2 -- caso base de los numeros primos, el 2
  nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n -1)) -- numero primo minimo desde el nEsimo anterior

  -- |Documentacion de minimoPrimoDesde
  -- minimoPrimoDesde busca el primer primo desde un numero dado
  minimoPrimoDesde :: Int -> Int
  minimoPrimoDesde n  | esPrimo n = n
                      | otherwise = minimoPrimoDesde (n + 1)
