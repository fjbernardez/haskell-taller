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


  -- |Documentacion de factorial
  -- factorial calcula el factorial del argumento
  factorial :: Int -> Int
  factorial 0 = 1
  factorial n = n * factorial (n -1)

  -- |Documentacion de menorFactDesde
  -- menorFactDesde busca el primer numero DESDE m que se correspodnde con algun k!, k pertenece a los naturales
  menorFactDesde :: Int -> Int
  menorFactDesde m = menorFactDesdeAux 1 m

  -- |Documentacion de menorFactDesdeAux
  -- menorFactDesdeAux busca el primer numero DESDE index que se corresponda con algun k!, k pertenece a los naturales
  menorFactDesdeAux :: Int -> Int -> Int
  menorFactDesdeAux index cota  | (factorial index >= cota) = factorial index
                                | otherwise = menorFactDesdeAux (index+1) cota


  -- |Documentacion de esFactorial
  -- esFactorial indica si un numero es resutado de calcular el factorial de k, para k natural
  esFactorial :: Int -> Bool
  esFactorial n = esFactorialAux n 1


  -- |Documentacion de esFactorialAux
  -- esFactorialAux indica si un numero es resutado de calcular el factorial de k >= index con k natural
  esFactorialAux :: Int -> Int -> Bool
  esFactorialAux n index  | factorial index > n = False
                          | factorial index == n = True
                          | otherwise = esFactorialAux n (index + 1)

  -- |Documentacion de mayorFactHasta
  -- mayorFactHasta busca el maximo n <= al argumento tal que n = k! para k natural
  mayorFactHasta :: Int -> Int
  mayorFactHasta m = mayorFactHastaAux m m

  -- |Documentacion de mayorFactHastaAux
  -- mayorFactHastaAux busca el mayor numero menor o igual a la cota tal que se corresponda con algun k! para k natural
  mayorFactHastaAux :: Int -> Int -> Int
  mayorFactHastaAux index cota  | index <= 0 = 1
                                | factorial index <= cota = factorial index
                                | otherwise = mayorFactHastaAux (index-1) cota

  -- |Documentacion de esFibonacci
  -- esFibonacci indica si el parametro es un numero de Fibonacci --Ejercicio 9
  esFibonacci :: Int -> Bool
  esFibonacci numero = validaFibonacciDesde 0 1 numero

  -- |Documentacion de validaFibonacciDesde
  -- validaFibonacciDesde indica si el numero dado es pertenece a Fibonacci, buscando de forma ascendente desde donde se indique
  validaFibonacciDesde :: Int -> Int -> Int -> Bool
  validaFibonacciDesde anterior base numero | numero == 0 = True
                                            | numero == (anterior+base) = True
                                            | numero < (anterior+base) = False
                                            | otherwise = validaFibonacciDesde (base) (anterior+base) numero
