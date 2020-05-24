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

  -- |Documentacion de esSumaInicialDePrimos
  -- esSumaInicialDePrimos indica si n es igual a la suma de los m primeros numeros primos, para algun m. --Ejercicio 10
  esSumaInicialDePrimos :: Int -> Bool
  esSumaInicialDePrimos n = esSumaInicialDePrimosAux n 0 2

  -- |Documentacion de esSumaInicialDePrimosAux
  -- esSumaInicialDePrimosAux verifica si la cota se corresponde con la suma consecutiva de los primeros numeros primos
  esSumaInicialDePrimosAux :: Int -> Int -> Int -> Bool
  esSumaInicialDePrimosAux cota suma index  | cota == suma = True
                                            | cota < suma = False
                                            | otherwise = esSumaInicialDePrimosAux cota (suma + index) (proximoPrimo index)

  -- |Documentacion de proximoPrimo
  -- proximoPrimo retorna el numero primo siguiente al pasado como parametro
  proximoPrimo :: Int -> Int
  proximoPrimo cota = proximoPrimoDesde cota 1

  -- |Documentacion de proximoPrimoDesde
  -- proximoPrimoDesde retorna el numero primo siguiente al pasado como parametro, empezanod desde el indice indicado
  proximoPrimoDesde :: Int -> Int -> Int
  proximoPrimoDesde cota index  | nEsimoPrimo(index) > cota = nEsimoPrimo(index)
                                | nEsimoPrimo(index) <= cota = proximoPrimoDesde cota (index + 1)

  -- |Documentacion de tomaValorMax
  -- tomaValorMax retorna el numero dado un numero entero "base" ≥ 1 y otro valor "extremo" ≥ base devuelve algun numero "medio" coprendido en el intervalo
  -- tal que sumaDivisores(medio) = max{sumaDivisores(i) | base ≤ i ≤ extremo} --Ejercicio 11
  tomaValorMax :: Int -> Int -> Int
  tomaValorMax base extremo | base == extremo = base
                            | sumaDivisores base >= sumaDivisores extremo = tomaValorMax base (pred extremo)
                            | otherwise = tomaValorMax (succ base) extremo

  -- |Documentacion de tomaValorMin
  -- tomaValorMin retorna el numero dado un numero entero "base" ≥ 1 y otro valor "extremo" ≥ base devuelve algun numero "medio" coprendido en el intervalo
  -- tal que sumaDivisores(medio) = min{sumaDivisores(i) | base ≤ i ≤ extremo} --Ejercicio 12
  tomaValorMin :: Int -> Int -> Int
  tomaValorMin base extremo | base == extremo = base
                            | sumaDivisores base >= sumaDivisores extremo = tomaValorMax (succ base) extremo
                            | otherwise = tomaValorMax base (pred extremo)

  -- |Documentacion de esSumaDeDosPrimos
  -- esSumaDeDosPrimos dado un numero natural n determine si puede escribirse como suma de dos numeros primos --Ejercicio 13
  esSumaDeDosPrimos :: Int -> Bool
  esSumaDeDosPrimos n = esSumaDeDosPrimosAux n 2 3

  -- |Documentacion de esSumaDeDosPrimos
  -- esSumaDeDosPrimosAux dado un numero natural n determine si puede escribirse como suma de dos numeros primos
  esSumaDeDosPrimosAux :: Int -> Int -> Int -> Bool
  esSumaDeDosPrimosAux n minimo maximo  | (minimo == maximo) && (minimo + maximo /= n) = False
                                        | minimo + minimo > n = False
                                        | (minimo + minimo) == n || (minimo + maximo) == n = True
                                        | minimo + maximo < n = esSumaDeDosPrimosAux n minimo (proximoPrimo maximo)
                                        | otherwise = esSumaDeDosPrimosAux n (proximoPrimo minimo) (primoAnterior maximo)

  -- |Documentacion de primoAnterior
  -- primoAnterior retorna el numero primo anterior al pasado como parametro. Retorna 2 si se le pasa 2 como argumento
  primoAnterior :: Int -> Int
  primoAnterior n = primoAnteriorAux n 1

  -- |Documentacion de primoAnteriorAux
  -- primoAnteriorAux da funcionalidad a primoAnterior añadiendo un indice de busqueda
  primoAnteriorAux :: Int -> Int -> Int
  primoAnteriorAux n index  | not (esPrimo n) = 0
                            | n == 2 = 2
                            | proximoPrimo (nEsimoPrimo index) == n = nEsimoPrimo index
                            | otherwise = primoAnteriorAux n (succ index)
