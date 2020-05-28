module Bernardez_37806391_grupo11
where
  {-
    Este modulo son las funciones correspondientes a la primera entrega del trabajo practico. Ellas son:
      -sonCoprimos :: Integer -> Integer -> Bool
      -es2Pseudoprimo :: Integer -> Bool
      -cantidad3Pseudoprimos :: Integer -> Integer
      -kesimo2y3Pseudoprimo :: Integer -> Integer
      -esCarmichael :: Integer -> Bool
    más las funcionciones auxiliares que se consideraron necesarias para resolucion o mejor comprension. Ellas son:
      -maximoComunDivisor
      -esPrimo
      -menorDivisor
      -menorDivisorDesde
      -es3Pseudoprimo
      -primer2y3Pseudoprimo
      -siguiente2y3Pseudoprimo
      -esApseudoprimo
      -verificaCarmichaelDesde


    Aclaraciones:
    - No tengo libreta aún razon por la que el nombre del archivo, respetando el formato, incluye mi numero de documento en su lugar
    - Se utiliza tipo Integer en lugar de Int en funciones que podrian implementarlo para evitar errores de tipado
    - La funcion "esApseudoprimo" fue construida finalizando el trabajo practico para la implementacion de "esCarmichael", y no
      crei conveniente reemplazar los llamados de "es2Pseudoprimo" y "es3Pseudoprimo" en todo el codigo. Pero es claro que es posible
      hacerlo indicando el parametro "a" en cada caso. Ejemplo: "es3Pseudoprimo n" coincide con "esApseudoprimo 3 n"
    - La funcion "esCarmichael" con el parametro 75361 retorna true como es de esperar, aunque el tiempo de ejecución es de aprox 5 minutos (309 segundos)

    Muchas gracias
    Bernardez Francisco
    bernardez.f@gmail.com
    DNI:37806391
  -}


  {-funciones de la primera entrega: -}


  {- Documentacion de sonCoprimos
  sonCoprimos dados dos numeros naturales decide si son coprimos
  input:
  n: cualquier numero natural
  m: cualquier numero natural
  output:
  True: n y m son coprimos
  False: n y m no son coprimos
  -}
  sonCoprimos :: Integer -> Integer -> Bool
  sonCoprimos n m = maximoComunDivisor n m == 1

  {- Documentacion de es2Pseudoprimo
  es2Pseudoprimo dado un numero natural decide si es 2-pseudoprimo.
  input:
  n: cualquier numero natural
  output:
  True: n es 2-pseudoprimo
  False: n no es 2-pseudoprimo
  -}
  es2Pseudoprimo :: Integer -> Bool
  es2Pseudoprimo n  | esPrimo n = False
                    | otherwise = mod (pred (2 ^ (pred n))) n == 0

  {- Documentacion de cantidad3Pseudoprimos
  cantidad3Pseudoprimos dado un numero natural m calcula la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive
  input:
  m: cualquier numero natural
  output:
  cantidad de 3-pseudoprimos calculada
  -}
  cantidad3Pseudoprimos :: Integer -> Integer
  cantidad3Pseudoprimos m | m == 1 = 0
                          | es3Pseudoprimo m = 1 + cantidad3Pseudoprimos (pred m)
                          | otherwise = cantidad3Pseudoprimos (pred m)

  {- Documentacion de kesimo2y3Pseudoprimo
  kesimo2y3Pseudoprimo dado un numero natural k calcula el k-esimo numero que es simulaneamente 2-pseudoprimo y 3-pseudoprimo
  input:
  k: cualquier numero natural
  output:
  k-esimo numero que es simulaneamente 2-pseudoprimo y 3-pseudoprimo calculado

  nota: Inspirado en la funcion nEsimoPrimo de la clase 5
  "el k-esimo sera el siguiente valor que verifique simultaneamente ser 2psuedoprimo y 3psuedoprimo del k-esimo - 1"
  -}
  kesimo2y3Pseudoprimo :: Integer -> Integer
  kesimo2y3Pseudoprimo 1 = primer2y3Pseudoprimo
  kesimo2y3Pseudoprimo k = siguiente2y3Pseudoprimo (succ (kesimo2y3Pseudoprimo (pred k)))

  {- Documentacion de esCarmichael
  esCarmichael dado un numero natural n decide si es un numero de Carmichael
  input:
  n: cualquier numero natural
  output:
  True: n es un numero de Carmichael
  False: n no es un numero de Carmichael
  -}
  esCarmichael :: Integer -> Bool
  esCarmichael n = verificaCarmichaelDesde n (pred n)


  {-funciones auxiliares: -}


  {- Documentacion de maximoComunDivisor
  maximoComunDivisor dados dos numeros naturales calcula el maximo comun dividor
  aplicando el algoritmo de Euclides
  input:
  n: cualquier numero natural
  m: cualquier numero natural
  output:
  maximo comun dividor calculado

  nota: el orden de los parametros permutando en la recursión
  -}
  maximoComunDivisor :: Integer -> Integer -> Integer
  maximoComunDivisor n 0 = n
  maximoComunDivisor n m = maximoComunDivisor m (mod n m)

  {- Documentacion de esPrimo
  esPrimo indica si el numero natural pasado como argumento es primo
  input:
  n: cualquier numero natural
  output:
  True: n es primo
  False: n no es primo
  -}
  esPrimo :: Integer -> Bool
  esPrimo n = menorDivisor n == n

  {- Documentacion de menorDivisor
  menorDivisor calcula el menor divisor (mayor que 1) de un natural n
  input:
  n: cualquier numero natural
  output:
  menor divisor encontrado o 0 (cero) en cualquier otro caso
  -}
  menorDivisor :: Integer -> Integer
  menorDivisor n = menorDivisorDesde n 2

  {- Documentacion de menorDivisorDesde
  menorDivisorDesde devuelve el menor divisor de un numero natural n, comenzando desde un natural k
  input:
  n: cualquier numero natural
  k: numero natural desde el cual comienza la busqueda ascendente de un dividor de n
  output:
  menor divisor encontrado o 0 (cero) en cualquier otro caso
  -}
  menorDivisorDesde :: Integer -> Integer -> Integer
  menorDivisorDesde n k   | k > n = 0
                          | k == 1 || k == n  = k
                          | mod n k == 0 = k
                          | otherwise = menorDivisorDesde n (succ k)

  {- Documentacion de es3Pseudoprimo
  es3Pseudoprimo dado un numero natural decide si es 3-pseudoprimo
  input:
  n: cualquier numero natural
  output:
  True: n es 3-pseudoprimo
  False: n no es 3-pseudoprimo
  -}
  es3Pseudoprimo :: Integer -> Bool
  es3Pseudoprimo n  | esPrimo n = False
                    | otherwise = mod (pred (3 ^ (pred n))) n == 0

  {- Documentacion de primer2y3Pseudoprimo
  primer2y3Pseudoprimo retorna el primer valor natural que verica ser simultaneamente 2-pseudoprimo y 3-pseudoprimo
  input:
  inicio: cualquier numero natural
  output:
  primer numero natural que verifica ser simulaneamente 2-pseudoprimo y 3-pseudoprimo encontrado o 0 (cero) en cualquier otro caso
  -}
  primer2y3Pseudoprimo :: Integer
  primer2y3Pseudoprimo = siguiente2y3Pseudoprimo 2

  {- Documentacion de siguiente2y3Pseudoprimo
  siguiente2y3Pseudoprimo busca el primer numero natural que verifica ser simulaneamente 2-pseudoprimo y 3-pseudoprimo desde
  el inicio indicado
  input:
  inicio: cualquier numero natural
  output:
  primer numero natural que verifica ser simulaneamente 2-pseudoprimo y 3-pseudoprimo encontrado o 0 (cero) en cualquier otro caso
  -}
  siguiente2y3Pseudoprimo :: Integer -> Integer
  siguiente2y3Pseudoprimo inicio  | inicio < 2 = 0
                                        | es2Pseudoprimo inicio && es3Pseudoprimo inicio = inicio
                                        | otherwise = siguiente2y3Pseudoprimo (succ inicio)

  {- Documentacion de esApseudoprimo
  esApseudoprimo dado un numero natural decide si es a-pseudoprimo, siendo a un numero natural
  input:
  n: cualquier numero natural
  a: numero natural para realizar el calculo a^(n−1) − 1
  output:
  True: n es a-pseudoprimo
  False: n no es a-pseudoprimo
  -}
  esApseudoprimo :: Integer -> Integer -> Bool
  esApseudoprimo a n  | esPrimo n = False
                      | otherwise = mod (pred (a ^ (pred n))) n == 0

  {- Documentacion de verificaCarmichaelDesde
  verificaCarmichaelDesde verifica las condiciones de Carmichael un numero n natural desde un valor indicado hasta el 1
  input:
  n: cualquier numero natural
  decrementador: valor que decrementa decrementa en los llamados recursivos hasta el 1
  output:
  True: n es un numero de Carmichael
  False: n no es un numero de Carmichael o decrementador >= n
  -}
  verificaCarmichaelDesde :: Integer -> Integer -> Bool
  verificaCarmichaelDesde n decrementador | decrementador >= n = False -- pensado para dar resultados solo para decrementador = n - 1 como establecen las condicionee de Carmichael
                                          | decrementador == 0 = True -- finalice la decrementacion sin que ningun numero rompa las condiciones
                                          | sonCoprimos decrementador n && esApseudoprimo decrementador n = verificaCarmichaelDesde n (pred decrementador)
                                          | sonCoprimos decrementador n = False -- indica que el decrementador es coprimo con n, y entonces no fue Aseudiprimo. Rompe las condicionee de Carmichael
                                          | otherwise = verificaCarmichaelDesde n (pred decrementador)
