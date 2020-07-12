module Ejercitacion11
where
  import Clase11

  {-
    este modulo son los ejercicios de la clase 11
  -}


  {- funciones principales: -}

  -- |Documentacion de cociente -- Ej 7
  -- cociente retorna el resultado de realizar un cociente entre dos numeros complejos
  cociente :: Complejo -> Complejo -> Complejo
  cociente a b = (real, imaginario)
   where
     real = (re a * re b + im a * im b) / ((re b)**2 + (im b)**2)
     imaginario = ( - (re a * im b) + im a * re b) / ((re b)**2 + (im b)**2)

  -- |Documentacion de potencia -- Ej 8
  -- potencia retorna el resultado de realizar la potencia de un numero complejo
  potencia :: Complejo -> Integer -> Complejo
  potencia numero 1 = numero
  potencia numero n = producto numero (potencia numero (pred n))

  -- |Documentacion de solucionesCuadratica -- Ej 9
  -- solucionesCuadratica tome los coeficientes a, b y c y devuelve las dos raıces. En caso de haber una sola, devolverla dos veces
  solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
  solucionesCuadratica 0 _ _ = undefined
  solucionesCuadratica a b c  | discriminante == 0 = ((raizRealUnica, 0), (raizRealUnica, 0))
                              | discriminante > 0 = ((raizReal1, 0), (raizReal2, 0))
                              | otherwise = ((raizRealUnica, parteCompleja), conjugado (raizRealUnica, parteCompleja))
    where
      discriminante = b**2 - 4 * a * c
      raizRealUnica = (-b) / (2 * a)
      raizReal1 = ( (-b) + sqrt (b**2 - 4 * a * c) ) / (2 * a)
      raizReal2 = ( (-b) - sqrt (b**2 - 4 * a * c) ) / (2 * a)
      parteCompleja = sqrt (abs (b**2 - 4 * a * c)) / (2 * a)


  --  segunda parte --

  -- |Documentacion de potenciasRaizNEsima -- Ej 2
  -- potenciasRaizNEsima dados k y n enteros con 0 ≤ k < n, devuelve la lista con las potencias 0, 1, . . . , n−1 de la raız n-esima asociada a k
  potenciasRaizNEsima :: Integer -> Integer -> [Complejo]
  potenciasRaizNEsima k n | n == 1 = [(0, 0)]
                          | otherwise = (potencia kesimaRaiz n) : potenciasRaizNEsima k (pred n)
    where
      kesimaRaiz = (cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))


  -- |Documentacion de solucionesCuadraticaCoefComplejos -- Ej 5
  -- solucionesCuadraticaCoefComplejos
  solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
  solucionesCuadraticaCoefComplejos (0,0) _ _ = undefined
  solucionesCuadraticaCoefComplejos a b c | im discriminante == 0 = solucionesComplejasConBCero a b c
                                          | otherwise = solucionesComplejasConBDistintoCero a b c
    where
      discriminante = suma (potencia b 2) (producto (-4,0) (producto a c)) -- -4 para restar en lugar de sumar.


  {- funciones auxiliares: -}

  --  segunda parte --

  solucionesComplejasConBCero :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
  solucionesComplejasConBCero a b c | re discriminante > 0 = (suma (av,bv) (0.5,0), suma (av,bv) (-0.5,0))
                                    | otherwise = (suma (av,bv) (0,0.5), suma (av,bv) (0,-0.5))
    where
      discriminante = suma (potencia b 2) (producto (-4,0) (producto a c)) -- -4 para restar en lugar de sumar.
      (av,bv) = producto (-1,0) (producto b (producto (0.5,0) a))

  solucionesComplejasConBDistintoCero :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
  solucionesComplejasConBDistintoCero a b c | im discriminante > 0 = ((raizComplejoPositiva, raizComplejoNegativa), producto (-1,0) ((raizComplejoPositiva, raizComplejoNegativa)))
                                            | otherwise = ((raizComplejoPositiva, -1 * raizComplejoNegativa), (-1 * raizComplejoPositiva, raizComplejoNegativa) )
    where
      discriminante = suma (potencia b 2) (producto (-4,0) (producto a c)) -- -4 para restar en lugar de sumar.
      raizComplejoPositiva = sqrt (( re discriminante + sqrt ((re discriminante)**2 + (im discriminante)**2) ) / 2)
      raizComplejoNegativa = sqrt (( (-1) * re discriminante + sqrt ((re discriminante)^2 + (im discriminante)^2) ) / 2)

      --http://fernandorevilla.es/blog/2015/02/07/raiz-cuadrada-de-un-numero-complejo/























--
