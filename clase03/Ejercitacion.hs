module Ejercitacion
where
  {-
    este modulo completa la ejercitacion de la clase 3 junto con las funciones
    añadias al modulo Naturales.hs en ./clase/Naturales.hs
  -}

  -- |Documentacion de tribonacci
  -- tribonacci responde a la funcion partida dada como consigna de la clase 3, ejercico 1
tribonacci :: Int -> Int
tribonacci n  | n <= 2 = n
              | otherwise = tribonacci(n - 1) + tribonacci(n - 2) + tribonacci(n - 3)

-- |Documentacion de esMultiplo3
-- esMultiplo3 indica si elñ argumento esmultiplo de 3
esMultiplo3 :: Int -> Bool
esMultiplo3 n | n == 0 = True
              | n < 3 = False
              | otherwise = esMultiplo3 (n - 3)


-- |Documentacion de digitoUnidades
-- digitoUnidades indica dado un numero natural su dıgito de las unidades
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

-- 8 digitoDecenas: dado un numero natural, extrae su dıgito de las decenas.
digitoDecenas :: Int -> Int
digitoDecenas x | x < 10 = 0
                | otherwise = digitoUnidades (div (x - digitoUnidades x) 10)

-- |Documentacion de diabolico
-- diabolico indica si todos los digitos de un numero son 6
diabolico :: Int -> Bool
diabolico n | n < 10 = n == 6
            | otherwise = digitoUnidades n == 6 && diabolico (div n 10)


-- |Documentacion de diabolicoExtendido
-- diabolicoExtendido indica si todos los digitos de un numero son iguales
diabolicoExtendido :: Int -> Bool
diabolicoExtendido n  | n < 10 = True
                      | otherwise = digitoUnidades n == digitoDecenas n && diabolicoExtendido (div n 10)
