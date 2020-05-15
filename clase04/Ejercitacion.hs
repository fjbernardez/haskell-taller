module Ejercitacion
where
  {-
    este modulo es la ejercitacion de la clase 4
  -}

-- |Documentacion de sumatoriaG1
-- sumatoria que responde al ejercicio 4
sumatoriaG1 :: (Int, Int) -> Int
sumatoriaG1 (i,n) | i == n = i ^ (n)
                  | otherwise = i ^ (n) + sumatoriaG1 (i,n-1)

-- |Documentacion de sumatoriaG2
-- sumatoria que responde al ejercicio 5
-- sumatoriaG2 :: Int -> Int
-- sumatoriaG2 0 = 0
-- sumatoriaG2 n = sumatoriaG1 (pred n, n) + ??????????

-- |Documentacion de sumatoriaG2
-- sumatoria que responde al ejercicio 5
sumatoriaG2 :: Int -> Int
sumatoriaG2 0 = 0
sumatoriaG2 n = sumatoriaG2 (n - 1) + sumatoriaG2Aux (n, 1)

-- |Documentacion de sumatoriaG2Aux
-- funcion creada para la implementacion de sumatoriaG2
sumatoriaG2Aux :: (Int, Int) -> Int
sumatoriaG2Aux (n, q) | n == q = n ^ n
                      | otherwise = q ^ n + sumatoriaG2Aux (n , succ q)
