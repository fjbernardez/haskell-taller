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
