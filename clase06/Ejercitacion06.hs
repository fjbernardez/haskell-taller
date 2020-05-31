module Ejercitacion06
where
  {-
    este modulo es la ejercitacion de la clase 6
  -}

  -- |Documentacion de productoria
  -- productoria devuelve la productoria de los elementos
  productoria :: [Int] -> Int
  productoria [] = 1 --nulo multiplicativo
  productoria (x:xs) = x * productoria xs
