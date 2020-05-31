module Ejercitacion06
where
  import Clase06
  {-
    este modulo es la ejercitacion de la clase 6
  -}

  -- |Documentacion de productoria
  -- productoria devuelve la productoria de los elementos
  productoria :: [Int] -> Int
  productoria [] = 1 --nulo multiplicativo
  productoria (x:xs) = x * productoria xs

  -- |Documentacion de sumarN
  -- sumarN dado un numero N y una lista xs, suma N a cada elemento de xs
  -- sumarN :: Int -> [Int] -> [Int]
  -- sumarN n lista | tail list == []
