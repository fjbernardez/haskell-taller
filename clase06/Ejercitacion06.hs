module Ejercitacion06
where
  import Clase06
  {-
    este modulo es la ejercitacion de la clase 6
  -}

  {- funciones principales: -}

  -- |Documentacion de productoria --ejercicio 1
  -- productoria devuelve la productoria de los elementos
  productoria :: [Int] -> Int
  productoria [] = 1 --nulo multiplicativo
  productoria (x:xs) = x * productoria xs

  -- |Documentacion de sumarN --ejercicio 2
  -- sumarN dado un numero N y una lista xs, suma N a cada elemento de xs
  sumarN :: Int -> [Int] -> [Int]
  sumarN n [] = []
  sumarN n lista  | longitud lista == 1 = head lista + n : [] --retorno una lista que contenga solo el ultimo elemento para empezar la concatenacion
                  | otherwise = addElemento (head lista + n) (sumarN n (tail lista))

  -- |Documentacion de sumarElPrimero --ejercicio 3
  -- sumarElPrimero dada una lista no vacia, suma el primer elemento a toda la lista
  sumarElPrimero :: [Int] -> [Int]
  sumarElPrimero [] = []
  sumarElPrimero (x:xs) = sumarN x (x:xs)


  {- funciones auxiliares: -}

  -- |Documentacion de addElemento
  -- addElemento añade un numero n elemento a la lista indicada
  addElemento :: Int -> [Int] -> [Int]
  addElemento n []        = n : []
  addElemento n (x : xs)  = n : (x : xs)





--
