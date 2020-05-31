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

  -- |Documentacion de sumarElUltimo --ejercicio 4
  -- sumarElUltimo dada una lista no vacia, suma el ultimo elemento a toda la lista
  sumarElUltimo :: [Int] -> [Int]
  sumarElUltimo [] = []
  sumarElUltimo (x:xs) = sumarN (darUltimo (x:xs)) (x:xs)

  -- |Documentacion de pares --ejercicio 5
  -- pares dada una lista devuelve otra lista con los elementos pares de la lista original
  pares :: [Int] -> [Int]
  pares [] = [] -- este caso no solo evita errores, si no que resulta caso base cuando se analizaron todos los elemtos de la lista
  pares (x:xs) | esPar x = addElemento x (pares (xs))
               | otherwise = pares xs


  {- funciones auxiliares: -}

  -- |Documentacion de addElemento
  -- addElemento añade un numero n elemento a la lista indicada -- a : []
  addElemento :: Int -> [Int] -> [Int]
  addElemento n []        = n : []
  addElemento n (x:xs)  = n : (x:xs)

  -- |Documentacion de darUltimo
  -- darUltimo retorna el ultimo elemnto de la lista indicada --last []
  darUltimo :: [Int] -> Int
  darUltimo (x:xs) | longitud (x:xs) == 1 = x
                   | otherwise = darUltimo xs

  -- |Documentacion de esPar
  -- esPar indica si el arumento es par
  esPar :: Int -> Bool
  esPar n = mod n 2 == 0
