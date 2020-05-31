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
                  | otherwise = anadir (head lista + n) (sumarN n (tail lista))

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
  pares (x:xs) | esPar x = anadir x (pares (xs))
               | otherwise = pares xs

   -- |Documentacion de quitar --ejercicio 6
   -- quitar elimina la primera aparicion del elemento en la lista (de haberla)
  quitar :: Int -> [Int] -> [Int]
  quitar n [] = []
  quitar n (x:xs) | x == n = xs
                  | otherwise = anadir x (quitar n xs)

  -- |Documentacion de quitarTodas --ejercicio 7
  -- quitarTodas elimina todas las apariciones del elemento en la lista (de haberlos)
  quitarTodas :: Int -> [Int] -> [Int]
  quitarTodas n [] = []
  quitarTodas n (x:xs)  | x == n && longitud (x:xs) == 1 = []
                        | longitud (x:xs) == 1 = [x]
                        | x == n = quitarTodas n xs
                        | otherwise = anadir x (quitarTodas n xs)

  -- |Documentacion de hayRepetidos --ejercicio 8
  -- hayRepetidos indica si existe algun elemento repetido
  hayRepetidos :: [Int] -> Bool
  hayRepetidos [] = False
  hayRepetidos (x:xs) | pertenece x xs = True
                      | otherwise = hayRepetidos xs

  -- |Documentacion de eliminarRepetidosAlFinal --ejercicio 9
  -- eliminarRepetidosAlFinal deja en la lista la primera aparicion de cada elemento, eliminando las repeticiones adicionales
  eliminarRepetidosAlFinal :: [Int] -> [Int]
  eliminarRepetidosAlFinal [] = []
  eliminarRepetidosAlFinal (x:xs) | not (pertenece x xs) = anadir x (eliminarRepetidosAlFinal xs) -- not (pertenece x xs) = "no esta repetido" pertenece x xs = "esta repetido"
                                  | pertenece x xs = anadir x (eliminarRepetidosAlFinal (quitarTodas x xs))
                                  | otherwise = eliminarRepetidosAlFinal xs

  -- |Documentacion de eliminarRepetidosAlInicio --ejercicio 10
  -- eliminarRepetidosAlInicio que deja en la lista la ultima aparicion de cada elemento, eliminando las repeticiones adicionales
  eliminarRepetidosAlInicio :: [Int] -> [Int]
  eliminarRepetidosAlInicio [] = []
  eliminarRepetidosAlInicio (x:xs)  | not (pertenece x xs) = anadir x (eliminarRepetidosAlInicio xs)
                                    | otherwise = eliminarRepetidosAlInicio xs


  -- |Documentacion de maximo --ejercicio 11
  -- maximo que calcula el maximo elemento de una lista no vacia
  maximo :: [Int] -> Int
  maximo (x:xs) | longitud (x:xs) == 2 = max x (head xs)
                | x > (head xs) = maximo (anadir x (tail xs)) -- descarto head de tail de la lista, osea el primer elemento de la cola de la lista
                | otherwise = maximo xs -- descarto head

  -- |Documentacion de ordenar --ejercicio 12
  -- ordenar ordena los elementos de forma creciente
  ordenar :: [Int] -> [Int]
  ordenar [] = []
  ordenar [x] = [x]
  ordenar (x:xs) = anadir (minimo (x:xs)) ( ordenar ( quitar ( minimo (x:xs) ) (x:xs) ) )

  -- |Documentacion de reverso --ejercicio 13
  -- reverso  dada una lista invierte su orden
  reverso :: [Int] -> [Int]
  reverso [] = []
  reverso [x] = [x]
  reverso (x:xs) = anadir ( darUltimo (x:xs) ) (reverso (quitarUltimo (x:xs) ) )



  {- funciones auxiliares: -}

  -- |Documentacion de anadir
  -- anadir añade un numero n elemento a la lista indicada -- a : []
  anadir :: Int -> [Int] -> [Int]
  anadir n []     = n : []
  anadir n (x:xs) = n : (x:xs)

  -- |Documentacion de darUltimo
  -- darUltimo retorna el ultimo elemnto de la lista indicada --last []
  darUltimo :: [Int] -> Int
  darUltimo (x:xs) | longitud (x:xs) == 1 = x
                   | otherwise = darUltimo xs

  -- |Documentacion de esPar
  -- esPar indica si el arumento es par
  esPar :: Int -> Bool
  esPar n = mod n 2 == 0

  -- |Documentacion de minimo
  -- minimo que calcula el minimo elemento de una lista no vacia
  minimo :: [Int] -> Int
  minimo (x:xs) | longitud (x:xs) == 2 = min x (head xs)
                | x < (head xs) = minimo (anadir x (tail xs)) -- descarto head de tail de la lista, osea el primer elemento de la cola de la lista
                | otherwise = minimo xs -- descarto head

  -- |Documentacion de quitarUltimo
  -- quitarUltimo retorna la otra lista sin el ultimo elemnto
  quitarUltimo :: [Int] -> [Int]
  quitarUltimo [] = []
  quitarUltimo [x] = [] --quito ultimo elemento
  quitarUltimo (x:xs) = anadir (x) (quitarUltimo xs)











--
