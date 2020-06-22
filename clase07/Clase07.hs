module Clase07
where
  {-
    este modulo corresponde a la clase 7 - Calcular conjunto de partes
  -}

  type Set a = [a]

  vacio :: (Eq a) => Set a
  vacio = []

  pertenece :: (Eq a) => a -> Set a -> Bool
  pertenece _ []  = False
  pertenece x (y:ys)  | x == y = True
                      | otherwise = pertenece x ys

  agregar :: (Eq a) => a -> Set a -> Set a
  agregar x c | pertenece x c = c
              | otherwise = x:c

  incluido :: (Eq a) => Set a -> Set a -> Bool
  incluido [] conjunto = True
  incluido (x:xs) conjunto = pertenece x conjunto && incluido xs conjunto

  iguales :: (Eq a) => Set a -> Set a -> Bool
  iguales conjunto1 conjunto2  = incluido conjunto1 conjunto2  && incluido conjunto2 conjunto1

  agregarATodos :: (Eq a) => a -> Set(Set a) -> Set(Set a)
  agregarATodos elemento [] = []
  agregarATodos elemento (x:xs) = agregar (agregar elemento x) (agregarATodos elemento xs)
