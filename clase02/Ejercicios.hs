module Ejercicios
where
  {-
    este modulo son los ejercicios de la clase 2
  -}

-- import clase.* --> Preguntar!

-- |Documentacion de estanRelacionados
-- estanRelacionados indica si existe relacion entre los dos numeros reales dados
estanRelacionados :: (Float, Float) -> Bool
estanRelacionados (x, y)   | x <= 3 && y <= 3 = True
                        | x <= 7 && y <= 7 = True
                        | x > 7 && y > 7 = True
                        | otherwise = False
-- ¿por que no puedo borrar "= True" al final de cada rama?


-- |Documentacion de prodInt
-- prodInt calcula el producto interno de dos vectores pertenecientes a R2
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- |Documentacion de todoMenor
-- todoMenor indica si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

-- |Documentacion de distanciaPuntos
-- distanciaPuntos calcula la distancia entre dos puntos de R2
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt ( (x1 - x2)**2 + (y1 - y2)**2 )

-- |Documentacion de sumaTerna
-- sumaTerna  dada una terna de enteros, calcula la suma de sus tres elementos
sumaTerna :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumaTerna (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

-- |Documentacion de posicPrimerPar
-- posicPrimerPar   dada una terna de enteros, devuelve la posicion del primer numero par si es que hay alguno, y devuelve 4 si son todos impares
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z)   | mod x 2 == 0 = 1
                        | mod y 2 == 0 = 2
                        | mod z 2 == 0 = 3
                        | otherwise = 4

-- |Documentacion de crearPar
-- crearPar crea un par perteneciente a R2 a partir de sus dos componentes dadas
crearPar :: t1 -> t -> (t1, t)
crearPar x y = (x,y)

-- |Documentacion de invertir
-- invertir invierte los elementos del par pasado como parametro
invertir :: (t1, t) -> (t, t1)
invertir (x, y) = (y, x)
