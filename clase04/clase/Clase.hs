module Clase
where
  {-
    este modulo son los ejercicios de la clase 4
  -}

-- |Documentacion de sumatoria
-- sumatoria de enteros por recursividad
sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n - 1)

-- |Documentacion de sumatoria'
-- sumatoria' de enteros por por Gauss
sumatoria' :: Int -> Int
sumatoria' n = div (n*(n+1)) 2


-- -------------------------
-- Ejercicios f1, f2, f3, f4  ---------------------------------------
-- -------------------------


-- |Documentacion de f1
-- f1 suma las primera n potencias de la base 2 con recursividad
f1 :: Int -> Int
f1 0 = 1
f1 n = 2 ^ n + f1(n - 1)

-- |Documentacion de f1'
-- f1' suma las primera n potencias de la base 2
f1' :: Int -> Int
f1' n = 2 ^ (n + 1) - 1

-- |Documentacion de f2
-- f2 suma las primera n potencias de la base q con recursividad
f2 :: (Int, Float) -> Float
f2 (1, q) = q
f2 (n, q) = q ^ n + f2(n - 1, q)

-- |Documentacion de f3
-- f3 suma las primera n + n potencias de la base q con recursividad
f3 :: (Int, Float) -> Float
f3 (1, q) = q ^ 2 + q
f3 (n, q) = f3(n - 1, q) + q ^ (2 * n - 1) + q ^ (2 * n)

-- |Documentacion de f4
-- f4 suma las primera n + n potencias de la base q con recursividad, empezando en n
f4 :: (Int, Float) -> Float
f4 (1, q) = q ^ 2 + q
f4 (n, q) = f4 ((n - 1), q) + q ^ (2 * n -1) + q ^ (2 * n) - q ^ (n -1)

-- |Documentacion de f4'
-- f4' suma las primera n + n potencias de la base, empezando en n
f4' :: (Int, Float) -> Float
f4' (n, q) = f3 (n, q) - f2 (n-1, q)


-- |Documentacion de factorial
-- factorial calcula el factorial del argumento
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)


-- |Documentacion de eAprox
-- eAprox aproxima el valor del numero e
eAprox :: Integer -> Float
eAprox 0 = 1
eAprox n = eAprox (n - 1) + 1 / fromIntegral(factorial n)

-- |Definicion de la contante e como instancia de los 11 primeros terminos de la funcion eAprox
e :: Float
e = eAprox 10


-- -------------------------
-- Ejercicios de sumatorias dobles  ---------------------------------
-- -------------------------

-- |Documentacion de f11
-- f11 calcula una sumatoria doble implementando funciones de sumatorias ya definidas, utilizando recursividad.
f11 :: (Int, Int) -> Int
f11 (0, m) = 0
f11 (n, m) = f11(n-1, m) + round( f2 (m, fromIntegral(n)))

-- |Documentacion de sumaPotencias
-- sumaPotencias suma todas las potencias de la forma q ^ (a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m
sumaPotencias :: (Float, (Int, Int)) -> Float
sumaPotencias (_,(_,0)) = 0
sumaPotencias (q, (n, m)) = sumaPotencias (q, (n, m - 1)) + q ^ m * (f2 (n, q))

-- |Documentacion de sumaRacionales
-- sumaRacionales suma todos los numeros racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.
sumaRacionales :: (Int, Int) -> Float
sumaRacionales (n, 0) = 0
sumaRacionales (n, m) = sumaRacionales (n, m-1) + fromIntegral(sumatoria n) / fromIntegral(m)

-- -------------------------
-- Moraleja de la clase  --------------------------------------------
-- -------------------------

-- Para realizar las sumas es necesario escribir todos los terminos generales y encontrar donde hacer la recursion.
