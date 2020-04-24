module Ejercicios where

import Clase01

-- 1 absoluto: calcula el valor absoluto de un numero entero.
absoluto :: Int -> Int
absoluto x  | x < 0 = x * (-1)
            | otherwise = x
-- 2 maximoabsoluto: devuelve el maximo entre el valor absoluto de dos numeros enteros.
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y  | absoluto x > absoluto y = absoluto x
                    | otherwise = absoluto y
-- 3 maximo3: devuelve el maximo entre tres numeros enteros.
maximoEntreTresEnteros :: Int -> Int -> Int -> Int
maximoEntreTresEnteros x y z  | maximo x y < z = z
                              | maximo z x < y = y
                              | maximo y z < x = x
-- 4 algunoEs0: dados dos numeros racionales, decide si alguno de los dos es igual a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).
algunoEsCeroConPattern :: Float -> Float -> Int
algunoEsCeroConPattern x y  | x == 0.0 = 1
                            | y == 0.0 = 1
                            |otherwise = 0
-- 5 ambosSon0: dados dos numeros racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).
ambosSonCero :: Float -> Float -> Int
ambosSonCero x y  | x == 0 && y == 0 = 1
                  | otherwise = 0
-- 6 esMultiploDe: dados dos numeros naturales, decidir si el primero es multiplo del segundo.
esMultiploDe :: Int -> Int -> Int
esMultiploDe x y  | mod x y == 0 = 1
                  | otherwise = 0
-- 7 digitoUnidades: dado un numero natural, extrae su dıgito de las unidades.
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10
-- 8 digitoDecenas: dado un numero natural, extrae su dıgito de las decenas.
digitoDecenas :: Int -> Int
digitoDecenas x | x < 10 = 0
                | otherwise = digitoUnidades (div (x - digitoUnidades x) 10)
