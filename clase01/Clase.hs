module Clase where

f x y = x * x + y * y
g x y z = x + y + z * z

doble :: Int -> Int
doble x = 2 * x

suma x y = x + y

normaVectorial x1 x2 = sqrt (x1^2 + x2^2)

funcionConstante8 x = 8

signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

maximo :: Int -> Int -> Int
maximo x y  | x >= y = x
            | otherwise = y

f1 n | n >= 3 = 5

f3 n  | n >= 3 = 5
      | n == 2 = undefined
      | otherwise = 8

f5 n  | n <= 9 = 7
      | n >= 3 = 5

i  n  | n == 0 = 1
      | n /= 0 = 0

discriminante b c | expresion > 0 = 2
                  | expresion == 0 = 1
                  | otherwise = 0
                  where expresion = b ^2 - 4* c

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = ( x >= y ) || z
