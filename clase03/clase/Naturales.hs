module Naturales
where
    import Prelude(Int,Bool(True,False),succ,pred,(||),(&&),not,otherwise)

    {-
    las funciones suma y mult fueron trabajadas en clase. Se añaden como parte
    de la ejercitacion a este moculo las funciones:
    -resta
    -menor
    -mayor
    -iguales
    -}

    -- |Documentacion de suma
    -- suma realiza la suma de dos numeros esteros
    suma :: Int -> Int -> Int
    --suma m n = suma (succ (pred m)) n = suma (pred m) (succ n)
    suma 0 n = n
    suma m n = suma (pred m) (succ n)

    -- |Documentacion de mult
    -- mult realiza la multiplicacion de dos numeros esteros
    mult :: Int -> Int -> Int
    -- m * n = (1 + (m-1)) * n = n + (m-1)*n
    mult 0 n = 0
    mult m n = suma n (mult (pred m) n)

    -- |Documentacion de resta
    -- resta realiza la resta de dos numeros esteros de la forma: n - m
    resta :: Int -> Int -> Int
    resta 0 _ = 0
    resta m 0 = m
    resta m n = resta (pred m) (pred n)

    -- |Documentacion de menor
    -- menor determina si el primer argumento es menor
    menor :: Int -> Int -> Bool
    menor 0 0 = False
    menor _ 0 = False
    menor 0 _ = True
    menor n m = menor (pred n) (pred m)

    -- |Documentacion de mayor
    -- mayor determina si el primer argumento es mayor
    mayor :: Int -> Int -> Bool
    mayor 0 0 = False
    mayor _ 0 = True
    mayor 0 _ = False
    mayor n m = mayor (pred n) (pred m)

    -- |Documentacion de igual
    -- igual determina si los argumentos son iguales
    igual :: Int -> Int -> Bool
    igual 0 0 = True
    igual _ 0 = False
    igual 0 _ = False
    igual n m = igual (pred n) (pred m)
