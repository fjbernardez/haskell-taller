module Clase09
where
  import Conjuntos
  {-
    este modulo son los ejercicios de la clase 9
    :set +s (para dar el tiempo de ejecucion)
  -}

  {- funciones principales: -}

  -- |Documentacion de digitos -- Ej 1
  -- digitos dados n ≥ 0 y b > 1, retorne su representacion por listas en base b
  digitos :: Int -> Int -> [Int]
  digitos 0 b = []
  digitos n b = (mod n b) : (digitos (div n b) b)


  -- |Documentacion de numero -- Ej 2
  -- numero dada la representacion por listas de n ≥ 0 en base b y la base b > 1, retorne n
  numero :: [Int] -> Int -> Int
  numero lista b = numeroAuxiliar lista b 0

  -- |Documentacion de divisores -- Ej 3
  -- divisores dado un valor n distinto de 0 retorna el conjunto de sus divisores positivos
  divisores :: Int -> Set Int
  divisores 0 = []
  divisores n = listaDeDivisoresPositivosDesde 1 n

  -- |Documentacion de divisores -- Ej 4
  -- divisores dado un valor n distinto de 0 retorna el conjunto de sus divisores positivos
  mcdDef :: Int -> Int -> Int
  mcdDef a 0 = abs a
  mcdDef 0 b = abs b
  mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b ))

  {-
    Ej 5
    argumentos:
    a= 10^10
    b= 2*10^10
    tiempo = ?? (no termino en mas de 30 minutos)
  -}

  -- |Documentacion de mcd -- Ej 6
  -- mcd dados a, b ∈ Z, b distinto de 0, calcule (a : b) usando el algoritmo de Euclides
  mcd :: Int -> Int -> Int
  mcd n 0 = n
  mcd n m = mcd m (mod n m)

  {-
    Ej 7
    argumentos:
    a= 10^10
    b= 2*10^10
    tiempo = (0.01 secs, 76,448 bytes)
    diferencia = abs (0.01 - ??) = ?
  -}

  -- |Documentacion de mcm -- Ej 8
  -- mcm dados a ≥ 0 y b ≥ 0 calcule el minimo d ≥ 0 que sea multiplo tanto de a como de b
  mcm :: Int -> Int -> Int
  mcm a b = mcm' (max a b) (min a b) 1

  -- |Documentacion de emcd -- Ej 9
  -- emcd dados a y b, utilice el algoritmo de Euclides extendido para obtener una tripla ((a : b), s,t) tal que sa + tb = (a : b)
  emcd :: Int -> Int -> (Int, Int, Int)
  emcd a 0 = (a,1,2)
  emcd a b = (m, t, s - t * (div a b))
    where (m,s,t) = emcd b (mod a b)


  -- |Documentacion de emcdMinS -- Ej 10
  -- emcdMinS dados a distinto de 0 y b distinto de 0 encuentre el par s,t ∈ Z tal que sa + tb = (a : b) donde s ≥ 0 sea lo mínimo posible
  emcdMinS :: Int -> Int -> (Int, Int)
  emcdMinS a b = emcdMinS' a b 1


  {- funciones auxiliares: -}

  -- |Documentacion de numeroAuxiliar
  -- numeroAuxiliar añade la variable posicion, para la implementacion de la funcion numeros
  numeroAuxiliar :: [Int] -> Int -> Int -> Int
  numeroAuxiliar [] _ _ = 0
  numeroAuxiliar (x:xs) base posicion = (base ^ posicion) * x + numeroAuxiliar xs base (succ posicion)

  -- |Documentacion de listaDeDivisoresPositivosDesde
  -- listaDeDivisoresPositivosDesde retorna una lista de divisores positivos comenzando en el valor indicado
  listaDeDivisoresPositivosDesde :: Int -> Int -> Set Int
  listaDeDivisoresPositivosDesde inicio numero  | inicio > numero = []
                                                | inicio > (div numero 2) && (inicio <= numero) = numero : []
                                                | (mod numero inicio) == 0 = inicio : listaDeDivisoresPositivosDesde (succ inicio) numero
                                                | otherwise = listaDeDivisoresPositivosDesde (succ inicio) numero

  -- |Documentacion de maximo
  -- maximo que calcula el maximo elemento de una lista
  maximo :: [Int] -> Int
  maximo [x] = x
  maximo (x:xs) = maximo ( (max x (head xs)) : (tail xs) )

  -- |Documentacion de mcm
  -- mcm' implmentacion auxliar para la la funcion mcm
  mcm' :: Int -> Int -> Int -> Int
  mcm' 0 0 _ = 0
  mcm' mayor menor i  | (mod (menor * i) mayor ) == 0 = menor * i
                      | otherwise = mcm' mayor menor (succ i)

  -- |Documentacion de emcdMinS'
  -- emcdMinS' implmentacion auxliar para la la funcion emcdMinS
  emcdMinS' :: Int -> Int -> Int -> (Int, Int)
  emcdMinS' a b k | (sigma + (div b d)*k) < 0 = (sigma + (div b d)*(k + 1), tau - (div a d)*(k + 1))
                  | otherwise                 = emcdMinS' a b (k - 1)
                where (d, sigma, tau) = emcd a b
