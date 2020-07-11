module Ejercitacion10
where
  import Clase05
  import Clase09
  import Clase10
  import ClaseAdicional

  {-
    este modulo son los ejercicios de la clase 10
  -}


  {- funciones principales: -}

  -- |Documentacion de cadaEcTieneSoluc -- Ej 1
  -- cadaEcTieneSoluc dado un sistema general, decide si cada una de sus ecuaciones vista independientemente de las otras, tiene solucion
  cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool
  cadaEcTieneSoluc [] = False
  cadaEcTieneSoluc [ec] = tieneEcEquivalente ec
  cadaEcTieneSoluc (ec:ecs) = (tieneEcEquivalente ec) && (cadaEcTieneSoluc ecs)

  -- |Documentacion de tieneSolucionSimplif -- Ej 2
  -- tieneSolucionSimplif dado un sistema simplificado, decide si tiene solucion
  tieneSolucionSimplif :: [(Int, Int)] -> Bool
  tieneSolucionSimplif sistema = sistemaEquivSinPrimosMalosAux_Bool sistema (todosLosPrimosMalos sistema)

  -- |Documentacion de tieneSolucion -- Ej 3
  -- tieneSolucion  dado un sistema general, decide si tiene solucion
  tieneSolucion :: [(Int, Int, Int)] -> Bool
  tieneSolucion sistema | cadaEcTieneSoluc sistema = tieneSolucionSimplif sistemaEquivalente
                        | otherwise = False
    where
      sistemaEquivalente = sistemaSimplifEquiv sistema

  -- |Documentacion de dirichlet -- Ej 4
  -- dirichlet dados dos nuumeros coprimos r y m con 1 ≤ r < m, encuentra un numero primo en la clase de congruencia X ≡ r (mod m).
  dirichlet :: Int -> Int -> Int
  dirichlet r m   | (r >= m) || (r <= 0) = undefined
                  | otherwise = dirichletDesde r m 1

  {- funciones auxiliares: -}

  -- indica si existe solucion equivalente para el sistema. De no exister el mismo no tiene solucion.
  tieneEcEquivalente :: (Int, Int, Int) -> Bool
  tieneEcEquivalente (a, b, m) = (mod b d) == 0
   where d = mcd a m

  ----------------------------------------------------------------------------------------------------------------------------------------------------

  -- misma logica que la vista en clase, solo se ajustaron los tipos para indicar True o False

  solucDosEcPotenciasPrimoOrd_Bool :: (Int, Int) -> (Int, Int) -> Bool
  solucDosEcPotenciasPrimoOrd_Bool (r1, m1) (r2, m2)  | mod (r2-r1) m1 == 0 = True
                                                      | otherwise = False

  solucDosEcPotenciasPrimo_Bool :: (Int, Int) -> (Int, Int) -> Bool
  solucDosEcPotenciasPrimo_Bool (r1, m1) (r2, m2)   | m1 <= m2 = solucDosEcPotenciasPrimoOrd_Bool (r1, m1) (r2, m2)
                                                    | otherwise = solucDosEcPotenciasPrimoOrd_Bool (r2, m2) (r1, m1)

  solucSistemaPotenciasPrimo_Bool :: [(Int, Int)] -> Bool
  solucSistemaPotenciasPrimo_Bool [e2] = True -- "neutro" False && True = False True && True = True
  solucSistemaPotenciasPrimo_Bool (e1:e2:es) = (solucDosEcPotenciasPrimo_Bool e1 e2) && ( solucSistemaPotenciasPrimo_Bool (e2:es) )

  sistemaEquivSinPrimosMalosAux_Bool :: [(Int, Int)] -> [Int] -> Bool
  sistemaEquivSinPrimosMalosAux_Bool sist [] = True -- "neutro" False && True = False True && True = True
  sistemaEquivSinPrimosMalosAux_Bool sist (p:ps) = (solucSistemaPotenciasPrimo_Bool pri) && (sistemaEquivSinPrimosMalosAux_Bool seg ps)
   where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p
  ----------------------------------------------------------------------------------------------------------------------------------------------------

  -- añade a dirichlet un contador y un inicio en la progresion geometrica
  dirichletDesde :: Int -> Int -> Int -> Int
  dirichletDesde r m i  | esPrimo (valor) = valor
                        | otherwise = dirichletDesde r m (succ i)
    where
      valor = r + m * i
