module Clase10
where
  import Clase05
  import ClaseAdicional
  import Clase09


-- 1 ---------------------

  ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
  ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                          | otherwise = (div a d, div b d, div m d)
   where d = mcd a m

  -- necesita que a y m sean coprimos
  solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
  solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
   where (d, s, t) = emcd a m

  -- resuelve una ecuacion lineal de congruencia
  solucionEc :: (Int, Int, Int) -> (Int, Int)
  solucionEc e = solucionEcConPropAdic (ecEquivalente e)

  --hasta aca, resulto para sistemas de una sola ecuacion

  -- 2 --------------------

  sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
  sistemaSimplifEquiv [] = []
  sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)

  -- ahora, simplifico un sistema completo

  -- 3 --------------------

  modulos :: [(Int, Int)] -> [Int] -- descarta todos los r, conservando los modulos del sistema en una lista
  modulos [] = []
  modulos ((r, m):es) = m:(modulos es)

  mayorModulo :: [(Int, Int)] -> Int -- tomo el modulo mas grande del sistema
  mayorModulo sist = maximum (modulos sist)

  cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int -- aux con un "contador de primos"
  cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
                                | otherwise = cotaParaPrimoMaloDesde sist (n+1)

  cotaParaPrimoMalo :: [(Int, Int)] -> Int -- encuentro el MAYOR primo que no supera al mayor de los modulos del sistema. No el numero, si no que "n primo" es.
  cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1

  cantidadMultiplos :: [Int] -> Int -> Int -- recorro la lista de modulos, contando cuantos de ellos son divisibles por el n-esimo primo.
  cantidadMultiplos [] _ = 0
  cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                             | otherwise = cantidadMultiplos ms n

  esPrimoMalo :: [(Int, Int)] -> Int -> Bool -- Si la cantidad de multiplos es mayor a dos, entonces en un primo malo.
  esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2


  todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int] -- aux que añade "contador de primos"
  todosLosPrimosMalosHasta _ 0 = []
  todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                                  | otherwise = todosLosPrimosMalosHasta sist (n-1)

  todosLosPrimosMalos :: [(Int, Int)] -> [Int] -- retorna una lista con los primos malos para modulos del sistema dado
  todosLosPrimosMalos [] = []
  todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)

  -- aca, estoy encontrando la lista de los primos malos. Basicamente esos numeros que son un problema a la hora de aplicarl el TCR. Teniendolos puedo
  -- proceder a encontrar un sistema equivalente que me no los tenga.

  -- 4 --------------------

  solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int) -- Se deben pasar ordenadas las ecuaciones. El segundo modulo mas grande o igual que el primero.
  solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                                | otherwise = undefined

  solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int) -- ordeno las ecuaciones, dejando el primero modulo menor o igual al segundo.
  solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                             | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

  --resuelve un sistema en que todos los modulos son potencias de un mismo primo. Esta es una condicion necesaria.
  -- si los restos son congruentes con el modulo mas chico de las dos ecuaciones tomadas, descarto la ecuacion de modulo menor por que sus soluciones
  -- estan contenidas en la ecuacion de modulo mas grande. Este es el criterio para simplificar las ecuacion dos a dos.
  solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
  solucSistemaPotenciasPrimo [e] = e
  solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

  -- aca se resuelvo lo que se llamo la primera parte del sistema. Donde las ecuaciones tiene modulos potencias de un mismo primos. En resumen se descartan
  -- todas aquellas que no aportan infromacion, dejando la mas abarcativa.

  -- 5 --------------------

  desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
  desdoblarSistemaEnFcionPrimo [] _ = ([], [])
  desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                             | m == p^k = ((r, m):pri, seg)
                                             | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg) -- aca se genera el desdoblamiento real de las ecuaciones.
   where (pri, seg) = desdoblarSistemaEnFcionPrimo es p
         k = darPotencia m p

  sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
  sistemaEquivSinPrimosMalosAux sist [] = sist
  sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)
   where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

  -- Esto no es facil.

  -- en la clausula where, desdoblo el sistema en cada recursion para cada numero primo p. Entonces la logica que implementa la funcion esta trabajando
  -- con las dos partes de los sistemas por separado. Los que tienen modulo como potencia de p y los que no. Respectivamente pri y seg.
  -- Entonces, para cada primo luego de que se genere el desdoblamiento añado al sistema resultante LA RESOLUCION de los sistemas con modulos DE potencias
  -- de primos (PRI) y vuelvo a llamar a la funcion con la segunda parte del sistema que no tiene los modulos potencias del primo en la recursion.
  -- De esta forma estoy simplificando TODAS las soluciones a una ecuacion que contiene a los modulos que son potencias de un mismo primo.

  -- Mut importante que los primos que se evaluan son los PRIMOS MALOS, es decir, lo numero que estan moelstadno a la hora de aplicar el TCR. Entonces
  -- al desdoblar TODAS las ecuacion del sistema con estos primos malos y dejando solo la expresion equivalente obtengo el sistema equivalente sin ninguno
  -- de los primos malos. Que es presisamente lo que estoy buscando para poder dar la solucion aplicando el TCR.

  sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
  sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)

  -- Aca paso lo mas importante. Analizo cada ecuacion, la desdoblo en funcion de los primos malos y simplifico las llamadas "primeras partes" de cada sistemas
  -- en una unica ecuacion, obteniendo la minima expresion equivalente del sistema dado sin los primos malos. Osea, con modulos coprimos dos a dos.

  -- 6 -------------------

  solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
  solucSistemaModCoprimos [e] = e
  solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
   where (d, s, t) = emcd m1 m2
         r = mod (r1*t*m2 + r2*s*m1) (m1*m2)

  -- voy resolviendo de a dos ecuaciones hasta llegar a la solucion

  solucSistema :: [(Int, Int, Int)] -> (Int, Int)
  solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )

  -- Listo. El trabajo mas interesante fie el desdoblamiento del sistema considerando la lista de primos malos.
  -- Resumen:
  -- primera funcion: deja expresada cada ecuacion en una euivalente. Coprimiza cada ecuacion
  -- segunda funcion: la parte mas copleja. Genera el desdoblamiento en dos grupos de ecuacion, retornando un sistema equivalente sin primos malos,
  --                  dejando asi la posibilidad de aplicar el TCR
  -- tercera funcion: ya dadas todas las condiciones, aplica el TCR de a dos ecuaciones, hasta llegar a la expresion final.
