module Clase11
where

  type Complejo = (Float,Float)

  re :: Complejo -> Float
  re (a,_) = a

  im :: Complejo -> Float
  im (_,b) = b

  conjugado :: Complejo -> Complejo
  conjugado (a,b) = (a,-b)

  suma :: Complejo -> Complejo -> Complejo
  suma (a,b) (c,d) = (a+c,b+d)

  producto :: Complejo -> Complejo -> Complejo
  producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)


  --para hacer potencias de float, usar **
  inverso :: Complejo -> Complejo
  inverso (a,b) = (a/(a**2 + b**2),(-b)/(a**2 + b**2))


  modulo :: Complejo -> Float
  modulo (a,b) = sqrt(a**2 + b**2)

  argumento :: Complejo -> Float
  argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                  |cuadrante (a,b) == 2 = pi + (atan (b/a))
                  |cuadrante (a,b) == 3 = pi + (atan (b/a))
                  |otherwise = 2*pi + (atan (b/a))

  cuadrante :: Complejo -> Int
  cuadrante (a,b) |a>=0 && b>=0 = 1
                  |a<=0 && b>=0 = 2
                  |a<0 && b<=0 = 3
                  |a>=0 && b<=0 = 4

  pasarACartesianas :: Float -> Float -> Complejo
  pasarACartesianas r tita = (r*(cos tita),r*(sin tita))

  raizCuadrada :: Complejo -> (Complejo,Complejo)
  raizCuadrada z=(pasarACartesianas r tita ,pasarACartesianas r (tita+pi))
                    where r = sqrt (modulo z)
                          tita = (argumento z)/2

  raicesNEsimas :: Integer -> [Complejo]
  raicesNEsimas n = raicesNEsimasDesde 0 n

  raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
  raicesNEsimasDesde k n | k>= n = []
                         |otherwise =(kesimaRaiz):(raicesNEsimasDesde (k+1) n)
                         where kesimaRaiz =(cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))







































--


--
