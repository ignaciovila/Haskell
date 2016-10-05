potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia x y = x * potencia x (y-1)

potenciaDePotencia :: Integer -> Integer -> Integer
potenciaDePotencia _ 0 = 1
potenciaDePotencia x y = x ^ potenciaDePotencia x (y-1)

acker :: Integer -> Integer -> Integer
acker 0 n = n + 1
acker m 0 = acker (m-1) 1
acker m n = acker (m - 1) (acker m (n-1))

substring :: String -> Integer -> Integer -> String
substring xs 1 0 = []
substring xs 1 y = head xs : substring (tail xs) 1 (y-1)
substring xs x y = substring (tail xs) (x-1) y

replicarAux :: Char -> Integer -> String
replicarAux _ 0 = []
replicarAux c n = c : replicarAux c (n-1)

replicar :: String -> Integer -> String
replicar []  _ = []
replicar xs n = (replicarAux (head xs) n) ++ (replicar (tail xs) n)

data Estado = Encendido | Apagado deriving Show
type Canal = Integer
type Volumen = Integer
type Televisor = ( Estado , Canal , Volumen )

nuevoTelevisor :: Televisor
nuevoTelevisor = (Apagado, 1, 0)

powerTelevisor :: Televisor -> Televisor
powerTelevisor (Apagado, c, v) = (Encendido, c, v)
powerTelevisor (Encendido, c, v) = (Apagado, c, v)

subirVolumen :: Televisor -> Televisor
subirVolumen (e, c, 10) = (e, c, 10)
subirVolumen (e, c, v) = (e, c, v+1)

bajarVolumen :: Televisor -> Televisor
bajarVolumen (e, c, 0) = (e, c, 0)
bajarVolumen (e, c, v) = (e, c, v-1)

subirCanal :: Televisor -> Televisor
subirCanal (e, 7, v) = (e, 1, v)
subirCanal (e, n, v) = (e, n+1, v)

bajarCanal :: Televisor -> Televisor
bajarCanal (e, 1, v) = (e, 7, v)
bajarCanal (e, n, v) = (e, n-1, v)