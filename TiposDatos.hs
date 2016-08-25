f :: Bool -> Bool
f x = not x

f2 :: Bool -> Float
f2 x = pi

funcion3 :: Integer -> Integer -> Bool -> Bool
funcion3 x y b = b || ( x > y)

f3 :: a -> a
f3 x = x

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia p q = sqrt((fst p - fst q)^2 + (snd p - snd q)^2)

-- head devuelve el primer elementos
-- tail devuelve todos los elementos menos el primero
-- (:) agrega al principio
-- (++) concatena dos listas
-- length	devuelve la cantidad de elementos
-- reverse	invierte el orden

listar :: Integer -> Integer -> Integer -> [Integer]
listar a b c = [a,b,c]