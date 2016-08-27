par :: Integer -> Bool
par x = mod x 2 == 0

doblefact :: Integer -> Integer
doblefact n | n == 0 = 1
doblefact n | n > 0 = n * doblefact(n-2)

inv :: Float -> Float
inv x | x /= 0 = 1/x

igualdad :: Integer -> Integer -> Integer
igualdad x y | y == 0 || x == y = 1
	| otherwise = (igualdad (x-1) y) + (igualdad (x-1) (y-1))

ej4 :: Integer -> Integer
ej4 x | x==1 = 0
ej4 x |otherwise = auxiliar x x

auxiliar :: Integer -> Integer -> Integer
auxiliar 1 _ = 1
auxiliar x y | ((x > 0) && (not (par x)) && ((x^2) < y)) = (x + (auxiliar (x-2) y))
auxiliar x y | not (par x) = (auxiliar (x-2) y)
<<<<<<< HEAD
auxiliar x y | otherwise = (auxiliar (x-1) y)
=======
auxiliar x y | otherwise = (auxiliar (x-1) y)
>>>>>>> origin/master
