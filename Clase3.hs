par :: Integer -> Bool
par x | mod x 2 == 0 = True
par x | mod x 2 == 1 = False

doblefact :: Integer -> Integer
doblefact n | n == 0 = 1
doblefact n | n > 0 = n * doblefact(n-2)

inv :: Float -> Float
inv x | x /= 0 = 1/x

igualdad :: Integer -> Integer -> Integer
igualdad x y | y == 0 || x == y = 1
			| otherwise = (igualdad (x-1) y) + (igualdad (x-1) (y-1))

simcp :: Integer -> Integer
simcp x | x==1 = 0
simcp x |otherwise = tubiega x x

tubiega :: Integer -> Integer -> Integer
tubiega 1 _ = 1
tubiega x y | ((x > 0) && (not (par x)) && ((x^2) < y)) = (x + (tubiega (x-2) y))
tubiega x y | not (par x) = (tubiega (x-2) y)
tubiega x y | otherwise = (tubiega (x-1) y)