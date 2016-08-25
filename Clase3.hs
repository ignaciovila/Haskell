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

simp :: Integer -> Integer
simp x | x==1 = 0
simp x |otherwise = ej4 x x

ej4 :: Integer -> Integer -> Integer
ej4 1 _ = 1
ej4 x y | ((x > 0) && (not (par x)) && ((x^2) < y)) = (x + (ej4 (x-2) y))
ej4 x y | not (par x) = (ej4 (x-2) y)
ej4 x y | otherwise = (ej4 (x-1) y)