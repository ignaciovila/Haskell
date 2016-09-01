pertence :: Integer -> [Integer] -> Bool
pertence x xs 	| length xs == 0 = False
				| otherwise = x == head xs || pertence x (tail xs)

hayRepetidos :: [Integer] -> Bool
hayRepetidos xs	| length xs == 0 = False
				| otherwise = hayRepetidos (tail xs) || pertence (head xs) (tail xs)

quitar :: Integer -> [Integer] -> [Integer]
quitar x xs	| length xs == 0 = []
			| head xs == x = tail xs
			| otherwise = head xs : quitar x (tail xs)

eliminarDuplicados :: [Integer] -> [Integer]
eliminarDuplicados xs	| length xs == 0 = []
						| pertence (head xs) (tail xs) = eliminarDuplicados (quitar (head xs) xs)
						| otherwise = head xs : eliminarDuplicados (tail xs)


maximo :: [Integer] -> Integer
maximo xs 	| length xs == 1 = head xs
			| (head xs) > (head (tail xs)) = maximo (head xs : tail (tail xs))
			| otherwise = maximo (tail xs)

ordenar :: [Integer] -> [Integer]
ordenar xs	| length xs == 0 = []
			| otherwise = maximo xs : ordenar(quitar(maximo xs) xs)

invertir :: [Integer] -> [Integer]
invertir xs | length xs == 0 = []
			| otherwise = invertir(tail xs) ++ [head xs]

suma :: [Integer] -> [Integer] -> [Integer]
suma xs ys 	| length xs == 0 = []
			| otherwise = (head xs + head ys) : suma (tail xs) (tail ys)