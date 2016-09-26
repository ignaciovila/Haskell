pertenece :: Integer -> [Integer] -> Bool
pertenece x xs 	| length xs == 0 = False
				| otherwise = x == head xs || pertenece x (tail xs)

hayRepetidos :: [Integer] -> Bool
hayRepetidos xs	| length xs == 0 = False
				| otherwise = hayRepetidos (tail xs) || pertenece (head xs) (tail xs)

quitar :: Integer -> [Integer] -> [Integer]
quitar x xs	| length xs == 0 = []
			| head xs == x = tail xs
			| otherwise = head xs : quitar x (tail xs)

eliminarDuplicados :: [Integer] -> [Integer]
eliminarDuplicados xs	| length xs == 0 = []
						| pertenece (head xs) (tail xs) = eliminarDuplicados (quitar (head xs) xs)
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

type Conjunto = [Integer]

eliminarElemento :: Integer -> Conjunto -> Conjunto
eliminarElemento x xs = quitar x xs

agregarElemento :: Integer -> Conjunto -> Conjunto
agregarElemento x xs	| pertenece x xs = xs
						| otherwise = x : xs

union :: Conjunto -> Conjunto -> Conjunto
union xs ys | length xs == 0 = ys
			| otherwise = (head xs) : union (tail xs) ys

interseccion :: Conjunto -> Conjunto -> Conjunto
interseccion xs ys	| length xs == 0 = []
					| pertenece (head xs) ys = (head xs) : interseccion (tail xs) ys
					| otherwise = interseccion (tail xs) ys

inclusion :: Conjunto -> Conjunto -> Bool
inclusion xs ys | length xs == 0 = True
				| pertenece (head xs) ys = inclusion (tail xs) ys
				| otherwise = False

igualdadConjuntos :: Conjunto -> Conjunto -> Bool
igualdadConjuntos xs ys = (inclusion xs ys) && (inclusion ys xs)

diferenciaConjuntos :: Conjunto -> Conjunto -> Conjunto
diferenciaConjuntos xs ys	| length xs == 0 = []
							| pertenece (head xs) ys = diferenciaConjuntos (tail xs) ys
							| otherwise = (head xs) : diferenciaConjuntos (tail xs) ys

diferenciaSimetrica :: Conjunto -> Conjunto -> Conjunto
diferenciaSimetrica xs ys = diferenciaConjuntos (union xs ys) (interseccion xs ys)