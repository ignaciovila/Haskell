division :: Integer -> Integer -> (Integer, Integer)
division a b | a < b = (0, a)
division a b | otherwise = ((fst (division (a-b) b) + 1), (snd (division (a-b) b)))

divParcial :: Integer -> Integer -> [Integer]
divParcial a b = divAux b a

divisores :: Integer -> [Integer]
divisores a = divAux a a

divAux :: Integer -> Integer -> [Integer]
divAux 0 _ = []
divAux a b | ((mod b a) == 0) = a : (divAux (a-1) b)
divAux a b | otherwise = divAux (a-1) b

esPrimo :: Integer -> Bool
esPrimo a = length (divisores a) == 2

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

-- euclides :: Integer -> Integer -> (Integer, Integer)
-- euclides a b = (1, div ((mcd a b) - a) b)