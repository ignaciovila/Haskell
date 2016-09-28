data Vector = Vector2D Float Float | Vector3D Float Float Float
data Punto = Punto2D Float Float deriving (Show)
data Figura = Rectangulo Punto Punto | Circulo Punto Float deriving (Show)

circuloUnit :: Figura
circuloUnit = Circulo (Punto2D 0 0) 1

cuadrado :: Float -> Figura
cuadrado d = Rectangulo (Punto2D 0 0) (Punto2D (d/(sqrt 2)) (d/(sqrt 2)))

perimetro :: Figura -> Float
perimetro (Rectangulo (Punto2D x1 y1) (Punto2D x2 y2)) = 2*abs(x1-x2) + 2*abs(y1-y2)
perimetro (Circulo _ r) = 3.14*2*r

area :: Figura -> Float
area (Rectangulo (Punto2D x1 y1) (Punto2D x2 y2)) = abs((x1-x2)*(y1-y2))
area (Circulo _ r) = 3.14*r^2

data ParOrdenado a b = Par a b deriving (Show)

primero :: ParOrdenado a b -> a
primero (Par a _) = a

segundo :: ParOrdenado a b -> b
segundo (Par _ b) = b

data Lista a = ListaVacia | Agregar a ( Lista a ) deriving ( Show )

esVacia :: Lista a -> Bool
esVacia ListaVacia = True
esVacia _ = False

cabeza :: Lista a -> a
cabeza (Agregar x _) = x

cola :: Lista a -> Lista a
cola (Agregar _ xs) = xs

concatenar :: Lista a -> Lista a -> Lista a
concatenar ListaVacia l = l
concatenar (Agregar h t) l = Agregar h (concatenar t l)

longitud :: Lista a -> Integer
longitud ListaVacia = 0 
longitud (Agregar h t) = 1 + longitud t

suma :: Lista Float -> Float
suma ListaVacia = 0 
suma (Agregar h t) = h + suma t

posicion :: Lista a -> Integer -> a
posicion (Agregar h _) 1 = h
posicion (Agregar _ t) n = posicion t (n-1)

iniciales :: [ Char ] -> [ Char ] -> [ Char ]
iniciales nombre apellido = [n, a]
	where	( n : _ ) = nombre
			( a : _ ) = apellido