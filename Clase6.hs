type Posicion = ( Integer , Integer )
data Direccion = Norte | Este | Sur | Oeste deriving (Show)
type Tortuga = ( Posicion , Direccion )

arrancar :: Tortuga
arrancar = ((0,0), Sur)

derecha :: Direccion -> Direccion
derecha Norte = Este
derecha Este = Sur
derecha Sur = Oeste
derecha Oeste = Norte 

girarDerecha :: Tortuga -> Tortuga
girarDerecha (pos, dir) = (pos, derecha dir)

avanzar :: Tortuga -> Integer -> Tortuga
avanzar ((x, y), Norte) av = ((x , y + av), Norte)
avanzar ((x, y), Este) av = ((x + av, y), Este)
avanzar ((x, y), Sur) av = ((x , y - av), Sur)
avanzar ((x, y), Oeste) av = ((x - av, y), Oeste)