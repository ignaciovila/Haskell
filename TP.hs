module TP where

-- Tipo enumerado de bases nucleotidicas
data BaseNucleotidica = A | C | G | T | U deriving Show

-- Se asume que en una CadenaDNA no hay ninguna BaseNucleotidica de Uracilo (U)
type CadenaDNA = [BaseNucleotidica]

-- Se asume que en una CadenaDNA no hay ninguna BaseNucleotidica de Timina (T)
type CadenaRNA = [BaseNucleotidica]

-- Un codon son tres bases contiguas; se asume que un codon tiene solo bases
-- A, C, G y U
type Codon = (BaseNucleotidica, BaseNucleotidica, BaseNucleotidica)

-- Tipo enumerado para aminoacidos
data Aminoacido = Phe | Ser | Tyr | Cys | Leu | Trp | Pro | His | Arg | Gln |
    Ile | Thr | Asn | Lys | Met | Val | Ala | Asp | Gly | Glu deriving (Show, Eq)

-- Una proteina es una lista de aminoacidos
type Proteina = [Aminoacido]

-- Dada una BaseNucleotidica devuelve la base complementaria; se indefine para U
complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A
complementarBase C = G
complementarBase G = C

-- Dada una CadenaDNA devuelve la cadena complementaria
complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA xs = complementarBase (head xs) : complementarCadenaDNA (tail xs)

-- Dada una CadenaDNA devuelve la cadena reverse
obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA xs =  obtenerCadenaReverseDNA (tail xs) ++ [(head xs)]

-- Dada una CadenaDNA devuelve la cadena de RNA resultante de una transcripcion
transcribir :: CadenaDNA -> CadenaRNA
transcribir xs = auxTrans (complementarCadenaDNA xs)

-- Funcion auxiliar para reemplazar las T por U
auxTrans :: CadenaDNA -> CadenaRNA
auxTrans [] = []
auxTrans (T : xs) = U : (auxTrans xs)
auxTrans (n : xs) = n : (auxTrans xs)

sincronizaConCodonDeFin :: CadenaRNA -> Bool
sincronizaConCodonDeFin [] = False
sincronizaConCodonDeFin (x:y:z:xs) = esCodonDeFin (x,y,z) || sincronizaConCodonDeFin xs
sincronizaConCodonDeFin xs = False

obtenerProteina :: CadenaRNA -> Proteina
obtenerProteina [] = []
obtenerProteina (x:y:z:xs) = traducirCodonAAminoacido (x,y,z) : obtenerProteina xs

fraccionarCadenaRNA :: CadenaRNA -> [CadenaRNA]
fraccionarCadenaRNA [] = []
fraccionarCadenaRNA (A:U:G:xs) = xs:(fraccionarCadenaRNA xs)
fraccionarCadenaRNA (x:xs) = (fraccionarCadenaRNA xs)

-- Aux. Se asume que la cadena tiene fin
quitarCola :: CadenaRNA -> CadenaRNA
quitarCola (x:y:z:xs)	|esCodonDeFin (x,y,z) = []
						|otherwise =  [x,y,z] ++ (quitarCola xs)

limpiarLista :: [CadenaRNA] -> [CadenaRNA]
limpiarLista [] = []
limpiarLista ((x:y:z:c) : xs) 	|esCodonDeFin (x,y,z) = limpiarLista xs
								|sincronizaConCodonDeFin (x:y:z:c) = (quitarCola (x:y:z:c)) : (limpiarLista xs)
								|otherwise = limpiarLista xs
limpiarLista (c:xs) = limpiarLista xs

esCodonDeFin :: Codon -> Bool
esCodonDeFin (U,A,A) = True
esCodonDeFin (U,A,G) = True
esCodonDeFin (U,G,A) = True
esCodonDeFin _ = False

-- Asume que la cadena es información sin codones
obtenerProteinaDeRNAs :: [CadenaRNA] -> [Proteina]
obtenerProteinaDeRNAs [] = []
obtenerProteinaDeRNAs (x:xs) = (obtenerProteina x) : (obtenerProteinaDeRNAs xs)

obtenerProteinaDeRNA :: CadenaRNA -> [Proteina]
obtenerProteinaDeRNA xs = obtenerProteinaDeRNAs (limpiarLista (fraccionarCadenaRNA xs))


obtenerProteinas :: CadenaDNA -> [Proteina]
obtenerProteinas xs = (obtenerProteinaDeRNA (transcribir xs)) ++ (obtenerProteinaDeRNA (transcribir (obtenerCadenaReverseDNA xs))) ++ (obtenerProteinaDeRNA (transcribir (complementarCadenaDNA xs))) ++ (obtenerProteinaDeRNA (transcribir (obtenerCadenaReverseDNA(complementarCadenaDNA xs))))

-- Funcion que dado un codon devuelve el correspondiente aminoacido
traducirCodonAAminoacido:: Codon -> Aminoacido
traducirCodonAAminoacido (A, A, A) = Lys
traducirCodonAAminoacido (A, A, U) = Asn
traducirCodonAAminoacido (A, A, C) = Asn
traducirCodonAAminoacido (A, A, G) = Lys
traducirCodonAAminoacido (A, U, A) = Ile
traducirCodonAAminoacido (A, U, U) = Ile
traducirCodonAAminoacido (A, U, C) = Ile
traducirCodonAAminoacido (A, U, G) = Met
traducirCodonAAminoacido (A, C, A) = Thr
traducirCodonAAminoacido (A, C, U) = Thr
traducirCodonAAminoacido (A, C, C) = Thr
traducirCodonAAminoacido (A, C, G) = Thr
traducirCodonAAminoacido (A, G, A) = Arg
traducirCodonAAminoacido (A, G, U) = Ser
traducirCodonAAminoacido (A, G, C) = Ser
traducirCodonAAminoacido (A, G, G) = Arg
traducirCodonAAminoacido (U, A, U) = Tyr
traducirCodonAAminoacido (U, A, C) = Tyr
traducirCodonAAminoacido (U, U, A) = Leu
traducirCodonAAminoacido (U, U, U) = Phe
traducirCodonAAminoacido (U, U, C) = Phe
traducirCodonAAminoacido (U, U, G) = Leu
traducirCodonAAminoacido (U, C, A) = Ser
traducirCodonAAminoacido (U, C, U) = Ser
traducirCodonAAminoacido (U, C, C) = Ser
traducirCodonAAminoacido (U, C, G) = Ser
traducirCodonAAminoacido (U, G, U) = Cys
traducirCodonAAminoacido (U, G, C) = Cys
traducirCodonAAminoacido (U, G, G) = Trp
traducirCodonAAminoacido (C, A, A) = Gln
traducirCodonAAminoacido (C, A, U) = His
traducirCodonAAminoacido (C, A, C) = His
traducirCodonAAminoacido (C, A, G) = Gln
traducirCodonAAminoacido (C, U, A) = Leu
traducirCodonAAminoacido (C, U, U) = Leu
traducirCodonAAminoacido (C, U, C) = Leu
traducirCodonAAminoacido (C, U, G) = Leu
traducirCodonAAminoacido (C, C, A) = Pro
traducirCodonAAminoacido (C, C, U) = Pro
traducirCodonAAminoacido (C, C, C) = Pro
traducirCodonAAminoacido (C, C, G) = Pro
traducirCodonAAminoacido (C, G, A) = Arg
traducirCodonAAminoacido (C, G, U) = Arg
traducirCodonAAminoacido (C, G, C) = Arg
traducirCodonAAminoacido (C, G, G) = Arg
traducirCodonAAminoacido (G, A, A) = Glu
traducirCodonAAminoacido (G, A, U) = Asp
traducirCodonAAminoacido (G, A, C) = Asp
traducirCodonAAminoacido (G, A, G) = Glu
traducirCodonAAminoacido (G, U, A) = Val
traducirCodonAAminoacido (G, U, U) = Val
traducirCodonAAminoacido (G, U, C) = Val
traducirCodonAAminoacido (G, U, G) = Val
traducirCodonAAminoacido (G, C, A) = Ala
traducirCodonAAminoacido (G, C, U) = Ala
traducirCodonAAminoacido (G, C, C) = Ala
traducirCodonAAminoacido (G, C, G) = Ala
traducirCodonAAminoacido (G, G, A) = Gly
traducirCodonAAminoacido (G, G, U) = Gly
traducirCodonAAminoacido (G, G, C) = Gly
traducirCodonAAminoacido (G, G, G) = Gly