data BaseNucleotidica = A | C | G | T | U deriving Show
type CadenaDNA = [BaseNucleotidica]
type CadenaRNA = [BaseNucleotidica]
type Codon = (BaseNucleotidica,BaseNucleotidica, BaseNucleotidica)
data Aminoacido = Phe | Ser | Tyr | Cys | Leu | Trp | Pro | His | Arg | Gln | Ile | Thr | Asn | Lys | Met | Val | Ala | Asp | Gly | Glu deriving Show
type Proteina = [Aminoacido]

complementarBase :: BaseNucleotidica -> BaseNucleotidica
complementarBase A = T
complementarBase T = A
complementarBase C = G
complementarBase G = C

complementarCadenaDNA :: CadenaDNA -> CadenaDNA
complementarCadenaDNA [] = []
complementarCadenaDNA [a] = [(complementarBase a)]
complementarCadenaDNA xs = complementarBase (head xs) : complementarCadenaDNA (tail xs)

obtenerCadenaReverseDNA :: CadenaDNA -> CadenaDNA
obtenerCadenaReverseDNA [] = []
obtenerCadenaReverseDNA xs =  obtenerCadenaReverseDNA (tail xs) ++ [(head xs)]

auxTrans :: CadenaDNA -> CadenaRNA
auxTrans [] = []
auxTrans (T : xs) = U : (auxTrans xs)
auxTrans (n : xs) = n : (auxTrans xs)

transcribir :: CadenaDNA -> CadenaRNA
transcribir xs = auxTrans (complementarCadenaDNA xs)