data Resultado = UnResultado
    { partido        :: Partido
    , votos      :: Int
    , distrito :: Distrito
    } deriving Show

type Partido = String
type Distrito = String


totalVotos :: [Resultado] -> Partido -> Int
totalVotos r p = sum (map votos (filter ((==p).partido) r))
 

--------Idea de orden superrior "basica"------------
votosTotales :: [Resultado] -> Int
votosTotales res = sum ( map votos res)

consejalesxPartido :: [Resultado] -> [Int]
consejalesxPartido res = map cantConsejales res


cantConsejales :: Resultado -> Int
cantConsejales rdo = div (votos rdo) 100


descripciones :: [Resultado] -> [String]
descripciones res = map nn res

nn :: Resultado -> String
nn rdo = distrito rdo ++ "-" ++ partido rdo

cuantosAltos :: [Resultado] -> Int
cuantosAltos res = length (filter alto res)

alto :: Resultado -> Bool
alto rdo = votos rdo > 400

cuantosCumplen :: [a] -> (a -> Bool) -> Int
cuantosCumplen lista f = length (filter f lista)
---------------------------------------------------------------
 
-- Aplicacion parcial

fraude :: Int -> Resultado -> Resultado
fraude n (UnResultado p v d) = UnResultado p (n*v) d

fraudeGeneral :: Int -> [Resultado] -> [Resultado]
fraudeGeneral n res = map (fraude n) res

