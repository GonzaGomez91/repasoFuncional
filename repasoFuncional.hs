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
 
fraude :: Int -> Resultado -> Resultado
fraude n (UnResultado p v d) = UnResultado p (n*v) d

fraudeGeneral :: Int -> [Resultado] -> [Resultado]
fraudeGeneral n res = map (fraude n) res

----------------------------------------------------------

esVpara :: Int -> (Int -> Bool) -> Bool
esVpara x f = f x

--cuantosCumplen (esVpara 3) [even, odd, 5>]


--Composicion de funciones
-- h(x) = f o g (x)   se compone poniendo "." (punto)

sig x = x+1
cuad x = x * x

poli x = (sig.cuad) x -- es equivalente a poner. poli x = sig (cuad x)

filtrarMayores n = ((n<).sig.cuad) [3,5,1,0]


-- usamos composicion en totalVotos

totalVotosC :: [Resultado] -> Partido -> Int
totalVotosC r p = (sum . map votos . filter ((p==).partido)) r

