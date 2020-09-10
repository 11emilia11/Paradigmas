data Estacao = Inverno | Verao | Primavera | Outono
data Temperatura = Frio | Quente deriving (Show)

clima :: Estacao -> Temperatura
clima Inverno = Frio
clima _ = Quente


type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

mostraPessoa :: Pessoas -> String
mostraPessoa (Pessoa n i) = n ++ "-" ++ (show i)

data Forma = Circulo Float | Retangulo Float Float | Ponto Int Int

ehRedondo :: Forma -> Bool
ehRedondo (Circulo _) = True
ehRedondo (Retangulo _ _) = False
ehRedondo (Ponto _ _) = False

area :: Forma -> Maybe Float
area (Circulo r) | r < 0 = Nothing
                 | otherwise = Just (pi*r^2)
area (Retangulo l h) | l < 0 || h < 0 = Nothing  
                     | otherwise = Just (l*h)
area (Ponto x y) = Just 0.0 

getN (Just n) = n


data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
avalia :: Expr -> Int
avalia (Lit x) = x
avalia (Add e1 e2) = avalia(e1) + avalia(e2)
avalia (Sub e1 e2) = avalia(e1) - avalia(e2)


data ArvoreBin t = NilArv | No t (ArvoreBin t) (ArvoreBin t) deriving (Show)





