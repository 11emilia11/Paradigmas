import Data.Char

somaLista::[Int] -> Int
somaLista [] = 0
somaLista (a:as) = a + somaLista as


sumPairs :: [(Int, Int)] -> [Int]
sumPairs l = [x+y | (x,y) <- l]



maiorElem::[Int] -> Int
maiorElem l = maximum l

dobraLista::[Int] -> [Int]
dobraLista [] = []
dobraLista (a:as) = (2*a):(dobraLista as)

dobraListaPares l = [ 2*y | y <- l, even y]

member :: [Int] -> Int -> Bool
member l x = elem x l

--membro :: [Int] -> Int -> Bool
--membro l x = [ elem y l |  <- l,  


digits :: String -> String
digits s = [ y | y <- s , ('0'<= y) && (y <= '9') ]

digits2 :: String -> String
digits2 s = [y | y <- s, isDigit y]

takes :: Int -> [t] -> [t]
takes n (a:as) | n == 1 = [a]
               | otherwise = (a):(takes (n-1)(as))

drops :: Int -> [Int] -> [Int]
drops n (a:as) | n == 1 = as
               | otherwise = drops(n-1) (as)
               
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre", "Duna"),("Fernando", "Jonathan Strange"),("Fernando", "Duna")]

takeWhile2 c (a:as) | c a = [a] ++ takeWhile2 (c)(as)
                    | otherwise = []



dropWhile2 c (a:as) | c a = dropWhile (c)(as)
                    | otherwise = [a] ++ dropWhile2 (c)(as)


livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [ y | (x,y) <- bd, x == p ]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [x | (x,y) <- bd, y == l ]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l =  head [x /= "" | (x,y)<- bd, y == l]

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length [ x | (x,y) <- bd, x == p]

--emprestar :: BancoDados -> Pessoa -> Livro -> BandoDados
--emprestar bd p l = [ ]

-- devolver :: BandoDados -> Pessoa -> Livro -> BancoDados

