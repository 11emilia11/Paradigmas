--Um player de músicas digitais precisa ranquear as músicas de acordo com
-- os seus nomes. O ranking de cada música consiste na soma dos valores de
-- cada caractere alfabético do nome de acordo com seus valores na tabela ASCII.
-- Por exemplo, “olhar 43” deve retornar 111(o) + 108(l) + 104(h) + 97(a) + 114(r)
-- = 534. Desta forma, crie um programa em Haskell que dada uma lista de strings 
--(lista com nomes das músicas) retorne uma lista com os rankings de cada uma das
-- músicas descritas na lista de entrada. 
--OBS: Lembre-se que as funções isAlpha e ord já estão disponíveis, 
--onde a primeira retorna um booleano True caso o caractere passado como 
--parâmetro seja alfabético e False caso contrario, enquanto que a segunda retorna o valor ordinal de
-- um caractere de acordo com a tabela ASCII.



import Data.Char

rankingMusica :: String -> Int 
rankingMusica [] = 0
rankingMusica (x:xs) | isAlpha x = ord x + rankingMusica xs
                     | otherwise = rankingMusica xs

mapeiaRanking :: [String] -> [Int]
mapeiaRanking s = map rankingMusica s

