-- Crie uma função em Haskell alocar::[Int]->Int->[[Int]] que 
--recebe uma lista de inteiros e um inteiro n e ela deve retornar 
--uma lista com listas de n elementos de forma que os elementos
--nas sublistas são inseridos de forma consecutiva varrendo os
-- elementos da lista principal. Por exemplo: 
--alocar [4,23,2,5,6,8,12,57] 3 = [[4,23,2],[5,6,8],[12,57]] 



alocar :: [Int] -> Int -> [[Int]]
alocar l 0 = [[]] 
alocar [] _ = []
alocar (x:xs) 1 = [[x]] ++ alocar xs 1
alocar l 2 = [take (2) (l)] ++ alocar (drop (2) (l)) 2
alocar l n = [take (n) (l)] ++ alocar (drop (n) (l)) (n)
