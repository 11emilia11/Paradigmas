somaLista::[Int] -> Int
somaLista [] = 0
somaLista (a:as) = a + somaLista as


sumPairs :: [(Int, Int)] -> [Int]
sumPairs l = [x+y | (x,y) <- l]
--sumPairs [(x, y)] = [x+y]


maiorElem::[Int] -> Int
maiorElem l = maximum l

dobraLista::[Int] -> [Int]
dobraLista [] = []
dobraLista (a:as) = (2*a):(dobraLista as)

dobraListaPares l = [ 2*y | y <- l, even y]

member :: [Int] -> Int -> Bool
member l x = elem x l


digits :: String -> String
digits s = [ y | y <- s , ('0'<= y) && (y <= '9') ]

takes :: Int -> [t] -> [t]
takes n (a:as) | n == 1 = [a]
               | otherwise = (a):(takes (n-1)(as))

drops :: Int -> [Int] -> [Int]
drops n (a:as) | n == 1 = as
               | otherwise = drops(n-1) (as)
               


--takeWhile 
-- dropWhile 



