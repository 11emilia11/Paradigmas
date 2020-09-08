vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 5
vendas 3 = 8

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

totalVendas2 n | n == 0 = vendas 0
               | otherwise = vendas n + (totalVendas2 (n-1))

aplica2vezes :: (a -> a) -> a -> a
aplica2vezes f x = f (f x)

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z


mult10 z = (mult 10) z 1

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = (f n) + (total f (n-1))

--maxFun :: (Int -> Int) -> Int -> Int
--maxFun f 0 = f 0
--maxFun f n = maxi (maxFun f (n-1)) (f n)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n | f n >= f (n - 1) = isCrescent f (n-1)
               | otherwise = False

double :: [Int] -> [Int]
double [] = []
double (a:x) = (2*a) : double x


sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as


pares l = filter even l




