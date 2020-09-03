vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 5
vendas 3 = 8

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

totalVendas2 n | n == 0 = vendas 0
               | otherwise = vedas n + (totalVendas2 (n-1))

aplica2vezes :: (a -> a) -> a -> a
aplica2vezes f x = f (f x)

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z


mult10 z = (mul 10) z 1

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = (f n) + (total f (n-1))
