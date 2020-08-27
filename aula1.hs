
-- Exercicios da aula 1 

resposta :: Int
resposta = 42

maiorQue :: Bool
maiorQue = resposta > 71

sim :: Bool
sim = True

quadrado :: Int -> Int
quadrado x = x * x

tudoIgual :: Int -> Int -> Int -> Bool
tudoIgual x y z = (x == y) && (y == z)

maxi :: Int -> Int -> Int
maxi x y | x >= y = x
         | x == y = x
         | otherwise = y


maxi2 x y = if x > y then
               x
            else
               if x == y then
                  x
               else
                  y

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 10
vendas 2 = 5
vendas 3 = 8
vendas 4 = 10
vendas 5 = 5



-- total de vendas por casamento de padrao
totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas(n-1)



totalVendas2 :: Int -> Int
totalVendas2 n | n == 0 = vendas 0
               | otherwise = vendas n + totalVendas2 (n-1)


mediaSemanas n = fromIntegral(totalVendas n) / fromIntegral(n+1)

-- quantas vendas foram iguais a s no intervalo de 0 a n
vendasIguais :: Int -> Int -> Int 
vendasIguais s n | n == 0 = sumTrue(vendas 0 == s)
                 | n == 1 = sumTrue(vendas 1 == s) + sumTrue(vendas 0 == s)
                 | otherwise =  vendasIguais(s)(n - 1) + sumTrue(vendas n == s) 





somaTupla :: (Int, Int) -> Int
somaTupla x = fst x + snd x

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myNot2 :: Bool -> Bool
myNot2 x | x == True = False
         | otherwise = True


myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr False x = x

myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False x = False

fat :: Int -> Int
fat 0 = 0
fat 1 = 1
fat n = n * fat(n-1)


all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d | (a == b) && (c == d) && (b == d) = True
                  | otherwise = False

all3Equal :: Int -> Int -> Int -> Bool
all3Equal a b c | (a == b) && (b == c) = True
                | otherwise = False


equalCount :: Int -> Int -> Int -> Int
equalCount a b c |  all3Equal a b c == True = 3
                 | (a /= b) && (b /= c) && (a /= c) = 0
                 | (a /= b) && (b == c) = 2
                 | (a == c) && (a /= b) = 2
                 | (a == b) && (b /= c) = 2



media :: Int -> Int -> Int -> Int
media a b c = div(a + b + c) (3)


sumTrue :: Bool -> Int
sumTrue True = 1
sumTrue False = 0

acimaMedia :: Int -> Int -> Int -> Int
acimaMedia x y z = let m = media x y z
                       xm = sumTrue(x > m)
                       ym = sumTrue(y > m)
                       zm = sumTrue(z > m)
                    in xm + ym + zm
                       
                        

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ( r1, r2 )
               where r1 = (-b + sqrt(delta))/(2*a)
                     r2 = (-b - sqrt(delta))/(2*a)
                     delta = b^2 - 4*a*c

                     
addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | n == 1 = " " ++ ""
             | otherwise  = addEspacos(n-1) ++ " "

paraDireita :: Int -> String -> String 
paraDireita n s | n == 0 = s
                | n == 1 = addEspacos(n) ++ s
                | otherwise = addEspacos(n-1) ++ paraDireita(n-1)(s)


--menorMaior :: Int -> Int -> Int -> (Int, Int)
--menorMaior a b c | (a > b) && (a > c) && (c < b) = (c, a)
--                 | (b > a) && (b > c) && (c < a) = (c, b)




--imprimeSemanas :: Int -> String
--imprimeSemanas n | n == 0  = show(0) ++ show(vendas 0)
--                 | n == 1 =  show(0) ++ show(vendas 0) ++ show(1) ++ show(vendas 1)
--                 | otherwise = 


--imprimeTabela :: Int -> String
--imprimeTabela n =  cabecalho
--                  ++ imprimeSemanas n
--                  ++ imprimeTotal n
--                  ++ imprimeMedia n 
--             where cabecalho = "Semana " ++ "Venda"


                

