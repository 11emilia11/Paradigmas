-- Dada as funções "vendas" e "totalVendas" que utilizamos nas aulas desta semana
-- (videoaula e slides), crie uma função "relatorio::Int -> (Int, Int, Int, Float)"
-- que retorna um relatório dos dados até uma determinada semana passada como parâmetro.
-- O relatório é uma tupla onde o primeiro elemento é o total de vendas até aquela semana,
-- o segundo é o número da semana com mais vendas até aquela semana, o terceiro é a maior
-- quantidade de vendas até aquela semana, e o quarto é a média de vendas até aquela semana. 
--Dica: use funções intermediárias para calculcar o que se deseja. 

-- funcoes auxiliares

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 10
vendas 2 = 5
vendas 3 = 8
vendas 4 = 10
vendas 5 = 5

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas(n-1)

maxi :: Int -> Int -> Int
maxi x y | x >= y = x
         | x == y = x
         | otherwise = y


maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = maxi(maxVendas (n-1)) (vendas n)

maiorSemana :: Int -> Int
maiorSemana s | s == 0 = 0
              | vendas s == maxVendas s = s
              | otherwise = maiorSemana(s - 1)
              

mediaSemanas n = fromIntegral(totalVendas n) / fromIntegral(n+1)


-- funcao principal
relatorio :: Int -> (Int, Int, Int, Float)
relatorio s = (x, y, z, w)
           where x = totalVendas s
                 y = maiorSemana s
                 z = maxVendas s 
                 w = mediaSemanas s









 