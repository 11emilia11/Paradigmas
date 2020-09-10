--Uma lista é uma sublista de outra se os elementos da primeira ocorrem na
-- segunda seguindo a mesma ordem. Por exemplo, "ship" é uma sublista de 
--"fish and chips", mas não de "hippies". Já uma lista é uma subsequência de 
--outra se a primeira ocorre como uma sequência de elementos dentro da outra. 
--Por exemplo, "chip" é uma subsequência de "fish and chips", mas não de "chin up".
-- Defina duas funções, uma para verificar se uma string é sublista de outra 
--(sublista :: String -> String -> Bool), e a segunda para verificar se uma string
-- é uma subsequência de outra (subsequencia :: String -> String -> Bool). 

import Data.List

sublista :: String -> String -> Bool
sublista l1 l2 = elem l1 (subsequences l2)

subsequencia :: String -> String -> Bool
subsequencia l1 l2 = isInfixOf l1 l2

