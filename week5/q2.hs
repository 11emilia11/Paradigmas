  
-- Crie um programa em Haskell que leia um arquivo "romano.txt" 
--onde cada linha possui um número romano e deve gerar um arquivo "arabico.txt"
-- que contém a conversão de cada número romano do arquivo de entrada em
-- um número arábico (sistema numérico que utilizamos) ao longo de suas linhas.
--Os romanos escreviam números usando as letras I, V, X, L, C, D, M.
-- Assuma que os números a serem convertidos vão de 0 até 3000.
-- Pesquise na Internet as regras para as numerações romanas 
--(por exemplo, em http://www.novaroma.org/via_romana/numbers.html). 
--Abaixo segue algumas conversões:
--O número 1990 é MCMXC: 
--1000=M 900=CM 90=XC
--2008 é escrito como MMVIII:
--2000=MM 8=VIII
--Dica: implemente uma função "numeral" que talvez (tipo Maybe) converta um numeral
-- romano para uma String representando o número arábico.

import System.IO     
import Data.Char  

ehRomano :: Char -> Bool
ehRomano c | c == 'I' || c == 'V' || c == 'X' || c == 'L' || c == 'C' || c == 'D' || c == 'M' = True
           | otherwise = False 

valido s = foldr1 (&&) (map ehRomano s)

arabico 'I' = 1
arabico 'V' = 5
arabico 'X' = 10
arabico 'L' = 50
arabico 'C' = 100
arabico 'D' = 500
arabico 'M' = 1000

converte:: [Char] -> Int
converte "I" = arabico 'I' 
converte "II" = arabico 'I' + arabico 'I'
converte "IV" = -1 + arabico 'V'
converte "V" = arabico 'V'
converte "IX" = -1 + arabico 'X' 
converte "X" = arabico 'X'
converte "XL" = -10 + arabico 'L'
converte "L" = arabico 'L'
converte "XC" = -10 + arabico 'C'
converte "C" = arabico 'C'
converte "CD" = -100 + arabico 'D'
converte "D" = arabico 'D'
converte "CM" = -100 + arabico 'M'
converte "M" = arabico 'M'
converte (x:xs) = arabico x + converte xs

    
main = do     
    contents <- readFile "romano.txt"     
    writeFile "arabico.txt" (show(converte contents))
    putStr (contents)

                   



