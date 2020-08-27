-- Cria em Haskell uma função "mdc" 
-- que recebe dois inteiros e calcula o máximo divisor comum deles.

mdc :: Int -> Int -> Int
mdc a b | mod (a) (b) > 0 = mdc b (mod (a) (b))
        | otherwise = b
