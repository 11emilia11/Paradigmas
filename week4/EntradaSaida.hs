escreverSaidaPadrao :: String -> IO ()
escreverSaidaPadrao x = putStrLn x



put4vezes :: String -> IO ()
put4vezes x = do putStrLn x
                 putStrLn x
                 putStrLn x
                 putStrLn x

leituraEscrita = do x <- getLine
                    putStrLn x

interacao = do putStrLn "Digite o seu nome: "
               nome <- getLine
               putStr "Seu nome ao contrario eh: "
               putStrLn (reverse nome)




