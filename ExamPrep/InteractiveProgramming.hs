putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x 
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' x = do putStr' x
                 putChar '\n'

strLen' :: IO ()
strLen' = do putStr' "Enter a string: "
             xs <- getLine
             putStr' "The string has "
             putStr' (show (length xs))
             putStrLn' " characters"