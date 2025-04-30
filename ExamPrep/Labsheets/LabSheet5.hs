type Board = [Int]
-- board - [1,3,5,7]
displayBoard :: Board -> IO()
displayBoard = print

emptyBoard :: Board -> Bool
emptyBoard = foldr ((&&) . (==0)) True

makeMove :: Board -> Int -> Int -> Board
makeMove board row howMany = take row board ++ [board !! row - howMany] ++ drop (row + 1) board

isValidInt' :: String -> Bool
isValidInt' "" = True
isValidInt' (s:ss) = s `elem` ['0'..'9'] && isValidInt' ss

isValidInt :: String -> Bool
isValidInt (s:ss) = (s `elem` '-' : ['0'..'9']) && isValidInt' ss

sReadInt :: String -> IO Int
sReadInt msg = do putStrLn msg
                  x <- getLine
                  if isValidInt x then
                    return (read x :: Int)
                  else
                    sReadInt msg

play :: Board -> Bool -> IO()
play board isP1 = do displayBoard board
                     if isP1 then putStrLn "player1's turn" else putStrLn "player2's turn"
                     row <- sReadInt "Enter heap number: "
                     howMany <- sReadInt "Enter how many to take:"
                     let newBoard = makeMove board row howMany
                     if emptyBoard newBoard then
                        if isP1 then
                            putStrLn "player2 wins"
                        else
                            putStrLn "player1 wins"
                     else
                        play newBoard (not isP1)

nimGame :: IO()
nimGame = do putStrLn "Welcome to the game of nim"
             play [1,3,5,7] True