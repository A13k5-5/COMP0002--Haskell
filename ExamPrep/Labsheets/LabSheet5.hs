type Board = [Int]
-- board - [1,3,5,7]
displayBoard :: Board -> IO()
displayBoard = print

emptyBoard :: Board -> Bool
emptyBoard = foldr ((&&) . (==0)) True

makeMove :: Board -> Int -> Int -> Board
makeMove board row howMany = take row board ++ [board !! row - howMany] ++ drop (row + 1) board

play :: Board -> Bool -> IO()
play board isP1 = do displayBoard board
                     if isP1 then putStrLn "player1's turn" else putStrLn "player2's turn"
                     putStr "Enter heap number: "
                     heap <- getLine
                     let row = (read heap :: Int)
                     putStr "Enter how many to take: "
                     num <- getLine
                     let howMany = (read num :: Int)
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