squares :: [Int] -> [Int]
squares = map square
    where square x = x*x

sumSquares :: [Int] -> Int
sumSquares xs = sum $ map square xs
    where square x = x^2

allPositive :: [Int] -> Bool
allPositive = foldr ((&&) . (>0)) True