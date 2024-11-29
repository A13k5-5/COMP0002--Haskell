squareLst :: [Int] -> [Int]
squareLst = map (^ 2)

sumSqrs :: [Int] -> Int
sumSqrs x = sum (squareLst x)

allPositives :: [Int] -> Bool
allPositives ns = length [x | x <- ns, x > 0] == length ns

minVal :: Int -> (Int -> Int) -> Int
minVal n f = minimum [f x | x <- [0..n]]

lstVals :: Int -> (Int -> Int) -> [Int]
lstVals n f = [f x | x <- [0..n]]

boolTbl :: Int -> (Int -> Int) -> [Bool]
boolTbl n f = [False | x <- lstVals n f, x /= head (lstVals n f)]

one :: Int -> Int
one x = 1

allEqual :: Int -> (Int -> Int) -> Bool
allEqual n f = null [False | x <- lstVals n f, x /= head (lstVals n f)]

