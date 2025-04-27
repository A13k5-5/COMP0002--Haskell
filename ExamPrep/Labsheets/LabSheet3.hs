mult :: Num a => [a] -> a
mult [] = 0
mult xs = foldr (*) 1 xs

posList :: [Int] -> [Int]
posList = filter (>0)

trueList :: [Bool] -> Bool
trueList = foldr (&&) True

evenList :: [Int] -> Bool
evenList xs = length (filter even xs) == length xs

evenList' :: [Int] -> Bool
evenList' = foldr ((&&) . even) True

maxList :: Ord a => [a] -> a
maxList (x:xs) = foldr max x xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi = filter (\x -> x >= lo && x <= hi)

countPositives :: [Int] -> Int
countPositives xs = length (filter (>0) xs)

myLength :: [a] -> Int
myLength = foldr ((+) . (\x -> 1)) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []