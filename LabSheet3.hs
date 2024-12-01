mult :: [Int] -> Int
mult = foldr (*) 1

posList :: [Int] -> [Int]
posList = filter (>0)

trueList :: [Bool] -> Bool
trueList = foldr (&&) True

evenList :: [Int] -> Bool
-- evenList xs = trueList $ map even xs
evenList = trueList . map even

maxList :: (Ord a) => [a] -> a
maxList xs = foldr max (head xs) xs

-- Can be also done using foldr1 - then the first element of folding is the head
-- maxList = foldr1 max

inRange :: Ord a => a -> a -> [a] -> [a]
inRange up low = filter (>=low) . filter (<=up)

-- Maks' version of inRange using a lambda function
inRange2 :: Ord a => a -> a -> [a] -> [a]
inRange2 up low = foldr (\x ans -> if x>=low && x<=up then x:ans else ans) [] 

countPositives :: [Int] -> Int
countPositives = foldr ((+) . (\ x -> if x > 0 then 1 else 0)) 0

myMap :: (a -> a) -> [a] -> [a]
myMap f = foldr ((:) . f) []

myLength' :: [a] -> Int
myLength' = foldr ((+) . const 1) 0