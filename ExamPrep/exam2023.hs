divides :: Int -> Int -> Bool
a `divides` b = b `mod` a == 0

prime :: Int -> Bool
prime n = not (foldr1 (||) (map (`divides` n) [2..n-1]))

cube :: Num a => a -> a
cube a = a * a * a

mySum :: Num a => [a] -> a
mySum [] = 0
mySum xs = foldr (+) 0 (map cube xs)

insert :: Int -> [Int] -> [Int]
insert y xs = [a | a <- xs, a <= y] ++ [y] ++ [a | a <- xs, a > y]

insert' :: Int -> [Int] -> [Int]
insert' y [] = [y]
insert' y (x:xs) | x < y = [x] ++ (insert' y xs)
                 | otherwise = y : (x : xs)

tailSumTo :: Int -> Int -> Int
tailSumTo 1 acc = acc + 1
tailSumTo x acc = tailSumTo (x - 1) (acc + x)