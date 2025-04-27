inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi [] = []
inRange lo hi (x:xs) = if x >= lo && x <= hi then x:inRange lo hi xs else inRange lo hi xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)
    where insert' x [] = [x]
          insert' x xs = if x <= head xs then x : xs else head xs : insert' x (tail xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys