import Data.Char
square :: Int -> Int
square a = a * a

pyth :: Int -> Int -> Int
pyth a b = square a + square b

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square c == pyth a b

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c = isTriple a b c || isTriple b c a || isTriple c a b

halfEvens :: [Int] -> [Int]
halfEvens [] = []
halfEvens (x:xs) = if even x then div x 2 : halfEvens xs else x: halfEvens xs

halfEvens' :: [Int] -> [Int]
halfEvens' xs = [if even x then div x 2 else x | x <- xs]

inRange :: Int -> Int -> [Int] -> [Int]
inRange low high = filter (\x -> (x <= high) && (x >= low)) 

inRange' :: Int -> Int -> [Int] -> [Int]
inRange' low high xs = [x | x <- xs, x >= low, x <= high]

countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

capitalised :: [Char] -> [Char]
capitalised (s:ss) = toUpper s : [toLower s | s <- ss]

lowerWord :: [Char] -> [Char]
lowerWord w = [toLower c | c <- w]

title :: [[Char]] -> [[Char]]
title [] = []
title (w : ws) = capitalised w : [if length x > 3 then capitalised x else lowerWord x | x <- ws]