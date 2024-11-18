import Data.Char

square :: Int -> Int
square x = x ^ 2

pyth :: Int -> Int -> Int
pyth x y = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = pyth a b == square c

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny a b c = isTriple a b c || isTriple b c a || isTriple c a b

halfEvens :: [Int] -> [Int]
halfEvens a = [if even x then div x 2 else x | x <- a]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b list = [x | x <- list, x >= min a b && x <= max a b]

countPositives :: [Int] -> Int
countPositives list = length [1 | x <- list, x > 0]

capitalised :: String -> String
capitalised (h : hs) = toUpper h : [toLower x | x <- hs]

lowerWord :: String -> String
lowerWord word = [toLower c | c <- word]

title :: [String] -> [String]
title [] = []
title (h : words) = capitalised h : [if length word > 3 then capitalised word else lowerWord word | word <- words]

safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

-- safeTail xs | null xs = []
--             | otherwise tail xs

-- safeTail [] = []
-- safeTail (x : xs) = xs
