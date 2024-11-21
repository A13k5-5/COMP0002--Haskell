import Data.Char

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b (x:xs)
  | a <= x && x <= b = x : inRange a b xs
  | otherwise        = inRange a b xs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x:xs)
  | x > 0     = 1 + countPositives xs
  | otherwise = countPositives xs

capitalised' :: Bool -> [Char] -> [Char]
capitalised' isFirst [] = []
capitalised' isFirst (x:xs)
  | isFirst   = toUpper x : capitalised' False xs
  | otherwise = toLower x : capitalised' False xs

capitalised :: [Char] -> [Char]
capitalised = capitalised' True

title :: [String] -> Int -> [String]
title [] i = []
title (h:hs) i  | i == 0 || length h >= 4   = capitalised h : title hs (i + 1)
                | otherwise                 = h : title hs (i + 1)

-- Merges two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a b | head a > head b = head b : merge a (tail b)
          | otherwise       = head a : merge (tail a) b


