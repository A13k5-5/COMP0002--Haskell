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

