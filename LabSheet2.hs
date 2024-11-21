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

insert :: Ord a => a -> [a] -> [a]
insert num [] = [num]
insert num nums | num < head nums = num : nums
                | otherwise       = head nums : insert num (tail nums)

-- Insertion sort
isort :: Ord a => [a] -> [a]
-- Recursive definition
-- isort [] = []
-- isort (x:xs) = insert x (isort xs)
isort = foldr insert []

-- Merges two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a b | head a > head b = head b : merge a (tail b)
          | otherwise       = head a : merge (tail a) b

rotor :: Int -> [Char] -> [Char]
rotor shift txt | shift < 0  = error "Too damn small"
                | shift > 25 = error "Too damn big"
                | otherwise  =  drop shift txt ++ take shift txt

makeKey :: Int -> [(Char, Char)]
makeKey shift = zip ['A'..'Z'] (rotor shift ['A'..'Z'])

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c dict | c `elem` [key | (key, val) <- dict] = head [val | (key, val) <- dict, key == c]
              | otherwise = c

encipher :: Int -> Char -> Char
encipher shift c = lookUp c (makeKey shift)

normalise :: [Char] -> [Char]
normalise [] = []
normalise (s:ss) | s `elem` ['0'..'9'] || toUpper s `elem` ['A'..'Z'] = toUpper s : normalise ss
                 | otherwise = normalise ss

encipherStr :: Int -> [Char] -> [Char]
encipherStr shift [] = []
encipherStr shift txt = [encipher shift c | c <- normalise txt]

