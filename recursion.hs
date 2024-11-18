factorial :: Int -> Int
-- Non-recursive definition
-- factorial n = product [1 .. n]

-- Recursive definition
factorial n
  | n < 0 = error "Factorial is not defined for negative numbers"
  | n == 0 = 1
  | n > 0 = n * factorial (n - 1)

-- insert - inserts an ints into a sorted list
insert :: Int -> [Int] -> [Int]
-- Non-recursive
-- insert a b = [x | x <- b, x < a] ++ [a] ++ [x | x <- b, x >= a]

-- Recursive
insert n [] = [n]
insert n (x : xs) = if n > x then x : insert n xs else n : (x : xs)

-- insert a b
--  | null b = [a]
--  |

first :: [Int] -> Int
first [] = 0
first (x : xs) = x + 1

addFirstTwo :: [Int] -> Int
addFirstTwo [] = 0
addFirstTwo (x1 : xs) = if null xs then x1 else x1 + head xs

first' :: [Int] -> Int
first' [] = 0
first' xs = head xs + 1

addFirstTwo' :: [Int] -> Int
addFirstTwo' [] = 0
addFirstTwo' xs = if length xs == 1 then head xs else head xs + xs !! 1

firstDigit' :: [String] -> [String]
firstDigit' [] = []
firstDigit' (h : s) = h : [a | a <- s, a == h]