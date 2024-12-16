squares :: [Int] -> [Int]
squares = map (^2)

sumSquares :: [Int] -> Int
sumSquares = sum . squares

positives :: [Int] -> Bool
positives = foldr ((&&) . (>0)) True


minimum' :: (Int -> Int) -> Int -> Int
minimum' f n = minimum(map f [0..n])

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = foldr ((&&) . (== f(0))) True (map f [1..n])

posFunc :: (Int -> Int) -> Int -> Bool
posFunc f n = positives (map f [0..n])

-- increasingFunc :: (Int -> Int) -> Int -> Bool

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

double :: Int -> Int
double x = 2 * x

powersOfTwo :: Int -> Int
powersOfTwo n = iter n double 1

data ABO = A|B|AB|O

data Answer = Yes | No | Unknown
