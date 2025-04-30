nextTrib :: (Int, Int, Int) -> (Int, Int, Int)
nextTrib (a, b, c) = (b, c, a + b + c)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

tribonacci :: Int -> [Int]
tribonacci k = take k (map fst3 $ iterate nextTrib (0, 0, 1))