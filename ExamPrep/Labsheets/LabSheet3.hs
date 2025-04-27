mult :: Num a => [a] -> a
mult [] = 0
mult xs = foldr (*) 1 xs