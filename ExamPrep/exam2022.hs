divides :: Int -> Int -> Bool
a `divides` b = b `mod` a == 0

prime :: Int -> Bool
prime 2 = True
prime 1 = False
prime n = not (foldr1 (||) (map (`divides` n) [2..n-1]))