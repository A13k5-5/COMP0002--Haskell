-- pre: n >= 0
-- post fac n = n!
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- base: n = 0 -> fac 0 = 0! -> fac 0 = 1
-- Assume fac k = k! we need to show that fac (k+1) == (k+1!)
-- fac (k+1) = (k+1) * fac k
-- fac (k+1) = (k+1) * k!
-- fac (k+1) = (k+1)!
-- Hence shown.

-- pre: n > 0
-- post fib n = fibonacci (n)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- base: n = 0 => fib 0 = 0 and fib 1 = 1
-- fib 2 = fib 0 + fib 1 = 2
-- Assume: fib k = fib (k-1) + fib (k-2)
-- We will try to show that fib (k+1) is true as well
-- fib (k+1) = fib k + fib (k-1)


-- pre: True
-- post: reverse xs = reversed xs,
reverse'' :: [a] -> [a] -> [a]
reverse'' [] sx = sx
reverse'' (x:xs) sx = reverse'' xs (x:sx)

-- Base: reverse [] = []
-- Assume reverse [k] = reversed [k]
-- We will try to show that reverse [k+1] = reversed [k]
-- reverse [k+1] = reverse [k+1, k, k-1...] = reverse [k] ++ [k+1] = reversed [k+1]

reverse' :: [a] -> [a]
reverse' xs = reverse'' xs []

nimGame :: IO ()
nimGame = do putStrLn "Hello"