firstPlusOne :: Num a => [a] -> a
firstPlusOne [] = 0
firstPlusOne (x:xs) = x + 1

addFirstTwo :: Num a => [a] -> a
addFirstTwo [] = 0
addFirstTwo [x] = x
addFirstTwo (x:y:xs) = x + y 

firstPlusOne' :: Num a => [a] -> a
firsPlusOne' [] = 0
firstPlusOne' xs = head xs + 1

addFirstTwo' :: Num a => [a] -> a
addFirstTwo' [] = 0
addFirstTwo' [a] = a
addFirstTwo' xs = sum(take 2 xs)