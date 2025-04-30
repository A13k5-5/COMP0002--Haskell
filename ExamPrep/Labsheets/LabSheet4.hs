squares :: [Int] -> [Int]
squares = map square
    where square x = x*x

sumSquares :: [Int] -> Int
sumSquares xs = sum $ map square xs
    where square x = x^2

allPositive :: [Int] -> Bool
allPositive = foldr ((&&) . (>0)) True

minZeroToN :: (Int -> Int) -> Int -> Int
minZeroToN f n = minimum $ map f [1..n]

data RhType = Positive | Negative deriving (Show, Eq)
data ABOType = A | B | AB | O deriving (Show, Eq)

type BloodType = (RhType, ABOType)

patient :: Int -> BloodType
patient n | n == 0 = (Positive, A)
          | n == 2 = (Negative,AB)
          | n == 3 = (Negative,A)
          | n == 4 = (Negative,O)
          | n == 5 = (Positive,AB)

showRh :: RhType -> IO()
showRh = print

showABO :: ABOType -> IO()
showABO = print
showBloodType :: BloodType -> IO()
showBloodType = print

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (rh1, abo1) (rh2, abo2) | abo1 == abo2 = True
                                    | abo1 == O = True
                                    | abo2 == AB = True
                                    | otherwise = False

data Answer = Yes | No | Unknown deriving Eq
wonky :: Answer -> Answer
wonky a | a == Yes = No
        | a == No = Yes
        | otherwise = Unknown

data Shape = Rect Float Float | Ellipse Float Float

area :: Shape -> Float
area (Rect x y) = x * y
area (Ellipse x y) = pi * x * y