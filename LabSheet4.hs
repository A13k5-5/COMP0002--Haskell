squares :: [Int] -> [Int]
squares = map (^ 2)

sumSquares :: [Int] -> Int
sumSquares = sum . squares

positives :: [Int] -> Bool
positives = foldr ((&&) . (> 0)) True

minimum' :: (Int -> Int) -> Int -> Int
minimum' f n = minimum (map f [0 .. n])

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = foldr ((&&) . (== f (0))) True (map f [1 .. n])

posFunc :: (Int -> Int) -> Int -> Bool
posFunc f n = positives (map f [0 .. n])

-- increasingFunc :: (Int -> Int) -> Int -> Bool

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n - 1) f x)

double :: Int -> Int
double x = 2 * x

powersOfTwo :: Int -> Int
powersOfTwo n = iter n double 1

data RhType = Positive | Negative deriving (Show, Eq)

data ABOType = A | B | AB | O deriving (Show, Eq)

type BloodType = (RhType, ABOType)

patient :: Int -> (RhType, ABOType)
patient n
  | n == 1 = (Positive, B)
  | n == 2 = (Negative, AB)
  | n == 3 = (Negative, A)
  | n == 4 = (Negative, O)
  | n == 5 = (Positive, AB)

showRh :: RhType -> IO ()
showRh = print

showABO :: ABOType -> IO ()
showABO = print

showBloodType :: BloodType -> IO ()
showBloodType (rh, abo) = do
  showRh rh
  showABO abo

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (rh1, abo1) (rh2, abo2)
  | abo1 == abo2 = True
  | abo1 == O = True
  | abo2 == AB = True

-- 7

data Answer = Yes | No | Unknown deriving (Eq)

wonky :: Answer -> Answer
wonky a
  | a == Yes = No
  | a == No = Unknown
  | a == Unknown = Yes

-- shortest is 3
-- longest is 3
-- depends on the version, weird question

-- 8

data Shape
  = Ellipse Float Float
  | Rect Float Float

area :: Shape -> Float
area (Rect x y) = x * y
area (Ellipse r1 r2) = pi * r1 * r2
