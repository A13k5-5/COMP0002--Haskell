data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving Show

t :: Tree Int
t = Node (Leaf 1) 2 (Leaf 3)
