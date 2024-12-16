data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

numLeaf :: Tree a -> Int
numLeaf (Leaf x) = 1
numLeaf (Node left right) = numLeaf left + numLeaf right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = abs z <= 1
                             where z = numLeaf left - numLeaf right

