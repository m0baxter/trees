
module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Ord, Show)

--Make this balanced?
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert a Leaf = Node Leaf a Leaf
insert b t@(Node left a right)
      | b == a = t
      | b < a  = Node (insert b left) a right
      | b > a  = Node left a (insert b right)

--Make this balanced?
delete :: Ord a => a -> BinaryTree a -> BinaryTree a
delete _ Leaf = Leaf
delete b (Node left a right)
      | b == a = del (Node left a right)
      | b < a = Node (delete b left) a right
      | b > a = Node left a (delete b right)

--Helper for delete:
del :: Ord a => BinaryTree a -> BinaryTree a
del ( Node Leaf a right ) = right
del ( Node left a Leaf )  = left
del (Node left a right) = Node left leastR (delete leastR right)
   where leastR = least right

--Helper for delete:
least :: Ord a => BinaryTree a -> a
least ( Node Leaf a _ ) = a
least ( Node left _ _ )  = least left

isIn :: Ord a => a -> BinaryTree a -> Bool
isIn _ Leaf = False
isIn b (Node left a right)
      | b == a = True
      | b < a = isIn b left
      | b > a = isIn b right

--Creates a balanced tree:
listToTree :: Ord a => [a] -> BinaryTree a
listToTree [] = Leaf
listToTree [a] = Node Leaf a Leaf
listToTree [a, b] = insert a (Node Leaf b Leaf)
listToTree as = Node (listToTree left) a (listToTree right)
   where (left, a, right) = midSplit as

--helper for listToTree:
midSplit :: [a] -> ([a], a, [a])
midSplit as = ( take n as, head back, tail back)
   where n = (length as) `div` 2
         front = take n as
         back  = drop n as

height :: BinaryTree a -> Int
height Leaf = 0
height (Node left _ right) = 1 + max (height left)  (height right)

bFactor :: BinaryTree a -> Int
bFactor Leaf = 0
bFactor (Node left _ right) = (height right) - (height left)

isBalanced :: BinaryTree a -> Bool
isBalanced t = (abs (bFactor t) ) <= 1

