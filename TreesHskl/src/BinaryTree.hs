
module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert a Leaf = Node Leaf a Leaf
insert b (Node left a right)
      | b == a = Node left a right
      | b < a  = Node (insert b left) a right
      | b > a  = Node left a (insert b right)

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

