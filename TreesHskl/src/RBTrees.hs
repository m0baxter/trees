
module RBTree where

data RBTree a = Leaf | NodeR (RBTree a) a (RBTree a) | NodeB (RBTree a) a (RBTree a)
   deriving (Eq, Ord, Show)

change :: RBTree a -> RBTree a
change Leaf = Leaf
change (NodeR l a r) = NodeB l a r
change (NodeB l a r) = NodeR l a r

emptyTree :: RBTree a
emptyTree = Leaf

insert :: Ord a => a -> RBTree a -> RBTree a
insert a Leaf = NodeB leaf a Leaf

delete :: Ord a => a -> RBTree a -> RBTree a

listToRBTree :: Ord a => [a] -> RBTree a
listToRBTree [] = Leaf
listToRBTree (a:as) = insert a (listToBRTree as)

