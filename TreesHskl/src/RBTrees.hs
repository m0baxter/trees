
module RBTree where

data Colour = Red | Black
   deriving (Eq, Show)

data RBTree a = Leaf | Node Colour (RBTree a) a (RBTree a)
   deriving (Eq, Show)

emptyTree :: RBTree a
emptyTree = Leaf

isIn :: Ord a => a -> RBTree a -> Bool
isIn _ Leaf = False
isIn a (Node _ left b right)
   | a == b = True
   | a < b = isIn a left
   | a > b = isIn a right

insert :: Ord a => a -> RBTree a -> RBTree a
insert a tree = makeBlack $ ins tree
   where ins Leaf = Node Red Leaf a Leaf
         ins (Node c left b right)
            | a < b = balance $ Node c (ins left) b right
            | a > b = balance $ Node c left b (ins right)
            | a == b = tree
         makeBlack (Node _ left b right) = Node Black left b right

balance :: RBTree a -> RBTree a
balance (Node Black (Node Red (Node Red a x b) y c ) z d ) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red a x (Node Red b y c) ) z d ) = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d) )  = Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d)) )  = Node Red (Node Black a x b) y (Node Black c z d)
balance tree = tree

fromList :: Ord a => [a] -> RBTree a
fromList [] = Leaf
fromList (a:as) = insert a $ fromList as

