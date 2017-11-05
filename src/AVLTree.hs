module AVLTree (insert, find, treeOf) where

--import qualified Foldable as F

data Tree a = Tree a (Tree a) (Tree a) | Empty deriving (Show)

find :: (Ord a) => Tree a -> a -> Maybe a
find Empty _ = Nothing
find (Tree a left right) key =
  case a `compare` key of
    EQ -> Just a
    LT -> find right key
    GT -> find left key

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty v = Tree v Empty Empty
insert (Tree a left right) v = if v < a
  then balance $ Tree a (insert left v) right
  else balance $ Tree a left (insert right v)

treeOf :: (Ord a) => [a] -> Tree a
treeOf = foldl insert Empty

balance :: (Ord a) => Tree a -> Tree a
-- uncomment to ignore balancing
--balance = id
balance t = fst $ balanceInternal t

-- returns height along with balanced tree itself
balanceInternal :: (Ord a) => Tree a -> (Tree a, Int)
balanceInternal Empty = (Empty, 0)
balanceInternal tree@(Tree a left right)
  | lh == rh || lh == rh + 1 = (tree, lh + 1)
  | lh == rh - 1 = (tree, rh + 1)
  | lh < rh =
    let
      (Tree rv rlSubtree rrSubtree) = right
      newLeft = (Tree a left rlSubtree)
      newRight = rrSubtree
    in ((Tree rv newLeft newRight), rh + 1)
   | lh > rh =
     let
      (Tree lv llSubtree lrSubtree) = left
      newLeft = llSubtree
      newRight = (Tree a lrSubtree right)
    in ((Tree lv newLeft newRight), lh + 1)
  where
    (bl, lh) = balanceInternal left
    (br, rh) = balanceInternal right
    
