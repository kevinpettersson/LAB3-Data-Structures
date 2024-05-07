--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where
import Debug.Trace (traceEvent)
import Data.Maybe
--import Control.Applicative (Alternative(empty))
--mport Data.Sequence (Seq(Empty), empty)

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node {  level :: Int,
                                value :: a,
                                left  :: AATree a,
                                right :: AATree a
                              } deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

-- Takes a value and checks if the value exists in the tree, if it less than parent value we check in the left sub-tree
-- otherwise we check in the right sub-tree and if value dosent exist func returns nothing.
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node _ v l r)
  | x < v = get x l
  | x > v = get x r
  | otherwise = Just v

-- Takes a value and creates a node with two empty children.
singleton :: a -> AATree a
singleton x = Node 1 x Empty Empty

-- If the tree is empty, create a call singleton function.
-- If value already exist do nothing, return same tree.
-- If value is less than parent value, 
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty             = singleton x
insert x tree
  | isJust (get x tree)    = tree                           
insert x (Node lvl v l r)
  | x < v                  = split (skew (Node lvl v (insert x l) r))
  | otherwise              = split (skew (Node lvl v l (insert x r)))

skew :: AATree a -> AATree a
skew Empty                = Empty
skew (Node lvl v Empty r) = Node lvl v Empty r
skew (Node lvl v (Node llvl lv ll lr) r)
  | llvl == lvl = skew (Node llvl lv ll (Node lvl v lr r)) 
  | otherwise   =  Node lvl v (Node llvl lv ll lr) r

-- You may find it helpful to define
split :: AATree a -> AATree a
split Empty                                  = Empty
split (Node _ _ _ Empty)                     = Empty
split (Node lvl v l 
      (Node rlvl rv rl Empty))               = Node lvl v l (Node rlvl rv rl Empty)
split (Node lvl v l 
      (Node rlvl rv rl 
      (Node rrlvl rrv rrl rrr)))
  | lvl == rlvl && rlvl == rrlvl              = split (Node rlvl rv (Node (lvl+1) v l rl) (Node (rrlvl+1) rrv rrl rrr))
  | otherwise                                 = Node lvl v l (Node rlvl rv rl (Node rrlvl rrv rrl rrr))


-- T = Node lvl v r l
-- R = Node rlvl rv rl rr
-- X = Node rrlvl rrv rrl rrr

-- After split: 

-- R = Node rlvl rv (Node lvl v l rl) (Node rrlvl rrv rrl rrr)    R           T  R  X
--                                                              T   X       A   B 
--                                                            A   B                                                                        
--skew (Node lvl v l (Node rLvl rv rl rr))
  -- | lvl == rLvl = Node rLvl rv (Node lvl v l rl) rr
  -- | otherwise = Node lvl v l (Node rLvl rv rl rr)

-- Recursivly calls the function on the left and right sub-tree and appending the value in between, creating a list.
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ v l r) = inorder l ++ [v] ++ inorder r

-- Calculates the total amount of nodes in the tree.
size :: AATree a -> Int
size Empty = 0
size (Node _ _ l r) = 1 + size l + size r

-- Returns 0 if the tree is empty and otherwise adds 1 and choosing the maximum height of the left and right sub-tree.
-- By recursively calling the height function.
height :: AATree a -> Int
height tree = case tree of
  Empty -> 0
  (Node _ _ l r) -> 1 + max (height l) (height r)

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree tree =
  isSorted (inorder tree) && all checkLevels (nodes tree)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
-- Recursivly checks that left node is less or equal to the right node. And that the level of the left node 
-- is greater or equal to the right node.
isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels Empty                  = True
checkLevels (Node _ _ Empty Empty) = True
checkLevels (Node lvl v l r)       = checkLeft && checkRight && checkLevels l && checkLevels r
  where
    checkLeft = case l of
      Empty                -> True
      (Node leftLVL _ _ _) -> leftLVL < lvl
    checkRight = case r of
      Empty                 -> True
      (Node rightLVL _ _ _) -> rightLVL <= lvl

isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False

leftSub :: AATree a -> AATree a
leftSub Empty          = Empty
leftSub (Node _ _ l _) = l

rightSub :: AATree a -> AATree a
rightSub Empty          = Empty
rightSub (Node _ _ _ r) = r

--------------------------------------------------------------------------------

