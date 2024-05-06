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
--import Control.Applicative (Alternative(empty))
--mport Data.Sequence (Seq(Empty), empty)

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node {
                  level :: Int,
                  value :: a,
                  left  :: AATree a,
                  right :: AATree a
                } deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

-- Just a om värdet finns i trädet, eller Nothing om värdet inte finns
-- x < v: Om det sökta värdet x är mindre än nodens värde v, fortsätter sökningen rekursivt i nodens vänstra underträd l
-- x > v: Om det sökta värdet x är större än nodens värde v, fortsätter sökningen rekursivt i nodens högra underträd r.
-- otherwise: Om det sökta värdet x inte är mindre än eller större än v (d.v.s. x == v), 
-- har vi hittat noden som innehåller det sökta värdet, och funktionen returnerar Just v.
get :: Ord a => a -> AATree a -> Maybe a
-- get = error "get not implemented"
get _ Empty = Nothing
get x (Node _ v l r)
  | x < v = get x l
  | x > v = get x r
  | otherwise = Just v

-- You may find it helpful to define
split :: AATree a -> AATree a
split t = t


skew :: AATree a -> AATree a
skew t = t

singleton :: a -> AATree a
singleton x = Node 1 x Empty Empty

-- and call these from insert.
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty              = singleton x
insert x (Node level y l r) = case compare x y of 
  LT -> Node level y l (insert x r)
  EQ -> Node level x l r
  GT -> Node level y (insert x l) r


--Först rekursivt samla alla värden från vänstra underträdet l, vilket säkerställer att alla värden som är mindre än nodens värde v behandlas först.
--Sedan lägg till nodens värde v till listan. Eftersom detta är en nod vi besöker efter dess vänstra underträd men innan dess högra underträd, 
--placeras det exakt mitt emellan de värden som är mindre och större än det.
--Slutligen rekursivt samla alla värden från högra underträdet r, vilket innefattar alla värden som är större än nodens värde v.
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ v l r) = inorder l ++ [v] ++ inorder r

-- Räknar antalet noder
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
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
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
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty Empty = True

leftSub :: AATree a -> AATree a
leftSub Empty = Empty
leftSub (Node _ _ l _) = l 

rightSub :: AATree a -> AATree a
rightSub Empty = Empty
rightSub (Node _ _ _ r) = r 

--------------------------------------------------------------------------------

