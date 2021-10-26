module HW1.T3 where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (a, _) _ _ _) = a

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, a) _ _ _) = a

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l val r) = case compare x val of
  LT -> tmember x l
  GT -> tmember x r
  EQ -> True

correct :: Tree a -> a -> Tree a -> Tree a
correct l val r = Branch (tsize l + tsize r + 1, max (tdepth l) (tdepth r) + 1) l val r

rotateR :: Tree a -> Tree a
rotateR (Branch _ (Branch _ ql qval qr) pval pr) = correct ql qval (correct qr pval pr)
rotateR t = t

rotateL :: Tree a -> Tree a
rotateL (Branch _ ql qval (Branch _ pl pval pr)) = correct (correct ql qval pl) pval pr
rotateL t = t

bfactor :: Tree a -> Int
bfactor Leaf = 0
bfactor (Branch _ l _ r) = tdepth l - tdepth r

balanced :: Tree a -> Tree a
balanced Leaf = Leaf
balanced (Branch _ l val r) = case bfactor $ correct l val r of
  2 -> if bfactor r < 0 then correct l val (rotateR r) else correct l val r
  -2 -> if bfactor l > 0 then correct (rotateL l) val r else correct l val r
  _ -> correct l val r

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = correct Leaf a Leaf
tinsert a (Branch _ l val r) = case compare a val of
                               LT -> balanced $ correct (tinsert a l) val r
                               GT -> balanced $ correct l val (tinsert a r)
                               EQ -> correct l val r

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (x:xs) = tinsert x (tFromList xs)
