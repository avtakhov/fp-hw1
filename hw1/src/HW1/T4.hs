module HW1.T4 where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ x Leaf = x
tfoldr f x (Branch _ l val r) = (tfoldr f (f val (tfoldr f x r)) l)
