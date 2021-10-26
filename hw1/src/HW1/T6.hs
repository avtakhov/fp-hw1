module HW1.T6 where

import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)

mcat :: Monoid a => [Maybe a] -> a
mcat xs = mconcat $ map (fromMaybe mempty) xs

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart l = (mconcat (lefts l), mconcat (rights l))
