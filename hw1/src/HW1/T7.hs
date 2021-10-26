module HW1.T7 where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) xs = a :+ xs
  (<>) (a :+ as) xs = a :+ (as <> xs)

data Inclusive a b = This a | That b | Both a b


instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (That b) = Both a b
  (<>) (This a) (This b) = This (a <> b)
  (<>) (That a) (That b) = That (a <> b)
  (<>) (Both x y) (Both a b) = Both (x <> a) (y <> b)
  (<>) (This a) (Both x y) = Both (a <> x) y
  (<>) (That b) (Both x y) = Both x (b <> y)
  (<>) (Both x y) (This a) = Both (x <> a) y
  (<>) (Both x y) (That b) = Both x (y <> b)
  (<>) (That a) (This b) = Both b a

newtype DotString = DS String

instance Semigroup DotString where
  (<>) a (DS "nullstring") = a
  (<>) (DS "nullstring") a = a
  (<>) (DS a) (DS b) = DS $ a ++ "." ++ b

instance Monoid DotString where
  mempty = DS "nullstring"

newtype Fun a = F (a -> a)
instance Semigroup (Fun a) where
  (<>) (F a) (F b) = F (a . b)

instance Monoid (Fun a) where
  mempty = F id
