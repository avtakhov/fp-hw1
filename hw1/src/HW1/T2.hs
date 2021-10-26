module HW1.T2 where

data N = Z | S N

nplus :: N -> N -> N -- addition
nplus Z n = n
nplus (S n) m = S (nplus n m)

nmult :: N -> N -> N -- multiplication
nmult Z _ = Z
nmult (S n) m = nplus m (nmult n m)

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub n Z = Just n
nsub Z (S _) = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp Z Z = EQ
ncmp Z (S _) = LT
ncmp (S _) Z = GT
ncmp (S n) (S m) = ncmp n m

nEven, nOdd :: N -> Bool -- parity checking
nEven Z = True
nEven (S Z) = False
nEven (S (S n)) = nEven n
nOdd Z = False
nOdd (S n) = nEven n

ndiv :: N -> N -> N -- integer division
ndiv a b = case nsub a b of
  Nothing -> Z
  Just n -> S $ ndiv n b

nmod :: N -> N -> N -- modulo operation
nmod a b = case nsub a b of
  Nothing -> a
  Just n -> nmod n b
