module HW1.T5 where

import Data.List.NonEmpty (NonEmpty ((:|)), cons)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep xs =
  let next = (takeWhile ((/=) sep) xs)
   in case drop (length next) xs of
        [] -> next :| []
        (s : ss) -> cons next $ splitOn s ss

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (x :| xs) = foldl (++) x (map ((:) sep) xs)
