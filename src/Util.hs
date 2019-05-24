module Util where

partitionWhile :: (a -> Bool) -> [a] -> ([a],[a])
partitionWhile f [] = ([],[])
partitionWhile f (x:xs)
  | f x       = (x:as,bs)
  | otherwise = ([],x:xs)
    where
    (as,bs) = partitionWhile f xs
