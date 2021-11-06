module Partition.Main where

-- Partitioning a list with a predicate, predicate + record, and either.

import Data.Bifunctor (first, second)

-- Predicate.

-- >>> partitionP even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionP :: (a -> Bool) -> [a] -> ([a], [a])
partitionP _ [] = ([], [])
partitionP p (a : as)
  | p a = first (a :) rest
  | otherwise = second (a :) rest
  where
    rest = partitionP p as

-- Predicate + Record.

-- >>> partitionP' even [1..10]
-- Partitioned {matches = [2,4,6,8,10], mismatches = [1,3,5,7,9]}
partitionP' :: (a -> Bool) -> [a] -> Partitioned a
partitionP' p = partitioned . go p
  where
    partitioned (matches, mismatches) = Partitioned matches mismatches
    go _ [] = ([], [])
    go p (a : as)
      | p a = first (a :) rest
      | otherwise = second (a :) rest
      where
        rest = go p as

data Partitioned a = Partitioned
  { matches :: [a],
    mismatches :: [a]
  }
  deriving (Show)

-- Selector.

-- >>> partitionE (\x -> if even x then Left x else Right x) [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionE :: (a -> Either l r) -> [a] -> ([l], [r])
partitionE _ [] = ([], [])
partitionE sel (a : as) = case sel a of
  Left l -> first (l :) rest
  Right r -> second (r :) rest
  where
    rest = partitionE sel as

-- >>> partitionP'' even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionP'' :: (a -> Bool) -> [a] -> ([a], [a])
partitionP'' p = partitionE sel
  where
    sel a = if p a then Left a else Right a

main :: IO ()
main = return ()
