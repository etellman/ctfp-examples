module Atif.Partition
  ( takeN,
    partition,
  )
where

import Data.List (intersect)

-- all the ways to partition a list into non-overlapping sub-lists
partition :: Eq a => Int -> [a] -> [[[a]]]
partition numGroups xs =
  let elementsPerGroup = length xs `div` numGroups
      groups = takeN (==) elementsPerGroup xs
      overlapping x y = not . null $ intersect x y
   in takeN overlapping numGroups groups

-- all the ways to select n elements from a list
takeN :: (a -> a -> Bool) -> Int -> [a] -> [[a]]
takeN _ 0 _ = [[]]
takeN _ _ [] = []
takeN eq n (x : xs) =
  let withX = fmap (x :) $ takeN eq (n - 1) (filter (not . eq x) xs)
      withoutX = takeN eq n xs
   in withX ++ withoutX
