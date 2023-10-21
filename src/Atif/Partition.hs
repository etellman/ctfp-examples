module Atif.Partition
  ( combinations,
    partition,
    takeN,
  )
where

import Data.List (intersect)

-- take up to n partitions where none of the partitions have a group in common
takeN :: Eq a => Int -> [[a]] -> [[a]]
takeN 0 _ = []
takeN _ [] = []
takeN n (x : xs) = x : takeN (n - 1) (filter (\y -> (null $ intersect x y)) xs)

-- all the ways to partition a list into non-overlapping sub-lists
partition :: Eq a => Int -> [a] -> [[[a]]]
partition numGroups xs =
  let elementsPerGroup = length xs `div` numGroups
      groups = combinations (==) elementsPerGroup xs
   in combinations (\x y -> not . null $ intersect x y) numGroups groups

-- all the ways to select n elements from a list
combinations :: (a -> a -> Bool) -> Int -> [a] -> [[a]]
combinations _ 0 _ = [[]]
combinations _ _ [] = []
combinations eq n (x : xs) =
  let withX = fmap (x :) $ combinations eq (n - 1) (filter (not . eq x) xs)
      withoutX = combinations eq n xs
   in withX ++ withoutX
