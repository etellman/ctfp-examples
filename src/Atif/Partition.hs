module Atif.Partition
  ( takeN,
    groups,
  )
where

import Data.List (intersect)

groups :: Int -> [Int] -> [[[Int]]]
groups nGroups xs =
  let combinations = takeN (length xs `div` nGroups) xs
    in partition nGroups combinations

takeN :: Int -> [a] -> [[a]]
takeN 0 _ = [[]]
takeN _ [] = []
takeN n (x : xs) =
  let withX = fmap (x :) $ takeN (n - 1) xs
      withoutX = takeN n xs
   in withX ++ withoutX

partition :: Int -> [[Int]] -> [[[Int]]]
partition _ [] = []
partition nGroups (x : xs) =
  let rest = filter (null . intersect x) xs
   in fmap (x :) (takeN (nGroups - 1) rest) ++ partition nGroups xs
