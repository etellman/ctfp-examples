module Atif.Partition
  ( takeN,
    groups,
  )
where

import Data.List (intersect)

data StudentCategory = UBB | LSJ deriving (Show, Eq)

data Student = Student
  { name :: String,
    category :: StudentCategory
  }
  deriving (Show, Eq)

groups :: Int -> [Int] -> [[[Int]]]
groups nGroups students =
  let combinations = takeN (length students `div` nGroups) students
    in partition nGroups combinations

takeN :: Int -> [a] -> [[a]]
takeN 0 _ = [[]]
takeN _ [] = []
takeN n (x : students) =
  let withX = fmap (x :) $ takeN (n - 1) students
      withoutX = takeN n students
   in withX ++ withoutX

partition :: Int -> [[Int]] -> [[[Int]]]
partition _ [] = []
partition nGroups (student : students) =
  let rest = filter (null . intersect student) students
   in fmap (student :) (takeN (nGroups - 1) rest) ++ partition nGroups students
