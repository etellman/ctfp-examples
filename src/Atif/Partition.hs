module Atif.Partition
  ( takeN,
    children,
    Partition (..),
  )
where

import Data.List ((\\))

data StudentCategory = UBB | LSJ deriving (Show, Eq)

data Partition = Partition
  { group :: [Int],
    next :: [Partition]
  }
  deriving (Show, Eq)

data Student = Student
  { name :: String,
    category :: StudentCategory
  }
  deriving (Show, Eq)

takeN :: Int -> [a] -> [[a]]
takeN 0 _ = [[]]
takeN _ [] = []
takeN n (x : xs) =
  let withX = fmap (x :) $ takeN (n - 1) xs
      withoutX = takeN n xs
   in withX ++ withoutX

children :: Int -> [Int] -> [Int] -> [Partition]
children 0 _ _ = undefined
children _ [] g = [Partition g []]
children n xs g =
  let rest = xs \\ g
      ps2 = fmap (children n rest) (takeN n rest)
   in [Partition g (concat ps2)]
