module Atif.Partition
  ( takeN,
  )
where

data StudentCategory = UBB | LSJ deriving (Show, Eq)

data Partition = Partition
  { group :: [Int],
    others :: [Int]
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
takeN n (x:xs) =
  let withX = fmap (x:) $ takeN (n - 1) xs
      withoutX = takeN n xs
    in withX ++ withoutX
