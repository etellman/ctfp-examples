module Lib.H
  (
    H (..),
  )
where


data H a = H a deriving (Eq, Show)

instance Functor H where
  fmap f (H a) = H (f a)
