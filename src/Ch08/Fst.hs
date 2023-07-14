module Ch08.Fst
  ( Fst (..),
  )
where

import Data.Bifunctor

data Fst a b = Fst a deriving (Eq, Show)

instance Bifunctor Fst where
  bimap f _ (Fst x) = Fst (f x)
