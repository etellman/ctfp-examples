module Ch08.K2
  ( K2 (..),
  )
where

import Data.Bifunctor

data K2 c a b = K2 c deriving (Eq, Show)

instance Bifunctor (K2 c) where
  bimap _ _ (K2 x) = K2 x
