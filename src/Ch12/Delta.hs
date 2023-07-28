module Ch12.Delta
  ( Delta (..),
  )
where

data Delta a = Delta deriving (Eq, Show)

instance Functor Delta where
  fmap _ Delta = Delta
