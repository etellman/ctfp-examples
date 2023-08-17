module Lib.G
  ( G (..),
  )
where

data G a = G a deriving (Eq, Show)

instance Functor G where
  fmap f (G a) = G (f a)
