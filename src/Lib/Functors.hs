module Lib.Functors
  ( F (..),
    G (..),
    fToG,
  )
where

data F a = F a deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

data G a = G a deriving (Eq, Show)

instance Functor G where
  fmap f (G a) = G (f a)

fToG :: F a -> G a
fToG (F x) = G x
