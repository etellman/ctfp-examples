module Lib.Functors
  ( F (..),
    G (..),
    alpha,
  )
where

data F a = F a deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

data G a = G a deriving (Eq, Show)

instance Functor G where
  fmap f (G a) = G (f a)

alpha :: F a -> G a
alpha (F x) = G x
