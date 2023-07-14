module Ch08.PreList
  ( PreList (..),
  )
where

import Data.Bifunctor

data PreList a b = Nil | Cons a b deriving (Eq, Show)

instance Bifunctor PreList where
  bimap _ _ Nil = Nil
  bimap f g (Cons x y) = Cons (f x) (g y)
