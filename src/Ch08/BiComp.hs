module Ch08.BiComp
  ( BiComp (..),
  )
where

import Data.Bifunctor

newtype BiComp bf fu gu a b = BiComp (bf (fu a) (gu b)) deriving (Eq, Show)

instance
  (Bifunctor bf, Functor fu, Functor gu) =>
  Bifunctor (BiComp bf fu gu)
  where
  bimap f1 f2 (BiComp x) = BiComp ((bimap (fmap f1) (fmap f2)) x)
