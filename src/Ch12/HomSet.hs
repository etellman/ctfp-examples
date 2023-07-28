module Ch12.HomSet
  ( HomSet (..),
    evaluate,
  )
where

import Data.Functor.Contravariant

-- | morphisms from b to a
data HomSet a b = HomSet [Op a b]

-- | converts (b -> a) to (c -> a) -- changes the source object
instance Contravariant (HomSet a) where
  contramap f (HomSet x) = HomSet $ fmap (contramap f) x

evaluate :: b -> HomSet a b -> [a]
evaluate x (HomSet ops) =
  let runOp op = getOp op $ x
   in fmap runOp ops
