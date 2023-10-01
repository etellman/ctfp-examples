module Ch23.Catamorphism
  ( cata,
    fibF,
    ListF (..),
    toList,
    lenAlg,
    sumAlg,
    eqListF,
  )
where

import Ch23.Fix
import Ch23.NatF
import Control.Comonad

-- add intermediate steps to clarify what's going on
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

fibF :: NatF (Integer, Integer) -> (Integer, Integer)
fibF ZeroF = (1, 1)
fibF (SuccF x) =
  let (m, n) = fibF x
   in (n, m + n)

data ListF e a = NilF | ConsF e a deriving (Eq, Show)

instance Comonad (ListF e) where
  extend :: (ListF e a -> b) -> ListF e a -> ListF e b
  extend f x@(ConsF e _) = ConsF e (f x)
  extend _ NilF = NilF

  extract :: ListF e a -> a
  extract (ConsF _ a) = a
  extract NilF = undefined

instance Functor (ListF e) where
  fmap :: (a -> b) -> ListF e a -> ListF e b
  fmap _ NilF = NilF
  fmap f (ConsF e a) = ConsF e (f a)

toList :: [e] -> Fix (ListF e)
toList [] = Fix NilF
toList (x : xs) = Fix $ ConsF x (toList xs)

eqListF :: Eq e => Fix (ListF e) -> [e] -> Bool
eqListF (Fix NilF) [] = True
eqListF (Fix NilF) _ = False
eqListF _ [] = False
eqListF (Fix (ConsF x xs)) (y : ys) = x == y && xs `eqListF` ys

lenAlg :: ListF e Int -> Int
lenAlg NilF = 0
lenAlg (ConsF _ n) = n + 1

sumAlg :: ListF Double Double -> Double
sumAlg NilF = 0.0
sumAlg (ConsF x total) = x + total
