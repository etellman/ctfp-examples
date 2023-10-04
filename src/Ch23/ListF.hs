module Ch23.ListF
  ( ListF (..),
    toList,
    lenAlg,
    sumAlg,
    eqListF,
  )
where

import Ch23.Fix

data ListF e a = NilF | ConsF e a deriving (Eq, Show)

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
