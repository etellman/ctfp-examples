module Ch23.Catamorphism
  ( cata,
    fibF,
    ListF (..),
    toList,
    fromList,
    lenAlg,
    eqListF,
  )
where

import Ch23.Fix
import Ch23.NatF

-- add intermediate steps to clarify what's going on
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

fibF :: NatF (Integer, Integer) -> (Integer, Integer)
fibF ZeroF = (1, 1)
fibF (SuccF x) =
  let (m, n) = fibF x
   in (n, m + n)

data ListF e a = NilF | ConsF e a deriving (Eq, Show)

instance Functor (ListF e) where
  fmap _ NilF = NilF
  fmap f (ConsF x y) = ConsF x (f y)

fromList :: Fix (ListF e) -> [e]
fromList (Fix NilF) = []
fromList (Fix (ConsF x xs)) = x : fromList xs

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
