module Ch23.Algebra (cata) where

import Ch23.Fix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix
