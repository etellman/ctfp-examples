module Ch23.Coalgebra (ana) where

import Ch23.Fix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
