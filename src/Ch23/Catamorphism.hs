module Ch23.Catamorphism (cata) where

import Ch23.Fix
import Control.Comonad

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix
