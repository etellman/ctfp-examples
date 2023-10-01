module Ch23.Catamorphism
  ( cata,
    fibF,
  )
where

import Ch23.Fix
import Ch23.NatF
import Control.Comonad

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

fibF :: NatF (Integer, Integer) -> (Integer, Integer)
fibF ZeroF = (1, 1)
fibF (SuccF x) =
  let (m, n) = fibF x
   in (n, m + n)
