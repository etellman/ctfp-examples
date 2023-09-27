module Ch23.Fix
  ( Fix (..),
    unfix,
    lenAlg,
    ListF (..),
  )
where

data Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix x) = x

data ListF e a = NilF e | ConsF e a

lenAlg :: ListF e Int -> Int
lenAlg (NilF _) = 0
lenAlg (ConsF _ n) = n + 1
