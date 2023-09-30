module Ch23.Fix
  ( Fix (..),
    unfix,
  )
where

data Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix x) = x
