module Ch22.MonoidalCategory
  ( mu,
    eta,
    alpha,
    lambda,
    rho,
  )
where

mu :: Monoid m => (m, m) -> m
mu (x, y) = mappend x y

eta :: Monoid m => () -> m
eta = mempty

alpha :: ((a, b), c) -> (a, (b, c))
alpha ((x, y), z) = (x, (y, z))

lambda :: ((), a) -> a
lambda = snd

rho :: (a, ()) -> a
rho = fst
