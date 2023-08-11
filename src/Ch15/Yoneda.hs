module Ch15.Yoneda
  ( toBeta,
    toAlpha,
    phi,
    psi,
  )
where

import Control.Monad.Reader

toAlpha :: (b -> m b) -> a -> (Reader a b -> m b)
toAlpha newM x = \f -> newM (runReader f x)

toBeta :: (m b -> b) -> (m b -> Reader a b)
toBeta fromM = \x -> reader (\_ -> (fromM x))

phi :: (Reader a a -> m a) -> m a
phi alpha = alpha (reader id)

psi :: (Functor m) => m a -> (Reader a b -> m b)
psi ma = \h -> fmap (runReader h) ma
