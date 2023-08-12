module Ch15.Yoneda
  ( toBeta,
    toAlpha,
    phi,
    psi,
    fromSingletonList,
    toSingletonList,
    listLength,
    listIndex,
  )
where

import Control.Monad.Reader

toAlpha :: (b -> m b) -> a -> (Reader a b -> m b)
toAlpha toM x = \f -> toM (runReader f x)

toBeta :: (m b -> b) -> (m b -> Reader a b)
toBeta fromM = \mx -> reader (\_ -> (fromM mx))

phi :: (Reader a a -> m a) -> m a
phi alpha = alpha (reader id)

psi :: (Functor m) => m a -> (Reader a b -> m b)
psi ma = \h -> fmap (runReader h) ma

fromSingletonList :: [a] -> a
fromSingletonList [x] = x
fromSingletonList _ = undefined

toSingletonList :: a -> [a]
toSingletonList x = [x]

listLength :: [a] -> Reader a Int
listLength xs = reader $ \_ -> length xs

listIndex :: [a] -> Reader Int a
listIndex xs = reader $ \i -> xs !! i
