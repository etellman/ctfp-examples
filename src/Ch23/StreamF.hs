module Ch23.StreamF
  ( StreamF (..),
    squares,
    timesN,
    toListC,
    primes,
  )
where

import Ch23.Algebra
import Ch23.Fix

data StreamF e a = StreamF e a deriving (Eq, Show)

instance Functor (StreamF e) where
  fmap f (StreamF e a) = StreamF e (f a)

squares :: Num a => [a] -> StreamF a [a]
squares (x : xs) = StreamF (x ^ (2 :: Int)) xs
squares _ = undefined

timesN :: Num a => a -> [a] -> StreamF a [a]
timesN n (x : xs) = StreamF (x * n) xs
timesN _ [] = undefined

primes :: Integral a => [a] -> StreamF a [a]
primes (x : xs) =
  let multiple a b = b `mod` a == 0
   in StreamF x $ (filter (not . multiple x)) xs
primes [] = undefined

toListC :: Fix (StreamF e) -> [e]
toListC = cata a1
  where
    a1 :: StreamF e [e] -> [e]
    a1 (StreamF e a) = e : a
