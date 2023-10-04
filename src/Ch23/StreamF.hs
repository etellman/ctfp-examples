module Ch23.StreamF
  ( StreamF (..),
    squares,
    timesn,
    toListC,
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

timesn :: Num a => a -> [a] -> StreamF a [a]
timesn n (x : xs) = StreamF (x * n) xs
timesn _ [] = undefined

toListC :: Fix (StreamF e) -> [e]
toListC = cata a1
  where
    a1 :: StreamF e [e] -> [e]
    a1 (StreamF e a) = e : a
