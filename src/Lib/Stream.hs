module Lib.Stream
  ( Stream (..),
    countingStream,
  )
where

import Control.Comonad
import Data.Distributive
import Data.Functor.Rep

data Stream a = Cons a (Stream a) deriving (Show)

streamTail :: Stream a -> Stream a
streamTail (Cons _ xs) = xs

countingStream :: Int -> Stream Int
countingStream n = Cons n (countingStream (n + 1))

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

eqN :: (Eq a) => Int -> Stream a -> Stream a -> Bool
eqN 0 _ _ = True
eqN n (Cons x xs) (Cons y ys) = x == y && eqN (n - 1) xs ys

instance (Eq a) => Eq (Stream a) where
  xs == ys = eqN 100 xs ys

instance Distributive Stream where
  distribute :: Functor f => f (Stream a) -> Stream (f a)
  distribute xs = Cons (fmap extract xs) (distribute $ fmap streamTail xs)

  collect :: Functor f => (a -> Stream b) -> f a -> Stream (f b)
  collect h = distribute . fmap h

instance Representable Stream where
  type Rep Stream = Int

  tabulate :: (Int -> a) -> Stream a
  tabulate f = Cons (f 0) (tabulate (f . (+ 1)))

  index :: Stream a -> (Int -> a)
  index (Cons b bs) n
    | n == 0 = b
    | otherwise = index bs (n - 1)

instance Comonad Stream where
  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f as@(Cons _ as') = Cons (f as) (extend f as')

  extract :: Stream a -> a
  extract (Cons a _) = a
