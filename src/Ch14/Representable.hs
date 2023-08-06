module Ch14.Representable
  ( listAlpha,
    Representable (..),
    Stream (..),
  )
where

import Data.Kind

-- not representable, since listBeta can't be defined
listAlpha :: (Int -> x) -> [x]
listAlpha h = fmap h [12]

-- | t may be some kind of container
class Representable t where
  -- type of the contents of the container
  type Rep t :: Type

  -- alpha -- function to container
  tabulate :: (Rep t -> a) -> t a

  -- beta -- container to function
  index :: t a -> (Rep t -> a)

data Stream a = Cons a (Stream a) deriving (Show)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

eqN :: (Eq a) => Int -> Stream a -> Stream a -> Bool
eqN 0 _ _ = True
eqN n (Cons x xs) (Cons y ys) = x == y && eqN (n - 1) xs ys

instance (Eq a) => Eq (Stream a) where
  xs == ys = eqN 100 xs ys

instance Representable Stream where
  type Rep Stream = Int
  tabulate f = Cons (f 0) (tabulate (f . (+ 1)))
  index (Cons b bs) n
    | n == 0 = b
    | otherwise = index bs (n - 1)
