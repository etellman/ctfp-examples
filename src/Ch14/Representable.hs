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

data Stream x = Cons x (Stream x)

instance Representable Stream where
  type Rep Stream = Int
  tabulate f = Cons (f 0) (tabulate (f . (+ 1)))
  index (Cons b bs) n
    | n == 0 = b
    | otherwise = index bs (n - 1)
