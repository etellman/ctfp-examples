module Ch12.Product
  ( C (..),
    AB (..),
    alpha,
    delta,
    d0,
    d1,
    p,
    q,
    ZeroOne (..),
  )
where

import Data.Functor.Contravariant

data ZeroOne = Zero | One deriving (Eq, Show)

data C = C deriving (Eq, Show)

data AB = A | B deriving (Eq, Show)

d0 :: ZeroOne -> AB
d0 Zero = A
d0 One = undefined

d1 :: ZeroOne -> AB
d1 Zero = undefined
d1 One = B

p :: C -> AB
p C = A

q :: C -> AB
q C = B

delta :: ZeroOne -> C
delta _ = C

alpha :: (Op AB C) -> (Op AB ZeroOne)
alpha = contramap delta
