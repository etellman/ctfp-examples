module Ch04.Optional
  ( Optional (..),
    (>=>),
    safeReciprocal,
  )
where

import Data.Ratio

data Optional a = Exactly a | Empty deriving (Show, Eq)

-- | compose f and g, applying g to the result of f
(>=>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
(f >=> g) x =
  case (f x) of
    Empty -> Empty
    Exactly y' -> g y'

safeReciprocal :: Rational -> Optional Rational
safeReciprocal x
  | numerator x == 0 = Empty
  | otherwise = Exactly (denominator x % numerator x)
