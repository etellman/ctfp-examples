module Ch12.Continuous
  ( ToString (..),
    eitherString,
  )
where

import Data.Functor.Contravariant

newtype ToString a = ToString {getShow :: a -> String}

instance Contravariant ToString where
  contramap :: (a -> b) -> (ToString b -> ToString a)
  contramap f (ToString g) = ToString (g . f)

eitherString :: (Int -> String) -> (Char -> String) -> ToString (Either Int Char)
eitherString f g =
  let op x = case x of
        Left n -> f n
        Right c -> g c
   in ToString op
