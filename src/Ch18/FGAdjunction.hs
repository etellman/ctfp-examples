module Ch18.FGAdjunction () where

import Data.Functor.Adjunction
import Lib.F
import Lib.G

instance Adjunction G F where
  unit :: a -> F (G a)
  unit x = F (G x)

  counit :: G (F a) -> a
  counit (G (F x)) = x

  leftAdjunct :: (G a -> b) -> (a -> F b)
  leftAdjunct f x = F $ f (G x)

  rightAdjunct :: (a -> F b) -> (G a -> b)
  rightAdjunct f (G x) = fromF (f x)
