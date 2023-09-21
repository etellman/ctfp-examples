module Ch22.State3
  (
  )
where

import Data.Functor.Adjunction
import Lib.Reader2

newtype Prod s a = Prod (a, s)

instance Functor (Prod s) where
  fmap f = \(Prod (a, s)) -> Prod (f a, s)

instance Adjunction (Prod s) (Reader2 s) where
  -- unit :: a -> F (G a)
  unit :: a -> Reader2 s (Prod s a)
  unit a = reader2 (\s -> Prod (a, s))

  -- counit :: G (F a) -> a
  counit :: Prod s (Reader2 s a) -> a
  counit (Prod (r, s)) = runReader2 r s
