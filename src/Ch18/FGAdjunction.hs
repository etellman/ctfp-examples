module Ch18.FGAdjunction
  ( unit,
    counit,
    fgUnit,
    fgCounit,
  )
where

import Data.Functor.Adjunction
import Lib.Functors
import Lib.Stream

fgUnit :: F a -> G a
fgUnit (F x) = G x

fgCounit :: G a -> F a
fgCounit (G x) = F x

instance Adjunction F Stream where
  unit :: a -> Stream (F a)
  unit x = Cons (F x) undefined

  counit :: F (Stream a) -> a
  counit (F (Cons x _)) = x
