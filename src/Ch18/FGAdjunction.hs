module Ch18.FGAdjunction
  ( unit,
    counit,
    fgUnit,
    fgCounit,
  )
where

import Data.Distributive
import Data.Functor.Adjunction
import Data.Functor.Rep
import Lib.Functors

fgUnit :: F a -> G a
fgUnit (F x) = G x

fgCounit :: G a -> F a
fgCounit (G x) = F x

instance Adjunction F G where
  unit = undefined
  counit = undefined

instance Distributive G where
  distribute = undefined
  collect = undefined

instance Representable G where
  type Rep G = Int

  tabulate :: (Int -> a) -> G a
  tabulate f = G (f 0)

  index :: G a -> (Int -> a)
  index (G x) = \n -> if (n == 0) then x else undefined
