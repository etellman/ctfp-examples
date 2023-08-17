module Lib.NaturalTransformations
  (
    fToG,
    fToH,
    gToH,
  )
where

import Lib.F
import Lib.G
import Lib.H

fToG :: F a -> G a
fToG (F x) = G x

fToH :: F a -> H a
fToH (F x) = H x

gToH :: G a -> H a
gToH (G x) = H x
