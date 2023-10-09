module Ch24.Lens
  ( Lens (..),
    coalg,
  )
where

import Lib.Store

class Lens a s where
  set :: a -> s -> a
  get :: a -> s

coalg :: Lens a s => a -> Store s a
coalg a = Store (set a) (get a)
