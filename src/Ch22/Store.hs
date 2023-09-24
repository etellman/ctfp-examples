module Ch22.Store
  ( Store (..),
  )
where

import Control.Comonad

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extend :: (Store s a -> b) -> Store s a -> Store s b
  extend f x@(Store g s) =
    let h _ = f x
     in Store (h . g) s

  extract :: Store s a -> a
  extract (Store f s) = f s
