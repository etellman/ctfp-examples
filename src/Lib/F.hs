module Lib.F
  ( F (..),
    fromF,
  )
where

import Data.Distributive
import Data.Functor.Rep

data F a = F a deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

fromF :: F a -> a
fromF (F x) = x

instance Distributive F where
  distribute :: Functor m => m (F a) -> F (m a)
  distribute x = F (fmap fromF x)

  collect :: Functor m => (a -> F b) -> m a -> F (m b)
  collect h = distribute . fmap h

instance Representable F where
  type Rep F = ()

  tabulate :: (() -> a) -> F a
  tabulate h = F (h ())

  index :: F a -> (() -> a)
  index (F x) () = x
