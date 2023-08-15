module Lib.Functors
  ( F (..),
    G (..),
    H (..),
    fToG,
    fToH,
    gToH,
  )
where

import Data.Distributive
import Data.Functor.Adjunction
import Data.Functor.Rep

data F a = F a deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

data G a = G a deriving (Eq, Show)

instance Functor G where
  fmap f (G a) = G (f a)

data H a = H a deriving (Eq, Show)

instance Functor H where
  fmap f (H a) = H (f a)

fToG :: F a -> G a
fToG (F x) = G x

fToH :: F a -> H a
fToH (F x) = H x

gToH :: G a -> H a
gToH (G x) = H x

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

instance Adjunction G F where
  unit :: a -> F (G a)
  unit x = F (G x)

  counit :: G (F a) -> a
  counit (G (F x)) = x
