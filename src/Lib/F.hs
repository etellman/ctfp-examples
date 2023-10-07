module Lib.F
  ( F (..),
    bind,
  )
where

import Control.Comonad
import Data.Distributive
import Data.Functor.Rep

data F a = F {fromF :: a} deriving (Eq, Show)

instance Functor F where
  fmap f (F a) = F (f a)

instance Distributive F where
  distribute :: Functor m => m (F a) -> F (m a)
  distribute x = F (fmap fromF x)

  collect :: Functor m => (a -> F b) -> m a -> F (m b)
  collect h = distribute . fmap h

instance Representable F where
  type Rep F = ()

  tabulate :: (() -> a) -> F a
  tabulate h = F $ h ()

  index :: F a -> (() -> a)
  index (F x) = \() -> x

instance Applicative F where
  pure :: a -> F a
  pure x = F x

  (<*>) :: F (a -> b) -> F a -> F b
  (F f) <*> (F x) = F $ f x

instance Monad F where
  (>>=) :: F a -> (a -> F b) -> F b
  F x >>= f = f x

join :: F (F a) -> F a
join (F (F a)) = F a

-- alternative definition
bind :: F a -> (a -> F b) -> F b
bind x f = join $ fmap f x

instance Comonad F where
  extend :: (F a -> b) -> F a -> F b
  extend f x = F (f x)

  extract :: F a -> a
  extract = fromF
