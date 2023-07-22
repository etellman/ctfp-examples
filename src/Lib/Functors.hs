module Lib.Functors
  ( F (..),
    G (..),
    H (..),
    fToG,
    fToH,
    gToH,
  )
where

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
