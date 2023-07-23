module Ch10.TwoCategoryTest (tests) where

import Ch10.NaturalProperty
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

data F' a = F' a deriving (Eq, Show)

instance Functor F' where
  fmap f (F' a) = F' (f a)

data G' a = G' a deriving (Eq, Show)

instance Functor G' where
  fmap f (G' a) = G' (f a)

alpha :: F a -> F' a
alpha (F x) = F' x

beta :: G a -> G' a
beta (G x) = G' x

f'ToG' :: F' a -> G' a
f'ToG' (F' x) = G' x

f'ToG :: F' a -> G a
f'ToG (F' x) = G x

tests :: TestTree
tests =
  testGroup
    "2-Categories"
    [ testProperty "F -> G" $ prop_natural fToG (eq F),
      testProperty "F -> G -> G'" $ prop_natural (beta . fToG) (eq F),
      testProperty "F -> F' -> G" $ prop_natural (f'ToG . alpha) (eq F),
      testProperty "F -> F' -> G'" $ prop_natural (f'ToG' . alpha) (eq F)
    ]
