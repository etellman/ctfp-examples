module Ch10.ReaderNaturalTest (tests) where

import Ch10.NaturalProperty
import Control.Monad.Reader
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

constReader :: Int -> Reader () Int
constReader n = reader $ const n

toNothing :: Reader () Int -> Maybe Int
toNothing _ = Nothing

toJust :: Reader () Int -> Maybe Int
toJust f = Just $ runReader f ()

toF :: Reader () Int -> F Int
toF f = F $ runReader f ()

tests :: TestTree
tests =
  testGroup
    "Reader Natural Transformations"
    [ testProperty "to nothing" $ prop_natural toNothing (eq constReader),
      testProperty "to constant" $ prop_natural toJust (eq constReader),
      testProperty "to F" $ prop_natural toF (eq constReader)
    ]
