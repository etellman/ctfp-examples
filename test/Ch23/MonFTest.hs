module Ch23.MonFTest (tests) where

import Control.Comonad
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog

prop_algebraMorphism :: Property
prop_algebraMorphism = property $ do
  -- set up
  fn <- forAll $ F <$> ord <$> Gen.alpha
  let f = extract :: (F Int -> Int)
      g = extract :: (F Char -> Char)
      m = chr :: Int -> Char

  -- exercise and verify
  (g . fmap m) fn === (m . f) fn

tests :: TestTree
tests =
  testGroup
    "Ch23.MonFTest"
    [ testProperty "morphism" prop_algebraMorphism
    ]
