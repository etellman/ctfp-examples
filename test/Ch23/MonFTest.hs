module Ch23.MonFTest (tests) where

import Ch23.Fix
import Control.Comonad
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_lenAlg :: Property
prop_lenAlg = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)

  -- exercise and verify
  lenAlg (ConsF 'a' n) === n + 1

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
    [ testGroup
        "F-algebra category"
        [ testProperty "morphism" prop_algebraMorphism
        ],
      testGroup
        "list length algebra"
        [ testProperty "non-empty" prop_lenAlg,
          testCase "empty" $ lenAlg (NilF ()) @=? 0
        ]
    ]
