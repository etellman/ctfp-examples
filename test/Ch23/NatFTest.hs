module Ch23.NatFTest (tests) where

import Ch23.Fix
import Ch23.NatF
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_naturalToInt :: (a -> Int) -> (Int -> a) -> Property
prop_naturalToInt toInt fromInt = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)

  -- exercise and verify
  (toInt . fromInt) n === n

tests :: TestTree
tests =
  testGroup
    "Ch23.NatFTest"
    [ testGroup
        "Natural to Int"
        [ testProperty "non-zero" $ prop_naturalToInt natToInt intToNat,
          testCase "zero" $ natToInt ZeroF @=? 0
        ],
      testGroup
        "Natural to Int with Fix"
        [ testProperty "non-zero" $ prop_naturalToInt natToIntFix intToNatFix,
          testCase "zero" $ natToIntFix (Fix ZeroF) @=? 0
        ]
    ]
