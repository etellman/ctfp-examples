module Ch23.CatamorphismTest (tests) where

import Ch23.Fix
import Ch23.NatF
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_commute :: Property
prop_commute = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)
  let ffx = fmap Fix $ intToNat n :: NatF (Fix NatF)
      m = natToInt . fmap m . unfix :: Fix NatF -> Int

  -- exercise and verify
  (m . Fix) ffx === (natToInt . fmap m) ffx

tests :: TestTree
tests =
  testGroup
    "Ch23.CatamorphismTest"
    [ testProperty "non-zero" $ prop_commute
    ]
