module Ch23.CatamorphismTest (tests) where

import Ch23.Fix
import Ch23.NatF
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- add intermediate steps to clarify what's going on
m :: Fix NatF -> Int
m x =
  let nextLevel = unfix x :: NatF (Fix NatF)
      natural = fmap m nextLevel :: NatF Int
      alg = natToInt
   in alg natural

prop_commute :: Property
prop_commute = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)
  let ffx = fmap Fix $ intToNat n :: NatF (Fix NatF)
      m' = natToInt . fmap m' . unfix

  -- exercise and verify
  (m . Fix) ffx === (natToInt . fmap m) ffx
  (m' . Fix) ffx === (natToInt . fmap m') ffx

tests :: TestTree
tests =
  testGroup
    "Ch23.CatamorphismTest"
    [ testProperty "commute" $ prop_commute
    ]
