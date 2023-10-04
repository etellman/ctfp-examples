module Ch23.CoalgebraTest (tests) where

import Ch23.Coalgebra
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
  let m = ana intToNat :: Int -> Fix NatF
      x = (unfix . m) n :: NatF (Fix NatF)
      y = (fmap Fix . intToNat) n :: NatF (Fix NatF)

  -- exercise and verify
  x === y

tests :: TestTree
tests = testGroup "Ch23.CoalgebraTest" [
  testProperty "commute" $ prop_commute]
