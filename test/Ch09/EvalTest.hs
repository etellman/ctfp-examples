module Ch09.EvalTest (tests) where

import TestLib.Assertions
import Ch09.Eval
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_eval :: Property
prop_eval =
  property $ do
    -- set up
    let fs = [(+), (-), (*)]
    n <- forAll $ Gen.int (Range.constant 2 100)
    op <- forAll $ Gen.int (Range.constant 0 ((length fs) - 1))

    let f = (fs !! op) n

    -- exercise and verify
    evalF f @== f

tests :: TestTree
tests = testProperty "eval" prop_eval
