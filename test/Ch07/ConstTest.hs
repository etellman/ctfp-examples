module Ch07.ConstTest (tests) where

import Data.Functor.Const
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    -- exercise and verify
    fmap id (Const x) === Const (id x)

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)
    f <- intFunction
    g <- intFunction

    -- exercise and verify
    fmap (f . g) (Const x) === (fmap f . fmap g) (Const x)

tests :: TestTree
tests =
  testGroup
    "Constant"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
