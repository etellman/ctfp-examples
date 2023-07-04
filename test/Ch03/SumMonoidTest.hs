module Ch03.SumMonoidTest (tests) where

import Data.Semigroup
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    i <- forAll $ Gen.int (Range.constant 0 1000)
    j <- forAll $ Gen.int (Range.constant 0 1000)

    let f = Sum i
        g = Sum j

    -- exercise and verify
    f <> g === Sum (i + j)

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    f <- forAll $ Sum <$> Gen.int (Range.constant 0 1000)
    let sumId = Sum 0

    -- exercise and verify
    f <> sumId === f
    sumId <> f === f

tests :: TestTree
tests =
  testGroup
    "Sum Monoid"
    [ testProperty "compose" prop_compose,
      testProperty "identity" prop_identity
    ]
