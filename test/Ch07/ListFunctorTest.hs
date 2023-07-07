module Ch07.ListFunctorTest (tests) where

import Assertions.Hedgehog
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity :: Property
prop_identity =
  property $ do
    (fmap id) `eqIntsF` id

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    (fmap f . fmap g) `eqIntsF` fmap (f . g)

tests :: TestTree
tests =
  testGroup
    "List Functor"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
