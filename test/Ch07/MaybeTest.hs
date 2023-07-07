module Ch07.MaybeTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    let idInt = id :: (Int -> Int)

    -- exercise and verify
    fmap idInt (Just x) === Just (idInt x)
    fmap idInt Nothing === Nothing

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ n)

    -- exercise and verify
    fmap f (Just x) === Just (f x)
    fmap f Nothing === Nothing

tests :: TestTree
tests =
  testGroup
    "Maybe Functor"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
