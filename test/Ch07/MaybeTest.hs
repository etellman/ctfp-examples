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
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)
        f' = fmap f :: Maybe Int -> Maybe Int
        g' = fmap g :: Maybe Int -> Maybe Int

    -- exercise and verify
    (f' . g') (Just x) === Just ((f . g) x)
    (f' . g') Nothing === Nothing

tests :: TestTree
tests =
  testGroup
    "Maybe"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
