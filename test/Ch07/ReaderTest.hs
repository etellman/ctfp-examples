module Ch07.ReaderTest (tests) where

import Assertions.Hedgehog
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    k <- forAll $ Gen.int (Range.constant 2 100)
    let h = (* k) :: Int -> Int

    -- exercise and verify
    (fmap id) h @== h

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)
    k <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m) :: Int -> Int
        g = (* n) :: Int -> Int
        h = (* k) :: Int -> Int

    -- exercise and verify
    (fmap f . fmap g) h @== fmap (f . g) h

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
