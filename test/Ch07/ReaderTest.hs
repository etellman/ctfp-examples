module Ch07.ReaderTest (tests) where

import Assertions.Hedgehog
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eq :: ((Int -> Int) -> (Int -> Int)) -> (Int -> Int) -> PropertyT IO ()
eq f' f = do
  k <- forAll $ Gen.int (Range.constant (-100) 100)
  let h = (* k)

  f' h @== f . h

prop_identity :: Property
prop_identity = property $ (fmap id) `eq` id

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 1 100)
    let f = (+ n)

    -- exercise and verify
    fmap f `eq` f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    (fmap f . fmap g) `eq` (f . g)

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
