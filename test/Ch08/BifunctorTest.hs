module Ch08.BifunctorTest (tests) where

import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eq ::
  ((Int, Int) -> (Int, Int)) ->
  ((Int -> Int), (Int -> Int)) ->
  PropertyT IO ()
eq fg (f, g) = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (x, y) === (f x, g y)

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    (bimap f g) `eq` (f, g)

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let f = (+ m)
        g = (* m)
        h = (* n)
        j = (+ n)

    -- exercise and verify
    ((bimap f g) . (bimap h j)) `eq` (f . h, g . j)

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" $ property $ (bimap id id) `eq` (id, id),
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
