module Ch08.BifunctorTest (tests) where

import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((Int, Int) -> (Int, Int)) ->
  PropertyT IO ()
(-->) (f, g) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (x, y) === (f x, g y)

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    (f, g) --> bimap f g

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
    (f . h, g . j) --> (bimap f g) . (bimap h j)

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" $ property $ (id, id) --> bimap id id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
