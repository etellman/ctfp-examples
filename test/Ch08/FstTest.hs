module Ch08.FstTest (tests) where

import Ch08.Fst
import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((Fst Int Int) -> (Fst Int Int)) ->
  PropertyT IO ()
(-->) (f, _) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (Fst x) === Fst (f x)

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
    "K2"
    [ testProperty "identity" $ property $ (id, id) --> bimap id id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
