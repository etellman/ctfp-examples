module Ch07.ReaderTest (tests) where

import Assertions.Hedgehog
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: (Int -> Int) -> ((Int -> Int) -> (Int -> Int)) -> PropertyT IO ()
(-->) f f' = do
  k <- forAll $ Gen.int (Range.constant (-100) 100)
  let h = (* k)

  f' h @== f . h

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 1 100)
    let f = (+ n)

    -- exercise and verify
    f --> fmap f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    f . g --> fmap f . fmap g

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" $ property $ id --> fmap id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
