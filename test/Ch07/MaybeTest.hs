module Ch07.MaybeTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eq :: (Maybe Int -> Maybe Int) -> (Int -> Int) -> PropertyT IO ()
eq f' f = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f' (Just x) === Just (f x)

verify :: (Int -> Int) -> PropertyT IO ()
verify f = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  (fmap f) (Just x) === Just (f x)
  (fmap f) Nothing === Nothing

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ n)

    -- exercise and verify
    verify f

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
    (fmap f . fmap g) Nothing === Nothing

tests :: TestTree
tests =
  testGroup
    "Maybe"
    [ testProperty "identity" $ property (verify id),
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
