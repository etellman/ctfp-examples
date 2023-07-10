module Ch08.Maybe2Test (tests) where

import Ch08.Maybe2
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: (Int -> Int) -> (Maybe2 Int -> Maybe2 Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f' (just x) === just (f x)

infixr 0 -->

verify :: (Int -> Int) -> PropertyT IO ()
verify f = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  (lift f) nothing === nothing
  (lift f) (just x) === just (f x)

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
    f . g --> lift f . lift g
    f . g --> lift (f . g)
    (lift f . lift g) nothing === nothing

prop_toMaybe :: Property
prop_toMaybe =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    -- exercise and verify
    toMaybe (just x) === Just x
    toMaybe nothing === (Nothing :: Maybe Int)

prop_fromMaybe :: Property
prop_fromMaybe =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    -- exercise and verify
    fromMaybe (Just x) === just x
    fromMaybe Nothing === (nothing :: Maybe2 Int)

prop_toFromMaybe :: Property
prop_toFromMaybe =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    -- exercise and verify
    (fromMaybe . toMaybe) (just x) === just x
    (fromMaybe . toMaybe) nothing === (nothing :: Maybe2 Int)

prop_fromToMaybe :: Property
prop_fromToMaybe =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)

    -- exercise and verify
    (toMaybe . fromMaybe) (Just x) === Just x
    (toMaybe . fromMaybe) Nothing === (Nothing :: Maybe Int)

tests :: TestTree
tests =
  testGroup
    "Maybe2"
    [ testProperty "identity" $ property (verify id),
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose,
      testProperty "to Maybe" prop_toMaybe,
      testProperty "from Maybe" prop_fromMaybe,
      testProperty "to/from Maybe" prop_toFromMaybe,
      testProperty "from/to Maybe" prop_fromToMaybe
    ]
