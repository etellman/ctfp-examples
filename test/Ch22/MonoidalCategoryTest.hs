module Ch22.MonoidalCategoryTest (tests) where

import Ch22.MonoidalCategory
import Data.Bifunctor
import Data.Monoid
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_alpha :: Property
prop_alpha = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)
  z <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  alpha ((x, y), z) === (x, (y, z))

prop_associativity :: (Monoid m, Show m, Eq m) => (Int -> m) -> Property
prop_associativity toM = property $ do
  -- set up
  x <- forAll $ toM <$> Gen.int (Range.constant (-100) 100)
  y <- forAll $ toM <$> Gen.int (Range.constant (-100) 100)
  z <- forAll $ toM <$> Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  (mu . bimap id mu . alpha $ ((x, y), z)) === (mu . bimap mu id $ ((x, y), z))

prop_unit :: (Monoid m, Show m, Eq m) => (Int -> m) -> Property
prop_unit toM = property $ do
  -- set up
  x <- forAll $ toM <$> Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  mu (eta (), x) === x
  mu (x, eta ()) === x

  (mu . bimap eta id) ((), x) === lambda ((), x)
  (mu . bimap id eta) (x, ()) === rho (x, ())

prop_monoid :: (Monoid m, Show m, Eq m) => String -> (Int -> m) -> TestTree
prop_monoid name toM =
  testGroup
    name
    [ testProperty "associativity" $ prop_associativity toM,
      testProperty "unit" $ prop_unit toM
    ]

tests :: TestTree
tests =
  testGroup
    "Ch22.MonoidalCategoryTest"
    [ testProperty "alpha" $ prop_alpha,
      prop_monoid "Sum" Sum,
      prop_monoid "Product" Product
    ]
