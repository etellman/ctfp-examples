module Ch07.IdentityTest (tests) where

import Data.Functor.Identity
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eq :: (Identity Int -> Identity Int) -> (Int -> Int) -> PropertyT IO ()
eq f' f = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f' (Identity x) === Identity (f x)

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ n)

    -- exercise and verify
    fmap f `eq` f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant 2 100)
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    fmap (f . g) (Identity x) === (fmap f . fmap g) (Identity x)

tests :: TestTree
tests =
  testGroup
    "Identity"
    [ testProperty "identity" $ property $ fmap id `eq` id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
