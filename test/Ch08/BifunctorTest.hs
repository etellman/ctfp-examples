module Ch08.BifunctorTest (tests) where

import Assertions.Hedgehog
import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_identity :: Property
prop_identity =
  property $ do
    -- exercise
    let idPair = bimap id id

    -- verify
    idPair `eqPairF` id

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

        fg = bimap f g
        hj = bimap h j

    -- exercise and verify
    (fg . hj) `eqPairF` \(x, y) -> ((f . h) x, (g . j) y)

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" prop_identity,
      testProperty "compose" prop_compose
    ]
