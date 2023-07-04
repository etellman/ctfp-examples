module Ch03.OrMonoidTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

newtype Or = Or {andValue :: Bool} deriving (Eq, Ord, Show)

instance Semigroup Or where
  (Or x) <> (Or y) = Or (x || y)

instance Monoid Or where
  mempty = Or False

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    x <- forAll $ Gen.bool
    y <- forAll $ Gen.bool

    let f = Or x
        g = Or y

    -- exercise and verify
    f <> g === Or (x || y)

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    f <- forAll $ Or <$> Gen.bool

    -- exercise and verify
    f <> mempty === f
    mempty <> f === f

tests :: TestTree
tests =
  testGroup
    "Or"
    [ testProperty "compose" prop_compose,
      testProperty "identity" prop_identity
    ]
