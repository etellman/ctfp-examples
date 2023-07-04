module Ch03.AndMonoidTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog

newtype And = And {andValue :: Bool} deriving (Eq, Ord, Show)

instance Semigroup And where
  (And x) <> (And y) = And (x && y)

instance Monoid And where
  mempty = And True

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    x <- forAll $ Gen.bool
    y <- forAll $ Gen.bool

    let f = And x
        g = And y

    -- exercise and verify
    f <> g === And (x && y)

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    f <- forAll $ And <$> Gen.bool

    -- exercise and verify
    f <> mempty === f
    mempty <> f === f

tests :: TestTree
tests =
  testGroup
    "And"
    [ testProperty "compose" prop_compose,
      testProperty "identity" prop_identity
    ]
