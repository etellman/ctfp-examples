module Ch07.ReaderTest (tests) where

import Assertions.Hedgehog
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: (Char -> Int) -> ((Char -> Char) -> (Char -> Int)) -> PropertyT IO ()
(-->) f f' = do
  c <- forAll $ Gen.alpha
  (f' toUpper) c === (f . toUpper) c

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 1 100)
    let f = (n +) . ord

    -- exercise and verify
    f --> fmap f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (m +)
        g = (n +) . ord

    -- exercise and verify
    f . g --> fmap f . fmap g

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 1 100)
    let f = (n +)

    -- exercise and verify
    (fmap id) f @== id . f

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
