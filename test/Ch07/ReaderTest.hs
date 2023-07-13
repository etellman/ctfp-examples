module Ch07.ReaderTest (tests) where

import Assertions.Hedgehog
import Control.Monad.Reader
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: (Char -> Int) -> Reader Char Int -> PropertyT IO ()
(-->) f f' = do
  c <- forAll $ Gen.alpha

  f c === runReader f' c

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ ord --> mapReader ord (reader id)

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (n +)
        g = ord

    -- exercise and verify
    f . g --> (mapReader f . mapReader g) (reader id)

prop_identity :: Property
prop_identity =
  property $ do
    id @== runReader $ mapReader id (reader id)

tests :: TestTree
tests =
  testGroup
    "Reader"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
