module Ch07.MaybeTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Maybe Int -> Maybe Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f' (Just x) === Just (f x)
  f' Nothing === Nothing

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    f --> fmap f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction

    -- exercise and verify
    f . g --> fmap f . fmap g

tests :: TestTree
tests =
  testGroup
    "Maybe"
    [ testProperty "identity" $ property $ id --> fmap id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
