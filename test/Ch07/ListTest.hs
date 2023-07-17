module Ch07.ListTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> ([Int] -> [Int]) -> PropertyT IO ()
(-->) f f' = do
  xs <- forAll $ Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 0 100)

  cover 1 "empty" $ null xs
  cover 70 "non-empty" $ (not . null) xs

  f' xs === map f xs

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
    "List"
    [ testProperty "identity" $ property $ id --> fmap id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
