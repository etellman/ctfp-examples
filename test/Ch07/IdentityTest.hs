module Ch07.IdentityTest (tests) where

import Data.Functor.Identity
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Identity Int -> Identity Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f' (Identity x) === Identity (f x)

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
    "Identity"
    [ testProperty "identity" $ property $ id --> fmap id,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
