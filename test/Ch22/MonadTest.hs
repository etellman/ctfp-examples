module Ch22.MonadTest (tests) where

import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

eqFish ::
  (Int -> F Int) ->
  (Int -> F Int) ->
  PropertyT IO ()
eqFish f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f x === g x

f3 :: Int -> F (F (F Int))
f3 = F . F . F

prop_associativity :: Property
prop_associativity = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  ((join . join) $ f3 x) === F x

prop_fish :: Property
prop_fish = property $ do
  -- set up
  f <- fmap F <$> intFunction
  g <- fmap F <$> intFunction

  -- exercise and verify
  (f >=> g) `eqFish` (join . fmap g . f)

prop_compose :: Property
prop_compose = property $ do
  -- set up
  let eqF3 = eq f3

  -- exercise and verify
  (join . join . id) `eqF3` (join . id . join)

tests :: TestTree
tests =
  testGroup
    "Ch22.MonadTest"
    [ testProperty "associativity" $ prop_associativity,
      testProperty "fish" $ prop_fish,
      testProperty "compose" $ prop_compose
    ]
