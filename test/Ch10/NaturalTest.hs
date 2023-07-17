module Ch10.NaturalTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

eq ::
  (F Int -> G Int) ->
  (F Int -> G Int) ->
  PropertyT IO ()
eq f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f (F x) === g (F x)

prop_natural :: Property
prop_natural =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    (fmap f . alpha) `eq` (alpha . fmap f)

tests :: TestTree
tests = testProperty "natural transformation" prop_natural
