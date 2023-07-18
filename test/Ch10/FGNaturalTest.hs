module Ch10.FGNaturalTest (tests) where

import Ch10.NaturalTest
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

fgEq ::
  (F Int -> G Int) ->
  (F Int -> G Int) ->
  PropertyT IO ()
fgEq f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f (F x) === g (F x)

tests :: TestTree
tests = testProperty "natural transformation" $ prop_natural fToG fgEq
