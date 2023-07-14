module Ch08.K2Test (tests) where

import Ch08.BifunctorTest
import Ch08.K2
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((K2 Int Int Int) -> (K2 Int Int Int)) ->
  PropertyT IO ()
(-->) (_, _) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (K2 x) === K2 x

tests :: TestTree
tests = bifunctorTests "K2" (-->)
