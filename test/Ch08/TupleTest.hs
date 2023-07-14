module Ch08.TupleTest (tests) where

import Ch08.BifunctorTest
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((Int, Int) -> (Int, Int)) ->
  PropertyT IO ()
(-->) (f, g) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (x, y) === (f x, g y)

tests :: TestTree
tests = bifunctorTests "Tuple" (-->)
