module Ch08.PreListTest (tests) where

import Ch08.BifunctorTest
import Ch08.PreList
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((PreList Int Int) -> (PreList Int Int)) ->
  PropertyT IO ()
(-->) (f, g) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (Cons x y) === Cons (f x) (g y)
  fg Nil === Nil

tests :: TestTree
tests = bifunctorTests "PreList" (-->)
