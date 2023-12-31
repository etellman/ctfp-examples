module Ch08.FstTest (tests) where

import Ch08.BifunctorTest
import Ch08.Fst
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((Fst Int Int) -> (Fst Int Int)) ->
  PropertyT IO ()
(-->) (f, _) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (Fst x) === Fst (f x)

tests :: TestTree
tests = bifunctorTests "Fst" (-->)
