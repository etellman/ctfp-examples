module Ch08.SndTest (tests) where

import Ch08.BifunctorTest
import Ch08.Snd
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty

(-->) ::
  ((Int -> Int), (Int -> Int)) ->
  ((Snd Int Int) -> (Snd Int Int)) ->
  PropertyT IO ()
(-->) (_, g) fg = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  fg (Snd x) === Snd (g x)

tests :: TestTree
tests = bifunctorTests "Snd" (-->)
