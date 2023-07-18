module Ch10.ConstTest (tests) where

import Ch10.NaturalProperty
import Data.Functor.Const
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

length' :: [a] -> Const Int a
length' [] = Const 0
length' (_ : xs) = Const $ 1 + getConst (length' xs)

eq ::
  (Eq (m Int), Show (m Int)) =>
  ([Int] -> m Int) ->
  ([Int] -> m Int) ->
  PropertyT IO ()
eq f g = do
  xs <- forAll $ Gen.list (Range.constant 0 20) (Gen.int $ Range.constant 0 100)

  H.cover 1 "empty" $ null xs
  H.cover 80 "non-empty" $ (not . null) xs

  f xs === g xs

tests :: TestTree
tests =
  testGroup
    "List to Maybe Natural Transformations"
    [
    testProperty "length" $ prop_natural length' eq
    ]
