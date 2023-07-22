module Ch10.ListNaturalTest (tests) where

import Ch10.NaturalProperty
import Data.Functor.Const
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eqList ::
  (Eq (m Int), Show (m Int)) =>
  ([Int] -> m Int) ->
  ([Int] -> m Int) ->
  PropertyT IO ()
eqList f g = do
  xs <- forAll $ Gen.list (Range.constant 0 20) (Gen.int $ Range.constant 0 100)

  H.cover 1 "empty" $ null xs
  H.cover 80 "non-empty" $ (not . null) xs

  f xs === g xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | mapping all lists to Nothing is a valid natural transformation
toNothing :: [a] -> Maybe a
toNothing = const Nothing

length' :: [a] -> Const Int a
length' [] = Const 0
length' (_ : xs) = Const $ 1 + getConst (length' xs)

tests :: TestTree
tests =
  testGroup
    "List Natural Transformations"
    [ testProperty "safeHead" $ prop_natural safeHead eqList,
      testProperty "always nothing" $ prop_natural toNothing eqList,
      testProperty "length with Const" $ prop_natural length' eqList
    ]
