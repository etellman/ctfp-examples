module Ch10.ListToMaybeTest (tests) where

import Ch10.NaturalProperty
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | mapping all lists to Nothing is a valid natural transformation
toNothing :: [a] -> Maybe a
toNothing = const Nothing

listMaybeEq ::
  ([Int] -> Maybe Int) ->
  ([Int] -> Maybe Int) ->
  PropertyT IO ()
listMaybeEq f g = do
  xs <- forAll $ Gen.list (Range.constant 0 20) (Gen.int $ Range.constant 0 100)

  H.cover 1 "empty" $ null xs
  H.cover 80 "non-empty" $ (not . null) xs

  f xs === g xs

tests :: TestTree
tests =
  testGroup
    "List to Maybe Natural Transformations"
    [ testProperty "safeHead" $ prop_natural safeHead listMaybeEq,
      testProperty "always nothing" $ prop_natural toNothing listMaybeEq
    ]
