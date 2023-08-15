module Lib.StreamTest (tests) where

import Ch10.NaturalProperty
import Data.Distributive as Dist
import Data.Functor.Rep
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Stream
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

-- | verifies that f(x) == g(x) for natural numbers
eqNonNegative :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f `eqNonNegative` g = do
  x <- forAll $ Gen.int (Range.constant 0 100)
  f x === g x

prop_memoizedMatchesOriginal :: Property
prop_memoizedMatchesOriginal =
  property $ do
    f <- intFunction
    let xs = tabulate f :: Stream Int

    (index xs) `eqNonNegative` f

prop_distribute :: Property
prop_distribute =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    let xs = countingStream x

    distribute (Just xs) === fmap Just xs

prop_collect :: Property
prop_collect =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    Dist.collect countingStream (Just x) === fmap Just (countingStream x)

eqF ::
  (Int -> a) ->
  (a -> (Int -> Int)) ->
  (a -> (Int -> Int)) ->
  PropertyT IO ()
eqF toM f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  n <- forAll $ Gen.int (Range.constant 0 100)

  (f . toM) x n === (g . toM) x n

tests :: TestTree
tests =
  testGroup
    "Ch14.StreamTest"
    [ testGroup
        "Memoized Function"
        [ testProperty "memoized value matches" prop_memoizedMatchesOriginal,
          testProperty "tabulate natural transformation" $ do
            prop_natural (tabulate :: (Int -> Int) -> Stream Int) (eq const),
          testProperty "index natural transformation" $ do
            prop_natural (index :: Stream Int -> (Int -> Int)) (eqF countingStream)
        ],
      testGroup
        "Distributive"
        [ testProperty "distribute Maybe" prop_distribute,
          testProperty "collect Maybe" prop_collect
        ]
    ]
