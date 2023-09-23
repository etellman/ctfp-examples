module Lib.StreamTest (tests) where

import Ch10.NaturalProperty
import Control.Comonad
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

eqF ::
  (Int -> a) ->
  (a -> (Int -> Int)) ->
  (a -> (Int -> Int)) ->
  PropertyT IO ()
eqF toM f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  n <- forAll $ Gen.int (Range.constant 0 100)

  (f . toM) x n === (g . toM) x n

-- | verifies that f(x) == g(x) for non-negative numbers
eqNonNegative :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f `eqNonNegative` g = do
  x <- forAll $ Gen.int (Range.constant 0 100)
  f x === g x

prop_memoizedMatchesOriginal :: Property
prop_memoizedMatchesOriginal = property $ do
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

prop_extend :: Property
prop_extend = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction

  let xs = countingStream x
  let withY = Cons y xs

  -- exercise and verify
  extend (fmap f) withY === Cons (fmap f withY) (extend (fmap f) xs)

prop_extract :: Property
prop_extract = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)
  let xs = Cons x (countingStream y)

  -- exercise and verify
  extract xs === x

tests :: TestTree
tests =
  testGroup
    "Lib.StreamTest"
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
        ],
      testGroup
        "Comonad"
        [ testProperty "extend" prop_extend,
          testProperty "extract" prop_extract
        ]
    ]
