module Ch14.RepresentableTest (tests) where

import Ch10.NaturalProperty
import Ch14.Representable
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

-- | verifies that f(x) == g(x) for natural numbers
eqNonNegative :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f `eqNonNegative` g = do
  x <- forAll $ Gen.int (Range.constant 0 100)
  f x === g x

prop_listAlpha :: Property
prop_listAlpha =
  property $ do
    h <- intFunction
    g <- intFunction

    -- create a list and then apply h to all the elements
    let f' = fmap h . listAlpha

    -- compose h with another function and then apply the result to all the elements in the list
    let f'' = listAlpha . fmap h

    f' g === f'' g

memoizedMatchesOriginal :: Property
memoizedMatchesOriginal =
  property $ do
    f <- intFunction
    let xs = tabulate f :: Stream Int

    (index xs) `eqNonNegative` f

countingStream :: Int -> Stream Int
countingStream n = Cons n (countingStream (n + 1))

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
    "Representable Functors"
    [ testProperty "list alpha" prop_listAlpha,
      testGroup
        "Memoized Function"
        [ testProperty "memoized value matches" memoizedMatchesOriginal,
          testProperty "tabulate natural transformation" $ do
            prop_natural (tabulate :: (Int -> Int) -> Stream Int) (eq const),
          testProperty "index natural transformation" $ do
            prop_natural (index :: Stream Int -> (Int -> Int)) (eqF countingStream)
        ]
    ]
