module Ch14.RepresentableTest (tests) where

import Ch14.Representable
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

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

    (index xs) `eqNat` f

tests :: TestTree
tests =
  testGroup
    "Representable Functors"
    [ testProperty "list alpha" prop_listAlpha,
      testProperty "memoized function" memoizedMatchesOriginal
    ]
