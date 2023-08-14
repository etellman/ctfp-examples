module Ch14.RepresentableTest (tests) where

import Ch14.Representable
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
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

tests :: TestTree
tests =
  testGroup
    "Ch14.RepresentableTest"
    [ testProperty "list alpha" prop_listAlpha
    ]
