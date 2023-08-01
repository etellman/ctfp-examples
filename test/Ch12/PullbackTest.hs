module Ch12.PullbackTest (tests) where

import Ch12.Pullback
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import TestLib.Assertions

tests :: TestTree
tests =
  testGroup
    "Pullback"
    [ testProperty "universal property" $ property $ (f . p . d) `eqChar` (g . q . d),
      testGroup
        "Factorize"
        [ testCase "d'" $ (f . p . d') () @?= (g . q . d') (),
          testCase "factorize through d" $ d' () @?= (d . h) ()
        ]
    ]
