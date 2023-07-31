module Ch12.EqualizerTest (tests) where

import Ch12.Equalizer
import Hedgehog as H
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.Assertions

tests :: TestTree
tests =
  testGroup
    "Equalizer"
    [ testProperty "all solutions" $ property $ (f . p) `eqFloat` (g . p),
      testGroup
        "One Solution"
        [ testCase "p'" $ (f . p') () @?= (g . p') (),
          testCase "factorize through p" $ p' () @?= (p . h) ()
        ]
    ]
