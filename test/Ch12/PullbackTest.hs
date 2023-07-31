module Ch12.PullbackTest (tests) where

import Ch12.Pullback
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

tests :: TestTree
tests = testProperty "Pullback" $ property $ (f . p) `eqChar` (g . p)
