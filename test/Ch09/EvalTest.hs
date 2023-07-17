module Ch09.EvalTest (tests) where

import Ch09.Eval
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

prop_eval :: Property
prop_eval =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    evalF f @== f

tests :: TestTree
tests = testProperty "eval" prop_eval
