module Lib.FTest (tests) where

import Data.Char
import Data.Distributive as Dist
import Data.Functor.Rep
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog

prop_distribute :: Property
prop_distribute =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    distribute (Just (F x)) === F (Just x)

prop_collect :: Property
prop_collect =
  property $ do
    -- set up
    c <- forAll $ Gen.alpha
    let h n = F (chr n)

    -- exercise and verify
    Dist.collect h (Just (ord c)) === F (Just c)

prop_tabulate :: Property
prop_tabulate = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  let h = const x

  -- exercise and verify
  tabulate h === F x

prop_index :: Property
prop_index = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise
  let h = index (F x)

  -- verify
  h () === x

tests :: TestTree
tests =
  testGroup
    "Lib.FTest"
    [ testGroup
        "Distributive"
        [ testProperty "distribute Maybe" prop_distribute,
          testProperty "collect Maybe" prop_collect
        ],
      testGroup
        "Representable"
        [ testProperty "tabulate" prop_tabulate,
          testProperty "index" prop_index
        ]
    ]
