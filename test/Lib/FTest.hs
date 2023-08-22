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
import TestLib.Assertions
import TestLib.IntFunction

-- eqF ::
--   (F Int -> F Int) ->
--   (F Int -> F Int) ->
--   PropertyT IO ()
-- eqF = eq F

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

prop_lift :: Property
prop_lift = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction

  -- exercise
  let fx = (F f) <*> (F x)

  -- verify
  fx === (F $ f x)

prop_fish :: Property
prop_fish = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction

  -- exercise
  let actual = F x >>= fmap pure f

  -- verify
  actual === (F $ f x)

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
        ],
      testGroup
        "Applicative"
        [ testProperty "pure" $ property $ pure @== F,
          testProperty "lift" prop_lift
        ],
      testGroup
        "Monad"
        [ testProperty "fish" prop_fish
        ]
    ]
