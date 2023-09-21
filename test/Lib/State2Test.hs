module Lib.State2Test (tests) where

import Control.Applicative
import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Lib.State2
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (State2 Int Int -> State2 Int Int) -> PropertyT IO ()
(-->) f f' = do
  s <- forAll $ Gen.int (Range.constant (-100) 100)
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  let ss = f' (state2 $ \s' -> (s', x))
      (_, x') = runState2 ss s

  f x === x'

infixr 0 -->

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    f <- intFunction
    fs <- intFunction
    g <- intFunction
    gs <- intFunction

    s <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let h = liftA2 (+) (state2 $ \s' -> (fs s', f s')) (state2 $ \s' -> (gs s', g s'))

    -- exercise
    let (hs, hx) = runState2 h s

    -- verify
    hx === (f s) + (g $ fs s)
    hs === (gs . fs $ s)

prop_bind :: Property
prop_bind =
  property $ do
    -- set up
    xi <- forAll $ Gen.int (Range.constant (-100) 100)
    sf <- intFunction
    let f s = (sf s, xi)

    xg <- intFunction
    sg <- intFunction
    let g x = state2 $ \s -> bimap sg xg (s, x)

    si <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let h = state2 f >>= g

    -- exercise and verify
    runState2 h si === ((sg . sf) si, xg xi)

prop_pure :: (Int -> State2 Int Int) -> Property
prop_pure f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    s <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    runState2 (f x) s === (s, x)

prop_join :: (State2 Int (State2 Int Int) -> State2 Int Int) -> Property
prop_join j =
  property $ do
    -- set up
    xi <- forAll $ Gen.int (Range.constant (-100) 100)
    sf <- intFunction
    sf' <- intFunction
    let rr = state2 (\s -> (sf s, state2 $ \s' -> (sf' s', xi)))

    si <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let r = j rr

    -- exercise and verify
    runState2 r si === ((sf' . sf) si, xi)

tests :: TestTree
tests =
  testGroup
    "Lib.State2Test"
    [ functorTests (-->),
      testGroup
        "Applicative"
        [ testProperty "pure" $ prop_pure pure,
          testProperty "liftA2" prop_liftA2
        ],
      testGroup
        "Monad"
        [ testProperty "return" $ prop_pure return,
          testProperty "bind" prop_bind,
          testProperty "join" $ prop_join join,
          testProperty "join alternate" $ prop_join join'
        ]
    ]
