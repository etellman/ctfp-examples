module Lib.Reader2Test (tests) where

import Lib.FunctorProperties
import Control.Applicative
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Reader2
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Reader2 Int Int -> Reader2 Int Int) -> PropertyT IO ()
(-->) f f' = do
  e <- forAll $ Gen.int (Range.constant (-100) 100)

  f e === runReader2 (f' (reader2 id)) e

infixr 0 -->

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction
    e <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let h = liftA2 (+) (reader2 f) (reader2 g)

    -- verify
    runReader2 h e === (f e) + (g e)

prop_bind :: Property
prop_bind =
  property $ do
    -- set up
    f <- intFunction
    e <- forAll $ Gen.int (Range.constant (-100) 100)
    let g y = reader2 (\e' -> y + e')

    -- exercise
    let h = reader2 f >>= g

    -- exercise and verify
    runReader2 h e === f e + e

prop_pure :: (Int -> Reader2 Int Int) -> Property
prop_pure f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    e <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    runReader2 (f x) e === x

prop_join :: Property
prop_join =
  property $ do
    -- set up
    f <- intFunction
    let rr = reader2 (\_ -> reader2 f)

    -- exercise and verify
    runReader2 (join rr) @== f

tests :: TestTree
tests =
  testGroup
    "Lib.Reader2Test"
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
          testProperty "join" prop_join
        ]
    ]
