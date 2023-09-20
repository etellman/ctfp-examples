module Lib.Reader2Test (tests) where

import Control.Applicative
import Data.Distributive as Dist
import Data.Functor.Rep
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
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

prop_distribute :: Property
prop_distribute =
  property $ do
    -- set up
    e <- forAll $ Gen.int (Range.constant (-100) 100)
    f <- intFunction

    -- exercise
    let r = distribute (Just (reader2 f))

    -- verify
    runReader2 r e === Just (f e)

prop_collect :: Property
prop_collect =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction

    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let fr = \x -> reader2 $ \y -> f x + g y

    -- exercise and verify
    runReader2 (Dist.collect fr (Just m)) n === Just (f m + g n)

prop_representable :: Property
prop_representable =
  property $ do
    -- set up
    f <- intFunction
    s <- forAll $ Gen.int (Range.constant (-100) 100)
    let r = tabulate f :: Reader2 Int Int

    -- exercise and verify
    index r s === f s

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
        ],
      testGroup
        "Distributive"
        [ testProperty "distribute" prop_distribute,
          testProperty "collect" prop_collect
        ],
      testProperty "representable" prop_representable
    ]
