module Lib.Reader2Test (tests) where

import Control.Applicative
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Reader2
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Reader2 Int Int -> Reader2 Int Int) -> PropertyT IO ()
(-->) f f' = do
  e <- forAll $ Gen.int (Range.constant (-100) 100)

  f e === runReader2 (f' (reader2 id)) e

infixr 0 -->

prop_morphism :: Property
prop_morphism = property $ do
  -- set up
  f <- intFunction

  -- exercise and verify
  f --> fmap f

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction

    -- exercise and verify
    f . g --> fmap f . fmap g

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction
    e <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let h = liftA2 (+) (reader2 f) (reader2 g)
        h' = (+) <$> (reader2 f) <*> (reader2 g)

    -- exercise and verify
    runReader2 h e === (f e) + (g e)
    runReader2 h' e === (f e) + (g e)

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

tests :: TestTree
tests =
  testGroup
    "Lib.Reader2Test"
    [ testGroup
        "Functor"
        [ testProperty "identity" $ property $ id --> fmap id,
          testProperty "morphism" prop_morphism,
          testProperty "compose" prop_compose
        ],
      testGroup
        "Applicative"
        [ testProperty "pure" $ prop_pure pure,
          testProperty "liftA2" prop_liftA2
        ],
      testGroup
        "Monad"
        [ testProperty "return" $ prop_pure return,
          testProperty "bind" prop_bind
        ]
    ]
