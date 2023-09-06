module Lib.Maybe2Test (tests) where

import Control.Applicative
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Lib.Maybe2
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Maybe2 Int -> Maybe2 Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  (Just2 $ f x) === (f' $ Just2 x)

infixr 0 -->

mapToNothing :: (Int -> Int) -> (Maybe2 Int -> Maybe2 Int) -> PropertyT IO ()
mapToNothing _ f' = f' Nothing2 === Nothing2

prop_pure :: (Int -> Maybe2 Int) -> Property
prop_pure f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    f x === Just2 x

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    y <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let actual = liftA2 (+) (Just2 x) (Just2 y)

    -- exercise and verify
    actual === Just2 (x + y)

prop_liftA2Nothing :: (Int -> Maybe2 Int) -> Property
prop_liftA2Nothing f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let actual = f x

    -- exercise and verify
    actual === Nothing2

prop_bind :: Property
prop_bind =
  property $ do
    -- set up
    f <- intFunction
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let actual = Just2 x >>= (\y -> Just2 $ f y)

    -- exercise and verify
    actual === (Just2 $ f x)

prop_bindNothing :: Property
prop_bindNothing =
  property $ do
    -- set up
    f <- intFunction

    -- exercise
    let actual = Nothing2 >>= (\x -> Just2 $ f x)

    -- exercise and verify
    actual === Nothing2

prop_join :: Property
prop_join =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    (join $ Just2 (Just2 x)) === Just2 x

prop_fish :: Property
prop_fish =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant (-1000) 1000)

    f <- intFunction
    let f' = \x -> Just $ f x

    g <- intFunction
    let g' = \x -> Just $ g x

    -- exercise and verify
    (f' >=> g' $ n) === Just (g . f $ n)

tests :: TestTree
tests =
  testGroup
    "Lib.Maybe2Test"
    [ testGroup
        "Functor"
        [ namedFunctorTests "Just" (-->),
          namedFunctorTests "Nothing" mapToNothing
        ],
      testGroup
        "Applicative"
        [ testProperty "pure" $ prop_pure pure,
          testProperty "liftA2" prop_liftA2,
          testProperty "liftA2 Nothing2 _" $
            prop_liftA2Nothing $
              \x -> liftA2 (+) Nothing2 (Just2 x),
          testProperty "liftA2 _ Nothing2" $
            prop_liftA2Nothing $
              \x -> liftA2 (+) (Just2 x) Nothing2
        ],
      testGroup
        "Monad"
        [ testProperty "return" $ prop_pure return,
          testProperty "bind" prop_bind,
          testProperty "bind Nothing2" prop_bindNothing,
          testProperty "join" prop_join,
          testCase "join (Just2 Nothing2)" $
            (join $ Just2 Nothing2 :: Maybe2 Int) @?= Nothing2,
          testCase "join Nothing2" $
            (join Nothing2 :: Maybe2 Int) @?= Nothing2
        ],
      testProperty ">=>" prop_fish
    ]
