module Lib.Writer2Test (tests) where

import Control.Applicative
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Lib.Writer2
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Writer2 String Int -> Writer2 String Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  s <- forAll $ Gen.string (Range.constant 0 100) Gen.alpha

  writer2 (f x, s) === f' (writer2 (x, s))

infixr 0 -->

prop_pure :: (Int -> Writer2 String Int) -> Property
prop_pure f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise and verify
    f x === writer2 (x, "")

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    y <- forAll $ Gen.int (Range.constant (-100) 100)
    wx <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha
    wy <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha

    -- exercise
    let actual = runWriter2 $ liftA2 (+) (writer2 (x, wx)) (writer2 (y, wy))

    -- exercise and verify
    actual === (x + y, wx ++ wy)

prop_bind :: Property
prop_bind =
  property $ do
    -- set up
    fx <- intFunction
    let f = \x -> let y = fx x in writer2 (y, show y)
    xi <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let actual = runWriter2 $ writer2 (xi, show xi) >>= f

    -- exercise and verify
    actual === (fx xi, (show xi) ++ (show $ fx xi))

prop_join :: Property
prop_join =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    wx <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha
    wwx <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha

    let xx = writer2 (writer2 (x, wx), wwx)

    -- exercise and verify
    join xx === writer2 (x, wwx ++ wx)

intToWriter :: (Int -> Int) -> w -> (Int -> Writer2 w Int)
intToWriter f w = \x -> writer2 (f x, w)

prop_fish :: Property
prop_fish =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant (-1000) 1000)

    f <- intFunction
    fw <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha
    let f' = intToWriter f fw

    g <- intFunction
    gw <- forAll $ Gen.string (Range.constant 1 100) Gen.alpha
    let g' = intToWriter g gw

    -- exercise and verify
    (f' >=> g' $ n) === writer2 (g . f $ n, fw ++ gw)

tests :: TestTree
tests =
  testGroup
    "Lib.Writer2Test"
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
      testProperty ">=>" prop_fish
    ]
