module Ch23.CatamorphismTest (tests) where

import Ch23.Catamorphism
import Ch23.Fix
import Ch23.NatF
import Control.Comonad
import Data.AEq ((~==))
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

(-->) :: (Int -> Int) -> (ListF Int Int -> ListF Int Int) -> PropertyT IO ()
(-->) f f' = do
  e <- forAll $ Gen.int (Range.constant (-100) 100)
  a <- forAll $ Gen.int (Range.constant (-100) 100)

  (extract $ f' (ConsF e a)) === f a

infixr 0 -->

-- add intermediate steps to clarify what's going on
m :: Fix NatF -> Int
m x =
  let nextLevel = unfix x :: NatF (Fix NatF)
      natural = fmap m nextLevel :: NatF Int
      alg = natToInt
   in alg natural

prop_commute :: Property
prop_commute = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)
  let ffx = fmap Fix $ intToNat n :: NatF (Fix NatF)
      m' = cata natToInt

  -- exercise and verify
  (m . Fix) ffx === (natToInt . fmap m) ffx
  (m' . Fix) ffx === (natToInt . fmap m') ffx

simple_fib :: Int -> Integer
simple_fib n =
  let fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
   in head $ drop n fibs

prop_fib :: Property
prop_fib = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 1 500)
  let fib = fst . cata fibF :: Fix NatF -> Integer
      fn = Fix $ intToNat n

  -- exercise and verify
  fib fn === simple_fib n

prop_lenAlg :: Property
prop_lenAlg = property $ do
  -- set up
  xs <-
    forAll $
      Gen.list
        (Range.constant 0 20)
        (Gen.int $ Range.constant (-100) 100)

  -- exercise and verify
  (cata lenAlg) (toList xs) === length xs

prop_toList :: Property
prop_toList = property $ do
  -- set up
  xs <-
    forAll $
      Gen.list
        (Range.constant 1 20)
        (Gen.int $ Range.constant (-100) 100)

  -- exercise
  let xsF = toList xs

  -- verify
  H.assert $ xsF `eqListF` xs

prop_sumAlg :: Property
prop_sumAlg = property $ do
  -- set up
  xs <-
    forAll $
      Gen.list
        (Range.constant 1 1000)
        (Gen.double $ Range.constant (-100) 100)

  -- exercise and verify
  H.assert $ (cata sumAlg) (toList xs) ~== sum xs

tests :: TestTree
tests =
  testGroup
    "Ch23.CatamorphismTest"
    [ testProperty "commute" $ prop_commute,
      testProperty "Fibonacci" $ prop_fib,
      functorTests (-->),
      testGroup
        "list"
        [ testProperty "to list" prop_toList,
          testGroup
            "length algebra"
            [ testProperty "non-empty" prop_lenAlg,
              testCase "empty" $ lenAlg NilF @=? 0
            ],
          testGroup
            "sum algebra"
            [ testProperty "non-empty" prop_sumAlg,
              testCase "empty" $ sumAlg NilF @=? 0.0
            ]
        ]
    ]
