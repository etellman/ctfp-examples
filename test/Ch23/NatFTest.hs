module Ch23.NatFTest (tests) where

import Ch23.Algebra
import Ch23.Fix
import Ch23.NatF
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

(-->) :: (Char -> Int) -> (NatF Char -> NatF Int) -> PropertyT IO ()
(-->) _ f' = do
  n <- forAll $ Gen.int (Range.constant 0 200)
  let nc = intToNat n :: NatF Char
      ni = intToNat n :: NatF Int

  f' nc === ni

infixr 0 -->

prop_naturalToInt :: (a -> Int) -> (Int -> a) -> Property
prop_naturalToInt toInt fromInt = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)

  -- exercise and verify
  (toInt . fromInt) n === n

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

tests :: TestTree
tests =
  testGroup
    "Ch23.NatFTest"
    [ functorTests (-->),
      testGroup
        "Natural to Int"
        [ testProperty "non-zero" $ prop_naturalToInt natToInt intToNat,
          testCase "zero" $ natToInt ZeroF @=? 0
        ],
      testGroup
        "Natural to Int with Fix"
        [ testProperty "non-zero" $ prop_naturalToInt natToIntFix intToNatFix,
          testCase "zero" $ natToIntFix (Fix ZeroF) @=? 0
        ],
      testProperty "Fibonacci" $ prop_fib
    ]
