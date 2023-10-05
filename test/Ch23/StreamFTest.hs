module Ch23.StreamFTest (tests) where

import Ch23.Coalgebra
import Ch23.StreamF
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

(-->) :: (Char -> Int) -> (StreamF String Char -> StreamF String Int) -> PropertyT IO ()
(-->) f f' = do
  e <- forAll $ Gen.string (Range.constant 0 100) Gen.alpha
  a <- forAll $ Gen.alpha

  f' (StreamF e a) === StreamF e (f a)

infixr 0 -->

eqFront :: (Eq a, Show a) => [a] -> [a] -> PropertyT IO ()
eqFront xs ys = take 20 xs === take 20 ys

prop_squaresAlg :: Property
prop_squaresAlg = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)
  let xs = [n ..]

  -- exercise and verify
  (toListC . (ana squares) $ xs) `eqFront` (fmap (^ (2 :: Int)) xs)

prop_timesN :: Property
prop_timesN = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 0 100)
  m <- forAll $ Gen.int (Range.constant 0 100)
  let xs = [m ..]

  -- exercise and verify
  (toListC . (ana (timesN n)) $ xs) `eqFront` (fmap (* n) xs)

case_primes :: TestTree
case_primes = testCase "primes" $ do
  -- set up
  let expected = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] :: [Int]

  -- exercise
  let actual = (ana primes) [2 ..]

  -- verify
  (take 10 $ toListC actual) @=? expected

tests :: TestTree
tests =
  testGroup
    "Ch23.StreamFTest"
    [ functorTests (-->),
      testProperty "squares" prop_squaresAlg,
      testProperty "times n" prop_timesN,
      case_primes
    ]
