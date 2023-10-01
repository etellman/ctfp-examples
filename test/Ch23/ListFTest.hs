module Ch23.ListFTest (tests) where

import Ch23.Catamorphism
import Ch23.Fix
import Ch23.ListF
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
    "Ch23.ListFTest"
    [
    -- functorTests (-->),
      testProperty "to list" prop_toList,
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
