module Ch13.HomomorphismTest (tests) where

import Ch13.Homomorphism
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_list_to_P :: Property
prop_list_to_P =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 1 100) (Gen.int $ Range.constant (-100) 100)
    listToP xs === (P $ product xs)

prop_list_to_S :: Property
prop_list_to_S =
  property $ do
    xs <- forAll $ Gen.list (Range.constant 1 100) (Gen.int $ Range.constant (-100) 100)
    listToS xs === (S $ sum xs)

tests :: TestTree
tests =
  testGroup
    "Homomorphism"
    [ testGroup
        "list to product"
        [ testProperty "non-empty list" prop_list_to_P,
          testCase "empty list" $ listToP [] @?= P 1
        ],
      testGroup
        "list to sum"
        [ testProperty "non-empty list" prop_list_to_S,
          testCase "empty list" $ listToP [] @?= P 1
        ]
    ]
