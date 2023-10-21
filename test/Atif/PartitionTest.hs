module Atif.PartitionTest (tests) where

import Atif.Partition
import Data.List (sort)
import Data.List.Unique (allUnique)
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Math.Combinatorics.Binomial (choose)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

prop_combinationsZero :: Property
prop_combinationsZero = property $ do
  -- set up
  xs <-
    forAll $
      Gen.list
        (Range.constant 0 20)
        (Gen.int $ Range.constant (-100) 100)

  -- exercise and verify
  combinations (==) 0 xs === [[]]

prop_combinationsNoChoices :: Property
prop_combinationsNoChoices = property $ do
  -- set up
  n <- forAll $ Gen.int (Range.constant 1 100)

  -- exercise and verify
  H.assert $ null $ combinations (==) n ([] :: [Int])

prop_takeOne :: Property
prop_takeOne = property $ do
  -- set up
  xs <-
    forAll $
      Gen.list
        (Range.constant 1 20)
        (Gen.int $ Range.constant (-100) 100)

  -- exercise and verify
  combinations (==) 1 xs === fmap (: []) xs

prop_combinations :: Property
prop_combinations = property $ do
  -- set up
  howMany <- forAll $ Gen.int (Range.constant 10 30)
  n <- forAll $ Gen.int (Range.constant 1 4)
  let xs = [1 .. howMany]

  -- exercise
  let actual = combinations (==) n xs

  -- verify
  length actual === howMany `choose` n
  H.assert $ all allUnique actual
  H.assert $ allUnique actual

prop_partition :: Property
prop_partition = property $ do
  -- set up
  numGroups <- forAll $ Gen.int (Range.constant 2 6)
  groupSize <- forAll $ Gen.int (Range.constant 2 4)
  let xs = [1 .. numGroups * groupSize]

  -- exercise
  let actual = takeN 100 $ take 10000 $ partition numGroups xs

  -- verify
  H.assert $ (not . null) actual
  H.assert $ all allUnique actual
  H.assert $ allUnique actual
  H.assert $ all (\g -> length g == numGroups) actual
  H.assert $ all (\g -> (sort . concat) g == xs) actual

tests :: TestTree
tests =
  testGroup
    "Atif.PartitionTest"
    [ testGroup
        "combinations"
        [ testProperty "select none" $ prop_combinationsZero,
          testProperty "no choices" $ prop_combinationsNoChoices,
          testProperty "take one" $ prop_takeOne,
          testProperty "take n" $ prop_combinations,
          testCase "take 3" $ do
            let xs = [1 .. 5] :: [Int]
                expected =
                  [ [1, 2, 3],
                    [1, 2, 4],
                    [1, 2, 5],
                    [1, 3, 4],
                    [1, 3, 5],
                    [1, 4, 5],
                    [2, 3, 4],
                    [2, 3, 5],
                    [2, 4, 5],
                    [3, 4, 5]
                  ]
            combinations (==) 3 xs @?= expected
        ],
      testGroup
        "partition"
        [ testCase "partition 4" $ do
            let xs = [1 .. 4] :: [Int]
                expected = [[[1, 2], [3, 4]], [[1, 3], [2, 4]], [[1, 4], [2, 3]]]
            (partition 2 xs) @?= expected,
          testProperty "partition" prop_partition
        ]
    ]
