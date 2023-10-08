module Ch24.CoherenceTest (tests) where

import Control.Comonad
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_return :: Monad m => (m Int -> Int) -> Property
prop_return alg = property $ do
  alg . return @== id

prop_join :: Monad m => (m Int -> Int) -> Property
prop_join alg = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  let mmx = (return . return) x

  -- exercise and verify
  (alg . join) mmx === (alg . fmap alg) mmx

tests :: TestTree
tests =
  testGroup
    "Ch24.CoherenceTest"
    [ testGroup
        "F"
        [ testProperty "return" $ prop_return (extract :: F Int -> Int),
          testProperty "join" $ prop_join (extract :: F Int -> Int)
        ],
      testGroup
        "list"
        [ testGroup
            "head"
            [ testProperty "return" $ prop_return head,
              testProperty "join" $ prop_join head
            ],
          testGroup
            "sum"
            [ testProperty "return" $ prop_return (sum :: [Int] -> Int),
              testProperty "join" $ prop_join (sum :: [Int] -> Int)
            ],
          testGroup
            "foldr"
            [ testProperty "return" $ prop_return (foldr (*) 1 :: [Int] -> Int),
              testProperty "join" $ prop_join (foldr (*) 1 :: [Int] -> Int)
            ]
        ]
    ]
