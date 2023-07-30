module Ch12.ProductTest (tests) where

import Ch12.Product
import Data.Functor.Contravariant
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Delta"
    [ testCase "D" $ do
        d0 Zero @?= A
        d1 One @?= B,
      testCase "delta" $ do
        delta Zero @?= C
        delta One @?= C,
      testCase "delta -> D natural transformation" $ do
        let alphaA = alpha $ Op p
            alphaB = alpha $ Op q
        (getOp alphaA $ Zero) @?= A
        (getOp alphaB $ One) @?= B
    ]
