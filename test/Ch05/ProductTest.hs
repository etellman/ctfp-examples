module Ch05.ProductTest (tests) where

import TestLib.Assertions
import TestLib.IntFunction
import Ch05.Product
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_product :: Property
prop_product =
  property $ do
    -- set up
    f <- intFunction
    let g = chr . (+ 20) . (flip mod 40) . abs

    -- exercise
    let p = factorize f g

    -- verify
    (fst . p) @== f
    (snd . p) @== g

prop_coProduct :: Property
prop_coProduct =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ n)
        g = ord

    -- -- exercise
    let cp = coFactorize f g

    -- verify
    (cp . Left) @== f
    (cp . Right) `eqChar` g

tests :: TestTree
tests =
  testGroup
    "Product and Co-Product"
    [ testProperty "product" prop_product,
      testProperty "co-product" prop_coProduct
    ]
