module Ch05.ProductTest (tests) where

import Assertions.Hedgehog
import Ch05.Product
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_product :: Property
prop_product =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = (+ m)
        g = (* n)

    -- exercise
    let p = factorize f g

    -- verify
    (fst . p) @== f
    (snd . p) @== g

tests :: TestTree
tests =
  testGroup
    "Product"
    [ testProperty "product" prop_product
    ]
