module Ch12.DeltaTest (tests) where

import Ch12.Delta
import Data.Char
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog

(-->) :: (a -> b) -> (Delta a -> Delta b) -> PropertyT IO ()
_ --> f' = do
  f' Delta === Delta

infixr 0 -->

tests :: TestTree
tests =
  testGroup
    "Product as Natural Transformation"
    [ testProperty "identity" $ property (id --> fmap id),
      testProperty "morphism" $ property (ord --> fmap ord),
      testProperty "composition" $ property (ord . chr --> fmap ord . fmap chr)
    ]
