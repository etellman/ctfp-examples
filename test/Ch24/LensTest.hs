module Ch24.LensTest (tests) where

import Control.Comonad
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_homomorphism :: Monad m => (m Int -> Int) -> Property
prop_homomorphism alg = property $ do
  alg . return @== id

tests :: TestTree
tests =
  testGroup
    "Ch24.LensTest"
    [
      testProperty "F" $ prop_homomorphism (extract :: F Int -> Int)
    ]
