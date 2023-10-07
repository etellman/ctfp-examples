module Ch24.AlgebraTest (tests) where

import Control.Comonad
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_return :: Property
prop_return = property $ do
  -- set up
  let alg = extract :: F Int -> Int

  -- exercise and verify
  alg . return @== id

prop_join :: Property
prop_join = property $ do
  -- set up
  ffx <- forAll $ F <$> F <$> Gen.int (Range.constant (-100) 100)
  let alg = extract :: F Int -> Int
      liftedAlg = fmap alg :: F (F Int) -> F Int

  -- exercise and verify
  (alg . join) ffx === (alg . liftedAlg) ffx

tests :: TestTree
tests =
  testGroup
    "Ch24.AlgebraTest"
    [ testProperty "return" $ prop_return,
      testProperty "join" $ prop_join
    ]
