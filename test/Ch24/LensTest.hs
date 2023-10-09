module Ch24.LensTest (tests) where

import Ch24.Lens
import Control.Comonad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Store
import Test.Tasty
import Test.Tasty.Hedgehog

data Pair = Pair Int Int deriving (Eq, Show)

coalgX :: Pair -> Store Int Pair
coalgX = coalg

instance Lens Pair Int where
  set (Pair _ y) x' = Pair x' y
  get (Pair x _) = x

prop_extract :: Property
prop_extract = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)
  let a = Pair x y

  (extract . coalgX) a === a

tests :: TestTree
tests =
  testGroup
    "Ch24.LensTest"
    [ testProperty "extract" $ prop_extract
    ]
