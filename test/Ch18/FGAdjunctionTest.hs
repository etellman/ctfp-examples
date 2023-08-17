module Ch18.FGAdjunctionTest (tests) where

import Ch18.FGAdjunction ()
import Data.Functor.Adjunction
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Lib.G
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

eqToFG ::
  (Int -> (F (G Int))) ->
  (Int -> (F (G Int))) ->
  PropertyT IO ()
eqToFG = eq id

eqF ::
  (F Int -> F Int) ->
  (F Int -> F Int) ->
  PropertyT IO ()
eqF = eq F

prop_unit :: Property
prop_unit = property $ do
  unit `eqToFG` (F . G)
  unit `eqToFG` leftAdjunct id

prop_counit :: Property
prop_counit = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  (F . counit . G) `eqF` id
  counit (G . F $ x) === x

prop_leftAdjunct :: Property
prop_leftAdjunct = property $ do
  -- set up
  fBase <- intFunction
  let f = \(G y) -> fBase y

  x <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  leftAdjunct f x === F (fBase x)
  leftAdjunct f x === (fmap f . unit) x

prop_rightAdjunct :: Property
prop_rightAdjunct = property $ do
  fBase <- intFunction
  let f = F . fBase

  x <- forAll $ Gen.int (Range.constant (-100) 100)

  -- exercise and verify
  rightAdjunct f (G x) === fBase x
  rightAdjunct f (G x) === (counit . fmap f) (G x)

tests :: TestTree
tests =
  testGroup
    "Ch18.FGAdjunctionTest"
    [ testProperty "unit" $ prop_unit,
      testProperty "counit" $ prop_counit,
      testProperty "leftAdjunct" prop_leftAdjunct,
      testProperty "rightAdjunct" prop_rightAdjunct
    ]
