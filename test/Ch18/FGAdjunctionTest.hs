module Ch18.FGAdjunctionTest (tests) where

import Data.Functor.Adjunction
import Hedgehog as H
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

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

tests :: TestTree
tests =
  testGroup
    "Ch18.FGAdjunctionTest"
    [ testProperty "F . G" $ property $ (F . G) `eqToFG` unit,
      testProperty "F . counit . G" $ property $ (F . counit . G) `eqF` id
    ]
