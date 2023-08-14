module Ch18.FGAdjunctionTest (tests) where

import Ch10.NaturalProperty
import Ch18.FGAdjunction
import Hedgehog as H
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "Ch18.FGAdjunctionTest"
    [ testProperty "F -> G" $ property $ (eq G) (fgUnit . fgCounit) id,
      testProperty "G -> F" $ property $ (eq F) (fgCounit . fgUnit) id
    ]
