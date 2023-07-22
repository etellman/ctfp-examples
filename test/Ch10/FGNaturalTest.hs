module Ch10.FGNaturalTest (tests) where

import Ch10.NaturalProperty
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "F/G/H Natural Transformations"
    [ testProperty "F -> G natural transformation" $ prop_natural fToG (eq F),
      testProperty "G -> H natural transformation" $ prop_natural gToH (eq G),
      testProperty "F -> H natural transformation" $ prop_natural fToH (eq F),
      testProperty "F -> H composition" $ prop_natural (gToH . fToG) (eq F),
      testProperty "F Identity" $ prop_natural id (eq F),
      testProperty "G Identity" $ prop_natural id (eq G),
      testProperty "H Identity" $ prop_natural id (eq H)
    ]
