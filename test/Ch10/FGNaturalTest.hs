module Ch10.FGNaturalTest (tests) where

import Ch10.NaturalProperty
import Hedgehog
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "F/G/H Natural Transformations"
    [ testProperty "F Identity" $ prop_natural id (eq F),
      testProperty "G Identity" $ prop_natural id (eq G),
      testProperty "H Identity" $ prop_natural id (eq H),
      testProperty "F -> G natural" $ prop_natural fToG (eq F),
      testProperty "G -> H natural" $ prop_natural gToH (eq G),
      testProperty "F -> H natural" $ prop_natural fToH (eq F),
      testGroup
        "Composition"
        [ testProperty "natural" $ prop_natural (gToH . fToG) (eq F),
          testProperty "commute" $ property $ do
            let eqF = eq F
            (gToH . fToG) `eqF` fToH
        ]
    ]
