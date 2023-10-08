module Ch24.CoherenceTest (tests) where

import Control.Comonad
import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions

prop_return :: Monad m => (m Int -> Int) -> Property
prop_return alg = property $ do
  alg . return @== id

prop_join :: Monad m => (m Int -> Int) -> Property
prop_join alg = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  let mmx = (return . return) x

  -- exercise and verify
  (alg . join) mmx === (alg . fmap alg) mmx

coherence_properties :: Monad m => String -> (m Int -> Int) -> TestTree
coherence_properties name alg =
  testGroup
    name
    [ testProperty "return" $ prop_return alg,
      testProperty "join" $ prop_join alg
    ]

tests :: TestTree
tests =
  testGroup
    "Ch24.CoherenceTest"
    [ coherence_properties "F" (extract :: F Int -> Int),
      testGroup
        "list"
        [ coherence_properties "head" head,
          coherence_properties "sum" (sum :: [Int] -> Int),
          coherence_properties "foldr" (foldr (*) 1 :: [Int] -> Int)
        ]
    ]
