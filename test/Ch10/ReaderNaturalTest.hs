module Ch10.ReaderNaturalTest (tests) where

import Ch10.NaturalProperty
import Control.Monad.Reader
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

eq ::
  (Eq (m Int), Show (m Int)) =>
  (Reader () Int -> (m Int)) ->
  (Reader () Int -> (m Int)) ->
  PropertyT IO ()
eq f g = do
  n <- forAll $ Gen.int (Range.constant 0 100)
  let constReader = reader $ const n

  f constReader === g constReader

toNothing :: Reader () Int -> Maybe Int
toNothing _ = Nothing

toJust :: Reader () Int -> Maybe Int
toJust f = Just $ runReader f ()

toF :: Reader () Int -> F Int
toF f = F $ runReader f ()

tests :: TestTree
tests =
  testGroup
    "Reader Natural Transformations"
    [ testProperty "to nothing" $ prop_natural toNothing eq,
      testProperty "to constant" $ prop_natural toJust eq,
      testProperty "to F" $ prop_natural toF eq
    ]
