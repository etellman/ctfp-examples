module Ch21.UnitConverterTest (tests) where

import Ch21.UnitConverter
import Control.Monad.Reader
import Data.Map
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_computePrice :: (String -> Double -> Reader Rates Double) -> Property
prop_computePrice calculator = property $ do
  -- set up
  toCurrency <- forAll $ Gen.double (Range.constant 0.1 2.0)
  euroPrice <- forAll $ Gen.double (Range.constant 1.0 100.0)
  currency <- forAll $ Gen.string (Range.constant 0 100) Gen.alpha
  let rates = singleton currency toCurrency

  -- exercise
  let currencyPrice = runReader (calculator currency euroPrice) rates

  -- verify
  currencyPrice === euroPrice * toCurrency

prop_roundTrip :: Property
prop_roundTrip = property $ do
  -- set up
  toCurrency <- forAll $ Gen.double (Range.constant 0.1 2.0)
  euroPrice <- forAll $ Gen.double (Range.constant 1.0 100.0)
  currency <- forAll $ Gen.string (Range.constant 0 100) Gen.alpha

  let rates = singleton currency toCurrency
      r1 = computePrice currency euroPrice :: Reader Rates Double
      r2 = reverseConvert currency :: Double -> Reader Rates Double
      r3 = r1 >>= r2 :: Reader Rates Double

  -- -- exercise
  let roundTrip = runReader r3 rates :: Double

  -- verify
  roundTrip === euroPrice



tests :: TestTree
tests =
  testGroup
    "Ch21.UnitConverterTest"
    [ testProperty "price v1" $ prop_computePrice computePrice,
      testProperty "price v2" $ prop_computePrice computePrice',
      testProperty "round trip" $ prop_roundTrip
    ]
