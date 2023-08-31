module Ch21.UnitConverterTest (tests) where

import Ch21.UnitConverter
import Control.Monad.Reader
import Data.Ratio
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Money
import Test.Tasty
import Test.Tasty.Hedgehog

genRational :: (Range Integer) -> (Range Integer) -> Gen Rational
genRational rn rd =
  let n = Gen.integral rn
      d = Gen.integral rd
   in (%) <$> n <*> d

genRational' :: Gen Rational
genRational' = genRational (Range.constant 1 100) (Range.constant 1 100)

prop_computePrice ::
  ( forall src dst.
    Dense src ->
    Integer ->
    Reader (ExchangeRate src dst) (Dense dst)
  ) ->
  Property
prop_computePrice calculator = property $ do
  -- set up
  rate <- forAll $ genRational'
  yenAmount <- forAll $ genRational'
  n <- forAll $ Gen.integral (Range.constant 0 100)

  let jpyToBtc = exchangeRate' rate :: ExchangeRate "JPY" "BTC"
      yen = dense' yenAmount

  -- exercise
  let btc = runReader (calculator yen n) jpyToBtc

  -- verify
  btc === (dense' $ yenAmount * rate * fromIntegral n)


tests :: TestTree
tests =
  testGroup
    "Ch21.UnitConverterTest"
    [ testProperty "price v3" $ prop_computePrice computePrice,
      testProperty "price v4" $ prop_computePrice computePrice'
    ]
