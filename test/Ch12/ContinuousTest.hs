module Ch12.ContinuousTest (tests) where

import Ch12.Continuous
import Data.Char
import Data.Functor.Contravariant
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

(-->) :: (Char -> Int) -> ToString Char -> PropertyT IO ()
f --> f' = do
  c <- forAll $ Gen.alpha

  (show . f) c === (getShow f') c

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ ord --> contramap ord (ToString show)

prop_identity :: Property
prop_identity =
  property $ id . show @== getShow (contramap id (ToString show))

prop_either_left :: Property
prop_either_left =
  property $ do
    n <- forAll $ Gen.int (Range.constant 0 100)
    f <- intFunction
    let eitherF = eitherString (show . f) undefined :: ToString (Either Int Char)

    (getShow eitherF) (Left n) === show (f n)

prop_either_right :: Property
prop_either_right =
  property $ do
    c <- forAll $ Gen.alpha
    f <- intFunction
    let eitherF = eitherString undefined (show . f . ord) :: ToString (Either Int Char)

    (getShow eitherF) (Right c) === show ((f . ord) c)

tests :: TestTree
tests =
  testGroup
    "Contravariant Functor"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" prop_morphism,
      testProperty "either left" prop_either_left,
      testProperty "either right" prop_either_right
    ]
