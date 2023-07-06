module Ch04.OptionalTest (tests) where

import Assertions.Hedgehog
import Ch04.Optional
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Data.Ratio

prop_composeNonEmpty :: Property
prop_composeNonEmpty =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = \x -> Exactly (x + m)
        g = \x -> Exactly (n * x)

    -- exercise and verify
    f >=> g @== \x -> Exactly (n * (x + m))

prop_safeReciprocalZero :: Property
prop_safeReciprocalZero =
  property $ do
    -- set up
    n <- forAll $ Gen.integral (Range.constant 1 100)

    -- exercise and verify
    safeReciprocal (0 % n) === Empty

prop_safeReciprocalNonZero :: Property
prop_safeReciprocalNonZero =
  property $ do
    -- set up
    m <- forAll $ Gen.integral (Range.constant 2 100)
    n <- forAll $ Gen.integral (Range.constant 2 100)

    -- exercise and verify
    safeReciprocal (m % n) === Exactly (n % m)

multiplyRational :: Integer -> Rational -> Optional Rational
multiplyRational k x = Exactly ((k % 1) * x)

prop_bindReciprocal :: Property
prop_bindReciprocal =
  property $ do
    -- set up
    m <- forAll $ Gen.integral (Range.constant (-4) 4)
    n <- forAll $ Gen.integral (Range.constant 2 100)
    k <- forAll $ Gen.integral (Range.constant 2 100)

    H.cover 5 "0/Empty" $ m == 0

    -- exercise and verify
    (safeReciprocal >=> multiplyRational k) (m % n) === safeReciprocal (m % (n * k))
    (multiplyRational k >=> safeReciprocal) (m % n) === safeReciprocal ((m * k) % n)

tests :: TestTree
tests =
  testGroup
    "Optional"
    [ testProperty "compose non-empty" prop_composeNonEmpty,
      testProperty "reciprocal 0 numerator" prop_safeReciprocalZero,
      testProperty "reciprocal non-0 numerator" prop_safeReciprocalNonZero,
      testProperty "bind reciprocal" prop_bindReciprocal
    ]
