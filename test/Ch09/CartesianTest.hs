module Ch09.CartesianTest (tests) where

import Ch09.Cartesian
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- | 1^a :: Int -> ()
prop_powersOfOne :: Property
prop_powersOfOne =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let unit = const ()

    -- exercise and verify
    unit n === ()

-- | a^1 :: () -> Int
prop_firstPower :: Property
prop_firstPower =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = const n

    -- exercise and verify
    f n === n

-- | a^(b + c) = a^b + a^c
-- (Int -> String) -> (Double -> String) -> (Either Int Double -> String)
prop_exponentialsOfSums :: Property
prop_exponentialsOfSums =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    x <- forAll $ Gen.double (Range.constant 2 100)

    let f = show . negate
        g = show . log
        fg = toSum f g

    -- exercise and verify
    fg (Left n) === f n
    fg (Right x) === g x

-- | a^(b + c) = a^b + a^c (other direction)
-- (Either Int Double -> String) -> ((Int -> String), (Double -> String))
prop_exponentialsOfSums2 :: Property
prop_exponentialsOfSums2 =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    x <- forAll $ Gen.double (Range.constant 2 100)

    let f = show . negate
        g = show . log
        (f', g') = fromSum $ toSum f g

    -- exercise and verify
    f' n === f n
    g' x === g x

-- | a^(b)^c = a^(b x c)
-- ((Int, Double) -> String) -> (Int -> Double -> String)
prop_exponentialsOfExponentials :: Property
prop_exponentialsOfExponentials =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    x <- forAll $ Gen.double (Range.constant 2 100)

    let f (m, y) = show $ fromIntegral m * y

    -- exercise and verify
    f (n, x) === (curry f) n x

-- | a^(b)^c = a^(b x c)
-- (Int -> Double -> String) -> ((Int, Double) -> String)
prop_exponentialsOfExponentials2 :: Property
prop_exponentialsOfExponentials2 =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    x <- forAll $ Gen.double (Range.constant 2 100)

    let f m y = show $ fromIntegral m * y

    -- exercise and verify
    f n x === (uncurry f) (n, x)

-- | (a x b)^c = a^c x b^c
-- (Int -> Double) -> (Int -> String) -> (Int -> (Double, String))
prop_exponentialsOfProducts :: Property
prop_exponentialsOfProducts =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = fromIntegral
        g = show
        fg = toProduct f g

    -- exercise and verify
    fg n === (f n, g n)

-- | (a x b)^c = a^c x b^c
-- (Int -> (Double, String)) -> ((Int -> Double), (Int -> String))
prop_exponentialsOfProducts2 :: Property
prop_exponentialsOfProducts2 =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)

    let f = fromIntegral
        g = show
        (f', g') = fromProduct $ toProduct f g

    -- exercise and verify
    f' n === f n
    g' n === g n

tests :: TestTree
tests =
  testGroup
    "Cartesian Closed Categories"
    [ testProperty "powers of 1" prop_powersOfOne,
      testProperty "first power" prop_firstPower,
      testProperty "exponentials of sums" prop_exponentialsOfSums,
      testProperty "exponentials of sums 2" prop_exponentialsOfSums2,
      testProperty "exponentials of exponentials" prop_exponentialsOfExponentials,
      testProperty "exponentials of exponentials 2" prop_exponentialsOfExponentials2,
      testProperty "exponentials of products" prop_exponentialsOfProducts,
      testProperty "exponentials of products 2" prop_exponentialsOfProducts2
    ]
