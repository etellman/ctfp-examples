module Ch04.WriterTest (tests) where

import TestLib.Assertions
import Ch04.Writer
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

add :: String -> Int -> Int -> Writer Int
add s x y = (x + y, s)

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant (-1000) 1000)
    let f = add "f" n
        g x = (even x, "g")

    -- exercise and verify
    f >=> g @== \x -> (even (x + n), "f g")

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant (-1000) 1000)
    let f = add "f" n
        writerId x = (x, "id")

    -- verify
    f >=> writerId @== add "f id" n
    writerId >=> f @== add "id f" n

tests :: TestTree
tests =
  testGroup
    "Writer"
    [ testProperty "compose" prop_compose,
      testProperty "identity" prop_identity
    ]
