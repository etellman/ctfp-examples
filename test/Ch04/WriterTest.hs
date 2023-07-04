module Ch04.WriterTest (tests) where

import Ch04.Writer
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    i <- forAll $ Gen.int (Range.constant 0 1000)
    let f x = (x + 1, "f") :: (Int, String)
        g x = (even x, "g")

    -- exercise
    let fg = f >=> g

    -- verify
    fg i === (even (i + 1), "g . f")

prop_identity :: Property
prop_identity =
  property $ do
    -- set up
    i <- forAll $ Gen.int (Range.constant 0 1000)
    let f x = (x + 1, "f") :: (Int, String)
        writerId x = (x, "id")

    -- verify
    (f >=> writerId) i === (i + 1, "id . f")
    (writerId >=> f) i === (i + 1, "f . id")

tests :: TestTree
tests =
  testGroup
    "Writer"
    [ testProperty "compose" prop_compose,
      testProperty "identity" prop_identity
    ]
