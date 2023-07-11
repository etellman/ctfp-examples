module Ch08.WriterTest (tests) where

import Control.Monad.Writer
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

logNumber :: Int -> String -> Writer String Int
logNumber x s = writer (x, s)

(-->) :: (Int -> Int) -> (Writer String Int -> Writer String Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  s <- forAll $ Gen.string (Range.constant 0 100) Gen.alpha

  f' (logNumber x s) === logNumber (f x) s

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ n)

    -- exercise and verify
    f --> fmap f

prop_composition :: Property
prop_composition =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant 2 100)
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ m)
        g = (* n)

    -- exercise and verify
    f . g --> fmap f . fmap g

tests :: TestTree
tests =
  testGroup
    "Writer"
    [ testProperty "identity" $ property $ id --> fmap id,
      testProperty "morphism" prop_morphism,
      testProperty "composition" prop_composition
    ]
