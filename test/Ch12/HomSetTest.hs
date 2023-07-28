module Ch12.HomSetTest (tests) where

import Ch12.HomSet
import Data.Char
import Data.Functor.Contravariant
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

sampleHomSet :: Int -> HomSet Int Int
sampleHomSet x =
  let ops =
        [ Op id,
          Op (+ x),
          Op (* x),
          Op (`div` x)
        ]
   in HomSet ops

(-->) :: (Char -> Int) -> (HomSet Int Int -> HomSet Int Char) -> PropertyT IO ()
f --> f' = do
  c <- forAll $ Gen.alpha
  x <- forAll $ Gen.int (Range.constant 2 100)
  let hs = sampleHomSet x

  evaluate (f c) hs === evaluate c (f' hs)

infixr 0 -->

prop_identity :: Property
prop_identity =
  property $ do
    x <- forAll $ Gen.int (Range.constant 2 100)
    y <- forAll $ Gen.int (Range.constant 2 100)
    let hs = sampleHomSet y

    evaluate (id x) hs === evaluate x (contramap id $ hs)

-- vToG :: DeltaV a b -> G a
-- vToG (DeltaV x _) = G x

tests :: TestTree
tests =
  testGroup
    "Product as Natural Transformation"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" $ property (ord --> contramap ord)
    ]
