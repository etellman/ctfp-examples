module Ch12.ProductTest (tests) where

import Data.Char
import Data.Functor.Contravariant
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- | another name for pair
data LimD a b = LimD a b deriving (Eq, Show)

-- | morphisms from b to a
data HomSet a b = HomSet [Op a b]

-- | converts (b -> a) to (c -> a) -- changes the source object
instance Contravariant (HomSet a) where
  contramap f (HomSet x) = HomSet $ fmap (contramap f) x

evaluate :: b -> HomSet a b -> [a]
evaluate x (HomSet ops) =
  let runOp op = getOp op $ x
   in fmap runOp ops

(-->) :: (Char -> Int) -> (HomSet Float Int -> HomSet Float Char) -> PropertyT IO ()
f --> f' = do
  c <- forAll $ Gen.alpha
  x <- forAll $ Gen.float (Range.constant 2 100)
  y <- forAll $ Gen.float (Range.constant 2 100)

  let ops =
        [ Op fromIntegral,
          Op $ (+ x) . fromIntegral,
          Op $ (* y) . fromIntegral
        ]
      homSet = HomSet ops

  evaluate (f c) homSet === evaluate c (f' homSet)

infixr 0 -->

-- vToG :: DeltaV a b -> G a
-- vToG (DeltaV x _) = G x

tests :: TestTree
tests =
  testGroup
    "Product as Natural Transformation"
    [ testProperty "morphism" $ property (ord --> contramap ord)
    ]
