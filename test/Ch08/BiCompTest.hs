module Ch08.BiCompTest (tests) where

import Ch08.BiComp
import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

type Maybe2 a b =
  BiComp
    Either -- bf
    (Const ()) -- fu
    Identity -- gu
    a -- a
    b -- b

nothing :: Maybe2 Int Int
nothing = BiComp (Left (Const ()))

just :: Int -> Maybe2 Int Int
just x = BiComp (Right (Identity x))

verify :: (Int -> Int) -> PropertyT IO ()
verify f = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)

  (first f) nothing === nothing
  (second f) (just x) === just (f x)

prop_morphism :: Property
prop_morphism =
  property $ do
    -- set up
    n <- forAll $ Gen.int (Range.constant 2 100)
    let f = (+ n)

    -- exercise and verify
    verify f

tests :: TestTree
tests =
  testGroup
    "Maybe2"
    [ testProperty "identity" $ property (verify id),
      testProperty "morphism" prop_morphism
    ]
