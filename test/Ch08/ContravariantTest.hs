module Ch08.ContravariantTest (tests) where

import Data.Char
import Data.Functor.Contravariant
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

(-->) :: (Eq a, Show a) => (Char -> a) -> Op a Char -> PropertyT IO ()
(-->) f f' = do
  c <- forAll $ Gen.alpha

  f c === (getOp f') c

infixr 0 -->

prop_morphism :: Property
prop_morphism =
  property $ ord --> contramap ord (Op id)

prop_compose :: Property
prop_compose =
  property $ do
    -- set up
    f <- intFunction
    let g = ord

    -- exercise and verify
    f . g --> Op ((getOp $ contramap f (Op id)) . (getOp $ contramap g (Op id)))

prop_identity :: Property
prop_identity =
  property $ id @== getOp (contramap id (Op id))

tests :: TestTree
tests =
  testGroup
    "Contravariant Functor"
    [ testProperty "identity" prop_identity,
      testProperty "morphism" prop_morphism,
      testProperty "compose" prop_compose
    ]
