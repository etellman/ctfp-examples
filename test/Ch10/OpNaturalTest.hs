module Ch10.OpNaturalTest (tests) where

import Ch10.NaturalProperty
import Data.Functor.Contravariant
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

eqPred ::
  (Eq a, Show a) =>
  ((Op Bool Int) -> (Op a Int)) ->
  ((Op Bool Int) -> (Op a Int)) ->
  PropertyT IO ()
eqPred f g = do
  n <- forAll $ Gen.int (Range.constant 0 100)

  getOp (f $ Op even) n === getOp (g $ Op even) n

predicateToString :: Op Bool a -> Op String a
predicateToString (Op f) =
  let f' x = if f x then "T" else "F"
   in Op f'

data Color = Red | Blue deriving (Eq, Show)

predicateToColor :: Op Bool a -> Op Color a
predicateToColor (Op f) =
  let f' x = if f x then Red else Blue
   in Op f'

tests :: TestTree
tests =
  testGroup
    "Contravariant Natural Transformations"
    [ testProperty "predicate to string" $ prop_contraNatural predicateToString eqPred,
      testProperty "predicate to color" $ prop_contraNatural predicateToColor eqPred
    ]
