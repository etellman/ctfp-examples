module Ch10.NaturalTest (prop_natural) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

prop_natural ::
  (Functor m, Functor n) =>
  ((m Int) -> (n Int)) ->
  ((m Int -> n Int) -> (m Int -> n Int) -> PropertyT IO ()) ->
  Property
prop_natural alpha eq =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    (fmap f . alpha) `eq` (alpha . fmap f)
