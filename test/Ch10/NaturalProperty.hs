module Ch10.NaturalProperty (prop_natural) where

import Hedgehog as H
import TestLib.IntFunction

-- | verifies a natural transformation between any two functors
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
