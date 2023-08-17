module Ch10.NaturalProperty
  ( prop_natural,
    prop_contraNatural,
  )
where

import Data.Functor.Contravariant
import Hedgehog
import TestLib.IntFunction

-- | verifies a natural transformation between any two functors
prop_natural ::
  (Functor m, Functor n) =>
  (m Int -> n Int) ->
  ((m Int -> n Int) -> (m Int -> n Int) -> PropertyT IO ()) ->
  Property
prop_natural alpha eqMorphisms =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    (fmap f . alpha) `eqMorphisms` (alpha . fmap f)

-- | verifies a natural transformation between contravariant functors
prop_contraNatural ::
  ((Op a Int) -> (Op b Int)) ->
  (((Op a Int) -> (Op b Int)) -> ((Op a Int) -> (Op b Int)) -> PropertyT IO ()) ->
  Property
prop_contraNatural alpha eqMorphisms =
  property $ do
    -- set up
    f <- intFunction

    -- exercise and verify
    (contramap f . alpha) `eqMorphisms` (alpha . contramap f)
