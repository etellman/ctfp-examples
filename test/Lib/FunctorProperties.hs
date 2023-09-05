module Lib.FunctorProperties (functorTests) where

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

prop_morphism ::
  Functor m =>
  ((Int -> Int) -> (m Int -> m Int) -> PropertyT IO ()) ->
  Property
prop_morphism mapsTo = property $ do
  -- set up
  f <- intFunction

  -- exercise and verify
  f `mapsTo` fmap f

prop_identity ::
  Functor m =>
  ((Int -> Int) -> (m Int -> m Int) -> PropertyT IO ()) ->
  Property
prop_identity mapsTo = property $ id `mapsTo` fmap id

prop_compose ::
  Functor m =>
  ((Int -> Int) -> (m Int -> m Int) -> PropertyT IO ()) ->
  Property
prop_compose mapsTo =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction

    -- exercise and verify
    (f . g) `mapsTo` (fmap f . fmap g)

functorTests ::
  Functor m =>
  ((Int -> Int) -> (m Int -> m Int) -> PropertyT IO ()) ->
  TestTree
functorTests mapsTo =
  testGroup
    "Functor"
    [ testProperty "identity" $ prop_identity mapsTo,
      testProperty "morphism" $ prop_morphism mapsTo,
      testProperty "compose" $ prop_compose mapsTo
    ]
