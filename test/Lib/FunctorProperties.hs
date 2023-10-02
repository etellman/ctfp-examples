module Lib.FunctorProperties
  ( functorTests,
    namedFunctorTests,
  )
where

import Data.Char
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

prop_morphism ::
  Functor m =>
  ((Char -> Int) -> (m Char -> m Int) -> PropertyT IO ()) ->
  Property
prop_morphism mapsTo = property $ do
  -- set up
  f <- intFunction
  let cf = f . ord

  -- exercise and verify
  cf `mapsTo` fmap cf

prop_compose ::
  Functor m =>
  ((Char -> Int) -> (m Char -> m Int) -> PropertyT IO ()) ->
  Property
prop_compose mapsTo =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction
    let cg = g . ord

    -- exercise and verify
    (f . cg) `mapsTo` (fmap f . fmap cg)

functorTests ::
  Functor m =>
  ((Char -> Int) -> (m Char -> m Int) -> PropertyT IO ()) ->
  TestTree
functorTests mapsTo = namedFunctorTests "Functor" mapsTo

namedFunctorTests ::
  Functor m =>
  String ->
  ((Char -> Int) -> (m Char -> m Int) -> PropertyT IO ()) ->
  TestTree
namedFunctorTests name mapsTo =
  testGroup
    name
    [ testProperty "morphism" $ prop_morphism mapsTo,
      testProperty "compose" $ prop_compose mapsTo
    ]
