module Ch08.BifunctorTest
  ( prop_identity,
    prop_morphism,
    prop_compose,
    bifunctorTests,
  )
where

import Data.Bifunctor
import Hedgehog as H
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

prop_morphism ::
  Bifunctor a =>
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  Property
prop_morphism (-->) =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction

    -- exercise and verify
    (f, g) --> bimap f g

prop_compose ::
  Bifunctor a =>
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  Property
prop_compose (-->) =
  property $ do
    -- set up
    f <- intFunction
    g <- intFunction
    h <- intFunction
    j <- intFunction

    -- exercise and verify
    (f . h, g . j) --> ((bimap f g) . (bimap h j))

prop_identity ::
  Bifunctor a =>
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  Property
prop_identity (-->) =
  property $ (id, id) --> bimap id id

bifunctorTests ::
  Bifunctor a =>
  String ->
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  TestTree
bifunctorTests name (-->) =
  testGroup
    name
    [ testProperty "identity" $ prop_identity (-->),
      testProperty "morphism" $ prop_morphism (-->),
      testProperty "compose" $ prop_compose (-->)
    ]
