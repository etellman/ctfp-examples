module Ch08.BifunctorTest
  ( prop_identity,
    prop_morphism,
    prop_compose,
    bifunctorTests,
  )
where

import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

prop_morphism ::
  Bifunctor a =>
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  Property
prop_morphism (-->) =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let f = (+ m)
        g = (* n)

    -- exercise and verify
    (f, g) --> bimap f g

prop_compose ::
  Bifunctor a =>
  ((Int -> Int, Int -> Int) -> (a Int Int -> a Int Int) -> PropertyT IO ()) ->
  Property
prop_compose (-->) =
  property $ do
    -- set up
    m <- forAll $ Gen.int (Range.constant (-100) 100)
    n <- forAll $ Gen.int (Range.constant (-100) 100)

    let f = (+ m)
        g = (* m)
        h = (* n)
        j = (+ n)

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
