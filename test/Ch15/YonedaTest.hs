module Ch15.YonedaTest (tests) where

import Ch10.NaturalProperty
import Ch15.Yoneda
import Control.Monad.Reader
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.Assertions
import TestLib.IntFunction

constReader :: Int -> Reader a Int
constReader n = reader $ const n

-- | TODO
eqAlpha ::
  (Int -> m Int) ->
  (m Int -> Reader Char Int) ->
  (m Int -> Reader Char Int) ->
  PropertyT IO ()
eqAlpha newM f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  c <- forAll $ Gen.alpha

  runReader (f (newM x)) c === runReader (g (newM x)) c

prop_yonedaNatural ::
  (Eq (m Int), Show (m Int), Functor m) =>
  (Int -> m Int) ->
  Property
prop_yonedaNatural newM =
  property $ do
    f <- intFunction
    c <- forAll $ Gen.alpha

    let alpha = toAlpha newM c
        eqMorphisms = eq constReader

    -- exercise and verify
    (fmap f . alpha) `eqMorphisms` (alpha . fmap f)

eqReaderToF ::
  ((Reader Char Char -> F Char) -> (Reader Char Char -> F Char)) ->
  ((Reader Char Char -> F Char) -> (Reader Char Char -> F Char)) ->
  PropertyT IO ()
f `eqReaderToF` g = do
  x <- forAll $ Gen.alpha
  let toF r = F (runReader r x)

  (f toF) (reader toUpper) === (g toF) (reader toUpper)

prop_listLength :: Property
prop_listLength =
  property $ do
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    xs <-
      forAll $
        Gen.list
          (Range.constant 0 20)
          (Gen.int $ Range.constant (-100) 100)

    let f = listLength xs

    -- exercise and verify
    runReader f x === length xs

prop_listIndex :: Property
prop_listIndex =
  property $ do
    n <- forAll $ Gen.int (Range.constant 1 100)
    i <- forAll $ Gen.int (Range.constant 0 (n - 1))
    xs <- forAll $ Gen.list (Range.constant n n) (Gen.int $ Range.constant (-100) 100)

    let f = listIndex xs

    -- exercise and verify
    runReader f i === xs !! i

tests :: TestTree
tests =
  testGroup
    "Ch15.YonedaTest"
    [ testGroup
        "F"
        [ testProperty "to Reader" $ do
            prop_natural (toBeta (\(F x) -> x)) (eqAlpha F),
          testProperty "from Reader" $ prop_yonedaNatural F
        ],
      testGroup
        "list"
        [ testProperty "from singleton" $ do
            prop_natural (toBeta fromSingletonList) (eqAlpha toSingletonList),
          testProperty "to singleton" $ prop_yonedaNatural toSingletonList,
          testProperty "length" $ prop_listLength,
          testProperty "index" $ prop_listIndex
        ],
      testGroup
        "phi and psi identity"
        [ testProperty "phi . psi" $ property $ (eq F) (phi . psi) id,
          testProperty "psi . phi" $ property $ (psi . phi) `eqReaderToF` id
        ]
    ]
