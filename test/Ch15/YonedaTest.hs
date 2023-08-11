module Ch15.YonedaTest (tests) where

import Ch10.NaturalProperty
import Ch15.Yoneda
import Control.Monad.Reader
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog
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

-- | verifies a natural Yoneda transformation
prop_yoneda_natural ::
  (Eq (m Int), Show (m Int), Functor m) =>
  (Int -> m Int) ->
  Property
prop_yoneda_natural newM =
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

fromSingletonList :: [a] -> a
fromSingletonList [x] = x
fromSingletonList _ = undefined

toList :: a -> [a]
toList x = [x]

tests :: TestTree
tests =
  testGroup
    "Ch15.YonedaTest"
    [ testGroup
        "F"
        [ testProperty "to Reader" $ do
            prop_natural (toBeta (\(F x) -> x)) (eqAlpha F),
          testProperty "from Reader" $ prop_yoneda_natural F
        ],
      testGroup
        "list"
        [ testProperty "to Reader" $ do
            prop_natural (toBeta fromSingletonList) (eqAlpha toList),
          testProperty "from Reader" $ prop_yoneda_natural toList
        ],
      testGroup
        "phi and psi identity"
        [ testProperty "phi . psi" $ property $ (eq F) (phi . psi) id,
          testProperty "psi . phi" $ property $ (psi . phi) `eqReaderToF` id
        ]
    ]
