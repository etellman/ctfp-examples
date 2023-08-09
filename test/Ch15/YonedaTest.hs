module Ch15.YonedaTest (tests) where

import Ch10.NaturalProperty
import Ch15.Yoneda
import Control.Monad.Reader
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

    let alpha = fromReader newM c
        eqMorphisms = eq constReader

    -- exercise and verify
    (fmap f . alpha) `eqMorphisms` (alpha . fmap f)

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
            prop_natural (toReader (\(F x) -> x)) (eqAlpha F),
          testProperty "from Reader" $ prop_yoneda_natural F
        ],
      testGroup
        "list"
        [ testProperty "to Reader" $ do
            prop_natural (toReader fromSingletonList) (eqAlpha toList),
          testProperty "from Reader" $ prop_yoneda_natural toList
        ]
    ]
