module Ch22.StoreTest (tests) where

import Ch22.Store
import Control.Comonad
import Data.Char
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Char -> Int) -> (Store Char Char -> Store Char Int) -> PropertyT IO ()
(-->) f f' = do
  c <- forAll $ Gen.alpha

  (f . toUpper) c === extract (f' (Store toUpper c))

infixr 0 -->

prop_extend :: Property
prop_extend = property $ do
  -- set up
  s <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction
  g <- intFunction

  -- exercise
  let extended = extend (g . extract) (Store f s)

  -- exercise and verify
  extract extended === (g . f) s

prop_extract :: Property
prop_extract = property $ do
  -- set up
  s <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction

  -- exercise and verify
  extract (Store f s) === f s

prop_duplicate :: Property
prop_duplicate = property $ do
  -- set up
  s <- forAll $ Gen.int (Range.constant (-100) 100)
  f <- intFunction

  -- exercise and verify
  (extract . extract $ duplicate (Store f s)) === f s

tests :: TestTree
tests =
  testGroup
    "Ch22.StoreTest"
    [ functorTests (-->),
      testGroup
        "Comonad"
        [ testProperty "extend" prop_extend,
          testProperty "extract" prop_extract,
          testProperty "duplicate" prop_duplicate
        ]
    ]
