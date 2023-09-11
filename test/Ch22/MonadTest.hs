module Ch22.MonadTest (tests) where

import Control.Monad
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.F
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

eqF ::
  (Int -> F Int) ->
  (Int -> F Int) ->
  PropertyT IO ()
eqF f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f x === g x

prop_associativity :: Property
prop_associativity = property $ do
  -- set up
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  let f = F x
  let ff = F f
  let fff = F ff

  -- exercise and verify
  (join . join) fff === f

prop_fish :: Property
prop_fish = property $ do
  -- set up
  f <- fmap F <$> intFunction
  g <- fmap F <$> intFunction

  -- exercise and verify
  (f >=> g) `eqF` (join . fmap g . f)

prop_identity :: Property
prop_identity = property $ do
  undefined
  -- -- set up
  -- x <- forAll $ Gen.int (Range.constant (-100) 100)
  -- let fx = F x

  -- -- exercise and verify
  -- let g' = fmap g :: F Int -> F (F Int)
  --     fgx = g' $ f x :: F (F Int)

  -- (f >=> g) x === join fgx

tests :: TestTree
tests =
  testGroup
    "Ch22.MonadTest"
    [ testProperty "associativity" $ prop_associativity,
      testProperty "fish" $ prop_fish,
      testProperty "identity" $ prop_identity
    ]
