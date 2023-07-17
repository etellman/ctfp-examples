module Ch10.NaturalTest (tests) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Functors
import Test.Tasty
import Test.Tasty.Hedgehog

eq ::
  (F Int -> G Int) ->
  (F Int -> G Int) ->
  PropertyT IO ()
eq f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  f (F x) === g (F x)

data NamedFunction a b = NamedFunction String (a -> b)

instance Show (NamedFunction a b) where
  show (NamedFunction name _) = name

intFunction :: String -> Gen (NamedFunction Int Int)
intFunction name = do
  let fs = [(+), (-), (*)]

  n <- Gen.int (Range.constant 2 100)
  op <- Gen.int (Range.constant 0 ((length fs) - 1))

  return $ NamedFunction name ((fs !! op) n)

prop_natural :: Property
prop_natural =
  property $ do
    -- set up
    (NamedFunction _ f) <- forAll $ intFunction "f"

    -- exercise and verify
    (fmap f . alpha) `eq` (alpha . fmap f)

tests :: TestTree
tests = testProperty "natural transformation" prop_natural
