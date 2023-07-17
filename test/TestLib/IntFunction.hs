module TestLib.IntFunction (intFunction) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data NamedFunction a b = NamedFunction String (a -> b)

instance Show (NamedFunction a b) where
  show (NamedFunction name _) = name

genIntFunction :: String -> Gen (NamedFunction Int Int)
genIntFunction name = do
  n <- Gen.int (Range.constant 2 1000)
  let fs = fmap (NamedFunction name) [(+ n), ((-) n), (* n)]

  Gen.element fs

intFunction :: PropertyT IO (Int -> Int)
intFunction = do
  name <- forAll $ Gen.string (Range.constant 5 5) Gen.alpha
  (NamedFunction _ f) <- forAll $ genIntFunction name

  return f
