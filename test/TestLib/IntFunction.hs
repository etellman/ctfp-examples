module TestLib.IntFunction (intFunction) where

import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Printf

data NamedFunction a b = NamedFunction String (a -> b)

operator :: String -> Int -> (Int -> Int -> Int) -> NamedFunction Int Int
operator name n f = NamedFunction (printf "(%s %d)" name n) (f n)

instance Show (NamedFunction a b) where
  show (NamedFunction name _) = name

genIntFunction :: Gen (NamedFunction Int Int)
genIntFunction = do
  n <- Gen.int (Range.constant 2 1000)
  let fs = [ (operator "+" n (+)), (operator "-" n (-)), (operator "*" n (*)) ]

  Gen.element fs

intFunction :: PropertyT IO (Int -> Int)
intFunction = do
  (NamedFunction _ f) <- forAll $ genIntFunction
  return f
