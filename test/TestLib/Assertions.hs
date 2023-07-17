module TestLib.Assertions
  ( (==>),
    (@==),
    eqCharF,
    eqIntsF,
    eqPairF,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | verifies that f(x) == g(x) for a reasonable number of xs
(@==) :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f @== g = do
  x <- forAll $ Gen.int (Range.constant (-20) 20)
  f x === g x

infixr 0 @==

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqCharF :: (Show a, Eq a) => (Char -> a) -> (Char -> a) -> PropertyT IO ()
f `eqCharF` g = do
  c <- forAll $ Gen.alpha
  f c === g c

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqIntsF :: (Show a, Eq a) => ([Int] -> a) -> ([Int] -> a) -> PropertyT IO ()
f `eqIntsF` g = do
  xs <- forAll $ Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 0 100)

  cover 2 "empty" $ null xs
  cover 70 "non-empty" $ (not . null) xs

  f xs === g xs

-- | verifies that f(x) == g(x) for a reasonable number of pairs
eqPairF :: (Show a, Eq a) => ((Int, Int) -> a) -> ((Int, Int) -> a) -> PropertyT IO ()
f `eqPairF` g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  f (x, y) === g (x, y)

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>
