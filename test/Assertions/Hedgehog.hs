module Assertions.Hedgehog
  ( (==>),
    (@==),
    eqCharF,
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

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqCharF :: (Show a, Eq a) => (Char -> a) -> (Char -> a) -> PropertyT IO ()
f `eqCharF` g = do
  c <- forAll $ Gen.alpha
  f c === g c

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>
