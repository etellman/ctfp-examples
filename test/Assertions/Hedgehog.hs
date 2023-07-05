module Assertions.Hedgehog
  ( (==>),
    (@==),
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | verifies that f(x) == g(x) for a reasonable number of xs
(@==) :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f @== g = do
  x <- forAll $ Gen.int (Range.constant (-1000) 1000)
  f x === g x

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>
