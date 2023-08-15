module TestLib.Assertions
  ( (==>),
    (@==),
    eqChar,
    eqFloat,
    eqInts,
    eqPair,
    eq,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | compare two functions with arbitrary domains, as long as there is a way to convert an integer
-- to a value in the domain
eq ::
  (Eq b, Show b) =>
  (Int -> a) ->
  (a -> b) ->
  (a -> b) ->
  PropertyT IO ()
eq toDomain f g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  (f . toDomain) x === (g . toDomain) x

-- | verifies that f(x) == g(x) for a reasonable number of xs
(@==) :: (Show a, Eq a) => (Int -> a) -> (Int -> a) -> PropertyT IO ()
f @== g = do
  x <- forAll $ Gen.int (Range.constant (-20) 20)
  f x === g x

infixr 0 @==

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqChar :: (Show a, Eq a) => (Char -> a) -> (Char -> a) -> PropertyT IO ()
f `eqChar` g = do
  c <- forAll $ Gen.alpha
  f c === g c

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqInts :: (Show a, Eq a) => ([Int] -> a) -> ([Int] -> a) -> PropertyT IO ()
f `eqInts` g = do
  xs <- forAll $ Gen.list (Range.constant 0 10) (Gen.int $ Range.constant 0 100)

  cover 2 "empty" $ null xs
  cover 70 "non-empty" $ (not . null) xs

  f xs === g xs

-- | verifies that f(x) == g(x) for a reasonable number of xs
eqFloat :: (Show a, Eq a) => (Float -> a) -> (Float -> a) -> PropertyT IO ()
f `eqFloat` g = do
  x <- forAll $ Gen.float $ Range.constant (-1000) 1000

  f x === g x

-- | verifies that f(x) == g(x) for a reasonable number of pairs
eqPair :: (Show a, Eq a) => ((Int, Int) -> a) -> ((Int, Int) -> a) -> PropertyT IO ()
f `eqPair` g = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  y <- forAll $ Gen.int (Range.constant (-100) 100)

  f (x, y) === g (x, y)

(==>) :: MonadTest m => Bool -> Bool -> m ()
(==>) a b = assert $ not a || b

infixr 0 ==>
