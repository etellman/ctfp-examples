module Lib.Cont2Test (tests) where

import Control.Applicative
import Data.Bifunctor
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.FunctorProperties
import Lib.Cont2
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Cont2 Int Int -> Cont2 Int Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  g <- intFunction

  (g . f) x === runCont2 (f' $ cont2 (\k -> k x)) g

infixr 0 -->

-- prop_liftA2 :: Property
-- prop_liftA2 =
--   property $ do
--     -- set up
--     f <- intFunction
--     fs <- intFunction
--     g <- intFunction
--     gs <- intFunction

--     s <- forAll $ Gen.int (Range.constant (-100) 100)

--     -- exercise
--     let h = liftA2 (+) (cont2 $ \s' -> (fs s', f s')) (cont2 $ \s' -> (gs s', g s'))

--     -- exercise
--     let (hs, hx) = runCont2 h s

--     -- verify
--     hx === (f s) + (g $ fs s)
--     hs === (gs . fs $ s)

-- prop_bind :: Property
-- prop_bind =
--   property $ do
--     -- set up
--     xi <- forAll $ Gen.int (Range.constant (-100) 100)
--     sf <- intFunction
--     let f s = (sf s, xi)

--     xg <- intFunction
--     sg <- intFunction
--     let g x = cont2 $ \s -> bimap sg xg (s, x)

--     si <- forAll $ Gen.int (Range.constant (-100) 100)

--     -- exercise
--     let h = cont2 f >>= g

--     -- exercise and verify
--     runCont2 h si === ((sg . sf) si, xg xi)

-- prop_pure :: (Int -> Cont2 Int Int) -> Property
-- prop_pure f =
--   property $ do
--     -- set up
--     x <- forAll $ Gen.int (Range.constant (-100) 100)
--     s <- forAll $ Gen.int (Range.constant (-100) 100)

--     -- exercise and verify
--     runCont2 (f x) s === (s, x)

-- prop_join :: Property
-- prop_join =
--   property $ do
--     -- set up
--     xi <- forAll $ Gen.int (Range.constant (-100) 100)
--     sf <- intFunction
--     sf' <- intFunction
--     let rr = cont2 (\s -> (sf s, cont2 $ \s' -> (sf' s', xi)))

--     si <- forAll $ Gen.int (Range.constant (-100) 100)

--     -- exercise
--     let r = join rr

--     -- exercise and verify
--     runCont2 r si === ((sf' . sf) si, xi)

tests :: TestTree
tests =
  testGroup
    "Lib.Cont2Test"
    [ functorTests (-->)
      -- testGroup
      --   "Applicative"
      --   [ testProperty "pure" $ prop_pure pure,
      --     testProperty "liftA2" prop_liftA2
      --   ],
      -- testGroup
      --   "Monad"
      --   [ testProperty "return" $ prop_pure return,
      --     testProperty "bind" prop_bind,
      --     testProperty "join" prop_join
      --   ]
    ]
