module Lib.Cont2Test (tests) where

import Control.Applicative
import Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib.Cont2
import Lib.FunctorProperties
import Test.Tasty
import Test.Tasty.Hedgehog
import TestLib.IntFunction

(-->) :: (Int -> Int) -> (Cont2 Int Int -> Cont2 Int Int) -> PropertyT IO ()
(-->) f f' = do
  x <- forAll $ Gen.int (Range.constant (-100) 100)
  g <- intFunction

  (g . f) x === runCont2 (f' $ continue x) g

infixr 0 -->

continue :: Int -> Cont2 Int Int
continue x = cont2 $ \f -> f x

prop_liftA2 :: Property
prop_liftA2 =
  property $ do
    -- set up
    g <- intFunction
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    y <- forAll $ Gen.int (Range.constant (-100) 100)

    -- exercise
    let h = liftA2 (+) (continue x) (continue y)

    -- verify
    (runCont2 h) g === g (x + y)

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

prop_pure :: (Int -> Cont2 Int Int) -> Property
prop_pure f =
  property $ do
    -- set up
    x <- forAll $ Gen.int (Range.constant (-100) 100)
    g <- intFunction

    -- exercise and verify
    runCont2 (f x) g === g x

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
    [ functorTests (-->),
      testGroup
        "Applicative"
        [ testProperty "pure" $ prop_pure pure,
          testProperty "liftA2" prop_liftA2
        ]
        -- testGroup
        --   "Monad"
        --   [ testProperty "return" $ prop_pure return,
        --     testProperty "bind" prop_bind,
        --     testProperty "join" prop_join
        --   ]
    ]