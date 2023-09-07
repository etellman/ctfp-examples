module Lib.Cont2
  ( Cont2,
    runCont2,
    cont2,
    -- join,
  )
where

import Control.Applicative

newtype Cont2 r a = Cont2 ((a -> r) -> r)

cont2 :: ((a -> r) -> r) -> Cont2 r a
cont2 f = Cont2 f

runCont2 :: Cont2 r a -> (a -> r) -> r
runCont2 (Cont2 k) h = k h

instance Functor (Cont2 r) where
  fmap :: (a -> b) -> Cont2 r a -> Cont2 r b
  fmap aToB (Cont2 aToRRunner) = cont2 $ \bToR ->
    let aToR = bToR . aToB
     in aToRRunner aToR

-- instance Applicative (Cont2 s) where
--   pure x = Cont2 (\s -> (s, x))
--   liftA2 ::
--     (f -> g -> h) ->
--     Cont2 s f ->
--     Cont2 s g ->
--     Cont2 s h
--   liftA2 k (Cont2 f) (Cont2 g) = cont2 $ \s ->
--     let (s', x) = f s
--         (s'', y) = g s'
--      in (s'', k x y)

-- instance Monad (Cont2 e) where
--   (>>=) :: Cont2 s a -> (a -> Cont2 s b) -> Cont2 s b
--   contA >>= aToContB = cont2 $ \s ->
--     let (sx, x) = runCont2 contA s
--      in runCont2 (aToContB x) sx

-- join :: Cont2 s (Cont2 s a) -> Cont2 s a
-- join ss = ss >>= id
