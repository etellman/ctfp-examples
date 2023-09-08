module Lib.Cont2
  ( Cont2,
    runCont2,
    cont2,
    -- join,
  )
where

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

instance Applicative (Cont2 r) where
  pure x = Cont2 (\f -> f x)
  (<*>) :: Cont2 r (a -> b) -> Cont2 r a -> Cont2 r b
  Cont2 aToBToRRunner <*> Cont2 aToRRunner = cont2 $ \bToR ->
    aToBToRRunner $ \aToB ->
      let aToR = bToR . aToB
       in aToRRunner aToR

-- instance Monad (Cont2 e) where
--   (>>=) :: Cont2 s a -> (a -> Cont2 s b) -> Cont2 s b
--   contA >>= aToContB = cont2 $ \s ->
--     let (sx, x) = runCont2 contA s
--      in runCont2 (aToContB x) sx

-- join :: Cont2 s (Cont2 s a) -> Cont2 s a
-- join ss = ss >>= id
