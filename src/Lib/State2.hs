module Lib.State2
  ( State2,
    runState2,
    state2,
    join,
  )
where

import Control.Applicative

newtype State2 s a = State2 (s -> (s, a))

state2 :: (s -> (s, a)) -> State2 s a
state2 f = State2 f

runState2 :: State2 s a -> s -> (s, a)
runState2 (State2 f) s = f s

instance Functor (State2 s) where
  fmap :: (a -> b) -> State2 s a -> State2 s b
  fmap f (State2 g) = state2 $ \s ->
    let (s', x) = g s
     in (s', f x)

instance Applicative (State2 s) where
  pure x = State2 (\s -> (s, x))
  liftA2 ::
    (f -> g -> h) ->
    State2 s f ->
    State2 s g ->
    State2 s h
  liftA2 k (State2 f) (State2 g) = state2 $ \s ->
    let (s', x) = f s
        (s'', y) = g s'
     in (s'', k x y)

instance Monad (State2 e) where
  (>>=) :: State2 s a -> (a -> State2 s b) -> State2 s b
  stateA >>= aToStateB = state2 $ \s ->
    let (sx, x) = runState2 stateA s
     in runState2 (aToStateB x) sx

join :: State2 s (State2 s a) -> State2 s a
join ss = ss >>= id
