module Lib.Writer2
  ( Writer2,
    runWriter2,
    writer2,
  )
where

import Control.Applicative

newtype Writer2 w a = Writer2 (a, w) deriving (Show, Eq)

writer2 :: (a, w) -> Writer2 w a
writer2 pair = Writer2 pair

runWriter2 :: Writer2 w a -> (a, w)
runWriter2 (Writer2 pair) = pair

instance Functor (Writer2 e) where
  fmap :: (a -> b) -> Writer2 w a -> Writer2 w b
  fmap f (Writer2 (a, w)) = Writer2 (f a, w)

instance (Monoid w) => Applicative (Writer2 w) where
  pure a = Writer2 (a, mempty)
  liftA2 ::
    (a -> b -> c) ->
    Writer2 w a ->
    Writer2 w b ->
    Writer2 w c
  liftA2 f (Writer2 (x, xw)) (Writer2 (y, yw)) =
    writer2 $ (f x y, xw `mappend` yw)

instance (Monoid w) => Monad (Writer2 w) where
  (>>=) :: Writer2 w a -> (a -> Writer2 w b) -> Writer2 w b
  Writer2 (x, xw) >>= aToWriterB =
    let (y, yw) = runWriter2 $ aToWriterB x
     in writer2 (y, xw `mappend` yw)
