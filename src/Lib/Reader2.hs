module Lib.Reader2
  ( Reader2,
    runReader2,
    reader2,
  )
where

import Control.Applicative

newtype Reader2 e a = Reader2 (e -> a)

reader2 :: (e -> a) -> Reader2 e a
reader2 f = Reader2 f

runReader2 :: Reader2 e a -> e -> a
runReader2 (Reader2 f) e = f e

instance Functor (Reader2 e) where
  fmap f (Reader2 g) = Reader2 (f . g)

instance Applicative (Reader2 e) where
  pure e = Reader2 (const e)
  liftA2 ::
    (f -> g -> h) ->
    Reader2 e f ->
    Reader2 e g ->
    Reader2 e h
  liftA2 k (Reader2 x) (Reader2 y) =
    let z e = k (x e) (y e)
     in Reader2 z

instance Monad (Reader2 e) where
  (>>=) :: Reader2 e a -> (a -> Reader2 e b) -> Reader2 e b
  readerA >>= aToReaderB =
    let f e =
          let a = runReader2 readerA e
              readerB = aToReaderB a
           in runReader2 readerB e
     in Reader2 f
