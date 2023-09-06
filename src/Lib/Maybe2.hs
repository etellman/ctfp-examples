module Lib.Maybe2
  ( Maybe2 (..),
  )
where

import Control.Applicative

data Maybe2 a = Just2 a | Nothing2 deriving (Eq, Show)

instance Functor Maybe2 where
  fmap :: (a -> b) -> Maybe2 a -> Maybe2 b
  fmap f (Just2 x) = Just2 $ f x
  fmap _ Nothing2 = Nothing2

instance Applicative Maybe2 where
  pure x = Just2 x
  liftA2 :: (a -> b -> c) -> Maybe2 a -> Maybe2 b -> Maybe2 c
  liftA2 f (Just2 x) (Just2 y) = Just2 (f x y)
  liftA2 _ _ _ = Nothing2

instance Monad Maybe2 where
  (>>=) :: Maybe2 a -> (a -> Maybe2 b) -> Maybe2 b
  Just2 x >>= f = f x
  Nothing2 >>= _ = Nothing2
