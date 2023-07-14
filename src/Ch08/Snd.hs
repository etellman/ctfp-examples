module Ch08.Snd
  ( Snd (..),
  )
where

import Data.Bifunctor

data Snd a b = Snd b deriving (Eq, Show)

instance Bifunctor Snd where
  bimap _ g (Snd x) = Snd (g x)
