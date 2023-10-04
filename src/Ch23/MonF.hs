module Ch23.MonF
  ( MonF (..),
  )
where

data MonF a = MEmpty | MAppend a a deriving (Eq, Show)
