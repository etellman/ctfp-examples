module Ch23.MonF
  ( MonF (..),
    MonF',
  )
where

import Ch23.Fix

data MonF a = MEmpty | MAppend a a deriving (Eq, Show)

type MonF' = Fix MonF

