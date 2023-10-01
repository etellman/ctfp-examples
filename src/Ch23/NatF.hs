module Ch23.NatF
  ( NatF (..),
    natToInt,
    intToNat,
    natToIntFix,
    intToNatFix,
    fibF,
  )
where

import Ch23.Fix

data NatF a = ZeroF | SuccF (NatF a) deriving (Eq, Show)

instance Functor NatF where
  fmap :: (a -> b) -> NatF a -> NatF b
  fmap _ ZeroF = ZeroF
  fmap f (SuccF x) = SuccF (fmap f $ x)

natToInt :: NatF a -> Int
natToInt ZeroF = 0
natToInt (SuccF x) = natToInt x + 1

intToNat :: Int -> NatF a
intToNat 0 = ZeroF
intToNat n = SuccF $ intToNat (n - 1)

natToIntFix :: Fix NatF -> Int
natToIntFix (Fix ZeroF) = 0
natToIntFix (Fix (SuccF x)) = natToIntFix (Fix x) + 1

intToNatFix :: Int -> Fix NatF
intToNatFix 0 = Fix ZeroF
intToNatFix n = Fix $ SuccF (intToNat (n - 1))

fibF :: NatF (Integer, Integer) -> (Integer, Integer)
fibF ZeroF = (1, 1)
fibF (SuccF x) =
  let (m, n) = fibF x
   in (n, m + n)
