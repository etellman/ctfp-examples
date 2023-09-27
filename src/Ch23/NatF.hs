module Ch23.NatF
  ( NatF (..),
    natToInt,
    intToNat,
    natToIntFix,
    intToNatFix,
  )
where

import Ch23.Fix

data NatF a = ZeroF | SuccF (NatF a)

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
