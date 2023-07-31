module Ch12.Pullback
  ( f,
    g,
    p,
  )
where

import Data.Char

f :: (Char, Int) -> Int
f (_, y) = y + 1

g :: (Char, Int) -> Int
g (x, _) = ord x + 1

p :: Char -> (Char, Int)
p t = (t, ord t)
