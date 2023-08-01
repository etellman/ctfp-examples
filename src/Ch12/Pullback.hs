module Ch12.Pullback
  ( f,
    g,
    d,
    p,
    q,
    d',
    h,
  )
where

import Data.Char

f :: Char -> Int
f x = ord x + 1

g :: Int -> Int
g = (+ 1)

d :: Char -> (Char, Int)
d t = (t, ord t)

p :: (Char, Int) -> Char
p = fst

q :: (Char, Int) -> Int
q = snd

d' :: () -> (Char, Int)
d' = const ('a', 97)

h :: () -> Char
h = const 'a'
