module Ch12.Equalizer
  ( f,
    g,
    p,
    p',
    h,
  )
where

f :: (Float, Float) -> Float
f (x, y) = 2 * y + x

g :: (Float, Float) -> Float
g (x, y) = y - x

p :: Float -> (Float, Float)
p t = (t, (-2) * t)

p' :: () -> (Float, Float)
p' = const (0, 0)

h :: () -> Float
h = const 0
