module Ch04.Writer
  ( Writer,
    (>=>),
  )
where

import Text.Printf

type Writer a = (a, String)

-- | compose f and g, applying g to the result of f
(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
(f >=> g) x =
  let (y, s1) = f x
      (z, s2) = g y
   in (z, printf "%s %s" s1 s2)
