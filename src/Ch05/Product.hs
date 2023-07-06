module Ch05.Product (factorize) where

factorize :: (c -> a) -> (c -> b) -> (c -> (a, b))
factorize f g = \x -> (f x, g x)
