module Ch05.Product
  ( factorize,
    coFactorize,
  )
where

factorize :: (c -> a) -> (c -> b) -> (c -> (a, b))
factorize f g = \x -> (f x, g x)

coFactorize :: (a -> c) -> (b -> c) -> (Either a b -> c)
coFactorize f g = \x ->
  case (x) of
    (Left x') -> f x'
    (Right x') -> g x'
