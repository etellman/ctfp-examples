module Ch09.Eval
  ( evalF,
  )
where

evalF :: (a -> b) -> a -> b
evalF f = f
