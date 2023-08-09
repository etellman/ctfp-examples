module Ch15.Yoneda
  ( toReader,
    fromReader,
  )
where

import Control.Monad.Reader

fromReader :: (b -> m b) -> a -> (Reader a b -> m b)
fromReader newM x = \f -> newM (runReader f x)

toReader :: (m b -> b) -> (m b -> Reader a b)
toReader fromM = \x -> reader (\_ -> (fromM x))
