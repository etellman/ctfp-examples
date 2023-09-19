module Ch22.State3
  (
  )
where

import Data.Distributive
import Data.Functor.Adjunction
import Data.Functor.Rep

newtype Prod s a = Prod (a, s)

newtype Reader s a = Reader (s -> a)

runReader :: Reader s a -> s -> a
runReader (Reader f) s = f s

instance Functor (Prod s) where
  fmap f = \(Prod (a, s)) -> Prod (f a, s)

instance Functor (Reader s) where
  fmap f = \(Reader g) -> Reader $ f . g

instance Distributive (Reader s) where
  distribute :: Functor m => m (Reader s a) -> Reader s (m a)
  distribute x = collect Reader (fmap runReader x)

-- distribute a = Reader $ \s -> (flip runReader s) a
-- collect f x = Reader $ \e -> collect (\a -> runReader (f a) e) x

-- distribute x = F (fmap fromF x)
-- distribute x = Reader (\s -> (fmap runReader x) s)

-- collect :: Functor m => (a -> Reader s b) -> m a -> Reader s (m b)
-- collect h = undefined

-- collect h = distribute . fmap h

instance Representable (Reader s) where
  type Rep (Reader s) = ()

  tabulate :: (() -> a) -> Reader s a
  tabulate = undefined

  -- tabulate h = F $ h ()

  index :: Reader s a -> (() -> a)
  index = undefined

-- index (F x) = \() -> x

instance Adjunction (Prod s) (Reader s) where
  -- unit :: a -> F (G a)
  unit a = Reader (\s -> Prod (a, s))

-- counit :: G (F a) -> a
-- counit (G (F x)) = x

-- leftAdjunct :: (G a -> b) -> (a -> F b)
-- leftAdjunct f x = F $ f (G x)

-- rightAdjunct :: (a -> F b) -> (G a -> b)
-- rightAdjunct f (G x) = fromF (f x)
