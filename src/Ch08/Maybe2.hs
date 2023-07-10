module Ch08.Maybe2 (Maybe2, nothing, just, lift, toMaybe, fromMaybe) where

import Ch08.BiComp
import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity

type Maybe2 a = BiComp Either (Const ()) Identity a a

nothing :: Maybe2 a
nothing = BiComp (Left (Const ()))

just :: a -> Maybe2 a
just x = BiComp (Right (Identity x))

lift :: (a -> a) -> Maybe2 a -> Maybe2 a
lift f x@(BiComp (Left (Const ()))) = first f x
lift f x@(BiComp (Right (Identity _))) = second f x

toMaybe :: Maybe2 a -> Maybe a
toMaybe (BiComp (Left (Const ()))) = Nothing
toMaybe (BiComp (Right (Identity x))) = Just x

fromMaybe :: Maybe a -> Maybe2 a
fromMaybe Nothing = BiComp (Left (Const ()))
fromMaybe (Just x) = BiComp (Right (Identity x))
