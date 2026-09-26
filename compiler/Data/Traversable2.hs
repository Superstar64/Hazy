module Data.Traversable2 (Traversable2 (..), traverse2', sequence2', fmap2Default) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor2 (Functor2)
import Data.NaturalTransformation (NaturalTransformation (..))

class (Functor2 t) => Traversable2 t where
  traverse2 :: (Applicative f) => NaturalTransformation a (Compose f b) -> t a -> f (t b)
  sequence2 :: (Applicative f) => t (Compose f a) -> f (t a)
  sequence2 = traverse2 $ Morph id

traverse2' :: (Traversable2 t, Applicative f) => NaturalTransformation a f -> t a -> f (t Identity)
traverse2' (Morph t) = traverse2 (Morph $ Compose . fmap Identity . t)

sequence2' :: (Traversable2 t, Applicative f) => t f -> f (t Identity)
sequence2' = traverse2 (Morph $ Compose . fmap Identity)

fmap2Default :: (Traversable2 t) => NaturalTransformation a b -> t a -> t b
fmap2Default (Morph f) x = runIdentity $ traverse2 (Morph $ Compose . Identity . f) x
