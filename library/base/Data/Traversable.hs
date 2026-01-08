{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE_HAZY StableImports #-}
module Data.Traversable
  ( Traversable (..),
    for,
    forM,
    mapAccumL,
    mapAccumR,
    fmapDefault,
    foldMapDefault,
  )
where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Monoid (Monoid)
import Prelude (Foldable, Functor, error)

class (Functor t, Foldable t) => Traversable t where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequenceA :: (Applicative f) => t (f a) -> f (t a)
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  sequence :: (Monad m) => t (m a) -> m (t a)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = error "todo"

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = error "todo"

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL = error "todo"

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR = error "todo"

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault = error "todo"

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault = error "todo"
