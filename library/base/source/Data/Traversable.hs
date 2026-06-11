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

import Hazy.Prelude (placeholder)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = placeholder

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = placeholder

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL = placeholder

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR = placeholder

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault = placeholder

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault = placeholder
