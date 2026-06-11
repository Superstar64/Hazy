module Data.Foldable
  ( Foldable (..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    sequenceA_,
    asum,
    mapM_,
    forM_,
    sequence_,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Hazy.Prelude (Foldable (..), placeholder)

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM = placeholder

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM = placeholder

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ = placeholder

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ = placeholder

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = placeholder

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = placeholder

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = placeholder

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = placeholder

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = placeholder

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy = placeholder

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy = placeholder

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem = placeholder

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find = placeholder
