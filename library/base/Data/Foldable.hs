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

import Control.Applicative (Alternative, Applicative)
import Control.Monad (Monad, MonadPlus)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import Data.Ord (Ord, Ordering)
import Prelude (Num, error)

class Foldable t where
  fold :: (Monoid m) => t m -> m
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldMap' :: (Monoid m) => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: (Eq a) => a -> t a -> Bool
  maximum :: (Ord a) => t a -> a
  minimum :: (Ord a) => t a -> a
  sum :: (Num a) => t a -> a
  product :: (Num a) => t a -> a

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM = error "todo"

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM = error "todo"

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ = error "todo"

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ = error "todo"

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = error "todo"

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = error "todo"

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ = error "todo"

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = error "todo"

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = error "todo"

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = error "todo"

concat :: (Foldable t) => t [a] -> [a]
concat = error "todo"

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap = error "todo"

and :: (Foldable t) => t Bool -> Bool
and = error "todo"

or :: (Foldable t) => t Bool -> Bool
or = error "todo"

any :: (Foldable t) => (a -> Bool) -> t a -> Bool
any = error "todo"

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all = error "todo"

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy = error "todo"

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy = error "todo"

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem = error "todo"

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find = error "todo"
