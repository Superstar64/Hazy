module Control.Monad
  ( Functor (..),
    Monad (..),
    MonadFail (..),
    MonadPlus (..),
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    (=<<),
    (>=>),
    (<=<),
    forever,
    void,
    join,
    msum,
    mfilter,
    filterM,
    mapAndUnzipM,
    zipWithM,
    zipWithM_,
    foldM,
    foldM_,
    replicateM,
    replicateM_,
    guard,
    when,
    unless,
    liftM,
    liftM2,
    liftM3,
    liftM4,
    liftM5,
    ap,
    (<$!>),
  )
where

import Control.Applicative (Alternative, Applicative)
import Data.Bool (Bool)
import Data.Foldable (Foldable, msum)
import Data.Functor
  ( Functor (..),
  )
import Data.Int (Int)
import Data.String (String)
import Data.Traversable (Traversable, mapM, sequence)
import Hazy (Monad (..), MonadFail (..), placeholder)
import Prelude (error)

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ = placeholder

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = placeholder

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = placeholder

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = placeholder

infixr 1 =<<

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = placeholder

infixr 1 >=>

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) = placeholder

infixr 1 <=<

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = placeholder

forever :: (Applicative f) => f a -> f b
forever = placeholder

void :: (Functor f) => f a -> f ()
void = placeholder

join :: (Monad m) => m (m a) -> m a
join = placeholder

mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter = placeholder

filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM = placeholder

mapAndUnzipM :: (Applicative m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM = placeholder

zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM = placeholder

zipWithM_ :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ = placeholder

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM = placeholder

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ = placeholder

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM = placeholder

replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ = placeholder

guard :: (Alternative f) => Bool -> f ()
guard = placeholder

when :: (Applicative f) => Bool -> f () -> f ()
when = placeholder

unless :: (Applicative f) => Bool -> f () -> f ()
unless = placeholder

liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM = placeholder

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 = placeholder

liftM3 :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 = placeholder

liftM4 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 = placeholder

liftM5 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 = placeholder

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = placeholder

infixl 4 <$!>

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
(<$!>) = placeholder
