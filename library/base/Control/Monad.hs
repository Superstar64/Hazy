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
import Prelude (error)

class (Applicative m) => Monad m where
  infixl 1 >>, >>=
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

class (Monad m) => MonadFail m where
  fail :: String -> m a

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ = error "todo"

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = error "todo"

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = error "todo"

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = error "todo"

infixr 1 =<<

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = error "todo"

infixr 1 >=>

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) = error "todo"

infixr 1 <=<

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = error "todo"

forever :: (Applicative f) => f a -> f b
forever = error "todo"

void :: (Functor f) => f a -> f ()
void = error "todo"

join :: (Monad m) => m (m a) -> m a
join = error "todo"

mfilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mfilter = error "todo"

filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM = error "todo"

mapAndUnzipM :: (Applicative m) => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM = error "todo"

zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM = error "todo"

zipWithM_ :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ = error "todo"

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM = error "todo"

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ = error "todo"

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM = error "todo"

replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ = error "todo"

guard :: (Alternative f) => Bool -> f ()
guard = error "todo"

when :: (Applicative f) => Bool -> f () -> f ()
when = error "todo"

unless :: (Applicative f) => Bool -> f () -> f ()
unless = error "todo"

liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM = error "todo"

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 = error "todo"

liftM3 :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 = error "todo"

liftM4 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 = error "todo"

liftM5 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 = error "todo"

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = error "todo"

infixl 4 <$!>

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
(<$!>) = error "todo"
