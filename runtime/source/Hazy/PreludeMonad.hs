module Hazy.PreludeMonad where

import Hazy.Prelude

infixl 4 <$>

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f a = do
  a' <- a
  return (f a')

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')
