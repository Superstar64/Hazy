module Semantic.Unify.Solve where

import Control.Monad (ap, liftM)
import Control.Monad.ST (ST)

newtype Solve s a = Solve (ST s a)

instance Functor (Solve s) where
  fmap = liftM

instance Applicative (Solve s) where
  pure a = Solve (pure a)
  (<*>) = ap

instance Monad (Solve s) where
  Solve m >>= f = Solve (m >>= (\(Solve a) -> a) . f)
