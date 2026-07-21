module Semantic.Unify.Generalizable where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import Data.STRef (STRef)
import Semantic.Check.Mask (Mask)
import Semantic.Scope (Environment (..))
import {-# SOURCE #-} Semantic.Unify.Type (Box, Logical)
import {-# SOURCE #-} Semantic.Unify.Zonk (Zonk)

-- todo, this is O(n^2) due to STRefs not having an order
-- especially not a heterogeneous order

data Collected s scopes where
  Collect :: STRef s (Box s scopes) -> Collected s scopes
  Reach :: Collected s scopes -> Collected s (scope ':+ scopes)

-- |
-- See rational of Zonker in `Zonk` module.
data Collector s s' where
  Collector :: Mask -> Collector s s

instance Generalizable TypeF

class (Zonk typef) => Generalizable typef where
  collect :: Collector s s' -> typef (Logical s) scopes -> ST s [Collected s' scopes]
