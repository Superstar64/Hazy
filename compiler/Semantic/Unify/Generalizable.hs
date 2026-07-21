module Semantic.Unify.Generalizable where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import Data.STRef (STRef, readSTRef)
import Semantic.Check.Mask (Mask)
import qualified Semantic.Check.Mask as Mask
import Semantic.Scope (Environment (..))
import Semantic.Unify.Type (Box (..), Logical (..))
import Semantic.Unify.Zonk (Zonk)

-- todo, this is O(n^2) due to STRefs not having an order
-- especially not a heterogeneous order

data Collected s scopes where
  Collect :: STRef s (Box s scopes) -> Collected s scopes
  Reach :: Collected s scopes -> Collected s (scope ':+ scopes)

instance Eq (Collected s scopes) where
  Collect left == Collect right = left == right
  Reach left == Reach right = left == right
  _ == _ = False

-- |
-- See rational of Zonker in `Zonk` module.
data Collector s s' where
  Collector :: Mask -> Collector s s

class (Zonk typef) => Generalizable typef where
  collect :: Collector s s' -> typef (Logical s) scopes -> ST s [Collected s' scopes]

instance Generalizable TypeF where
  collect (Collector mask) = collect
    where
      collect :: TypeF (Logical s) scope -> ST s [Collected s scope]
      collect = \case
        Type.Logical (Box reference) ->
          readSTRef reference >>= \case
            Solved typex -> collect typex
            Unsolved {erasure}
              | Mask.valid mask erasure -> pure [Collect reference]
              | otherwise -> pure []
        Type.Logical (Shift logical) -> fmap Reach <$> collect (Type.Logical logical)
        Type.Variable {} -> pure []
        Type.Constructor {} -> pure []
        Type.Call function argument -> do
          function <- collect function
          argument <- collect argument
          pure (function ++ argument)
        Type.Function argument result -> do
          argument <- collect argument
          result <- collect result
          pure $ argument ++ result
        Type.Type universe -> do
          collect universe
        Type.Constraint -> pure []
        Type.Small -> pure []
        Type.Large -> pure []
        Type.Universe -> pure []
        Type.Levity -> pure []
