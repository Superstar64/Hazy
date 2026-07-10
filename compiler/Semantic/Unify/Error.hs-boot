module Semantic.Unify.Error where

import Control.Monad.ST (ST)
import Core.Tree.Type (TypeF)
import Data.STRef (STRef)
import Semantic.Check.Context (Context)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..))
import {-# SOURCE #-} Semantic.Unify.Type (Box, Logical)
import Syntax.Position (Position)

abort :: Position -> Error s -> ST s a

data Error s where
  Unify :: Context s scope -> TypeF (Logical s) scope -> TypeF (Logical s) scope -> Error s
  Occurs :: Context s scope -> STRef s (Box s scope) -> TypeF (Logical s) scope -> Error s
  Mismask :: Context s scope -> TypeF (Logical s) scope -> Error s
  Constrain :: Context s scope -> Type2.Index scope -> TypeF (Logical s) scope -> [TypeF (Logical s) scope] -> Error s
  Unshift :: Context s (scope ':+ scopes) -> TypeF (Logical s) (scope ':+ scopes) -> Error s
