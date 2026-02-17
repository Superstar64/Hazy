module Stage3.Unify.Error where

import Control.Monad.ST (ST)
import Data.STRef (STRef)
import Stage1.Position (Position)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..))
import Stage3.Check.Context (Context)
import {-# SOURCE #-} Stage3.Unify.Type (Box, Type)

abort :: Position -> Error s -> ST s a

data Error s where
  Unify :: Context s scope -> Type s scope -> Type s scope -> Error s
  Occurs :: Context s scope -> STRef s (Box s scope) -> Type s scope -> Error s
  Constrain :: Context s scope -> Type2.Index scope -> Type s scope -> [Type s scope] -> Error s
  Unshift :: Context s (scope ':+ scopes) -> Type s (scope ':+ scopes) -> Error s
