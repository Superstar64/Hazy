module Semantic.Check.Simple.Constraints where

import Core.Tree.Constraints (Constraints (..))
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Check.Simple.Constraint as Constraint
import Semantic.Scope (Environment (..), Local)
import {-# SOURCE #-} qualified Semantic.Unify as Unify

lift :: Constraints scope -> Unify.Constraints s scope
lift = \case
  Constraints constraints -> Unify.constraints $ Constraint.lift <$> constraints
  None -> Unify.none

instanciate :: Strict.Vector (Unify.Type s scope) -> Constraints (Local ':+ scope) -> Unify.Constraints s scope
instanciate fresh = \case
  Constraints constraints -> Unify.constraints $ Constraint.instanciate fresh <$> constraints
  None -> Unify.none
