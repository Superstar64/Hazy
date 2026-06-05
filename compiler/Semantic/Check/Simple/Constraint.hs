module Semantic.Check.Simple.Constraint where

import Core.Tree.Constraint (Constraint (..))
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Check.Simple.Type as Type (instanciate', lift)
import qualified Semantic.Index.Type as Type (unlocal)
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import {-# SOURCE #-} qualified Semantic.Unify as Unify
import Prelude hiding (head)

lift :: Constraint scope -> Unify.Constraint s scope
lift Constraint {classx, head, arguments} =
  Unify.constraintx
    classx
    head
    (Type.lift <$> arguments)

instanciate :: Strict.Vector (Unify.Type s scope) -> Constraint (Local ':+ scope) -> Unify.Constraint s scope
instanciate fresh Constraint {classx, head, arguments} =
  Unify.constraintx
    (Type2.map Type.unlocal classx)
    head
    (Type.instanciate' fresh <$> arguments)
