module Stage3.Simple.Constraint where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Type as Type (unlocal)
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import qualified Stage3.Simple.Type as Type (instanciate', lift)
import {-# SOURCE #-} qualified Stage3.Unify as Unify
import Stage4.Tree.Constraint (Constraint (..))
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
