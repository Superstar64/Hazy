module Stage2.Temporary.Partial.More.Class where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Resolve)
import Stage2.Temporary.Complete.Method (Method)
import Stage2.Tree.Constraint (Constraint)
import Stage2.Tree.TypePattern (TypePattern)

data Class scope = Class
  { parameter :: !(TypePattern Position Resolve scope),
    constraints :: Strict.Vector (Constraint Position Resolve scope),
    methods :: !(Strict.Vector (Method (Local ':+ scope)))
  }
