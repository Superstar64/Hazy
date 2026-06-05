module Semantic.Resolve.Temporary.Partial.More.Class where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Resolve.Temporary.Complete.Method (Method)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Constraint (Constraint)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data Class scope = Class
  { parameter :: !(TypePattern Position Resolve scope),
    constraints :: Strict.Vector (Constraint Position Resolve scope),
    methods :: !(Strict.Vector (Method (Local ':+ scope)))
  }
