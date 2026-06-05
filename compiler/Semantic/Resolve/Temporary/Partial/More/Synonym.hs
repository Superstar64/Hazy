module Semantic.Resolve.Temporary.Partial.More.Synonym where

import qualified Data.Vector.Strict as Strict (Vector)
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Stage (Resolve)
import Semantic.Tree.Type (Type)
import Semantic.Tree.TypePattern (TypePattern)
import Syntax.Position (Position)

data Synonym scope = Synonym
  { parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    synonym :: Type Position Resolve (Local ':+ scope)
  }
