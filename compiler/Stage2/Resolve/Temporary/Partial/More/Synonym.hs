module Stage2.Resolve.Temporary.Partial.More.Synonym where

import qualified Data.Vector.Strict as Strict (Vector)
import Stage1.Position (Position)
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Stage (Resolve)
import Stage2.Tree.Type (Type)
import Stage2.Tree.TypePattern (TypePattern)

data Synonym scope = Synonym
  { parameters :: !(Strict.Vector (TypePattern Position Resolve scope)),
    synonym :: Type Position Resolve (Local ':+ scope)
  }
