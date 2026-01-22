module Stage3.Tree.TypePattern where

import Stage1.Variable (VariableIdentifier)
import qualified Stage4.Tree.Type as Simple

data TypePattern scope = TypePattern
  { typex :: !(Simple.Type scope),
    name :: !VariableIdentifier
  }
  deriving (Show)

_type :: TypePattern scope -> Simple.Type scope
_type = typex
