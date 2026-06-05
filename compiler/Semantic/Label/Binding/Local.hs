module Semantic.Label.Binding.Local where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift (..))
import Syntax.Variable (VariableIdentifier)

type LocalBinding :: Environment -> Type
newtype LocalBinding scope = LocalBinding
  { name :: VariableIdentifier
  }

instance Shift LocalBinding where
  shift LocalBinding {name} = LocalBinding {name}
