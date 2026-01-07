module Stage2.Label.Binding.Local where

import Data.Kind (Type)
import Stage1.Variable (VariableIdentifier)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift (..))

type LocalBinding :: Environment -> Type
newtype LocalBinding scope = LocalBinding
  { name :: VariableIdentifier
  }

instance Shift LocalBinding where
  shift LocalBinding {name} = LocalBinding {name}
