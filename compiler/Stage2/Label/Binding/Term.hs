module Stage2.Label.Binding.Term where

import Data.Kind (Type)
import Stage1.Variable (QualifiedVariable)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shift)

type TermBinding :: Environment -> Type
data TermBinding scope
  = TermBinding
      { name :: !QualifiedVariable
      }
  | SharedTermBinding

instance Shift TermBinding where
  shift TermBinding {name} = TermBinding {name}
  shift SharedTermBinding = SharedTermBinding
