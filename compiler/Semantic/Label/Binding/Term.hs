module Semantic.Label.Binding.Term where

import Data.Kind (Type)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift, shift)
import Syntax.Variable (QualifiedVariable)

type TermBinding :: Environment -> Type
data TermBinding scope
  = TermBinding
      { name :: !QualifiedVariable
      }
  | SharedTermBinding

instance Shift TermBinding where
  shift TermBinding {name} = TermBinding {name}
  shift SharedTermBinding = SharedTermBinding
