module Semantic.Label.Binding.Type where

import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment)
import Semantic.Shift (Shift (shift))
import Syntax.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)

type TypeBinding :: Environment -> Type
data TypeBinding scope = TypeBinding
  { name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor)
  }

instance Shift TypeBinding where
  shift TypeBinding {name, constructorNames} = TypeBinding {name, constructorNames}
