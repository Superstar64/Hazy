module Stage2.Label.Binding.Type where

import Data.Kind (Type)
import qualified Data.Vector.Strict as Strict
import Stage1.Variable (QualifiedConstructor, QualifiedConstructorIdentifier)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift (shift))

type TypeBinding :: Environment -> Type
data TypeBinding scope = TypeBinding
  { name :: !QualifiedConstructorIdentifier,
    constructorNames :: !(Strict.Vector QualifiedConstructor)
  }

instance Shift TypeBinding where
  shift TypeBinding {name, constructorNames} = TypeBinding {name, constructorNames}
