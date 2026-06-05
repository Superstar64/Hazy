module Generate.Binding.Term where

import Data.Kind (Type)
import Generate.Variable (Variable)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift

type Binding :: Environment -> Type
data Binding scope = Binding
  { name :: !Variable,
    strict :: !Bool
  }

binding :: Variable -> Binding scope
binding name = Binding {name, strict = False}

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map _ Binding {name, strict} = Binding {name, strict}
