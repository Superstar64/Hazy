module Stage5.Generate.Binding.Term where

import Data.Kind (Type)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage5.Generate.Variable (Variable)

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
