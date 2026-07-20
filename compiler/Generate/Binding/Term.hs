module Generate.Binding.Term where

import Data.Kind (Type)
import Generate.Variable (Variable)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Binding :: Environment -> Type
data Binding scope = Binding
  { name :: !Variable,
    strict :: !Bool
  }

binding :: Variable -> Binding scope
binding name = Binding {name, strict = False}

instance Shift0.Functor Binding where
  map = Shift.mapDefault

instance Shift.Functor Binding where
  map _ Binding {name, strict} = Binding {name, strict}
