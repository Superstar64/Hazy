module Generate.Binding.Term where

import Data.Kind (Type)
import Generate.Variable (Variable)
import Semantic.Scope (Environment)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0

type Binding :: Environment -> Type
newtype Binding scope = Binding Variable

instance Shift0.Functor Binding where
  map = Shift.mapDefault

instance Shift.Functor Binding where
  map _ (Binding name) = Binding name
