module Stage5.Generate.Binding.Term where

import Data.Kind (Type)
import Data.Text (Text)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage5.Generate.Global (Global)

type Binding :: Environment -> Type
data Binding scope
  = Local !Text
  | Global !Global

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map _ (Local name) = Local name
  map _ (Global global) = Global global
