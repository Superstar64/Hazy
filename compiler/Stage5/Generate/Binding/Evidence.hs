module Stage5.Generate.Binding.Evidence where

import Data.Kind (Type)
import Data.Text (Text)
import Stage2.Scope (Environment)
import Stage2.Shift (Shift (..), shiftDefault)
import qualified Stage2.Shift as Shift

type Binding :: Environment -> Type
newtype Binding scope = Binding Text

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map _ (Binding text) = Binding text
