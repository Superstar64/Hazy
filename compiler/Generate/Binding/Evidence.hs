module Generate.Binding.Evidence where

import Data.Kind (Type)
import Data.Text (Text)
import Semantic.Scope (Environment)
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift

type Binding :: Environment -> Type
newtype Binding scope = Binding Text

instance Shift Binding where
  shift = shiftDefault

instance Shift.Functor Binding where
  map _ (Binding text) = Binding text
