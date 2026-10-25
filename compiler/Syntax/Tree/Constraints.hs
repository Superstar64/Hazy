module Syntax.Tree.Constraints where

import qualified Data.Vector.Strict as Strict
import Syntax.FreeVariables (FreeTypeVariables (..))
import Syntax.Tree.Constraint (Constraint)
import Syntax.Tree.Type (Type)

data Constraints position
  = Constraints !(Strict.Vector (Constraint position))
  | Value {levity :: !(Type position)}
  | Lazy
  | Strict
  deriving (Show)

instance FreeTypeVariables Constraints where
  freeTypeVariables = \case
    Constraints constraints -> foldMap freeTypeVariables constraints
    Value {levity} -> freeTypeVariables levity
    Lazy -> []
    Strict -> []
