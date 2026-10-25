module Semantic.Tree.Constraints where

import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Tree.Constraint (Constraint)
import qualified Semantic.Tree.Constraint as Constraint
import Semantic.Tree.Type (Type)
import qualified Semantic.Tree.Type as Type

data Constraints position stage scope
  = Constraints !(Strict.Vector (Constraint position stage scope))
  | Value {levity :: !(Type position stage scope)}
  | Lazy
  | Strict
  deriving (Show, Eq)

instance Shift0.Functor (Constraints position stage) where
  map = Shift.mapDefault

instance Shift.Functor (Constraints position stage) where
  map category = \case
    Constraints constraints -> Constraints $ Shift.map category <$> constraints
    Value {levity} -> Value {levity = Shift.map category levity}
    Lazy -> Lazy
    Strict -> Strict

instance FreeTypeVariables (Constraints position) where
  freeTypeVariables target = \case
    Constraints constraints -> foldMap (freeTypeVariables target) constraints
    Value {levity} -> freeTypeVariables target levity
    Lazy -> []
    Strict -> []

anonymize :: Constraints position stage scope -> Constraints () stage scope
anonymize = \case
  Constraints constraints -> Constraints $ Constraint.anonymize <$> constraints
  Value {levity} -> Value {levity = Type.anonymize levity}
  Lazy -> Lazy
  Strict -> Strict
