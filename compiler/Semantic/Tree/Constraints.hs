module Semantic.Tree.Constraints where

import qualified Data.Vector.Strict as Strict
import Semantic.FreeVariables (FreeTypeVariables (..))
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Constraint (Constraint)
import qualified Semantic.Tree.Constraint as Constraint

data Constraints position stage scope
  = Constraints !(Strict.Vector (Constraint position stage scope))
  | None
  deriving (Show, Eq)

instance Shift (Constraints position stage) where
  shift = shiftDefault

instance Shift.Functor (Constraints position stage) where
  map category = \case
    Constraints constraints -> Constraints $ Shift.map category <$> constraints
    None -> None

instance FreeTypeVariables (Constraints position) where
  freeTypeVariables target = \case
    Constraints constraints -> foldMap (freeTypeVariables target) constraints
    None -> []

anonymize :: Constraints position stage scope -> Constraints () stage scope
anonymize = \case
  Constraints constraints -> Constraints $ Constraint.anonymize <$> constraints
  None -> None
