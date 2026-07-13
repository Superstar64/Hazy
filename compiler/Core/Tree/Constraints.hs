module Core.Tree.Constraints where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (ConstraintF)
import qualified Core.Tree.Constraint as Constraint
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Vacuous)
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraints as Semantic

type Constraints = ConstraintsF Vacuous

data ConstraintsF logical scope
  = Constraints !(Strict.Vector (ConstraintF logical scope))
  | None
  deriving (Show)

instance (Shift.Functor logical) => Shift (ConstraintsF logical) where
  shift = shiftDefault

instance (Shift.Functor logical) => Shift.Functor (ConstraintsF logical) where
  map category = \case
    Constraints constraints -> Constraints $ Shift.map category <$> constraints
    None -> None

instance (logical ~ Vacuous) => Shift2.Functor (ConstraintsF logical) where
  map = Substitute.mapDefault

instance (logical ~ Vacuous) => Substitute.Functor (ConstraintsF logical) where
  map category = \case
    Constraints constraints -> Constraints $ Substitute.map category <$> constraints
    None -> None

data ConstraintCount
  = ConstraintCount !Int
  | Null
  deriving (Show)

constraintCount :: Constraints scope -> ConstraintCount
constraintCount = \case
  Constraints constraints -> ConstraintCount (length constraints)
  None -> Null

simplify :: Semantic.Constraints position Check scope -> Constraints scope
simplify = \case
  Semantic.Constraints constraints -> Constraints $ Constraint.simplify <$> constraints
  Semantic.None -> None
