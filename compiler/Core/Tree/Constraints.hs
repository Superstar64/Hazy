module Core.Tree.Constraints where

import qualified Core.Functor as Core
import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (ConstraintF)
import qualified Core.Tree.Constraint as Constraint
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraints as Semantic

type Constraints = ConstraintsF Void

data ConstraintsF logical scope
  = Constraints !(Strict.Vector (ConstraintF logical scope))
  | None
  deriving (Show)

instance Shift0.Functor (ConstraintsF logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstraintsF logical) where
  map category = \case
    Constraints constraints -> Constraints $ Shift.map category <$> constraints
    None -> None

instance (logical ~ Void) => Substitute.Functor (ConstraintsF logical) where
  map = Substitute.mapType

instance Substitute.TypeFunctor ConstraintsF where
  mapType category = \case
    Constraints constraints -> Constraints $ Substitute.mapType category <$> constraints
    None -> None

instance Core.Functor ConstraintsF where
  map f = \case
    Constraints constraints -> Constraints $ Core.map f <$> constraints
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
