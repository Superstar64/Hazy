module Core.Tree.Constraints where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (Constraint)
import qualified Core.Tree.Constraint as Constraint
import qualified Data.Vector.Strict as Strict
import Semantic.Shift (Shift (..), shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraints as Semantic

data Constraints scope
  = Constraints !(Strict.Vector (Constraint scope))
  | None
  deriving (Show)

instance Shift Constraints where
  shift = shiftDefault

instance Shift.Functor Constraints where
  map = Shift2.mapDefault

instance Shift2.Functor Constraints where
  map = Substitute.mapDefault

instance Substitute.Functor Constraints where
  map category = \case
    Constraints constraints -> Constraints $ Substitute.map category <$> constraints
    None -> None

data ConstraintCount
  = ConstraintCount !Int
  | Null

constraintCount :: Constraints scope -> ConstraintCount
constraintCount = \case
  Constraints constraints -> ConstraintCount (length constraints)
  None -> Null

simplify :: Semantic.Constraints position Check scope -> Constraints scope
simplify = \case
  Semantic.Constraints constraints -> Constraints $ Constraint.simplify <$> constraints
  Semantic.None -> None
