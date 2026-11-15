module Core.Tree.Constraints where

import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (ConstraintF)
import qualified Core.Tree.Constraint as Constraint
import Core.Tree.Type (TypeF)
import qualified Core.Tree.Type as Type
import qualified Core.Type.Substitute as Type.Substitute
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Scope as Scope
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraints as Semantic

type Constraints = ConstraintsF Void

data ConstraintsF logical scope
  = Constraints !(Strict.Vector (ConstraintF logical scope))
  | Value
      { levity :: !(TypeF logical scope)
      }
  deriving (Show)

instance (Show logical) => Scope.Show (ConstraintsF logical) where
  showsPrec = showsPrec

instance Shift0.Functor (ConstraintsF logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstraintsF logical) where
  map category = \case
    Constraints constraints -> Constraints $ Shift.map category <$> constraints
    Value {levity} ->
      Value
        { levity = Shift.map category levity
        }

instance (logical ~ Void) => Substitute.Functor (ConstraintsF logical) where
  map = Type.Substitute.mapDefault

instance Type.Substitute.Functor ConstraintsF where
  map category = \case
    Constraints constraints -> Constraints $ Type.Substitute.map category <$> constraints
    Value {levity} ->
      Value
        { levity = Type.Substitute.map category levity
        }

simplify :: Semantic.Constraints position Check scope -> Constraints scope
simplify = \case
  Semantic.Constraints constraints -> Constraints $ Constraint.simplify <$> constraints
  Semantic.Value {levity} ->
    Value
      { levity = Type.simplify levity
      }
  Semantic.Lazy -> Value {levity = Type.lazy}
  Semantic.Strict -> Value {levity = Type.strict}
