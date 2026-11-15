module Core.Tree.Constraint where

import qualified Core.Substitute as Substitute
import Core.Tree.Type (Type, TypeF (Variable), (#))
import qualified Core.Tree.Type as Type
import qualified Core.Type.Substitute as Type.Substitute
import qualified Data.Vector.Strict as Strict
import Data.Void (Void)
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import qualified Semantic.Shift as Shift
import qualified Semantic.Shift0 as Shift0
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraint as Solved

type Constraint = ConstraintF Void

data ConstraintF logical scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (TypeF logical (Local ':+ scope)))
  }
  deriving (Show)

instance Shift0.Functor (ConstraintF logical) where
  map = Shift.mapDefault

instance Shift.Functor (ConstraintF logical) where
  map category Constraint {classx, head, arguments} =
    Constraint
      { classx = Shift.map category classx,
        head,
        arguments = Shift.map (Shift.Over category) <$> arguments
      }

instance (logical ~ Void) => Substitute.Functor (ConstraintF logical) where
  map = Type.Substitute.mapDefault

instance Type.Substitute.Functor ConstraintF where
  map category Constraint {classx, head, arguments} =
    Constraint
      { classx = Shift.map (Substitute.general $ Type.Substitute.category $ category) classx,
        head,
        arguments = Type.Substitute.map (Type.Substitute.over category) <$> arguments
      }

argument :: Constraint scope -> Type (Local ':+ scope)
argument Constraint {head, arguments} =
  foldl (#) (Variable (Local.Local head)) arguments

simplify :: Solved.Constraint position Check scope -> Constraint scope
simplify Solved.Constraint {classx, head, arguments} = do
  Constraint
    { classx,
      head,
      arguments = Type.simplify <$> arguments
    }
