module Core.Tree.Constraint where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Type (Type (Call, Variable))
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import qualified Semantic.Index.Local as Local
import qualified Semantic.Index.Type2 as Type2
import Semantic.Scope (Environment (..), Local)
import Semantic.Shift (Shift, shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Stage (Check)
import qualified Semantic.Tree.Constraint as Solved

data Constraint scope = Constraint
  { classx :: !(Type2.Index scope),
    head :: !Int,
    arguments :: !(Strict.Vector (Type (Local ':+ scope)))
  }
  deriving (Show)

instance Shift Constraint where
  shift = shiftDefault

instance Shift.Functor Constraint where
  map = Shift2.mapDefault

instance Shift2.Functor Constraint where
  map = Substitute.mapDefault

instance Substitute.Functor Constraint where
  map category Constraint {classx, head, arguments} =
    Constraint
      { classx = Substitute.map category classx,
        head,
        arguments = Substitute.map (Substitute.Over category) <$> arguments
      }

argument :: Constraint scope -> Type (Local ':+ scope)
argument Constraint {head, arguments} =
  foldl Call (Variable (Local.Local head)) arguments

simplify :: Solved.Constraint position Check scope -> Constraint scope
simplify Solved.Constraint {classx, head, arguments} = do
  Constraint
    { classx,
      head,
      arguments = Type.simplify <$> arguments
    }
