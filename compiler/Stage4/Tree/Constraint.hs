module Stage4.Tree.Constraint where

import qualified Data.Vector.Strict as Strict
import qualified Stage2.Index.Local as Local
import qualified Stage2.Index.Type2 as Type2
import Stage2.Scope (Environment (..), Local)
import Stage2.Shift (Shift, shift, shiftDefault)
import qualified Stage2.Shift as Shift
import qualified Stage3.Tree.Constraint as Solved
import qualified Stage4.Shift as Shift2
import qualified Stage4.Substitute as Substitute
import Stage4.Tree.Type (Type (Call, Variable))

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

simplify :: Solved.Constraint scope -> Constraint scope
simplify Solved.Constraint {classx, head, arguments'} = do
  Constraint
    { classx,
      head,
      arguments = arguments'
    }
