module Stage4.Tree.Class where

import qualified Data.Vector.Strict as Strict
import Stage2.Scope (Environment ((:+)), Local)
import Stage2.Shift (Shift (shift), shiftDefault)
import qualified Stage2.Shift as Shift
import Stage4.Tree.Constraint (Constraint)
import Stage4.Tree.Scheme (Scheme)
import Stage4.Tree.Type (Type)
import qualified Stage4.Tree.Type as Type

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }
  deriving (Show)

instance Shift Class where
  shift = shiftDefault

instance Shift.Functor Class where
  map category Class {parameter, constraints, methods} =
    Class
      { parameter = Shift.map category parameter,
        constraints = Shift.map category <$> constraints,
        methods = Shift.map (Shift.Over category) <$> methods
      }

kind :: Class scope -> Type scope
kind Class {parameter} = Type.Function parameter Type.Constraint
