module Core.Tree.Class where

import qualified Core.Shift as Shift2
import qualified Core.Substitute as Substitute
import Core.Tree.Constraint (Constraint)
import Core.Tree.MethodInfo (MethodInfo (..))
import Core.Tree.Scheme (Scheme)
import Core.Tree.Type (Type)
import qualified Core.Tree.Type as Type
import qualified Data.Vector.Strict as Strict
import Semantic.Scope (Environment ((:+)), Local)
import Semantic.Shift (Shift (shift), shiftDefault)
import qualified Semantic.Shift as Shift

data Class scope = Class
  { parameter :: !(Type scope),
    constraints :: !(Strict.Vector (Constraint scope)),
    methods :: !(Strict.Vector (Scheme (Local ':+ scope)))
  }
  deriving (Show)

instance Shift Class where
  shift = shiftDefault

instance Shift.Functor Class where
  map = Shift2.mapDefault

instance Shift2.Functor Class where
  map = Substitute.mapDefault

instance Substitute.Functor Class where
  map category Class {parameter, constraints, methods} =
    Class
      { parameter = Substitute.map category parameter,
        constraints = Substitute.map category <$> constraints,
        methods = Substitute.map (Substitute.Over category) <$> methods
      }

kind :: Class scope -> Type scope
kind Class {parameter} = Type.Function parameter Type.Constraint

info :: Class scope -> MethodInfo scope
info Class {constraints} = MethodInfo {constraintCount = length constraints}
