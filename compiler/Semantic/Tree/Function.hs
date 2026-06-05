{-# LANGUAGE_HAZY UnorderedRecords #-}

module Semantic.Tree.Function where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (..))
import qualified Semantic.FreeVariables as FreeTermVariables
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Pattern (Pattern)
import Semantic.Tree.RightHandSide (RightHandSide)
import Syntax.Position (Position)

data Function layout stage scope
  = Plain
      {rightHandSide :: !(RightHandSide layout stage scope)}
  | Bound
      { functionPosition :: !Position,
        patternx :: !(Pattern stage scope),
        function :: !(Function layout stage (Scope.Pattern ':+ scope))
      }
  deriving (Show)

instance Shift (Function layout stage) where
  shift = shiftDefault

instance Shift.Functor (Function layout stage) where
  map category = \case
    Plain {rightHandSide} -> Plain {rightHandSide = Shift.map category rightHandSide}
    Bound {functionPosition, patternx, function} ->
      Bound
        { functionPosition,
          patternx = Shift.map category patternx,
          function = Shift.map (Shift.Over category) function
        }

instance FreeTermVariables (Function layout) where
  freeTermVariables target = \case
    Plain {rightHandSide} -> freeTermVariables target rightHandSide
    Bound {function} -> freeTermVariables (FreeTermVariables.Over target) function

instance Connect Function where
  connect = \case
    Plain {rightHandSide} ->
      Plain
        { rightHandSide = connect rightHandSide
        }
    Bound {functionPosition, patternx, function} ->
      Bound
        { functionPosition,
          patternx,
          function = connect function
        }
  seperate = \case
    Plain {rightHandSide} ->
      Plain
        { rightHandSide = seperate rightHandSide
        }
    Bound {functionPosition, patternx, function} ->
      Bound
        { functionPosition,
          patternx,
          function = seperate function
        }
