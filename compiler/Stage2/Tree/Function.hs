{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Function where

import Stage1.Position (Position)
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

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
