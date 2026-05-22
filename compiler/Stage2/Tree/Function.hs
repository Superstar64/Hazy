{-# LANGUAGE_HAZY UnorderedRecords #-}

module Stage2.Tree.Function where

import Stage1.Position (Position)
import qualified Stage1.Tree.Pattern as Stage1 (Pattern (startPosition))
import qualified Stage1.Tree.RightHandSide as Stage1 (RightHandSide (..))
import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (..))
import qualified Stage2.FreeVariables as FreeTermVariables
import Stage2.Layout (Normal)
import Stage2.Resolve.Context (Context)
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Stage (Resolve)
import Stage2.Tree.Pattern (Pattern)
import qualified Stage2.Tree.Pattern as Pattern (augment, resolve)
import Stage2.Tree.RightHandSide (RightHandSide)
import qualified Stage2.Tree.RightHandSide as RightHandSide (resolve)

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

-- todo complain when lambda variables shadow other lambda variables
resolve ::
  Context scope ->
  [Stage1.Pattern Position] ->
  Stage1.RightHandSide Position ->
  Function Normal Resolve scope
resolve context patterns rightHandSide1 = case patterns of
  [] -> Plain {rightHandSide = RightHandSide.resolve context rightHandSide1}
  (pattern1 : patterns) ->
    Bound
      { functionPosition = Stage1.startPosition pattern1,
        patternx,
        function = resolve (Pattern.augment patternx context) patterns rightHandSide1
      }
    where
      patternx = Pattern.resolve context pattern1
