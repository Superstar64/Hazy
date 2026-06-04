module Stage2.Tree.Alternative where

import Stage2.Connect (Connect (..))
import Stage2.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Stage2.FreeVariables as FreeTermVariables
import Stage2.Scope (Environment ((:+)))
import qualified Stage2.Scope as Scope (Pattern)
import Stage2.Shift (Shift, shiftDefault)
import qualified Stage2.Shift as Shift
import Stage2.Tree.Pattern (Pattern)
import Stage2.Tree.RightHandSide (RightHandSide)

data Alternative layout stage scope
  = Alternative
  { parameter :: !(Pattern stage scope),
    rightHandSide :: !(RightHandSide layout stage (Scope.Pattern ':+ scope))
  }
  deriving (Show)

instance Shift (Alternative layout stage) where
  shift = shiftDefault

instance Shift.Functor (Alternative layout stage) where
  map category (Alternative patternx rightHandSide) =
    Alternative
      (Shift.map category patternx)
      (Shift.map (Shift.Over category) rightHandSide)

instance FreeTermVariables (Alternative layout) where
  freeTermVariables target Alternative {rightHandSide} =
    freeTermVariables (FreeTermVariables.Over target) rightHandSide

instance Connect Alternative where
  connect Alternative {parameter, rightHandSide} =
    Alternative
      { parameter,
        rightHandSide = connect rightHandSide
      }
  seperate Alternative {parameter, rightHandSide} =
    Alternative
      { parameter,
        rightHandSide = seperate rightHandSide
      }
