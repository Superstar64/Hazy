module Semantic.Tree.Alternative where

import Semantic.Connect (Connect (..))
import Semantic.FreeVariables (FreeTermVariables (freeTermVariables))
import qualified Semantic.FreeVariables as FreeTermVariables
import Semantic.Scope (Environment ((:+)))
import qualified Semantic.Scope as Scope (Pattern)
import Semantic.Shift (Shift, shiftDefault)
import qualified Semantic.Shift as Shift
import Semantic.Tree.Pattern (Pattern)
import Semantic.Tree.RightHandSide (RightHandSide)

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
